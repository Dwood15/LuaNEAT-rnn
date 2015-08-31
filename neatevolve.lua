--[[ MarI/O by SethBling
-- Intended for use with the BizHawk emulator and Super Mario World
-- 	Additions by Dwood15 on the TAS forums, as well as some help from henke37, 
-- who gave the inspiration for how to add the extra inputs
-- the 'Restart' counter was given thanks to F9Alejandro 

-- TODO: remove network visualization, find way to represent it visually in a better way.
-- TODO: refactorization, optimization, make the script more consistent across the board, as well as more readable
]]--
--local includeFile = require "smw-bizhawk"
require("smw-bizhawk")
--shamelessly stolen from smw-bizhawk on github
local mainmemory = mainmemory

-- Compatibility
local u8  = mainmemory.read_u8
local s8  = mainmemory.read_s8
local u16 = mainmemory.read_u16_le
local s16 = mainmemory.read_s16_le
local u24 = mainmemory.read_u24_le
local s24 = mainmemory.read_s24_le
local WRAM = WRAM
local SMW = SMW

function getCurrentRoom()
	return bit.lshift(u8(WRAM.room_index), 16) + bit.lshift(u8(WRAM.room_index + 1), 8) + u8(WRAM.room_index + 2)
end

--I only care about screens to do timeout math. I don't actually care enough to index ALL of the room ID's for this script
 -- default zero
local function getLevelStats()
	return u8(WRAM.level_index), u8(WRAM.game_mode), u8(WRAM.end_level_timer), getCurrentRoom()
end 

local Current_Level_Index, game_mode, End_Level_Timer, CurrentRoomID = getLevelStats()

--read_screens is in smw-bizhawk
local give_fitBonus = false
local levelType, currLevelScreenCount, hScreenCurrent, hScreenCurrCount, vScreenCurrent, vScreenCurrCount = read_screens()

function getPlayerStats()
	return s16(WRAM.x), s16(WRAM.y), u24(WRAM.mario_score), u8(WRAM.game_over_time_out_flag), u8(WRAM.exit_level_byte), u8(WRAM.mario_lives)
end

--local current_level = level.new({1, 2, 3}) -- how to declare new level objects

function initializeConstants()
		Filename = {"DP1.State"} -- This way we can train the program on levels more efficiently.
		ButtonNames = {
			"A",
			"B",
			"X",
			"Y",
			"Up",
			"Down",
			"Left",
			"Right"
		}
		fNameIndex = 1
		BOXRADIUS = 6
		INPUTSIZE = 4909 --(BOXRADIUS*2+1)*(BOXRADIUS*2+1) --13 * 13 = 169
		
		Inputs = 4910 --INPUTSIZE + 1 --increment the 2nd value if you wish to add more
								-- if you wish to add more. 
								-- lua is one-indexed unless you tell it otherwise.
		Outputs = #ButtonNames

		POPULATION = 200
		DELTADISJOINT = 2.0
		DELTAWEIGHTS = 0.4
		DELTATHRESHOLD = 1.0
		
		STALESPECIES = 10
		MUTATECONNECTIONSCHANCE = 0.33
		PERTURBCHANCE = 0.90
		CROSSOVERCHANCE = 0.75
		LINKMUTATIONCHANCE = 1.80
		NODEMUTATIONCHANCE = 0.65
		BIASMUTATIONCHANCE = 0.45
		STEPSIZE = 0.2
		DISABLEMUTATIONCHANCE = .20
		ENABLEMUTATIONCHANCE = .45
		TIMEOUTCONST = 1500
		RANDOMCULLCHANCE = .01 --TODO: this and extinction
		MAXNODES = 500000
end
		
function getPositions() --get mario location and score, along with screen values
	if gameinfo.getromname() == "Super Mario World (USA)" then
	local last_level_exit_byte = level_exit_byte 
		marioX, marioY, marioScore, ai_failed_flag, level_exit_byte, mario_lives = getPlayerStats()
		if level_exit_byte ~= last_level_exit_byte and level_exit_byte == 128 then died = true end
		local layer1x = s16(0x1A)
		local layer1y = s16(0x1C)	
		screenX = marioX-layer1x
		screenY = marioY-layer1y
		local tmpScrnX = hScreenCurrent
		local tmpScrnY = vScreenCurrent
		levelType, currLevelScreenCount, hScreenCurrent, hScreenCurrCount, vScreenCurrent, vScreenCurrCount = read_screens()
			
		--Only bother updating if it's not the same
		if hScreenCurrent ~= tmpScrnX then
			if lasthScreenCurrent ~= 0 then 		
				give_fitBonus = true
			end		
			lasthScreenCurrent = tmpScrnX
		end
		
		if vScreenCurrent ~= tmpScrnY then 
			if lastvScreenCurrent ~= 0 then 		
				give_fitBonus = true
			end		
			lastvScreenCurrent = tmpScrnY
		end
		
		Current_Level_Index, game_mode, End_Level_Timer, CurrentRoomID = getLevelStats()
	end
end

function getTile(dx, dy)
	x = math.floor((marioX+dx+8)/16)
	y = math.floor((marioY+dy)/16)
	return memory.readbyte(0x1C800 + math.floor(x/0x10)*0x1B0 + y*0x10 + x%0x10)
end

function getSprites()
	local sprites = {}
	for slot= 0, SMW.sprite_max - 1 do -- May as well search the whole bit instead of doing the first 12 or so sprites.
									-- This way, mario will actually know where he is!
		local status = memory.readbyte(WRAM.sprite_status+slot)
			if status ~= 0 then -- we only care about objects we can interact with.....
				spritex = memory.readbyte(0xE4+slot) + memory.readbyte(WRAM.sprite_x_high+slot)*256 --5344
				spritey = memory.readbyte(0xD8+slot) + memory.readbyte(WRAM.sprite_y_high+slot)*256
				spriteNo = u8(WRAM.sprite_number + slot)
				sprites[#sprites+1] = {["ID"]= spriteNo, ["x"]=spritex, ["y"]=spritey}
			end
	end            
	return sprites
end
 
function getExtendedSprites()
	local extended = {}
	for slot=0, 11 do -- the list of extended sprites here...
		local number = memory.readbyte(0x170B+slot)
		if number > 1 and number ~= 15 then
			spritex = memory.readbyte(0x171F+slot) + memory.readbyte(0x1733+slot)*256
			spritey = memory.readbyte(0x1715+slot) + memory.readbyte(0x1729+slot)*256
			extended[#extended+1] = {["ID"]= number, ["x"]=spritex, ["y"]=spritey}
		end
	end          
	return extended
end

function idToBinaryArray(spriteID, inputArray)
	powers = {256, 128, 64, 32, 16, 8, 4, 2}
	if spriteID == nil then spriteID = 0xFF end
	for i = 1, #powers do
		if spriteID > powers[i] then
			spriteID = spriteID - powers[i]
			inputArray[i] = 1
		else
			inputArray[i] = - 1
		end
	end
	return inputArray
end

--Yes, this code is repetitive. If there is a faster, more efficient way of doing this, please message dwood15
-- using reddit or tasvideos.com forums.
local function directionalGet(dx, dy)
	local	extInd = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}
	if distx < 0 then
		extInd[1] = -1
		else if	distx == 0 then 
			extInd[2] = -1
			else if distx > 0 then
				extInd[3] = -1
			end
		end
	end
	if disty < 0 then
		extInd[4] = -1
		else if	disty == 0 then 
			extInd[5] = -1
				else if disty > 0 then
					extInd[6] = -1
				end
		end
	end	
	distx = math.abs(distx)
	disty = math.abs(disty)
	if distx < 7 then
		extInd[7] = -1
		if distx < 6 then
			extInd[8] = -1
			if distx < 5 then
				extInd[9] = -1
				if distx < 4 then
					extInd[10] = -1
					if distx < 3 then
						extInd[11] = -1
						if distx < 2 then
							extInd[12] = -1
							if distx < 1 then
								extInd[13] = -1
							end
						end
					end
				end
			end
		end
	end
	if disty < 7 then 
		extInd[14] = -1
		if disty < 6 then 
			extInd[15] = -1
			if disty < 5 then
				extInd[16] = -1
				if disty < 4 then
					extInd[17] = -1
					if disty < 3 then
						extInd[18] = -1
						if disty < 2 then
							extInd[19] = -1
							if disty < 1 then
								extInd[20] = -1
							end
						end
					end
				end
			end
		end
	end
	return extInd
end

function getInputs()
        getPositions()
        sprites = getSprites()
        extended = getExtendedSprites()
       
        local inputs = {}
		-- 6 * 16 = 96 so we search 96 up, 96 down, 96 left, and 96 to the right. 
		-- tile is 16 bytes large?
        for dy=-BOXRADIUS*16, BOXRADIUS*16,16 do
				-- -96 to 96
                for dx= -BOXRADIUS*16, BOXRADIUS*16,16 do
				--we start from the bottom left, and seek from there to the right.
                        inputs[#inputs + 1] = 0 --we add the input
						local ind = { 1, 1, 1, 1, 1, 1, 1, 1 }
						local extInd = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}
                        tile = getTile(dx, dy) --get the point at that position on the screen?
                        if tile == 1 and marioY+dy < 0x1B0 then
                                inputs[#inputs] = 1 --firing neuron
                        end
							
                        --So mario just knows when something is nearby, not what type it is. Based on proximity and level memorization entirely.
                        for i = 1, #sprites do
                                distx = sprites[i]["x"] - (marioX+dx)
                                disty = sprites[i]["y"] - (marioY+dy)
                                if math.abs(distx) <= 8 or math.abs(disty) <= 8 then
									inputs[#inputs] = -1 --the sprite is nearby
									extInd = directionalGet(distx, disty)				
									--fire a neuron based on the TYPE of sprite around, so we need the type...
								ind = idToBinaryArray(sprites[i]["ID"], ind)
								end
                        end
 
                        for i = 1,#extended do
                                distx = extended[i]["x"] - (marioX+dx)
                                disty = extended[i]["y"] - (marioY+dy)
                                if math.abs(distx) < 8 and math.abs(disty) < 8 then
									inputs[#inputs] = -1 -- the sprite is nearby/input is firing, i don't think mario has any way to tell the type of sprite???
									ind = {1, 1, 1, 1, 1, 1, -1, -1}
									extInd = directionalGet(distx, disty)
                                end
                        end
						for i = 1, #ind do
							inputs[#inputs + 1] = ind[i]
						end
						for i = 1, #extInd do
							inputs[#inputs + 1] = extInd[i]
						end
                end
        end
		local player_blocked_status = u8(WRAM.player_blocked_status)
		local blocked_status = {1, 1, 1, 1, 1, 1, 1, 1}
		if bit.check(player_blocked_status, 0) then blocked_status[1] = -1 end
		if bit.check(player_blocked_status, 1) then blocked_status[2] = -1 end
		if bit.check(player_blocked_status, 2) then blocked_status[3] = -1 end
		if bit.check(player_blocked_status, 3) then blocked_status[4] = -1 end
		if bit.check(player_blocked_status, 4) then blocked_status[5] = -1 end
		if bit.check(player_blocked_status, 5) then blocked_status[6] = -1 end
		if bit.check(player_blocked_status, 6) then blocked_status[7] = -1 end
		if bit.check(player_blocked_status, 7) then blocked_status[8] = -1 end
		for i = 1, #blocked_status do
			inputs[#inputs + 1] = blocked_status[i]
		end
        --mariovx = memory.read_s8(0x7B)
        --mariovy = memory.read_s8(0x7D)
        return inputs --getExtraInputs(inputs)
end

function sigmoid(x)
	return 2/(1+math.exp(-4.9*x))-1
end

function newInnovation()
	pool.innovation = pool.innovation + 1
	return pool.innovation
end

function newPool()
	local pool = {}
	pool.species = {}
	pool.generation = 0
	pool.innovation = Outputs
	pool.currentSpecies = 1
	pool.currentGenome = 1
	pool.currentFrame = 0
	pool.maxFitness = 0
	return pool
end

function newSpecies()
	local species = {}
	species.topFitness = 0
	species.staleness = 0
	species.genomes = {}
	species.averageFitness = 0
	return species
end

function newGenome()
	local genome = {}
	genome.genes = {}
	genome.fitness = 0
	genome.adjustedFitness = 0
	genome.network = {}
	genome.maxneuron = 0
	genome.globalRank = 0
	genome.mutationRates = {}
	genome.mutationRates.connections = MUTATECONNECTIONSCHANCE
	genome.mutationRates.link = LINKMUTATIONCHANCE
	genome.mutationRates.bias = BIASMUTATIONCHANCE
	genome.mutationRates.node = NODEMUTATIONCHANCE
	genome.mutationRates.enable = ENABLEMUTATIONCHANCE
	genome.mutationRates.disable = DISABLEMUTATIONCHANCE
	genome.mutationRates.step = STEPSIZE
	
	return genome
end

function copyGenome(genome)
	local genome2 = newGenome()
	for g=1,#genome.genes do
		table.insert(genome2.genes, copyGene(genome.genes[g]))
	end
	genome2.maxneuron = genome.maxneuron
	genome2.mutationRates.connections = genome.mutationRates.connections
	genome2.mutationRates.link = genome.mutationRates.link
	genome2.mutationRates.bias = genome.mutationRates.bias
	genome2.mutationRates.node = genome.mutationRates.node
	genome2.mutationRates.enable = genome.mutationRates.enable
	genome2.mutationRates.disable = genome.mutationRates.disable
	
	return genome2
end

function basicGenome()
	local genome = newGenome()
	local innovation = 1

	genome.maxneuron = Inputs
	mutate(genome)
	
	return genome
end

function newGene()
	local gene = {}
	gene.into = 0
	gene.out = 0
	gene.weight = 0.0
	gene.enabled = true
	gene.innovation = 0
	
	return gene
end

function copyGene(gene)
	local gene2 = newGene()
	gene2.into = gene.into
	gene2.out = gene.out
	gene2.weight = gene.weight
	gene2.enabled = gene.enabled
	gene2.innovation = gene.innovation
	
	return gene2
end

function newNeuron()
	local neuron = {}
	neuron.incoming = {}
	neuron.value = 0.0
	
	return neuron
end

function generateNetwork(genome)
	local network = {}
	network.neurons = {}
	
	for i = 1, Inputs do
		network.neurons[i] = newNeuron()
	end
	
	for o=1,Outputs do
		network.neurons[MAXNODES+o] = newNeuron()
	end
	
	table.sort(genome.genes, function (a,b)
		return (a.out < b.out)
	end)
	for i=1,#genome.genes do
		local gene = genome.genes[i]
		if gene.enabled then
			if network.neurons[gene.out] == nil then
				network.neurons[gene.out] = newNeuron()
			end
			local neuron = network.neurons[gene.out]
			table.insert(neuron.incoming, gene)
			if network.neurons[gene.into] == nil then
				network.neurons[gene.into] = newNeuron()
			end
		end
	end
	
	genome.network = network
end

function evaluateNetwork(network, inputs)
	table.insert(inputs, 1)
	if #inputs ~= Inputs then
		console.writeline("Incorrect # of inputs, we have: " .. #inputs .." Expected: " .. Inputs)
		return {}
	end
       
	for i=1,Inputs do
		network.neurons[i].value = inputs[i]
	end
       
	for _,neuron in pairs(network.neurons) do
		if #neuron.incoming > 0 then
			local sum = 0
			for j = 1,#neuron.incoming do
				local incoming = neuron.incoming[j]
				local other = network.neurons[incoming.into]
				sum = sum + incoming.weight * other.value
			end
			neuron.value = sigmoid(sum)
		end
	end
       
	local outputs = {}
	for o=1,Outputs do
		local button = "P1 " .. ButtonNames[o]
		if network.neurons[MAXNODES+o].value > 0 then
			outputs[button] = true
		else
			outputs[button] = false
		end
	end
       
        return outputs
end

function crossover(g1, g2)
	-- Make sure g1 is the higher fitness genome
	if g2.fitness > g1.fitness then
		tempg = g1
		g1 = g2
		g2 = tempg
	end --swap the genes

	local child = newGenome()
	
	local innovations2 = {}
	for i=1,#g2.genes do
		local gene = g2.genes[i]
		innovations2[gene.innovation] = gene
	end
	
	for i=1,#g1.genes do
		local gene1 = g1.genes[i]
		local gene2 = innovations2[gene1.innovation]
		if gene2 ~= nil and math.random(2) == 1 and gene2.enabled then
			table.insert(child.genes, copyGene(gene2))
		else
			table.insert(child.genes, copyGene(gene1))
		end
	end
	
	child.maxneuron = math.max(g1.maxneuron,g2.maxneuron)
	
	for mutation,rate in pairs(g1.mutationRates) do
		child.mutationRates[mutation] = rate
	end
	
	return child
end

function randomNeuron(genes, nonInput)
	local neurons = {}
	if not nonInput then
		for i=1, Inputs do
			neurons[i] = true
		end
	end
	for o=1, Outputs do
		neurons[MAXNODES+o] = true
	end
	
	for i=1,#genes do
		if (not nonInput) or genes[i].into > Inputs then
			neurons[genes[i].into] = true
		end
		if (not nonInput) or genes[i].out > Inputs then
			neurons[genes[i].out] = true
		end
	end

	local count = 0
	for _,_ in pairs(neurons) do
		count = count + 1
	end
	local n = math.random(1, count)
	
	for k,v in pairs(neurons) do
		n = n-1
		if n == 0 then
			return k
		end
	end
	
	return 0
end

function containsLink(genes, link)
	for i=1,#genes do
		local gene = genes[i]
		if gene.into == link.into and gene.out == link.out then
			return true
		end
	end
end

function pointMutate(genome)
	local step = genome.mutationRates.step
	
	for i=1,#genome.genes do
		local gene = genome.genes[i]
		if math.random() < PERTURBCHANCE then
			gene.weight = gene.weight + math.random() * step*2 - step
		else
			gene.weight = math.random()*4-2
		end
	end
end

function linkMutate(genome, forceBias)
	local neuron1 = randomNeuron(genome.genes, false)
	local neuron2 = randomNeuron(genome.genes, true)
	 
	local newLink = newGene()
	if neuron1 <= Inputs and neuron2 <= Inputs then
		--Both input nodes
		return
	end
	if neuron2 <= Inputs then
		-- Swap output and input
		local temp = neuron1
		neuron1 = neuron2
		neuron2 = temp
	end

	newLink.into = neuron1
	newLink.out = neuron2
	if forceBias then
		newLink.into = Inputs
	end
	
	if containsLink(genome.genes, newLink) then
		return
	end
	newLink.innovation = newInnovation()
	newLink.weight = math.random()*4-2
	
	table.insert(genome.genes, newLink)
end

function nodeMutate(genome)
	if #genome.genes == 0 then
		return
	end

	genome.maxneuron = genome.maxneuron + 1

	local gene = genome.genes[math.random(1,#genome.genes)]
	if not gene.enabled then
		return
	end
	gene.enabled = false
	
	local gene1 = copyGene(gene)
	gene1.out = genome.maxneuron
	gene1.weight = 1.0
	gene1.innovation = newInnovation()
	gene1.enabled = true
	table.insert(genome.genes, gene1)
	
	local gene2 = copyGene(gene)
	gene2.into = genome.maxneuron
	gene2.innovation = newInnovation()
	gene2.enabled = true
	table.insert(genome.genes, gene2)
end

function enableDisableMutate(genome, enable)
	local candidates = {}
	for _,gene in pairs(genome.genes) do
		if gene.enabled == not enable then
			table.insert(candidates, gene)
		end
	end
	
	if #candidates == 0 then
		return
	end
	
	local gene = candidates[math.random(1,#candidates)]
	gene.enabled = not gene.enabled
end

function mutate(genome)
	for mutation,rate in pairs(genome.mutationRates) do
		if math.random(1,2) == 1 then
			genome.mutationRates[mutation] = 0.95*rate
		else
			genome.mutationRates[mutation] = 1.05263*rate
		end
	end

	if math.random() < genome.mutationRates.connections then
		pointMutate(genome)
	end
	
	local p = genome.mutationRates.link
	while p > 0 do
		if math.random() < p then
			linkMutate(genome, false)
		end
		p = p - 1
	end

	p = genome.mutationRates.bias
	while p > 0 do
		if math.random() < p then
			linkMutate(genome, true)
		end
		p = p - 1
	end
	
	p = genome.mutationRates.node
	while p > 0 do
		if math.random() < p then
			nodeMutate(genome)
		end
		p = p - 1
	end
end

function disjoint(genes1, genes2)
	local i1 = {}
	for i = 1,#genes1 do
		local gene = genes1[i]
		i1[gene.innovation] = true
	end

	local i2 = {}
	for i = 1,#genes2 do
		local gene = genes2[i]
		i2[gene.innovation] = true
	end
	
	local disjointGenes = 0
	for i = 1,#genes1 do
		local gene = genes1[i]
		if not i2[gene.innovation] then
			disjointGenes = disjointGenes+1
		end
	end
	
	for i = 1,#genes2 do
		local gene = genes2[i]
		if not i1[gene.innovation] then
			disjointGenes = disjointGenes+1
		end
	end
	
	local n = math.max(#genes1, #genes2)
	
	return disjointGenes / n
end

function weights(genes1, genes2)
	local i2 = {}
	for i = 1,#genes2 do
		local gene = genes2[i]
		i2[gene.innovation] = gene
	end

	local sum = 0
	local coincident = 0
	for i = 1,#genes1 do
		local gene = genes1[i]
		if i2[gene.innovation] ~= nil then
			local gene2 = i2[gene.innovation]
			sum = sum + math.abs(gene.weight - gene2.weight)
			coincident = coincident + 1
		end
	end
	
	return sum / coincident
end
	
function sameSpecies(genome1, genome2)
	local dd = DELTADISJOINT*disjoint(genome1.genes, genome2.genes)
	local dw = DELTAWEIGHTS*weights(genome1.genes, genome2.genes) 
	return dd + dw < DELTATHRESHOLD
end

function rankGlobally()
	local global = {}
	for s = 1,#pool.species do
		local species = pool.species[s]
		for g = 1,#species.genomes do
			table.insert(global, species.genomes[g])
		end
	end
	table.sort(global, function (a,b)
		return (a.fitness < b.fitness)
	end)
	
	for g=1,#global do
		global[g].globalRank = g
	end
end

function calculateAverageFitness(species)
	local total = 0
	
	for g=1,#species.genomes do
		local genome = species.genomes[g]
		total = total + genome.globalRank
	end
	
	species.averageFitness = total / #species.genomes
end

function totalAverageFitness()
	local total = 0
	for s = 1,#pool.species do
		local species = pool.species[s]
		total = total + species.averageFitness
	end

	return total
end

function cullSpecies(cutToOne)
	for s = 1,#pool.species do
		local species = pool.species[s]
		
		table.sort(species.genomes, function (a,b)
			return (a.fitness > b.fitness)
		end)
		
		local remaining = math.ceil(#species.genomes/2)
		if cutToOne then
			remaining = 1
		end
		while #species.genomes > remaining do
			table.remove(species.genomes)
		end
	end
end

function breedChild(species)
	local child = {}
	if math.random() < CROSSOVERCHANCE then
		g1 = species.genomes[math.random(1, #species.genomes)]
		g2 = species.genomes[math.random(1, #species.genomes)]
		child = crossover(g1, g2)
	else
		g = species.genomes[math.random(1, #species.genomes)]
		child = copyGenome(g)
	end
	
	mutate(child)
	
	return child
end

function removeStaleSpecies()
	local survived = {}

	for s = 1,#pool.species do
		local species = pool.species[s]
		
		table.sort(species.genomes, function (a,b)
			return (a.fitness > b.fitness)
		end)
		
		if species.genomes[1].fitness > species.topFitness then
			species.topFitness = species.genomes[1].fitness
			species.staleness = 0
		else
			species.staleness = species.staleness + 1
		end
		if species.staleness < STALESPECIES or species.topFitness >= pool.maxFitness then
			table.insert(survived, species)
		end
	end

	pool.species = survived
end

function removeWeakSpecies()
	local survived = {}

	local sum = totalAverageFitness()
	for s = 1,#pool.species do
		local species = pool.species[s]
		-- breed = math.floor(species.averageFitness / sum * POPULATION)
		if math.floor(species.averageFitness / sum * POPULATION) >= 1 then
			table.insert(survived, species)
		end
	end

	pool.species = survived
end 

function addToSpecies(child)
	local foundSpecies = false
	for s=1,#pool.species do
		local species = pool.species[s]
		if not foundSpecies and sameSpecies(child, species.genomes[1]) then
			table.insert(species.genomes, child)
			foundSpecies = true
		end
	end
	
	if not foundSpecies then
		local childSpecies = newSpecies()
		table.insert(childSpecies.genomes, child)
		table.insert(pool.species, childSpecies)
	end
end

function newGeneration()
	--console.writeline("New generation")
	cullSpecies(false) -- Cull the bottom half of each species
	rankGlobally()
	removeStaleSpecies()
	rankGlobally()
	for s = 1,#pool.species do
		local species = pool.species[s]
		calculateAverageFitness(species)
	end
	
	removeWeakSpecies()
	
	local sum = totalAverageFitness()
	local children = {} 
	for s = 1,#pool.species do
		local species = pool.species[s]
		breed = math.floor(species.averageFitness / sum * POPULATION) - 1
		for i=1,breed do
			table.insert(children, breedChild(species))
		end
	end
	
	cullSpecies(true) -- Cull all but the top member of each species
	while #children + #pool.species < POPULATION do
		local species = pool.species[math.random(1, #pool.species)]
		table.insert(children, breedChild(species))
	end
	for c=1,#children do
	
		local child = children[c]
		addToSpecies(child)
	end
	
	pool.generation = pool.generation + 1
	os.execute("mkdir AIData\\Gen" .. pool.generation) 
	writeNeuralNetworkFile("AIData\\Gen" .. pool.generation .. "backup." .. "." .. forms.gettext(saveLoadFile))
end
	
function initializePool()
	pool = newPool()

	for i=1, POPULATION do
		basic = basicGenome()
		addToSpecies(basic)
	end

	initializeRun()
end

function clearJoypad()
	controller = {}
	for b = 1,#ButtonNames do
		controller["P1 " .. ButtonNames[b]] = false
	end
	joypad.set(controller)
end

function evaluateCurrent()
	local species = pool.species[pool.currentSpecies]
	local genome = species.genomes[pool.currentGenome]

	inputs = getInputs()
	controller = evaluateNetwork(genome.network, inputs)
	
	if controller["P1 Left"] and controller["P1 Right"] then
		controller["P1 Left"] = false
		controller["P1 Right"] = false
	end
	if controller["P1 Up"] and controller["P1 Down"] then
		controller["P1 Up"] = false
		controller["P1 Down"] = false
	end

	joypad.set(controller)
end

function nextGenome()
	pool.currentGenome = pool.currentGenome + 1
	if pool.currentGenome > #pool.species[pool.currentSpecies].genomes then
		pool.currentGenome = 1
		pool.currentSpecies = pool.currentSpecies+1
		if pool.currentSpecies > #pool.species then
			--console.writeline("NextGenome")
			newGeneration()
			pool.currentSpecies = 1
		end
	end
end

 function fitnessAlreadyMeasured()
	 local species = pool.species[pool.currentSpecies]
	 local genome = species.genomes[pool.currentGenome]
	
	 return genome.fitness ~= 0
end
-- --instead of doing object name into

 --warning: overwrites files
 function createNewCSV(filename, dataStrings)
	local file = io.open(filename, 'w')
	file:write(dataStrings)
	file:close()
 end

function appendToCSV(filename, datastring)
	local file = io.open(filename, 'a')
	file:write(datastring)
	file:close()
 end

function writeNeuralNetworkFile(filename)
        local file = io.open(filename, "w")
	file:write(pool.generation .. "\n")
	file:write(pool.maxFitness .. "\n")
	file:write(#pool.species .. "\n")
        for n,species in pairs(pool.species) do
		file:write(species.topFitness .. "\n")
		file:write(species.staleness .. "\n")
		file:write(#species.genomes .. "\n")
		for m,genome in pairs(species.genomes) do
			file:write(genome.fitness .. "\n")
			file:write(genome.maxneuron .. "\n")
			for mutation,rate in pairs(genome.mutationRates) do
				file:write(mutation .. "\n")
				file:write(rate .. "\n")
			end
			file:write("done\n")
			
			file:write(#genome.genes .. "\n")
			for l,gene in pairs(genome.genes) do
				file:write(gene.into .. " ")
				file:write(gene.out .. " ")
				file:write(gene.weight .. " ")
				file:write(gene.innovation .. " ")
				if(gene.enabled) then
					file:write("1\n")
				else
					file:write("0\n")
				end
			end
		end
        end
        file:close()
end

function savePool()
	local filename = forms.gettext(saveLoadFile)
	writeNeuralNetworkFile(filename)
end

function loadNeuralNetFile(filename)
    local file = io.open(filename, "r")
	pool = newPool()
	pool.generation = file:read("*number")
	pool.maxFitness = file:read("*number")
	forms.settext(maxFitnessLabel, "Max Fitness: " .. math.floor(pool.maxFitness))

        local numSpecies = file:read("*number")
        for s=1,numSpecies do
		local species = newSpecies()
		table.insert(pool.species, species)
		species.topFitness = file:read("*number")
		species.staleness = file:read("*number")
		local numGenomes = file:read("*number")
		for g=1,numGenomes do
			local genome = newGenome()
			table.insert(species.genomes, genome)
			genome.fitness = file:read("*number")
			genome.maxneuron = file:read("*number")
			local line = file:read("*line")
			while line ~= "done" do
				genome.mutationRates[line] = file:read("*number")
				line = file:read("*line")
			end
			local numGenes = file:read("*number")
			for n=1,numGenes do
				local gene = newGene()
				table.insert(genome.genes, gene)
				local enabled
				gene.into, gene.out, gene.weight, gene.innovation, enabled = file:read("*number", "*number", "*number", "*number", "*number")
				if enabled == 0 then
					gene.enabled = false
				else
					gene.enabled = true
				end
				
			end
		end
	end
        file:close()
	
	while fitnessAlreadyMeasured() do
		nextGenome()
	end
	initializeRun()
	pool.currentFrame = pool.currentFrame + 1
end
 
function loadPool()
	local filename = forms.gettext(saveLoadFile)
	loadNeuralNetFile(filename)
end

function buildForm()
	event.onexit(onExit)
	form = forms.newform(260, 280, "Fitness")
	maxFitnessLabel = forms.label(form, "Max Fitness: " .. math.floor(pool.maxFitness), 5, 8)
	showNetwork = forms.checkbox(form, "Show Map", 5, 30)
	showMutationRates = forms.checkbox(form, "Show M-Rates", 5, 52)
	restartButton = forms.button(form, "Restart", initializePool, 5, 77)
	saveButton = forms.button(form, "Save", savePool, 5, 102)
	loadButton = forms.button(form, "Load", loadPool, 80, 102)
	saveLoadFile = forms.textbox(form, Filename[fNameIndex] .. ".pool", 170, 25, nil, 5, 148)
	saveLoadLabel = forms.label(form, "Save/Load:", 5, 129)
	playTopButton = forms.button(form, "Play Top", playTop, 5, 170)
	hideBanner = forms.checkbox(form, "Hide Banner", 5, 190)
end

function playTop()
	local maxfitness = 0
	local maxs, maxg
	for s,species in pairs(pool.species) do
		for g,genome in pairs(species.genomes) do
			if genome.fitness > maxfitness then
				maxfitness = genome.fitness
				maxs = s
				maxg = g
			end
		end
	end
	
	pool.currentSpecies = maxs
	pool.currentGenome = maxg
	pool.maxFitness = maxfitness
	forms.settext(maxFitnessLabel, "Max Fitness: " .. math.floor(pool.maxFitness))
	initializeRun()
	pool.currentFrame = pool.currentFrame + 1
	return
end

function onExit()
	forms.destroy(form)
end

function initializeBaseVariables()
	marioX, marioY, marioScore, ai_failed_flag, level_exit_byte, mario_lives = getPlayerStats() 
	last_level_exit_byte = level_exit_byte
	died = false
	originLevelIndex = u8(0x13bf)
	baseScore = marioScore
	lastScore = baseScore
	lastMarioX = marioX
	lastMarioY = marioY
	leftmost = marioX
	rightmost = marioX
	topmost = marioY
	pool.currentFrame = 0
	timeout = TIMEOUTCONST
	fitnessBonus = 0
	bestScore = marioScore
	rightmost = lastMarioX
	levelType, currLevelScreenCount, hScreenCurrent, hScreenCurrCount, vScreenCurrent, vScreenCurrCount = read_screens()
	lasthScreenCurrent = hScreenCurrent
	lastvScreenCurrent = vScreenCurrent
	lastLevelIndex = 0
	Current_Level_Index, game_mode, End_Level_Timer, CurrentRoomID = getLevelStats()
	lastGameMode = game_mode
	give_fitBonus = false
end

function initializeRun()
	savestate.load(Filename[fNameIndex]);
	clearJoypad()
	pool.currentFrame = 0
	--levelIndex = 0x13bf TODO: Find some way to update levelIndex properly
	local species = pool.species[pool.currentSpecies]
	local genome = species.genomes[pool.currentGenome]
	generateNetwork(genome)
	evaluateCurrent()
	timeout = TIMEOUTCONST
	fNameIndex = fNameIndex + 1
	if fNameIndex > #Filename then fNameIndex = 1  end
	fitnessBonus = 0
	initializeBaseVariables()
	emu.limitframerate(false)
	createNewCSV("AIData\\Gen" .. pool.generation .. "\\Spec" .. pool.currentSpecies .. "Genom" .. pool.currentGenome .. ".csv",
	"Frame,Game Mode ID,Level Idx,H Scrn,V Scrn,X Pos,Y Pos,X Speed,Y Speed,Powerup ID, Lives\n")
	collectgarbage()
end

-- MAIN
	timeout = TIMEOUTCONST
	initializeConstants()
	os.execute("mkdir AIData\\Gen0") 
	createNewCSV("AIData\\FinalStats.csv",
	"TimeStamp,Generations,Species,Genome,Fitness,Game Mode ID,Current Level Index,Horizontal Screen,"
	.. "Vertical Screen,X Position,Y Position,Score Change,Died\n");
	if pool == nil then	initializePool() end	
	writeNeuralNetworkFile("AIData\\Gen0\\backup.pool")
	buildForm()
	initializeBaseVariables()
	
while true do

	local species = pool.species[pool.currentSpecies]
	local genome = species.genomes[pool.currentGenome]
	
	if pool.currentFrame % 4 == 0 then --can't merge this, other functions call evalCurrent()
		evaluateCurrent()
		appendToCSV("AIData\\Gen" .. pool.generation .. "\\Spec" .. pool.currentSpecies .. "Genom" .. pool.currentGenome .. ".csv",
		"" .. pool.currentFrame .. "," .. game_mode .. "," .. Current_Level_Index .. "," .. hScreenCurrent .. "," .. vScreenCurrent .. "," .. 
		marioX .. "," .. marioY .. "," .. s8(WRAM.x_speed) .. "," .. s8(WRAM.y_speed) .. "," .. u8(WRAM.powerup) .. "," .. mario_lives .. "\n")
	end

	joypad.set(controller)
	lastMarioX = marioX
	lastMarioY = marioY
	lastScore = marioScore

	lastRoomID = CurrentRoomID
	getPositions()
	if marioX == lastMarioX and lastScore == marioScore and lastMarioY == marioY and game_mode ~= SMW.game_mode_overworld then
		if pool.currentFrame % 3 == 0 then timeout = timeout - 3 end --the timeout evaluates so fast mario doesn't change positions
		else if hScreenCurrent ~= lasthScreenCurrent or CurrentRoomID ~= lastRoomID or lastvScreenCurrent ~= vScreenCurrent then
			if give_fitBonus and level_exit_byte ~= 128 then
				fitnessBonus = fitnessBonus + 25
				timeout = TIMEOUTCONST
				give_fitBonus = false
				else timeout = timeout - 2 end
		else if pool.currentFrame % 4 == 0 then timeout = timeout - 1 
			else if game_mode == SMW.game_mode_overworld and lastGameMode ~= SMW.game_mode_overworld then
				fitnessBonus = fitnessBonus + 200
				timeout = TIMEOUTCONST
				else if End_Level_Timer ~= 0 and level_exit_byte ~= 128 then
					fitnessBonus = fitnessBonus + 1000
					timeout = TIMEOUTCONST
					else if game_mode == SMW.game_mode_overworld and lastGameMode == SMW.game_mode_overworld then timeout = timeout - 4	end
				end
			end
		end
		end
	end
	if level_exit_byte ~= 128 then
		timeoutBonus = math.floor(pool.currentFrame / 6)
	end 
	
	if marioX > rightmost then rightmost = marioX end
	if marioY < topmost then topmost = marioY end
	if marioScore > lastScore and marioScore > bestScore then bestScore = marioScore end
	
	local anim_trig = u8(WRAM.animation_trigger) -- player animation trigger ram address 0x71 

	if ai_failed_flag ~= 0x0 then 
		fitnessBonus = -1000
		timeout = -1 
		timeoutBonus = -1
	end
	
	if level_exit_byte ~= 0x00 and level_exit_byte ~= 128 then
		pool.currentFrame = 0 --reset 
		timeout = -1
		if level_exit_byte == 0xE0 then	fitnessBonus = fitnessBonus * 1.2
		else if level_exit_byte == 0x01 or level_exit_byte == 0x02 then fitnessBonus = fitnessBonus * 2 
		else if level_exit_byte == 128 then fitnessBonus = 0 timeout = 0 timeoutBonus = 0 
																							end
												end
									end
		timeoutBonus = -1 
	end
	
	if timeout + timeoutBonus <= 0 then
		local fitness = rightmost + fitnessBonus + timeoutBonus + math.floor(bestScore * .10) -  math.floor(pool.currentFrame * .25)
		
		if rightmost > 2000 then
			fitness = fitness + 250
		end 
		genome.fitness = fitness
		if fitness > pool.maxFitness then
			-- applying some regularization, probably not the best way, but I think this is better....
			if fitness > pool.maxFitness * 2 then fitness = math.floor(fitness * .66) end
			pool.maxFitness = fitness
			forms.settext(maxFitnessLabel, "Max Fitness: " .. math.floor(pool.maxFitness))
			writeNeuralNetworkFile("backup." .. pool.generation .. "." .. forms.gettext(saveLoadFile))
			if fitness < -1 then fitness = -1 end
		end
		
		appendToCSV("AIData\\FinalStats.csv", "" .. os.time() .. "," .. pool.generation .. "," .. pool.currentSpecies .. "," .. pool.currentGenome .. "," .. 
		fitness .. "," .. game_mode .. "," .. Current_Level_Index .. "," .. hScreenCurrent .. "," .. vScreenCurrCount .. "," .. 
		marioX .. "," .. marioY .. "," .. marioScore - baseScore .. "," .. tostring(died) .. "\n")
		
		pool.currentSpecies = 1
		pool.currentGenome = 1
		
		while fitnessAlreadyMeasured() do nextGenome() end
		initializeRun()

	end
		
	if forms.ischecked(hideBanner) then
		gui.drawBox(0, 0, 300, 30, 0xD0FFFFFF, 0xD0FFFFFF)
		gui.drawText(0, 0, string.format("Level Value: %x, Timeout: %i", level_exit_byte, timeout), 0xFF000000, 11) 
		gui.drawText(0, 12, "Fitness: " ..  math.floor(rightmost + fitnessBonus + math.floor(bestScore / 10) - math.floor(pool.currentFrame / 6)), 0xFF000000, 11)
		gui.drawText(100, 12, "Max Fitness: " .. pool.maxFitness, 0xFF000000, 11)
	end
	pool.currentFrame = math.floor(pool.currentFrame) + 1
	emu.frameadvance();
end