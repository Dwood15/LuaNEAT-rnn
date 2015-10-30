--[[ MarI/O by SethBling, Heavily extended by Dwood15
-- Intended for use with the BizHawk emulator and Super Mario World
-- Special shoutout to henke37, who gave the inspiration for how to add extra inputs
-- 

-- TODO: remove network visualization, find way to represent it better.
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

local	Filename = {"DP1.State"} -- This way we can train the program on levels more efficiently.
local	ButtonNames = {
			"A",
			"B",
			"X",
			"Y",
			"Up",
			"Down",
			"Left",
			"Right"
		}
	
function initializeConstants()

	fNameIndex 					= 1

	STALEXWEIGHT 				= .39 --
	STALEYWEIGHT 				= .33
	STALEDEATHWEIGHT 			= .1
	STALESCOREWEIGHT 			= .18
	
	STALEGENOMERATIO 			= .40 -- staleness < (#species.genomes * STALEGENOMERATIO)
	
	MAXEVALS 					= 2 -- The number of times which we 
	CURRENTRUN 					= 0
	GENERATIONSPERTEST 			= 100 
	BEGINDECAYPERCENT 			= 50 -- 50% - The percentage of generations we allow without one completing the level before we begin penalizing for not making progress.
	GENERATIONALDECAYRATE 		= .25 -- decayrate * population = decayrate. The per-generational number which we reduce the number of generations left till a full restart, so essentially									
										--	if(pool.currGeneration/generationspertest) >= begindecaypercent 	 then GENERATIONSPERTEST = GENERATIONSPERTEST - Gen
										--NOT IMPLEMENTED YET

	MINPOPULATION 				= 30
	MINDESIREDGENOMES 			= 2 -- not implemented
	
	DELTADISJOINT 				= .65
	DELTAWEIGHTS 				= 0.4
	DELTATHRESHOLD 				= 1.0
	STALESPECIES 				= 20
	MUTATECONNECTIONSCHANCE		= 0.4
	PERTURBCHANCE 				= 0.90
	CROSSOVERCHANCE 			= 0.75
	LINKMUTATIONCHANCE 			= 3.0
	NODEMUTATIONCHANCE 			= 0.65
	BIASMUTATIONCHANCE 			= 0.45
	STEPSIZE 					= 0.23
	DISABLEMUTATIONCHANCE 		= .40
	ENABLEMUTATIONCHANCE 		= .60
	TIMEOUTCONST 				= 900
	STANDSTILLPENALTY 			= .60
	RANDOMCULLCHANCE 			= .01 --TODO: this and extinction
	MAXNODES 					= 255000
end	
		
function getPositions() --get mario location and score, along with screen values
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
			inputArray[i] = -1
		else
			inputArray[i] = 1
		end
	end
	return inputArray
end

--Yes, this code is repetitive. If there is a faster, more efficient way of doing this, please message dwood15 using reddit or tasvideos.com forums.

local function directionalGet(dx, dy)
	local	extInd = {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}
	if distx < 0 then
		extInd[1] = 1
		else if	distx == 0 then 
			extInd[2] = 1
			else if distx > 0 then
				extInd[3] = 1
			end
		end
	end
	if disty < 0 then
		extInd[4] = 1
		else if	disty == 0 then 
			extInd[5] = 1
				else if disty > 0 then
					extInd[6] = 1
				end
		end
	end	
	distx = math.abs(distx)
	disty = math.abs(disty)
	if distx < 7 then
		extInd[7] = 1
		if distx < 6 then
			extInd[8] = 1
			if distx < 5 then
				extInd[9] = 1
				if distx < 4 then
					extInd[10] = 1
					if distx < 3 then
						extInd[11] = 1
						if distx < 2 then
							extInd[12] = 1
							if distx < 1 then
								extInd[13] = 1
							end
						end
					end
				end
			end
		end
	end
	if disty < 7 then 
		extInd[14] = 1
		if disty < 6 then 
			extInd[15] = 1
			if disty < 5 then
				extInd[16] = 1
				if disty < 4 then
					extInd[17] = 1
					if disty < 3 then
						extInd[18] = 1
						if disty < 2 then
							extInd[19] = 1
							if disty < 1 then
								extInd[20] = 1
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
		-- tile is 16 * 6 bytes
        for dy=-BOXRADIUS*16, BOXRADIUS*16,16 do
				-- -96 to 96
                for dx= -BOXRADIUS*16, BOXRADIUS*16,16 do
				--we start from the bottom left, and seek from there to the right.
                        inputs[#inputs + 1] = 0 --we add the input
						local x = math.floor((marioX+dx+8)/16)
						local y = math.floor((marioY+dy)/16)
						
						local ind = { -1, -1, -1, -1, -1, -1, -1, -1 }
						local extInd = {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}
						
                        tile = getTile(dx, dy) --get the point at that position on the screen?
						if tilesSeen["L" .. Current_Level_Index .. "X" .. x .. "Y" .. y] == nil and marioY+dy < 0x1B0 then
							tilesSeen["L" .. Current_Level_Index .. "X" .. x .. "Y" .. y] = 1
							totalSeen = totalSeen + 1;
							if tile == 1 then
								totalSeen = totalSeen + 5;
							end
							timeout = TIMEOUTCONST
						end
						
                        if tile == 1 and marioY+dy < 0x1B0 then
                            inputs[#inputs] = 1 --firing neuron
                        end
							
                        --So mario just knows when something is nearby, not what type it is. Based on proximity and level memorization entirely.
                        for i = 1, #sprites do
                                distx = sprites[i]["x"] - (marioX+dx)
                                disty = sprites[i]["y"] - (marioY+dy)
                                if math.abs(distx) <= 8 and math.abs(disty) <= 8 then
									inputs[#inputs] = -1 --the sprite is nearby
									extInd = directionalGet(distx, disty)				
									--fire a neuron based on the TYPE of sprite around...
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
		
		player_blocked_status = u8(WRAM.player_blocked_status)
		blocked_status = {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}
		
		for i = 0, 7, 1 do	
			if bit.check(player_blocked_status, i) then 
				inputs[#inputs + 1] = 1
			else
				inputs[#inputs + 1] = -1
			end
		end

        return inputs
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
	species.staleness = 0.001
	species.genomes = {}
	species.averageFitness = 0
	species.distancefromMean = 0
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
	genome.FinalStats = {}
	genome.FinalStats.X = 0
	genome.FinalStats.Y = 0
	genome.FinalStats.Score = 0
	genome.FinalStats.Died = false
	genome.FinalStats.game_mode = 0
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
	genome2.FinalStats = {}
	genome2.FinalStats.X = 0
	genome2.FinalStats.Y = 0
	genome2.FinalStats.Score = 0
	genome2.FinalStats.Died = false	
	genome2.FinalStats.game_mode = 0
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
	--table.insert(inputs, 1)
	if #inputs ~= Inputs then
		console.writeline("Incorrect # of inputs, we have: " .. #inputs .." Expected: " .. Inputs)
		return {}
	end
       
	for i=1,Inputs do
		network.neurons[i].value = inputs[i]
	end
    for i = 1, MAXEVALS do
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
			gene.weight = gene.weight + (math.random() * STEPSIZE * 2) - STEPSIZE
		else
			gene.weight = (math.random()*4)-2
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
		p = p - .5
	end

	p = genome.mutationRates.bias
	while p > 0 do
		if math.random() < p then
			linkMutate(genome, true)
		end
		p = p - .5
	end
	
	p = genome.mutationRates.node
	while p > 0 do
		if math.random() < p then
			nodeMutate(genome)
		end
		p = p - .5
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

--The range for gene.weight is [-2, 2]
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
			sum = sum + math.abs(gene.weight - gene2.weight) -- The largest value for sum will be 4, the smallest is zero.
			coincident = coincident + 1
		end
	end
	
	return sum / coincident
end
	
function sameSpecies(genome1, genome2)
	local dd = DELTADISJOINT*disjoint(genome1.genes, genome2.genes) -- The largest value will be 1, the smallest will be 0. 
	local dw = DELTAWEIGHTS*weights(genome1.genes, genome2.genes) -- the smallest value is 0, the largest value is 4 * #genes in genome
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
	--console.writeline("Species avgFitness: " .. total .. "Species genomes: " .. #species.genomes)
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

function removeStaleSpecies() --this is where the novelty f() is important
    local survived = {}
	console.writeline("Removing stale, there are: " .. #pool.species .. " species")
    for s = 1, #pool.species do
	
        local species = pool.species[s]
		
		console.writeline("genome count for specie #" .. s .. ": " .. #species.genomes )
		local stale = 0.0
		
		for g = 1, #species.genomes do
			for gtop = #species.genomes, 1 do 
				if gtop ~= g then
				if species.genomes[g].FinalStats.X == species[gtop].FinalStats.X then stale = stale + STALEXWEIGHT end --highest weight
				if species.genomes[g].FinalStats.Y == species[gtop].FinalStats.Y then stale = stale + STALEYWEIGHT end --2nd
				if species.genomes[g].FinalStats.Score == species[gtop].FinalStats.Score then stale = stale + STALESCOREWEIGHT end --3rd
				if species.genomes[g].FinalStats.Died  and species[gtop].FinalStats.Died then stale = stale + STALEDEATHWEIGHT end -- 4th
				end
			end
		end
		console.writeline("Staleness: " .. stale .. " for species: " .. s)
		
				--I need to revisit this statement. I like where it's going, but it needs to be double checked.
		if stale > (#species.genomes * STALEGENOMERATIO ) then stale = math.ceil(stale) end
			--console.writeline("reset stale species for species: " .. s .. " of gen: " .. pool.generation .. " stalenes: " .. staleness)
			
		if species.genomes[1].fitness > pool.maxFitness then
			species.topFitness = species.genomes[1].fitness
		else species.staleness = species.staleness + stale end
		
		console.writeline("species.staleness: " .. species.staleness)
        if species.staleness < STALESPECIES then
            table.insert(survived, species)
        end
	end
	
    pool.species = survived
	console.writeline(" " .. #pool.species .. " spec survived the stale calculations.")
end

function cullSpecies(cutToOne)
	for s = 1, #pool.species do
		local species = pool.species[s]
		
--		table.sort(species.genomes, function (a,b)
--			return (a.fitness > b.fitness)
--		end)
		
		local remaining = math.ceil(math.random(1, #species.genomes))
		if remaining <= 0 then remaining = 2 end
		if remaining > #species.genomes then remaining = #species.genomes end
		if cutToOne then remaining = 1	end --Some randomness to keep things spicy.
		while #species.genomes > remaining do table.remove(species.genomes) end
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

function removeWeakSpecies()
	local survived = {}

	local totalAvgFitness = totalAverageFitness()
	
	for s = 1,#pool.species do
		local species = pool.species[s]
		local result = math.floor(species.averageFitness / (totalAvgFitness * MINPOPULATION))
		if result >= 1 then
			table.insert(survived, species)
		end
	end

	pool.species = survived
end 

function addToSpecies(child)
	local foundSpecies = false
	for s=1, #pool.species do
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
	console.writeline("New generation")
	--Sort the genomes by fitness.
	for s = 1, #pool.species do table.sort(pool.species[s].genomes, function(a, b) return (a.fitness > b.fitness) end) end 

	rankGlobally()
	
	local totalAvgFitness = totalAverageFitness()
	removeStaleSpecies()
	
	cullSpecies(false) -- Cull the bottom half of each species
	
	for s = 1, #pool.species do
		local species = pool.species[s]
		calculateAverageFitness(species)
	end
	console.write("\nRemoving weak species, there are: " .. #pool.species .. " but ")
	removeWeakSpecies()
	console.write(#pool.species .. " survived\n")
	totalAvgFitness = totalAverageFitness()
	local children = {} 
	
	for s = 1,#pool.species do
		local species = pool.species[s]
		local breed = math.floor(species.averageFitness / totalAvgFitness * MINPOPULATION) - 1
		for i=1, breed do
			table.insert(children, breedChild(species))
		end
	end
	
	cullSpecies(true) -- Cull all but the top member of each species
	if #pool.species == 0 then 
		newPool()
		console.writeline("We killed all species, created new pool")
	end
	
	while #children + #pool.species < MINPOPULATION do
		local species = pool.species[math.random(1, (#pool.species))]
		table.insert(children, breedChild(species))
	end
	for c=1,#children do
		local child = children[c]
		addToSpecies(child)
	end
	
	pool.generation = pool.generation + 1
	writeNeuralNetworkFile("AIData\\Gen" .. pool.generation .. "backup." .. forms.gettext(saveLoadFile))
end
	
function initializePool()
	pool = newPool()

	for i=1, MINPOPULATION do
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

function evaluateCurrent(updateInputs)
	local species = pool.species[pool.currentSpecies]
	local genome = species.genomes[pool.currentGenome]

	if updateInputs then inputs = getInputs() end
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
			writeNeuralNetworkFile("AIData\\Gen" .. pool.generation .. "backup" .. pool.generation .. "." .. forms.gettext(saveLoadFile))
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
 function createNewCSV(filename, datastring)
	local file = io.open(filename, 'w')
	if file ~= nil then 
	file:write(datastring)
	file:close()
	else 
	console.writeline("Unable to open file: " .. filename)
	end
 end

function appendToCSV(filename, datastring)
	local file = io.open(filename, 'a')
	if file ~= nil then
		file:write(datastring)
		file:close()
	else
	console.writeline("Unable to open file: " .. filename)
	end
 end

function writeNeuralNetworkFile(filename)
    local file = io.open(filename, "w")
	if file ~= nill then
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
	else
		console.writeline("unable to write file: " .. filename)
	end
end

function savePool()
	local filename = forms.gettext(saveLoadFile)
	writeNeuralNetworkFile(filename)
end

function loadNeuralNetFile(filename)
    local file = io.open(filename, "r")
	if file ~= nil then 
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
	else 
	console.writeline("Could not open: " .. filename .. " please restart.")
	end
end

function loadPool()
	local filename = forms.gettext(saveLoadFile)
	loadNeuralNetFile(filename)
end

function writeConfigFile()
	local file = io.open("ai.cfg", "w")
	if file ~= nil then 
	file:write(fNameIndex .. "\n")
	file:write(STALEXWEIGHT .. "\n")
	file:write(STALEYWEIGHT .. "\n")
	file:write(STALEDEATHWEIGHT .. "\n")
	file:write(STALESCOREWEIGHT .. "\n")
	file:write(STALEGENOMERATIO .. "\n")
	
	file:write(MAXEVALS .. "\n")
	file:write(CURRENTRUN .. "\n")
	file:write(GENERATIONSPERTEST .. "\n")
	file:write(BEGINDECAYPERCENT .. "\n")
	file:write(GENERATIONALDECAYRATE .. "\n")
	
	file:write(MINPOPULATION .. "\n")
	file:write(MINDESIREDGENOMES .. "\n")
	
	file:write(DELTADISJOINT .. "\n")
	file:write(DELTAWEIGHTS .. "\n")
	file:write(DELTATHRESHOLD .. "\n")
	file:write(STALESPECIES .. "\n")
	file:write(MUTATECONNECTIONSCHANCE .. "\n")
	file:write(PERTURBCHANCE .. "\n")
	file:write(CROSSOVERCHANCE .. "\n")
	file:write(LINKMUTATIONCHANCE .. "\n")
	file:write(NODEMUTATIONCHANCE .. "\n")
	file:write(BIASMUTATIONCHANCE .. "\n")
	file:write(STEPSIZE .. "\n")
	file:write(DISABLEMUTATIONCHANCE .. "\n")
	file:write(ENABLEMUTATIONCHANCE .. "\n")
	file:write(TIMEOUTCONST .. "\n")
	file:write(STANDSTILLPENALTY .. "\n")
	file:write(RANDOMCULLCHANCE .. "\n")
	file:write(MAXNODES .. "\n")
	else 
	console.writeline("Unable to write config file to dir")
	end
end
				
function readConfigFile()
	local file = io.open("ai.cfg", "r")
	if file ~= nil then
		fNameIndex = file:read("*number")

		STALEXWEIGHT = file:read("*number") --
		STALEYWEIGHT = file:read("*number")
		STALEDEATHWEIGHT = file:read("*number")
		STALESCOREWEIGHT = file:read("*number")
		
		STALEGENOMERATIO = file:read("*number") -- staleness < (#species.genomes * STALEGENOMERATIO)
		
		MAXEVALS = file:read("*number")
		CURRENTRUN = file:read("*number")
		GENERATIONSPERTEST = file:read("*number")
		BEGINDECAYPERCENT = file:read("*number") -- 50% - The percentage of generations we allow without one completing the level before we begin penalizing for not making progress.
		GENERATIONALDECAYRATE = file:read("*number") -- decayrate * population = decayrate. The per-generational number which we reduce the number of generations left till a full restart, so essentially
		-- (generationspertest = generationspertest - (1 + (decayrate * populationsize))
		--	if(pool.currGeneration/generationspertest) >= begindecaypercent 		
		--NOT IMPLEMENTED YET

		MINPOPULATION = file:read("*number")
		MINDESIREDGENOMES = file:read("*number")
		
		DELTADISJOINT = file:read("*number")
		DELTAWEIGHTS = file:read("*number")
		DELTATHRESHOLD = file:read("*number")
		STALESPECIES = file:read("*number")
		MUTATECONNECTIONSCHANCE = file:read("*number")
		PERTURBCHANCE = file:read("*number")
		CROSSOVERCHANCE = file:read("*number")
		LINKMUTATIONCHANCE = file:read("*number")
		NODEMUTATIONCHANCE = file:read("*number")
		BIASMUTATIONCHANCE = file:read("*number")
		STEPSIZE = file:read("*number")
		DISABLEMUTATIONCHANCE = file:read("*number")
		ENABLEMUTATIONCHANCE = file:read("*number")
		TIMEOUTCONST = file:read("*number")
		STANDSTILLPENALTY = file:read("*number")
		RANDOMCULLCHANCE = file:read("*number") --TODO: this and extinction
		MAXNODES = file:read("*number")

		else 
		console.writeline("Could not find config file.\nUsing defaults\n") 
		initializeConstants()
		writeConfigFile()
	end
	
	BOXRADIUS = 6
	INPUTSIZE = 4909 --(BOXRADIUS*2+1)*(BOXRADIUS*2+1) --13 * 13 = 169
	Inputs = INPUTSIZE
						-- if you wish to add more. 
						-- lua is one-indexed unless you tell it otherwise.
	Outputs = #ButtonNames
	tilesSeen = {}
	totalSeen = 0
	timeout = TIMEOUTCONST
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

function initializeBaseVariables()
	marioX, marioY, marioScore, ai_failed_flag, level_exit_byte, mario_lives = getPlayerStats() 
	last_level_exit_byte = level_exit_byte
	died = false
	framesStandingStill = 0
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
	timeoutBonus = 0
end

function initializeRun()
	savestate.load(Filename[fNameIndex]);
	tilesSeen = {}
	totalSeen = 0
	clearJoypad()
	pool.currentFrame = 0
	--levelIndex = 0x13bf TODO: Find some way to update levelIndex properly
	species = pool.species[pool.currentSpecies]
	genome = species.genomes[pool.currentGenome]
	generateNetwork(genome)
	evaluateCurrent(true)
	timeout = TIMEOUTCONST
	fNameIndex = fNameIndex + 1
	if fNameIndex > #Filename then fNameIndex = 1  end
	fitnessBonus = 0
	initializeBaseVariables()
	emu.limitframerate(false)
	timeoutBonus = 0
	collectgarbage()
	--createNewCSV("AIData\\Gen" .. pool.generation .. "\\Spec" .. pool.currentSpecies .. "Genom" .. pool.currentGenome .. ".csv", "Frame,Game Mode ID,Level Idx,H Scrn,V Scrn,X Pos,Y Pos,X Speed,Y Speed,Powerup ID, Lives\n")
end

os.execute("mkdir AIData\\")

while true do
	if pool ~= nil then 
		console.writeline("Saving Config File\n")
		writeConfigFile()
	else
		console.writeline("Reading Config File\n")
		readConfigFile()
	end
	
	CURRENTRUN = CURRENTRUN + 1
--	os.execute("mkdir PastRuns\\Run" .. CURRENTRUN)
	os.execute("move AIData\\* PastRuns\\Run" .. CURRENTRUN)
--	os.execute("mkdir AIData\\Gen0") 
	createNewCSV("AIData\\FinalStats" .. CURRENTRUN .. ".csv",
	"TimeStamp,Generations,Species,Genome,Fitness,Game Mode ID,Current Level Index,Horizontal Screen,"
	.. "Vertical Screen,X Position,Y Position,Score Change,Died\n");
	
	console.writeline("Constants Initialized - Pool Initializing")
	
	initializePool()	
	
	console.writeline("Writing First Network File")
	
	writeNeuralNetworkFile("AIData\\Gen0backup.pool")
	buildForm()
	initializeBaseVariables()
	console.writeline("Base Variables initialized")

	while pool.generation < GENERATIONSPERTEST do
		
		local species = pool.species[pool.currentSpecies]
		local genome = species.genomes[pool.currentGenome]
		
		--if pool.currentFrame % 4 == 0 then --can't merge this, other functions call evalCurrent()
		evaluateCurrent(pool.currentFrame % 4 == 0) --else evaluateCurrent(false) 
		timeout = timeout - 1

		joypad.set(controller)
		lastMarioX = marioX
		lastMarioY = marioY
		lastScore = marioScore

		lastRoomID = CurrentRoomID
		getPositions()
		
		if marioX == lastMarioX and lastScore == marioScore and lastMarioY == marioY and game_mode ~= SMW.game_mode_overworld then
			framesStandingStill = framesStandingStill + 1
			if pool.currentFrame % 3 == 0 then 
				timeout = timeout - math.ceil(STANDSTILLPENALTY * framesStandingStill) 
			end --the timeout evaluates so fast mario doesn't change positions
			--IF NOT STANDING STILL, and all this other stuff
		else if (give_fitBonus and level_exit_byte ~= 128) and (hScreenCurrent ~= lasthScreenCurrent or CurrentRoomID ~= lastRoomID or lastvScreenCurrent ~= vScreenCurrent) then 
			fitnessBonus = fitnessBonus + 40
			timeout = TIMEOUTCONST
			give_fitBonus = false
			-- if we go to the overworld without death
		else if game_mode == SMW.game_mode_overworld and lastGameMode ~= SMW.game_mode_overworld and not died then
			fitnessBonus = fitnessBonus + 450
			timeout = TIMEOUTCONST * 2
			console.writeline("Game mode is overworld, last is level, and didn't die.")
			lastGameMode = game_mode
			-- if the level has hit it's end
		else if lastGameMode == SMW.game_mode_overworld and game_mode ~= SMW.game_mode_overworld then
			timeout = TIMEOUTCONST * 3
			fitnessBonus = fitnessBonus + 450
			lastGameMode = game_mode
			console.writeline("In a level, when last in overworld")
		else if End_Level_Timer ~= 0 and level_exit_byte ~= 128 then
				--fitnessBonus = fitnessBonus + 1
				timeout = TIMEOUTCONST
		else if game_mode == SMW.game_mode_overworld and lastGameMode == SMW.game_mode_overworld then timeout = timeout - 6	end
						end
					end
				end
			end
		end
		--console.writeline("Timeout test: " .. timeout)
		if timeout == TIMEOUTCONST then framesStandingStill = 0 end

		if level_exit_byte ~= 128 then timeoutBonus = math.floor(pool.currentFrame * .1) else timeoutBonus = 0 end 
		
		if marioX > rightmost then rightmost = marioX end
		if marioY < topmost then topmost = marioY end
		if marioScore > lastScore and marioScore > bestScore then bestScore = marioScore end
		
		local anim_trig = u8(WRAM.animation_trigger) -- player animation trigger ram address 0x71 
		
		if ai_failed_flag == 0x0 and level_exit_byte ~= 0x00 and level_exit_byte ~= 128 then
			pool.currentFrame = 0 --reset 
			timeout = TIMEOUTCONST
			if level_exit_byte == 0xE0 then	fitnessBonus = fitnessBonus * 1.1
			else if level_exit_byte == 0x01 or level_exit_byte == 0x02 then fitnessBonus = fitnessBonus * 1.2
			else if level_exit_byte == 128 then fitnessBonus = 0 timeout = 0 timeoutBonus = 0 
			end end end
			timeoutBonus = 0
			else if ai_failed_flag ~= 0x0 then timeout = 5 fitnessBonus = 0 timeoutBonus = 0 end
		end
		
		--console.writeline("timeout: " .. timeout .. " timeoutBonus: " .. timeoutBonus)
		if timeout + timeoutBonus <= 0 then 
		
		fitness = fitnessBonus + math.floor((bestScore * .10) + (totalSeen / (pool.currentFrame * .06)))
		if fitness == nil then console.writeline("Fitness is null for some reason....") end
		
			if fitness > pool.maxFitness then pool.maxFitness = fitness
				forms.settext(maxFitnessLabel, "Max Fitness: " .. math.floor(pool.maxFitness))
			end

			genome.fitness = fitness
			genome.FinalStats.X = math.floor(marioX)
			genome.FinalStats.Y = math.floor(marioY)
			genome.FinalStats.Score = marioScore
			genome.FinalStats.Died = died
			
			appendToCSV("AIData\\FinalStats" .. CURRENTRUN .. ".csv", "" .. os.time() .. "," .. pool.generation .. "," .. pool.currentSpecies .. "," .. pool.currentGenome .. "," .. 
			fitness .. "," .. game_mode .. "," .. Current_Level_Index .. "," .. hScreenCurrent .. "," .. vScreenCurrCount .. "," .. 
			marioX .. "," .. marioY .. "," .. marioScore - baseScore .. "," .. tostring(died) .. "\n")
			
			pool.currentSpecies = 1
			pool.currentGenome = 1
			
			while fitnessAlreadyMeasured() do nextGenome() end
			initializeRun()
		end
			
		if forms.ischecked(hideBanner) then
			gui.drawBox(0, 0, 300, 30, 0xD0FFFFFF, 0xD0FFFFFF)
			gui.drawText(0, 0, string.format("Level Value: %x, Timeout: %i", level_exit_byte, timeout + timeoutBonus), 0xFF000000, 11) 
			gui.drawText(0, 12, "Fitness: " ..  math.floor(rightmost + fitnessBonus + math.floor(bestScore * .10)), 0xFF000000, 11)
			gui.drawText(100, 12, "Max Fitness: " .. pool.maxFitness, 0xFF000000, 11)
		end
		pool.currentFrame = math.floor(pool.currentFrame) + 1
		emu.frameadvance();
		end
	end