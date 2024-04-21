-- Original Software Automatic Mouth (c) Don't Ask Software
--
-- Rewrite of SAM.C into Lua
--
-- (c) 2019 David Cuny
--

local PITCH_SCALE = 1/24

-- which synthesis method?
-- format synthesis is smoother, but not as clear
local formantSynthesisFlag = true

require "writeWaveFile"

-- FIXES
-- Reciter rule for "%" was missing test for "E"
-- Reciter cleans input before parsing
-- Reciter wasn't handling text that ended without padding
-- Reciter was missing "O" in the vowel test
-- Reciter has incorrect tests for the % rule
-- Interpolation didn't check that start began after the beginning
-- 1/14
-- Error with "J" - only has two phonemes. What's the missing phoneme?
-- Corrected error in extendPhonemeParts
-- Added f4 and f5 to formant synthesis
-- 1/16
-- Better estimation of bandwidths
-- TODO: Nonlinear interpolation
-- Voiced fricatives


-- The phonemes
--local phonemeText = " SAHMTHIHNXKX, AOR MEY5BIY4 NAAT. "
-- local phonemeText = "  TWAHZ BRIHLIHG AEND DHAX SLIYTHIY TAH4VZ, DIHD GAYR AEND GIHMBUL IHN DHAX WEYB. AHL MIHMSIY WER  DHAX BAOROW GROWVZ, AEND DHAX MOW5M RAETHS AWTGREYB. "
-- phonemeText = " DHIHS IHZ AH TEHST. DHIHS IHZ OW4NLIY AH TEHST. "
-- phonemeText = " FAORSKAOR AHND SEHVAHN YIHRZ AHGOW, AWER FAORFAADHERZ BRAOT FAORDH AXPAON DHIHS KAANTIHNEHNT AH NUW4 NEY5SHUN, KAANSIY4VD IHN LIH5BERTIY, AEND DEHDAHKEYTAHD TUX DHAX PRAH5PAAZIHSHUN DHAET AOL MEHN AAR KRIYEYTIXD IY4KWUL. NAW2 WIY2 AA2R IH1NGEY2CHD IH1N AH1 GREY2T SIH2VAH1L WAO2R, TEH2STIH1NG WEH2DHER1 DHAE2T NEYSHAHN, AO2R EH2NIY1 NEYSHAHN SOW2 KAH1NSIY2VD, AH1ND SOW2 DEH2DAH1KEY1TAH1D, KAE2N LAO2NG EH1NDYUH2R. "
local inText = "Hello, my name is SAM. I am a computer synthesizer. I automatically convert plain text into phonemes, and can deal with long strings of text. Obviously, this is old school technology. But what do you expect from less than two thousand lines of code?"

-- local inText = " Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal. Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battlefield of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that, that nation might live. It is altogether fitting and proper that we should do this. But, in a larger sense, we can not dedicate - we can not consecrate - we can not hallow - this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us - that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion, that we here highly resolve, that these dead shall not have died in vain, that this nation, under God, shall have a new birth of freedom - and that government of the people, by the people, for the people, shall not perish from the earth."


-- local inText = " Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal. Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battlefield of that war. "





-- Audio settings
local SAMPLE_RATE = 44100
local TWO_PI = 2 * math.pi
local MINUS_PI_T = -math.pi / SAMPLE_RATE
local TWO_PI_T = TWO_PI /SAMPLE_RATE

-- Speech settings
local globalPitch = 128 -- Not sure what this in actual frequency...
local speed = .01 * 1.3
local SAMPLES_PER_FRAME = SAMPLE_RATE * speed

local LINE_TERMINATOR = 255   -- end of buffer
local RENDER_BREAK = 254      -- silence for render buffer
local MAX_FRAMES = 100000 -- 232   -- maximum number of frames per breath to render


-- flags, should be set at command line
local debugFlag = true
local phoneticFlag = false

local smoothFlag = true
local contourRatio = 5 -- larger number means smaller pitch movement

-- globals
local mouth = 128 -- FIXME
local throat = 128  -- FIXME
local singMode = false


-- Holds wave file data
local outBuffer = {}

-- print **s** if **debugFlag** is **true**
local function debugPrint(s)
	if debugFlag then print(s) end
end


-----------------------------------------------
-- RECITER: Rewrite English text to phonemes --
-----------------------------------------------

-- Rules:
-- ' '    space or punctuation
--  #     One or more vowels
--  .     Voiced consonant: B, D, V, G, J, L, M, N, R, W or Z
--  &     CH or SH
--  @     TH, CH, or SH
--  ^     One consonant
--  +     A front vowel: E, I or Y
--  :     Zero or more vowels
--  %     ER, ES, ED, LY, FUL, ING (a suffix)

-- The rules
local LEFT_RULE, MID_RULE, RIGHT_RULE, OUT_RULE = {}, {}, {}, {}

-- Split a pattern in the form of "XXX(YYY)ZZZ" into "XXX", "YYY", "ZZZ"
-- Add to the rules tables
local function addRule( inRule, outRule )

	-- find '('
	local leftParenAt = string.find( inRule, "(", 1, true )
	if not leftParenAt then
		error("Missing '(' in rule ["..inRule.."].")
	end

	-- find ')'
	local rightParenAt = string.find( inRule, ")", leftParenAt+1, true )
	if not rightParenAt then
		error("Missing '(' in rule ["..inRule.."].")
	end

	-- insert into table
	table.insert( LEFT_RULE, string.sub(inRule, 1, leftParenAt-1 ) )
	table.insert( MID_RULE, string.sub(inRule, leftParenAt+1, rightParenAt-1) )
	table.insert( RIGHT_RULE, string.sub(inRule, rightParenAt+1) )
	table.insert( OUT_RULE, outRule )

end


-- Add rules into the tables
local function addRules( rules )
	for i = 1, #rules, 2 do
		addRule( rules[i], rules[i+1])
	end
end

addRules{
	" (LIBERTY) ",      "LIHBERTIY",
	" (PROPOSITION) ",  "PRAAPAHZIHSHAHN",
	" (CIVIL) ",        "SIHVAHL",
	" (BATTLEFIELD) ",  "BAETAHLFIYLD",
	" (LIVING) ",       "LIHVIHNG",
	" (FINAL) ",        "FAYNAHL",
	" (DEDICATE) ",     "DEHDAHKEYT",
	" (PORTION) ",      "PAORSHAHN",
	" (LIVE) ",         "LIHV",
	" (ALTOGETHER) ",   "AOLTAHGEHDHER",
	" (PROPER) ",       "PRAAPER",
	" (DEDICATE) ",     "DEHDAHKEYT",
	" (CONSECRATE) ",   "KAANSAHKREYT",
	" (HALLOW) ",       "/HAELOW",
	" (NOTE) ",         "NOWT",     -- This rule should be working!
	" (UNFINISHED) ",   "AHNFIHNIHSHT",
	" (HONORED) ",      "AANERD",
	" (INCREASED) ",    "IHNKRIYST",
	" (CAUSE) ",        "KAAZ",

	"( )",          " ",
	"(.)",          ".",
	-- A
	" (A.)",        "EH4Y. ",
	"(A) ",         "AH",
	" (ARE) ",      "AAR",
	" (AR)O",       "AXR",
	"(AR)#",        "EH4R",
	" ^(AS)#",      "EY4S",
	"(A)WA",        "AX",
	"(AW)",         "AO5",
	" :(ANY)",      "EH4NIY",
	"(A)^+#",       "EY5",
	"#:(ALLY)",     "ULIY",
	" (AL)#",       "UL",
	"(AGAIN)",      "AXGEH4N",
	"#:(AG)E",      "IHJ",
	"(A)^%",        "EY",
	"(A)^+:#",      "AE",
	" :(A)^+ ",     "EY4",
	" (ARR)",       "AXR",
	"(ARR)",        "AE4R",
	" ^(AR) ",      "AA5R",
	"(AR)",         "AA5R",
	"(AIR)",        "EH4R",
	"(AI)",         "EY4",
	"(AY)",         "EY5",
	"(AU)",         "AO4",
	"#:(AL) ",      "UL",
	"#:(ALS) ",     "ULZ",
	"(ALK)",        "AO4K",
	"(AL)^",        "AOL",
	" :(ABLE)",     "EY4BUL",
	"(ABLE)",       "AXBUL",
	"(A)VO",        "EY4",
	"(ANG)+",       "EY4NJ",
	"(ATARI)",      "AHTAA4RIY",
	"(A)TOM",       "AE",
	"(A)TTI",       "AE",
	" (AT) ",       "AET",
	" (A)T",        "AH",
	"(A)",          "AE",

	-- B
	" (B) ",        "BIY4",
	" (BE)^#",      "BIH",
	"(BEING)",      "BIY4IHNX",
	" (BOTH) ",     "BOW4TH",
	" (BUS)#",      "BIH4Z",
	"(BREAK)",      "BREY5K",
	"(BUIL)",       "BIH4L",
	"(B)",          "B",

	-- C
	" (C) ",        "SIY4",
	" (CH)^",       "K",
	"^E(CH)",       "K",
	"(CHA)R#",      "KEH5",
	"(CH)",         "CH",
	" S(CI)#",      "SAY4",
	"(CI)A",        "SH",
	"(CI)O",        "SH",
	"(CI)EN",       "SH",
	"(CITY)",       "SIHTIY",
	"(C)+",         "S",
	"(CK)",         "K",
	"(COMMODORE)",  "KAA4MAHDOHR",
	"(COM)",        "KAHM",
	"(CUIT)",       "KIHT",
	"(CREA)",       "KRIYEY",
	"(C)",          "K",

	-- D
	" (D) ",        "DIY4",
	" (DR.) ",      "DAA4KTER",
	"#:(DED) ",     "DIHD",
	".E(D) ",       "D",
	"#:^E(D) ",     "T",
	" (DE)^#",      "DIH",
	" (DO) ",       "DUW",
	" (DOES)",      "DAHZ",
	"(DONE) ",      "DAH5N",
	"(DOING)",      "DUW4IHNX",
	" (DOW)",       "DAW",
	"#(DU)A",       "JUW",
	"#(DU)^#",      "JAX",
	"(D)",          "D",

	-- E
	" (E) ",        "IYIY4",
	"#:(E) ",      "",
	"':^(E) ",     "",
	" :(E) ",       "IY",
	"#(ED) ",       "D",
	"#:(E)D ",      "",
	"(EV)ER",       "EH4V",
	"(E)^%",        "IY4",
	"(ERI)#",       "IY4RIY",
	"(ERI)",        "EH4RIH",
	"#:(ER)#",      "ER",
	"(ERROR)",      "EH4ROHR",
	"(ERASE)",      "IHREY5S",
	"(ER)#",        "EHR",
	"(ER)",         "ER",
	" (EVEN)",      "IYVEHN",
	"#:(E)W",       "",
	"@(EW)",        "UW",
	"(EW)",         "YUW",
	"(E)O",         "IY",
	"#:&(ES) ",     "IHZ",
	"#:(E)S ",      "",
	"#:(ELY) ",     "LIY",
	"#:(EMENT)",    "MEHNT",
	"(EFUL)",       "FUHL",
	"(EE)",         "IY4",
	"(EARN)",       "ER5N",
	" (EAR)^",      "ER5",
	"(EAD)",        "EHD",
	"#:(EA) ",      "IYAX",
	"(EA)SU",       "EH5",
	"(EA)",         "IY5",
	"(EIGH)",       "EY4",
	"(EI)",         "IY4",
	" (EYE)",       "AY4",
	"(EY)",         "IY",
	"(EU)",         "YUW5",
	"(EQUAL)",      "IY4KWUL",
	"(E)",          "EH",

	-- F
	" (F) ",        "EH4F",
	"(FUL)",        "FUHL",
	"(FRIEND)",     "FREH5ND",
	"(FATHER)",     "FAA4DHER",
	"(F)F",         "",
	"(F)",           "F",

	-- G
	" (G) ",         "JIY4",
	"(GIV)",         "GIH5V",
	" (G)I^",        "G",
	"(GE)T",         "GEH5",
	"SU(GGES)",      "GJEH4S",
	"(GG)",          "G",
	" B#(G)",        "G",
	"(G)+",          "J",
	"(GREAT)",       "GREY4T",
	"(GON)E",        "GAO5N",
	"#(GH)",          "",
	" (GN)",          "N",
	"(G)",            "G",

	-- H
	" (H) ",          "EY4CH",
	" (HAV)",         "/HAE6V",
	" (HERE)",        "/HIYR",
	" (HOUR)",        "AW5ER",
	"(HOW)",          "/HAW",
	"(H)#",           "/H",
	"(H)",            "",

	-- I
	" (IN)",          "IHN",
	" (I) ",          "AY4",
	"(I) ",           "AY",
	"(IN)D",          "AY5N",
	"SEM(I)",         "IY",
	" ANT(I)",        "AY",
	"(IER)",          "IYER",
	"#:R(IED) ",      "IYD",
	"(IED) ",         "AY5D",
	"(IEN)",          "IYEHN",
	"(IE)T",          "AY4EH",
	"(I')",           "AY5",
	" :(I)^%",        "AY5",
	" :(IE) ",        "AY4",
	"(I)%",           "IY",
	"(IE)",           "IY4",
	" (IDEA)",        "AYDIY5AH",
	"(I)^+:#",        "IH",
	"(IR)#",          "AYR",
	"(IZ)%",          "AYZ",
	"(IS)%",          "AYZ",
	"I^(I)^#",        "IH",
	"+^(I)^+",        "AY",
	"#:^(I)^+",       "IH",
	"(I)^+",          "AY",
	"(IR)",           "ER",
	"(IGH)",          "AY4",
	"(ILD)",          "AY5LD",
	" (IGN)",         "IHGN",
	"(IGN) ",         "AY4N",
	"(IGN)^",         "AY4N",
	"(IGN)%",         "AY4N",
	"(ICRO)",         "AY4KROH",
	"(IQUE)",         "IY4K",
	"(I)",            "IH",

	-- J
	" (J) ",          "JEY4",
	"(J)",            "J",

	-- K
	" (K) ",          "KEY4",
	" (K)N",          "",
	"(K)",            "K",

	-- L
	" (L) ",          "EH4L",
	"(LO)C#",         "LOW",
	"L(L)",           "",
	"#:^(L)%",        "UL",
	"(LEAD)",         "LIYD",
	" (LAUGH)",        "LAE4F",
	"(L)",            "L",

	-- M
	" (M) ",          "EH4M",
	" (MR.) ",        "MIH4STER",
	" (MS.)",         "MIH5Z",
	" (MRS.) ",       "MIH4SIXZ",
	"(MOV)",          "MUW4V",
	"(MACHIN)",       "MAHSHIY5N",
	"M(M)",           "",
	"(M)",            "M",

	-- N
	" (N) ",          "EH4N",
	"E(NG)+",         "NJ",
	"(NG)R",          "NXG",
	"(NG)#",          "NXG",
	"(NGL)%",         "NXGUL",
	"(NG)",           "NX",
	"(NK)",           "NXK",
	" (NOW) ",        "NAW4",
	"N(N)",           "",
	"(NON)E",         "NAH4N",
	"(N)",            "N",

	-- O
	" (O) ",          "OH4W",
	"(OF) ",          "AHV",
	" (OH) ",         "OW5",
	"(OROUGH)",       "ER4OW",
	"#:(OR) ",        "ER",
	"#:(ORS) ",       "ERZ",
	"(OR)",           "AOR",
	" (ONE)",         "WAHN",
	"#(ONE) ",        "WAHN",
	"(OW)",           "OW",
	" (OVER)",        "OW5VER",
	"PR(O)V",         "UW4",
	"(OV)",           "AH4V",
	"(O)^%",          "OW5",
	"(O)^EN",         "OW",
	"(O)^I#",         "OW5",
	"(OL)D",          "OW4L",
	"(OUGHT)",        "AO5T",
	"(OUGH)",         "AH5F",
	" (OU)",          "AW",
	"H(OU)S#",        "AW4",
	"(OUS)",          "AXS",
	"(OUR)",          "OHR",
	"(OULD)",         "UH5D",
	"(OU)^L",         "AH5",
	"(OUP)",          "UW5P",
	"(OU)",           "AW",
	"(OY)",           "OY",
	"(OING)",         "OW4IHNX",
	"(OI)",           "OY5",
	"(OOR)",          "OH5R",
	"(OOK)",          "UH5K",
	"F(OOD)",         "UW5D",
	"L(OOD)",         "AH5D",
	"M(OOD)",         "UW5D",
	"(OOD)",          "UH5D",
	"F(OOT)",         "UH5T",
	"(OO)",           "UW5",
	"(O')",          "OH",
	"(O)E",           "OW",
	"(O) ",           "OW",
	"(OA)",           "OW4",
	" (ONLY)",        "OW4NLIY",
	" (ONCE)",        "WAH4NS",
	"(ON'T)",        "OW4NT",
	"C(O)N",          "AA",
	"(O)NG",          "AO",
	" :^(O)N",        "AH",
	"I(ON)",          "UN",
	"#:(ON)",         "UN",
	"#^(ON)",         "UN",
	"(O)ST",          "OW",
	"(OF)^",          "AO4F",
	"(OTHER)",        "AH5DHER",
	"R(O)B",          "RAA",
	"^R(O):#",        "OW5",
	"(OSS) ",         "AO5S",
	"#:^(OM)",        "AHM",
	"(O)",            "AA",

	-- P
	" (P) ",          "PIY4",
	"(PH)",           "F",
	"(PEOPL)",        "PIY5PUL",
	"(POW)",          "PAW4",
	"(PUT) ",         "PUHT",
	"(P)P",           "",
	"(P)S",           "",
	"(P)N",           "",
	"(PROF.)",        "PROHFEH4SER",
	"(P)",            "P",

	-- Q
	" (Q) ",          "KYUW4",
	"(QUAR)",         "KWOH5R",
	"(QU)",           "KW",
	"(Q)",            "K",

	-- R
	" (R) ",          "AA5R",
	" (RE)^#",        "RIY",
	"(R)R",           "",
	"(R)",            "R",

	-- S
	" (S) ",          "EH4S",
	"(SH)",           "SH",
	"#(SION)",        "ZHUN",
	"(SOME)",         "SAHM",
	"#(SUR)#",        "ZHER",
	"(SUR)#",         "SHER",
	"#(SU)#",         "ZHUW",
	"#(SSU)#",        "SHUW",
	"#(SED)",         "ZD",
	"#(S)#",          "Z",
	"(SAID)",         "SEHD",
	"^(SION)",        "SHUN",
	"(S)S",           "",
	".(S) ",          "Z",
	"#:.E(S) ",       "Z",
	"#:^#(S) ",       "S",
	"U(S) ",          "S",
	" :#(S) ",        "Z",
	"##(S) ",         "Z",
	" (SCH)",         "SK",
	"(S)C+",          "",
	"#(SM)",          "ZUM",
	"#(SN)'",         "ZUM",
	"(STLE)",         "SUL",
	"(S)",            "S",

	-- T
	" (T) ",          "TIY4",
	" (THE) #",       "DHIY",
	" (THE) ",        "DHAX",
	"(TO) ",          "TUX",
	" (THAT)",        "DHAET",
	" (THIS) ",       "DHIHS",
	" (THEY)",        "DHEY",
	" (THERE)",       "DHEHR",
	"(THER)",         "DHER",
	"(THEIR)",        "DHEHR",
	" (THAN) ",       "DHAEN",
	" (THEM) ",       "DHAEN",
	"(THESE) ",       "DHIYZ",
	" (THEN)",        "DHEHN",
	"(THROUGH)",      "THRUW4",
	"(THOSE)",        "DHOHZ",
	"(THOUGH) ",      "DHOW",
	"(TODAY)",        "TUXDEY",
	"(TOMO)RROW",     "TUMAA5",
	"(TO)TAL",        "TOW5",
	" (THUS)",        "DHAH4S",
	"(TH)",           "TH",
	"#:(TED)",        "TIXD",
	"S(TI)#N",        "CH",
	"(TI)O",          "SH",
	"(TI)A",          "SH",
	"(TIEN)",         "SHUN",
	"(TUR)#",         "CHER",
	"(TU)A",          "CHUW",
	" (TWO)",         "TUW",
	"&(T)EN ",        "",
	"(T)",            "T",

	-- U
	" (U) ",          "YUW4",
	" (UN)I",         "YUWN",
	" (UN)",          "AHN",
	" (UPON)",        "AXPAON",
	"@(UR)#",         "UH4R",
	"(UR)#",          "YUH4R",
	"(UR)",           "ER",
	"(U)^ ",          "AH",
	"(U)^^",          "AH5",
	"(UY)",           "AY5",
	" G(U)#",         "",
	"G(U)%",          "",
	"G(U)#",          "W",
	"#N(U)",          "YUW",
	"@(U)",           "UW",
	"(U)",            "YUW",

	-- V
	" (V) ",          "VIY4",
	"(VIEW)",         "VYUW5",
	"(V)",            "V",

	-- W
	" (W) ",          "DAH4BULYUW",
	" (WERE)",        "WER",
	"(WA)SH",         "WAA",
	"(WA)ST",         "WEY",
	"(WA)S",          "WAH",
	"(WA)T",          "WAA",
	"(WHERE)",        "WHEHR",
	"(WHAT)",         "WHAHT",
	"(WHOL)",         "/HOWL",
	"(WHO)",          "/HUW",
	"(WH)",           "WH",
	"(WAR)#",         "WEHR",
	"(WAR)",          "WAOR",
	"(WOR)^",         "WER",
	"(WR)",           "R",
	"(WOM)A",         "WUHM",
	"(WOM)E",         "WIHM",
	"(WEA)R",         "WEH",
	"(WANT)",         "WAA5NT",
	"ANS(WER)",       "ER",
	"(W)",            "W",

	-- X
	" (X) ",          "EH4KR",
	" (X)",           "Z",
	"(X)",            "KS",

	-- Y
	" (Y) ",          "WAY4",
	"(YOUNG)",        "YAHNX",
	" (YOUR)",        "YOHR",
	" (YOU)",         "YUW",
	" (YES)",         "YEHS",
	" (Y)",           "Y",
	"F(Y)",           "AY",
	"PS(YCH)",        "AYK",
	"#:^(Y)",         "IY",
	"#:^(Y)I",        "IY",
	" :(Y) ",         "AY",
	" :(Y)#",         "AY",
	" :(Y)^+:#",      "IH",
	" :(Y)^#",        "AY",
	"(Y)",            "IH",

	-- Z
	" (Z) ",          "ZIY4",
	"(Z)",            "Z",

	-- Punctuation
	"(!)",            ".",
	--"(\")",           "-AH5NKWOWT-",
	--"(")",            "KWOW4T-",
	"(#)",            " NAH4MBER",
	"($)",            " DAA4LER",
	"(%)",            " PERSEH4NT",
	"(&)",            " AEND",
	"(\')",           "",
	"(*)",            " AE4STERIHSK",
	"(+)",            " PLAH4S",
	"(,)",            ",",
	" (-) ",          "-",
	"(-)",            "",
	"(.)",            " POYNT",
	"(/)",            " SLAE4SH",
	"(0)",            " ZIY4ROW",
	" (1ST)",         "FER4ST",
	" (10TH)",        "TEH4NTH",
	"(1)",            " WAH4N",
	" (2ND)",         "SEH4KUND",
	"(2)",            " TUW4",
	" (3RD)",         "THER4D",
	"(3)",            " THRIY4",
	"(4)",            " FOH4R",
	" (5TH)",         "FIH4FTH",
	"(5)",            " FAY4V",
	" (64) ",         "SIH4KSTIY FOHR",
	"(6)",            " SIH4KS",
	"(7)",            " SEH4VUN",
	" (8TH)",         "EY4TH",
	"(8)",            " EY4T",
	"(9)",            " NAY4N",
	"(:)",            ".",
	"(;)",            ".",
	"(<)",            " LEH4S DHAEN",
	"(=)",            " IY4KWULZ",
	"(>)",            " GREY4TER DHAEN",
	"(?)",            "?",
	"(@)",            " AE6T",
	"(^)",            " KAE4RIXT"
}



-- Returns end position if string at **a** matches **pattern**, otherwise returns nil
local function matchAt( s, at, pattern )
	local len = string.len(pattern)
	if string.sub( s, at, at+len-1 ) == pattern then
		-- return end position
		return at+len
	else
		-- return nil
		return nil
	end
end


local function isPunctuationMark(c)
	return c == "."
		or c == "!"
		or c == "?"
		or c == ","
		or c == "("
		or c == ")"
		or c == "-"
		or c == ";"

end


-- return true if character is A, E, I, O or U
local function isVowelChar(c)
	return
		c == "A" or c == "E" or c == "I" or c == "O" or c == "U"
end


-- return true if character is alpha and not a vowel
local function isConsonantChar(c)
	-- not vowel
	if isVowelChar(c) then
		return false
	end

	-- alphanumeric
	return c >= "A" and c <= "Z"
end


-- Bad rule encountered. Throw error
local function badRule( ruleIndex, char )
	error("Unknown character '"..char.."' in rule #"..ruleIndex..", ["..LEFT_RULE[ruleIndex].."("..MID_RULE[ruleIndex]..")"..RIGHT_RULE[ruleIndex].."]")
end


-- return true if text at position matches rightRule
local function rightMatch(text, at, ruleIndex)

	-- get the rule
	local rightRule = RIGHT_RULE[ruleIndex]

	-- no pattern to match?
	if string.len(rightRule) == 0 then
		return true
	end

	-- move past mid portion
	at = at + string.len(MID_RULE[ruleIndex])

	-- move forward through the rule (left to right)
	for i = 1, string.len(rightRule) do

		-- get a character from the rule
		local rule = string.sub(rightRule,i,i)

		-- get a character from the current position
		local c = string.sub(text,at,at)
		if c == "" then c = " " end

		-- space?
		if rule == " " then
			if c ~= " " and not isPunctuationMark(c) then
				return false
			end

			at = at + 1

			-- alphanumeric
		elseif (rule >= "A" and rule <= "Z") then
			-- return false if doesn't match exactly
			if rule ~= c then
				return false
			end

			-- advance to next character in rule
			at = at + 1

		elseif rule == '#' then

			-- one or more vowels

			-- fail if current character is not a vowel
			if not isVowelChar(c) then
				return false
			end

			-- proceed until not on a vowel
			while at < string.len(text) and isVowelChar(string.sub(text,at,at)) do
				-- move past vowel
				at = at + 1
			end


		elseif rule == ':' then

			-- zero or more consonants
			while at < string.len(text) and isConsonantChar(string.sub(text,at,at)) do
				-- move past consonant
				at = at + 1
			end


		elseif rule == '^' then
			-- one consonant
			if not isConsonantChar(c) then
				return false
			end

			-- advance past the consonant
			at = at + 1

		elseif rule == '%' then
			-- ER, ES, ED, LY, FUL, ING (a suffix)
			local newAt = matchAt( text, at, "ER" )
				or matchAt( text, at, "E" )
				or matchAt( text, at, "ES" )
				or matchAt( text, at, "ED" )
				or matchAt( text, at, "LY" )
				or matchAt( text, at, "FUL" )
				or matchAt( text, at, "ING" )

			if newAt then
				at = newAt
			else
				return false
			end


		elseif rule == '.' then
			-- One of B, D, V, G, J, L, M, N, R, W or Z (voiced consonants)
			if c ~= "B"
				and c ~= "D"
				and c ~= "V"
				and c ~= "G"
				and c ~= "J"
				and c ~= "L"
				and c ~= "M"
				and c ~= "N"
				and c ~= "R"
				and c ~= "W"
				and c ~= "Z" then
				-- no match
				return false
			end

			-- move past
			at = at + 1

		elseif rule == "+" then
			-- One of E, I or Y (a "front" vowel)
			if c ~= "E" and c ~= "I" and c ~= "Y" then
				-- no match, fail
				return false
			end

			-- move past vowel
			at = at + 1

		else
			-- fatal error - bad character in rule
			badRule( ruleIndex, rule )

		end

	end

	-- no tests failed
	return true

end

-- Returns **true** if the _prior_ characters match the rule. This rule marches **backwards**
-- because it doesn't know how many characters it's going to match.
local function leftMatch(text, at, ruleIndex)

	-- get the left match rule
	local leftRule = LEFT_RULE[ruleIndex]

	-- nothing to match?
	if string.len(leftRule) == 0 then
		return true
	end

	-- move prior to current character
	at = at - 1

	-- move backwards through the rule (right to left)
	for i = string.len(leftRule), 1, -1 do

		-- get the rule from the left match
		local rule = string.sub(leftRule,i,i)

		-- current character in text
		local c = string.sub(text,at,at)
		if c == "" then c = " " end

		-- space?
		if rule == " " then
			if c ~= " " and not isPunctuationMark(c) then
				return false
			end

			-- move ahead (going backwards)
			at = at - 1

		elseif (rule >= "A" and rule <= "Z") or rule == "'" then
			-- exact match required
			if c ~= rule then
				return false
			end

			-- move past matching character (to the left)
			at = at - 1

		elseif rule == '@' then
			-- 'TH', 'CH', 'SH'
			if not matchAt( text, at-1, "TH" )
				and not matchAt( text, at-1, "CH" )
				and not matchAt( text,  at-1, "SH" ) then
				return false
			end

			-- success
			at = at - 1

		elseif rule == '&' then
			-- 'CH', 'SH'
			if not matchAt( text, at-1, "CH" )
				and not matchAt( text,  at-1, "SH" ) then
				return false
			end

			-- success
			at = at - 1

		elseif rule == '#' then
			-- one or more vowels

			-- fail if not a vowel
			if not isVowelChar(c) then
				return false
			end

			-- eat required vowel, and any optional vowels
			while at >= 1 and isVowelChar(string.sub(text,at,at)) do
				-- move past (to the left)
				at = at - 1
			end

		elseif rule == ':' then
			-- zero or more consonants
			while at >= 1 and isConsonantChar(string.sub(text,at,at)) do
				-- move past (to the left)
				at = at - 1
			end

		elseif rule == '^' then
			-- one consonant
			if not isConsonantChar(c) then
				-- no match
				return false
			end

			-- move past (to the left)
			at =  at - 1

		elseif rule == '.' then
			-- One of B, D, V, G, J, L, M, N, R, W or Z (voiced consonants)
			if c ~= "B"
				and c ~= "D"
				and c ~= "V"
				and c ~= "G"
				and c ~= "J"
				and c ~= "L"
				and c ~= "M"
				and c ~= "N"
				and c ~= "R"
				and c ~= "W"
				and c ~= "Z" then
				-- no match, fail
				return false
			end

			-- move past (to the left)
			at = at - 1

		elseif rule == '+' then
			-- One of E, I or Y (a "front" vowel)
			if c ~= "E" and c ~= "I" and c ~= "Y" then
				-- no match, fail
				return false
			end

			-- move past (to the left)
			at = at - 1

		else
			-- fatal error - bad rule
			badRule( ruleIndex, rule )

		end
	end

	-- no tests failed
	return true

end


-- Return string in upper case, removing or replacing "bad" characters
local function cleanupText( s )
	local out = ""
	for i = 1, string.len(s) do
		-- get a character
		local c = string.sub(s, i, i)

		-- set to upper case
		c = string.upper(c)

		-- replace curly braces with square braces
		if c == "{" then
			c = "["
		elseif c == "}" then
			c = "]"
		end

		-- only accept character set between " " and "Z"
		if c >= " " and c <= "Z" then
			out = out .. c
		end
	end

	return out
end

-- convert a word to phonemes
-- word preceded by a single space, and ended with a single space
-- like: " word "
function reciter(text)

	-- convert to upper case
	text = cleanupText(text)

	-- start at beginning
	local at = 1

	-- contains result when done translating
	local outString = ""

	-- solve
	while at <= string.len(text) do
		-- flag no rule found
		local foundRule = false

		-- search all the rules for a match on the curent character
		for i = 1, #LEFT_RULE do

			if matchAt( text, at, MID_RULE[i])
				and leftMatch(text, at, i )
				and rightMatch(text, at, i ) then

				-- debuggings
				debugPrint("MATCHED RULE "..LEFT_RULE[i].."("..MID_RULE[i]..")"..RIGHT_RULE[i],OUT_RULE[i])

				-- advance past the matching portion
				at = at + string.len(MID_RULE[i])

				-- append the out portion
				outString = outString .. OUT_RULE[i]

				-- set flag and exit loop
				foundRule = true
				break
			end
		end

		-- failed?
		if at < string.len(text) and not foundRule then
			error("Unable to parse text '"..text.."] - stopped at "..string.sub(text,at))
		end

	end

	return outString

end







------------------------
-- GLOTTIS SIMULATION --
------------------------

local glottis = {
	f0              = 0,       		-- current frequency of glottis
	amp             = 1,		      -- current amplitude of glottal pulse
	at		          = 1,		      -- current position 0..1; 1 forces a restart
	pulsePercent	  = .65,	      -- percent of time there is a pulse (default was .65) <-- SMALLER IS BUZZIER
	risingPercent	  = .8,	        -- percent of the pulse that's spent rising  <-- FASTER FALL MAKES BRIGHTER SOUND
	pulseSamples    = 0,          -- number of samples pulse is rising+falling
	risingSamples   = 0,          -- number of samples pulse is rising
	fallingSamples  = 0           -- number of samples pulse is falling
}

-- Set the frequency for the next glottal pulse
glottis.set = function(frequency, amp)

	-- set frequency and amplitude
	glottis.f0 = frequency
	glottis.amp = amp

	-- calculate size of pulse in samples
	glottis.at = 0
	glottis.pulseSamples = math.floor( glottis.pulsePercent * SAMPLE_RATE / glottis.f0 )
	glottis.risingSamples = math.floor( glottis.pulseSamples * glottis.risingPercent )
	glottis.fallingSamples = glottis.pulseSamples - glottis.risingSamples

end


-- Return the next sample from the glottis
glottis.tick = function(noiseIn)

	if noiseIn == nil then
		noiseIn = 0
	end

	-- increment the position in the pulse
	glottis.at = glottis.at + 1

	-- SYNTHESIZE THE GLOTTAL PULSE

	-- calculate sample
	local sample = 0

	-- what's the position in the glottal pulse?
	if glottis.at > glottis.pulseSamples then

		-- closed glottis portion of wave
		glottis.sample = 0;

		-- reduce noise by 50%
		glottis.sample = noiseIn / 2;


	elseif glottis.at <= glottis.risingSamples then
		-- position in rising pulse, from 0..1
		local position = glottis.at/glottis.risingSamples

		-- rising portion of pulse
		sample = .5 * (1-math.cos(math.pi*position));

		-- add noise
		sample = sample + (position * noiseIn);

	else

		-- position in falling pulse, from 0..1
		local position = (glottis.at - glottis.risingSamples) / (glottis.pulseSamples-glottis.risingSamples);

		-- falling portion of pulse
		sample = math.cos(math.pi*.5*position);

	end

	return sample * glottis.amp

end







---------------------
-- FORMANT FILTERS --
---------------------


-- Formant filters (used for fricatives)


local function calculateResponse( resonator )

	if resonator.amplitude == nil then
		resonator.amplitude = 1
	end

	-- calculate resonator response
	local r = math.exp(MINUS_PI_T * resonator.bandwidth)

	resonator.c = -(r*r)
	resonator.b = r * 2 * math.cos(TWO_PI_T * resonator.frequency)
	resonator.a = 1 - resonator.b - resonator.c

end


-- Create a new formant filter
local function resonatorSet( resonator, frequency, bandwidth, amplitude )

	-- initialize
	resonator.frequency = frequency
	resonator.bandwidth = bandwidth
	resonator.amplitude = amplitude

	calculateResponse( resonator )

end

-- Create a new formant filter
local function resonatorCreate( frequency, bandwidth, amplitude )

	-- initialize
	local resonator = {}

	resonatorSet( resonator, frequency, bandwidth, amplitude )

	-- calculate the resonator response
	calculateResponse( resonator )

	-- clear the history
	resonator.out = 0
	resonator.z1 = 0
	resonator.z2 = 0

	return resonator

end

-- Run a sample through the formant filter
local function resonatorTick( resonator, input )

	-- oral resonator 6
	local out = resonator.a*input + resonator.b*resonator.z1 + resonator.c*resonator.z2
	resonator.z2, resonator.z1 = resonator.z1, out

	-- scale the output
	return out * resonator.amplitude

end

--



-- FIXME: This code doesn't work with formants anymore
local stressToInflection = { 0, 0, -30, -24, -18, -12, -6, 0, 6, 12, 18 }

-- Table of stress values
local stressValue = { ["*"]=0, ["1"]=1, ["2"]=2, ["3"]=3, ["4"]=4, ["5"]=5, ["6"]=6, ["7"]=7, ["8"]=8 }


-- holds the phoneme index, stress and length from the input text
local pIndex, pStress, pLength

-- holds the segment to be rendered
local outIndex, outStress, outLength


local function lerp(startValue, endValue, t)

	-- interpolate value
	return startValue + t * (endValue - startValue)

end


-- Given a value **x** between **x1** and **x1**, return
-- the ratio. Inverse of **lerp** function.
local function getRatio( x1, x2, x )
	if x < x1 then
		return 0
	elseif x > x2 then
		return 1
	elseif x2 == x1 then
		return 1
	else
		return (x-x1) / (x2-x1)
	end
end


-- Tables to hold phoneme data
local pData = {}
local pIndexByName = {}

-- Return index of phoneme **s** in **pName** table.
-- Throws error if phoneme is not found.
local function getPhoneIndex( s )
	local i = pIndexByName[s]
	if not i then
		error("Phoneme '"..s.."' not found.")
	end
	return i
end




---------------
-- DEBUGGING --
---------------

local function dumpFrames(f1, f2, f3, amp1, amp2, amp3, pitch)

	-- skip if debugging is not set
	if not debugFlag then return end

	print("FRAMES----------------------")
	print("i", "f1", "f2", "f3", "amp1", "amp2", "amp3", "pitch")
	for i = 1, #f1 do
		print(i, f1[i], f2[i], f3[i], amp1[i], amp2[i], amp3[i], pitch[i])
	end
end


-- print state of phoneme buffer
local function debugPhonemes()

	-- skip if debug flag not set
	if not debugFlag then return end

	print("idx","phoneme","length","stress")

	-- loop through phonemes
	for i = 1, #pIndex do
		-- get the token index
		local p = pIndex[i]

		-- convert to token
		local token = "[??]"
		if p == LINE_TERMINATOR then
			-- exit loop
			break

		elseif p == RENDER_BREAK then
			token = "[BRK]"

		elseif pData[p] then
			token = pData[p].token

			-- add part if part > 1
			if pData[p].part > 1 then token = token.."."..pData[p].part end

			-- make space visible
			if token == " " then token = "[SPC]" end

		end

		-- print the data
		print(p, token, pLength[i], pStress[i])
	end
end




------------------------------
-- RENDER PHONEMES TO AUDIO --
------------------------------



-- For debugging the output
local function printOutput(f1, f2, f3, amp1, amp2, amp3, pitch)

	-- skip if debug flag not set
	if not debugFlag then return end

	print("Frame", "f1", "f2", "f3", "amp2", "amp2", "amp3", "pitch")
	for i = 1, #f1 do
		print(i, f1[i], f2[i], f3[i], amp1[i], amp2[i], amp3[i], pitch[i])
	end
end



-- Used to rescale amplitude
local amplitudeRescale = { 0, 1, 2, 2, 2, 3, 3, 4, 4, 5, 6, 8, 9, 11, 14, 16, 0 }

-- RENDER THE PHONEMES IN THE LIST
--
-- The phoneme list is converted into sound through the steps:
--
-- 1. Copy each phoneme <length> number of times into the frames list,
--    where each frame represents 10 milliseconds of sound.
--
-- 2. Determine the transitions lengths between phonemes, and linearly
--    interpolate the values across the frames.
--
-- 3. Offset the pitches by the fundamental frequency.
--
-- 4. Render the each frame.



-- Create a rising or falling inflection 30 frames prior to
-- index X. A rising inflection is used for questions, and
-- a falling inflection is used for statements.

local function addInflection(pitch, direction, endAt)

	-- FIXME: This function needs to work with real pitches now,
	-- instead of steps

	if true then return end

	-- back up 30 frames, but not prior to frame 1
	local pos = endAt - 30
	pos = math.max( pos, 1 )

	-- get the pitch
	local thePitch = pitch[pos]

	while true do
		thePitch = thePitch + direction -- * -5

		-- set the pitch
		pitch[pos] = thePitch

		-- move ahead one frame
		pos = pos + 1

		-- exit?
		if pos >= endAt then
			return
		end
	end
end


-- Given a start and end position, interpolates integer values
-- through the tanle
local function interpolate( t, startIndex, endIndex )

	if startIndex > endIndex then
		return
	end

	-- FIXME: still a work in progress
	print("Interpolate", startIndex, endIndex, "size", #t)
	-- amount to change over the range of frames
	local delta = (t[endIndex] - t[startIndex]) / (endIndex - startIndex)
	local accum = t[startIndex]
	for i = startIndex+1, endIndex do
		accum = accum + delta
		t[i] = accum
	end

end

-- Given a position (0..1), return the value on an
-- "S" shaped cubic ease in/out curve (0..1)
local function easeInOutTanh(t)

	-- change range from 0..1 to -PI...PI
	t = math.pi * (t*2-1)

	-- get hyperbolic tangent value
	t = math.tanh(t)

	-- change range from -1..1 to 0..1
	return (t+1)/2

end


local function interpolateSmooth( t, startIndex, endIndex )
	local startValue = t[startIndex]
	local endValue = t[endIndex]
	local count = endIndex - startIndex
	for i = startIndex+1, endIndex do
		local tt = (i-1)/(count-1)
		t[i] = lerp( startValue, endValue, easeInOutTanh(tt) )
	end
end


-- Convert the data the tables **outIndex**, **outLength** and **outStress**
-- into frames that can be rendered
local function createFrames(outIndex, outLength, outStress)
	-- CREATE FRAMES
	--
	-- The length parameter in the list corresponds to the number of frames
	-- to expand the phoneme to. Each frame represents 10 milliseconds of time.
	-- So a phoneme with a length of 7 = 7 frames = 70 milliseconds duration.
	--
	-- The parameters are copied from the phoneme to the frame verbatim.

	-- print dump
	debugPhonemes()

	-- clear the frames
	local f1 = {}
	local f2 = {}
	local f3 = {}
	local amp1 = {}
	local amp2 = {}
	local amp3 = {}
	local frqF = {}
	local bwF = {}
	local ampF = {}
	local pitch = {}

	local frameIndex = 1
	local pos = 1
	while true do
		-- get the phoneme at the index
		local p = outIndex[pos]
		print(pos, outIndex[pos], outLength[pos], outStress[pos])

		-- if terminal phoneme, exit the loop
		if p == LINE_TERMINATOR then
			break
		end

		-- period or question mark changes inflection
		if p == getPhoneIndex(".") then
			-- add falling inflection
			addInflection(pitch, 1, frameIndex)
		elseif p == getPhoneIndex("?") then
			-- create rising inflection
			addInflection(pitch, -1, frameIndex)
		end

		-- get the stress inflection
		-- FIXME: This needs to be in frequency, not step
		local inflection = stressToInflection[outStress[pos]+1]

		-- get the data
		local data = pData[p]

		-- copy from the source to the frames list
		for _ = 1, outLength[pos] do
			-- copy phoneme values to frame
			f1[frameIndex] = data.f1
			f2[frameIndex] = data.f2
			f3[frameIndex] = data.f3
			amp1[frameIndex] = data.a1
			amp2[frameIndex] = data.a2
			amp3[frameIndex] = data.a3
			frqF[frameIndex] = data.fricFrq
			bwF[frameIndex] = data.fricBandwidth
			ampF[frameIndex] = data.fricAmplitude

			pitch[frameIndex] = globalPitch + inflection

			-- advance frame position
			frameIndex = frameIndex + 1

		end

		-- move to next phoneme
		pos = pos + 1

	end

	return f1, f2, f3, amp1, amp2, amp3, frqF, bwF, ampF, pitch

end



local function createTransitions(outIndex, outLength, f1, f2, f3, amp1, amp2, amp3, pitch)

	-- assume starts with space, skip start
	local pos = 1
	local phoneStart = pLength[1] + 1
	while true do

		-- get the current and following phoneme
		local pThis = outIndex[pos]
		local pNext = outIndex[pos+1]

		-- exit loop at line end token
		-- FIXME: This looks iffy...
		if pThis == LINE_TERMINATOR
			or pNext == LINE_TERMINATOR then
			break
		end

		-- get the ranking of each phoneme
		local rankThis = pData[pThis].blendRank
		local rankNext = pData[pNext].blendRank

		local inFrames, outFrames

		-- compare the rank - lower rank value is stronger
		if rankThis == rankNext then
			-- same rank, so use out blend lengths from each phoneme
			inFrames = pData[pThis].blendIn
			outFrames = pData[pNext].blendOut

		elseif rankThis > rankNext then
			-- first phoneme is stronger, so us it's blend lengths
			inFrames = pData[pThis].blendIn
			outFrames = pData[pThis].blendOut
		else
			-- second phoneme is stronger, so use it's blend lengths
			-- note the out/in are swapped
			inFrames = pData[pNext].blendOut
			outFrames = pData[pNext].blendIn
		end

		-- add length of prior phoneme
		phoneStart = phoneStart + outLength[pos]

		-- interpolate pitch from center of prior to center of this
		local midPrior = phoneStart - math.floor(outLength[pos]/2)
		local midThis = phoneStart + math.floor(outLength[pos+1]/2)

		-- clip
		midPrior = math.max( midPrior, 1 )
		midThis = math.min( midThis, #f1 )

		interpolate( pitch, midPrior, midThis)

		-- use blend size to calculate range
		local fromFrame = phoneStart - inFrames
		local toFrame = phoneStart + outFrames

		-- clip
		fromFrame = math.max( fromFrame, phoneStart - math.floor(outLength[pos]) )
		fromFrame = math.max( fromFrame, 1 )
		toFrame = math.min( midThis, phoneStart + math.floor(outLength[pos+1]) )

		-- interpolate the values over range
		interpolate( f1, fromFrame, toFrame)
		interpolate( f2, fromFrame, toFrame)
		interpolate( f3, fromFrame, toFrame)
		interpolate( amp1, fromFrame, toFrame)
		interpolate( amp2, fromFrame, toFrame)
		interpolate( amp3, fromFrame, toFrame)

		-- move ahead
		pos = pos + 1

	end

end



-- Subtracts the F1 frequency from the pitch to create a pitch contour. Without this, the output would be at a single
-- pitch level (monotone).
local function assignPitchContour(pitch, f1)

	-- don't adjust pitch if in sing mode
	if not singMode then
		-- iterate through the frames
		for i = 1, #pitch do
			-- subtract half the frequency of the formant 1.
			-- this adds variety to the voice
			-- pitch[i] = pitch[i] + (f1[i]-pitch[i])/contourRatio
			pitch[i] = pitch[i] - (f1[i]/2)
		end
	end

end


-- The values stored in the frequency table are the *step size*
-- to increment the phase, not the actual frequency. SAM's sample
-- rate is 22050, and the pre-computed sin table is 256 elements long
local function stepToFrequency( stepSize )
	if not stepSize then return nil end
	return math.floor(22050 / math.pi * stepSize / 256 )
end


local function renderFrames( f1, f2, f3, amp1, amp2, amp3, frqF, bwF, ampF, pitch )

	-- In traditional vocal synthesis, the glottal pulse drives filters, which
	-- are attenuated to the frequencies of the formants.
	--
	-- SAM generates these formants directly with sin and rectangular waves.
	-- To simulate them being driven by the glottal pulse, the waveforms are
	-- reset at the beginning of each glottal pulse.


	-- set initial values
	local phase1, phase2, phase3, phase4, phase5 = 0, 0, 0, 0, 0
	local delta1, delta2, delta3, delta4, delta5 = 0, 0, 0, 0, 0
	local a1, a2, a3, a4, a5 = 0, 0, 0, 0, 0
	local pulseWidth, samplesRemaining = 0, 0

	local f4 = 3500
	local f5 = 4500

	-- convert from step sizes to frequencies
	for frameIndex = 1, #f1 do
		f1[frameIndex] = stepToFrequency( f1[frameIndex] )
		f2[frameIndex] = stepToFrequency( f2[frameIndex] )
		f3[frameIndex] = stepToFrequency( f3[frameIndex] )
		pitch[frameIndex] = stepToFrequency( pitch[frameIndex] * PITCH_SCALE )
		print(frameIndex, pitch[frameIndex])
	end


	-- resonators
	local res1 = resonatorCreate(200,100,0)
	local res2 = resonatorCreate(400,100,0)
	local res3 = resonatorCreate(2400,100,0)
	local res4 = resonatorCreate(f4,100,0)
	local res5 = resonatorCreate(f5,100,0)

	for frameIndex = 1, #f1 do

		-- FIXME: Support for samples is removed
		-- Add synthesis for sampled consonants

		-- calculate the next rates and amplitudes
		-- local newFrame = true

		-- create frication for frame
		local fricationFilter = nil
		if frqF[frameIndex] then
			fricationFilter = resonatorCreate(frqF[frameIndex],bwF[frameIndex],ampF[frameIndex])
		end


		-- render samples for the frame
		local limit = math.floor(SAMPLES_PER_FRAME)-1
		for i = 0, limit, 1 do

			local tFrame = i/limit

			-- calculate the contribution
			local sample = math.sin(phase1) * a1 + math.sin(phase2) * a2 + math.sin(phase3) * a3
				+ math.sin(phase4) * a4 + math.sin(phase5) * a5

			-- position in the cycle
			local t = 1
			if pulseWidth > 0 then
				t = (samplesRemaining/(pulseWidth-1))
			end

			-- smooth the wave
			if smoothFlag then
				if t < .25 then
					sample = sample * math.sin(getRatio( 0, .5, t ) * math.pi)
				elseif t > .75 then
					sample = sample * math.cos(getRatio( .75, 1.25, t) * math.pi)
				end
			end

			if formantSynthesisFlag then
				local pulse = glottis.tick() * 10
				local noise = math.random()*2-1
				--sample = (resonatorTick( res1, resonatorTick( res2, resonatorTick( res3, pulse + noise/10))))
				-- add upper formants as filtered noise
				sample = (resonatorTick( res1, resonatorTick( res2, resonatorTick( res3, resonatorTick( res4, resonatorTick( res5, pulse + noise ) )))))
			end
			-- is there frication?
			if fricationFilter then
				-- replace sample with frication
				sample = sample + resonatorTick( fricationFilter, math.random()*2-1 )
			end

			-- add to output
			table.insert( outBuffer, sample )

			-- decrement the remaining samples for the pulse
			samplesRemaining = samplesRemaining - 1

			if samplesRemaining < 1 then
				-- reset the phases
				phase1, phase2, phase3, phase4, phase5 = 0, 0, 0, 0, 0

				-- interpolate the frequencies
				local lerpF1 = lerp( f1[frameIndex], f1[frameIndex+1] or f1[frameIndex], tFrame )
				local lerpF2 = lerp( f2[frameIndex], f2[frameIndex+1] or f2[frameIndex], tFrame )
				local lerpF3 = lerp( f3[frameIndex], f3[frameIndex+1] or f3[frameIndex], tFrame )

				-- calculate the rate
				delta1 = 2 * math.pi * lerpF1 / SAMPLE_RATE
				delta2 = 2 * math.pi * lerpF2 / SAMPLE_RATE
				delta3 = 2 * math.pi * lerpF3 / SAMPLE_RATE
				delta4 = 2 * math.pi * f4 / SAMPLE_RATE
				delta5 = 2 * math.pi * f5 / SAMPLE_RATE

				-- interpolate the amplitudes
				a1 = lerp( amp1[frameIndex], amp1[frameIndex+1] or amp1[frameIndex], tFrame )
				a2 = lerp( amp2[frameIndex], amp2[frameIndex+1] or amp2[frameIndex], tFrame )
				a3 = lerp( amp3[frameIndex], amp3[frameIndex+1] or amp3[frameIndex], tFrame )
				a4 = a1 * .05
				a5 = a1 * .05
				-- interpolate the pitch
				local lerpPitch = lerp( pitch[frameIndex], pitch[frameIndex+1] or pitch[frameIndex], tFrame )

				-- calculate the width of the pulse and set the samples remaining to the pulse width
				pulseWidth = SAMPLE_RATE / lerpPitch
				samplesRemaining = pulseWidth

				if formantSynthesisFlag then
					-- set the glottis
					glottis.set(lerpPitch, a1)

					-- estimate bandwidth
					local bw1 = (50 * (1+(lerpF1/1000)))
					local bw2 = (50 * (1+(lerpF2/1000)))
					local bw3 = (50 * (1+(lerpF3/1000)))
					local bw4 = (50 * (1+(f4/1000)))
					local bw5 = (50 * (1+(f5/1000)))
					bw4 = 150
					bw5 = 150

					-- more accurate calculation
					local f1_div_500 = lerpF1/500
					bw1 = 15 * math.pow(1/f1_div_500, 2) + 20 * math.pow(f1_div_500, 1/2) + (5 * math.pow(f1_div_500,2))

					local priorBw2 = bw2
					if lerpF2 == lerpF3 then
						-- prevent division by zero
						bw2 = 22 + (16 * math.pow(f1_div_500,2))
					else
						bw2 = 22 + (16 * math.pow(f1_div_500,2)) + (12000/(lerpF3-lerpF2))
					end
					bw2 = math.max( bw2, priorBw2 )

					bw3 = (25 * math.pow(f1_div_500,2)) + (4 * math.pow(lerpF2/500,2)) + (10 * lerpF3 * (f4-lerpF3))

					-- set the resonators
					resonatorSet( res1, lerpF1, bw1, 1 )
					resonatorSet( res2, lerpF2, bw2, 1 )
					resonatorSet( res3, lerpF3, bw3, 1 )
					resonatorSet( res4, lerpF3, bw4, 1 )
					resonatorSet( res5, lerpF3, bw5, 1 )

				end

			else
				-- increment the angles
				phase1 = phase1 + delta1
				phase2 = phase2 + delta2
				phase3 = phase3 + delta3
				phase4 = phase4 + delta4
				phase5 = phase5 + delta5
			end
		end
	end
end




local function rescaleAmp( a )

	local index = math.floor( a+1 )
	if a < 1 then return 0 end
	local t = getRatio( index, index+1, a )
	return lerp( amplitudeRescale[index], amplitudeRescale[index+1], t )
end


local function rescaleAmplitudes(amp1, amp2, amp3)

	--
	-- Rescale volume from a linear scale to decibels.
	--
	-- rescale the amplitudes
	for i = 1, #amp1 do
		print(amp1[i], amp2[i], amp3[i] )
		amp1[i] = rescaleAmp(amp1[i])
		amp2[i] = rescaleAmp(amp2[i])
		amp3[i] = rescaleAmp(amp3[i])

		if not amp1[i] or not amp2[i] or not amp3[i] then
			print( amp1[i], amp2[i], amp3[i] )
			error("Bad amp")
		end

	end

end




local function renderPhonemes(outIndex, outLength, outStress)

	debugPhonemes()

	-- check for empty table
	if outIndex[1] == LINE_TERMINATOR then
		return
	end

	-- create the tables holding frame data
	local f1, f2, f3, amp1, amp2, amp3, frqF, bwF, ampF, pitch = createFrames(outIndex, outLength, outStress)
	dumpFrames(f1, f2, f3, amp1, amp2, amp3, pitch)

	-- create transitions between phonemes
	debugPrint("Creating transitions")
	createTransitions( outIndex, outLength, f1, f2, f3, amp1, amp2, amp3, pitch )
	dumpFrames(f1, f2, f3, amp1, amp2, amp3, pitch)

	-- create a pitch contour
	debugPrint("Setting Pitch Contour")
	assignPitchContour(pitch, f1)
	dumpFrames(f1, f2, f3, amp1, amp2, amp3, pitch)

	-- scale the amplitudes
	debugPrint("Rescaling Amplitudes")
	rescaleAmplitudes(amp1, amp2, amp3)
	dumpFrames(f1, f2, f3, amp1, amp2, amp3, pitch)

	if debugFlag then
		printOutput(f1, f2, f3, amp1, amp2, amp3, pitch)
	end

	-- render the frames
	debugPrint("Rendering the frames")
	renderFrames(f1, f2, f3, amp1, amp2, amp3, frqF, bwF, ampF, pitch)

end


-- CONVERT PHONEMES

local function buildPhoneme( data )

	-- insert into the pData table
	table.insert( pData, data )

	-- set the total steps
	data.parts = data.part

	-- if part == 0, insert into name lookup
	if data.part == 1 then
		pIndexByName[data.token] = #pData
	else
		-- get the index of the primary
		local i = pIndexByName[data.token]
		if i == nil then
			error("Phoneme '"..data.token.."' not yet in table.")
		end

		-- set the steps
		pData[i].parts = data.parts
	end
end


-- The phoneme data
--[[
buildPhoneme{ token=" ", part=1, f1=0, f2=0, f3=0, a1=0, a2=0, a3=0, length=0, stress=0, blendRank=0, blendIn=0, blendOut=0 }
buildPhoneme{ token=".", part=1, f1=520, f2=1836, f3=2494, a1=0, a2=0, a3=0, length=18, stress=18, blendRank=31, blendIn=2, blendOut=2 }
buildPhoneme{ token="?", part=1, f1=520, f2=1836, f3=2494, a1=0, a2=0, a3=0, length=18, stress=18, blendRank=31, blendIn=2, blendOut=2 }
buildPhoneme{ token=",", part=1, f1=520, f2=1836, f3=2494, a1=0, a2=0, a3=0, length=18, stress=18, blendRank=31, blendIn=2, blendOut=2 }
buildPhoneme{ token="-", part=1, f1=520, f2=1836, f3=2494, a1=0, a2=0, a3=0, length=8, stress=8, blendRank=31, blendIn=2, blendOut=2 }
buildPhoneme{ token="IY", part=1, f1=274, f2=2303, f3=3015, a1=13, a2=10, a3=8, length=8, stress=11, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="IH", part=1, f1=383, f2=1974, f3=2549, a1=13, a2=11, a3=7, length=8, stress=9, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="EH", part=1, f1=493, f2=1809, f3=2494, a1=14, a2=13, a3=8, length=8, stress=11, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="AE", part=1, f1=658, f2=1699, f3=2412, a1=15, a2=14, a3=8, length=8, stress=14, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="AA", part=1, f1=712, f2=1096, f3=2440, a1=15, a2=13, a3=1, length=11, stress=15, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="AH", part=1, f1=603, f2=1206, f3=2385, a1=15, a2=12, a3=1, length=6, stress=11, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="AO", part=1, f1=548, f2=822, f3=2412, a1=15, a2=12, a3=0, length=12, stress=16, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="UH", part=1, f1=438, f2=987, f3=2248, a1=15, a2=11, a3=1, length=10, stress=12, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="AX", part=1, f1=548, f2=1206, f3=2440, a1=12, a2=9, a3=0, length=5, stress=6, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="IX", part=1, f1=383, f2=1974, f3=2549, a1=13, a2=11, a3=7, length=5, stress=6, blendRank=5, blendIn=4, blendOut=4 }
buildPhoneme{ token="ER", part=1, f1=493, f2=1316, f3=1699, a1=12, a2=11, a3=5, length=11, stress=14, blendRank=5, blendIn=4, blendOut=4 }
buildPhoneme{ token="UX", part=1, f1=383, f2=987, f3=2248, a1=15, a2=12, a3=1, length=10, stress=12, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="OH", part=1, f1=493, f2=822, f3=2412, a1=15, a2=12, a3=0, length=10, stress=14, blendRank=10, blendIn=4, blendOut=4 }
buildPhoneme{ token="RX", part=1, f1=493, f2=1370, f3=1699, a1=13, a2=12, a3=6, length=10, stress=12, blendRank=2, blendIn=3, blendOut=3 }
buildPhoneme{ token="LX", part=1, f1=438, f2=987, f3=3015, a1=13, a2=8, a3=1, length=9, stress=11, blendRank=8, blendIn=3, blendOut=2 }
buildPhoneme{ token="WX", part=1, f1=329, f2=767, f3=2193, a1=13, a2=8, a3=0, length=8, stress=8, blendRank=5, blendIn=4, blendOut=4 }
buildPhoneme{ token="YX", part=1, f1=383, f2=1864, f3=2549, a1=14, a2=12, a3=7, length=7, stress=8, blendRank=5, blendIn=4, blendOut=4 }
buildPhoneme{ token="WH", part=1, f1=274, f2=658, f3=2467, a1=13, a2=8, a3=0, length=9, stress=11, blendRank=11, blendIn=3, blendOut=2 }
buildPhoneme{ token="R", part=1, f1=493, f2=1370, f3=1645, a1=12, a2=10, a3=5, length=7, stress=10, blendRank=10, blendIn=3, blendOut=2 }
buildPhoneme{ token="L", part=1, f1=383, f2=822, f3=3015, a1=13, a2=8, a3=1, length=6, stress=9, blendRank=9, blendIn=3, blendOut=2 }
buildPhoneme{ token="W", part=1, f1=274, f2=658, f3=2467, a1=13, a2=8, a3=0, length=8, stress=8, blendRank=8, blendIn=3, blendOut=2 }
buildPhoneme{ token="Y", part=1, f1=219, f2=2248, f3=3015, a1=13, a2=10, a3=8, length=6, stress=8, blendRank=8, blendIn=3, blendOut=2 }
buildPhoneme{ token="M", part=1, f1=164, f2=1261, f3=2220, a1=12, a2=3, a3=0, length=7, stress=8, blendRank=160, blendIn=1, blendOut=1 }
buildPhoneme{ token="N", part=1, f1=164, f2=1480, f3=3317, a1=9, a2=9, a3=0, length=7, stress=8, blendRank=8, blendIn=2, blendOut=1 }
buildPhoneme{ token="NX", part=1, f1=164, f2=2357, f3=2769, a1=9, a2=6, a3=3, length=7, stress=8, blendRank=8, blendIn=3, blendOut=1 }
buildPhoneme{ token="DX", part=1, f1=164, f2=1480, f3=3317, a1=0, a2=0, a3=0, length=2, stress=3, blendRank=23, blendIn=2, blendOut=1 }
buildPhoneme{ token="Q", part=1, f1=466, f2=1836, f3=2494, a1=0, a2=0, a3=0, length=5, stress=5, blendRank=31, blendIn=1, blendOut=1 }
buildPhoneme{ token="S", part=1, f1=164, f2=2001, f3=2714, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=18, blendIn=3, blendOut=1 }
buildPhoneme{ token="SH", part=1, f1=164, f2=2165, f3=2906, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=18, blendIn=3, blendOut=1 }
buildPhoneme{ token="F", part=1, f1=164, f2=712, f3=2220, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=18, blendIn=3, blendOut=1 }
buildPhoneme{ token="TH", part=1, f1=164, f2=1809, f3=3317, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=18, blendIn=3, blendOut=1 }
buildPhoneme{ token="/H", part=1, f1=383, f2=2001, f3=2549, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=30, blendIn=1, blendOut=1 }
buildPhoneme{ token="/X", part=1, f1=438, f2=1014, f3=2248, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=30, blendIn=1, blendOut=1 }
buildPhoneme{ token="Z", part=1, f1=246, f2=1398, f3=2549, a1=11, a2=3, a3=0, length=6, stress=6, blendRank=20, blendIn=3, blendOut=2 }
buildPhoneme{ token="ZH", part=1, f1=274, f2=1809, f3=2823, a1=11, a2=5, a3=1, length=6, stress=6, blendRank=20, blendIn=3, blendOut=2 }
buildPhoneme{ token="V", part=1, f1=219, f2=1096, f3=2083, a1=11, a2=3, a3=0, length=7, stress=8, blendRank=20, blendIn=3, blendOut=2 }
buildPhoneme{ token="DH", part=1, f1=274, f2=1288, f3=2549, a1=11, a2=4, a3=0, length=6, stress=6, blendRank=20, blendIn=2, blendOut=1 }
buildPhoneme{ token="CH", part=1, f1=164, f2=2165, f3=2769, a1=0, a2=0, a3=0, length=6, stress=6, blendRank=23, blendIn=2, blendOut=0 }
buildPhoneme{ token="CH", part=2, f1=164, f2=2165, f3=2769, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=23, blendIn=3, blendOut=1 }
buildPhoneme{ token="J", part=1, f1=164, f2=1809, f3=3317, a1=1, a2=0, a3=0, length=8, stress=9, blendRank=26, blendIn=2, blendOut=0 }
buildPhoneme{ token="J", part=2, f1=137, f2=2165, f3=2769, a1=11, a2=5, a3=1, length=3, stress=4, blendRank=26, blendIn=3, blendOut=1 }
buildPhoneme{ token="J", part=3, f1=164, f2=3015, f3=3317, a1=0, a2=10, a3=14, length=1, stress=2, blendRank=29, blendIn=0, blendOut=0 }
buildPhoneme{ token="J", part=4, f1=0, f2=0, f3=0, a1=2, a2=2, a3=1, length=30, stress=1, blendRank=29, blendIn=0, blendOut=5 }
buildPhoneme{ token="EY", part=1, f1=493, f2=1974, f3=2467, a1=14, a2=14, a3=9, length=13, stress=14, blendRank=2, blendIn=5, blendOut=5 }
buildPhoneme{ token="AY", part=1, f1=712, f2=1041, f3=2412, a1=15, a2=13, a3=1, length=12, stress=15, blendRank=2, blendIn=5, blendOut=5 }
buildPhoneme{ token="OY", part=1, f1=548, f2=822, f3=2412, a1=15, a2=12, a3=0, length=12, stress=15, blendRank=2, blendIn=5, blendOut=5 }
buildPhoneme{ token="AW", part=1, f1=712, f2=1151, f3=2412, a1=15, a2=13, a3=1, length=12, stress=15, blendRank=2, blendIn=5, blendOut=5 }
buildPhoneme{ token="OW", part=1, f1=493, f2=822, f3=2412, a1=15, a2=12, a3=0, length=14, stress=14, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="UW", part=1, f1=329, f2=932, f3=2248, a1=13, a2=8, a3=0, length=9, stress=14, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="B", part=1, f1=164, f2=712, f3=2220, a1=2, a2=0, a3=0, length=6, stress=8, blendRank=26, blendIn=2, blendOut=2 }
buildPhoneme{ token="B", part=2, f1=164, f2=712, f3=2220, a1=4, a2=1, a3=0, length=1, stress=2, blendRank=29, blendIn=0, blendOut=0 }
buildPhoneme{ token="B", part=3, f1=164, f2=712, f3=2220, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=27, blendIn=2, blendOut=1 }
buildPhoneme{ token="D", part=1, f1=164, f2=1809, f3=3317, a1=2, a2=0, a3=0, length=5, stress=7, blendRank=26, blendIn=2, blendOut=2 }
buildPhoneme{ token="D", part=2, f1=164, f2=1809, f3=3317, a1=4, a2=1, a3=0, length=1, stress=2, blendRank=29, blendIn=0, blendOut=0 }
buildPhoneme{ token="D", part=3, f1=164, f2=1809, f3=3317, a1=0, a2=0, a3=0, length=1, stress=1, blendRank=27, blendIn=3, blendOut=1 }
buildPhoneme{ token="G", part=1, f1=164, f2=3015, f3=3070, a1=1, a2=0, a3=0, length=6, stress=7, blendRank=26, blendIn=2, blendOut=2 }
buildPhoneme{ token="G", part=2, f1=164, f2=3015, f3=3015, a1=4, a2=1, a3=0, length=1, stress=2, blendRank=29, blendIn=0, blendOut=0 }
buildPhoneme{ token="G", part=3, f1=164, f2=3015, f3=3015, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=27, blendIn=4, blendOut=1 }
buildPhoneme{ token="GX", part=1, f1=164, f2=2303, f3=2577, a1=1, a2=0, a3=0, length=6, stress=7, blendRank=26, blendIn=2, blendOut=2 }
buildPhoneme{ token="GX", part=2, f1=164, f2=2303, f3=2577, a1=4, a2=1, a3=0, length=1, stress=2, blendRank=29, blendIn=0, blendOut=0 }
buildPhoneme{ token="GX", part=3, f1=164, f2=2303, f3=2577, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=27, blendIn=3, blendOut=1 }
buildPhoneme{ token="P", part=1, f1=164, f2=712, f3=2220, a1=0, a2=0, a3=0, length=8, stress=8, blendRank=23, blendIn=2, blendOut=2 }
buildPhoneme{ token="P", part=2, f1=164, f2=712, f3=2220, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=29, blendIn=0, blendOut=0 }
buildPhoneme{ token="P", part=3, f1=164, f2=712, f3=2220, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=23, blendIn=2, blendOut=2 }
buildPhoneme{ token="T", part=1, f1=164, f2=1809, f3=3317, a1=0, a2=0, a3=0, length=4, stress=6, blendRank=23, blendIn=2, blendOut=2 }
buildPhoneme{ token="T", part=2, f1=164, f2=1809, f3=3317, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=29, blendIn=0, blendOut=0 }
buildPhoneme{ token="T", part=3, f1=164, f2=1809, f3=3317, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=23, blendIn=2, blendOut=1 }
buildPhoneme{ token="K", part=1, f1=164, f2=2988, f3=2769, a1=0, a2=0, a3=0, length=6, stress=7, blendRank=23, blendIn=3, blendOut=3 }
buildPhoneme{ token="K", part=2, f1=274, f2=2357, f3=2769, a1=12, a2=10, a3=7, length=1, stress=2, blendRank=29, blendIn=0, blendOut=0 }
buildPhoneme{ token="K", part=3, f1=274, f2=2988, f3=3070, a1=0, a2=0, a3=0, length=4, stress=4, blendRank=23, blendIn=3, blendOut=2 }
buildPhoneme{ token="KX", part=1, f1=164, f2=2303, f3=2577, a1=0, a2=0, a3=0, length=6, stress=7, blendRank=23, blendIn=3, blendOut=3 }
buildPhoneme{ token="KX", part=2, f1=164, f2=2303, f3=2577, a1=0, a2=10, a3=5, length=1, stress=1, blendRank=29, blendIn=0, blendOut=0 }
buildPhoneme{ token="KX", part=3, f1=164, f2=2303, f3=2577, a1=0, a2=0, a3=0, length=4, stress=4, blendRank=23, blendIn=3, blendOut=2 }
buildPhoneme{ token="UL", part=1, f1=1206, f2=3481, f3=219, a1=15, a2=0, a3=19, length=199, stress=5, blendRank=23, blendIn=176, blendOut=160 }
buildPhoneme{ token="UM", part=1, f1=520, f2=3481, f3=27, a1=15, a2=0, a3=16, length=255, stress=5, blendRank=23, blendIn=160, blendOut=160 }
buildPhoneme{ token="UN", part=1, f1=0, f2=0, f3=0, a1=0, a2=0, a3=0, length=0, stress=0, blendRank=0, blendIn=0, blendOut=0 }
--]]

buildPhoneme{ token=" ", part=1, f1=0, f2=0, f3=0, a1=0, a2=0, a3=0, length=0, stress=0, blendRank=0, blendIn=0, blendOut=0 }
buildPhoneme{ token=".", part=1, f1=19, f2=67, f3=91, a1=0, a2=0, a3=0, length=18, stress=18, blendRank=31, blendIn=2, blendOut=2 }
buildPhoneme{ token="?", part=1, f1=19, f2=67, f3=91, a1=0, a2=0, a3=0, length=18, stress=18, blendRank=31, blendIn=2, blendOut=2 }
buildPhoneme{ token=",", part=1, f1=19, f2=67, f3=91, a1=0, a2=0, a3=0, length=18, stress=18, blendRank=31, blendIn=2, blendOut=2 }
buildPhoneme{ token="-", part=1, f1=19, f2=67, f3=91, a1=0, a2=0, a3=0, length=8, stress=8, blendRank=31, blendIn=2, blendOut=2 }
buildPhoneme{ token="IY", part=1, f1=10, f2=84, f3=110, a1=13, a2=10, a3=8, length=8, stress=11, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="IH", part=1, f1=14, f2=72, f3=93, a1=13, a2=11, a3=7, length=8, stress=9, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="EH", part=1, f1=18, f2=66, f3=91, a1=14, a2=13, a3=8, length=8, stress=11, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="AE", part=1, f1=24, f2=62, f3=88, a1=15, a2=14, a3=8, length=8, stress=14, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="AA", part=1, f1=26, f2=40, f3=89, a1=15, a2=13, a3=1, length=11, stress=15, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="AH", part=1, f1=22, f2=44, f3=87, a1=15, a2=12, a3=1, length=6, stress=11, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="AO", part=1, f1=20, f2=30, f3=88, a1=15, a2=12, a3=0, length=12, stress=16, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="UH", part=1, f1=16, f2=36, f3=82, a1=15, a2=11, a3=1, length=10, stress=12, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="AX", part=1, f1=20, f2=44, f3=89, a1=12, a2=9, a3=0, length=5, stress=6, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="IX", part=1, f1=14, f2=72, f3=93, a1=13, a2=11, a3=7, length=5, stress=6, blendRank=5, blendIn=4, blendOut=4 }
buildPhoneme{ token="ER", part=1, f1=18, f2=48, f3=62, a1=12, a2=11, a3=5, length=11, stress=14, blendRank=5, blendIn=4, blendOut=4 }
buildPhoneme{ token="UX", part=1, f1=14, f2=36, f3=82, a1=15, a2=12, a3=1, length=10, stress=12, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="OH", part=1, f1=18, f2=30, f3=88, a1=15, a2=12, a3=0, length=10, stress=14, blendRank=10, blendIn=4, blendOut=4 }
buildPhoneme{ token="RX", part=1, f1=18, f2=50, f3=62, a1=13, a2=12, a3=6, length=10, stress=12, blendRank=2, blendIn=3, blendOut=3 }
buildPhoneme{ token="LX", part=1, f1=16, f2=36, f3=110, a1=13, a2=8, a3=1, length=9, stress=11, blendRank=8, blendIn=3, blendOut=2 }
buildPhoneme{ token="WX", part=1, f1=12, f2=28, f3=80, a1=13, a2=8, a3=0, length=8, stress=8, blendRank=5, blendIn=4, blendOut=4 }
buildPhoneme{ token="YX", part=1, f1=14, f2=68, f3=93, a1=14, a2=12, a3=7, length=7, stress=8, blendRank=5, blendIn=4, blendOut=4 }
buildPhoneme{ token="WH", part=1, f1=10, f2=24, f3=90, a1=13, a2=8, a3=0, length=9, stress=11, blendRank=11, blendIn=3, blendOut=2 }
buildPhoneme{ token="R", part=1, f1=18, f2=50, f3=60, a1=12, a2=10, a3=5, length=7, stress=10, blendRank=10, blendIn=3, blendOut=2 }
buildPhoneme{ token="L", part=1, f1=14, f2=30, f3=110, a1=13, a2=8, a3=1, length=6, stress=9, blendRank=9, blendIn=3, blendOut=2 }
buildPhoneme{ token="W", part=1, f1=10, f2=24, f3=90, a1=13, a2=8, a3=0, length=8, stress=8, blendRank=8, blendIn=3, blendOut=2 }
buildPhoneme{ token="Y", part=1, f1=8, f2=82, f3=110, a1=13, a2=10, a3=8, length=6, stress=8, blendRank=8, blendIn=3, blendOut=2 }
buildPhoneme{ token="M", part=1, f1=6, f2=46, f3=81, a1=12, a2=3, a3=0, length=7, stress=8, blendRank=160, blendIn=1, blendOut=1 }
buildPhoneme{ token="N", part=1, f1=6, f2=54, f3=121, a1=9, a2=9, a3=0, length=7, stress=8, blendRank=8, blendIn=2, blendOut=1 }
buildPhoneme{ token="NX", part=1, f1=6, f2=86, f3=101, a1=9, a2=6, a3=3, length=7, stress=8, blendRank=8, blendIn=3, blendOut=1 }
buildPhoneme{ token="DX", part=1, f1=6, f2=54, f3=121, a1=0, a2=0, a3=0, length=2, stress=3, blendRank=23, blendIn=2, blendOut=1 }
buildPhoneme{ token="Q", part=1, f1=17, f2=67, f3=91, a1=0, a2=0, a3=0, length=5, stress=5, blendRank=31, blendIn=1, blendOut=1 }
buildPhoneme{ token="S", part=1, f1=6, f2=73, f3=99, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=18, blendIn=3, blendOut=1 }
buildPhoneme{ token="SH", part=1, f1=6, f2=79, f3=106, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=18, blendIn=3, blendOut=1 }
buildPhoneme{ token="F", part=1, f1=6, f2=26, f3=81, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=18, blendIn=3, blendOut=1 }
buildPhoneme{ token="TH", part=1, f1=6, f2=66, f3=121, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=18, blendIn=3, blendOut=1 }
buildPhoneme{ token="/H", part=1, f1=14, f2=73, f3=93, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=30, blendIn=1, blendOut=1 }
buildPhoneme{ token="/X", part=1, f1=16, f2=37, f3=82, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=30, blendIn=1, blendOut=1 }
buildPhoneme{ token="Z", part=1, f1=9, f2=51, f3=93, a1=11, a2=3, a3=0, length=6, stress=6, blendRank=20, blendIn=3, blendOut=2 }
buildPhoneme{ token="ZH", part=1, f1=10, f2=66, f3=103, a1=11, a2=5, a3=1, length=6, stress=6, blendRank=20, blendIn=3, blendOut=2 }
buildPhoneme{ token="V", part=1, f1=8, f2=40, f3=76, a1=11, a2=3, a3=0, length=7, stress=8, blendRank=20, blendIn=3, blendOut=2 }
buildPhoneme{ token="DH", part=1, f1=10, f2=47, f3=93, a1=11, a2=4, a3=0, length=6, stress=6, blendRank=20, blendIn=2, blendOut=1 }
buildPhoneme{ token="CH", part=1, f1=6, f2=79, f3=101, a1=0, a2=0, a3=0, length=6, stress=6, blendRank=23, blendIn=2, blendOut=0 }
buildPhoneme{ token="CH", part=2, f1=6, f2=79, f3=101, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=23, blendIn=3, blendOut=1 }
buildPhoneme{ token="J", part=1, f1=6, f2=66, f3=121, a1=1, a2=0, a3=0, length=8, stress=9, blendRank=26, blendIn=2, blendOut=0 }
buildPhoneme{ token="J", part=2, f1=5, f2=79, f3=101, a1=11, a2=5, a3=1, length=3, stress=4, blendRank=26, blendIn=3, blendOut=1 }
buildPhoneme{ token="???", part=1, f1=6, f2=110, f3=121, a1=0, a2=10, a3=14, length=1, stress=2, blendRank=29, blendIn=0, blendOut=0 }	-- FIXME
buildPhoneme{ token="???", part=1, f1=0, f2=0, f3=0, a1=2, a2=2, a3=1, length=30, stress=1, blendRank=29, blendIn=0, blendOut=5 }	 -- FIXME
buildPhoneme{ token="EY", part=1, f1=18, f2=72, f3=90, a1=14, a2=14, a3=9, length=13, stress=14, blendRank=2, blendIn=5, blendOut=5 }
buildPhoneme{ token="AY", part=1, f1=26, f2=38, f3=88, a1=15, a2=13, a3=1, length=12, stress=15, blendRank=2, blendIn=5, blendOut=5 }
buildPhoneme{ token="OY", part=1, f1=20, f2=30, f3=88, a1=15, a2=12, a3=0, length=12, stress=15, blendRank=2, blendIn=5, blendOut=5 }
buildPhoneme{ token="AW", part=1, f1=26, f2=42, f3=88, a1=15, a2=13, a3=1, length=12, stress=15, blendRank=2, blendIn=5, blendOut=5 }
buildPhoneme{ token="OW", part=1, f1=18, f2=30, f3=88, a1=15, a2=12, a3=0, length=14, stress=14, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="UW", part=1, f1=12, f2=34, f3=82, a1=13, a2=8, a3=0, length=9, stress=14, blendRank=2, blendIn=4, blendOut=4 }
buildPhoneme{ token="B", part=1, f1=6, f2=26, f3=81, a1=2, a2=0, a3=0, length=6, stress=8, blendRank=26, blendIn=2, blendOut=2 }
buildPhoneme{ token="B", part=2, f1=6, f2=26, f3=81, a1=4, a2=1, a3=0, length=1, stress=2, blendRank=29, blendIn=0, blendOut=0 }
buildPhoneme{ token="B", part=3, f1=6, f2=26, f3=81, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=27, blendIn=2, blendOut=1 }
buildPhoneme{ token="D", part=1, f1=6, f2=66, f3=121, a1=2, a2=0, a3=0, length=5, stress=7, blendRank=26, blendIn=2, blendOut=2 }
buildPhoneme{ token="D", part=2, f1=6, f2=66, f3=121, a1=4, a2=1, a3=0, length=1, stress=2, blendRank=29, blendIn=0, blendOut=0 }
buildPhoneme{ token="D", part=3, f1=6, f2=66, f3=121, a1=0, a2=0, a3=0, length=1, stress=1, blendRank=27, blendIn=3, blendOut=1 }
buildPhoneme{ token="G", part=1, f1=6, f2=110, f3=112, a1=1, a2=0, a3=0, length=6, stress=7, blendRank=26, blendIn=2, blendOut=2 }
buildPhoneme{ token="G", part=2, f1=6, f2=110, f3=110, a1=4, a2=1, a3=0, length=1, stress=2, blendRank=29, blendIn=0, blendOut=0 }
buildPhoneme{ token="G", part=3, f1=6, f2=110, f3=110, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=27, blendIn=4, blendOut=1 }
buildPhoneme{ token="GX", part=1, f1=6, f2=84, f3=94, a1=1, a2=0, a3=0, length=6, stress=7, blendRank=26, blendIn=2, blendOut=2 }
buildPhoneme{ token="GX", part=2, f1=6, f2=84, f3=94, a1=4, a2=1, a3=0, length=1, stress=2, blendRank=29, blendIn=0, blendOut=0 }
buildPhoneme{ token="GX", part=3, f1=6, f2=84, f3=94, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=27, blendIn=3, blendOut=1 }
buildPhoneme{ token="P", part=1, f1=6, f2=26, f3=81, a1=0, a2=0, a3=0, length=8, stress=8, blendRank=23, blendIn=2, blendOut=2 }
buildPhoneme{ token="P", part=2, f1=6, f2=26, f3=81, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=29, blendIn=0, blendOut=0 }
buildPhoneme{ token="P", part=3, f1=6, f2=26, f3=81, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=23, blendIn=2, blendOut=2 }
buildPhoneme{ token="T", part=1, f1=6, f2=66, f3=121, a1=0, a2=0, a3=0, length=4, stress=6, blendRank=23, blendIn=2, blendOut=2 }
buildPhoneme{ token="T", part=2, f1=6, f2=66, f3=121, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=29, blendIn=0, blendOut=0 }
buildPhoneme{ token="T", part=3, f1=6, f2=66, f3=121, a1=0, a2=0, a3=0, length=2, stress=2, blendRank=23, blendIn=2, blendOut=1 }
buildPhoneme{ token="K", part=1, f1=6, f2=109, f3=101, a1=0, a2=0, a3=0, length=6, stress=7, blendRank=23, blendIn=3, blendOut=3 }
buildPhoneme{ token="K", part=2, f1=10, f2=86, f3=101, a1=12, a2=10, a3=7, length=1, stress=2, blendRank=29, blendIn=0, blendOut=0 }
buildPhoneme{ token="K", part=3, f1=10, f2=109, f3=112, a1=0, a2=0, a3=0, length=4, stress=4, blendRank=23, blendIn=3, blendOut=2 }
buildPhoneme{ token="KX", part=1, f1=6, f2=84, f3=94, a1=0, a2=0, a3=0, length=6, stress=7, blendRank=23, blendIn=3, blendOut=3 }
buildPhoneme{ token="KX", part=2, f1=6, f2=84, f3=94, a1=0, a2=10, a3=5, length=1, stress=1, blendRank=29, blendIn=0, blendOut=0 }
buildPhoneme{ token="KX", part=3, f1=6, f2=84, f3=94, a1=0, a2=0, a3=0, length=4, stress=4, blendRank=23, blendIn=3, blendOut=2 }
buildPhoneme{ token="UL", part=1, f1=44, f2=127, f3=8, a1=15, a2=0, a3=19, length=199, stress=5, blendRank=23, blendIn=176, blendOut=160 }
buildPhoneme{ token="UM", part=1, f1=19, f2=127, f3=1, a1=15, a2=0, a3=16, length=255, stress=5, blendRank=23, blendIn=160, blendOut=160 }
buildPhoneme{ token="UN", part=1, f1=0, f2=0, f3=0, a1=0, a2=0, a3=0, length=0, stress=0, blendRank=0, blendIn=0, blendOut=0 }





-- Set the fricative for the phoneme
local function setFricative( token, part, frequency, bandwidth, amplitude )

	-- find root token
	local p = getPhoneIndex(token)
	if pData[p+part-1].part ~= part then
		error("Phoneme '"..token.."' does not have a part "..part)
	else
		-- offset to the part
		p = p + part - 1
	end

	-- store the values
	pData[p].fricFrq = frequency
	pData[p].fricBandwidth = bandwidth
	pData[p].fricAmplitude = amplitude

end


-- Add frication to phonemes
setFricative( "S", 1, 4500, 300, 1 )
setFricative( "SH", 1, 2600, 250, 1 )
setFricative( "CH", 1, 4500, 100, .2 )
setFricative( "TH", 1, 1200, 500, 1 )
setFricative( "F", 1, 1200, 500, 1 )
setFricative( "/H", 1, 500, 500, .5 )

setFricative( "P", 1, 2600, 50, 0 )
setFricative( "K", 1, 2600, 50, 0 )
setFricative( "G", 1, 2600, 50, 0 )
setFricative( "KX", 1, 2600, 50, 0 )
setFricative( "GX", 1, 2600, 50, 0 )
setFricative( "B", 1, 2600, 50, 0 )
setFricative( "P", 1, 2600, 50, 0 )
setFricative( "T", 1, 4500, 100, 0)
setFricative( "D", 1, 4500, 100, 0 )


-- Experimental
setFricative( "K", 2, 2600, 50, .0125 )
setFricative( "KX", 2, 2600, 50, .0125 )
setFricative( "G", 2, 2600, 50, .0125 )
setFricative( "GX", 2, 2600, 50, .0125 )
setFricative( "B", 2, 2600, 50, .125 )
setFricative( "P", 2, 2600, 50, .125 )
setFricative( "T", 2, 4500, 100, .2 )
setFricative( "D", 2, 4500, 100, .125 )

-- aspiration
setFricative( "K", 3, 500, 500, .0125 )
setFricative( "KX", 3, 500, 500, .0125 )
setFricative( "G", 3, 500, 500, .0125 )
setFricative( "GX", 3, 500, 500, .0125 )
setFricative( "B", 3, 500, 500, .0125 )
setFricative( "P", 3, 500, 500, .0125 )
setFricative( "T", 3, 500, 500, .0125 )


-- In SAM there are two bitflag tables
--  bit flag table 1
--    bit 1 (1)     unvoiced plosive
--    bit 2 (2)     plosive
--    bit 3 (4)     voiced
--    bit 4 (8)     consonants that extend the duration of vowels that follow
--    bit 5 (16)    diphthong
--    bit 6 (32)    vowel produced in back of throat (back vowels)
--    bit 7 (64)    consonant
--    bit 8 (128)   vowels, including R and W sounds (for stress)
--
--  bit flag table 2
--    bit 1 (1)     punctuation
--    bit 2 (2)     unused
--    bit 3 (4)     alevolar consonant, produced behind upper front teeth
--    bit 4 (8)     nasal (M,N,NX)
--    bit 5 (16)    glide (R,L,W,Y)
--    bit 6 (32)    affricative
--    bit 7 (64)    punctuation or glottal stop
--    bit 8 (128)   non-phomemes - punctuation or space


-- Create a table of flags for the given phoneme names
local function flagTable(phoneNames)
	-- the table to build
	local t = {}

	-- set the indexes to true for each phoneme name
	for _, name in ipairs(phoneNames) do
		t[getPhoneIndex(name)] = true
	end

	-- return the table
	return t
end


local isUnvoicedPlosive = flagTable{"P","T","K","KX"}
local isPlosive = flagTable{"B","D","G","GX","P","T","K","KX"}
local isVoiced = flagTable{"IY","IH","EH","AE","AA","AH","AO","UH","AX","IX","ER","UX","OH","RX","LX",
	"WX","YX","WH","R","L","W","Y","M","N","NX","Q","Z","ZH","V","DH","J","EY","AY","OY","AW","OW","UW",
	"B","D","G","GX"}
-- FIXME: Why is this not being referenced anymore?
local isVowelExtender = flagTable{"M","N","NX","DX","Q","CH","J","B","D","G","GX","P","T","K","KX"}
local isDiphthong = flagTable{"EY","AY","OY","AW","OW","UW"}
local isBackVowel = flagTable{"IY","IH","EH","AE","AA","AH","AX","IX","EY","AY","OY"}
local isConsonant = flagTable{"WH","R","L","W","Y","M","N","NX","DX","Q","S","SH","F","TH","/H","/X",
	"Z","ZH","V","DH","CH","J","B","D","G","GX","P","T","K","KX","UM","UN"}
local isVowel = flagTable{"IY","IH","EH","AE","AA","AH","AO","UH","AX","IX","ER","UX","OH","RX","LX",
	"WX","YX","EY","AY","OY","AW","OW","UW","UL","UM","UN"}
local isPunctuation = flagTable{".","?",",","-"}
local isAlveolar = flagTable{"N","DX","S","TH","Z","DH","D","T"}
local isNasal = flagTable{"M","N","NX"}
local isGlide = flagTable{"R","L","W","Y"}
local isAffricative = flagTable{"S","SH","F","TH","Z","ZH","V","DH","CH"}
-- FIXME: Why is this not being used anymore?
local isRest = flagTable{".","?",",","-","Q"}
-- FIXME: Why is this not being used anymore?
local isNotPhoneme = flagTable{" ",".","?",",","-"}


-- Insert at **position** a phoneme with a given index, length and stress
local function insert( position, iIndex, iLength, iStress )

	-- sanity test value
	if not iStress then error("iStress is nil") end

	table.insert( pIndex, position, iIndex )
	table.insert( pLength, position, iLength )
	table.insert( pStress, position, iStress )
end


-- Apply allophonic replacements
local function applyAllophones()

	debugPrint("applyRules")
	local pos = 0

	-- Loop through phonemes
	while true do

		-- increment position and get the phoneme
		pos = pos + 1
		local p = pIndex[pos]

		-- If end of phonemes flag reached, exit routine
		if p == LINE_TERMINATOR then
			-- end of phonemes, exit routine
			return

		elseif p == nil then
			-- safety check, should not happen
			error("nil found in pIndex, missing end of line token")

		elseif p == getPhoneIndex(" ") then
		-- skip spaces

		elseif isDiphthong[p] then

			-- RULE:
			--    <DIPHTHONG> -> <DIPHTHONG> WX | YX
			-- Example: OIL, COW

			-- Ends in IY sound?
			if isBackVowel[p] then
				-- insert YX after
				debugPrint("RULE: insert YX following *Y diphthong")
				insert(pos+1, getPhoneIndex("YX"), 0, pStress[pos])
			else
				-- insert WX after
				debugPrint("RULE: insert WX following *W diphthong")
				insert(pos+1, getPhoneIndex("WX"), 0, pStress[pos])
			end

		elseif p == getPhoneIndex("UL") then

			-- RULE:
			--    UL -> AX L
			-- Example: MEDDLE

			debugPrint("RULE: UL -> AX L")
			pIndex[pos] = getPhoneIndex("AX")
			insert(pos+1, getPhoneIndex("L"), 0, pStress[pos])

		elseif p == getPhoneIndex("UM") then

			-- RULE:
			--        UM -> AX M
			-- Example: ASTRONOMY

			debugPrint("RULE: UM -> AX M")
			pIndex[pos] = getPhoneIndex("AX")
			insert(pos+1, getPhoneIndex("M"), 0, pStress[pos])

			-- don't advance
			pos = pos - 1

		elseif p == getPhoneIndex("UN") then

			-- RULE:
			--       UN -> AX N
			-- Example: FUNCTION

			debugPrint("RULE: UM -> AX N")
			pIndex[pos] = getPhoneIndex("AX")
			insert(pos+1, getPhoneIndex("N"), 0, pStress[pos])

			-- don't advance
			pos = pos - 1

		elseif isVowel[p] and pStress[pos] ~= 0
			and pIndex[pos+1] == getPhoneIndex(" ")
			and isVowel[pIndex[pos+2]] and (pStress[pos+2] or 0) ~= 0 then

			-- RULE:
			--       <STRESSED VOWEL> <SILENCE> <STRESSED VOWEL> -> <STRESSED VOWEL> <SILENCE> Q <VOWEL>
			-- Example: AWAY EIGHT

			debugPrint("RULE: Insert glottal stop between two stressed vowels with space between them");
			insert(pos+2, getPhoneIndex("Q"), 0, 0)

		elseif p == getPhoneIndex("R") and pIndex[pos-1] == getPhoneIndex("T") then

			-- RULES FOR PHONEMES BEFORE R
			--        T R -> CH R
			-- Example: TRACK

			-- change T to CH, but don't advance
			debugPrint("RULE: T R -> CH R")
			pIndex[pos-1] = getPhoneIndex("CH")
			pos = pos - 1

		elseif p == getPhoneIndex("R") and pIndex[pos-1] == getPhoneIndex("D") then

			-- RULES FOR PHONEMES BEFORE R
			--        D R -> J R
			-- Example: DRY

			-- change T to J, but don't advance
			debugPrint("RULE: T R -> J R")
			pIndex[pos-1] = getPhoneIndex("J")
			pos = pos - 1

		elseif p == getPhoneIndex("R") and isVowel[pIndex[pos-1]] then

			-- RULES FOR PHONEMES BEFORE R
			--        <VOWEL> R -> <VOWEL> RX
			-- Example: ART

			-- change R to RX and advance
			debugPrint("RULE: R -> RX")
			pIndex[pos] = getPhoneIndex("RX")

		elseif p == getPhoneIndex("L") and isVowel[pIndex[pos-1]] then

			-- RULE:
			--       <VOWEL> L -> <VOWEL> LX
			-- Example: ALL

			-- change L to LX and advance
			debugPrint("RULE: L -> LX")
			pIndex[pos] = getPhoneIndex("LX")


		elseif p == getPhoneIndex("S") and pIndex[pos-1] == getPhoneIndex("S") then

			-- RULE:
			--       G S -> G Z
			--
			-- Can't get to fire -
			--       1. The G -> GX rule intervenes
			--       2. Reciter already replaces GS -> GZ

			-- change S to Z and advance
			debugPrint("RULE: G S -> G Z")
			pIndex[pos] = getPhoneIndex("Z")

		elseif p == getPhoneIndex("K") and isBackVowel[pIndex[pos+1]] then

			-- RULE:
			--             K <BACK VOWEL> -> KX <BACK VOWEL>
			-- Example: COW

			-- replace K with KX, and advance
			debugPrint("RULE: K <BACK VOWEL> -> KX <BACK VOWEL>")
			pIndex[pos] = getPhoneIndex("KX")


		elseif p == getPhoneIndex("G") and isBackVowel[pIndex[pos+1]] then

			-- RULE:
			--             G <BACK VOWEL> -> GX <BACK VOWEL>
			-- Example: GO

			-- replace G with GX, and advance
			debugPrint("RULE: G <BACK VOWEL> -> GX <BACK VOWEL>")
			pIndex[pos] = getPhoneIndex("GX")


		elseif p == getPhoneIndex("S") and isUnvoicedPlosive[pIndex[pos+1]] then

			-- RULE:
			--      S P -> S B
			--      S T -> S D
			--      S K -> S G
			--      S KX -> S GX
			-- Examples: SPY, STY, SKY, SCOWL

			-- replace unvoiced plosive with voiced plosive, and advance
			if pIndex[pos+1] == getPhoneIndex("P") then
				debugPrint("RULE: S P -> S B")
				pIndex[pos+1] = getPhoneIndex("B")

			elseif pIndex[pos+1] == getPhoneIndex("T") then
				debugPrint("RULE: S T -> S D")
				pIndex[pos+1] = getPhoneIndex("D")

			elseif pIndex[pos+1] == getPhoneIndex("K") then
				debugPrint("RULE: S K -> S G")
				pIndex[pos+1] = getPhoneIndex("G")

			elseif pIndex[pos+1] == getPhoneIndex("KX") then
				debugPrint("RULE: S KX -> S KX")
				pIndex[pos+1] = getPhoneIndex("GX")
			end


		elseif p == getPhoneIndex("UW") and isAlveolar[pIndex[pos-1]] then

			-- RULE:
			--      <ALVEOLAR> UW -> <ALVEOLAR> UX
			--
			-- Example: NEW, DEW, SUE, ZOO, THOO, TOO

			debugPrint("RULE: <ALVEOLAR> UW -> <ALVEOLAR> UX")
			pIndex[pos] = getPhoneIndex("UX")


		elseif p == getPhoneIndex("CH") then

			-- RULE:
			--       CH -> CH CH' (CH requires two phonemes to represent it)
			-- Example: CHEW

			-- insert additional phoneme and move ahead
			debugPrint("CH -> CH CH+1")
			insert( pos+1, p+1, 0, pStress[pos])


		elseif p == getPhoneIndex("J") then

			-- RULE:
			--       J -> J J' (J requires two phonemes to represent it)
			-- Example: JAY

			-- insert additional phoneme and advance
			debugPrint("J -> J J+1")
			-- insert additional phoneme and move ahead
			insert( pos+1, p+1, 0, pStress[pos] )


		elseif (p == getPhoneIndex("T") or p == getPhoneIndex("D"))
			and isVowel[pIndex[pos-1]]
			and isVowel[pIndex[pos+1]] and pStress[pos+1] ~= 0 then

			-- RULE: Soften T or D between a vowel and a stressed vowel
			--       <VOWEL> T <STRESSED VOWEL> -> <VOWEL> DX <STRESSED VOWEL>
			--       <VOWEL> D <STRESSED VOWEL>  -> <VOWEL> DX <STRESSED VOWEL>
			-- Example: PARTY, TARDY

			-- replace T or D with DX and advance
			debugPrint("<VOWEL> T|D <STRESSED VOWEL> -> <VOWEL> DX <STRESSED VOWEL>")
			pIndex[pos] = getPhoneIndex("DX")

		end


	end

end


-- Rules that adjust the lengths of phonemes (mostly vowels)
--
--  Lengthens voiced non-affricatives between vowel and punctuation mark by 1.5
--  <VOWEL> DX <CONSONANT> -> shorten <VOWEL> by 1
--  <VOWEL> RX <CONSONANT> -> shorten <VOWEL> by 1
--  <VOWEL> <UNVOICED PLOSIVE> - decrease vowel by 1/16th
--  <VOWEL> <VOICED CONSONANT> - increase vowel by 1/2 + 1
--  <NASAL> <PLOSIVE> - <NASAL> = 5, <PLOSIVE> = 6
--  <PLOSIVE> {optional SPACES} <PLOSIVE> - shorten both PLOSIVES to 1/2 + 1
--  <PLOSIVE> <GLIDE> - decrease <GLIDE> by 2



local function adjustLengths()

	-- Lengthen voiced sounds between vowel and punctuation mark. This puts additional
	-- stress just before punctuation.
	--
	-- Search for punctuation. If found, back up to the first vowel, then
	-- process all phonemes between there and up to (but not including) the punctuation.
	-- If any phoneme is found that is not a fricative or unvoiced, the duration is
	-- increased by (length * 1.5) + 1. This lengthens the ends of words just before punctuation.

	local pos = 0
	local priorVowelIndex

	-- iterate through the phoneme list
	while true do

		-- move ahead
		pos = pos + 1

		-- get a phoneme
		local p = pIndex[pos];

		if p == LINE_TERMINATOR then
			-- exit loop if on end of buffer token
			break

		elseif p == nil then
			error("nil found in pIndex, missing end of line token")

		elseif isVowel[p] then
			-- store position of vowel
			priorVowelIndex = pos

		elseif isPunctuation[p] then
			-- FIXME: Should error if no prior vowel
			-- if there was a prior vowel, extend from vowel to punctuation
			if priorVowelIndex then
				for i = priorVowelIndex, pos-1 do
					-- lengthen?
					if not isAffricative[pIndex[i]] or isVoiced[pIndex[i]] then
						-- lengthen the phoneme
						debugPrint("RULE: Lengthen "..(pData[pIndex[i]].token).." between <VOWEL> and <PUNCTUATION> by 1.25*length+1")
						print("initial length", pLength[i])
						pLength[i] = pLength[i] + 1 + math.floor(pLength[i]*.5)
						print("end length", pLength[i])
					end
				end
			end

			-- clear prior vowel index
			priorVowelIndex = nil

		end

	end

	-- Adjust the lengths of some sounds

	-- Loop through all phonemes
	pos = 0
	while true do
		-- advance
		pos = pos + 1

		-- get the phoneme
		local p = pIndex[pos];

		-- process phoneme
		if p == LINE_TERMINATOR then
			-- exit when end token is encountered
			return

		elseif isVowel[p] then
			-- <VOWEL> ...
			if pIndex[pos+1] == getPhoneIndex("DX") and isConsonant[pIndex[pos+2]] then
				debugPrint("RULE: <VOWEL> DX <CONSONANT> -> shorten <VOWEL> by 1")
				print("before:", pLength[pos])
				pLength[pos] = pLength[pos] - 1
				print("after:", pLength[pos])

			elseif pIndex[pos+1] == getPhoneIndex("RX") and isConsonant[pIndex[pos+2]] then
				debugPrint("RULE: <VOWEL> RX <CONSONANT> -> shorten <VOWEL> by 1")
				print("before:", pLength[pos])
				pLength[pos] = pLength[pos] - 1
				print("after:", pLength[pos])

			elseif not isVoiced[pIndex[pos+1]] and isPlosive[pIndex[pos+1]] then
				debugPrint("RULE: <VOWEL> <UNVOICED PLOSIVE> - decrease vowel by 1/16th")
				print("before:", pLength[pos])
				pLength[pos] = math.floor(pLength[pos] - pLength[pos]/16)
				print("after:", pLength[pos])

			elseif isVoiced[pIndex[pos+1]] and isConsonant[pIndex[pos+1]] then
				debugPrint("RULE: <VOWEL> <VOICED CONSONANT> - increase vowel "..pData[p].token.." by 1/4 + 1")
				print("before:", pLength[pos])
				pLength[pos] = pLength[pos] + math.floor(pLength[pos]/4) + 1
				print("after:", pLength[pos])

			end

		elseif isNasal[p] and isPlosive[pIndex[pos+1]] then
			debugPrint("RULE: <NASAL> <PLOSIVE> - set length nasal = 5, plosive = 6")
			pLength[pos] = 5
			pLength[pos+1] = 6

		elseif isPlosive[p] then
			-- skip interventing spaces, if any
			local pastSpaces = pos+1
			while pIndex[pastSpaces] == getPhoneIndex(" ") do
				pastSpaces = pastSpaces + 1
			end
			if isPlosive[pastSpaces] then
				debugPrint("RULE: <PLOSIVE> {optional SPACES} <PLOSIVE> - shorten both PLOSIVES to 1/2 + 1")
				pLength[pos] = math.floor(1 + pLength[pos]/2)
				pLength[pastSpaces] = math.floor(1 + pLength[pastSpaces]/2)
			end

		elseif isGlide[p] and isPlosive[pIndex[pos-1]] then
			-- note this checks for a *preceding* plosive, so it doesn't interfere with the prior rule
			debugPrint("RULE: <PLOSIVE> <GLIDE> - decrease <GLIDE> by 2")
			pLength[pos] = pLength[pos]-2
		end

	end

end


local function extendPhonemeParts()

	-- loop through each position in buffer
	local pos = 1
	while pIndex[pos] ~= LINE_TERMINATOR do
		-- get the phoneme
		local p = pIndex[pos]
		local pNext = pIndex[pos]

		-- get the number of parts
		local parts = pData[p].parts

		-- if not already expanded...
		if parts > 1 and pNext and pData[pNext].parts ~= 1 and pIndex[pos+1] ~= p+1 then

			-- insert additional steps
			if parts > 1 then insert(pos+1, p+1, pData[p+1].length, pStress[pos]) end
			if parts > 2 then insert(pos+2, p+2, pData[p+2].length, pStress[pos]) end
			if parts > 3 then insert(pos+3, p+3, pData[p+3].length, pStress[pos]) end

		end

		-- move forward
		pos = pos + parts

	end

end


-- If a consonant is followed by a stressed vowel, copy stress+1 to the consonant.
local function copyStress()
	-- loop through all the phonemes
	local pos = 1
	while true do
		-- get the phomene and the next
		local p = pIndex[pos]

		-- exit at end of line marker
		if p == LINE_TERMINATOR then return end

		-- consonant?
		if isConsonant[p] then
			-- get next phoneme
			local pNext = pIndex[pos+1]

			-- exit if terminator
			if pNext == LINE_TERMINATOR then return end

			-- next phoneme is vowel?
			if isVowel[pNext]
				and pStress[pos+1] > 0 then
				-- copy prior stress + 1
				pStress[pos] = pStress[pos+1] + 1
			end
		end

		pos = pos + 1

	end
end


-- Set the length of the phonemes. If the stress flag is set,
-- use the stressed length, otherwise use the regular length.
local function setPhonemeLength()

	local pos = 1
	while pIndex[pos] ~= LINE_TERMINATOR do
		-- get the phoneme index
		local p = pIndex[pos]

		if pStress[pos] == 0 then
			-- set to unstressed length
			pLength[pos] = pData[p].length

		else
			-- set to stressed length
			pLength[pos] = pData[p].stress
		end

		-- move ahead
		pos = pos + 1
	end
end


-- Inserts break flags into the token list to limit the length
-- of the rendered output. Breaks are always inserted after punctuation,
-- and optionally after spaces if needed to keep the render length down.
local function insertBreaks()

	local renderLength = 0
	local lastSpaceAt = nil
	local pos = 0

	-- loop until terminator reached
	while true do

		-- move ahead and get the phoneme
		pos = pos + 1
		local p = pIndex[pos]

		-- exit if end of line marker
		if p == LINE_TERMINATOR then return end

		-- add length to total length
		renderLength = renderLength + pLength[pos]

		-- is there enough frames available to add this?
		if renderLength <= MAX_FRAMES then

			if isPunctuation[p] then
				-- insert a break after the punctuation
				insert(pos+1, RENDER_BREAK, 0, 0)

				-- skip past break that was just inserted
				pos = pos + 1

				-- space?
			elseif p == getPhoneIndex(" ") then
				-- a break will be inserted here if the length gets too long
				lastSpaceAt = pos
			end

		else

			-- not enough room to add the new phoneme.
			-- replace the last space that was found with a "Q" (glottal stop)
			pIndex[lastSpaceAt] = getPhoneIndex("Q")
			pLength[lastSpaceAt] = 4
			pStress[lastSpaceAt] = 0

			-- clear the length count
			renderLength = 0

			-- insert a render break after the breath
			insert(lastSpaceAt+1, RENDER_BREAK, 0, 0)

			-- backup processing to after the break that was just inserted
			pos = lastSpaceAt+1

		end

	end
end



-- Copy the segments delimited by breaks into the output buffer, and
-- sends to be rendered
local function renderSegments()

	print("RENDER SEGMENTS ----")

	-- clear the ouput buffers
	outIndex = {}
	outLength = {}
	outStress = {}

	local pos = 1
	local outPos = 1

	-- loop until the terminator is reached
	while true do

		-- get the phoenem
		local p = pIndex[pos]

		if p == LINE_TERMINATOR then
			-- insert a line terminator and render
			debugPrint("SEGMENT ENDED WITH LINE TERMINATOR --------")
			outIndex[outPos] = p
			renderPhonemes(outIndex, outLength, outStress)

			-- exit
			return

		elseif p == RENDER_BREAK then
			-- insert a line terminator and render
			debugPrint("SEGMENT ENDED WITH RENDER BREAK --------")
			outIndex[outPos] = LINE_TERMINATOR
			renderPhonemes(outIndex, outLength, outStress)

			-- reset position in output buffer
			outPos = 1

		elseif p == getPhoneIndex(" ") then
		-- FIXME: shouldn't this skip everything with a length zero?
		-- skip spaces

		else
			-- copy phoneme paremeters to output array
			outIndex[outPos] = pIndex[pos]
			outLength[outPos] = pLength[pos]
			outStress[outPos] = pStress[pos]

			-- advance position in the output array
			outPos = outPos + 1

		end

		-- move ahead
		pos = pos + 1

	end

end




-- Convert a string of phonemes into the buffer
local function parsePhonemes(s)

	-- clear the buffer
	pIndex = {}
	pStress = {}
	pLength = {}

	local pos = 0

	-- loop through the data
	local i = 1
	while i <= string.len(s) do

		-- stress marker?
		local token = string.sub(s, i, i)
		if stressValue[token] then
			-- set stress value and move ahead
			pStress[pos] = stressValue[token]
			i = i + 1
		else

			-- search on two character match
			local token = string.sub(s, i, i+1)
			local index = pIndexByName[token]
			if not index then
				-- search on 1 character match
				token = string.sub(s, i, i)
				index = pIndexByName[token]
				if not index then
					-- error
					error("Unknown phoneme '"..string.sub(s, i-5, i).."' at position "..i.." in '"..s.."'")
				end
			end

			-- print("<"..token..">", index, pData[index].token )

			-- insert token
			pos = pos + 1
			pIndex[pos] = index
			pStress[pos] = 0
			pLength[pos] = 0

			-- move forward in string
			i = i + string.len(token)
		end
	end

	-- insert line terminator
	pos = pos + 1
	pIndex[pos] = LINE_TERMINATOR

end

local function main(s)

	-- init()

	local phonemeText = ""

	-- Parser1 converts English text to Phonemes
	if not phoneticFlag then
		-- convert the english text into phonemes
		phonemeText = reciter( s )
	else
		-- text is already phonemes
		phonemeText = s
	end


	debugPrint("Phonetic input: "..phonemeText)

	-- convert the text into phonemene flags
	parsePhonemes(phonemeText)

	-- if debug, show result of parser 1
	if debugFlag then debugPhonemes() end

	-- replace phonemes with allophonic equivalents if source was english text
	if not phoneticFlag then applyAllophones() end

	-- process
	debugPrint("copyStress...")
	copyStress()

	debugPrint("setPhonemeLength")
	setPhonemeLength()

	debugPrint("adjustLengths...")
	adjustLengths()

	debugPrint("extendPhonemeParts...")
	extendPhonemeParts()

	debugPrint("insertBreaks...")
	insertBreaks()

	-- if debug, show results
	if debugFlag then debugPhonemes() end

	-- render the phonemes
	renderSegments()


end

-- normalize the data
function normalize(buffer)
	-- find the largest value in the buffer
	local max = buffer[1]
	for i = 2, #buffer do
		max = math.max(max, math.abs(buffer[i]))
	end

	-- divide all values by max, so the
	-- largest value is 1
	for i = 1, #buffer do
		buffer[i] = buffer[i]/max
	end
end


---------------------
-- RUN THE PROGRAM --
---------------------

-- run the synthesizer
main(inText)

-- normalize the audio data
normalize(outBuffer)

-- output the result
writeWaveFile( "sam_out.wav", 24, outBuffer )

print("Done")
