
Main Program
/* Open character set file
10 OPEN #1,4,0,"D:TELEN.SET"
/* ICCOM = Read Binary
POKE 850,7
/* Buffer length = $400
POKE 856,0:POKE 857,4
/* Buffer address = $9800
POKE 852,0:POKE 853,152
/* Execute IOCB
FOUR=USR(ADR("hÅLVfl"))
 PLA /* pull argument count
 LDX #$16 /* set IOCB number
 JMP $E456 /* call CIOV
CLOSE #1

/* Point to new character set
11 POKE 756,152
/* Initialize variables
900 FOUR=4:FIVE=5
OPEN #1,FOUR,0,"D:TELINI.DAT"
910 INPUT
#1,ONE,TWO,THREE,ZERO,C128,C20,UPPLT,LFPLT,CLWND,WTCLR,PAUSE,MOND,NEWP,MNS
TR,TRSR,TRSR0,SPEC,ARRW
920 INPUT
#ONE,GTCHR,RSTAT,RSPLL,RHITS,MNSTR0,NEWP0,SS,TF,SC,LG,SP,FNUP,FNLF,FNS
/* Turn off cursor
1000 POKE 752,ONE
/* Initialize constants
TEN=TWO*FIVE:SIX=TWO*THREE:SEV=SIX+ONE:EIG=SEV+ONE:NIN=EIG+ONE
/* Initialize strings
1015 DIM
S$(18),CM$(13),MO$(160),MI$(30),MA$(NIN),SP$(C20),IN1$(C20),IN2$(SIX),IN3$(NIN),C$(TEN)
1020 DIM
NM$(C20),D$(C20),CR$(23),SPF$(44),M$(EIG),FO$(FOUR),TRS$(30),FNT$(FIVE*FIVE),RT$(ONE)
/* Initialize arrays and variables
1100 DIM P(THREE,THREE),S(FIVE),SF(TEN+ONE),I(TEN),B(FOUR),EX(100),B$(24)
1103 DIM FM(C20),FL(C20),FH(C20):XO=1.6915:YO=1.4278:ZO=1.2462:W0=4694
/* Initialize string
1120 INPUT #ONE,S$:INPUT #ONE,FO$:INPUT #ONE,TRS$:INPUT #ONE,CM$:INPUT
#ONE,B$:INPUT #ONE,SPF$:INPUT #ONE,FNT$
/* Move cursor 23 spaces right
1125 CR$="23 Right Arrow"
/* EOL character
:RT$=CHR$(155):
/* Displays “Welcome to Telengard Would you like to: [S]tart a new character or”
FOR Q=ONE TO NIN:INPUT #ONE,MO$:PRINT MO$:NEXT Q
/* Initialize monster list
1135 MO$="GNOLL KOBOLD SKELETONHOBBIT ZOMBIE ORC FIGHTER MUMMY ELF
GHOUL DWARF TROLL WRAITH "
1145 MO$(105)="OGRE MINOTAURGIANT SPECTER VAMPIRE DEMON DRAGON "
/* Close initialization file
1190 CLOSE #ONE:
/* Open keyboard channel
OPEN #ONE,FOUR,ZERO,"K:"
/* Print rest of startup message
1214 PRINT " Read in an old one?";
/* Wait for a key to be pressed
1215 GOSUB GTCHR:IF C$="&" THEN 1215
/* If player didn’t press R goto 1400
1216 IF C$<>"R" THEN PRINT "START":GOSUB PAUSE:GOTO 1400
/* Read in old game
/* Get the players name
1220 PRINT "Read":PRINT "Your name?";:GOSUB 16200:NM$=D$:PRINT
/* Create file name to load player from
1222 MI$="D:":MI$(THREE)=NM$:IF LEN(MI$)>TEN THEN MI$=MI$(ONE,TEN)
1225 MI$(LEN(MI$)+ONE)=".PLR"
/* Open player save file
TRAP 1235:OPEN #TWO,FOUR,ZERO,MI$:TRAP TRSR0:GOTO 1245
/* Error opening character file
1235 PRINT "%Can't read char"
/* Restart game
1240 CLOSE #TWO:GOSUB PAUSE:CLR :RUN
/* Read saved game
1245 INPUT #TWO,NM$:PRINT NM$;" found"
1250 INPUT #TWO,VS
1255 FOR I=ZERO TO FIVE:INPUT #TWO,Q:S(I)=Q:NEXT I
1260 INPUT #TWO,LV:INPUT #TWO,GD:INPUT #TWO,TG:INPUT #TWO,EX:INPUT #TWO,CH
1265 INPUT #TWO,HP:INPUT #TWO,CX:INPUT #TWO,CY:INPUT #TWO,CZ:INPUT #TWO,SU
1270 INPUT #TWO,CS:FOR I=ONE TO TEN:INPUT #TWO,Q:I(I)=Q:INPUT #TWO,Q:SF(I)=Q:NEXT
I:INPUT #TWO,Q:SF(11)=Q
1275 FOR I=ONE TO C20:INPUT #TWO,Q:FM(I)=Q:INPUT #TWO,Q:FL(I)=Q:INPUT
#TWO,Q:FH(I)=Q:NEXT I
1280 GOSUB PAUSE:FOR I=ONE TO FOUR:INPUT #TWO,Q:B(I)=Q:NEXT I
1282 FOR I=ZERO TO 100:INPUT #TWO,Q:EX(I)=Q:NEXT I
1285 IF CZ=ZERO THEN CZ=ONE
1290 IF CZ<ZERO THEN GOTO 1235
1295 PRINT NM$;" read":CLOSE #TWO:IF MI$(THREE,FOUR)="SV" THEN 1298
/* Delete save file
1296 XIO 33,#TWO,ZERO,ZERO,MI$
1298 CLOSE #TWO
1300 GOSUB PAUSE:PRINT "}":GOSUB RSTAT:GOSUB 30025:SY=ONE:GOTO NEWP
/* New character
1400 PRINT "}<RET> to use stats"
/* Generate the six stats from 3 to 18 and print the stat name and value
1500 FOR I=ZERO TO FIVE:Q=ZERO:FOR Q1=ONE TO
THREE:Q=Q+INT(RND(ONE)*SIX+ONE):NEXT Q1:S(I)=Q
1510 PRINT S$(I*THREE+ONE,I*THREE+THREE);" ";S(I);" ":NEXT I
/* If player doesn’t press enter, regenerate stats
1515 GOSUB GTCHR:IF C$<>RT$ THEN 1400
/* Set hit points to constitution
1520 HP=S(THREE):
/* Set current hit points to max hit points
CH=HP:
/* Set gold and saved gold to zero
GD=ZERO:TG=GD:
/* Set Dungeon level to 1
CZ=ONE:
/* Set player level to 1
LV=CZ:
/* Clear inventory
FOR I=ONE TO TEN:I(I)=TG:SF(I)=GD:NEXT I:SF(11)=ZERO
/* Initialize box combination
1600 GOSUB 19800:
/* Set starting position in dungeon
CX=TWO:CY=70:
/* Initialize experience, spell units and current spell units
EX=ZERO:SU=ONE:CS=ONE:
/* Get players name
PRINT "}Your name noble sir? ";
1602 GOSUB 16200:IF D$="" THEN D$="DEMO":PRINT D$;
1604 NM$=D$:PRINT
/* Pad name to 7 characters
1605 IF LEN(NM$)<SEV THEN NM$(LEN(NM$)+ONE)=" ":GOTO 1605
/* Start of play
1610 PRINT "You descend into the";RT$;"depths of TELENGARD";RT$;"Beware ...”
1630 GOSUB PAUSE:GOSUB PAUSE:PRINT "}":L=ONE:GOSUB RSTAT:GOSUB 30200:GOSUB
15000:SY=ONE:GOTO MNSTR
/* Start of each turn
/* Decrement spell effects
2000 FOR I=ONE TO 11:SF(I)=SF(I)-ONE:NEXT I:
L=CZ:GOSUB CLWND:IF SY=ONE THEN GOSUB 15000
2010 SY=ONE:
/* Handle ring of regeneration
IF (I(SIX)=ZERO) OR (CH=HP) THEN GOTO MNSTR
2015 CH=CH+I(SIX):IF (CH>HP) THEN CH=HP
2020 GOSUB RHITS
/* Handle Encounter
/* MNSTR:
/* 30% of the time there is a monster unless Time Stop is in effect
3000 IF RND(ONE)>THREE/TEN OR SF(NIN)>ZERO THEN GOTO TRSR
/* No monster 80% of the time if Invisible
3001 IF SF(SIX)>ZERO AND RND(ONE)>ONE/FIVE THEN GOTO TRSR
/* Show monster on screen
3005 POKE SC+332,THREE:
/* Get monster type
M=INT(RND(ONE)*C20+ONE): IF SF(SEV)>ZERO AND M<FIVE THEN 3005
3006 IF INT(RND(ONE)*C20+ONE)<=I(FOUR) THEN 20900
/* Calculate monster level
3007 ? "ý";:ML=INT((RND(ONE) (THREE/TWO))*(CZ*TWO+TWO)+ONE):
/* Determine if undead
GOSUB 20700
/* If Astral Walk is in place and monster is under, 20% of the time select a new monster type
3008 IF SF(FOUR+FOUR)>ZERO AND UN=ONE AND RND(ONE)>FOUR/FIVE THEN 3005
/* Display monster name
3010 GOSUB 16500:GOSUB 19700:PRINT "YOU ENCOUNTER A LVL ";ML;" ";M$
/* Handle time stop
3011 IF SF(NIN)>ZERO AND M<16 AND M<>11 THEN GOSUB WTCLR:GOTO TRSR
/* Display monster level
3012 I=PEEK(SP):POKE SP,18:PRINT
3013 PRINT CR$;"LVL ";ML;" ";M$
3015 POKE SP,I-ONE:PRINT :
/* Monster hit points
MH=INT((RND(ONE) (ONE/TWO))*ML*M+ONE):L=ML
/* 5% of the time the monster will take one of three random actions
3017 IF RND(ONE)>0.95 THEN ON RND(ONE)*THREE+ONEGOTO3030,3045,3072
/* If the monster isn’t a fighter then it won’t heal you
3020 IF M<>NIN THEN 3040
/* Second change at healing based on your charisma
3025 IF RND(ONE)>0.04*S(FIVE) THEN 3040
/* Monster heals you
3030 PRINT "The ";M$;" likes your body":PRINT "He heals you to full strength":CH=HP
3037 GOSUB RSTAT:GOSUB PAUSE:GOSUB 20600:POKE SC+332,ZERO:GOTO TRSR
/* If monster is a hobbit it attempts to steal based on your charisma
3040 IF M<>FOUR OR RND(ONE)<S(FIVE)/C20 THEN 3070
/* Monster steals something
3045 PRINT "The ";M$;" makes a quick move":GOSUB PAUSE
/* Check if anything to steal
3050 FOR I=ONE TO TEN:IF I(I)>ZERO THEN 3054
3052 NEXT I:PRINT "You have nothing he wants to steal!":GOTO 3037
/* Randomly pick an item to steal
3054 I=INT(RND(ONE)*TEN+ONE):IF I(I)<ONE THEN 3054
/* Get item name
3055 MI=I:GOSUB 28200
/* Item is stolen
3056 PRINT "He steals ";:IF I<EIG THEN PRINT "your ";MI$:I(I)=ZERO:GOTO 3037
3060 PRINT "a ";MI$:I(I)=I(I)-ONE:GOTO 3037
/* If monster is a dragon determine if he will give you something based on your charisma
3070 IF M<>C20 OR RND(ONE)*30>S(FIVE) THEN 3090
/* Randomly pick one of the first 7 items and check if it’s level is greater then the monsters
3072 I=INT(RND(ONE)*SEV+ONE):IF I(I)>=ML THEN 3090
/* Increase the level of the item
3074 C=ML-I(I):C=INT(RND(ONE)*C+ONE):I(I)=I(I)+C
3076 PRINT "The ";M$;" likes you!":GOSUB WTCLR
3077 MI=I:GOSUB 28200
3078 PRINT "He gives you a ";MI$;" +";I(I):GOSUB RSTAT:GOSUB WTCLR
3080 GOSUB 20600:GOTO TRSR
/* If you dexterity is high enough you might get an instant kill
3090 IF RND(ONE)>ONE/TWO+S(FOUR)*0.02 THEN 3300
/* Ask player what to do
3100 PRINT "Fight, Cast, or Evade:";:GOSUB GTCHR
3102 IF C$="&" THEN PRINT "WAIT"
/* Be sure the player input a valid command
3105 FOR I=ONE TO FOUR:IF FO$(I,I)=C$ THEN 3115
3110 NEXT I:PRINT :PRINT "The ";M$;" is not amused":GOSUB WTCLR:GOTO 3100
/* Jump to appropriate command handler
3115 ON I GOTO3200,3400,3700,3300
/* ----------------------------------------------------------------------------------------------------------
/* Fight
/* ----------------------------------------------------------------------------------------------------------
/* (RND(0-20) + Level + Armor level + strenght)/2 + (4 if using strenght potion) >= 10 then hit
3200 I=INT(RND(ONE)*C20)+LV+I(ONE)+S(ZERO)/TWO:PRINT "Fight"
3202 IF SF(ONE)>ZERO THEN I=I+FOUR
3204 IF I<TEN THEN PRINT "You missed...":GOTO 3300
/* RND(0-8) + (RND * Level * 2 + Sword + Sword Level + 1) + (5 if using strength potion) = damage
3205 I=INT(RND(ONE)*EIG+RND(ONE)*LV*TWO+I(ONE)+ONE):IF SF(ONE)>ZERO THEN
I=I+FIVE
3207 PRINT "You do ";I;" points damage"
/* Return monster hit points and see if still alive
3210 MH=MH-I:IF MH>ZERO THEN 3300
/* Monster is dead
3220 PRINT "It died...";
3223 POKE SC+332,ZERO:
/* Calculate experience
E=ML*M*TEN:EX=EX+E:GOSUB PAUSE
3225 GOSUB 20600:PRINT "You gain ";E;" experience points":GOSUB RSTAT:GOSUB 18000
3230 GOSUB WTCLR:GOTO 3900
/* Initialize monster bonuses
3300 DB=ONE:PA=ZERO:DR=ZERO:MB=ZERO:GOTO 3330
/* RND(0-20) + Monster Level – player armor level – player shield level + monster bonus
3305 I=INT(RND(ONE)*C20)+ML-I(TWO)-I(THREE)+MB
/* If SPECTER, VAMPIRE, or DEMON and protection from evil spell you get a bonus of -6 to hit
3306 IF M>16 AND M<C20 AND SF(FOUR)>ZERO THEN I=I-FOUR-TWO
/* Check for hit
3310 IF I<TEN THEN PRINT "It missed...";:GOSUB WTCLR:GOTO 3100
/* Damage = (RND(0-8) + RND() * Monster level * 2 + 0) * Monster Bonus
3315 I=INT((RND(ONE)*EIG+RND(ONE)*ML*TWO+ONE)*DB):PRINT "It does ";I;" points damage";
/* Reduce player hit points and check for death
3320 CH=CH-I:GOSUB RHITS:IF CH<ONE THEN 9000
3321 GOSUB PAUSE:
/* Check for special effects
IF RND(ONE)>DR OR SF(FOUR)>ZERO THEN 3326
/* Drain level
3322 GOSUB CLWND:PRINT "It drains a level!!!":EX=INT(EX/TWO)
3323 IF LV=ONE THEN EX=-ONE
3324 GOSUB 18000:GOSUB WTCLR:GOTO 3100
/* Paralyzed, monster gets another attack
3326 IF RND(ONE)>PA THEN GOSUB CLWND:GOTO 3100
3327 GOSUB CLWND:PRINT "You're paralized!!!!";:GOSUB WTCLR:GOTO 3300
/* Wraith drains level 1/10 of the time
3330 IF M=13 THEN DR=ONE/TEN:GOTO 3305
/* Specter drains level 1/5 of the time
3335 IF M=17 THEN DR=TWO/TEN:GOTO 3305
/* Vampire drains level and paralize 3/10 of the time
3340 IF M=18 THEN DR=THREE/TEN:PA=DR:GOTO 3305
/* Ghoul paralizes ½ of the time
3345 IF M=TEN THEN PA=ONE/TWO:GOTO 3305
/* Demon attack
3350 IF M<>19 THEN 3365
3352 GOSUB WTCLR
3355 IF RND(ONE)>THREE/FIVE THEN PRINT "It uses it's
SWORD!!!":MB=FOUR:DB=THREE:GOTO 3305
3360 PRINT "It uses it's WHIP!!":MB=TWO:DB=TWO:GOTO 3305
/* Dragon attack
3365 IF M<>C20 THEN 3305
3370 IF RND(ONE)>THREE/TEN THEN MB=FIVE:DB=TWO:GOTO 3305
3375 PRINT "The DRAGON breaths FIRE!!!";:GOSUB WTCLR
3378 I=INT(RND(ONE)*C20*ML+ONE):IF RND(ONE)<S(THREE)/C20 THEN 3382
3380 PRINT "You partially dodge it":I=INT((I+ONE)/TWO)
3382 PRINT "You burn for ";I;" points damage":GOTO 3320
/* Cast
3400 D=ONE:GOTO 21000
/******************************************************
/* Evade
/******************************************************
3700 Q=INT(RND(ONE)*18+ONE):PRINT "EVADE"
/* Determine success based on dexerity and Elven Boots item
3705 IF Q<S(FOUR)+I(FIVE) THEN 3710
/* Evade failed
3707 PRINT "You're rooted to the spot":GOSUB WTCLR:GOSUB 16500:GOTO 3300
/* Randomly pick an evade direction
3710 GOTO 3720+TEN*INT(RND(ONE)*FOUR)
/* Try to evade north
3720 C=ONE:IF USR(FNUP,LC)<THREE THEN 3795
/* Try to evade south
3730 C=TWO:IF USR(FNUP,P(TWO,ONE))<THREE THEN 3795
/* Try to evade west
3740 C=THREE:IF USR(FNLF,LC)<THREE THEN 3795
/* Try to evade east
3750 C=FOUR:IF USR(FNLF,P(ONE,TWO))<THREE THEN 3795
/* Try to evade north again
3760 C=ONE:IF USR(FNUP,LC)<THREE THEN 3795
/* Try to evade south again
3770 C=TWO:IF USR(FNUP,P(TWO,ONE))<THREE THEN 3795
/* Try to evade west again
3780 C=THREE:IF USR(FNLF,LC)<THREE THEN 3795
/* Couldn’t evade
3790 GOTO 3707
/* Move in evade direction
3795 GOSUB 20600:GOSUB CLWND:GOTO 8120
/* When a monster has been killed it will have treasure 50% of the time
3900 IF RND(ONE)>ONE/TWO THEN 4002
/* ----------------------------------------------------------------------------------------------------------
/* Treasure
/* ----------------------------------------------------------------------------------------------------------
TRSR:
/* 1/5 of the time there is treasure
4000 IF RND(ONE)>ONE/FIVE THEN GOTO SPEC
/* 15% of the time there is a trap
4002 T=ZERO:IF RND(ONE)>0.85 THEN T=ONE
4005 POKE SC+332,FOUR:GOSUB 16500:C=ZERO:
/* Skip to treasure test 30% of the time
IF RND(ONE)>SEV/TEN THEN 4100
/* ----------------------------------------------------------------------------------------------------------
/* Handle normal treasure
/* ----------------------------------------------------------------------------------------------------------
/* Display treasure name
4007 PRINT "You see some ";:I=INT(RND(ONE)*FIVE)
4010 Q=FIVE+ONE:PRINT TRS$(I*Q+ONE,I*Q+Q):PRINT "<RET> to pick up:";
/* You detect a trap if detect trap spell is in effect or 10% of the time otherwise
4011 IF T=ONE THEN IF SF(TWO)>ZERO AND RND(ONE)>ONE/TEN THEN PRINT :PRINT "You
detect TRAPS!";
/* Get user input and check if return was pressed
4012 GOSUB GTCHR:IF C$=RT$ THEN 4020
/* Player doesn’t pick up treasure
4015 PRINT "Leave it":GOSUB WTCLR:GOTO SPEC
/* Player picks up treasure, check if there is no trap
4020 PRINT "Snarf it":IF T=ZERO THEN 4040
/* Handle trap damage
4025 Q=INT(RND(ONE)*THREE*L+ONE):GOSUB CLWND
4030 PRINT "It's TRAPPED!":PRINT "You suffer ";Q;" points damage"
4035 CH=CH-Q:GOSUB RHITS:IF CH<ONE THEN GOSUB PAUSE:GOTO 9000
/* Get gold from treasure
4040 J=INT(RND(ONE)*I*L*C20*TEN+ONE):PRINT "It's worth ";J;" gold"
4045 GD=GD+J:IF I=ZERO AND RND(ONE)>FOUR/FIVE THEN GOSUB RSTAT:GOSUB
CLWND:GOTO 4200
4050 GOSUB RSTAT:GOTO 4900
/* Skip to items 50% of the time
4100 IF RND(ONE)>ONE/TWO THEN 4200
/* Handle treasure chest
4105 PRINT "You have found a Treasure Chest!!"
4110 PRINT "<RET> to open it:";
/* You detect a trap if detect trap spell is in effect or 10% of the time otherwise
4115 IF T=ONE THEN IF SF(TWO)>ZERO AND RND(ONE)>ONE/TEN THEN PRINT :PRINT "You
detect TRAPS!";
/* Get player input and check for Return key
4120 GOSUB GTCHR:IF C$<>RT$ THEN PRINT "Ignore it":GOSUB WTCLR:GOTO SPEC
/* Open chest
4125 PRINT "Open it":IF T=ZERO THEN 4140
/* Handle chest trap
4130 GOSUB WTCLR:PRINT "Chest EXPLODES!!":I=INT(RND(ONE)*TEN*CZ+ONE):CH=CH-I
4135 PRINT "You suffer ";I;" points damage":GOSUB RHITS:IF CH<ONE THEN 9000
/* Calculate amount of gold in chest
4140 GOSUB WTCLR:I=INT(RND(ONE)*1000*L TWO+ONE)
/* 10% of the time there is nothing inside
4145 IF RND(ONE)>NIN/TEN THEN PRINT "Inside, there is only cobwebs...":GOTO 4900
/* Get gold
4150 PRINT "Inside is ";I;" gold pieces!":GD=GD+I:GOSUB RSTAT:IF RND(ONE)>ONE/TWO THEN
4900
4155 C=ONE:GOSUB WTCLR
/* Found an item
4200 I=INT(RND(ONE)*TEN+ONE):PRINT "You see a ";:MI=I:GOSUB 28200
/* Calculate item level
4205 J=INT(RND(ONE) (ONE/TWO)*(L+ONE)+ONE):IF I>SEV THEN PRINT MI$:GOTO 4215
4210 PRINT MI$;" +";J
/* Prompt player to pick it up
4215 PRINT "<RET> to pick it up:";:GOSUB GTCHR:IF C$=RT$ THEN 4225
4220 PRINT "Leave it":GOTO 4240
/* Check if it is cursed
4225 IF I*T<>ONE THEN 4235
/* Handle curse damage
4230 PRINT :PRINT "It is CURSED!!":I=INT(RND(ONE)*I*FIVE+ONE)
4232 PRINT "You suffer ";I;" damage points";:CH=CH-I:GOSUB RSTAT:IF CH<ONE THEN 9000
4234 GOTO 4900
/* Add item to inventory
4235 PRINT "It's yours!":I(I)=I(I)+ONE:IF I<EIG THEN I(I)=J
4237 GOSUB RSTAT
4240 IF C=ONE AND RND(ONE)>ONE/TWO THEN GOSUB WTCLR:GOTO 4200
4900 POKE SC+332,ZERO:GOSUB WTCLR
/* 50% of the time evaded monsters will get closer
4910 IF RND(ONE)>ONE/TWO THEN GOSUB MNSTR0:IF M<>ZERO THEN 3010
/* ----------------------------------------------------------------------------------------------------------
/* Special stuff
/* ----------------------------------------------------------------------------------------------------------
/* Special items
/* 1 = Inn
/* 2 = Pit
/* 3 = Teleporter
/* 4 = Stairway
/* 5 = Alter
/* 6 = Fountain
/* 7 = Cube
/* 8 = Throne
/* 9 = Box
/* Get items one level up
6000 X=CX:Y=CY:Z=CZ-ONE:GOSUB 10010:J=USR(FNS,HI):
/* Get item on current level
I=USR(FNS,LC)
/* If on level one, nothing one level up */
6005 IF CZ=ONE THEN J=ZERO
/* Be sure special item is less then 9 */
6010 Q=FOUR+FIVE:IF I>Q THEN I=I-Q:GOTO 6010
6012 IF J>Q THEN J=J-Q:GOTO 6012
/* If on level 50, no down stairs */
6013 IF CZ=FIVE*TEN AND I=FOUR THEN I=ZERO
/* No item on this level, no stair on previous, then no special */
6014 IF I=ZERO AND J<>FOUR THEN GOTO ARRW
/* Stair on previous level, do stairs */
6015 IF J=FOUR THEN GOTO 6500
/* Do special objects*/
6020 ON I GOTO 6100,6300,6400,6500,6600,6700,6800,6900,7100
/************************************************************
/* Inn
/************************************************************
6100 IF CZ<>ONE THEN 6200
6102 J=FOUR:GOTO 6500
6103 IN1=CY*CY:IN2=CX+CY:IN3=CX*THREE+CY*SEV:GOSUB 28100
6105 GOSUB CLWND:GOSUB 29000:POKE SC+373,56
/* Draw Inn
6106 POKE SP,ZERO:POKE SP+ONE,ZERO:PRINT “
½½½½¿¿¿¿¿¿¿¿¿¿¿¿
¨ª
½¾¾¾
¦¦§§
½¾¾¾¾¾
¦¦ §§
½¾¾¾¾¾¾
ÄÄÄÄÄÄ
¾¾¾¾¾¾
";
6107 PRINT "½Ä½¾Ä½¾ÄÄÄÄÄÄ¼¾Ä¼¾Ä";:PRINT "½½¿¿¿¿¿
":PRINT
6108 PRINT " ";IN1$:PRINT " ";IN2$:PRINT " ";IN3$
6110 GOSUB 16600:PRINT "INN";:GOSUB 16500:PRINT "You have found the":PRINT " ";IN1$;"
";IN2$;" ";IN3$
/* Add carried gold to expereince and total gold, then clear to 0
6111 PRINT "They cash in your gold":EX=EX+GD:TG=TG+GD:GD=ZERO:GOSUB RSTAT
6115 GOSUB 18000:GOSUB 30200:GOSUB CLWND:CH=HP:PRINT "You have ";TG;" in the safe"
/* Clear all spell effects
6116 FOR I=ZERO TO 11:SF(I)=ZERO:NEXT I
/* Restore spell points
6119 CS=SU:PRINT "You spend the night":GOSUB PAUSE
/* Prompt for save or re-enter dungeon
6120 PRINT "You feel better":GOSUB RSTAT:GOSUB CLWND
6125 PRINT "Press <RET> to return to the dungeon,"
6130 PRINT "<^S> to save character on disk:";
6135 GOSUB GTCHR:IF C$="&" THEN 6135
6140 IF C$="" THEN PRINT :GOTO 8600
6142 IF C$<>RT$ THEN PRINT "???":GOSUB WTCLR:GOTO 6125
6145 PRINT "Reenter":GOSUB WTCLR:CZ=ONE:GOTO NEWP
6200 GOSUB 16600:PRINT "
 ":GOSUB 16500:PRINT "You feel heavy for a moment"
6210 GOSUB PAUSE:CZ=CZ-ONE:GOTO NEWP
/************************************************************
/* Pit
/************************************************************
/* On 50th level, can’t go any deeper
6300 IF CZ=50 THEN 6200
6302 GOSUB 16600:PRINT "":GOSUB 16500:PRINT "You see a pit"
/* Check for levitate spell
6303 IF SF(FIVE)>ZERO THEN PRINT "You are hovering above a pit":GOTO 6310
/* RND(0-20) > Dexterity + Elven boot level
6305 IF INT(RND(ONE)*C20)>S(FOUR)+I(FIVE) THEN 6330
/* Ask if player wants to descend
6310 PRINT "Do you want to descend?";:GOSUB GTCHR:IF C$="Y" THEN 6320
6315 PRINT "No":GOSUB WTCLR:GOTO ARRW
6320 PRINT "Yes":GOTO 6335
/* Fell in pit. Handle damage and move down one level
6330 PRINT "You fall in!!":L=THREE:GOSUB NEWP0:GOSUB PAUSE
6335 CZ=CZ+ONE:GOTO NEWP
/***************************************************************
/* Teleporter
/***************************************************************
/* Display message and clear monster stack
6400 GOSUB 16600:PRINT " * ":GOSUB 16500
6403 PRINT "ZZAP!! You've been teleported...":GOSUB 30200
/* Calculate new position
6405 IF USR(LG,ONE,CX+CY,ONE)=ZERO THEN CZ=CZ-ONE:IF
USR(LG,ONE,CX+CY,TWO)=TWO THEN CZ=CZ+TWO
6410 CX=CX+CZ*EIG+CY*13:CY=CY+CZ*SIX+CX*17
6415 Q=C20*TEN:IF CX>Q THEN CX=CX-Q:GOTO 6415
6420 IF CY>Q THEN CY=CY-Q:GOTO 6420
6425 IF CZ=ZERO THEN CZ=ONE
6430 IF CZ>FIVE*TEN THEN CZ=FIVE*TEN
6435 IF RND(ONE)>FOUR/FIVE THEN 6405
6440 GOTO NEWP
/************************************************************
/* Stairway
/************************************************************
/* Check for up stairway
6500 GOSUB 16600:IF J=FOUR THEN PRINT "[";
/* Check for down stairway
6505 IF I=FOUR THEN PRINT "]";
6510 GOSUB CLWND:PRINT "You have found a circular stairway"
/* There is an Inn above
6512 IF J=FOUR AND CZ=ONE THEN PRINT "You see LIGHT above"
/* Prompt the player
6515 PRINT "Do you want to ";:IF J=FOUR THEN PRINT "go Up, ";
6525 IF I=FOUR THEN PRINT "go Down, ";
6530 PRINT :PRINT "or Stay on the same level?";:GOSUB GTCHR
6532 IF C$="&" THEN C$="S"
/* Check if you can go in the select direction
6535 IF (I<>FOUR AND C$="D") OR (J<>FOUR AND C$="U") THEN 6550
/* Go up
6540 IF C$="U" THEN PRINT "Up";:CZ=CZ-ONE:IF CZ>ZERO THEN GOTO NEWP
/* Go to Inn
6542 IF CZ<=ZERO THEN 6103
/* Go down
6545 IF C$="D" THEN PRINT "Down";:CZ=CZ+ONE:GOTO NEWP
/* Stay
6547 IF C$="S" THEN PRINT "Stay";:GOSUB CLWND:GOTO ARRW
6550 PRINT "???";:GOTO 6510
/************************************************************
/* Alter
/************************************************************
/* Display message
6600 GOSUB 16600:PRINT "":GOSUB CLWND:PRINT "You have found a Holy Altar"
6605 PRINT "Press <RET> to worship";:GOSUB GTCHR:IF C$=RT$ THEN 6630
/* Don’t workship. 30% of the time create an undead monster
6610 IF RND(ONE)>SEV/TEN THEN GOTO ARRW
6615 PRINT :PRINT "Dirty Pagan TRASH!";:GOSUB 16600:PRINT "# "
6620 M=INT(RND(ONE)*C20+ONE):GOSUB 20700:IF UN=ZERO THEN 6620
6625 GOSUB WTCLR:GOTO 3007
/* Worship
6630 PRINT :PRINT "Press <RET> to donate money";:GOSUB GTCHR
6632 IF C$<>RT$ THEN 6610
/* Get amount of gold to donate
6634 PRINT :PRINT "How much gold?";:GOSUB 20800:GOSUB CLWND
6636 IF C>GD THEN PRINT "You don't have that much!";:GOTO 6615
/* If not enough gold, create a monster
6640 IF C<FIVE*TEN*CZ THEN PRINT "";:GOTO 6615
/* Deduct gold, see if workshipping gives a bonus
6645 GD=GD-C:GOSUB RSTAT:IF RND(ONE)<C/(GD+C) THEN 6660
6655 PRINT "Thank you for your donation":GOSUB WTCLR:GOTO ARRW
/* Randomly increase a spell effect
6660 I=INT(RND(ONE)*SEV+ONE):IF SF(I)<ZERO THEN SF(I)=ZERO
6665 SF(I)=SF(I)+INT(RND(ONE)*TEN*TEN*C/(GD+C)+ONE)
6680 PRINT "You have been heard":GOSUB WTCLR:GOTO ARRW
/************************************************************************
/* Fountain
/************************************************************************
/* Display message
6700 GOSUB 16600:PRINT " ": GOSUB CLWND: PRINT "You have found a fountain"
/* Determine color of water
6701 C=INT(RND(ONE)*FIVE+ONE)
6702 PRINT "with running ";FNT$(C*FIVE-FOUR,C*FIVE);" water"
/* Prompt to drink
6705 PRINT "<RET> to drink some:";:GOSUB GTCHR:IF C$=RT$ THEN 6715
/* Clear screen and continue game
6710 GOSUB CLWND:GOTO ARRW
/* 3/5 of the time something skip to special types of fountain
6715 GOSUB CLWND:IF RND(ONE)>THREE/FIVE THEN 6750
/* Skip healing effect
6717 IF RND(ONE)>ONE-C*THREE/C20 THEN 6730
/* Healing fountain. Add (3 * dungeon level) + 1 to hit points
6720 PRINT "You feel better":GOSUB PAUSE:I=INT(RND(ONE)*THREE*CZ+ONE)
6725 PRINT "You heal ";I;" hit points":CH=CH+I:IF CH>HP THEN CH=HP
6727 GOSUB RHITS:GOSUB WTCLR:GOTO ARRW
/* Skip poison fountain
6730 IF RND(ONE)>THREE*C/C20 THEN 6745
/* Poison Fountain. Subtract (3 * dungeon level) + 1 from hit points. Check if dead
6735 PRINT "It's POISON!!!":I=INT(RND(ONE)*THREE*CZ+ONE):PRINT "You lose ";I;" hit points"
6740 CH=CH-I:GOSUB RHITS:IF CH<ONE THEN 9000
6742 GOSUB WTCLR:GOTO ARRW
/* Got experience points
6745 GOSUB 20100:GOSUB RSTAT:GOSUB WTCLR:GOTO ARRW
/* Feel refreshed by no other effect
6750 MI$="You feel refreshed!":IF RND(ONE)>TWO/FIVE THEN PRINT MI$:GOSUB
WTCLR:GOTO ARRW
/* Check for drunk fountain
6752 IF RND(ONE)>ONE/TWO THEN 6780
/* Check for lose items fountain
6755 IF RND(ONE)>ONE/TWO THEN 6770
/* Magic fountain, increase number of spells
6760 PRINT "Magic power SURGES through your body":CS=CS+INT(RND(ONE)*FOUR*CZ+ONE)
6765 PRINT "You now have ";CS;" spells":GOSUB RSPLL:GOSUB WTCLR:GOTO ARRW
/* Handle lose item fountain. Remove all items
6770 PRINT "You have been disposessed!!":FOR I=ONE TO TEN:I(I)=ZERO:NEXT I:GOSUB PAUSE
6775 GOSUB WTCLR:PRINT "}":GOSUB RSTAT:GOSUB 15000:GOSUB 30025:GOTO ARRW
/* Handle drunk fountain
6780 PRINT MI$:GOSUB WTCLR:PRINT "Actually you're DRUNK!!"
6785 Q=TEN+ONE:IF SF(Q)<ZERO THEN SF(Q)=ZERO
6786 SF(Q)=SF(Q)+INT(RND(ONE)*16+ONE):GOSUB 15000:GOSUB CLWND:GOTO ARRW
/***********************************************************
/* Cube
/***********************************************************
/* Display message
6800 GOSUB 16600:PRINT " ":GOSUB CLWND:PRINT "You see a large gray misty cube"
6805 PRINT "<RET> to walk in:";:GOSUB GTCHR:IF C$<>RT$ THEN GOTO ARRW
/* 80% of the time you can pick the level to go to
6810 IF RND(ONE)>ONE/FIVE THEN 6815
/* Goto random level
6812 CZ=INT(RND(ONE)*FIVE*TEN+ONE):GOTO 6820
/* Get level number to goto
6815 PRINT :PRINT "A number from 1 to 50:";:GOSUB 20800:PRINT :IF C$="&" THEN 6812
6817 IF C<ONE OR C>FIVE*TEN THEN GOTO 6810
6818 CZ=C
6820 GOSUB 29000:POKE SP,TWO:POKE SP+ONE,ONE:PRINT "You float":PRINT " in
space....";:GOTO NEWP
/**********************************************************
/* Throne
/**********************************************************
/*Show message
6900 GOSUB 16600:PRINT " ":GOSUB CLWND:PRINT "You see a jewel encrusted throne"
6905 PRINT "Do you want to Pry some jewels,"
6910 PRINT "Sit down, Read the runes,":PRINT " or Ignore it:";
6915 GOSUB GTCHR:IF C$="I" OR C$="&" THEN PRINT "Ignore";:GOSUB WTCLR:GOTO ARRW
/* Pry jewels
6920 IF C$<>"P" THEN 6950
/* 30% of the time a monster appears
6925 PRINT "Pry";:GOSUB WTCLR:IF RND(ONE)>SEV/TEN THEN 7030
/* 60% of the time nothing happens
6930 IF RND(ONE)>TWO/FIVE THEN 7025
/* Got the jewel, increase gold
6935 PRINT "They pop into your greedy hands!!":I=INT(RND(ONE)*TEN*TEN*TEN*CZ+ONE)
6940 PRINT "They are worth ";I;" gold":GD=GD+I:GOSUB RSTAT:GOSUB WTCLR:GOTO ARRW
/* Sit
6950 IF C$<>"S" THEN 6985
/* 30% of the time a monster appears
6955 PRINT "Sit";:GOSUB WTCLR:IF RND(ONE)>SEV/TEN THEN 7030
/* 40% of the time nothing happens
6960 IF RND(ONE)>THREE/FIVE THEN 7025
/* 60% of the time your are teleported
6965 IF RND(ONE)>TWO/FIVE THEN 6403
/* 50% of the time you loose ½ experiece
6970 PRINT "A loud ýGONGý sounds!";:GOSUB WTCLR:IF RND(ONE)<ONE/TWO THEN 6975
6973 EX=INT(EX/TWO):IF LV=ONE THEN EX=-ONE
6974 GOSUB 18000:GOTO ARRW
/* If your level is less then or equal to the dungeon level, then you get an experience bonus
6975 IF LV>CZ THEN PRINT "Nothing happens...";:GOTO 7010
6980 EX=INT(1000*TWO^LV+ONE/TWO):GOSUB 18000:GOTO ARRW
/* Read
6985 IF C$<>"R" THEN 7015
6990 PRINT "Read";:GOSUB WTCLR
/* 30% of the time create a monster
6992 IF RND(ONE)>SEV/TEN THEN 7030
/* Determine success based on player’s intelligence
6995 IF RND(ONE)<S(ONE)/C20 THEN 7005
/* Failed to read
7000 PRINT "You don't understand them...";:GOTO 7010
/* Read
7005 PRINT :PRINT "A mysterious magic grips you..":GOSUB 20200
7010 GOSUB WTCLR:GOTO ARRW
7015 PRINT "???";:GOSUB WTCLR:GOTO 6900
7025 PRINT "Nothing happens...";:GOTO 7010
7030 M=INT(RND(ONE)*C20+ONE):GOSUB 19700
7032 PRINT :PRINT "The ";M$;" KING returns!!":GOSUB WTCLR:GOSUB 16600:GOSUB 20700
7035 PRINT "# ":ML=INT(RND(ONE)*CZ*FIVE)+FIVE:GOTO 3010
/***********************************************************************
/* Box
/***********************************************************************
/* Show box
7100 GOSUB 16600:PRINT "  ":GOSUB CLWND
/* Prompt user to press button
7105 PRINT "You see a small box with four colored"
7110 PRINT "lights. Ðush buttons or Égnore:";:GOSUB GTCHR
/* User chooses not to push button
7115 IF C$<>"P" THEN PRINT "Ignore":GOSUB WTCLR:GOTO ARRW
/* Prompt use for which button to press
7120 PRINT "Push":GOSUB WTCLR:CB=ONE
7125 PRINT "Push Òed, Çreen, Ùellow, Âlue":PRINT "or Ótop:";
/* Get input and check if user decides not to push
7130 GOSUB GTCHR:IF C$="S" OR C$="&" THEN PRINT "Stop";:GOSUB WTCLR:GOTO ARRW
/* Find which button was pressed, put name in M$
7135 Q=FOUR+TWO:FOR C=ONE TO FOUR:IF C$=B$(C*Q-FIVE,C*Q-FIVE) THEN M$=B$(C*QFIVE,C*Q):GOTO 7145
7140 NEXT C:
/* Player didn’t enter a valid button
PRINT "???";:GOSUB WTCLR:GOTO 7170
/* Print name of button that was pressed
7145 GOSUB 19705:PRINT M$:
/* Jump if wrong combination
IF B(CB)<>C THEN 7170
/* Next button in combination
7150 CB=CB+ONE:IF CB<FIVE THEN GOSUB WTCLR:GOTO 7125
/* Opened box
7155 PRINT "It opens!!!! ";:GOSUB WTCLR:PRINT "Inside you find jewels worth"
/* Calculate gold in box
7160 GOSUB 19800:I=INT(RND(ONE)*20000*CZ^TWO+ONE):PRINT I;" in
gold!!":GD=GD+I:GOSUB RSTAT
7165 GOSUB CLWND:GOTO ARRW
/* Calculate and display damage done by box
7170 I=INT(RND(ONE)*TWO*CZ+ONE):PRINT "An electric bolt shoots through you!!"
7175 GOSUB WTCLR:PRINT "You suffer ";I;" points damage":CH=CH-I:GOSUB RHITS
/* Check for death
7180 IF CH<ONE THEN 9000
7185 GOSUB WTCLR:GOTO ARRW
ARRW:
8000 M=ZERO:GOSUB CLWND
/* Get input from player
8005 PRINT "->";:GOSUB GTCHR:IF C$="&" THEN C$="S"
/* See if it’s a valid command
8100 FOR C=ONE TO 13:IF CM$(C,C)=C$ THEN 8118
8110 NEXT C
8115 PRINT "--NO";:FOR I=ONE TO 60:NEXT I:GOTO ARRW
8118 IF SF(11)>ZERO AND C<SIX THEN PRINT "You're confused -
>";:C=INT(RND(ONE)*FIVE+ONE)
/* Jump to command
8120 ON C GOTO 8150,8200,8250,8300,8350,8800,8700,8900,8970,8600,8910,8925,8945
/* W = North
8150 Q=USR(FNUP,LC):IF Q>TWO AND SF(EIG)<ONE OR CY=ONE THEN 8115
8155 PRINT "NORTH";:CY=CY-ONE:GOSUB 30100:GOTO NEWP
/* X = South
8200 Q=USR(FNUP,P(TWO,ONE)):IF Q>TWO AND SF(EIG)<ONE OR CY=C20*TEN THEN 8115
8205 PRINT "SOUTH";:CY=CY+ONE:GOSUB 30100:GOTO NEWP
/* A = West
8250 Q=USR(FNLF,LC):IF Q>TWO AND SF(EIG)<ONE OR CX=ONE THEN 8115
8255 PRINT "WEST";:CX=CX-ONE:GOSUB 30100:GOTO NEWP
/* D = East
8300 Q=USR(FNLF,P(ONE,TWO)):IF Q>TWO AND SF(EIG)<ONE OR CX=C20*TEN THEN 8115
8305 PRINT "EAST";:CX=CX+ONE:GOSUB 30100:GOTO NEWP
/* S = Stay
8350 PRINT "STAY":FOR Q=332 TO 334:POKE SC+Q,ZERO:NEXT Q:
/* If you stay evaded monsters get closer
GOSUB MNSTR0:IF M<>ZERO THEN 3010
8355 SY=ZERO:GOTO NEWP
/* Save game
8600 PRINT "Store ";NM$;", ";:IF NM$<>"DEMO" THEN 8610
8605 PRINT :PRINT "%Can not store DEMO character!":GOTO 8670
8610 MI$="D:":MI$(THREE)=NM$
8615 IF MI$(LEN(MI$))=" " AND LEN(MI$)>ZERO THEN MI$=MI$(ONE,LEN(MI$)-ONE):GOTO
8615
8616 MI$(LEN(MI$)+ONE)=".PLR":TRAP 8620:OPEN #TWO,EIG,ZERO,MI$:TRAP TRSR0:GOTO
8630
8620 PRINT :PRINT "%Error opening player file";:CLOSE #TWO:GOSUB PAUSE:GOTO 8670
8630 PRINT #TWO;NM$:PRINT #TWO;4.18:FOR I=ZERO TO FIVE:PRINT #TWO;S(I):NEXT I
8640 PRINT #TWO;LV:PRINT #TWO;GD:PRINT #TWO;TG:PRINT #TWO;EX:PRINT #TWO;CH
8645 PRINT #TWO;HP:PRINT #TWO;CX:PRINT #TWO;CY:PRINT #TWO;CZ:PRINT #TWO;SU
8650 PRINT #TWO;CS:FOR I=ONE TO TEN:PRINT #TWO;I(I):PRINT #TWO;SF(I):NEXT I
8655 PRINT #TWO;SF(11):FOR I=ONE TO C20:PRINT #TWO;FM(I):PRINT #TWO;FL(I)
8660 PRINT #TWO;FH(I):NEXT I:FOR I=ONE TO FOUR:PRINT #TWO;B(I):NEXT I
8663 FOR I=ZERO TO TEN*TEN:PRINT #TWO;EX(I):NEXT I
8665 CLOSE #TWO:PRINT NM$;" stored":CLR :RUN
8670 GOSUB WTCLR:IF CZ=ZERO THEN 6125)
8675 GOTO ARRW
/* Q = Quit
8700 PRINT "QUIT":PRINT "Are you sure?";:GOSUB GTCHR
8702 IF C$<>"Y" THEN PRINT "NO":GOSUB WTCLR:GOTO ARRW
8710 PRINT "YES":PRINT "GOOD BYE CRUEL WORLD!!":GOSUB PAUSE:GOTO 9000
/* Help
8800 GOSUB 29000:POKE SP,ZERO:POKE SP+ONE,ZERO:PRINT "Commands are:":PRINT
8805 PRINT "× Move NORTH":PRINT "Á Move WEST"
8810 PRINT "Ø Move SOUTH":PRINT "Ä Move EAST"
8815 PRINT "Ó Stay here"
8820 PRINT "Ã Cast a spell":PRINT "Ð Re-plot screen"
8825 PRINT "È Help screen (this)"
8830 PRINT "^Ó Store character":PRINT "^Ò Read Scroll Rescue"
8835 PRINT "^È Drink Healing pot.":PRINT "^Ð Drink Strength pot."
8837 PRINT "(^S means control-S)"
8840 PRINT :PRINT " Movement cluster:"
8845 PRINT " × North"
8850 PRINT " West Á Ä East"
8855 PRINT " South Ø"
8896 GOSUB CLWND:PRINT "Hit any key to continue...";:GOSUB GTCHR:GOSUB WTCLR
8898 GOSUB 15000:GOTO ARRW
/* Replot
8900 PRINT "Re-plot":GOSUB PAUSE:PRINT "}";:GOSUB RSTAT:GOSUB 15000
8905 GOSUB 30025:GOTO ARRW
/* Scroll of Rescue
8910 PRINT "Use Scroll of Rescue"
8915 Q=FOUR+FOUR:IF I(Q)<ONE THEN 8920
8917 I(Q)=I(Q)-ONE:CX=TWO:CY=70:CZ=ONE:PRINT "***ZAP!!***":GOSUB PAUSE:GOSUB
30200
8918 GD=ZERO:GOSUB RSTAT:GOTO NEWP
8920 PRINT "You don't have one!!"
8922 GOSUB WTCLR:GOTO ARRW
/* Drink healing potion
8925 Q=FOUR+FIVE:PRINT "Drink Healing Potion":IF I(Q)<ONE THEN 8920
8935 I(Q)=I(Q)-ONE:GOSUB RSTAT:CH=CH+INT(RND(ONE)*C20+ONE):IF CH>HP THEN CH=HP
8940 PRINT "You feel BETTER!":GOSUB RHITS:GOTO 8922
/* Drink strength potion
8945 PRINT "Drink Potion of Strength":IF I(TEN)<ONE THEN 8920
8950 IF SF(ONE)<ZERO THEN SF(ONE)=ZERO
8955 SF(ONE)=SF(ONE)+INT(RND(ONE)*C20+TEN+ONE):PRINT "Strength flows through your
body"
8960 I(TEN)=I(TEN)-ONE:GOSUB RSTAT:GOTO 8922
/* Cast
8970 D=ZERO:GOTO 21000
/* Player has died
9000 GOSUB CLWND:PRINT "You DIED!!":GOSUB WTCLR:IF SF(TEN)>ZERO THEN 27205
9002 PRINT "}Another ";:IF LV<FOUR THEN PRINT "not so ";
9005 PRINT "mighty adventurer":PRINT "bites the dust"
9010 PRINT "Do you want to try again?";
9012 GOSUB GTCHR:IF C$="N" THEN 9020
9013 IF C$="&" THEN 9012
9015 PRINT "YES":CLOSE #ONE:CLR :RUN
9020 PRINT "NO":PRINT "So long.....":GOTO 32767
/***************************************************************
/* Generate map
/***************************************************************
10010 Q=X*XO+Y*YO+Z*ZO+X*YO+Y*ZO+Z*XO:
/* HI = Q & 0xFF
HI=USR(LG,ONE,Q,TF-ONE):
Q=X*Y*ZO+Y*Z*XO+Z*X*YO
/* IF Q & 0x03 <> 0 then goto 10030
10020 IF USR(LG,ONE,Q,THREE)<>ZERO THEN 10030
/* Q = Q << 2
10022 Q=Q/FOUR:
/* Q = Q & 0x0F
Q=USR(LG,ONE,Q,15):
/* If Q > 9 then Q = Q - 9
IF Q>NIN THEN Q=Q-NIN
/* HI = HI + (Q >>8)
10024 HI=HI+Q*TF
/* If at left or right edge of maze turn on left wall
10030 IF X=ONE OR X=201 THEN HI=USR(LG,TWO,HI,12)
/* if at top of bottom of maze turn on top wall
10035 IF Y=ONE OR Y=201 THEN HI=USR(LG,TWO,HI,THREE)
10040 HI=INT(HI):RETURN

/* Plot upper wall
UPPLT:
10050 IF W<TWO THEN RETURN
/* Plot wall
10055 FOR Q=ZERO TO SIX:POKE SC+LC+Q,C128:NEXT Q:IF W>TWO THEN RETURN
/* Plot door
10060 FOR Q=TWO TO FOUR:POKE SC+LC+Q,82:NEXT Q:RETURN
10070 LC=496:W=USR(FNUP,P(TWO,TWO)):GOSUB 10050
/* Plot left wall
LFPLT:
10075 IF W<TWO THEN RETURN
/* Plot wall
10080 FOR Q=ZERO TO SIX:POKE SC+LC+Q*40,C128:NEXT Q:IF W>TWO THEN RETURN
/* Plot door
10085 FOR Q=TWO TO FOUR:POKE SC+LC+Q*40,124:NEXT Q:RETURN
14020 LC=FOUR:W=USR(FNLF,P(ZERO,ZERO)):GOSUB 10075
/* Redraw maze
15000 Z=CZ:
FOR Q2=ZERO TO THREE:
Y=CY+Q2-ONE:
FOR Q1=ZERO TO THREE:
X=CX+Q1-ONE:
GOSUB 10010
15005 P(Q2,Q1)=HI:
NEXT Q1:
NEXT Q2:
GOSUB 29000:GOSUB 16500:PRINT "↑↑"
/* Display any spells in effect
15007 SS=-ONE:FOR I=ONE TO 11:IF SF(I)<ONE THEN 15011
15009 IF PEEK(SP+ONE)>19 THEN PRINT :PRINT "↑↑";
15010 PRINT SPF$(I*FOUR-THREE,I*FOUR);":";
15011 NEXT I:GOSUB 16500:
/* Plot section 0,0 upper wall
IF USR(FNUP,P(ONE,ONE))>ONE THEN 15015
15012 IF USR(FNLF,P(ZERO,ONE))>ONE THEN 15015
15013 LC=FOUR:W=USR(FNUP,P(ZERO,ZERO)):GOSUB UPPLT
/* Plot section 0,0 left wall
15015 IF USR(FNUP,P(ONE,ZERO))>ONE THEN 15025
15016 IF USR(FNLF,P(ONE,ONE))>ONE THEN 15025
15020 LC=FOUR:W=USR(FNLF,P(ZERO,ZERO)):GOSUB LFPLT
/*Plot section 0,1 upper and left wall and section 0,2 left wall
15025 IF USR(FNUP,P(ONE,ONE))>ONE THEN 15040
15030 LC=TEN:W=USR(FNUP,P(ZERO,ONE)):GOSUB
UPPLT:W=USR(FNLF,P(ZERO,ONE)):GOSUB LFPLT
15035 LC=16:W=USR(FNLF,P(ZERO,TWO)):GOSUB LFPLT
/*Plot section 0,2 upper wall
15040 IF USR(FNUP,P(ONE,ONE))>ONE THEN 15045
15041 IF USR(FNLF,P(ZERO,TWO))>ONE THEN 15045
15042 LC=16:W=USR(FNUP,P(ZERO,TWO)):GOSUB UPPLT
/* Plot section 0,3 left wall
15045 IF USR(FNLF,P(ONE,TWO))>ONE THEN 15055
15046 IF USR(FNUP,P(ONE,TWO))>ONE THEN 15055
15050 LC=22:W=USR(FNLF,P(ZERO,THREE)):GOSUB LFPLT
/* Plot section 1,2 upper wall, section 1,3 left wall, section 2,2 upper wall
15055 IF USR(FNLF,P(ONE,TWO))>ONE THEN 15080
15060 LC=TF:W=USR(FNUP,P(ONE,TWO)):GOSUB
UPPLT:LC=262:W=USR(FNLF,P(ONE,THREE)):GOSUB LFPLT
15070 LC=496:W=USR(FNUP,P(TWO,TWO)):GOSUB UPPLT
/* Plot section 3,2 upper wall
15080 IF USR(FNUP,P(TWO,ONE))>ONE THEN 15085
15081 IF USR(FNLF,P(TWO,TWO))>ONE THEN 15085
15082 LC=736:W=USR(FNUP,P(THREE,TWO)):GOSUB UPPLT
/* Plot section 2,3 left wall
15085 IF USR(FNLF,P(ONE,TWO))>ONE THEN 15100
15086 IF USR(FNUP,P(TWO,TWO))>ONE THEN 15100
15090 LC=502:W=USR(FNLF,P(TWO,THREE)):GOSUB LFPLT
/* Plot section 2,2 left wall, 3,1 upper wall, and 2,1 left wall
15100 IF USR(FNUP,P(TWO,ONE))>ONE THEN 15120
15105 LC=496:W=USR(FNLF,P(TWO,TWO)):GOSUB LFPLT
15110 LC=730:W=USR(FNUP,P(THREE,ONE)):GOSUB
UPPLT:LC=490:W=USR(FNLF,P(TWO,ONE)):GOSUB LFPLT
/* Plot section 3,0 upperwall
15120 IF USR(FNUP,P(TWO,ONE))>ONE THEN 15125
15121 IF USR(FNLF,P(TWO,ONE))>ONE THEN 15125
15122 LC=724:W=USR(FNUP,P(THREE,ZERO)):GOSUB UPPLT
/* Plot section 2,0 left wall
15125 IF USR(FNLF,P(ONE,ONE))>ONE THEN 15140
15126 IF USR(FNUP,P(TWO,ZERO))>ONE THEN 15140
15130 LC=484:W=USR(FNLF,P(TWO,ZERO)):GOSUB LFPLT
/* Plot section 2,0 upper wall, section 1,0 left wall, and 1,0 upper wall
15140 IF USR(FNLF,P(ONE,ONE))>ONE THEN 15155
15145 LC=484:W=USR(FNUP,P(TWO,ZERO)):GOSUB UPPLT
15150 LC=244:W=USR(FNLF,P(ONE,ZERO)):GOSUB LFPLT:W=USR(FNUP,P(ONE,ZERO)):GOSUB
UPPLT
/* Plot section 1,1 upper and left walls
15155 LC=250:W=USR(FNLF,P(ONE,ONE)):GOSUB LFPLT:W=USR(FNUP,P(ONE,ONE)):GOSUB
UPPLT
/* Plot section 1,2 left and section 2,1 upper wall
15160 LC=TF:W=USR(FNLF,P(ONE,TWO)):GOSUB
LFPLT:LC=490:W=USR(FNUP,P(TWO,ONE)):GOSUB UPPLT
/* Draw player
15200 POKE SC+373,5:LC=P(ONE,ONE):
/* No light spell in effect
IF SF(THREE)<=ZERO THEN SETCOLOR TWO,ZERO,ZERO:RETURN
/* If Light spell in effect then show items in surrounding rooms
15300 SETCOLOR TWO,ZERO,FOUR:
/* NW room
/* No item do next room
IF USR(FNS,P(ZERO,ZERO))=ZERO THEN 15320
/* Check for obstructed view
15305 IF USR(FNUP,P(ONE,ZERO))<TWO THEN IF USR(FNLF,P(ONE,ONE))<TWO THEN 15315
15310 IF USR(FNUP,P(ONE,ONE))>ONE THEN 15320
15311 IF USR(FNLF,P(ZERO,ONE))>ONE THEN 15320
15315 POKE 40126,C128
/* N room
15320 IF USR(FNS,P(ZERO,ONE))<>ZERO THEN IF USR(FNUP,P(ONE,ONE))<TWO THEN POKE
40132,C128
/* NE room
15340 IF USR(FNS,P(ZERO,TWO))=ZERO THEN 15360
15345 IF USR(FNUP,P(ONE,ONE))<TWO THEN IF USR(FNLF,P(ZERO,TWO))<TWO THEN 15355
15350 IF USR(FNUP,P(ONE,TWO))>ONE THEN 15360
15351 IF USR(FNLF,P(ONE,TWO))>ONE THEN 15360
15355 POKE 40138,C128
/* E room
15360 IF USR(FNS,P(ONE,TWO))<>ZERO THEN IF USR(FNLF,P(ONE,TWO))<TWO THEN POKE
40378,C128
/* SE room
15380 IF USR(FNS,P(TWO,TWO))=ZERO THEN 15400
15385 IF USR(FNUP,P(TWO,TWO))<TWO THEN IF USR(FNLF,P(ONE,TWO))<TWO THEN 15395
15390 IF USR(FNUP,P(TWO,ONE))>ONE THEN 15400
15391 IF USR(FNLF,P(TWO,TWO))>ONE THEN 15400
15395 POKE 40618,C128
/* S room
15400 IF USR(FNS,P(TWO,ONE))<>ZERO THEN IF USR(FNUP,P(TWO,ONE))<TWO THEN POKE
40612,C128
/* SW room
15420 IF USR(FNS,P(TWO,ZERO))=ZERO THEN 15440
15425 IF USR(FNUP,P(TWO,ZERO))<TWO THEN IF USR(FNLF,P(ONE,ONE))<TWO THEN 15435
15430 IF USR(FNUP,P(TWO,ONE))>ONE THEN 15440
15431 IF USR(FNLF,P(TWO,ONE))>ONE THEN 15440
15435 POKE 40606,C128
/* W room
15440 IF USR(FNS,P(ONE,ZERO))<>ZERO THEN IF USR(FNLF,P(ONE,ONE))<TWO THEN POKE
40366,C128
15499 RETURN
/********************************************************
/* GTCHR: Read a character from the keyboard
/********************************************************
/* Loop for 50 counts or until key is pressed
16000 C$=STR$(Q): FOR Q=ONE TO 50: IF PEEK(764)=255 THEN 16010
/* Get the key that was pressed and put it in C$
16005 GET #ONE,Q:C$=CHR$(Q):RETURN
/* Loop back and try again
16010 NEXT Q:IF NM$="DEMO " THEN GOTO GTCHR
/* Return & if no key was pressed
16015 C$="&":RETURN
/****************************************************
/* Get multi character input from player
/****************************************************
16200 D$=""
16205 GOSUB GTCHR:IF C$="&" THEN 16205
16210 IF C$=RT$ THEN RETURN
16215 IF C$<>CHR$(126) THEN 16235
16220 IF LEN(D$)=ZERO THEN 16205
16225 PRINT "← ←";:IF LEN(D$)<TWO THEN 16200
16230 D$=D$(ONE,LEN(D$)-ONE):GOTO 16205
16235 IF LEN(D$)>=30 OR ((C$<"ZERO" OR C$>"9") AND (C$<"A" OR C$>"Z")) THEN 16205
16240 D$(LEN(D$)+ONE)=C$:PRINT C$;:GOTO 16205
/* Position cursor to display a message
16500 POKE SP,C20:POKE SP+ONE,ZERO:PRINT "¼":RETURN
/* Position cursor at 8,11
16600 POKE SP,EIG:POKE SP+ONE,11:RETURN
/* Clear text window
17000 POKE SP,ZERO:PRINT :QI=USR(1777):GOSUB 16500:RETURN
/* Pause, then clear text window
17500 GOSUB PAUSE:GOTO CLWND
/********************************************************************
/* Check for change of level
/********************************************************************
/* Check if enough experience to go up a level
18000 Q=TEN*TEN*TEN:IF EX<Q*TWO^LV THEN 18040
18010 GOSUB CLWND:PRINT "You went up a
level!ý";:LV=LV+ONE:J=INT(RND(ONE)*S(THREE)+ONE)
/* Increase hit points
18015 CH=CH+J:HP=HP+J:IF EX>Q*TWO LV THEN EX=INT(Q*(TWO LV)-ONE/TWO)
/* Increase spell points
18017 CS=CS+LV:SU=SU+LV
18020 GOSUB RSTAT:GOSUB WTCLR:PRINT "You gain ";J;" hit points":RETURN
/* Check for drop in level
18040 IF LV=ONE AND EX>=ZERO THEN RETURN
18050 IF EX>=Q*TWO^(LV-ONE) THEN RETURN
/* Decrease spell points
18055 CS=CS-LV:SU=SU-LV:IF CS<ZERO THEN CS=ZERO
18060 PRINT "You go down a level!";:LV=LV-ONE:J=INT(RND(ONE)*S(THREE)+ONE)
/* Decrease hit points
18070 CH=CH-J:HP=HP-J:GOSUB RSTAT:GOSUB WTCLR:PRINT "You lose ";J;" hit points"
/* If hit points less then zero, then player is dead
18075 IF CH>ZERO AND LV>ZERO THEN RETURN
18080 GOSUB PAUSE:GOTO 9000
/* Pause
19000 FOR Q=ONE TO 45:NEXT Q:RETURN
/* Short pause
19500 FOR Q=ONE TO FOUR:NEXT Q:RETURN
/* Get monster name
19700 M$=MO$(M*EIG-SEV,M*EIG)
/* Strip trailing spaces
19705 IF M$(LEN(M$))=" " THEN M$=M$(ONE,LEN(M$)-ONE):GOTO 19705
19710 RETURN
/* Initialize combination to open box
19800 FOR Q=ONE TO FOUR:B(Q)=INT(RND(ONE)*FOUR+ONE):NEXT Q:RETURN
/* Handle damage
20000 D=INT(RND(ONE)*L*SIX+ONE):PRINT "You suffer ";D;" hit points":CH=CH-D
20005 IF CH>ZERO THEN GOSUB RHITS:RETURN
20010 GOSUB RSTAT:GOSUB WTCLR:GOTO 9000
/* Gain/lose experience points
20100 I=INT(RND(ONE)*500*CZ+ONE):PRINT "You just ";
/* 50% of the time you should lose experience points. This line appears to have a bug, I=-I1 should be
I=-I to deduct experience. The way it is you I will always be zero so you won’t loose points.
20110 IF RND(ONE)>ONE/TWO THEN PRINT "lost ";:I=-I1:GOTO 20120
20115 PRINT "gained ";
20120 PRINT ABS(I);" experience points":EX=EX+I:GOSUB 18000:RETURN
20200 I=INT(RND(ONE)*6):IF RND(ONE)>ONE/TWO THEN 20220
20210 IF S(I)=18 THEN 20200
20215 PRINT "Your ";S$(I*THREE+ONE,I*THREE+THREE);" goes up";:S(I)=S(I)+ONE:GOTO 20240
20220 IF S(I)=THREE THEN 20200
20225 PRINT "Your ";S$(I*THREE+ONE,I*THREE+THREE);" goes down";:S(I)=S(I)-ONE
20240 PRINT " by 1":GOSUB RSTAT:GOSUB PAUSE:RETURN
20250 K=PEEK(SP):POKE SP,FIVE:POKE SP+ONE,ZERO:EN=TWO:GOTO 20322
20260 K=PEEK(SP):POKE SP,FOUR:POKE SP+ONE,ZERO:EN=THREE:GOTO 20320
/* Display stats
20300 EN=ONE:K=PEEK(SP):POKE SP,ZERO:POKE SP+ONE,ZERO
20301 PRINT CR$;NM$(ONE,SEV);" LVL ";LV;" "
20302 PRINT CR$;"STR ";S(ZERO);" ";:POKE SP+ONE,31:PRINT "CON ";S(THREE);" "
20305 PRINT CR$;"INT ";S(ONE);" ";:POKE SP+ONE,31:PRINT "DEX ";S(FOUR);" "
20310 PRINT CR$;"WIS ";S(TWO);" ";:POKE SP+ONE,31:PRINT "CHR ";S(FIVE);" "
20320 PRINT CR$;"HP ";CH;":";HP;" "
20321 IF EN=THREE THEN 20396
20322 PRINT CR$;"SU ";CS;":";SU;" "
20323 IF EN=TWO THEN 20396
20325 PRINT CR$;"EX ";EX;" "
20330 PRINT CR$;"GD ";GD;" "
20335 RESTORE 28310:FOR I=ONE TO TEN:READ MA$:IF I<FOUR THEN IF I(I)>=ZERO THEN
20345
20340 IF I(I)<ONE THEN 20380
20345 PRINT CR$;"";:IF I>SEV THEN 20365
20350 PRINT MA$;:IF I(I)>ZERO THEN PRINT " +";I(I);" ";:GOTO 20355
20352 PRINT " ";
20355 PRINT :GOTO 20380
20365 PRINT I(I);" ";MA$;" "
20380 NEXT I
20390 PRINT CR$;" ";
20396 IF K<THREE THEN K=THREE
20398 POKE SP,K-ONE:PRINT :RETURN
/* Clear prompt area and message window
20600 POKE SP,18:PRINT :PRINT CR$;" "
20605 GOSUB CLWND:POKE SC+332,ZERO:RETURN
/* Determine if monster is undead
20700 UN=ZERO:IF M=THREE OR M=FIVE OR M=EIG OR M=TEN OR M=13 OR M=17 OR M=18
THEN UN=ONE
20705 RETURN
/* Get a numeric value from the player
20800 D$=""
20805 GOSUB GTCHR:IF C$<>CHR$(126) THEN 20824
20810 IF D$="" THEN 20805
20815 PRINT "- -";:IF LEN(D$)=ONE THEN 20800
20820 D$=D$(ONE,LEN(D$)-ONE):GOTO 20805
20824 IF C$=RT$ AND D$="" THEN D$="0"
20825 IF C$=RT$ THEN C=VAL(D$):RETURN
20826 IF C$="&" THEN C=ZERO:RETURN
20827 IF LEN(D$)>NIN THEN 20805
20828 IF C$="-" AND D$="" THEN 20835
20830 IF C$<"0" OR C$>"9" THEN 20805
20835 PRINT C$;:D$(LEN(D$)+ONE)=C$:GOTO 20805
20900 GOSUB 16500:PRINT "You have not been noticed..."
20905 PRINT "<RET> to approach:";:GOSUB GTCHR
20910 GOSUB CLWND:IF C$=CHR$(155) THEN 3007
20915 POKE SC+332,0:GOTO TRSR
/* Prompt for spell level
21000 PRINT "Cast":GOSUB CLWND:PRINT "Spell level:";:GOSUB GTCHR:PRINT
C$:MI$="0":MI$(TWO)=C$:C=VAL(MI$)
/* Check spell level
21002 IF C>ZERO AND C<=INT(LV/THREE)+ONE AND C<SEV THEN 21010
21004 IF C=ZERO THEN 21910
21005 PRINT "You don't have that level spells!":GOSUB PAUSE:GOTO 21910
/* Check if you have enough spell units
21010 IF C>CS THEN PRINT "You don't have enough spell units!":GOTO 21900
/* Prompt for spell
21020 PRINT "Spell( = to list):";:GOSUB GTCHR:IF C$=RT$ OR C$="&" THEN 21910
21025 IF C$<"1" OR C$>"6" THEN C$="="
21030 IF C$="=" THEN 21040
21035 CS=CS-C:GOSUB RSPLL:POKE SP+ONE,18:GOTO 21050
/* Display spell list
21040 GOSUB CLWND:PRINT "":S=ONE:GOSUB 28000:FOR I=ONE TO THREE:PRINT I;"
";SP$:READ SP$:NEXT I
21045 PRINT "";:FOR I=ONE TO THREE:PRINT CR$(ONE,C20);I+THREE;" ";SP$:READ
SP$:NEXT I
21046 PRINT "Press any key to continue...";:GOSUB GTCHR:GOSUB CLWND
21047 IF C$<"1" OR C$>"6" THEN 21020
21048 GOTO 21035
21050 S=VAL(C$):GOSUB 28000:PRINT SP$:GOTO 21000+C*1000+S*100
21310 PRINT "The ";M$;" doesn't hear...":GOTO 21900
/* Handle spells that damage a monster
21700 MH=MH-I:PRINT "It suffers ";I;" hit points":IF MH>ZERO THEN 21900
21705 GOTO MOND
21750 PRINT "Not in melee!!":GOSUB WTCLR:GOTO 3300
/* Cast a combat spell when not in combat
21800 PRINT "You just wasted a combat spell!"
21900 GOSUB WTCLR:IF D=ONE THEN 3300
21905 GOTO ARRW
21910 GOSUB CLWND:IF D=ONE THEN 3100
21915 GOTO ARRW
21920 GOSUB WTCLR:GOSUB 20600:GOTO TRSR
21930 PRINT "Undead are already dead!!":GOTO 21900
/* Magic Missile Spell
22100 GOSUB WTCLR:IF D=ZERO THEN 21800
22105 I=INT(RND(ONE)*8+FIVE):GOTO 21700
/* Sleep Spell
22200 GOSUB WTCLR:IF D=ZERO THEN 21800
/* Check for undead, they don’t sleep
22202 IF UN=ONE THEN PRINT "Undead don't sleep!":GOTO 3300
/* If D20 < Intelligence then monster doesn’t sleep
22205 IF INT(RND(ONE)*C20+ONE)>S(ONE) THEN PRINT "The ";M$;" isn't sleepy!":GOTO 3300
/* Monster is asleep.
22210 PRINT "The ";M$;" is sleeping":PRINT "Press <RET> to kill:";:GOSUB GTCHR
22215 IF C$<>RT$ THEN GOSUB CLWND:GOTO 3710
/* 4/5th of the time the monster is killed, 1/5th it wakes up
22220 IF RND(ONE)>ONE/FIVE THEN GOTO MOND
22250 PRINT "It woke up!!":GOSUB PAUSE:GOTO 3300
/* Cure light wounds spell, Gain 1-9 Hit Points
22300 I=INT(RND(ONE)*EIG+ONE):PRINT "You feel better":CH=CH+I
22305 IF CH>HP THEN CH=HP
22310 GOSUB RHITS:GOTO 21900
/* Light spell. Increase light level to 5 - 15
22400 T=THREE:U=TEN+ONE:SETCOLOR TWO,ZERO,FOUR
/* Set length of spell effect, used by many spell routines
22402 IF SF(T)<ZERO THEN SF(T)=ZERO
22405 SF(T)=SF(T)+INT(RND(ONE)*U+FIVE):GOTO 21900
/* Turn Undead spell
22500 GOSUB WTCLR:IF D=ZERO THEN 21800
/* Check if Monster is undead
22505 IF ONE=UN THEN 22515
22510 PRINT "The ";M$;" is insulted":PRINT "at being called undead":GOTO 21900
/* Determine success/failure based on Intelligence
22515 IF RND(ONE)<S(TWO)/C20+LV/C20-ML/C20 THEN 22525
22520 PRINT "The ";M$;" listens with deaf ears":GOTO 21900
22525 ML=INT((ML/TWO)+ONE/TWO):PRINT "It runs in fear!!":GOTO 3223
/* Protection from evil spell
22600 T=FOUR:U=TEN+ONE:GOTO 22402
/* Web spell
23100 GOSUB WTCLR:IF D=ZERO THEN 21800
23105 IF INT(RND(ONE) TWO*C20+ML)>S(ONE) THEN PRINT "The ";M$;" dodges aside!":GOTO
3300
23110 PRINT "The ";M$;" is webbed!":PRINT "Press <RET> to kill:";:GOSUB GTCHR
23115 IF C$<>RT$ THEN GOSUB CLWND:GOTO 3710
23120 IF RND(ONE)-ML/C20>ONE/TWO THEN GOTO MOND
23125 PRINT "It broke free!!":GOSUB PAUSE:GOTO 3300
/* Levitate spell
23200 T=FIVE:U=C20+ONE:GOTO 22402
/* Cause light wounds spell
23300 GOSUB WTCLR:IF D=ZERO THEN 21800
23305 I=INT(RND(ONE)*12+THREE):GOTO 21700
/* Detect traps spell
23400 T=TWO:U=C20+ONE:GOTO 22402
/* Charm spell
23500 GOSUB CLWND:IF D=ZERO THEN 21800
/* Can’t charm undead
23502 IF UN=ONE THEN PRINT "The undead ignore your wiles!":GOTO 3300
23505 IF INT((RND(ONE) TWO)*C20+ONE)>S(FIVE) THEN PRINT "The ";M$;" resists you!":GOTO
3300
23510 PRINT "The ";M$;" is charmed":PRINT "Press <RET> to kill:";:GOSUB GTCHR
23515 GOTO 23115
/* Strength
23600 T=ONE:U=C20+ONE:GOTO 22402
/* Lightning bolt spell
24100 GOSUB CLWND:SETCOLOR TWO,SIX,TEN:PRINT "ZZZZAAAPPP!!!":SETCOLOR
TWO,ZERO,ZERO:IF D=ZERO THEN 21800
/* Determine damage
24105 GOSUB PAUSE:I=INT(RND(ONE)*SIX*LV+15):GOTO 21700
/* Cure serious wounds spell.
/* Re-gain 1-25 hit points
24200 I=INT(RND(ONE)*24+ONE):PRINT "You feel better":CH=CH+I
24205 IF CH>HP THEN CH=HP
24210 GOSUB RHITS:GOTO 21900
/* Continual light
24300 T=THREE:U=C20+TEN+ONE:SETCOLOR TWO,ZERO,FOUR:GOTO 22402
/* Invisibility spell
24400 T=SIX:U=C20+ONE:SETCOLOR TWO,ZERO,TWO:GOTO 22402
/* Hold monster spell
24500 GOSUB WTCLR:IF D=ZERO THEN 21800
/* Check monster level against Intelligence to see if spell succeeds
24505 IF INT(RND(ONE)*C20+ML)>S(ONE) THEN PRINT "The ";M$;" ignores you!":GOTO 3300
/* Prompt for kill
24510 PRINT "The ";M$;" is held!":PRINT "Press <RET> to kill:";:GOSUB GTCHR
24515 IF C$<>RT$ THEN GOSUB CLWND:GOTO 3710
/* Calculate chance of kill succeeding
24520 IF RND(ONE)>ML*6.0E-3 THEN GOTO MOND
/* Kill did not succeed
24525 PRINT "It broke free!!":GOSUB PAUSE:GOTO 3300
/* Phantasmal Force spell
24600 GOSUB WTCLR:IF D=ZERO THEN 21800
24605 IF INT(RND(ONE)*22+ML)<S(ONE) THEN 24615
24610 PRINT "The ";M$;" doesn't beleive!":GOTO 3300
24615 PRINT "It beleives!....Arrgh...":GOTO MOND
/* Pass wall Spell
25100 GOSUB WTCLR:IF D=ONE THEN 21750
/* Prompt for direction
25105 PRINT "Direction>";:GOSUB GTCHR
25110 FOR I=ONE TO FOUR:IF CM$(I,I)=C$ THEN 25120
25115 NEXT I:PRINT "???";:GOSUB CLWND:GOTO 25105
/* Check if player is at the edge of the maze
25120 IF (I=ONE AND CY>ONE) OR (I=TWO AND CY<200) OR (I=THREE AND C>ONE) OR
(I=FOUR AND CX<200) THEN 25125
25122 PRINT "Only stone there....":GOTO 21900
/* Move to new location
25125 GOSUB 30200:PRINT "****ÐÏÏÆ****":GOTO 8105+50*I
/* Fireball spell
25200 GOSUB CLWND:SETCOLOR TWO,TWO,TWO:PRINT "WHOOOOOSHH!!!":SETCOLOR
TWO,ZERO,ZERO:IF D=ZERO THEN 21800
25205 PRINT "The ";M$;" is burning!":I=INT(RND(ONE)*12*LV+15):GOSUB PAUSE:GOTO 21700
/* Cause serious wounds spell
25300 GOSUB WTCLR:IF D=ZERO THEN 21800
25305 I=INT(RND(ONE)*32+TEN):GOTO 21700
/* Flesh to stone spell
25400 GOSUB WTCLR:IF D=ZERO THEN 21800
25405 IF RND(ONE)>THREE/FIVE THEN PRINT "The ";M$;" isn't affected":GOTO 3300
25410 PRINT "One stone statue....":GOTO MOND
/* Fear spell
25500 T=SEV:U=C20+TEN:GOTO 22402
/* Finger of death spell
25600 GOSUB WTCLR:PRINT "ÄÉÅ¡¡¡¡¡¡":IF D=ZERO THEN 21800
25603 IF UN=ONE THEN 21930
25605 GOSUB PAUSE:IF RND(ONE)>0.3+ML*0.04-LV*0.03 THEN GOTO MOND
25610 PRINT "The ";M$;" laughs!":GOSUB WTCLR:GOTO 3300
/* Teleport spell
26100 GOSUB WTCLR:IF D=ONE THEN 21750
26105 PRINT "+North/South:";:GOSUB 20800:NY=C:PRINT :PRINT "+East/West:";:GOSUB 20800
26110 NX=C:PRINT :PRINT "+Up/Down:";:GOSUB 20800:NZ=C:I=SQR(NX TWO+NY
TWO+(NZ*FIVE) TWO)-ONE/TEN
26115 PRINT :IF I>LV*FIVE THEN PRINT "Too far...try again";:GOSUB WTCLR:GOTO 26105
26120 NX=CX+NX:NY=CY-NY:NZ=CZ-NZ
26125 IF NX>ZERO AND NX<201 AND NY>ZERO AND NY<201 AND NZ<51 THEN 26135
26130 PRINT "Only stone there....The spell fails..";:GOTO 21900
26135 IF NZ<ONE THEN PRINT "Only thin air...The spell fails";:GOTO 21900
26140 CX=NX:CY=NY:CZ=NZ:PRINT "***POOF!***";:GOSUB 30200:GOTO NEWP
/* Astral walk spell
26200 T=EIG:U=T*TWO:GOTO 22402
/* Power word kill spell
26300 GOSUB WTCLR:IF D=ZERO THEN 21800
26305 PRINT "QWERTY!!!!":GOSUB PAUSE:IF UN=ZERO AND RND(ONE)<EIG/TEN THEN
GOTO MOND
26307 IF UN=ONE THEN 21930
26310 PRINT "The ";M$;" doesn't hear...":GOTO 21900
/* Ice storm spell
26400 GOSUB WTCLR:IF D=ZERO THEN 21800
26405 PRINT "BRRRR!!!!":I=60:GOTO 21700
/* Wall of fire spell
26500 GOSUB WTCLR:SETCOLOR TWO,TWO,TWO:PRINT "WWHHOOOOOSSHHH!!!"
26505 PRINT "A wall of fire appears about you":GOSUB PAUSE:GOSUB WTCLR:SETCOLOR
TWO,ZERO,ZERO:IF D=ZERO THEN 21800
26510 IF RND(ONE)>TWO/FIVE THEN PRINT "The ";M$;" is gone":POKE SC+332,ZERO:GOTO
21920
26515 PRINT "The ";M$;" walks through!!!":I=INT(RND(ONE)*TEN+TEN):GOTO 21700
/* Plague spell
26600 GOSUB WTCLR:IF D=ZERO THEN 21800
26605 IF UN=ONE THEN 21930
26610 PRINT "Black DEATH for the ";M$:GOSUB WTCLR:IF RND(ONE)>S(TWO)/C20 THEN
PRINT "It is immune!":GOTO 21900
26620 IF RND(ONE)*TWO<S(THREE)*0.03+ONE THEN GOTO MOND
26625 PRINT "The spell backfires!!":GOSUB WTCLR:GOTO 9000
/* Time stop spell
27100 GOSUB WTCLR:PRINT "Time is frozen, monsters cannot"
27105 PRINT "attack you":IF SF(NIN)<ZERO THEN SF(NIN)=ZERO
27110 SF(NIN)=SF(NIN)+INT(RND(ONE)*C20+FOUR):IF D=ONE THEN GOTO TRSR
27115 GOTO 21900
/* Raise dead spell
27200 T=TEN:U=T*FOUR:GOTO 22402
27205 ? "Raise DEAD":GOSUB WTCLR:S(THREE)=S(THREE)-ONE:SF(TEN)=ZERO:IF
RND(ONE)>S(THREE)*0.06 THEN 27220
27210 PRINT "It works!!":CH=HP:GOSUB RSTAT:GOTO 21900
27220 PRINT "It doesn't work!":GOTO 9000
/* Holy symbol spell
27300 GOSUB WTCLR:IF D=ZERO THEN 21800
27305 PRINT "----";:MI$=".O*
 ":FOR I=ONE TO EIG:PRINT MI$(I,I);"-";:GOSUB 19500:NEXT I:PRINT
27307 GOSUB PAUSE:IF RND(ONE)<NIN/TEN THEN GOTO MOND
27310 PRINT "The ";M$;" doesn't see...":GOTO 21900
/* Word of recall spell
27400 GOSUB WTCLR:IF D=ONE THEN 21750
27405 FOR I=ZERO TO TEN:I(I)=ZERO:NEXT I:GD=ZERO:PRINT "}":GOSUB RSTAT:GOSUB
30200
27410 CX=TWO:CY=70:CZ=ONE:PRINT "***ÚÁÐ!!!***":GOSUB PAUSE
/* Restoration spell
27500 GOSUB WTCLR:PRINT "You feel BETTER!":CH=HP:GOSUB RHITS:GOTO 21900
/* Prismatic wall spell
27600 GOSUB WTCLR:PRINT "A scintillating shifting wall appears":J=ZERO:IF D=ZERO THEN
21800
27605 FOR I=60 TO 79:POKE SC+373,I:NEXT I:J=J+1:IF J<FOUR THEN 27605
27607 POKE SC+332,ZERO:POKE SC+373,56
27615 PRINT "The ";M$;" is gone":GOTO 21920
/* Get spell name, spell number in S
28000 RESTORE 28005+FIVE*C:FOR IQ=ONE TO S:READ SP$:NEXT IQ:RETURN
/* Level 1 spells
28010 DATA MAGIC MISSILE,SLEEP,CURE LIGHT WOUNDS,LIGHT,TURN
UNDEAD,PROT/EVIL
/* Level 2 spells
28015 DATA WEB,LEVITATE,CAUSE LIGHT WNDS,DETECT TRAPS,CHARM,STRENGTH
/* Level 3 spells
28020 DATA LIGHTNING BOLT,CURE SERIOUS WNDS,CONTINUAL
LIGHT,INVISIBILITY,HOLD MONSTER,PHANTSML FORCES
/* Level 4 spells
28025 DATA PASS WALL,FIREBALL,CAUSE SERIOUS WND,FLESH TO STONE,FEAR,FINGER
OF DEATH
/* Level 5 spells
28030 DATA TELEPORT,ASTRAL WALK,POWER WORD KILL,ICE STORM,WALL OF
FIRE,PLAGUE
/* Level 6 spells
28035 DATA TIME STOP,RAISE DEAD,HOLY SYMBOL,WORD OF
RECALL,RESTORATION,PRISMATIC WALL
/* Generate random Inn description
28100 IN1=INT(IN1-INT(IN1/TEN)*TEN+ONE/TWO):IN2=INT(IN2-INT(IN2/TEN)*TEN+ONE/TWO)
28102 IN3=INT(IN3-INT(IN3/TEN)*TEN+ONE/TWO)
28105 RESTORE 28120:FOR IQ=ONE TO IN1:READ IN1$:NEXT IQ
28110 RESTORE 28140:FOR IQ=ONE TO IN2:READ IN2$:NEXT IQ
28115 RESTORE 28160:FOR IQ=ONE TO IN3:READ IN3$:NEXT IQ:RETURN
/* Inn description data
28120 DATA SALTY,BOLD,LOUD,OLD,GOODLY,WORTHY,LOFTY,FINE,ROCKY,AGED
28140 DATA ROAD,EYE,TOOTH,DRAGON,MUG,DEMON,WHARF,BRIDGE,MEADE,ALE
28160 DATA
TAVERN,ALEHOUSE,CELLAR,CLUB,INN,HOUSE,INN,LODGE,MEADHALL,RESTHOUSE
/* Return item name for item number in MI
28200 RESTORE 28210:FOR IQ=ONE TO MI:READ MI$:NEXT IQ:RETURN
28210 DATA SWORD,ARMOR,SHIELD,ELVEN CLOAK,ELVEN BOOTS,RING OF
REGENERATION
28215 DATA RING OF PROTECTION,SCROLL OF RESCUE,POTION OF HEALING,POTION OF
STRENGTH
28310 DATA SWORD,ARMOR,SHIELD,ELVN CLK,ELVN BTS,RING REG,RING PROT,SCRL
RESC,POT HEAL,POT STRG
/* Clear map area
29000 QI=USR(1740):RETURN
/* Pull a monster off the monster stack
30000 M=ZERO:IF FM(ONE)=ZERO THEN 30020
30010 M=FM(ONE):ML=FL(ONE):MH=FH(ONE):GOSUB 20700
30020 FOR I=ONE TO 19:FM(I)=FM(I+ONE):FL(I)=FL(I+ONE):FH(I)=FH(I+ONE):NEXT I
30022 FM(C20)=ZERO:FL(C20)=ZERO:FH(C20)=ZERO:IF M<>ZERO THEN POKE SC+332,THREE
/* Display monster stack
30025 POKE SP,ZERO:POKE SP+ONE,22:FOR Q=ONE TO C20:IF FM(Q)<>ZERO THEN ?
"-";:NEXT Q:RETURN
30030 PRINT " -";:NEXT Q:RETURN
/* Push a monster onto the monster stack
30100 FOR Q=19 TO ONE STEP (-
ONE):FM(Q+ONE)=FM(Q):FL(Q+ONE)=FL(Q):FH(Q+ONE)=FH(Q):NEXT Q
30105 IF M=ZERO THEN FM(ONE)=ZERO:FL(ONE)=ZERO:FH(ONE)=ZERO:GOTO 30025
30110 FM(ONE)=M:FL(ONE)=ML:FH(ONE)=MH:GOTO 30025
/* Clear monster stack.
30200 FOR Q=ONE TO C20:FM(Q)=ZERO:FL(Q)=ZERO:FH(Q)=ZERO:NEXT Q:GOTO 30025
32767 END
Assembly Functions
Note: These are loaded into memory by the startup program
Purpose: Performs logical functions
Parameters:
 Function, Param1, Param2
Function = 1
 Return Param1 & Param2 & 0XFF
Function = 2
 Return (Param1 XOR 0xFF) & (Param2 XOR 0xFF) & 0xFF
LG:
0600: 68 PLA /* Pull number of parameters
0601: d0 2a BNE $062d /* Branch if it’s not zero
0603: 20 23 06 JSR $0623 /* Pull the rest of the params off the stack
0606: a9 00 LDA #$00 /* Return 0x0001
0608: 85 d4 STA $d4
060a: a9 01 LDA #$01
060c: 85 d5 STA $d5
060e: 60 RTS
060f: 20 23 06 JSR $0623 /* Pull the rest of the params off the stack
0612: a9 02 LDA #$02 /* Return 0x0202
0614: 85 d4 STA $d4
0616: 85 d5 STA $d5
0618: 60 RTS

0619: 20 23 06 JSR $0623 /* Pull the rest of the params off the stack
061c: a9 01 LDA #$01 /* Return 0x0101
061e: 85 d4 STA $d4
0620: 85 d5 STA $d5
0622: 60 RTS
/* Pull the rest of the parameters off the stack
0623: 8a TXA
0624: f0 06 BEQ $062c
0626: 68 PLA
0627: 68 PLA
0628: ca DEX
0629: 4c 23 06 JMP $0623
062c: 60 RTS

062d: aa TAX /* X = number of parameters
062e: 68 PLA /* Pull high byte
062f: 68 PLA /* Pull low byte
0630: ca DEX /* next parameters
0631: f0 e6 BEQ $0619 /* Finished all parameters
0633: a8 TAY /* Y = low byte
0634: d0 0f BNE $0645 /* Branch if low byte <> 0
0636: e0 01 CPX #$01 /* Branch if not on parameter 1
0638: d0 d5 BNE $060f /*
063a: 68 PLA /* Get parameters
063b: 68 PLA /*
063c: 49 ff EOR #$ff /* Invert it
063e: 85 d4 STA $d4 /* return it
0640: a9 00 LDA #$00
0642: 85 d5 STA $d5
0644: 60 RTS
/* AND the 2 parameters together and return
0645: c0 01 CPY #$01 /* First parameter = 1
0647: d0 1e BNE $0667 /* branch if not
0649: e0 02 CPX #$02 /* branch if less then 2 parameters left
064b: 30 c2 BMI $060f /*
064d: a9 ff LDA #$ff
064f: 8d ff 06 STA $06ff
0652: 68 PLA /* High byte of parameters
0653: 68 PLA /* Low byte of parameters
0654: 2d ff 06 AND $06ff /* AND with other parameters
0657: 8d ff 06 STA $06ff
065a: ca DEX /* next parameter
065b: d0 f5 BNE $0652 /*
065d: ad ff 06 LDA $06ff /* Return value
0660: 85 d4 STA $d4 /*
0662: a9 00 LDA #$00 /*
0664: 85 d5 STA $d5 /*
0666: 60 RTS /*
/* AND the Inverse of the 2 parameters together and return
0667: c0 02 CPY #$02 /* First parameter = 2
0669: d0 22 BNE $068d /* branch if not
066b: e0 02 CPX #$02 /* branch is less then 2 parameters left
066d: 30 a0 BMI $060f /*
066f: a9 ff LDA #$ff
0671: 8d ff 06 STA $06ff
0674: 68 PLA /* High byte param 2
0675: 68 PLA /* Low byte param 2
0676: 49 ff EOR #$ff /* Invert it
0678: 2d ff 06 AND $06ff /* AND with other parameter
067b: 8d ff 06 STA $06ff
067e: ca DEX /* next parameter
067f: d0 f3 BNE $0674 /*
0681: ad ff 06 LDA $06ff /* Invert the result
0684: 49 ff EOR #$ff /*
0686: 85 d4 STA $d4 /* Return value
0688: a9 00 LDA #$00 /*
068a: 85 d5 STA $d5 /*
068c: 60 RTS /*

068d: 4c 03 06 JMP $0603
Purpose: Returns upper 8 bits of value in 1st parameter
FNS:
0690: 68 PLA
0691: aa TAX
0692: e0 01 CPX #$01
0694: f0 03 BEQ $0699
0696: 4c 0f 06 JMP $060f
0699: 68 PLA
069a: 85 d4 STA $d4
069c: a9 00 LDA #$00
069e: 85 d5 STA $d5
06a0: 68 PLA
06a1: 60 RTS
Purpose: Returns bits 0-1 of value in 1st parameter
FNUP:
06a2: 68 PLA /* Number of parameters
06a3: aa TAX
06a4: e0 01 CPX #$01 /* Branch if 1 parameter
06a6: f0 03 BEQ $06ab /*
06a8: 4c 0f 06 JMP $060f
06ab: 68 PLA /* Read parameter
06ac: 68 PLA /*
06ad: 29 03 AND #$03 /* And low byte with #$03
06af: 85 d4 STA $d4 /* Store in return register
06b1: a9 00 LDA #$00
06b3: 85 d5 STA $d5
06b5: 60 RTS
Purpose: Returns bits 2-3 of value in 1st parameter
FNLF:
06b6: 68 PLA /* Number of parameters
06b7: aa TAX
06b8: e0 01 CPX #$01 /* Branch if 1 parameter
06ba: f0 03 BEQ $06bf
06bc: 4c 0f 06 JMP $060f
06bf: 68 PLA /* Read parameters
06c0: 68 PLA /*
06c1: 4a LSR /* Shift right twice
06c2: 4a LSR
06c3: 29 03 AND #$03
06c5: 85 d4 STA $d4
06c7: a9 00 LDA #$00
06c9: 85 d5 STA $d5
06cb: 60 RTS
Purpose: Clear map area of screen
1740:
06cc: 68 PLA /* Pull parameter count (always 0)
06cd: a9 3f LDA #$3f
06cf: 85 d4 STA $d4
06d1: a9 9c LDA #$9c
06d3: 85 d5 STA $d5
06d5: a2 14 LDX #$14
06d7: a0 16 LDY #$16
06d9: a9 00 LDA #$00
06db: 91 d4 STA ($d4),Y
06dd: 88 DEY
06de: d0 fb BNE $06db
06e0: a5 d4 LDA $d4
06e2: 18 CLC
06e3: 69 28 ADC #$28
06e5: 85 d4 STA $d4
06e7: a5 d5 LDA $d5
06e9: 69 00 ADC #$00
06eb: 85 d5 STA $d5
06ed: ca DEX
06ee: d0 e7 BNE $06d7
06f0: 60 RTS
06f1: a0 a0 LDY #$a0
06f3: 68 PLA
06f4: a9 00 LDA #$00
06f6: 99 5f 9f STA $9f5f,Y
06f9: 88 DEY
06fa: d0 fa BNE $06f6
06fc: 60 RTS
Alternate version
There are two versions of the main program on the disk image that’s commonly available. It’s unknown
whether both versions existed on the original disk. The version in this document is called DSKTEL.BAS,
the other version is called DSKTELEN.BAS. DSKTEL.BAS appears to be the newer version.
This that where changed between DSKTLEN.BASE and DSKTEL.BAS:
- A redefined character set is used to display certain objects like the Pit, Alter, Teleporter, etc.
- More use of constant variables instead of constant values to reduce memory usage.
- Different screen colors used for certain magic effects
- Line 3000 – Added effect of time stop spell on monster encounters
- Line 3007 – Change in display of monster
- Line 3030 – Correct “likes you body” to “likes your body”
- Line 4130 – Changed “Chest EXPLODES!!!!!” to “Chest EXPLODES!!”
- Line 8605 – Changed “Can not store DEMO character” to “Can not store DEMOnstration
character”
- Line 20010 – Removed “Your life has been TERMINATED” message.
- Line 28010 – Fixed spelling of Magic Missile