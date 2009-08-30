;;; i-ching.el --- Cast an i-ching, and come up with a hexagram.
(defconst i-ching-version "0.2")
;; Copyright (c)2008 Jonathan Arkell. (by)(nc)(sa)  Some rights reserved.
;; Author: Jonathan Arkell <jonnay@jonnay.net>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;; Hexigram Interpretations are from:
;; http://en.wikipedia.org/wiki/I_Ching_hexagrams

;;; Commentary:
(defgroup i-ching '()
  "This package will show you an iching hexigram, and give you the
translation from the i-ching.

You can even plug in your own randomizer function, and/or hexagram
grenrator.  This package comes provided with a standard randomizer,
and a coin generator, but you could (for instance) build a generator
that grabs a random lolcat from icanhazcheezburger, and turns it into
a hexagram.")
  
;;; Installation:
;; Put i-ching.el somewhere in your load-path.
;; (Use M-x show-variable RET load-path to see what your load path is.)
;; Add this to your Emacs init file.
;(require 'i-ching)


;;; TODO:
;; - the traditional Chinese casting method based on the date of the Chinese
;;   calendar would be hella coo.  Finding info about that is HARD.
;; - yarrow stalk method
;; - buffer method

;;; CHANGELOG:
;; v 0.2 - Hexagram lookup
;;       - Added Translated Judgment and commentary
;;       - added random.org randomizer source
;;       - added changing lines interpretation
;; v 0.1 - Initial release

;;; Code:

;;* custom random
(defcustom i-ching-randomize-method 'i-ching-standard-randomizer
  "Method of randomization for i-ching.

The possible values (for the default installation) are:
`i-ching-standard-randomizer' - use the built-in Emacs random functions
`i-ching-random-org-randomizer' - use output from random.org webservice

In the future, the following methods will be available:
`i-ching-fuckup-randomizer' - use a illuminatus style randomizer.

You can also write your own method, see `i-ching-standard-randomizer'
for an example."
  :type 'function
  :group 'i-ching)

;;* custom cast
(defcustom i-ching-casting-method 'i-ching-coin-caster
  "Method of casting the i-ching.

Traditionally, i-ching was cast with yarrow stalks, or coins.  by
customizing this method, you can choose how the hexigrams are generated.

Currently the possible values are:
`i-ching-coin-caster' - Cast the i-ching, with virtual coins.

In the future, these ones will be written:
`i-ching-yarrow-caster' - Cast the i-ching with virtual yarrow stalks.

`i-ching-buffer-caster' - Cast the i-ching with the contents of the
    current buffer.
`i-ching-chinese-date-caster' - Cast using the chinese calendar.
`i-ching-western-date-caster' - Cast using the Gregorian calendar.

You can also write your own methods.  See `i-ching-cast-hexagram' for
help on writing your own caster.  Also see `i-ching-coin-caster' as an
example."
  :type 'function
  :group 'i-ching)

;;* custom bigram
(defcustom i-ching-bigram-interpretation
  '(("::" "Old Yin,    Changing, Winter, Thymine")
    ("|:" "Young Yin,  Static,   Spring, Cytosine")
	("||" "Old Yang,   Changing, Summer, Adenine")
	(":|" "Young Yang, Static,   Autumn, Guanine"))
  "Interpretation of the 'bi-grams'.

In case you don't agree with my interpretations, you can add to, or edit this
variable as you see fit.

Note, these interpretations, and the use of bigrams are not strictly of Chinese
origin.")

;;* custom trigram
(defcustom i-ching-trigram-interpretation
  '((":::" . "坤 The Receptive, Field,    Pure Yin,          SW, 地 Earth,    Devoted,           Receptive")
	("::|" . "艮 Keeping Still, Bound,    Articulated Limit, NE, 山 Mountain, Resting,           Completion")
	(":|:" . "坎 The Abysmal,   Gorge,    Axial Rotation,    N,  水 Water,    Dangerous,         In-motion")
	(":||" . "巽 The Gentle,    Ground,   Softness,          SE, 風 Wind,     Penetrating,       Gentle enterance")
	("|::" . "震 The Arousing,  Shake,    Spark,             E,  雷 Thunder,  Inciting Movement, Initiative ")
	("|:|" . "離 The Clinging,  Radience, Cohesion,          S,  火 Fire,     Light Giving,      Clinging ")
	("||:" . "兌 The Joyous,    Open,     Buoyant Resivoir,  W,  澤 Swamp,    Pleasure,          Tranquil")
	("|||" . "乾 The Creative,  Force,    Pure Yang,         NW, 天 Sky,      Strong,            Creative"))
  "Interpretation of the Trigrams.

Just in case you don't agree with my interpretation, you can edit this variable
and change it.   It is an a-list in the format of: (trigram interpretation).
Each trigram is described as a series of 3 colons (:) or vertical bars (|),
thus, Heaven/The Creative is `|||' and Earth/The Receptive is `:::'.
The leftmost position is the bottom, and rightmost is the top.
i.e. |:: Arousing, ::| Stilling"
  :type '(alist :key-type string :value-type string)
  :group 'i-ching)

;;* custom hexagram
(defcustom i-ching-hexagram-interpretation
  '(("||||||" . "01. Force                 (乾)   The Creative    Possessing Creative Power & Skill ")
    ("::::::" . "02. Field                 (坤)   The Receptive   Needing Knowledge & Skill ; Do not force matters and go with the flow ,  ; ;")
    ("|:::|:" . "03. Sprouting             (屯)   Difficulty at the Beginning     Sprouting ")
    (":|:::|" . "04. Enveloping            (蒙)   Youthful Folly  Detained, Enveloped and Inexperienced , ")
    ("|||:|:" . "05. Attending             (需)   Waiting     Uninvolvement (Wait for now), Nourishment ")
    (":|:|||" . "06. Arguing               (訟)   Conflict    Engagement in Conflict ")
    (":|::::" . "07. Leading               (師)   The Army    Bringing Together, Teamwork ")
    ("::::|:" . "08. Grouping              (比)   Holding Together    Union ")
    ("|||:||" . "09. Small Accumulating    (小畜) Small Taming    Accumulating Resources")
    ("||:|||" . "10. Treading              (履)   Treading (Conduct)  Continuing with Alertness")
    ("|||:::" . "11. Pervading             (泰)   Peace   Pervading")
    (":::|||" . "12. Obstruction           (否)   Standstill  Stagnation")
    ("|:||||" . "13. Concording People     (同人) Fellowship  Fellowship, Partnership")
    ("||||:|" . "14. Great Possessing      (大有) Great Possession    Independence, Freedom")
    ("::|:::" . "15. Humbling              (謙)   Modesty     Being Reserved, Refraining")
    (":::|::" . "16. Providing-For         (豫)   Enthusiasm  Inducement, New Stimulus")
    ("|::||:" . "17. Following             (隨)   Following   Following")
    (":||::|" . "18. Corrupting            (蠱)   Work on the Decayed     Repairing")
    ("||::::" . "19. Nearing               (臨)   Approach    Approaching Goal, Arriving ")
    ("::::||" . "20. Viewing               (觀)   Contemplation   The Withholding")
    ("|::|:|" . "21. Gnawing Bite          (噬嗑) Biting Through  Deciding")
    ("|:|::|" . "22. Adorning              (賁)   Grace   Embellishing")
    (":::::|" . "23. Stripping             (剝)   Splitting Apart     Stripping, Flaying")
    ("|:::::" . "24. Returning             (復)   Return  Returning")
    ("|::|||" . "25. Without Embroiling    (無妄) Innocence   Without Rashness")
    ("|||::|" . "26. Great Accumulating    (大畜) Great Taming    Accumulating Wisdom")
    ("|::::|" . "27. Swallowing            (頤)   Mouth Corners   Seeking Nourishment")
    (":||||:" . "28. Great Exceeding       (大過) Great Preponderance     Great Surpassing")
    (":|::|:" . "29. Gorge                 (坎)   The Abysmal Water   Darkness, Gorge")
    ("|:||:|" . "30. Radiance              (離)   The Clinging    Clinging, Attachment")
    ("::|||:" . "31. Conjoining            (咸)   Influence   Attraction")
    (":|||::" . "32. Persevering           (恆)   Duration    Perseverance")
    ("::||||" . "33. Retiring              (遯)   Retreat     Withdrawing")
    ("||||::" . "34. Great Invigorating    (大壯) Great Power     Great Boldness")
    (":::|:|" . "35. Prospering            (晉)   Progress    Expansion, Promotion")
    ("|:|:::" . "36. Brightness Hiding     (明夷) Darkening of the Light  Brilliance Injured")
    ("|:|:||" . "37. Dwelling People       (家人) The Family  Family")
    ("||:|:|" . "38. Polarising            (睽)   Opposition  Division, Divergence")
    ("::|:|:" . "39. Limping               (蹇)   Obstruction     Halting, Hardship")
    (":|:|::" . "40. Taking-Apart          (解)   Deliverance     Liberation, Solution")
    ("||:::|" . "41. Diminishing           (損)   Decrease    Decrease")
    ("|:::||" . "42. Augmenting            (益)   Increase    Increase")
    ("|||||:" . "43. Parting               (夬)   Breakthrough    Separation")
    (":|||||" . "44. Coupling              (姤)   Coming to Meet  Encountering")
    (":::||:" . "45. Clustering            (萃)   Gathering Together  Association, Companionship")
    (":||:::" . "46. Ascending             (升)   Pushing Upward  Growing Upward")
    (":|:||:" . "47. Confining             (困)   Oppression  Exhaustion")
    (":||:|:" . "48. Welling               (井)   The Well    Replenishing, Renewal")
    ("|:|||:" . "49. Skinning              (革)   Revolution  Abolishing the Old")
    (":|||:|" . "50. Holding               (鼎)   The Cauldron    Establishing the New")
    ("|::|::" . "51. Shake                 (震)   Arousing    Mobilizing")
    ("::|::|" . "52. Bound                 (艮)   The Keeping Still   Immobility")
    ("::|:||" . "53. Infiltrating          (漸)   Development     Auspicious Outlook, Infiltration")
    ("||:|::" . "54. Converting The Maiden (歸妹)  The Marrying Maiden     Marrying")
    ("|:||::" . "55. Abounding             (豐)   Abundance   Goal Reached, Ambition Achieved")
    ("::||:|" . "56. Sojourning            (旅)   The Wanderer    Travel")
    (":||:||" . "57. Ground                (巽)   The Gentle  Subtle Influence")
    ("||:||:" . "58. Open                  (兌)   The Joyous  Overt Influence")
    (":|::||" . "59. Dispersing            (渙)   Dispersion  Dispersal")
    ("||::|:" . "60. Articulating          (節)   Limitation  Discipline")
    ("||::||" . "61. Centre Confirming     (中孚) Inner Truth     Staying Focused, Avoid Misrepresentation")
    ("::||::" . "62. Small Exceeding       (小過) Small Preponderance     Small Surpassing")
    ("|:|:|:" . "63. Already Fording       (既濟) After Completion    Completion")
    (":|:|:|" . "64. Not-Yet Fording       (未濟) Before Completion   Incompletion"))
  "Interpretation of the Hexigrams.

So far this is just a direct copy of the content from wikipedia."
  :type '(alist :key-type string :value-type string)
  :group 'i-ching)

;;* hexagram custom fixme
(defcustom i-ching-hexagram-translation
 '(("||||||" "THE CREATIVE works sublime success,\nFurthering through perseverance."
	         "The movement of heaven is full of power. \nThus the superior man makes himself strong and \nuntiring."
			 "Nine at the beginning means:\nHidden dragon. Do not act."
			 "Nine in the second place means:\nDragon appearing in the field.\nIt furthers one to see the great man."
			 "Nine in the third place means:\nAll day long the superior man is creatively active.\nAt nightfall his mind is still beset with cares.\nDanger. No blame."
			 "Nine in the fourth place means:\nWavering flight over the depths.\nNo blame."
			 "°Nine in the fifth place means:\nFlying dragon in the heavens.\nIt furthers one to see the great man."
			 "Nine at the top means:\nArrogant dragon will have cause to repent.")
   ("::::::" "THE RECEPTIVE brings about sublime success,\nFurthering through the perseverance of a mare.\nIf the superior man undertakes something and tries to lead,\nHe goes astray;\nBut if he follows, he finds guidance.\nIt is favorable to find friends in the west and south,\nTo forego friends in the east and north.\nQuiet perseverance brings good fortune." ;
             "The earth's condition is receptive devotion.\nThus the superior man who has breadth of character\nCarries the outer world."
             "Six at the beginning means:\nWhen there is hoarfrost underfoot,\nSolid ice is not far off."
             "°Six in the second place means:\nStraight, square, great.\nWithout purpose,\nYet nothing remains unfurthered."
             "Six in the third place means:\nHidden lines.\nOne is able to remain persevering.\nIf by chance you are in the service of a king,\nSeek not works, but bring to completion."
             "Six in the fourth place means:\nA tied-up sack. No blame, no praise."
             "Six in the fifth place means:\nA yellow lower garment brings supreme good fortune."
             "Six at the top means:\nDragons fight in the meadow.\nTheir blood is black and yellow.")
   ("|:::|:" "DIFFICULTY AT THE BEGINNING works supreme success,\nFurthering through perseverance.\nNothing should be undertaken.\nIt furthers one to appoint helpers."
             "Clouds and thunder:\nThe image of DIFFICULTY AT THE BEGINNING.\nThus the superior man\nBrings order out of confusion."
             "°Nine at the beginning means:\nHesitation and hindrance.\nIt furthers one to remain persevering.\nIt furthers one to appoint helpers."
             "Six in the second place means:\nDifficulties pile up.\nHorse and wagon part.\nHe is not a robber;\nHe wants to woo when the time comes.\nThe maiden is chaste,\nShe does not pledge herself.\nTen years--then she pledges herself."
             "Six in the third place means:\nWhoever hunts deer without the forester\nOnly loses his way in the forest.\nThe superior man understands the signs of the time\nAnd prefers to desist.\nTo go on brings humiliation."
             "	\nSix in the fourth place means:\nHorse and wagon part.\nStrive for union.\nTo go brings good fortune.\nEverything acts to further."
             "° Nine in the fifth place means:\nDifficulties in blessing.\nA little perseverance brings good fortune.\nGreat perseverance brings misfortune."
             "Six at the top means:\nHorse and wagon part.\nBloody tears flow.")
   (":|:::|" "YOUTHFUL FOLLY has success.\nIt is not I who seek the young fool;\nThe young fool seeks me.\nAt the first oracle I inform him. \nIf he asks two or three times, it is importunity.\nIf he importunes, I give him no information.\nPerseverance furthers."
             "A spring wells up at the foot of the mountain:\nThe image of YOUTH.\nThus the superior man fosters his character\nBy thoroughness in all that he does."
             "Six at the beginning means:\nTo make a fool develop\nIt furthers one to apply discipline.\nThe fetters should be removed.\nTo go on in this way bring humiliation."
             "° Nine in the second place means:\nTo bear with fools in kindliness brings good fortune.\nTo know how to take women\nBrings good fortune.\nThe son is capable of taking charge of the household."
             "Six in the third place means:\nTake not a maiden who. When she sees a man of bronze,\nLoses possession of herself.\nNothing furthers."
             "Six in the fourth place means:\nEntangled folly bring humiliation.\n"
			 "° Six in the fifth place means:\nChildlike folly brings good fortune. "
             "Nine at the top means:\nIn punishing folly\nIt does not further one\nTo commit transgressions.\nThe only thing that furthers \nIs to prevent transgressions.")
   ("|||:|:" "WAITING. If you are sincere, \nYou have light and success.\nPerseverance brings good fortune.\nIt furthers one to cross the great water."
             "Clouds rise up to heaven:\nThe image of WAITING.\nThus the superior man eats and drinks,\nIs joyous and of good cheer. "
             "Nine at the beginning means:\nWaiting in the meadow.\nIT furthers one to abide in what endures.\nNo blame."
             "Nine in the second place means:\nWaiting on the sand.\nThere is some gossip.\nThe end brings good fortune."
             "Nine in the third place means:\nWaiting in the mud\nBrings about the arrival of the enemy."
             "Six in the fourth place means:\nWaiting in blood.\nGet out of the pit."
             "° Nine in the fifth place means:\nWaiting at meat and drink.\nPerseverance brings good fortune."
             "Six at the top means:\nOne falls into the pit.\nThree uninvited guests arrive.\nHonor them, and in the end there will be good fortune.\n")
   (":|:|||" "CONFLICT. You are sincere\nAnd are being obstructed.\nA cautious halt halfway brings good fortune.\nGoing through to the end brings misfortune.\nIt furthers one to see the great man.\nIt does not further one to cross the great water."
             "Heaven and water go their opposite ways:\nThe image of CONFLICT.\nThus in all his transactions the superior man\nCarefully considers the beginning."
             "Six at the beginning means:\nIf one does not perpetuate the affair,\nThere is a little gossip.\nIn the end, good fortune comes."
             "Nine in the second place means:\nOne cannot engage in conflict;\nOne returns home, gives way.\nThe people of his town,\nThree hundred households, \nRemain free of guilt."
             "\nSix in the third place means:\nTo nourish oneself on ancient virtue induces perseverance.\nDanger. In the end, good fortune comes.\nIf by chance you are in the service of a king,\nSeek not works."
             "Nine in the fourth place means:\nOne cannot engage in conflict.\nOne turns back and submits to fate,\nChanges one's attitude, \nAnd finds peace in perseverance.\nGood fortune."
             "° Nine in the fifth place means:\nTo contend before him\nBrings supreme good fortune."
             "\nNine at the top means:\nEven if by chance a leather belt is bestowed on one,'\nBy the end of a morning\nIt will have been snatched away three times.")
   (":|::::" "THE ARMY. The army needs perseverance\nAnd a strong man.\nGood fortune without blame."
             "In the middle of the earth is water:\nThe image of THE ARMY.\nThus the superior man increases his masses\nBy generosity toward the people."
             "Six at the beginning means:\nAn army must set forth in proper order.\nIf the order is not good, misfortune threatens."
             "° Nine in the second place means:\nIn the midst of the army.\nGood fortune. No blame.\nThe king bestows a triple decoration."
             "Six in the third place means:\nPerchance the army carries corpses in the wagon.\nMisfortune."
             "Six in the fourth place means:\nThe army retreats. No blame."
             "° Six in the fifth place means:\nThere is game in the field.\nIt furthers one to catch it.\nWithout blame.\nLet the eldest lead the army.\nThe younger transports corpses;\nThen perseverance brings misfortune."
             "Six at the top means:\nThe great prince issues commands,\nFounds states, vests families with fiefs.\nInferior people should not be employed.")
   ("::::|:" "HOLDING TOGETHER brings good fortune.\nInquire of the oracle once again\nWhether you possess sublimity, constancy, and perseverance;\nThen there is no blame.\nThose who are uncertain gradually join.\nWhoever come too late\nMeets with misfortune."
             "On the earth is water:\nThe image of HOLDING TOGETHER.\nThus the kings of antiquity\nBestowed the different states as fiefs\nAnd cultivated friendly relations\nWith the feudal lords."
             "Six at the beginning means:\nHold to him in truth and loyalty;\nThis is without blame.\nTruth, like a full earthen bowl\nThus in the end\nGood fortune comes from without."
             "Hold to him inwardly.\nPerseverance brings good fortune.\n"
			 "Six in the third place means:\nYou hold together with the wrong people."
             "Six in the fourth place means:\nHold to him outwardly also.\nPerseverance brings good fortune."
             "° Nine in the fifth place means:\nManifestation of holding together.\nIn the hunt the king uses beaters on three sides only\nAnd forgoes game that runs off in front.\nThe citizens need no warning.\nGood fortune.\n"
			 "\nSix at the top means:\nHe finds no head for holding together.\nMisfortune.\n")
   ("|||:||" "THE TAMING POWER OF THE SMALL\nHas success.\nDense clouds, no rain from our western region."
             "The wind drives across heaven:\nThe image of THE TAMING POWER OF THE SMALL.\nThus the superior man\nRefines the outward aspect of his nature."
             "Nine at the beginning means:\nReturn to the way.\nHow could there be blame in this?\nGood fortune."
             "Nine in the second place means:\nHe allows himself to be drawn into returning.\nGood fortune."
             "Nine in the third place means:\nThe spokes burst out of the wagon wheels.\nMan and wife roll their eyes.\n"
			 "°Six in the fourth place means:\nIf you are sincere, blood vanishes and fear gives way.\nNo blame.\n"
			 "°Nine in the fifth place means:\nIf you are sincere and loyally attached, \nYou are rich in your neighbor."
             "\nNine at the top means:\nThe rain comes, there is rest.\nThis is due to the lasting effect of character.\nPerseverance brings the woman into danger.\nThe moon is nearly full.\nIf the superior man persists,\nMisfortune comes.")
   ("||:|||" "TREADING. Treading upon the tail of the tiger.\nIt does not bite the man. Success."
             "Heaven above, the lake below:\nThe image of TREADING.\nThus the superior man discriminates between high and low,\nAnd thereby fortifies the thinking of the people."
             "Nine at the beginning means:\nSimple conduct. Progress without blame."
             "Nine in the second place means:\nTreading a smooth, level course.\nThe perseverance of a dark man\nBrings good fortune."
             "°Six in the third place means:\nA one-eyed man is able to see,\nA lame man is able to tread.\nHe treads on the tail of the tiger.\nThe tiger bites the man.\nMisfortune.\nThus does a warrior act on behalf of his great prince."
             "Nine in the fourth place means:\nHe treads on the tail of the tiger.\nCaution and circumspection\nLead ultimately to good fortune."
             "° Nine in the fifth place means:\nResolute conduct.\nPerseverance with awareness of danger."
             "Nine at the top means:\nLook to your conduct and weigh the favorable signs.\nWhen everything is fulfilled, supreme good fortune comes.")
   ("|||:::" "PEACE. The small departs,\nThe great approaches.\nGood fortune. Success."
             "Heaven and earth unite: the image of PEACE.\nThus the ruler\nDivides and completes the course of heaven and earth,\nAnd so aids the people."
             "Nine at the beginning means:\nWhen ribbon grass is pulled up, the sod comes with it.\nEach according to his kind.\nUndertakings bring good fortune."
             "° Nine in the second place means:\n Bearing with the uncultured in gentleness,\n Fording the river with resolution,\n Not neglecting what is distant,\n Not regarding one's companions:\nThus one may manage to walk in the middle."
             "Nine in the third place means:\nNo plain not followed by a slope.\nNo going not followed by a return.\n He who remains persevering in danger\nIs without blame.\nDo not complain about this truth;\nEnjoy the good fortune you still possess.\n"
             "Six in the fourth place means:\nHe flutters down, not boasting of his wealth,\nTogether with his neighbor,\nGuileless and sincere."
             "° Six in the fifth place means:\nThe sovereign I\nGives his daughter in marriage.\nAnd supreme good fortune."
             "Six at the top means:\nThe wall falls back into the moat.\nUse no army now.\nMake your commands known within your own town.\nPerseverance brings humiliation.")
   (":::|||" "STANDSTILL. Evil people do not further\nThe perseverance of the superior man.\nThe great departs; the small approaches."
             "Heaven and earth do not unite:\nThe image of STANDSTILL.\nThus the superior man falls back upon his inner worth \nIn order to escape the difficulties.\nHe does not permit himself to be honored with revenue."
             "Six at the beginning means:\nWhen ribbon grass is pulled up, the sod comes with it.\nEach according to his kind.\nPerseverance brings good fortune and success."
             "°Six in the second place means:\nThey bear and endure;\nThis means good fortune for inferior people.\nThe standstill serves to help the great man to attain success."
             "	\nSix in the third place means:\nThey bear shame."
             "\nNine in the fourth place means:\nHe who acts at the command of the highest \nRemains without blame.\nThose of like mind partake of the blessing."
             "° Nine in the fifth place means:\nStandstill is giving way.\nGood fortune for the great man.\n"What if it should fail, what if it should fail?"In this way he ties it to a cluster of mulberry shoots."
             "Nine at the top means:\nThe standstill comes to an end.\nFirst standstill, then good fortune.")
   ("|:||||" "FELLOWSHIP WITH MEN in the open.\nSuccess.\nIt furthers one to cross the great water.\nThe perseverance of the superior man furthers."
             "Heaven together with fire:\nThe image of FELLOWSHIP WITH MEN.\nThus the superior man organizes the clans\nAnd makes distinctions between things."
             "Nine at the beginning means:\nFellowship with men at the gate.\nNo blame."
             "° Six in the second place means:\nFellowship with men in the clan.\nHumiliation."
             "Nine in the third place means:\nHe hides weapons in the thicket;\nHe climbs the high hill in front of it.\nFor three years he does not rise up."
             "Nine in the fourth place means:\nHe climbs up on his wall; he cannot attack.\nGood fortune."
             "\n° Nine in the fifth place means:\nMen bound in fellowship first weep and lament,\nBut afterward they laugh.\nAfter great struggles they succeed in meeting."
             "Nine at the top means:\nFellowship with men in the meadow.\nNo remorse.")
   ("||||:|" "POSSESSION IN GREAT MEASURE.\nSupreme success."
             "Fire in heaven above:\nthe image of POSSESSION IN GREAT MEASURE.\nThus the superior man curbs evil and furthers good,\nAnd thereby obeys the benevolent will of heaven."
             "Nine at the beginning means:\nNo relationship with what is harmful;\nThere is no blame in this.\nIf one remains conscious of difficulty,\nOne remains without blame."
             "Nine in the second place means:\nA big wagon for loading.\nOne may undertake something.\nNo blame."
             "Nine in the third place means:\nA prince offers it to the Son of Heaven.\nA petty man cannot do this."
             "Nine in the fourth place means:\nHe makes a difference\nBetween himself and his neighbor.\nNo blame."
             "°Six in the fifth place means:\nHe whose truth is accessible, yet dignified,\nHas good fortune."
             "Nine at the top means:\nHe is blessed by heaven.\nGood fortune.\nNothing that does not further.")
   ("::|:::" "MODESTY creates success.\nThe superior man carries things through."
             "Within the earth, a mountain:\nThe image of MODESTY.\nThus the superior man reduces that which is too much,\nAnd augments that which is too little.\nHe weighs things and makes them equal."
             "Six at the beginning means:\nA superior man modest about his modesty\nMay cross the great water.\nGood fortune."
             "Six in the second place means:\nModesty that comes to expression. Perseverance brings good fortune."
			 "°Nine in the third place means:\nA superior man of modesty and merit\nCarries things to conclusion.\nGood fortune."
             "Six in the fourth place means:\nNothing that would not further modesty\nIn movement."
             "Six in the fifth place means:\nNo boasting of wealth before one's neighbor. \nIt is favorable to attack with force.\nNothing that would not further."
             "Six at the top means:\nModesty that comes to expression.\nIt is favorable to set armies marching\nTo chastise one's own city and one's country.")
   (":::|::" "ENTHUSIASM. It furthers one to install helpers\nAnd to set armies marching."
             "Thunder comes resounding out of the earth:\nThe image of ENTHUSIASM.\nThus the ancient kings made music \nIn order to honor merit,\nAnd offered it with splendor\nTo the Supreme Deity,\nInviting their ancestors to be present."
             "Six at the beginning means:\nEnthusiasm that expresses itself\nBrings misfortune."
             "Six in the second place means:\nFirm as a rock. Not a whole day.\nPerseverance brings good fortune.	Firm as a rock, what need of a whole day?\nThe judgment can be known.\nThe superior man knows what is hidden and what is evident.\nHe knows weakness, he knows strength as well.\nHence the myriads look up to him."
             "Six in the third place means:\nEnthusiasm that looks upward creates remorse.\nHesitation brings remorse."
             "°Nine in the fourth place means:\nThe source of enthusiasm.\nHe achieves great things.\nDoubt not.\nYou gather friends around you\nAs a hair clasp gathers the hair."
             "Six in the fifth place means:\nPersistently ill, and still does not die."
             "Six at the top means:	\nDeluded enthusiasm.\nBut if after completion one changes, \nThere is no blame.")
   ("|::||:" "FOLLOWING has supreme success.\nPerseverance furthers. No blame."
             "Thunder in the middle of the lake:\nThe image of FOLLOWING.\nThus the superior man at nightfall\nGoes indoors for rest and recuperation."
             "°Nine at the beginning means:\nThe standard is changing.\nPerseverance brings good fortune.\nTo go out of the door in company\nProduces deeds."
             "Six in the second place means:\nIf one clings to the little boy,\nOne loses the strong man."
             "Six in the third place means:\nIf one clings to the strong man,\nOne loses the little boy.\nThrough following one finds what one seeks.\nIt furthers one to remain persevering."
             "Nine in the fourth place means:\nFollowing creates success.\nPerseverance brings misfortune.\nTo go one's way with sincerity brings clarity.\nHow could there be blame in this?"
             "°Nine in the fifth place means:\nSincere in the good. Good fortune."
             "Six at the top means:\nHe meets with firm allegiance\nAnd is still further bound.\nThe king introduces him\nTo the Western Mountain.")
   (":||::|" "WORK ON WHAT HAS BEEN SPOILED\nHas supreme success.\nIt furthers one to cross the great water.\nBefore the starting point, three days.\nAfter the starting point, three days."
             "The wind blows low on the mountain:\nThe image of DECAY.\nThus the superior man stirs up the people\nAnd strengthens their spirit."
             "Six in the beginning means:\nSetting right what has been spoiled by the father.\nIf there is a son, \nNo blame rests upon the departed father. \nDanger. In the end good fortune."
             "Nine in the second place means:\nSetting right what has been spoiled by the mother.\nOne must not be too persevering.\n"
             "\nNine in the third place means:\nSetting right what has been spoiled by the father.\nThere will be a little remorse. No great blame."
             "Six in the fourth place means:\nTolerating what has been spoiled by the father.\nIn continuing one sees humiliation.\n"
             "° Six in the fifth place means:\nSetting right what has been spoiled by the father.\nOne meets with praise."
             "Nine at the top means:\nHe does not serve kings and princes,\nSets himself higher goals.\n")
   ("||::::" "APPROACH has supreme success.\nPerseverance furthers.\nWhen the eighth month comes,\nThere will be misfortune."
             "The earth above the lake:\nThe image of APPROACH.\nThus the superior man is inexhaustible\nIn his will to teach,\nAnd without limits\nIn his tolerance and protection of the people."
             "° Nine at the beginning means:\nJoint approach.\nPerseverance brings good fortune."
             "° Nine in the second place means:\nJoint approach.\nGood fortune.\nEverything furthers."
             "Six in the third place means:\nComfortable approach.\nNothing that would further.\nIf one is induced to grieve over it,\nOne becomes free of blame."
             "Six in the fourth place means:\nComplete approach.\nNo blame.\n"
             "Six in the fifth place means:\nWise approach.\nThis is right for a great prince.\nGood fortune."
             "Six at the top means:\nGreat hearted approach.\nGood-hearted approach.\nGood fortune. No blame.")
   ("::::||" "CONTEMPLATION. The ablution has been made, \nBut not yet the offering.\nFull of trust they look up to him."
             "The wind blows over the earth:\nThe image of CONTEMPLATION.\nThus the kings of old visited the regions of the world,\nContemplated the people,\nAnd gave them instruction."
             "Six at the beginning means:\nBoy like contemplation.\nFor an inferior man, no blame.\nFor a superior man, humiliation."
             "Six in the second place means:\nContemplation through the crack of the door.\nFurthering for the perseverance of a woman."
             "Six in the third place means:\nContemplation of my life \nDecides the choice\nBetween advance and retreat."
             "Six in the fourth place means:\nContemplation of the light of the kingdom.\nIt furthers one to exert influence as the guest of a king."
             "° Nine in the fifth place means:\nContemplation of my life.\nThe superior man is without blame."
             "° Nine at the top means:\nContemplation of his life.\nThe superior man is without blame.")
   ("|::|:|" "BITING THROUGH has success.\nIt is favorable to let justice be administered."
             "Thunder and lighting:\nThe image of BITING THROUGH.\nThus the kings of former times made firm the laws\nThrough clearly defined penalties."
             "Nine at the beginning means:\nHis feet are fastened in the stocks,\nSo that his toes disappear.\nNo blame."
             "Six  in the second place means:\nBites through tender meat,\nSo that his nose disappears.\nNo blame."
             "\nSix  in the third place means:\nBites on old dried meat \nAnd strikes on something poisonous.\nSlight humiliation.  No blame."
             "\nNine in the fourth place means:\nBites on dried gristly meat.\nReceives metal arrows.\nIt furthers one to be mindful of difficulties\nAnd to be persevering.\nGood fortune. "
             "° Six in the fifth place means:\nBites on dried lean meat.\nReceives yellow gold.\nPerseveringly aware of danger.\nNo blame."
             "Nine at the top means:\nHis neck is fastened in the wooden cangue,\nSo that his ears disappear.\nMisfortune.")
   ("|:|::|" "GRACE has success.\nIn small matters\nIt is favorable to undertake something."
             "Fire at the foot of the mountain:\nThe image of GRACE.\nThus does the superior man proceed \nWhen clearing up current affairs.\nBut he dare not decide controversial issues in this way."
             "Nine at the beginning means:\nHe lends grace to his toes, leaves the carriage, and walks."
             "° Six in the second place means:\nLends grace to the beard on his chin."
             "Nine in the third place means:\nGraceful and moist.\nConstant perseverance brings good fortune."
             "Six in the fourth place means:\nGrace or simplicity?\nA white horse comes as if on wings.\nHe is not a robber,\nHe will woo at the right time."
             "Six in the fifth place means:\nGrace in the hills and gardens.\nThe roll of silk is meager and small.\nHumiliation, but in the end good fortune."
             "° Nine at the top means:\nSimple grace. No blame.")
   (":::::|" "SPLITTING APART. IT does not further one \nTo go anywhere."
             "The mountain rests on the earth:\nThe image of SPLITTING APART.\nThus those above can ensure their position\nOnly by giving generously to those below."
             "Six at the beginning means:\nThe leg of the bed is split.\nThose who persevere are destroyed.\nMisfortune."
             "Six in the second place means:\nThe bed is split at the edge.\nThose who persevere are destroyed.\nMisfortune."
             "Six in the third place means:\nHe splits with them. No blame."
             "Six in the fourth place means:\nThe bed is split up to the skin.\nMisfortune."
             "Six in the fifth place means:\nA shoal of fishes. Favor comes through the court ladies.\nEverything acts to further."
             "° Nine at the top means:\nThere is a large fruit still uneaten.\nThe superior man receives a carriage.\nThe house of the inferior man is split apart.")
   ("|:::::" "RETURN. Success.\nGoing out and coming in without error.\nFriends come without blame.\nTo and fro goes the way.\nOn the seventh day comes return.\nIt furthers one to have somewhere to go."
             "Thunder within the earth:\nThe image of THE TURNING POINT.	\nThus the kings of antiquity closed the passes \nAt the time of solstice.\nMerchants and strangers did not go about,\nAnd the ruler\nDid not travel through the provinces."
             "° Nine at the beginning means:\nReturn from a short distance.\nNo need for remorse.\nGreat good fortune."
             "Six in the second place means:\nQuiet return. Good fortune."
             "Six in the third place means:\nRepeated return. Danger. No blame."
             "Six in the fourth place means:\nWalking in the midst of others,\nOne returns alone."
             "\nSix in the fifth place means:\nNoblehearted return. No remorse."
             "Six at the top means:\nMissing the return. Misfortune.\nMisfortune from within and without.\nIf armies are set marching in this way,\nOne will in the end suffer a great defeat, \nDisastrous for the ruler of the country.\nFor ten years\nIt will not be possible to attack again.")
   ("|::|||" "INNOCENCE. Supreme success.\nPerseverance furthers.\nIf someone is not as he should be,\nHe has misfortune,\nAnd it does not further him\nTo undertake anything."
             "Under heaven thunder rolls:\nAll things attain the natural state of innocence.\nThus the kings of old,\nRich in virtue, and in harmony with the time,\nFostered and nourished all beings."
             "° Nine at the beginning means:\nInnocent behavior brings good fortune."
             "Six in the second place means:\nIf one does not count on the harvest while plowing,\nNor on the use of the ground while clearing it,\nIt furthers one to undertake something."
             "Six in the third place means:\nUndeserved misfortune.\nThe cow that was tethered by someone\nIs the wanderer's gain, the citizen's loss."
             "Nine in the fourth place means:\nHe who can be persevering \nRemains without blame.\n"
             "° Nine in the fifth place means:\nUse no medicine in an illness\nIncurred through no fault of your own.\nIt will pass of itself."
             "Nine at the top means:\nInnocent action brings misfortune.\nNothing furthers.")
   ("|||::|" "THE TAMING POWER OF THE GREAT.\nPerseverance furthers.\nNot eating at home brings good fortune.\nIt furthers one to cross the great water."
             "Heaven within the mountain:\nThe image of THE TAMING POWER OF THE GREAT.\nThus the superior man acquaints himself with many sayings of antiquity\nAnd many deeds of the past,\nIn order to strengthen his character thereby."
             "Nine at the beginning means:\nDanger is at hand. It furthers one to desist."
             "Nine in the second place means:\nThe axletrees are taken from the wagon."
             "Nine in the third place means.\nA good horse that follows others.\nAwareness of danger,\nWith perseverance, furthers.\nPractice chariot driving and armed defense daily. \nIt furthers one to have somewhere to go."
             "Six in the fourth place means:\nThe headboard of a young bull.\nGreat good fortune."
             "° Six in the fifth place means:\nThe tusk of a gelded boar.\nGood fortune."
             "° Nine at the top means:\nOne attains the way of heaven.\nSuccess.")
   ("|::::|" "THE CORNERS OF THE MOUTH.\nPerseverance brings good fortune.\nPay heed to the providing of nourishment\nAnd to what a man seeks\nTo fill his own mouth with."
             "At the foot of the mountain, thunder:\nThe image of PROVIDING NOURISHMENT.\nThus the superior man is careful of his words\nAnd temperate in eating and drinking."
             "Nine at the beginning means:\nYou let your magic tortoise go,\nAnd look at me with the corners of your mouth drooping.\nMisfortune."
             "Six in the second place means:\nTurning to the summit for nourishment,\nDeviating from the path\nTo seek nourishment from the hill.\nContinuing to do this brings misfortune."
             "Six in the third place means:\nTurning away from nourishment.\nPerseverance brings misfortune.\nDo not act thus for ten years.\nNothing serves to further.\n"
             "Six in the fourth place means:\nTurning to the summit\nFor provision of nourishment\nBrings good fortune.\nSpying about with sharp eyes\nLike a tiger with insatiable craving.\nNo blame."
             "° Six in the fifth place means:\nTurning away from the path.\nTo remain persevering brings good fortune.\nOne should not cross the great water."
             "° Nine at the top means:\nThe source of nourishment.\nAwareness of danger brings good fortune.\nIt furthers one to cross the great water.")
   (":||||:" "PREPONDERANCE OF THE GREAT.\nThe ridgepole sags to the breaking point.\nIt furthers one to have somewhere to go.\nSuccess."
             "The lake rises above the trees:\nThe image of PREPONDERANCE OF THE GREAT.\nThus the superior man, when he stands alone,\nIs unconcerned,\nAnd if he has to renounce the world,\nHe is undaunted."
             "Six at the beginning means:\nTo spread white rushes underneath.\nNo blame."
             "° Nine in the second place means:\nA dry poplar sprouts at the root.\nAn older man takes a young wife.\nEverything furthers."
             "\nNine in the third place means:\nThe ridgepole sags to the breaking point.\nMisfortune.\n"
             "° Nine in the fourth place means:\nThe ridgepole is braced. Good fortune.\nIf there are ulterior motives, it is humiliating."
             "Nine in the fifth place means:\nA withered poplar puts forth flowers.\nAn older woman takes a husband. \nNo blame. No praise."
             "Six at the top means:\nOne must go through the water.\nIt goes over one's head.\nMisfortune. No blame.")
   (":|::|:" "The Abysmal repeated.\nIf you are sincere, you have success in your heart,\nAnd whatever you do succeeds."
             "Water flows on uninterruptedly and reaches its foal:\nThe image of the Abysmal repeated.\nThus the superior man walks in lasting virtue\nAnd carries on the business of teaching."
             "Six at the beginning means:\nRepetition of the Abysmal.\nIn the abyss one falls into a pit.\nMisfortune."
             "° Nine in the second place means:\nThe abyss is dangerous.\nOne should strive to attain small things only."
             "Six in the third place means:\nForward and backward, abyss on abyss.\nIn danger like this, pause at first and wait,\nOtherwise you will fall into a pit in the abyss.\nDo not act this way."
             "Six in the fourth place means:\nA jug of wine, a bowl of rice with it ;\nEarthen vessels\nSimply handed in through the Window.\nThere is certainly no blame in this."
             "° Nine in the fifth place means:\nThe abyss is not filled to overflowing,\nIt is filled only to the rim.\nNo blame."
             "Six at the top means:\nBound with cords and ropes,\nShut in between thorn-hedged prison walls:\nFor three years one does not find the way.\nMisfortune.")
   ("|:||:|" "THE CLINGING. Perseverance furthers.\nIt brings success.\nCare of the cow brings good fortune."
             "That which is bright rises twice:\nThe image of FIRE.\nThus the great man, by perpetuating this brightness,\nIllumines the four quarters of the world."
             "Nine at the beginning means:\nThe footprints run crisscross.\nIf one is seriously intent, no blame."
             "° Six in the second place means:\nYellow light. Supreme good fortune."
             "Nine in the third place means:\nIn the light of the setting sun,\nMen either beat the pot and sing\nOr loudly bewail the approach of old age.\nMisfortune."
             "Nine in the fourth place means:\nIts coming is sudden;\nIt flames up, dies down, is thrown away."
             "° Six in the fifth place means:\nTears in floods, sighing and lamenting.\nGood fortune. "
             "Nine at the top means:\nThe king used him to march forth and chastise.\nThen it is best to kill the leaders\nAnd take captive the followers. No blame.")
   ("::|||:" "Influence. Success.\nPerseverance furthers.\nTo take a maiden to wife brings good fortune."
             "A lake on the mountain:\nThe image of influence.\nThus the superior man encourages people to approach him\nBy his readiness to receive them."
             "Six at the beginning means:\nThe influence shows itself in the big toe."
             "Six in the second place means:\nThe influence shows itself in the calves of the legs.\nMisfortune.\nTarrying brings good fortune."
             "Nine in the third place means:\nThe influence shows itself in the thighs.\nHolds to that which follows it.\nTo continue is humiliating.\n"
             "° Nine in the fourth place means:\nPerseverance brings good fortune.\nRemorse disappears.\nIf a man is agitated in mind,\nAnd his thoughts go hither and thither,\nOnly those friends \nOn whom he fixes his conscious thoughts\nWill follow."
             "° Nine in the fifth place means:\nThe influence shows itself in the back of the neck.\nNo remorse."
             "Six at the top means:\nThe influence shows itself in the jaws, cheeks, and tongue.")
   (":|||::" "DURATION. Success. No blame.\nPerseverance furthers.\nIt furthers one to have somewhere to go."
             "Thunder and wind: the image of DURATION.\nThus the superior man stands firm \nAnd does not change has direction."
             "Six at the beginning means:\nSeeking duration too hastily brings misfortune persistently.\nNothing that would further."
             "° Nine in the second place means:\nRemorse disappears.\n"
             "Nine in the third place means:\nHe who does not give duration to his character\nMeets with disgrace.\nPersistent humiliation."
             "Nine in the fourth place means:\nNo game in the field."
             "Six in the fifth place means:\nGiving duration to one's character through perseverance.\nThis is good fortune for a woman, misfortune for a man."
             "Six at the top means:\nRestlessness as an enduring condition brings misfortune.")
   ("::||||" "RETREAT. Success.\nIn what is small, perseverance furthers."
             "Mountain under heaven: the image of RETREAT.\nThus the superior man keeps the inferior man at a distance,\nNot angrily but with reserve."
             "°Six at the beginning means:\nAt the tail in retreat. This is dangerous.\nOne must not wish to undertake anything."
             "Six in the second place means:\nhe holds him fast with yellow oxhide.\nNo one can tear him loose."
             "Nine in the third place means:\nA halted retreat \nIs nerve-wracking and dangerous.\nTo retain people as men- and maidservants\nBrings good fortune."
             "Nine in the fourth place means:\nVoluntary retreat brings good fortune to the superior man\nAnd downfall to the inferior man."
             "° Nine in the fifth place means:\nFriendly retreat. Perseverance brings good fortune."
             "Nine at the top means:\nCheerful retreat. Everything serves to further.")
   ("||||::" "THE POWER OF THE GREAT. Perseverance furthers."
             "Thunder in heaven above:\nThe image of THE POWER OF THE GREAT.\nThus the superior man does not tread upon paths\nThat do not accord with established order."
             "Nine at the beginning means:\nPower in the toes.\nContinuing brings misfortune.\nThis is certainly true."
             "Nine in the second place means:\nPerseverance brings good fortune."
             "Nine in the third place means:\nThe inferior man works through power.\nThe superior man does not act thus.\nTo continue is dangerous.\nA goat butts against a hedge\nAnd gets its horns entangled."
             "° Nine in the fourth place means:\nPerseverance brings good fortune.\nRemorse disappears.\nThe hedge opens; there is no entanglement.\nPower depends upon the axle of a big cart."
             "Six in the fifth place means:\nLoses the goat with ease.\nNo remorse."
             "Six at the top means:\nA goat butts against a hedge.\nIt cannot go backward, it cannot go forward.\nNothing serves to further.\nIf one notes the difficulty, this brings good fortune.")
   (":::|:|" "PROGRESS. The powerful prince\nIs honored with horses in large numbers.\nIn a single day he is granted audience three times."
             "The sun rises over the earth:\nThe image of PROGRESS.\nThus the superior man himself\nBrightens his bright virtue."
             "Six at the beginning means:\nProgressing, but turned back.\nPerseverance brings good fortune.\nIf one meets with no confidence, one should remain calm.\nNo mistake."
             "Six in the second place means:\nProgressing, but in sorrow.\nPerseverance brings good fortune.\nThen one obtains great happiness from one's ancestress."
             "Six in the third place means:\nAll are in accord. Remorse disappears."
             "Nine in the fourth place means:\nProgress like a hamster.\nPerseverance brings danger."
             "° Six in the fifth place means:\nRemorse disappears.\nTake not gain and loss to heart.\nUndertakings bring good fortune.\nEverything serves to further."
             "\nNine at the top means:\nMaking progress with the horns is permissible\nOnly for the purpose of punishing one's own city.\nTo be conscious of danger brings good fortune.\nNo blame. \nPerseverance brings humiliation.")
   ("|:|:::" "DARKENING OF THE LIGHT. In adversity\nIt furthers one to be persevering."
             "The light has sunk into the earth:\nThe image of DARKENING OF THE LIGHT.\nThus does the superior man live with the great mass:\nHe veils his light, yet still shines."
             "Nine at the beginning means:\nDarkening of the light during flight.\nHe lowers his wings.\nThe superior man does not eat for three days\nOn his wanderings.\nBut he has somewhere to go.\nThe host has occasion to gossip about him."
             "° Six in the second place means:\nDarkening of the light injures him in the left thigh.\nHe gives aid with the strength of a horse.\nGood fortune."
             "Nine in the third place means:\nDarkening of the light during the hunt in the south.\nTheir great leader is captured.\nOne must not expect perseverance too soon."
             "Six in the fourth place means:\nHe penetrates the left side of the belly.\nOne gets at the very heart of the darkening of the light."
             "° Six in the fifth place means:\nDarkening of the light as with Prince Chi.\nPerseverance furthers."
             "Six at the top means:\nNot light but darkness.\nFirst he climbed up to heaven,\nThen plunged into the depths of the earth.")
   ("|:|:||" "THE FAMILY. The perseverance of the woman furthers."
             "Wind comes forth from fire:\nThe image of THE FAMILY.\mThus the superior man has substance in his words\nAnd duration in his way of life."
             "Nine at the beginning means:\nFirm seclusion within the family.\nRemorse disappears."
             "° Six in the second place means:\nShe should not follow her whims.\nShe must attend within to the food.\nPerseverance brings good fortune."
             "Nine in the third place means:\nWhen tempers flare up in the family,\nToo great severity brings remorse.\nGood fortune nonetheless.\nWhen woman and chile dally and laugh\nIt leads in the end to humiliation."
             "Six in the fourth place means:\nShe is the treasure of the house.\nGreat good fortune."
             "° Nine in the fifth place means:\nAs a king he approaches his family.\nFear not.\nGood fortune."
             "Nine at the top means:\nHis work commands respect.'\nIn the end good fortune comes.")
   ("||:|:|" "OPPOSITION. In small matters, good fortune."
             "Above, fire; below. The lake.\nThe image of OPPOSITION.\nThus amid all fellowship \nThe superior man retains his individuality."
             "Nine at the beginning means:\nRemorse disappears.\nIf you lose your horse, do not run after it;\nIt will come back of its own accord.\nWhen you see evil people,\nGuard yourself against mistakes."
             "° Nine in the second place means:\nOne meets his lord in a narrow street.\nNo blame."
             "Six in the third place means:\nOne sees the wagon dragged back,\nThe oxen halted,\nA man's hair and nose cut off.\nNot a good beginning, but a good end."
             "Nine in the fourth place means:\nIsolated through opposition,\nOne meets a like-minded man\nWith whom one can associate in good faith.\nDespite the danger, no blame."
             "° Six in the fifth place means:\nRemorse disappears.\nThe companion bits his way through the wrappings.\nIf one goes to him,\nHow could it be a mistake?"
             "Nine at the top means:\nIsolated through opposition,\nOne sees one's companion as a pig covered with dirt,\nAs a wagon full of devils.\nFirst one draws a bow against him,\nthen one lays the bow aside.\nHe is not a robber; he will woo at the right time.\nAs one goes, rain falls; then good fortune comes.")
   ("::|:|:" "OBSTRUCTION. The southwest furthers.\nThe northeast does not further.\nIt furthers one to see the great man.\nPerseverance brings good fortune."
             "Water on the mountain:\nThe image of OBSTRUCTION.\nThus the superior man turns his attention to himself\nAnd molds his character."
             "Six at the beginning means:\nGoing leads to obstructions,\nComing meets with praise."
             "Six in the second place means:\nThe King's servant is beset by obstruction upon obstruction,\nBut it is not his own fault."
             "Nine in the third place means:\nGoing leads to obstructions;\nHence he comes back."
             "Six in the fourth place means:\nGoing leads to obstructions,\nComing leads to union."
             "° Nine in the fifth place means:\nIn the midst of the greatest obstructions,\nFriends come."
             "Six at the top means:\nGoing leads to obstructions,\nComing leads to great good fortune.\nIt furthers one to see the great man.")
   (":|:|::" "DELIVERANCE. The southwest furthers.\nIf there is no longer anything where one has to go,\nReturn brings good fortune.\nIf there is still something where one has to go,\nHastening brings good fortune."
             "Thunder and rain set in:\nThe image of DELIVERANCE.\nThus the superior man pardons mistakes \nAnd forgives misdeeds."
             "Six at the beginning means:\nWithout blame."
             "° Nine in the second place means:\nOne kills three foxes in the field\nAnd receives a yellow arrow.\nPerseverance brings good fortune."
             "Six in the third place means:\nIf a man carries a burden on his back\nAnd nonetheless rides in a carriage,\nHe thereby encourages robbers to draw near.\nPerseverance leads to humiliation."
             "Nine in the fourth place means:\nDeliver yourself from your great toe.\nThen the companion comes,\nAnd him you can trust."
             "° Six in the fifth place means:\nIf only the superior man can deliver himself,\nIt brings good fortune.\nThus he proves to inferior men that he is in earnest."
             "Six at the top means:\nThe prince shoots at a hawk on a high wall.\nHe kills it. Everything serves to further.")
   ("||:::|" "DECREASE combined with sincerity\nBrings about supreme good fortune\nWithout blame.\nOne may be persevering in this.\nIt furthers one to undertake something.\nHow is this to be carried out?\nOne may use two small bowls for the sacrifice."
             "At the foot of the mountain, the lake:\nThe image of DECREASE.\nThus the superior man controls his anger\nAnd restrains his instincts."
             "Nine at the beginning means:\nGoing quickly when one's tasks are finished\nIs without blame.\nBut one must reflect on how much one may decrease others."
             "Nine in the second place means:\nPerseverance furthers.\nTo undertake something brings misfortune.\nWithout decreasing oneself,\nOne is able to bring increase to others."
             "°Six in the third place means:\nWhen three people journey together,\nTheir number increases by one.\nWhen one man journeys alone,\nHe finds a companion."
             "Six in the fourth place means:\nIf a man deceases his faults,\nIt makes the other hasten to come and rejoice.\nNo blame."
             "° Six in the fifth place means:\nSomeone does indeed increase him.\nTen pairs of tortoises cannot oppose it.\nSupreme good fortune.\n"
             "Nine at the top means:\nIf one is increased without depriving other,\nThere is no blame.\nPerseverance brings good fortune.\nIt furthers one to undertake something.\nOne obtains servants\nBut no longer has a separate home.")
   ("|:::||" "INCREASE. It furthers one\nTo undertake something.\nIt furthers one to cross the great water."
             "Wind and thunder: the image of INCREASE.\nThus the superior man:\nIf he sees good, he imitates it;\nIf he has faults, he rids himself of them."
             "Nine at the beginning means:\nIt furthers one to accomplish great deeds.\nSupreme good fortune. No blame."
             "° Six in the second place means:\nSomeone does indeed increase him; \nTen pairs of tortoises cannot oppose it.\nConstant perseverance brings good fortune.\nThe king presents him before God.\nGood fortune."
             "Six in the third place means:\nOne is enriched through unfortunate events.\nNo blame, if you are sincere\nAnd walk in the middle,\nAnd report with a seal to the prince."
             "Six in the fourth place means:\nIf you walk in the middle \nAnd report the prince,\nHe will follow.\nIt furthers one to be used\nIn the removal of the capital."
             "° Nine in the fifth place means:\nIf in truth you have a kind heart, ask not.\nSupreme good fortune.\nTruly, kindness will be recognized as your virtue. "
             "Nine at the top means:\nHe brings increase to no one.\nIndeed, someone even strikes him.\nHe does not keep his heart constantly steady.\nMisfortune.")
   ("|||||:" "BREAK-THROUGH. One must resolutely make the matter known\nAt the court of the king.\nIt must be announced truthfully. Danger.\nIt is necessary to notify one's own city.\nIt does not further to resort to arms.\nIt furthers one to undertake something."
             "The lake has risen up to heaven:\nThe image of BREAK-THROUGH.\nThus the superior man\nDispenses riches downward\nAnd refrains from resting on his virtue."
             "Nine at the beginning means:\nMighty in the forward-striding toes.\nWhen one goes and is not equal to the task,\nOne makes a mistake."
             "Nine in the second place means:\nA cry of alarm. Arms at evening and at night.\nFear nothing."
             "Nine in the third place means:\nTo be powerful in the cheekbones \nBrings misfortune.\nThe superior man is firmly resolved.\nHe walks alone and is caught in the rain.\nHe is bespattered,\nAnd people murmur against him.\nNo blame."
             "Nine in the fourth place means:\nThere is no skin on his thighs,\nAnd walking comes hard.\nIf a man were to let himself be led like a sheep,\nRemorse would disappear.\nBut if these words are heard\nThey will not be believed."
             "° Nine in the fifth place means:\nIn dealing with weeds,\nFirm resolution is necessary.\nWalking in the middle\nRemains free of blame."
             "Six at the top means:\nNo cry.\nIn the end misfortune comes.")
   (":|||||" "COMING TO MEET. The maiden is powerful.\nOne should not marry such a maiden."
             "Under heaven, wind:\nThe image of COMING TO MEET.\nThus does the prince act when disseminating his commands\nAnd proclaiming them to the four quarters of heaven."
             "Six at the beginning means:\nIt must be checked with a brake of bronze.\nPerseverance brings good fortune.\nIf one lets it take its course, one experiences misfortune.\nEven a lean pig has it in him to rage around."
             "° Nine in the second place means:\nThere is a fish in the tank. No blame.\nDoes not further guests."
             "Nine in the third place means: \nThere is no skin on his thighs,\nAnd walking comes hard.\nIf one is mindful of the danger,\nNo great mistake is made."
             " 	Nine in the fourth place means:\nNo fish in the tank.\nThis leads to misfortune."
             "  ° Nine in the fifth place means:\nA melon covered with willow leaves.\nHidden lines.\nThen it drops down to one from heave."
             "Nine at the top means:\nHe comes to meet with his horns.\nHumiliation. No blame.")
   (":::||:" "GATHERING TOGETHER. Success.\nThe king approaches his temple.\nIt furthers one to see the great man.\nThis brings success. Perseverance furthers.\nTo bring great offerings creates good fortune.\nIt furthers one to undertake something."
             "Over the earth, the lake:\nThe image of GATHERING TOGETHER.\nThus the superior man renews his weapons\nIn order to meet the unforeseen."
             "Six at the beginning means:\nIf you are sincere, but not to the end,\nThere will sometimes be confusion, sometimes gathering together.\nIf you call out, \nThen after one grasp of the hand you can laugh again.\nRegret not. Going is without blame."
             "Six in the second place means:\nLetting oneself be drawn\nBrings good fortune and remains blameless.\nIf one is sincere,\nIt furthers one to bring even a small offering."
             "Six in the third place means:\nGathering together amid sighs.\nNothing that would further.\nGoing is without blame.\nSlight humiliation."
             "° Nine in the fourth place means:\nGreat good fortune. No blame."
             "° Nine in the fifth place means:\nIf in gathering together one has position,\nThis brings no blame.\nIf there are some who are not yet sincerely in the work,\nSublime and enduring perseverance is needed.\nThen remorse disappears."
             "Six at the top means:\nLamenting and sighing, floods of tears.\nNo blame.")
   (":||:::" "PUSHING UPWARD has supreme success.\nOne must see the great man.\nFear not.\nDeparture toward the south\nBrings good fortune."
             "Within the earth, wood grows:\nThe image of PUSHING UPWARD.\nThus the superior man of devoted character\nHeaps up small things\nIn order to achieve something high and great."
             "Six at the beginning means:\nPushing upward that meets with confidence\nBrings great good fortune."
             "Nine in the second place means:\nIf one is sincere,\nIt furthers one to bring even a small offering.\nNo blame."
             "Nine in the third place means:\nOne pushes upward into an empty city.\n"
             "Six in the fourth place means:\nThe king offers him Mount Ch'i.\nGood fortune. No blame."
             "° Six in the fifth place means:\nPerseverance brings good fortune.\nOne pushes upward by steps."
             "Six at the top means:\nPushing upward in darkness.\nIt furthers one\nTo be unremittingly persevering.")
   (":|:||:" "OPPRESSION. Success. Perseverance.\nThe great man brings about good fortune.\nNo blame.\nWhen one has something to say,\nIt is not believed."
             "There is not water in the lake:\nThe image of EXHAUSTION.\nThus the superior man stakes his life\nOn following his will."
             "Six at the beginning means:\nOne sits oppressed under a bare tree\nAnd strays into a gloomy valley.\nFor three years one sees nothing."
             "° Nine in the second place means:\nOne is oppressed while at meat and drink.\nThe man with the scarlet knee bands is just coming.\nIt furthers one to offer sacrifice.\nTo set forth brings misfortune.\nNo blame."
             "Six in the third place means:\nA man permits himself to be oppressed by stone,\nAnd leans on thorns and thistles.\nHe enters the house and does not see his wife.\nMisfortune."
             "Nine in the fourth place means:\nHe comes very quietly, oppressed in a golden carriage.\nHumiliation, but the end is reached."
             "° Nine in the fifth place means:\nHis nose and feet are cut off.\nOppression at the hands of the man with the purple knee bands.\nJoy comes softly.\nIt furthers one to make offerings and libations."
             "Six at the top means:\nHe is oppressed by creeping vines.\nHe moves uncertainly and says, \"Movement brings remorse.\"If one feels remorse over this and makes a start,\nGood fortune comes.")
   (":||:|:" "THE WELL. The town may be changed,\nBut the well cannot be changed.\nIt neither decreases nor increases.\nThey come and go and draw from the well.\nIf one gets down almost to the water\nAnd the rope does not go all the way,\nOr the jug breaks, it brings misfortune."
             "Water over wood: the image of THE WELL.\nThus the superior man encourages the people at their work,\nAnd exhorts them to help one another."
             "Six at the beginning means:\nOne does not drink the mud of the well.\nNo animals come to an old well."
             "Nine in the second place means:\nAt the well hole one shoots fishes.\nThe jug is broken and leaks."
             "Nine in the third place means:\nThe well is cleaned, but no one drinks from it.\nThis is my heart's sorrow,\nFor one might draw from it.\nIf the king were clear-minded,\nGood fortune might be enjoyed in common."
             "Six in the fourth place means:\nThe well is being lined. No blame."
             "° Nine in the fifth place means:\nIn the well there is a clear, cold spring\nFrom which one can drink."
             "Six at the top means:\nOne draws from the well\nWithout hindrance.\nIt is dependable.\nSupreme good fortune.")
   ("|:|||:" "REVOLUTION. On your own day\nYou are believed.\nSupreme success,\nFurthering through perseverance.\nRemorse disappears."
             "Fire in the lake: the image of REVOLUTION.\nThus the superior man\nSets the calendar in order\nAnd makes the seasons clear."
             "Nine at the beginning means:\nWrapped in the hide of a yellow cow."
             "Six in the second place means:\nWhen one's own day comes, one may create revolution.\nStarting brings good fortune. No blame."
             "Nine in the third place means:\nStarting brings misfortune.\nPerseverance brings danger.\nWhen talk of revolution has gone the rounds three times,\nOne may commit himself,\nAnd men will believe him."
             "Nine in the fourth place means:\nRemorse disappears. Men believe him.\nChanging the form of government brings good fortune."
             "° Nine in the fifth place means:\nThe great man changes like a tiger.\nEven before he questions the oracle\nHe is believed."
             "Six at the top means:\nThe superior man changes like a panther.\nThe inferior man molts in the face.\nStarting brings misfortune.\nTo remain persevering brings good fortune.")
   (":|||:|" "THE CALDRON. Supreme good fortune.\nSuccess."
             "Fire over wood:\nThe image of THE CALDRON.\nThus the superior man consolidates his fate \nBy making his position correct."
             "Six at the beginning means:\nA ting with legs upturned.\nFurthers removal of stagnating stuff.\nOne takes a concubine for the sake of her son.\nNo blame."
             "Nine in the second place means:\nThere is food in the ting.\nMy comrades are envious,\nBut they cannot harm me.\nGood fortune."
             "Nine in the third place means:\nThe handle of the ting is altered.\nOne is impeded in his way of life.\nThe fat of the pheasant is not eaten.\nOnce rain falls, remorse is spent.\nGood fortune comes in the end."
             "Nine in the fourth place means:\nThe legs of the ting are broken.\nThe prince's meal is spilled\nAnd his person is soiled.\nMisfortune."
             "° Six in the fifth place means:\nThe ting has yellow handles, golden carrying rings.\nPerseverance furthers."
             "° Nine at the top means:\nThe ting has rings of jade.\nGreat good fortune.\nNothing that would not act to further.")
   ("|::|::" "SHOCK brings success.\nShock comes-oh, oh!\nLaughing words -ha, ha!\nThe shock terrifies for a hundred miles,\nAnd he does not let fall the sacrificial spoon and chalice."
             "Thunder repeated: the image of SHOCK.\nThus in fear and trembling\nThe superior man sets his life in order\nAnd examines himself."
             "° Nine at the beginning means:\nShock comes-oh, oh!\nThen follow laughing words-ha, ha!\nGood fortune."
             "Six in the second place means:\nShock comes bringing danger.\nA hundred thousand times\nYou lose your treasures\nAnd must climb the nine hills.\nDo not go in pursuit of them.\nAfter seven days you will get them back again."
             "Six in the third place means:\nShock comes and makes one distraught.\nIf shock spurs to action\nOne remains free of misfortune.\n"
             "Nine in the fourth place means:\nShock is mired."
             "Six in the fifth place means:\nShock goes hither and thither.\nDanger.\nHowever, nothing at all is lost.\nYet there are things to be done."
             "Six at the top means:\nShock brings ruin and terrified gazing around.\nGoing ahead brings misfortune.\nIf it has not yet touched one's own body\nBut has reached one's neighbor first,\nThere is no blame.\nOne's comrades have something to talk about.")
   ("::|::|" "KEEPING STILL. Keeping his back still\nSo that he no longer feels his body.\nHe goes into his courtyard\nAnd does not see his people.\nNo blame."
             "Mountains standing close together:\nThe image of KEEPING STILL.\nThus the superior man\nDoes not permit his thoughts \nTo go beyond his situation."
             "Six at the beginning means:\nKeeping his toes still.\nNo blame.\nContinued perseverance furthers."
             "\nSix in e second place means:\nKeeping his calves still.\nHe cannot rescue him whom he follows.\nHis heart is not glad."
             "Nine in the third place means:\nKeeping his hips still.\nMaking his sacrum stiff.\nDangerous. The heart suffocates."
             "Six in the fourth place means:\nKeeping his trunk still.\nNo blame."
             "Six in the fifth place means:\nKeeping his jaws still.\nThe words have order.\nRemorse disappears."
             "° Nine at the top means:\nNoblehearted keeping still.\nGood fortune.")
   ("::|:||" "DEVELOPMENT. The maiden\nIs given in marriage.\nGood fortune.\nPerseverance furthers."
             "On the mountain, a tree:\nThe image of DEVELOPMENT.\nThus the superior man abides in dignity and virtue,\nIn order to improve the mores."
             "Six at the beginning means:\nThe wild goose gradually draws near the shore. \nThe young son is in danger.\nThere is talk. No blame."
             "° Six in the second place means:\nThe wild goose gradually draws near the cliff.\nEating and drinking in peace and concord.\nGood fortune."
             "Nine in the third place means:\nThe wild goose gradually draws near the plateau.\nThe man goes forth and does not return.\nThe woman carries a child but does not bring it forth.\nMisfortune. \nIt furthers one to fight off robbers."
             "Six in the fourth place means:\nThe wild goose goes gradually draws near the tree.\nPerhaps it will find a flat branch. No blame."
             "° Nine in the fifth place means:\nThe wild goose gradually draws near the summit.\nFor three years the woman has no child.\nIn the end nothing can hinder her.\nGood fortune."
             "Nine at the top means:\nThe wild goose gradually draws near the clouds heights.\nIts feathers can be used for the sacred dance.\nGood fortune.")
   ("||:|::" "THE MARRYING MAIDEN.\nUndertakings bring misfortune.\nNothing that would further."
             "Thunder over the lake:\nThe image of THE MARRYING MAIDEN.\nThus the superior man\nUnderstands the transitory\nIn the light of the eternity of the end."
             "Nine at the beginning means:\nThe marrying maiden as a concubine.\nA lame man who is able to tread.\nUndertakings bring good fortune."
             "Nine in the second place means:\nA one-eyed man who is able to see.\nThe perseverance of a solitary man furthers."
             "Six in the third place means:\nThe marrying maiden as a slave.\nShe marries as a concubine."
             "Nine in the fourth place means:\nThe marrying maiden draws out the allotted time.\nA late marriage comes in due course."
             "° Six in the fifth place means:\nThe sovereign I gave his daughter in marriage.\nThe embroidered garments of the princess\nWere not as gorgeous\nAs those of the serving maid.\nThe moon that is nearly full\nBrings good fortune."
             "Six at the top means:\nThe woman holds the basket, but there are no fruits in it.\nThe man stabs the sheep, but no blood flows.\nNothing that acts to further.")
   ("|:||::" "ABUNDANCE has success.\nThe king attains abundance.\nBe not sad.\nBe like the sun at midday."
             "Both thunder and lightning come:\nThe image of ABUNDANCE.\nThus the superior man decides lawsuits\nAnd carries out punishments."
             "Nine at the beginning means:\nWhen a man meets his destined ruler,\nThey can be together ten days,\nAnd it is not a mistake.\nGoing meets with recognition."
             "Six in the second place means:\nThe curtain is of such fullness\nThat the polestars can be seen at noon.\nThrough going one meets with mistrust and hate.\nIf one rouses him through truth,\nGood fortune comes."
             "Nine in the third place means:\nThe underbrush is of such abundance\nThat the small stars can be seen at noon.\nHe breaks his right arm . No blame."
             "Nine in the fourth place means:\nThe curtain is of such fullness\nThat the polestars can be seen at noon.\nHe meets his ruler, who is of like kind.\nGood fortune."
             "° Six in the fifth place means:\nLines are coming,\nBlessing and fame draw near.\nGood fortune."
             "Six at the top means:\nHis house is in a state of abundance.\nHe screens off his family.\nHe peers through the gate\nAnd no longer perceives anyone.\nFor three years he sees nothing.\nMisfortune.")
   ("::||:|" "The Wanderer. Success through smallness.\nPerseverance brings good fortune\nTo the Wanderer."
             "Fire on the mountain:\nThe image of THE WANDERER.\nThus the superior man\nIs clear-minded and cautious\nIn imposing penalties,\nAnd protracts no lawsuits."
             "Six at the beginning means:\nIf the wanderer busies himself with trivial things, \nHe draws down misfortune upon himself."
             "Six in the second place means:\nThe wanderer comes to an inn.\nHe has his property with him.\nHe wins the steadfastness of a young servant.\n"
             "Nine in the third place means:\nThe wanderer's inn burns down.\nHe loses the steadfastness of his young servant.\nDanger."
             "Nine in the fourth place means:\nThe wanderer rests in a shelter.\nHe obtains his property and an ax.\nMy heart is not glad."
             "° Six in the fifth place means:\nHe shoots a pheasant.\nIt drops with the first arrow.\nIn the end this brings both praise and office."
             "Nine at the top means:\nThe bird's nest burns up.\nThe wanderer laughs at first,\nThen must needs lament and weep.\nThrough carelessness he loses his cow.\nMisfortune.")
   (":||:||" "THE GENTLE. Success through what is small.\nIt furthers one to have somewhere to go.\nIt furthers one to see the great man."
             "Winds following one upon the other:\nThe image of THE GENTLY PENETRATING.\nThus the superior man \nSpreads his commands abroad\nAnd carries out his undertakings."
             "Six at the beginning means:\nIn advancing and in retreating,\nThe perseverance of a warrior furthers."
             "Nine in the second place means:\nPenetration under the bed.\nPriests and magicians are used in great number.\nGood fortune. No blame."
             "Nine in the third place means:\nRepeated penetration. Humiliation."
             "Six in the fourth place means:\nRemorse vanishes.\nDuring the hunt\nThree kinds of game are caught."
             "° Nine in the fifth place means:\nPerseverance brings good fortune.\nRemorse vanishes.\nNothing that does not further.\nNo beginning, but an end.\nBefore the change, three days.\nAfter the change, three days.\nGood fortune."
             "Nine at the top means:\nPenetration under the bed.\nHe loses his property and his ax.\nPerseverance brings misfortune.")
   ("||:||:" "THE JOYOUS. Success.\nPerseverance is favorable."
             "Lakes resting one on the other:\nThe image of THE JOYOUS.\nThus the superior man joins with his friends\nFor discussion and practice."
             "Nine at the beginning means:\nContented joyousness. Good fortune."
             "° Nine in the second place means:\nSincere joyousness. Good fortune.\nRemorse disappears."
             "Six in the third place means:\nComing joyousness. Misfortune."
             "Nine in the fourth place means:\nJoyousness that is weighed is not at peace.\nAfter ridding himself of mistakes a man has joy."
             "° Nine in the fifth place means:\nSincerity toward disintegrating influences is dangerous."
             "Six at the top means:\nSeductive joyousness.")
   (":|::||" "DISPERSION. Success.\nThe king approaches his temple.\nIt furthers one to cross the great water.\nPerseverance furthers."
             "The wind drives over the water:\nThe image of DISPERSION.\nThus the kings of old sacrificed to the Lord\nAnd built temples."
             "Six at the beginning means:\nHe brings help with the strength of a horse.\nGood fortune."
             "Nine in the second place means:\nAt the dissolution\nHe hurries to that which supports him.\nRemorse disappears."
             "Six in the third place means:\nHe dissolves his self. No remorse."
             "Six in the fourth place means:\nHe dissolves his bond with his group.\nSupreme good fortune.\nDispersion leads in turn to accumulation.\nThis is something that ordinary men do not think of."
             "° Nine in the fifth place means:\nHis loud cries are as dissolving as sweat.\nDissolution! A king abides without blame."
             "\nNine at the top means:\nHe dissolves his blood.\nDeparting, keeping at a distance, going out,\nIs without blame.")
   ("||::|:" "LIMITATION. Success.\nGalling limitation must not be persevered in."
             "Water over lake: the image of LIMITATION.\nThus the superior man\nCreates number and measure,\nAnd examines the nature of virtue and correct conduct."
             "Nine at the beginning means:\nNot going out of the door and the courtyard\nIs without blame."
             "Nine in the second place means:\nNot going out of the gate and the courtyard\nBrings misfortune."
             "Six in the third place means:\nHe who knows limitation\nWill have cause to lament.\nNo blame."
             "Six in the fourth place means:\nContented limitation. Success."
             "° Nine in the fifth place means:\nSweet limitation brings good fortune.\nGoing brings esteem.\n"
             "Six at the top means:\nGalling limitation.\nPerseverance brings misfortune.\nRemorse disappears.")
   ("||::||" "INNER TRUTH. Pigs and fishes.\nGood fortune.\nIt furthers one to cross the great water.\nPerseverance furthers."
             "Wind over lake: the image of INNER TRUTH.\nThus the superior man discusses criminal cases \nIn order to delay executions."
             "Nine at the beginning means:\nBeing prepared brings good fortune.\nIf there are secret designs, it is disquieting."
             "Nine in the second place means:\nA crane calling in the shade.\nIts young answers it.\nI have a good goblet.\nI will share it with you."
             "Six in the third place means:\nHe finds a comrade.\nNow he beats the drum, now he stops.\nNow he sobs, now he sings."
             "Six in the fourth place means:\nThe moon nearly at the full.\nThe team horse goes astray.\nNo blame."
             "° Nine in the fifth place means:\nHe possesses truth, which links together.\nNo blame."
             "Nine at the top means:\nCockcrow penetrating to heaven.\nPerseverance brings misfortune.")
   ("::||::" "PREPONDERANCE OF THE SMALL. Success.\nPerseverance furthers.\nSmall things may be done; great things should not be done.\nThe flying bird brings the message:\nIt is not well to strive upward,\nIt is well to remain below.\nGreat good fortune."
	         "Thunder on the mountain:\nThe image of PREPONDERANCE OF THE SMALL.\nThus in his conduct the superior man gives preponderance to reverence.\nIn bereavement he gives preponderance to grief.\nIn his expenditures he gives preponderance to thrift."
	         "Six at the beginning means:\nThe bird meets with misfortune through flying."
	         "° Six in the second place means:\nShe passes by her ancestor\nAnd meets her ancestress.\nHe does not reach his prince\nAnd meets the official.\nNo blame."
	         "Nine in the third place means:\nIf one is not extremely careful,\nSomebody may come up from behind and strike him.\nMisfortune."
	         "Nine in the fourth place means:\nNo blame. He meets him without passing by.\nGoing brings danger. One must be on guard.\nDo not act. Be constantly persevering."
	         "° Six in the fifth place means:\nDense clouds,\nNo rain from our western territory.\nThe prince shoots and hits him who is in the cave."
	         "Six at the top means:\nHe passes him by, not meeting him.\nThe flying bird leaves him.\nMisfortune.\nThis means bad luck and injury.")
   ("|:|:|:" "AFTER COMPLETION. Success in small matters.\nPerseverance furthers.\nAt the beginning good fortune.\nAt the end disorder."
	         "Water over fire: the image of the condition \nIn AFTER COMPLETION.\nThus the superior man \nTakes thought of misfortune\nAnd arms himself against it in advance."
	         "Nine at the beginning means:\nHe breaks his wheels.\nHe gets his tail in the water.\nNo blame."
	         "° Six in the second place means:\nThe woman loses the curtain of her carriage.\nDo not run after it;\nOn the seventh day you will get it."
	         "Nine in the third place means:\nThe Illustrious Ancestor\nDisciplines the Devil's Country.\nAfter three years he conquers it.\nInferior people must not be employed."
	         "Six in the fourth place means:\nThe finest clothes turn to rags.\nBe careful all day long."
	         "Nine in the fifth place means:\nThe neighbor in the east who slaughters an ox\nDoes not attain as much real happiness\nAs the neighbor in the west\nWith his small offering."
	         "Six at the top means:\nHe gets his head in the water. Danger.")
   (":|:|:|" "BEFORE COMPLETION. Success.\nBut if the little fox, after nearly completing the crossing,\nGets his tail in the water,\nThere is nothing that would further."
	         "Fire over water:\nThe image of the condition before transition.\nThus the superior man is careful\nIn the differentiation of things,\nSo that each finds its place."
	         "Six at the beginning means:\nHe gets his tail in the water.\nHumiliating."
	         "Nine in the second place means:\nHe brakes his wheels.\nPerseverance brings good fortune."
	         "Six in the third place means:\nBefore completion, attack brings misfortune.\nIt furthers one to cross the great water."
	         "Nine in the fourth place means:\nPerseverance brings good fortune.\nRemorse disappears.\nShock, thus to discipline the Devil's Country.\nFor three years, great realms are rewarded."
	         "° Six in the fifth place means:\nPerseverance brings good fortune.\nNo remorse.\nThe light of the superior man is true.\nGood fortune."
	         "Nine at the top means:\nThere is drinking of wine\nIn genuine confidence. No blame.\nBut if one wets his head,\nHe loses it, in truth."))
  "Text of the i-ching (translated).

This is the full text of the representative chapter in the i-ching.

the format is:
  (hex judgment image 1st-line 2nd-line 3rd-line 4th-line 5th-line 6th-line)
Where hex is the hexagram in |: ascii format.
judgement is the text of the judgement
image is the commentary of Confucius
nth-line is the commentary on changing lines."
  :type '(alist :key-type 'string :value-type (list 'string 'string 'string 'string 'string 'string 'string 'string))
  :group 'i-ching)

(defface i-ching-title-face
  '((default
	  (:weight bold :overline "black"
	   :box (:line-width 4 :color "black" :style nil)
	   :foreground "white" :background "black"))
	(nil nil))
  "Face for title bars"
  :group 'i-ching)

(defface i-ching-subtitle-face
  '((default
	  (:weight bold)
	  :underline "black"))
  "Face for sub-titles"
  :group 'i-ching)

;;* interactive
;;;###autoload
(defun i-ching-lookup ()
  "Look up an i-ching hexagram."
  (interactive)
  (let ((i-ching-casting-method 'i-ching-lookup-caster))
	(i-ching-cast)))

;;* interactive display
;;;###autoload
(defun i-ching-cast ()
  "Cast an i-ching, and put the result into an i-ching buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create "*i-ching*"))
  (toggle-read-only 0)
  (erase-buffer)
  (let* ((hexagram (i-ching-cast-hexagram))
		 (changing (> (apply 'max hexagram) 1))
		 (change (i-ching-change-hexagram hexagram))
		 (catalyst (i-ching-catalyst-hexagram hexagram)))
	(i-ching-title "Main Hexagram")
    (insert (i-ching-full-hexagram-interpretation hexagram))
	(insert "\n")
	(if changing
		(progn
		 (insert (i-ching-changing-translation hexagram catalyst))
		 (insert "\n\n")
		 (i-ching-title "Changing Hexagram")
		 (insert (i-ching-full-hexagram-interpretation change))
		 (insert "\n")
		 (i-ching-title "Catalyzing Hexagram")
		 (insert (i-ching-full-hexagram-interpretation catalyst)))
		"Static Hexagram.")
	(insert "\n")
	(i-ching-title "Correctness Hexagram")
	(insert (i-ching-hexagram-string (i-ching-correctness-hexagram hexagram)))
	(insert "\n\n")
	(i-ching-title "Correspondence Trigram")
	(insert (format "%s" (apply 'i-ching-trigram (i-ching-correspond-trigram hexagram)))))
  (goto-char (point-min))
  (toggle-read-only 1))

;;* display helper
(defun i-ching-title (title)
  "Return a nice TITLE string."
  (insert (propertize (concat title "\n")
					  'face "i-ching-title-face")))

;;* display helper
(defun i-ching-full-hexagram-interpretation (hex)
  "Output a full interpretation of HEX.

The full interpretation is the interpretation of a hex, plus its components."
  (concat (i-ching-hexagram-string hex)
		  "\n"
		  (i-ching-hexagram-components hex)
		  "\n"
		  (i-ching-hexagram-translation hex)))

;;* display helper change
(defun  i-ching-changing-translation (hex catalyst)
  "Show translations for changing line in HEX, with catalyst hexagram CATALYST."
  (let ((changing-trans (cdr (cdr (cdr (assoc (i-ching-number-to-ascii hex)
											  i-ching-hexagram-translation))))))
	(loop for line in catalyst
		  for num from 0
		  append
		  (case line
			(0 '(" "))
			(1 (list (format "Changing Line %s:\n%s\n" (+ 1 num) (nth num changing-trans)))))
		  into out
		  finally return (apply 'concat out))))

;;* display helper
(defun i-ching-hexagram-translation (hexagram)
  "Return the full translation of a HEXAGRAM as a string."
  (let ((trans (assoc (i-ching-number-to-ascii hexagram) i-ching-hexagram-translation)))
	(format "Judgment:\n%s\n\nImage:\n%s\n\n" (nth 1 trans) (nth 2 trans))))

;;* display
(defun i-ching-hexagram-string (hexagram)
  "Return a nicely formatted string from HEXAGRAM."
  (format "%s - %s"
		  (car (apply 'i-ching-hexagram hexagram))
		  (cdr (apply 'i-ching-hexagram hexagram))))

;;* display
(defun i-ching-number-to-ascii (hex)
  "Display a given HEX graphically."
  (loop for line in hex
		append
		(case line
		  (0 '(":"))
		  (1 '("|"))
		  (2 '(":"))
		  (3 '("|"))) into out
		finally return (apply 'concat out)))

(defun i-ching-ascii-to-number (hex)
  "Grab the numeric values from a hexagram from ascii.

HEX is a string representation of a hexagram, consisting of:

: = 8 (0 internally)
| = 7 (1 internally)
X = 6 (2 internally)
O = 9 (3 internally)"
  (loop for line in (append hex nil) ; turn it into a string
		append
		(case line
		  (?| '(1))
		  (?: '(0))
		  (?X '(2))
		  (?O '(3))
		  (t (error "Unexpected token %s in %s" line hex))) into out
		finally return out))

(when nil
	  (i-ching-ascii-to-number "||:O|::|:X:|")
	  (append "abc" nil)
	  )

;;* display
(defun i-ching-hexagram-components (hex)
  "Return a textual representaiton of the breakdown of hexagram HEX."
  (format (concat (propertize "Trigrams:\n"
							  'face "i-ching-subtitle-face")
				  " %s\n %s\n\n"
				  (propertize "Bigrams:\n"
							  'face "i-ching-subtitle-face")
				  " %s\n %s\n %s\n")
		  (i-ching-trigram (fourth hex) (fifth hex) (sixth hex))
		  (i-ching-trigram (first hex) (second hex) (third hex))
		  (i-ching-bigram (sixth hex)  (fifth hex))
		  (i-ching-bigram (fourth hex) (third hex))
		  (i-ching-bigram (second hex) (first hex))))

(when nil
	  (i-ching-hexagram-components (list 1 2 1 0 3 1)))

;;* unigram boolean
(defun i-ching-not (a)
  "Perform a not operation on unigram A."
  (i-ching-numerize
   (let ((a (i-ching-normalize a))
		 (b (i-ching-normalize b)))
	 (or (and a (not b)) (and (not a) b)))))

;;* unigram boolean
(defun i-ching-xor (a b)
  "Perform a xor operation on unigrams A and B."
  (i-ching-numerize
   (let ((a (i-ching-normalize a))
		 (b (i-ching-normalize b)))
	 (or (and a (not b)) (and (not a) b)))))

;;* unigram boolean
(defun i-ching-or (a b)
  "Perform an or operation on unigrams A and B."
  (i-ching-numerize
   (let ((a (i-ching-normalize a))
		 (b (i-ching-normalize b)))
	 (or a b))))

;;* unigram boolean
(defun i-ching-and (a b)
  "Perform an and operation on hexagrams A and B."
  (i-ching-numerize
   (let ((a (i-ching-normalize a))
		 (b (i-ching-normalize b)))
	 (and a b))))

;;* unigram
(defun i-ching-normalize (a)
  "Normalize unigram A to a boolean value.

Note that this operation will destroy any changing data."
  (= 1 (% a 2)))

;;* unigram
(defun i-ching-numerize (a)
  "Turn a boolean value A into a number."
  (if a 1 0))

;;* bigram
(defun i-ching-bigram (a b)
  "Take two lines (A B) and return a bigram."
  (assoc (i-ching-number-to-ascii (list a b))
		 i-ching-bigram-interpretation))

;;* trigram
(defun i-ching-trigram (a b c)
  "Take three lines (A B C) and return a trigram."
  (assoc (i-ching-number-to-ascii (list a b c))
		 i-ching-trigram-interpretation))

;;* hexagram
(defun i-ching-hexagram (a b c d e f)
  "Take 6 lines (A B C D E F) and return a hexagram."
  (assoc (i-ching-number-to-ascii (list a b c d e f))
		 i-ching-hexagram-interpretation))
(when nil
	  (i-ching-number-to-ascii (list 1 2 0 1 3 2))
	  (i-ching-hexagram 0 1 0 1 1 0)
	  (i-ching-trigram 1 1 1))

;;* cast interface
(defun i-ching-cast-hexagram ()
  "Casts a hexagram, and return a list.

This will cast the hexigram using the values of `i-ching-casting-method' and
`i-ching-randomize-method'.

The list returned will contain 6 elements (one for each line).  Each element
of the list will be a number:

0:  ---   ---   Static (young) Yin
1:  ---------   Static (young) Yang
2:  --- X ---   Dynamic (old) Yin
3:  ----O----   Dynamic (old) Yang.

Note that this is subject to change!"
  (apply i-ching-casting-method nil))

;;* change
(defun i-ching-change-hexagram (hex)
  "Take hexigram HEX and perform the changing line transformation."
  (mapcar (lambda (x)
			(case x
			  (0 0)
			  (1 1)
			  (2 1)
			  (3 0)))
	      hex))

;;*change
(defun i-ching-catalyst-hexagram (hex)
  "Build a catalyst hexagram from HEX.

The catalyst hexagram is a hexagram that when applied as a xor to the primary
hexagram, would emit the changing hexagram.

This is not traditional to the i-ching."
  (mapcar (lambda (x)
			(if (> x 1) 1 0)) hex))
 
(when nil
	  (i-ching-change-hexagram (list 0 1 2 3 0 1 2 3))
	  (i-ching-catalyst-hexagram (list 0 1 2 3 3 2 1))
	  (i-ching-catalyst-hexagram (list 3 0 1 3 1 2))
	  (i-ching-change-hexagram (list 1 1 3 1 0 0 2 0)))

;;* correspond nontrad
(defun i-ching-correspond-trigram (hex)
  "Given hexagram HEX, return a trigram representing the correspondence.

A hexagram is considered fully corresponding if the top trigram
mirrors the bottom.  If that is the case, then the trigram ||| is
returned.  (a yang line for every line-set in correspondence).

For instance, the hexagrams :|:|:| is considered fully correspondent.
But the hexagram :|::|: is not.

I do not think this is traditional in the i-ching."
  (list (i-ching-xor (first hex) (fourth hex))
		(i-ching-xor (second hex) (fifth hex))
		(i-ching-xor (third hex)  (sixth hex))))

(when nil
	  (i-ching-correspond-trigram (list 0 1 0 3 2 1))
	  (i-ching-correspond-trigram (list 1 0 1 1 0 1))
	  (i-ching-correspond-trigram (list 1 2 0 3 2 1)))

;;* correctness nontrad
(defun i-ching-correctness-hexagram (hex)
  "Given a hexagram HEX, return a hexagram showing its correctness.
Correctness is when a hexagram has yang lines in yang positions
and yin lines in yin positions.  hexagram 63 |:|:|: is considered
to be fully correct.

I do not think this is traditional in the i-ching."
  (list (i-ching-xor 0 (first hex))
		(i-ching-xor 1 (second hex))
		(i-ching-xor 0 (third hex))
		(i-ching-xor 1 (fourth hex))
		(i-ching-xor 0 (fifth hex))
		(i-ching-xor 1 (sixth hex))))

(when nil
	  (i-ching-correctness-hexagram (list 0 1 0 1 0 1))
	  (i-ching-correctness-hexagram (list 1 0 1 0 1 0))
	  (i-ching-correctness-hexagram (list 1 1 1 1 1 1))
	  (i-ching-correctness-hexagram (list 0 0 0 0 0 0))
	  (i-ching-correctness-hexagram (list 0 1 2 3 3 2)))

;;* cast interactive
(defun i-ching-lookup-caster ()
  "Do a look up on an i-ching hexagram.  Technically this is a casting."
  (i-ching-ascii-to-number (completing-read "Hexagram (: = yin, | = yang, X = changing yin, O = changing yang): "
											(mapcar 'car i-ching-hexagram-interpretation))))

;;* cast coin
(defun i-ching-coin-caster ()
  "Cast the i-ching using the flipping-three-coins method."
  (list (i-ching-coin-helper)
		(i-ching-coin-helper)
		(i-ching-coin-helper)
		(i-ching-coin-helper)
		(i-ching-coin-helper)
		(i-ching-coin-helper)))

;;* cast coin
(defun i-ching-coin-helper ()
  "Helper function for `i-ching-coin-caster'."
  (case (+ (apply i-ching-randomize-method (list 2 3))
		   (apply i-ching-randomize-method (list 2 3))
		   (apply i-ching-randomize-method (list 2 3)))
	(6 2)
	(7 1)
	(8 0)
	(9 3)))

;;* cast buffer
(defun i-ching-buffer-caster ()
  "Spit out an i-ching hexagram based on the md5 hash on the current buffer.

Note that the md5 hash is in the form of:
33483acc0fd3602a086fb7eeeedf4d95

::::|: - 08. Grouping (比 bǐ) 	Holding Together 	Union
|:::|: - 03. Sprouting (屯 chún) 	Difficulty at the Beginning  	Sprouting"
  (md5 (get-buffer "i-ching.el")))

;;* cast bead helper
(when (not (fboundp 'ring-convert-sequence-to-ring))
	  (defun ring-convert-sequence-to-ring (seq)
		"Convert sequence SEQ to a ring.  Return the ring.
If SEQ is already a ring, return it."
		(if (ring-p seq)
			seq
			(let* ((size (length seq))
				   (ring (make-ring size))
				   (count 0))
			  (while (< count size)
					 (if (or (ring-empty-p ring)
							 (not (equal (ring-ref ring 0) (elt seq count))))
						 (ring-insert-at-beginning ring (elt seq count)))
					 (setq count (1+ count)))
			  ring))))

;;* cast bead test
(defun i-ching-bead-caster ()
  "Cast the i-ching by selecting from a virtual string of beads.

This is done by selecting a position inside of a string of digits, that
contains every hexagram.  This apparently was done as a set of beads,
containing both black and white beads."

  "Notes: (these will be deleted soon)
  0000001001000101010000110100110010110110001110101110011110111111

  000000 1111110111100111010111000110110100110010110000101010001001
1st hex= the receptive
 (000000)1001000101010000110100110010110110001110101110011110111111
2nd hex= bound/field splitting apart
 0(000001)001000101010000110100110010110110001110101110011110111111

when you reach the last hexagrams you 'wrap around' the code...
 0)0000010010001010100001101001100101101100011101011100111101(11111
"
  
  (let ((casting-pos (apply i-ching-randomize-method 0 63))
		(casting-string
		 (ring-convert-sequence-to-ring
		  (list 0 0 0 0 0 0 1 0
				0 1 0 0 0 1 0 1
				0 1 0 0 0 0 1 1
				0 1 0 0 1 1 0 0
				1 0 1 1 0 1 1 0
				0 0 1 1 1 0 1 0
				1 1 1 0 0 1 1 1
				1 0 1 1 1 1 1 1))))
	(list (ring-plus1 casting-pos casting-string)
		  (ring-plus1 (+ 1 casting-pos) casting-string)
		  (ring-plus1 (+ 2 casting-pos) casting-string)
		  (ring-plus1 (+ 3 casting-pos) casting-string)
		  (ring-plus1 (+ 4 casting-pos) casting-string)
		  (ring-plus1 (+ 5 casting-pos) casting-string))))

;;* cast todo
(defun i-ching-chinese-date-caster ()
  "Cast the i-ching based on todays date on the chinese calendar.

Untill I can learn more about this method, I cannot actually write it.  :/"
  (calendar-chinese-date-string))

;;* cast nontrad
(defun i-ching-western-date-caster ()
  "Cast the i-ching according to todays date.

This is based strictly from the western date, as explained here:
http://www.netowne.com/eastern/iching/

This method is completely non-traditional, and may only be
loosely based on the plum-blossem method."
  (debug)
  (let* ((number-to-trigram
		  '((1 . (1 1 1))
			(2 . (1 1 0))
			(3 . (1 0 1))
			(4 . (1 0 0))
			(5 . (0 1 1))
			(6 . (0 1 0))
			(7 . (0 0 1))
			(8 . (0 0 0))))
		 (dst-time (decode-time))
		 (time (decode-time (encode-time (nth 0 dst-time)
										 (nth 1 dst-time)
										 (- (nth 2 dst-time)
											(if (nth 7 dst-time) 1 0))
										 (nth 3 dst-time)
										 (nth 4 dst-time)
										 (nth 5 dst-time))))
		 (year (+ 1 (% (nth 5 time)
					   12)))
		 (outer (+ 1 (% (+ (nth 3 time)
						   (nth 4 time)
						   year)
						8)))
		 (inner (+ 1 (% (+ (nth 3 time)
						   (nth 4 time)
						   year
						   (nth 2 time))
						8)))
		 (yao (% (+ (nth 3 time)
					(nth 4 time)
					year
					(nth 2 time))
				 6))
		 (hexagram (append (cdr (assoc inner number-to-trigram))
						   (cdr (assoc outer number-to-trigram)))))
	(loop for line in hexagram
		  for num from 0
		  collect
		  (if (= yao num)
			  (+ 2 line)
			  line) into out
		  finally return out)))

(when nil
	  (let ((i-ching-casting-method 'i-ching-western-date-caster))
		(i-ching-cast))
	  )

;;* random
(defun i-ching-standard-randomizer (min max)
  "A standard randomization function.  Return a number between MIN and MAX."
  (+ min (random (- (+ 1 max) min))))

;;* random
(defvar i-ching-random-org-cache nil
  "A cache of random numbers retrieved from `i-ching-random-org-randomizer'.")

;;* random
(defun i-ching-random-org-randomizer (min max)
  "Pull a random number from http://www.random.org/clients/http/ .

Returns a nubmer between MIN and MAX.

Uses a cache (`i-ching-random-org-cache') so repeated requests are not made."
  (when (null i-ching-random-org-cache)
		(save-excursion
		 (let ((random-buffer (url-retrieve-synchronously "http://www.random.org/integers/?num=100&min=1&max=999999&col=1&base=10&format=plain&rnd=new")))
		   (set-buffer random-buffer)
		   (goto-char (point-min))
		   (re-search-forward "HTTP/[0-9]+\\.[0-9]+ ")
		   (when (> (string-to-number (buffer-substring (point) (search-forward " ")))
					399)
				 (kill-buffer random-buffer)
				 (error "Cannot cast i-ching,  HTTP error from random.org"))
		   (search-forward "\n\n")
		   (let ((result (read (concat "(" (buffer-substring (point) (point-max)) ")"))))
			 (kill-buffer random-buffer)
			 (when (null result)
				   (error "Cannot read random numbers from random.org"))
			 (setq i-ching-random-org-cache result)))))
  (let ((rand-val (car i-ching-random-org-cache)))
	(setq i-ching-random-org-cache (cdr i-ching-random-org-cache))
	(+ min (% rand-val
			  (- (+ 1 max) min)))))

(when nil
	  (i-ching-random-org-randomizer 1 4)
	  (i-ching-random-org-randomizer 2 3))

;;* random todo
(defun i-ching-fuckup-randomizer (min max)
  "Perform an i-ching randomization function, using a process similar to FUCKUP.

This function should use a similar version of randomization as the computer
FUCKUP, as described by Robert Anton Wilson and Robert Shaes Illuminatus
Trology.

Uses standard MIN MAX arguments."
   )


;;*todo
;; Think about using these UTF-8 characters
"¦|"

(provide 'i-ching)

;;; i-ching ends here
