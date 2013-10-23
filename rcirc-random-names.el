;;; rcirc-random-names.el -- randomize names
;; Copyright 2009-2013  Alex Schroeder

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Maybe this will provide the illusion of a 50:50 gender presence
;; online. It's probably also confusing as hell.

;;; Code:

(require 'rcirc)

(defvar rcirc-random-names
  '("Jacob" "Michael" "Ethan" "Joshua" "Daniel" "Alexander" "Anthony"
    "William" "Christopher" "Matthew" "Jayden" "Andrew" "Joseph" "David"
    "Noah" "Aiden" "James" "Ryan" "Logan" "John" "Nathan" "Elijah"
    "Christian" "Gabriel" "Benjamin" "Jonathan" "Tyler" "Samuel"
    "Nicholas" "Gavin" "Dylan" "Jackson" "Brandon" "Caleb" "Mason" "Angel"
    "Isaac" "Evan" "Jack" "Kevin" "Jose" "Isaiah" "Luke" "Landon" "Justin"
    "Lucas" "Zachary" "Jordan" "Robert" "Aaron" "Brayden" "Thomas"
    "Cameron" "Hunter" "Austin" "Adrian" "Connor" "Owen" "Aidan" "Jason"
    "Julian" "Wyatt" "Charles" "Luis" "Carter" "Juan" "Chase" "Diego"
    "Jeremiah" "Brody" "Xavier" "Adam" "Carlos" "Sebastian" "Liam"
    "Hayden" "Nathaniel" "Henry" "Jesus" "Ian" "Tristan" "Bryan" "Sean"
    "Cole" "Alex" "Eric" "Brian" "Jaden" "Carson" "Blake" "Ayden" "Cooper"
    "Dominic" "Brady" "Caden" "Josiah" "Kyle" "Colton" "Kaden" "Eli"
    "Miguel" "Antonio" "Parker" "Steven" "Alejandro" "Riley" "Richard"
    "Timothy" "Devin" "Jesse" "Victor" "Jake" "Joel" "Colin" "Kaleb"
    "Bryce" "Levi" "Oliver" "Oscar" "Vincent" "Ashton" "Cody" "Micah"
    "Preston" "Marcus" "Max" "Patrick" "Seth" "Jeremy" "Peyton" "Nolan"
    "Ivan" "Damian" "Maxwell" "Alan" "Kenneth" "Jonah" "Jorge" "Mark"
    "Giovanni" "Eduardo" "Grant" "Collin" "Gage" "Omar" "Emmanuel"
    "Trevor" "Edward" "Ricardo" "Cristian" "Nicolas" "Kayden" "George"
    "Jaxon" "Paul" "Braden" "Elias" "Andres" "Derek" "Garrett" "Tanner"
    "Malachi" "Conner" "Fernando" "Cesar" "Javier" "Miles" "Jaiden"
    "Alexis" "Leonardo" "Santiago" "Francisco" "Cayden" "Shane" "Edwin"
    "Hudson" "Travis" "Bryson" "Erick" "Jace" "Hector" "Josue" "Peter"
    "Jaylen" "Mario" "Manuel" "Abraham" "Grayson" "Damien" "Kaiden"
    "Spencer" "Stephen" "Edgar" "Wesley" "Shawn" "Trenton" "Jared"
    "Jeffrey" "Landen" "Johnathan" "Bradley" "Braxton" "Ryder" "Camden"
    "Roman" "Asher" "Brendan" "Maddox" "Sergio" "Israel" "Andy" "Lincoln"
    "Erik" "Donovan" "Raymond" "Avery" "Rylan" "Dalton" "Harrison" "Andre"
    "Martin" "Keegan" "Marco" "Jude" "Sawyer" "Dakota" "Leo" "Calvin"
    "Kai" "Drake" "Troy" "Zion" "Clayton" "Roberto" "Zane" "Gregory"
    "Tucker" "Rafael" "Kingston" "Dominick" "Ezekiel" "Griffin" "Devon"
    "Drew" "Lukas" "Johnny" "Ty" "Pedro" "Tyson" "Caiden" "Mateo"
    "Braylon" "Cash" "Aden" "Chance" "Taylor" "Marcos" "Maximus" "Ruben"
    "Emanuel" "Simon" "Corbin" "Brennan" "Dillon" "Skyler" "Myles"
    "Xander" "Jaxson" "Dawson" "Kameron" "Kyler" "Axel" "Colby" "Jonas"
    "Joaquin" "Payton" "Brock" "Frank" "Enrique" "Quinn" "Emilio" "Malik"
    "Grady" "Angelo" "Julio" "Derrick" "Raul" "Fabian" "Corey" "Gerardo"
    "Dante" "Ezra" "Armando" "Allen" "Theodore" "Gael" "Amir" "Zander"
    "Adan" "Maximilian" "Randy" "Easton" "Dustin" "Luca" "Phillip"
    "Julius" "Charlie" "Ronald" "Jakob" "Cade" "Brett" "Trent" "Silas"
    "Keith" "Emiliano" "Trey" "Jalen" "Darius" "Lane" "Jerry" "Jaime"
    "Scott" "Graham" "Weston" "Braydon" "Anderson" "Rodrigo" "Pablo"
    "Saul" "Danny" "Donald" "Elliot" "Brayan" "Dallas" "Lorenzo" "Casey"
    "Mitchell" "Alberto" "Tristen" "Rowan" "Jayson" "Gustavo" "Aaden"
    "Amari" "Dean" "Braeden" "Declan" "Chris" "Ismael" "Dane" "Louis"
    "Arturo" "Brenden" "Felix" "Jimmy" "Cohen" "Tony" "Holden" "Reid"
    "Abel" "Bennett" "Zackary" "Arthur" "Nehemiah" "Ricky" "Esteban"
    "Cruz" "Finn" "Mauricio" "Dennis" "Keaton" "Albert" "Marvin" "Mathew"
    "Larry" "Moises" "Issac" "Philip" "Quentin" "Curtis" "Greyson"
    "Jameson" "Everett" "Jayce" "Darren" "Elliott" "Uriel" "Alfredo"
    "Hugo" "Alec" "Jamari" "Marshall" "Walter" "Judah" "Jay" "Lance"
    "Beau" "Ali" "Landyn" "Yahir" "Phoenix" "Nickolas" "Kobe" "Bryant"
    "Maurice" "Russell" "Leland" "Colten" "Reed" "Davis" "Joe" "Ernesto"
    "Desmond" "Kade" "Reece" "Morgan" "Ramon" "Rocco" "Orlando" "Ryker"
    "Brodie" "Paxton" "Jacoby" "Douglas" "Kristopher" "Gary" "Lawrence"
    "Izaiah" "Solomon" "Nikolas" "Mekhi" "Justice" "Tate" "Jaydon"
    "Salvador" "Shaun" "Alvin" "Eddie" "Kane" "Davion" "Zachariah"
    "Dorian" "Titus" "Kellen" "Camron" "Isiah" "Javon" "Nasir" "Milo"
    "Johan" "Byron" "Jasper" "Jonathon" "Chad" "Marc" "Kelvin" "Chandler"
    "Sam" "Cory" "Deandre" "River" "Reese" "Roger" "Quinton" "Talon"
    "Romeo" "Franklin" "Noel" "Alijah" "Guillermo" "Gunner" "Damon"
    "Jadon" "Emerson" "Micheal" "Bruce" "Terry" "Kolton" "Melvin"
    "Beckett" "Porter" "August" "Brycen" "Dayton" "Jamarion" "Leonel"
    "Karson" "Zayden" "Keagan" "Carl" "Khalil" "Cristopher" "Nelson"
    "Braiden" "Moses" "Isaias" "Roy" "Triston" "Walker" "Kale" "Jermaine"
    "Leon" "Rodney" "Kristian" "Mohamed" "Ronan" "Pierce" "Trace" "Warren"
    "Jeffery" "Maverick" "Cyrus" "Quincy" "Nathanael" "Skylar" "Tommy"
    "Conor" "Noe" "Ezequiel" "Demetrius" "Jaylin" "Kendrick" "Frederick"
    "Terrance" "Bobby" "Jamison" "Jon" "Rohan" "Jett" "Kieran" "Tobias"
    "Ari" "Colt" "Gideon" "Felipe" "Kenny" "Wilson" "Orion" "Kamari"
    "Gunnar" "Jessie" "Alonzo" "Gianni" "Omari" "Waylon" "Malcolm"
    "Emmett" "Abram" "Julien" "London" "Tomas" "Allan" "Terrell" "Matteo"
    "Tristin" "Jairo" "Reginald" "Brent" "Ahmad" "Yandel" "Rene" "Willie"
    "Boston" "Billy" "Marlon" "Trevon" "Aydan" "Jamal" "Aldo" "Ariel"
    "Cason" "Braylen" "Javion" "Joey" "Rogelio" "Ahmed" "Dominik"
    "Brendon" "Toby" "Kody" "Marquis" "Ulises" "Armani" "Adriel" "Alfonso"
    "Branden" "Will" "Craig" "Ibrahim" "Osvaldo" "Wade" "Harley" "Steve"
    "Davin" "Deshawn" "Kason" "Damion" "Jaylon" "Jefferson" "Aron"
    "Brooks" "Darian" "Gerald" "Rolando" "Terrence" "Enzo" "Kian" "Ryland"
    "Barrett" "Jaeden" "Ben" "Bradyn" "Giovani" "Blaine" "Madden" "Jerome"
    "Muhammad" "Ronnie" "Layne" "Kolby" "Leonard" "Vicente" "Cale"
    "Alessandro" "Zachery" "Gavyn" "Aydin" "Xzavier" "Malakai" "Raphael"
    "Cannon" "Rudy" "Asa" "Darrell" "Giancarlo" "Elisha" "Junior"
    "Zackery" "Alvaro" "Lewis" "Valentin" "Deacon" "Jase" "Harry"
    "Kendall" "Rashad" "Finnegan" "Mohammed" "Ramiro" "Cedric" "Brennen"
    "Santino" "Stanley" "Tyrone" "Chace" "Francis" "Johnathon" "Teagan"
    "Zechariah" "Alonso" "Kaeden" "Kamden" "Gilberto" "Ray" "Karter"
    "Luciano" "Nico" "Kole" "Aryan" "Draven" "Jamie" "Misael" "Lee"
    "Alexzander" "Camren" "Giovanny" "Amare" "Rhett" "Rhys" "Rodolfo"
    "Nash" "Markus" "Deven" "Mohammad" "Moshe" "Quintin" "Dwayne"
    "Memphis" "Atticus" "Davian" "Eugene" "Jax" "Antoine" "Wayne"
    "Randall" "Semaj" "Uriah" "Clark" "Aidyn" "Jorden" "Maxim" "Aditya"
    "Lawson" "Messiah" "Korbin" "Sullivan" "Freddy" "Demarcus" "Neil"
    "Brice" "King" "Davon" "Elvis" "Ace" "Dexter" "Heath" "Duncan" "Jamar"
    "Sincere" "Irvin" "Remington" "Kadin" "Soren" "Tyree" "Damarion"
    "Talan" "Adrien" "Gilbert" "Keenan" "Darnell" "Adolfo" "Tristian"
    "Derick" "Isai" "Rylee" "Gauge" "Harold" "Kareem" "Deangelo" "Agustin"
    "Coleman" "Zavier" "Lamar" "Emery" "Jaydin" "Devan" "Jordyn" "Mathias"
    "Prince" "Sage" "Seamus" "Jasiah" "Efrain" "Darryl" "Arjun" "Mike"
    "Roland" "Conrad" "Kamron" "Hamza" "Santos" "Frankie" "Dominique"
    "Marley" "Vance" "Dax" "Jamir" "Kylan" "Todd" "Maximo" "Jabari"
    "Matthias" "Haiden" "Luka" "Marcelo" "Keon" "Layton" "Tyrell" "Kash"
    "Raiden" "Cullen" "Donte" "Jovani" "Cordell" "Kasen" "Rory" "Alfred"
    "Darwin" "Ernest" "Bailey" "Gaige" "Hassan" "Jamarcus" "Killian"
    "Augustus" "Trevin" "Zain" "Ellis" "Rex" "Yusuf" "Bruno" "Jaidyn"
    "Justus" "Ronin" "Humberto" "Jaquan" "Josh" "Kasey" "Winston"
    "Dashawn" "Lucian" "Matias" "Sidney" "Ignacio" "Nigel" "Van" "Elian"
    "Finley" "Jaron" "Addison" "Aedan" "Braedon" "Jadyn" "Konner" "Zayne"
    "Franco" "Niko" "Savion" "Cristofer" "Deon" "Krish" "Anton" "Brogan"
    "Cael" "Coby" "Kymani" "Marcel" "Yair" "Dale" "Bo" "Jordon" "Samir"
    "Darien" "Zaire" "Ross" "Vaughn" "Devyn" "Kenyon" "Clay" "Dario"
    "Ishaan" "Jair" "Kael" "Adonis" "Jovanny" "Clinton" "Rey" "Chaim"
    "German" "Harper" "Nathen" "Rigoberto" "Sonny" "Glenn" "Octavio"
    "Blaze" "Keshawn" "Ralph" "Ean" "Nikhil" "Rayan" "Sterling" "Branson"
    "Jadiel" "Dillan" "Jeramiah" "Koen" "Konnor" "Antwan" "Houston"
    "Tyrese" "Dereon" "Leonidas" "Zack" "Fisher" "Jaydan" "Quinten" "Nick"
    "Urijah" "Darion" "Jovan" "Salvatore" "Beckham" "Jarrett" "Antony"
    "Eden" "Makai" "Zaiden" "Broderick" "Camryn" "Malaki" "Nikolai"
    "Howard" "Immanuel" "Demarion" "Valentino" "Jovanni" "Ayaan" "Ethen"
    "Leandro" "Royce" "Yael" "Yosef" "Jean" "Marquise" "Alden" "Leroy"
    "Gaven" "Jovany" "Tyshawn" "Aarav" "Kadyn" "Milton" "Zaid" "Kelton"
    "Tripp" "Kamren" "Slade" "Hezekiah" "Jakobe" "Nathanial" "Rishi"
    "Shamar" "Geovanni" "Pranav" "Roderick" "Bentley" "Clarence" "Lyric"
    "Bernard" "Carmelo" "Denzel" "Maximillian" "Reynaldo" "Cassius"
    "Gordon" "Reuben" "Samson" "Yadiel" "Jayvon" "Reilly" "Sheldon"
    "Abdullah" "Jagger" "Thaddeus" "Case" "Kyson" "Lamont" "Chaz" "Makhi"
    "Jan" "Marques" "Oswaldo" "Donavan" "Keyon" "Kyan" "Simeon" "Trystan"
    "Andreas" "Dangelo" "Landin" "Reagan" "Turner" "Arnav" "Brenton"
    "Callum" "Jayvion" "Bridger" "Sammy" "Deegan" "Jaylan" "Lennon" "Odin"
    "Abdiel" "Jerimiah" "Eliezer" "Bronson" "Cornelius" "Pierre" "Cortez"
    "Baron" "Carlo" "Carsen" "Fletcher" "Izayah" "Kolten" "Damari" "Hugh"
    "Jensen" "Yurem" "Emma" "Isabella" "Emily"
    "Madison" "Ava" "Olivia" "Sophia" "Abigail" "Elizabeth" "Chloe"
    "Samantha" "Addison" "Natalie" "Mia" "Alexis" "Alyssa" "Hannah"
    "Ashley" "Ella" "Sarah" "Grace" "Taylor" "Brianna" "Lily" "Hailey"
    "Anna" "Victoria" "Kayla" "Lillian" "Lauren" "Kaylee" "Allison"
    "Savannah" "Nevaeh" "Gabriella" "Sofia" "Makayla" "Avery" "Riley"
    "Julia" "Leah" "Aubrey" "Jasmine" "Audrey" "Katherine" "Morgan"
    "Brooklyn" "Destiny" "Sydney" "Alexa" "Kylie" "Brooke" "Kaitlyn"
    "Evelyn" "Layla" "Madeline" "Kimberly" "Zoe" "Jessica" "Peyton"
    "Alexandra" "Claire" "Madelyn" "Maria" "Mackenzie" "Arianna" "Jocelyn"
    "Amelia" "Angelina" "Trinity" "Andrea" "Maya" "Valeria" "Sophie"
    "Rachel" "Vanessa" "Aaliyah" "Mariah" "Gabrielle" "Katelyn" "Ariana"
    "Bailey" "Camila" "Jennifer" "Melanie" "Gianna" "Charlotte" "Paige"
    "Autumn" "Payton" "Faith" "Sara" "Isabelle" "Caroline" "Genesis"
    "Isabel" "Mary" "Zoey" "Gracie" "Megan" "Haley" "Mya" "Michelle"
    "Molly" "Stephanie" "Nicole" "Jenna" "Natalia" "Sadie" "Jada"
    "Serenity" "Lucy" "Ruby" "Eva" "Kennedy" "Rylee" "Jayla" "Naomi"
    "Rebecca" "Lydia" "Daniela" "Bella" "Keira" "Adriana" "Lilly" "Hayden"
    "Miley" "Katie" "Jade" "Jordan" "Gabriela" "Amy" "Angela" "Melissa"
    "Valerie" "Giselle" "Diana" "Amanda" "Kate" "Laila" "Reagan" "Jordyn"
    "Kylee" "Danielle" "Briana" "Marley" "Leslie" "Kendall" "Catherine"
    "Liliana" "Mckenzie" "Jacqueline" "Ashlyn" "Reese" "Marissa" "London"
    "Juliana" "Shelby" "Cheyenne" "Angel" "Daisy" "Makenzie" "Miranda"
    "Erin" "Amber" "Alana" "Ellie" "Breanna" "Ana" "Mikayla" "Summer"
    "Piper" "Adrianna" "Jillian" "Sierra" "Jayden" "Sienna" "Alicia"
    "Lila" "Margaret" "Alivia" "Brooklynn" "Karen" "Violet" "Sabrina"
    "Stella" "Aniyah" "Annabelle" "Alexandria" "Kathryn" "Skylar" "Aliyah"
    "Delilah" "Julianna" "Kelsey" "Khloe" "Carly" "Amaya" "Mariana"
    "Christina" "Alondra" "Tessa" "Eliana" "Bianca" "Jazmin" "Clara"
    "Vivian" "Josephine" "Delaney" "Scarlett" "Elena" "Cadence" "Alexia"
    "Maggie" "Laura" "Nora" "Ariel" "Elise" "Nadia" "Mckenna" "Chelsea"
    "Lyla" "Alaina" "Jasmin" "Hope" "Leila" "Caitlyn" "Cassidy" "Makenna"
    "Allie" "Izabella" "Eden" "Callie" "Haylee" "Caitlin" "Kendra"
    "Karina" "Kyra" "Kayleigh" "Addyson" "Kiara" "Jazmine" "Karla"
    "Camryn" "Alina" "Lola" "Kyla" "Kelly" "Fatima" "Tiffany" "Kira"
    "Crystal" "Mallory" "Esmeralda" "Alejandra" "Eleanor" "Angelica"
    "Jayda" "Abby" "Kara" "Veronica" "Carmen" "Jamie" "Ryleigh"
    "Valentina" "Allyson" "Dakota" "Kamryn" "Courtney" "Cecilia"
    "Madeleine" "Aniya" "Alison" "Esther" "Heaven" "Aubree" "Lindsey"
    "Leilani" "Nina" "Melody" "Macy" "Ashlynn" "Joanna" "Cassandra"
    "Alayna" "Kaydence" "Madilyn" "Aurora" "Heidi" "Emerson" "Kimora"
    "Madalyn" "Erica" "Josie" "Katelynn" "Guadalupe" "Harper" "Ivy" "Lexi"
    "Camille" "Savanna" "Dulce" "Daniella" "Lucia" "Emely" "Joselyn"
    "Kiley" "Kailey" "Miriam" "Cynthia" "Rihanna" "Georgia" "Rylie"
    "Harmony" "Kiera" "Kyleigh" "Monica" "Bethany" "Kaylie" "Cameron"
    "Teagan" "Cora" "Brynn" "Ciara" "Genevieve" "Alice" "Maddison" "Eliza"
    "Tatiana" "Jaelyn" "Erika" "Ximena" "April" "Marely" "Julie" "Danica"
    "Presley" "Brielle" "Julissa" "Angie" "Iris" "Brenda" "Hazel" "Rose"
    "Malia" "Shayla" "Fiona" "Phoebe" "Nayeli" "Paola" "Kaelyn" "Selena"
    "Audrina" "Rebekah" "Carolina" "Janiyah" "Michaela" "Penelope"
    "Janiya" "Anastasia" "Adeline" "Ruth" "Sasha" "Denise" "Holly"
    "Madisyn" "Hanna" "Tatum" "Marlee" "Nataly" "Helen" "Janelle"
    "Lizbeth" "Serena" "Anya" "Jaslene" "Kaylin" "Jazlyn" "Nancy"
    "Lindsay" "Desiree" "Hayley" "Itzel" "Imani" "Madelynn" "Asia"
    "Kadence" "Madyson" "Talia" "Jane" "Kayden" "Annie" "Amari" "Bridget"
    "Raegan" "Jadyn" "Celeste" "Jimena" "Luna" "Yasmin" "Emilia" "Annika"
    "Estrella" "Sarai" "Lacey" "Ayla" "Alessandra" "Willow" "Nyla"
    "Dayana" "Lilah" "Lilliana" "Natasha" "Hadley" "Harley" "Priscilla"
    "Claudia" "Allisson" "Baylee" "Brenna" "Brittany" "Skyler" "Fernanda"
    "Danna" "Melany" "Cali" "Lia" "Macie" "Lyric" "Logan" "Gloria" "Lana"
    "Mylee" "Cindy" "Lilian" "Amira" "Anahi" "Alissa" "Anaya" "Lena"
    "Ainsley" "Sandra" "Noelle" "Marisol" "Meredith" "Kailyn" "Lesly"
    "Johanna" "Diamond" "Evangeline" "Juliet" "Kathleen" "Meghan"
    "Paisley" "Athena" "Hailee" "Rosa" "Wendy" "Emilee" "Sage" "Alanna"
    "Elaina" "Cara" "Nia" "Paris" "Casey" "Dana" "Emery" "Rowan" "Aubrie"
    "Kaitlin" "Jaden" "Kenzie" "Kiana" "Viviana" "Norah" "Lauryn" "Perla"
    "Amiyah" "Alyson" "Rachael" "Shannon" "Aileen" "Miracle" "Lillie"
    "Danika" "Heather" "Kassidy" "Taryn" "Tori" "Francesca" "Kristen"
    "Amya" "Elle" "Kristina" "Cheyanne" "Haylie" "Patricia" "Anne"
    "Samara" "Skye" "Kali" "America" "Lexie" "Parker" "Halle" "Londyn"
    "Abbigail" "Linda" "Hallie" "Saniya" "Bryanna" "Bailee" "Jaylynn"
    "Mckayla" "Quinn" "Jaelynn" "Jaida" "Caylee" "Jaiden" "Melina" "Abril"
    "Sidney" "Kassandra" "Elisabeth" "Adalyn" "Kaylynn" "Mercedes"
    "Yesenia" "Elliana" "Brylee" "Dylan" "Isabela" "Ryan" "Ashlee"
    "Daphne" "Kenya" "Marina" "Christine" "Mikaela" "Kaitlynn" "Justice"
    "Saniyah" "Jaliyah" "Ingrid" "Marie" "Natalee" "Joy" "Juliette"
    "Simone" "Adelaide" "Krystal" "Kennedi" "Mila" "Tamia" "Addisyn"
    "Aylin" "Dayanara" "Sylvia" "Clarissa" "Maritza" "Virginia" "Braelyn"
    "Jolie" "Jaidyn" "Kinsley" "Kirsten" "Laney" "Marilyn" "Whitney"
    "Janessa" "Raquel" "Anika" "Kamila" "Aria" "Rubi" "Adelyn" "Amara"
    "Ayanna" "Teresa" "Zariah" "Kaleigh" "Amani" "Carla" "Yareli"
    "Gwendolyn" "Paulina" "Nathalie" "Annabella" "Jaylin" "Tabitha"
    "Deanna" "Madalynn" "Journey" "Aiyana" "Skyla" "Yaretzi" "Ada" "Liana"
    "Karlee" "Jenny" "Myla" "Cristina" "Myah" "Lisa" "Tania" "Isis"
    "Jayleen" "Jordin" "Arely" "Azul" "Helena" "Aryanna" "Jaqueline"
    "Lucille" "Destinee" "Martha" "Zoie" "Arielle" "Liberty" "Marlene"
    "Elisa" "Isla" "Noemi" "Raven" "Jessie" "Aleah" "Kailee" "Kaliyah"
    "Lilyana" "Haven" "Tara" "Giana" "Camilla" "Maliyah" "Irene" "Carley"
    "Maeve" "Lea" "Macey" "Sharon" "Alisha" "Marisa" "Jaylene" "Kaya"
    "Scarlet" "Siena" "Adyson" "Maia" "Shiloh" "Tiana" "Jaycee" "Gisselle"
    "Yazmin" "Eve" "Shyanne" "Arabella" "Sherlyn" "Sariah" "Amiya"
    "Kiersten" "Madilynn" "Shania" "Aleena" "Finley" "Kinley" "Kaia"
    "Aliya" "Taliyah" "Pamela" "Yoselin" "Ellen" "Carlie" "Monserrat"
    "Jakayla" "Reyna" "Yaritza" "Carolyn" "Clare" "Lorelei" "Paula"
    "Zaria" "Gracelyn" "Kasey" "Regan" "Alena" "Angelique" "Regina"
    "Britney" "Emilie" "Mariam" "Jaylee" "Julianne" "Greta" "Elyse"
    "Lainey" "Kallie" "Felicity" "Zion" "Aspen" "Carlee" "Annalise"
    "Iliana" "Larissa" "Akira" "Sonia" "Catalina" "Phoenix" "Joslyn"
    "Anabelle" "Mollie" "Susan" "Judith" "Destiney" "Hillary" "Janet"
    "Katrina" "Mareli" "Ansley" "Kaylyn" "Alexus" "Gia" "Maci" "Elsa"
    "Stacy" "Kaylen" "Carissa" "Haleigh" "Lorena" "Jazlynn" "Milagros"
    "Luz" "Leanna" "Renee" "Shaniya" "Charlie" "Abbie" "Cailyn" "Cherish"
    "Elsie" "Jazmyn" "Elaine" "Emmalee" "Luciana" "Dahlia" "Jamya"
    "Belinda" "Mariyah" "Chaya" "Dayami" "Rhianna" "Yadira" "Aryana"
    "Rosemary" "Armani" "Cecelia" "Celia" "Barbara" "Cristal" "Eileen"
    "Rayna" "Campbell" "Amina" "Aisha" "Amirah" "Ally" "Araceli" "Averie"
    "Mayra" "Sanaa" "Patience" "Leyla" "Selah" "Zara" "Chanel" "Kaiya"
    "Keyla" "Miah" "Aimee" "Giovanna" "Amelie" "Kelsie" "Alisson"
    "Angeline" "Dominique" "Adrienne" "Brisa" "Cierra" "Paloma" "Isabell"
    "Precious" "Alma" "Charity" "Jacquelyn" "Janae" "Frances" "Shyla"
    "Janiah" "Kierra" "Karlie" "Annabel" "Jacey" "Karissa" "Jaylah"
    "Xiomara" "Edith" "Marianna" "Damaris" "Deborah" "Jaylyn" "Evelin"
    "Mara" "Olive" "Ayana" "India" "Kendal" "Kayley" "Tamara" "Briley"
    "Charlee" "Nylah" "Abbey" "Moriah" "Saige" "Savanah" "Giada" "Hana"
    "Lizeth" "Matilda" "Ann" "Jazlene" "Gillian" "Beatrice" "Ireland"
    "Karly" "Mylie" "Yasmine" "Ashly" "Kenna" "Maleah" "Corinne" "Keely"
    "Tanya" "Tianna" "Adalynn" "Ryann" "Salma" "Areli" "Karma" "Shyann"
    "Kaley" "Theresa" "Evie" "Gina" "Roselyn" "Kaila" "Jaylen" "Natalya"
    "Meadow" "Rayne" "Aliza" "Yuliana" "June" "Lilianna" "Nathaly" "Ali"
    "Alisa" "Aracely" "Belen" "Tess" "Jocelynn" "Litzy" "Makena" "Abagail"
    "Giuliana" "Joyce" "Libby" "Lillianna" "Thalia" "Tia" "Sarahi"
    "Zaniyah" "Kristin" "Lorelai" "Mattie" "Taniya" "Jaslyn" "Gemma"
    "Valery" "Lailah" "Mckinley" "Micah" "Deja" "Frida" "Brynlee" "Jewel"
    "Krista" "Mira" "Yamilet" "Adison" "Carina" "Karli" "Magdalena"
    "Stephany" "Charlize" "Raelynn" "Aliana" "Cassie" "Mina" "Karley"
    "Shirley" "Marlie" "Alani" "Taniyah" "Cloe" "Sanai" "Lina" "Nola"
    "Anabella" "Dalia" "Raina" "Mariela" "Ariella" "Bria" "Kamari"
    "Monique" "Ashleigh" "Reina" "Alia" "Ashanti" "Lara" "Lilia" "Justine"
    "Leia" "Maribel" "Abigayle" "Tiara" "Alannah" "Princess" "Sydnee"
    "Kamora" "Paityn" "Payten" "Naima" "Gretchen" "Heidy" "Nyasia" "Livia"
    "Marin" "Shaylee" "Maryjane" "Laci" "Nathalia" "Azaria" "Anabel"
    "Chasity" "Emmy" "Izabelle" "Denisse" "Emelia" "Mireya" "Shea" "Amiah"
    "Dixie" "Maren" "Averi" "Esperanza" "Micaela" "Selina" "Alyvia"
    "Chana" "Avah" "Donna" "Kaylah" "Ashtyn" "Karsyn" "Makaila" "Shayna"
    "Essence" "Leticia" "Miya" "Rory" "Desirae" "Kianna" "Laurel" "Neveah"
    "Amaris" "Hadassah" "Dania" "Hailie" "Jamiya" "Kathy" "Laylah" "Riya"
    "Diya" "Carleigh" "Iyana" "Kenley" "Sloane" "Elianna")
  "List of popular names to be randomized.
The default names are taken from some random web page.
http://www.behindthename.com/top/lists/1000us2008.php")

(defvar rcirc-random-name-mapping (make-hash-table :test 'equal)
  "Hash-map mapping names to random names.")

(defun rcirc-get-name (name)
  "Get a random name for NAME.
Get it from the `rcirc-random-name-mapping' hashtable if possible.
Otherwise pick one at random from `rcirc-random-names'
and store it in the hashtable."
  (let ((result (gethash name rcirc-random-name-mapping)))
    (unless result
      (setq result (nth (random (length rcirc-random-names))
			rcirc-random-names))
      (puthash name result rcirc-random-name-mapping))
    result))

(defun rcirc-markup-random-names (sender response)
  "Replace all the nicks with picks from `rcirc-random-names'."
  (when rcirc-random-names-mode
    (with-syntax-table rcirc-nick-syntax-table
      (while (and (re-search-forward "\\w+" nil t)
		  ;; don't markup output of rcirc-do-realname
		  (not (looking-at " →")))
	(unless (get-text-property (match-beginning 0) 'random-name )
	  (let ((name (gethash (match-string-no-properties 0)
			       rcirc-random-name-mapping)))
	    (when name
	      (put-text-property (match-beginning 0) (match-end 0)
				 'display name))))))))

(add-to-list 'rcirc-markup-text-functions 'rcirc-markup-random-names)

(defadvice rcirc-facify (before rcirc-facify-random-names first activate)
  "Add random names to nicks based on `rcirc-random-names'."
  (when (and rcirc-random-names-mode
	     (or (eq face 'rcirc-other-nick)
		 (eq face 'rcirc-my-nick))
             (not (string= string "")))
    (setq string (propertize string 'display
			     (rcirc-get-name
			      (substring-no-properties string))))))

(defadvice rcirc-completion-at-point
  (after rcirc-completion-at-point-random-names activate)
  "Add the random names used to the possible completions."
  ;; shortcut: we're looking at nicks, not commands, if the first
  ;; value doesn't start with / -- also remember that ad-return-value
  ;; is (list beg (point) table)
  (when (and rcirc-random-names-mode
	     (not (eq (aref (first (third ad-return-value)) 0) ?/)))
    (let (names)
      (maphash (lambda (key value) (setq names (cons value names)))
	       rcirc-random-name-mapping)
      (setf (third ad-return-value)
	    (append names (third ad-return-value))))))

(defadvice rcirc-process-input-line
  (before rcirc-process-input-line-random-names activate)
  "Before sending something, let's replace random names with nicks again.
This is slow an inefficient, but it only happens when you send something."
  ;; don't use rcirc-nick-syntax-table since we're looking for
  ;; `rcirc-random-names'
  (when rcirc-random-names-mode
    (maphash (lambda (nick name)
	       (setq line (replace-regexp-in-string name nick line t t)))
	     rcirc-random-name-mapping)))

(defun-rcirc-command realname (args)
  "Reveal somebody's real name, or list all the entries
in `rcirc-random-name-mapping'."
  (interactive)
  (setq args (split-string args))
  (rcirc-do-realname (first args) process target))

(defun rcirc-do-realname (nick process target)
  "Implement /REALNAME."
  (let (names)
    (if nick
	(setq names (list (concat nick " → "
				  (gethash nick rcirc-random-name-mapping))))
      (maphash (lambda (key value)
		 (setq names (cons (concat key " → " value)
				   names)))
	       rcirc-random-name-mapping))
    (rcirc-print process (rcirc-nick process) "NOTICE" target
		 (mapconcat 'identity names "; "))))

(define-minor-mode rcirc-random-names-mode
  "This name replaces all the nicks in your IRC channels with random names."
  :lighter " Rnd"
  :global t)

(provide 'rcirc-random-names)
;; rcirc-random-names.el ends here
