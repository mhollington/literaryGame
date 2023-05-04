myTitle = "Frog Game"

-- this is used for drawing letters
letterWidth = 7

-- this command is issued when the app starts to get your first random word
getRandChoices = Random.generate 
                    NewImage 
                    oneRandIdx 

-- this asks for a random image in the allowed range
oneRandIdx = Random.int 0 (arrayLength - 1)


-- MODEL --
type alias Model = { guessedOneWrong : Bool
                   , score : Int
                   , state : Maybe (Shape Msg)
                   , letters : List (Char,(Float,Float))
                   , matched : List Char
                   , nextMatch : Char
                   , unmatched : List Char
                   , shape : { time : Float } -> List (Shape Msg)
                   , time : Float
                   , nextGen : Float
                   , playing : Bool
                   , angle : Float
                   , headPos : (Float, Float) 
                   , letterPos : (Float, Float)
                   , page : PageState
                   , timer : Float
                   , wordCounter : Int
                   }

init : Model
init  = { time = 0
        , nextGen = 2
        , shape = \t -> []
        , matched = []
        , nextMatch = 'c'
        , unmatched = ['a','t']
        , state = Nothing
        , letters= alphabet_pt2 ++ alphabet_pt1
        , score=0
        , guessedOneWrong=False
        , playing = False
        , angle = 0
        , headPos = (0, 0) 
        , letterPos = (0, 0)
        , page = Welcome
        , timer = 0
        , wordCounter = 0
        }


type PageState = Welcome | Game | EndPage
-- MESSAGE --
type Msg
    = Tick Float GetKeyState -- basic type needed, this gets sent to your update 30 times per second
    | GetNewPicture -- message from the button telling the program to find a new random number
    | NewImage Int -- the random number generator returns with a new image
    | Aim (Float, Float)
    | Tongue (Float, Float) Char
    | ChangePageState
    
 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangePageState -> 
          case model.page of 
            Welcome ->    
               ( { model | page = Game 
                         , playing = True
                         , score = 0
                         , wordCounter = 0
                         , timer = model.time + 60
                         }, Cmd.none )
            Game ->    
               ( { model | page = EndPage
                         , playing = False }, Cmd.none )   
            EndPage ->
              ( { model | page = Welcome
                          }, Cmd.none )
                       
        Tick t (_,(x,y),_) ->
            let
              deltaT = t - model.time
              (oldPosX,oldPosY) = model.headPos
              newPosY = if oldPosY > 100 then -- Limits the player not to move away from the frame
                                         100
                                      else if oldPosY < -100 then
                                         -100
                                      else (oldPosY + 30*deltaT*y) 
              newPosX = if oldPosX > 200 then
                                         200
                                      else if oldPosX < -250 then
                                         -250
                                      else oldPosX + 30*deltaT*x
            in
               ( { model | time = t      
                 }

                , Cmd.none )

        GetNewPicture ->
            ( model, Random.generate NewImage (Random.int 0 (arrayLength-1))
            )
        Aim (x, y) -> if x <= 0 then
                        ({ model | angle = findAngle (findCos (x, y) (0, 10)) 
                                ,  headPos = rotatePos (0, 20) (Basics.acos (findCos (x, y) (0, 10)))}
                                , Cmd.none)
                      else
                        ({ model | angle = (-1) * findAngle (findCos (x, y) (0, 10)) 
                                 , headPos = rotatePos (0, 20) ((-1) * Basics.acos (findCos (x, y) (0, 10)))
                                 }, Cmd.none)

        Tongue (x, y) char -> let 
                                goodChoice = classify model.nextMatch char
                                deltaScore = if goodChoice then 
                                               10 
                                             else 
                                               0
                               
                                          
                                (newMatched,newMatching,newUnmatched) =
                                  if goodChoice then
                                    case model.unmatched of
                                      c0 :: cs -> (model.matched ++ [model.nextMatch],c0,cs)
                                      [] -> (model.matched ++ [model.nextMatch],'#',[])
                                  else
                                    (model.matched,model.nextMatch,model.unmatched)
                                count = if newMatching == '#' then
                                          1
                                        else 0
                              in 
                                ({ model | headPos = (x, y)
                                         , matched = newMatched
                                         , score = model.score + deltaScore
                                         , nextMatch = newMatching
                                         , wordCounter = model.wordCounter + count
                                         , unmatched = newUnmatched
                                         , playing = if model.nextMatch == '#' then False else True
                                          }
                                         , Cmd.none)
                              
       
        NewImage n ->
            -- called with this message when the Random Number Generator returns
            let
                ( word, shape ) =
                    --extract the word and shape from the array
                    case
                        (Array.get n wordArray)
                    of
                        -- index exists
                        Just ( (user, wrd, grade), (name, school, sh) ) ->
                            ( wrd, sh )

                        -- index does not exist, use some defaults
                        Nothing ->
                            ( "", \t -> [] )
                (let0, lets) = case String.toList word of
                                    (l :: ll) -> (l, ll)
                                    [] -> ('*',[]) -- this should be impossible
            in
                --update the word and shape in our model, and don't launch another command
                ( { model | shape = shape
                          , matched = []
                          , nextMatch = let0
                          , unmatched = lets
                          , playing = True}
                , Cmd.none )




-- either bubble is far away, or it is the wrong character
-- take out stuff in update function make it my own
classify expected c = 
  if expected == c then
    True 
  else 
    False

myShapes model = [ rect 700 300 |> filled lightBlue |> move (0,20) |> notifyMouseMoveAt Aim
                 , polygon [ model.headPos, model.letterPos ] |> outlined (solid 2) red
                 , fish model model.angle 
                 , circle_list alphabet_pt1 [] 
                 , circle_list alphabet_pt2 []  

                 ] 


fish model angle =
  group
  [
   polygon [(0.5831,13.266),(3.4988,12.391),(4.9567,11.225),(6.1230,11.225),
   (7.2892,10.350),(7.8724,9.1845),(8.1640,8.3097),(9.0387,6.8519),(9.3302,5.1025),
   (9.0387,3.3530),(8.7471,2.4783),(10.496,1.8952),(11.371,1.6036),(12.246,2.4783),
   (13.703,5.3940),(14.578,6.8519),(15.453,6.5603),(14.578,4.5193),(12.537,-1.020),
   (11.079,-1.020),(9.0387,-0.437),(9.0387,-2.186),(9.0387,-3.936),(11.371,-3.353),
   (13.412,-3.353),(14.287,-3.936),(14.578,-5.102),(13.120,-7.143),(10.496,-9.184),
   (8.4555,-10.93),(7.8724,-11.51),(10.205,-10.93),(11.954,-10.35),(11.662,-11.80),
   (9.9134,-12.68),(6.9977,-13.84),(4.3735,-14.43),(3.4988,-14.14),(3.4988,-13.84),
   (4.3735,-12.68),(5.2482,-12.10),(2.0410,-13.26),(0.5831,-13.84),(-1.749,-13.26),
   (-4.665,-11.22),(-3.207,-12.97),(-3.207,-14.14),(-4.082,-14.14),(-6.123,-13.55),
   (-9.038,-12.10),(-11.07,-11.22),(-10.78,-10.35),(-11.07,-9.767),(-8.747,-10.35),
   (-7.289,-10.64),(-9.913,-8.892),(-11.95,-7.435),(-13.70,-4.519),(-13.70,-3.061),
   (-12.53,-2.769),(-9.038,-3.061),(-8.455,-3.061),(-7.872,-0.437),(-10.78,-0.728),
   (-11.95,-0.437),(-12.82,2.1867),(-13.70,5.9772),(-13.99,6.8519),(-13.12,7.4350),
   (-11.95,4.2277),(-11.07,2.1867),(-10.20,1.8952),(-8.455,2.4783),(-7.872,2.7699),
   (-8.164,4.8109),(-7.872,7.1435),(-6.997,8.3097),(-6.997,9.7676),(-6.123,10.933),
   (-4.665,11.225),(-4.082,10.933),(-3.207,12.100),(-0.874,13.266),(0.5831,13.266)]
    |> filled lightGreen
  , circle 2 |> filled white |> move (-5.2, 10)
  , circle 2 |> filled white |> move (5.2, 10)
  , circle 1 |> filled black |> move (-5.2, 10)
  , circle 1 |> filled black |> move (5.2, 10)
  , polygon [(14.287,7.1435),(14.287,6.8519),(13.995,8.0182),(11.954,8.8929),
    (11.662,9.4760),(11.954,10.059),(12.537,10.059),(13.120,9.4760),(13.412,8.8929),
    (13.995,8.8929),(13.995,9.7676),(13.995,10.642),(13.995,11.808),(13.412,12.391),
    (13.412,12.974),(13.995,13.266),(14.870,13.266),(14.870,12.683),(14.870,11.808),
    (14.578,11.225),(15.161,10.642),(15.453,10.350),(16.328,10.642),(17.202,11.517),
    (17.785,12.391),(17.785,12.974),(18.369,13.558),(18.952,13.558),(19.243,13.266),
    (19.243,12.100),(18.660,11.517),(17.785,10.933),(17.202,10.642),(16.911,9.4760),
    (16.911,9.1845),(17.785,9.1845),(18.952,9.1845),(19.535,9.1845),(19.826,9.7676),
    (20.410,9.7676),(20.701,9.4760),(20.701,8.6013),(20.118,8.0182),(19.535,8.0182),
    (17.785,8.0182),(16.619,7.7266),(15.744,6.8519),(15.453,6.2687),(14.287,7.1435)]
    |> filled orange 
  , polygon [(-12.8,7.4472),(-12.8,8.3781),(-11.40,9.3090),(-10.47,9.3090),(-10.24,10.24),
  (-10.70,10.472),(-11.40,10.472),(-11.86,10.007),(-12.56,9.7745),(-12.8,10.472),(-12.56,11.636),
  (-12.10,12.8),(-11.86,13.032),(-11.86,13.730),(-12.33,13.963),(-12.8,13.730),(-13.03,13.032),
  (-13.03,12.334),(-13.49,11.636),(-14.19,10.938),(-15.12,11.403),(-16.05,13.032),(-16.29,13.730),
  (-16.52,14.429),(-17.22,14.661),(-17.68,14.196),(-17.68,13.498),(-16.98,13.032),(-16.29,12.101),
  (-15.59,10.705),(-15.82,9.7745),(-16.75,10.007),(-18.38,10.24),(-18.61,10.705),(-19.31,10.938),
  (-19.78,10.24),(-19.31,9.5418),(-18.61,9.5418),(-17.68,9.5418),(-16.98,9.0763),(-15.12,8.1454),
  (-14.19,7.4472),(-12.8,7.4472)] |> filled orange
  
  , polygon [(11.636,-10.47),(13.730,-8.610),(15.127,-6.283),(15.592,-5.12),(16.290,-4.887),
  (16.523,-4.887),(16.756,-5.352),(16.523,-6.283),(16.058,-6.516),(15.592,-7.447),(15.127,-8.610),
  (15.127,-9.076),(17.221,-8.843),(18.618,-8.610),(19.083,-8.610),(19.316,-8.145),(20.014,-7.912),
  (20.48,-8.145),(20.48,-9.076),(20.014,-9.309),(19.316,-9.774),(18.850,-9.774),(18.385,-9.774),
  (17.687,-9.774),(16.523,-10.24),(15.825,-11.17),(16.058,-12.10),(16.989,-12.8),(17.92,-13.03),
  (18.385,-12.8),(19.083,-13.03),(19.083,-13.96),(18.385,-14.42),(17.687,-13.96),(17.221,-13.73),
  (16.058,-13.49),(14.661,-12.8),(14.429,-13.03),(14.661,-14.66),(15.127,-15.36),(14.661,-15.82),
  (13.963,-15.82),(13.498,-15.59),(13.265,-14.42),(13.032,-13.49),(12.567,-12.56),(11.869,-12.10),
  (11.636,-10.47)] |> filled orange
  
  , polygon [(-10.93,-10.24),(-13.03,-8.378),(-14.42,-5.818),(-14.89,-4.887),(-15.36,-4.421),
  (-16.29,-4.887),(-16.29,-5.585),(-15.12,-6.516),(-14.89,-7.68),(-14.66,-8.145),(-14.89,-8.378),
  (-17.22,-8.378),(-18.38,-8.145),(-19.08,-7.447),(-19.54,-7.447),(-20.01,-8.145),(-19.78,-8.843),
  (-19.31,-8.843),(-18.15,-8.843),(-16.98,-9.309),(-15.59,-10.00),(-15.12,-10.70),(-16.52,-12.10),
  (-17.22,-12.33),(-17.92,-12.33),(-18.15,-12.8),(-17.92,-13.73),(-16.98,-13.49),(-16.05,-13.03),
  (-15.59,-12.8),(-14.66,-12.56),(-14.42,-12.56),(-14.19,-12.33),(-13.73,-12.56),(-13.73,-13.49),
  (-14.19,-13.96),(-13.96,-14.89),(-13.26,-15.12),(-12.8,-14.89),(-12.56,-14.66),(-12.56,-13.96),
  (-12.56,-13.03),(-11.86,-11.86),(-10.70,-11.40),(-10.93,-10.24)] |> filled orange
   



 ]
  |> scale 3.4
  |> rotate (degrees angle)
  |> move (0,0)



view : Model -> Collage Msg
view model =
    collage 500
        500
  <| case model.page of 
       Welcome -> [ group [ rect 300 90 |> filled yellow 
                          , "Start Game" |> text |> size 50 |> filled black |> move (-110, -16)] |> notifyTap ChangePageState 
                   ,"Frog Game" |> text |> size 60 |> filled darkBlue |> move (-140, 150)                      
                                         ]
       Game ->  [ group (myShapes model)
                , (group <| model.shape { time = model.time }) --display shape
                    |> clip (square 100 |> ghost)
                    |> scale 1
                    |> move (0,-190)
                , (if List.isEmpty model.matched -- change it to score instead of checking list
                    then text (" Instructions: click on the letters to build the word" ++ "               Score: " ++ String.fromInt model.score)
                    else text (" You already have eaten: " ++ (String.concat <| List.map String.fromChar model.matched)
                                 ++ "                   Score: " ++ String.fromInt model.score)
                  )
                  |> size 10 |> centered |> filled blue |> move(0,230)
                , group [ roundedRect 60 30 5 |> filled yellow
                        , text "Pass" |> centered |> filled black
                                    ] |> scale 1.6 |> move (-130, -170) |> notifyTap GetNewPicture
                , case model.guessedOneWrong  of
                    True -> circle 10 |> filled red |> move (-120,180) |> scale 1.0
                    False -> text "" |>filled white
                , case model.nextMatch == '#'  of
                      True ->  rect 1000 1000 |> filled lightBlue |> makeTransparent 0 |> notifyEnter GetNewPicture
                      False -> group []
                , if model.timer - model.time > 0 then
                    group [ roundedRect 60 20 5 |> filled yellow
                          , text ("Timer: " ++ (String.left 2 (String.fromFloat (model.timer - model.time))))|> centered |> filled black |> move (0, -5)
                          ] |> scale 1.6 |> move (200, 200)
                  else
                    rect 1000 1000 |> filled lightBlue |> makeTransparent 0 |> notifyEnter ChangePageState
              , text ("matched "++Debug.toString model.matched++"nextMatch "++Debug.toString model.nextMatch++"unmatched "++Debug.toString model.unmatched)
                 |> size 10 |> centered |> filled green |> move (0,30)
                                          ]
       EndPage ->  [ text "Game Completed, Congratulations!!" |> centered |> size 30 |> filled green  |> move (0, 180)
                   , rect 400 300 |> filled grey 
                   , rect 400 300 |> outlined (solid 1) black
                   , text ("Final Score: " ++ (String.fromInt model.score)) |> centered |> size 30 |> filled blue |> move (0, 50) 
                   , text ("You completed " ++ (String.fromInt model.wordCounter) ++ " words") |> centered |> size 30 |> filled blue |> move (0, 0)
                   , group [ rect 200 60 |> filled yellow
                           , text "Home Page" |> centered |> size 40 |> filled black |> move (0, -10)
                           ] |> move (0, -200) |> notifyTap ChangePageState
                   
                                         ]                                  

-- draw a bubble with a letter in it



-- find rotation angle

print_list list textl y = 
  case list of 
    x :: xs ->
      print_list xs (textl ++ [text (Debug.toString x) |> filled black |> move (0, y)]) (y - 10)
    [] ->
      group textl
magnitude (x1, y1) = (x1^2 + y1^2)^(1/2)

dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

findCos (x1, y1) (x2, y2) = ( dotProduct (x1, y1) (x2, y2) ) / ( magnitude (x1, y1) * magnitude (x2, y2) )

findAngle x = (Basics.acos x) * (180 / pi) 

rotatePos (x1, y1) angle = (x1 * cos angle - y1 * sin angle, x1 * sin angle + y1 * cos angle)

alphabet_pt1 = List.map2 (\n x  -> (Char.fromCode (n + 97), x)) (List.range 0 12) alphabetPos1
alphabet_pt2 = List.map2 (\n x -> (Char.fromCode (n + 97), x)) (List.range 13 25) alphabetPos2
alphabet_posy = List.range 0 12

alphabetPos1 = make_pos 200 alphabet_posy []
alphabetPos2 = make_pos -200 alphabet_posy []


make_pos xPos list1 new_list = 
  case list1 of 
    x :: xs ->
      make_pos xPos xs (new_list ++ [(xPos, (toFloat (150 - 22 * x)))]) 
    [] ->
      new_list
      
      
-- command to ask for a lower-case letter
circle_list list circList =
  case list of 
    x :: xs ->
      let 
        character = (Tuple.first x)
        xPos = Tuple.first (Tuple.second x)
        yPos = Tuple.second (Tuple.second x)
      in
        circle_list
        xs 
        (circList ++ [(group [sampleLetter character]
                        |> move (xPos, yPos ) 
                        |> notifyTap (Tongue (xPos, yPos) character )) ]) 

    [] ->
      group circList 

sampleLetter lett = group [ circle 10 |> filled white 
                           , text (String.fromChar lett) 
                                 |> filled black  
                                 |> scale 1.2 
                                 |> move (-3, -3)
                          ]
