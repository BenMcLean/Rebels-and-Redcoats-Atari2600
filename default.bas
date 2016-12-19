/*
Rebels & Redcoats (WIP) by Ben McLean
*/
 set smartbranching on
 set kernel_options player1colors playercolors pfcolors
 rem *  Cost: loss of missile1 and missile0.

 

 const COLOR_SCORE_RED = $4A
 const COLOR_SCORE_BLUE = $8A
 const COLOR_SCORE_GREEN = $CA
 const COLOR_SCORE_YELLOW = $1A
 const COLOR_SCORE_GREY = $0A

 const scorefade = 1
 scorecolor = COLOR_SCORE_BLUE

 const ONE_COPY = 0
 const TWO_COPIES_CLOSE = 1
 const TWO_COPIES_MED = 2
 const THREE_COPIES_CLOSE = 3
 const TWO_COPIES_WIDE = 4
 const DOUBLE_SIZE = 5
 const THREE_COPIES_MED = 6
 const QUAD_SIZE = 7

 const PF_NO_REFLECT = $00
 const PF_REFLECT = $01
 const PF_SCORE = $02
 const PF_PRIORITY = $04
 const BALL_WIDTH_8= $31
 const BALL_WIDTH_4 = $21
 const BALL_WIDTH_2 = $11
 const BALL_WIDTH_1 = $01

 const HERO_X_POS_DEFAULT = 80
 const HERO_Y_POS_DEFAULT = 50
 const ENEMY_X_POS_DEFAULT = 63 : rem 136 is right side
 const ENEMY_Y_POS_DEFAULT = 15
 const ENEMY_Y_POS_HIDE_SCREEN = 255
 const ENEMY_X_POS_RIGHT_SPRITE = 32
 const ENEMY_X_POS_CENTER_SPRITE = 16
 const BULLET_X_POS_DEFAULT = 0
 const BULLET_Y_POS_DEFAULT = 0
 const BULLET_SPEED = 3

 const HALF_SECOND_MASK = $1F
 const NUM_OF_ENEMY_PATTERNS = 7

 const _Edge_Top = 9
 const _Edge_Bottom = 88
 const _Edge_Left = 1
 const _Edge_Right = 153

 const COLOR_DIRT = $f5
 const COLOR_GRASS = $C0
 const COLOR_STONE = $02

 dim enemyPattern = a
 dim frameCounter = b
 dim heroX = c
 dim heroY = d
 dim bulletX = e
 dim bulletY = f
 dim enemyX = g
 dim enemyY = h
 dim soldier = i
 dim facing = j
 def heroNorth=facing{0}
 heroNorth = 1
 def heroEast=facing{1}
 heroEast = 0
 def heroSouth=facing{2}
 heroSouth = 0
 def heroWest=facing{3}
 heroWest = 0
 def bulletNorth=facing{4}
 bulletNorth = 0
 def bulletEast=facing{5}
 bulletEast = 0
 def bulletSouth=facing{6}
 bulletSouth = 0
 def bulletWest=facing{7}
 bulletWest = 0

 dim game_flags = w

 dim mytemp1 = x
 dim mytemp2 = y
 dim mytemp3 = z

 heroX = HERO_X_POS_DEFAULT
 heroY = HERO_Y_POS_DEFAULT
 bulletX = BULLET_X_POS_DEFAULT
 bulletY = BULLET_Y_POS_DEFAULT
 enemyX = ENEMY_X_POS_DEFAULT
 enemyY = ENEMY_Y_POS_DEFAULT 
 enemyPattern = 0

 gosub drawLevel1



main
 
doEnemy
 rem  Update enemy pattern approximately every half second,
 rem  and scroll through enemy pattern sequence continually...
 frameCounter = frameCounter + 1
 temp1 = frameCounter & HALF_SECOND_MASK
 if temp1 = 0 then enemyPattern = enemyPattern + 1
 if enemyPattern > NUM_OF_ENEMY_PATTERNS then enemyPattern = 0
 rem enemyPattern=7

 rem soldier uses 3 bits. each one represents a soldier. 1 for alive, 0 for dead
 rem 0      hide the soldiers offscreen
 rem 1   X  NUSIZ1 = ONE_COPY, player1x = enemyX + ENEMY_X_POS_RIGHT_SPRITE
 rem 2  X   NUSIZ1 = ONE_COPY, player1x = enemyX + ENEMY_X_POS_CENTER_SPRITE
 rem 3  XX  NUSIZ1 = TWO_COPIES_CLOSE, player1x = enemyX + ENEMY_X_POS_CENTER_SPRITE
 rem 4 X    NUSIZ1 = ONE_COPY
 rem 5 X X  NUSIZ1 = TWO_COPIES_MED
 rem 6 XX   NUSIZ1 = TWO_COPIES_CLOSE
 rem 7 XXX  NUSIZ1 = THREE_COPIES_CLOSE

 if enemyPattern = 0 then soldier{0}=0 : soldier{1}=0 : soldier{2}=0 : goto enemyPatternSet
 if enemyPattern = 1 then soldier{0}=0 : soldier{1}=0 : soldier{2}=1 : goto enemyPatternSet
 if enemyPattern = 2 then soldier{0}=0 : soldier{1}=1 : soldier{2}=0 : goto enemyPatternSet
 if enemyPattern = 3 then soldier{0}=0 : soldier{1}=1 : soldier{2}=1 : goto enemyPatternSet
 if enemyPattern = 4 then soldier{0}=1 : soldier{1}=0 : soldier{2}=0 : goto enemyPatternSet
 if enemyPattern = 5 then soldier{0}=1 : soldier{1}=0 : soldier{2}=1 : goto enemyPatternSet
 if enemyPattern = 6 then soldier{0}=1 : soldier{1}=1 : soldier{2}=0 : goto enemyPatternSet
 if enemyPattern = 7 then soldier{0}=1 : soldier{1}=1 : soldier{2}=1
enemyPatternSet

 rem position enemies
 player1y = enemyY
 if soldier{0} then goto leftSoldierFirst
 if soldier{1} then goto centerSoldierFirst
 if !soldier{2} then player1y = ENEMY_Y_POS_HIDE_SCREEN : goto soldiersDone

 player1x = enemyX + ENEMY_X_POS_RIGHT_SPRITE
 NUSIZ1 = ONE_COPY
 goto soldiersDone

centerSoldierFirst
 player1x = enemyX + ENEMY_X_POS_CENTER_SPRITE
 if soldier{2} then NUSIZ1 = TWO_COPIES_CLOSE else NUSIZ1 = ONE_COPY
 goto soldiersDone

leftSoldierFirst
 player1x = enemyX
 if soldier{1} then goto leftAndCenterSoldier
 if soldier{2} then NUSIZ1 = TWO_COPIES_MED else NUSIZ1 = ONE_COPY
 goto soldiersDone

leftAndCenterSoldier
 if soldier{2} then NUSIZ1 = THREE_COPIES_CLOSE else NUSIZ1 = TWO_COPIES_CLOSE

soldiersDone

 rem Process the input 

 if !joy0left then goto afterJoyLeft
 heroWest = 1 : heroEast = 0 : heroNorth = 0 : heroSouth = 0
 if heroX <= _Edge_Left then goto afterJoyLeft
 mytemp1 = (heroX-17)/4
 mytemp2 = (heroY-1)/8
 mytemp3 = (heroY-8)/8
 if !pfread(mytemp1, mytemp2) then if !pfread(mytemp1, mytemp3) then heroX = heroX - 1
afterJoyLeft

 if !joy0right then goto afterJoyRight
 heroEast = 1 : heroWest = 0 : heroNorth = 0 : heroSouth = 0
 if heroX >= _Edge_Right then goto afterJoyRight
 mytemp1 = (heroX-10)/4
 mytemp2 = (heroY-1)/8
 mytemp3 = (heroY-8)/8
 if !pfread(mytemp1, mytemp2) then if !pfread(mytemp1, mytemp3) then heroX = heroX + 1
afterJoyRight

 if !joy0down then goto afterJoyDown
 heroSouth = 1 : heroNorth = 0 : heroEast = 0 : heroWest = 0
 if heroY >= _Edge_Bottom then goto afterJoyDown
 mytemp1 = (heroX-16)/4
 mytemp2 = heroY/8
 mytemp3 = (heroX-11)/4
 if !pfread(mytemp1, mytemp2) then if !pfread(mytemp3, mytemp2) then heroY = heroY + 1
afterJoyDown

 if !joy0up then goto afterJoyUp
 heroNorth = 1 : heroSouth = 0 : heroEast = 0 : heroWest = 0
 if heroY <= _Edge_Top then goto afterJoyUp
 mytemp1 = (heroX-16)/4
 mytemp2 = (heroY-9)/8
 mytemp3 = (heroX-11)/4
 if !pfread(mytemp1, mytemp2) then if !pfread(mytemp3, mytemp2) then heroY = heroY - 1
afterJoyUp

 rem if joy0up && enemyY > _Edge_Top then enemyY = enemyY - 1
 rem if joy0down && enemyY < _Edge_Bottom then enemyY = enemyY + 1
 rem if joy0right && enemyX < _Edge_Right then enemyX = enemyX + 1
 rem if joy0left && enemyX > _Edge_Left then enemyX = enemyX - 1
 
 if !joy0fire then goto afterJoyFire
 if bulletX = 0 && bulletY = 0 then goto gonnaFire
 goto afterJoyFire
gonnaFire
 bulletX = heroX+4 : bulletY = heroY-4
 bulletNorth = heroNorth
 bulletEast = heroEast
 bulletSouth = heroSouth
 bulletWest = heroWest
afterJoyFire

 rem Updating bullet position!
 if bulletX = BULLET_X_POS_DEFAULT && bulletY = BULLET_Y_POS_DEFAULT then goto bulletDone
 if collision(ball, playfield) then goto bulletBad
 if bulletX >= 3 && bulletY >= 3 && bulletX <= _Edge_Right && bulletY <= _Edge_Bottom then goto bulletOK

bulletBad
 bulletX = BULLET_X_POS_DEFAULT
 bulletY = BULLET_Y_POS_DEFAULT
 bulletNorth = 0 : bulletEast = 0
 bulletSouth = 0 : bulletWest = 0
 goto bulletDone
bulletOK
 if bulletNorth then bulletY = bulletY - BULLET_SPEED : goto bulletDone
 if bulletEast then bulletX = bulletX + BULLET_SPEED : goto bulletDone
 if bulletSouth then bulletY = bulletY + BULLET_SPEED : goto bulletDone
 if bulletWest then bulletX = bulletX - BULLET_SPEED
bulletDone

 player0x = heroX
 player0y = heroY

 ballx = bulletX
 bally = bulletY

 gosub drawMinuteman
 gosub drawRedcoat

 COLUBK = COLOR_GRASS
 CTRLPF = BALL_WIDTH_2
 ballheight = 1

 rem if collision(player0, playfield) then scorecolor = COLOR_SCORE_RED

 rem mytemp1 = ((heroX-15)/4)
 rem mytemp2 = ((heroY-1)/8)
 rem if pfread(mytemp1, mytemp2) then scorecolor = COLOR_SCORE_RED

 drawscreen

 goto main



drawMinuteman
 player0color:
 $00
 $00
 $F0
 $00
 $0E
 $0E
 $3E
 $00
end

 player0:
 %00111100
 %00011000
 %00011000
 %01011010
 %01011010
 %00111100
 %00011000
 %00111100
end
 return



drawRedcoat
 player1color:
 $00
 $00
 $42
 $0E
 $42
 $42
 $3E
 $00
end

 player1:
 %00111100
 %00011000
 %00011000
 %01011010
 %01011010
 %00111100
 %00011000
 %00111100
end
 return



drawLevel1
   pfcolors:
   $02
   $02
   $02
   $02
   $02
   $02
   $02
   $02
   $02
   $02
   $02
   $02
end
 playfield:
 ................................
 ................................
 .......XXXX...........XXXX......
 .......XXXX...........XXXX......
 ................................
 ................................
 ................................
 .......XXXX...........XXXX......
 .......XXXX...........XXXX......
 ................................
 ................................
end
 return
