* @ValidationCode : MjotMzMyMjk0MjkwOkNwMTI1MjoxNjQwODA5NjEzNzk3OmdpcmxvdzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMF9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 30 Dec 2021 00:26:53
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : girlow
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R20_AMR.0
PROGRAM GAME.OF.LIFE
*-----------------------------------------------------------------------------
* Game of Life Algorithm Implementation
* User can specify the size of the grid and number of generations
* 1 is a live cell
* 0 is a dead cell
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* 30-Dec-21    Girish Lowtun          Game of Life Algorithm Implementation
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE

    GOSUB INITIALISE

RETURN

*-----------------------------------------------------------------------------
INITIALISE:
*-----------
* Prompt user to input the number of rows, number of columns and
* number of generations to generate.
* If input not numeric, raise error and do not proceed further.
    CRT "Input number of rows: "
    INPUT V.ROW
    CRT "Input number of columns: "
    INPUT V.COLUMN
    CRT "Number of generations: "
    INPUT V.GEN
    CRT " "
    
    IF ISDIGIT(V.ROW) AND ISDIGIT(V.COLUMN) AND ISDIGIT(V.GEN) THEN
        V.INIT.GRID = ""
        V.NEXT.GRID = ""
        GOSUB MAIN.PROCESS
    END ELSE
        CRT "Non-numeric input detected."
    END

RETURN

*-----------------------------------------------------------------------------
MAIN.PROCESS:
*------------
* Initialise first grid and loop to generate the number of generations the user
* has input.
    FOR M = 1 TO V.GEN
        V.INIT.GRID = V.NEXT.GRID
        V.NEXT.GRID = ""
        IF M EQ 1 THEN
            GOSUB INITIALISE.GRID
        END
        GOSUB NEXT.GENERATION
    NEXT M
    
RETURN

*-----------------------------------------------------------------------------
INITIALISE.GRID:
*--------------*
* Create grid as per dimension input.
* Populate grid with 0 or 1, based on modulo of random number between 0-99
* divided by 2.
    FOR X = 1 TO V.ROW
        FOR Y = 1 TO V.COLUMN
            V.INIT.GRID<X,Y> = MOD(RND(100),2)
        NEXT Y
    NEXT X
    
    CRT "Initial State"
    V.DISPLAY.ARRAY = V.INIT.GRID
    GOSUB DISPLAY.GRID
    
RETURN

*-----------------------------------------------------------------------------
NEXT.GENERATION:
*--------------*
* Generate next generation by looping through each position of grid
* and checking the number of live neighbours.
    FOR X = 1 TO V.ROW
        FOR Y = 1 TO V.COLUMN
            V.CURR.ROW = X
            V.CURR.COL = Y
            GOSUB GET.LIVE.NEIGHBOURS
            IF V.INIT.GRID<X,Y> EQ 1 AND (V.LIVE.COUNTER EQ 2 OR V.LIVE.COUNTER EQ 3) THEN
                V.NEXT.GRID<X,Y> = 1
            END ELSE
                IF (V.INIT.GRID<X,Y> EQ 0 AND V.LIVE.COUNTER EQ 3) THEN
                    V.NEXT.GRID<X,Y> = 1
                END ELSE
                    V.NEXT.GRID<X,Y> = 0
                END
            END
        NEXT Y
    NEXT X
    
    CRT "Generation ":M
    V.DISPLAY.ARRAY = V.NEXT.GRID
    GOSUB DISPLAY.GRID
    
RETURN

*-----------------------------------------------------------------------------
GET.LIVE.NEIGHBOURS:
*--------------------
* From current position, check the number of live neighbours.
* 9 positions to consider - N, S, E, W, NE, SE, NW, SW, C
    V.LIVE.COUNTER = 0
    FOR I = V.CURR.ROW - 1 TO V.CURR.ROW + 1
        FOR J = V.CURR.COL - 1 TO V.CURR.COL + 1
;* if self position or out of bounds, break and move to next iteration.
            IF (I EQ V.CURR.ROW AND J EQ V.CURR.COL) OR (I EQ 0 OR J EQ 0) OR (I GT V.ROW OR J GT V.COLUMN) THEN
                CONTINUE
            END
;* if neighbouring cell is live, increment counter.
            IF V.INIT.GRID<I,J> EQ 1 THEN
                V.LIVE.COUNTER = V.LIVE.COUNTER + 1
            END
        NEXT J
    NEXT I
    
RETURN

*-----------------------------------------------------------------------------
DISPLAY.GRID:
*-----------*
* Display grid passed as parameter on screen.
    V.DISPLAY.ARRAY = CHANGE(V.DISPLAY.ARRAY,@VM,"*")
    FOR X = 1 TO V.ROW
        CRT V.DISPLAY.ARRAY<X>
    NEXT X
    CRT " "
    
RETURN

END