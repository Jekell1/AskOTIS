      * COPYMEMBER: LIBLP/LASER.CMD
      ******************************************************************
      *
      * IDENTIFICATION: LIBLP/LASER.CMD
      *
      * DESCRIPTION   : ESCAPE CODES FOR LASER PRINTING (OKIDATA)
      *
      *                 ESCAPE CODES FOR MODES OF: PCL and HP-GL/2.
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 2002.0930 {PR#00000} PARADATA <A15> [JIM ROSENAUER]
      *               ***NEW***
      *
      * KEC 2005.0802 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *               MODIFIED '03  LASER-FORM-FILE.' TO USE 'PCL###'
      *               INSTEAD OF 'PRT###' AND NOT ATTACHED '.PCL'
      *               AT THE END OF THE FILE PATH STRUCTURE.
      *
      * KEC 2006.0223 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *               ADDED "LASER-PCL-ORIENTATION-CODE."
      *
      * KEC 2006.0302 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *               ADDED THE FOLLOWING:
      *                  01  LASER-PCL-DEGREES-CODE.
      *                  01  LASER-TAG-DEGREES  PIC 9(03).
      *                  01  LASER-TAG-JUSTIFY  PIC X(01).
      *                  01  LASER-TAG-PAGE  PIC 9(02).
      *                  01  LASER-TAG-TEXT  PIC X(80).
      *
      * KEC 2007.0910 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *               ADDED  01  LASER-PCL-PAGE-COPIES-CODE.
      *
      * KEC 2007.1212 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *               ADDED  01  LASER-PCL-LINE-TERMINATE-CODE.
      *
      * KEC 2013.0626 {PR#04289} REGENCY <A15> [LORI MCBEE]
      *               ADDED FOR DIFFERENT UNDERLINES
      *               - ON/OFF/FIXED & FLOAT (SINGLE & DOUBLE)(
      *               ***NOTE*** DOUBLE APPEARS NOT TO WORK
      *               ***NOTE*** FLOAT  APPEARS NOT TO WORK
      *
      * KEC 2014.0804 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               Added LASER-EJECT-PAGE and LASER-PRINTER-RESET.
      *
      ******************************************************************
      *
      *    L A S E R - M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/HEX_CMD.CPY".

      *----------------------------------------------------{ 78 LEVELS }
       78  LASER-DOTS-X-MAX                          VALUE 8140.
       78  LASER-DOTS-Y-MAX                          VALUE 10160.
       78  LASER-DOTS-PER-INCH                       VALUE 600.
       78  LASER-JUSTIFY-CENTER                      VALUE "C".
       78  LASER-JUSTIFY-LEFT                        VALUE "L".
       78  LASER-JUSTIFY-RIGHT                       VALUE "R".
       78  LASER-PEN-WIDTH-DEFAULT                   VALUE 0.03.
       78  LASER-PLU-X-MAX                           VALUE 8140.
       78  LASER-PLU-Y-MAX                           VALUE 10160.
       78  LASER-PLU-PER-INCH                        VALUE 1016.

      *----------------------------------------------------{ 77 LEVELS }
       77  LASER-BEG-INCHES-X          PIC 9(02)V99  VALUE ZEROES.
       77  LASER-BEG-INCHES-Y          PIC 9(02)V99  VALUE ZEROES.
       77  LASER-END-INCHES-X          PIC 9(02)V99  VALUE ZEROES.
       77  LASER-END-INCHES-Y          PIC 9(02)V99  VALUE ZEROES.

       77  LASER-DOTS-X                PIC 9(05)     VALUE ZEROES.
       77  LASER-DOTS-Y                PIC 9(05)     VALUE ZEROES.
       77  LASER-INCHES-X              PIC 9(02)V99  VALUE ZEROES.
       77  LASER-INCHES-Y              PIC 9(02)V99  VALUE ZEROES.
       77  LASER-PCL-TEXT              PIC X(250)    VALUE SPACES.
       77  LASER-PLU-X                 PIC 9(05)     VALUE ZEROES.
       77  LASER-PLU-Y                 PIC 9(05)     VALUE ZEROES.

       77  LASER-PEN-WIDTH             PIC 9V9(2)    VALUE ZEROES.

       77  LASER-ESCAPE-ATTACHMENT     PIC X(80)     VALUE SPACES.
       77  LASER-ESCAPE-SEQUENCE       PIC X(200)    VALUE SPACES.
       77  LASER-SUB                   PIC 9(02)     VALUE ZEROES.

      *-----------------------------------------------------------------
       01  LASER-PRINTER-RESET.
           03  FILLER                  PIC X(01)     VALUE X"1B".
           03  FILLER                  PIC X(01)     VALUE X"45".

      *#################################################################
      *#################################################################
      *#################################################################
      *
      *    L A S E R - F O R M - W O R K E R S
      *
      *#################################################################
      *#################################################################
      *#################################################################
       77  LASER-FORM-PAGE-MAX             PIC 9(02) VALUE 01.

       77  LASER-FORM-STAT                 PIC X(02) VALUE "00".
           88  LASER-FORM-STAT-GOOD                VALUE "00".
           88  LASER-FORM-STAT-BAD                 VALUE "  " THRU "//"
                                                         "01" THRU "~~".

      *-----------------------------------------------------------------
       01  LASER-FORM-CAT-COMMAND.

           03  FILLER                      PIC X(04) VALUE "cat ".

           03  LASER-FORM-FILE.
               05  FILLER                  PIC X(01) VALUE "/".
               05  LASER-FORM-RELEASE      PIC X(07) VALUE "usr/A##".
               05  FILLER                  PIC X(05) VALUE "/PCL/".
               05  LASER-FORM-PROGRAM      PIC X(06) VALUE "PCL###".
               05  FILLER                  PIC X(01) VALUE ".".
               05  LASER-FORM-PAGE-NO      PIC 9(02) VALUE ZEROES.
      *KC----> 05  FILLER                  PIC X(04) VALUE ".PCL".

      *-----------------------------------------------------------------
       01  LASER-TAG-DEGREES           PIC 9(03)     VALUE 90.
       01  LASER-TAG-JUSTIFY           PIC X(01)     VALUE
                                                     LASER-JUSTIFY-LEFT.
       01  LASER-TAG-PAGE              PIC 9(02)     VALUE ZEROES.
       01  LASER-TAG-TEXT              PIC X(80)     VALUE SPACES.

      *#################################################################
      *#################################################################
      *#################################################################
      *
      *    L A S E R - H P G L - W O R K E R S
      *
      *#################################################################
      *#################################################################
      *#################################################################

      *--------------------------------------------------------{ ec%#B }
       01  LASER-HPGL-MODE-CODE.

           03  FILLER   PIC X(1)   VALUE HEX-ESCAPE.
           03  FILLER   PIC X(3)   VALUE "%0B".

      *----------------------------------------------------------{ ecE }
       01  LASER-HPGL-PAGE-BREAK-CODE.

           03  FILLER   PIC X(1)   VALUE HEX-ESCAPE.
           03  FILLER   PIC X(1)   VALUE HEX-UPPER-E.

      *-----------------------------------------------------------------
       01  LASER-HPGL-DRAW-BOX-CODE.

           03  FILLER   PIC X(4)   VALUE "SP1;".
           03  FILLER   PIC X(2)   VALUE "PW".

           03  LASER-HPGL-DRAW-BOX-PW-NO
                        PIC 9.9(2) VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE ";".
           03  FILLER   PIC X(2)   VALUE "PU".

           03  LASER-HPGL-DRAW-BOX-PU-X
                        PIC 9(5)   VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE ",".

           03  LASER-HPGL-DRAW-BOX-PU-Y
                        PIC 9(5)   VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE ";".
           03  FILLER   PIC X(2)   VALUE "EA".

           03  LASER-HPGL-DRAW-BOX-EA-X
                        PIC 9(5)   VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE ",".

           03  LASER-HPGL-DRAW-BOX-EA-Y
                        PIC 9(5)   VALUE ZEROES.

      *-----------------------------------------------------------------
       01  LASER-HPGL-DRAW-LINE-CODE.

           03  FILLER   PIC X(4)   VALUE "SP1;".
           03  FILLER   PIC X(2)   VALUE "PW".

           03  LASER-HPGL-DRAW-LINE-PW-NO
                        PIC 9.9(2) VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE ";".
           03  FILLER   PIC X(2)   VALUE "PU".

           03  LASER-HPGL-DRAW-LINE-PU-X
                        PIC 9(5)   VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE ",".

           03  LASER-HPGL-DRAW-LINE-PU-Y
                        PIC 9(5)   VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE ";".
           03  FILLER   PIC X(2)   VALUE "PD".

           03  LASER-HPGL-DRAW-LINE-PD-X
                        PIC 9(5)   VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE ",".

           03  LASER-HPGL-DRAW-LINE-PD-Y
                        PIC 9(5)   VALUE ZEROES.

      *-----------------------------------------------------------------
       01  LASER-HPGL-FILL-BOX-CODE.

           03  FILLER   PIC X(4)   VALUE "SP1;".
           03  FILLER   PIC X(2)   VALUE "PU".

           03  LASER-HPGL-FILL-BOX-PU-X
                        PIC 9(5)   VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE ",".

           03  LASER-HPGL-FILL-BOX-PU-Y
                        PIC 9(5)   VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE ";".
           03  FILLER   PIC X(2)   VALUE "RA".

           03  LASER-HPGL-FILL-BOX-RA-X
                        PIC 9(5)   VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE ",".

           03  LASER-HPGL-FILL-BOX-RA-Y
                        PIC 9(5)   VALUE ZEROES.

      *#################################################################
      *#################################################################
      *#################################################################
      *
      *    L A S E R - P C L - W O R K E R S
      *
      *#################################################################
      *#################################################################
      *#################################################################

      *-----------------------------------------------------{ ec&a###P }
       01  LASER-PCL-DEGREES-CODE.

           03  FILLER   PIC X(1)   VALUE HEX-ESCAPE.
           03  FILLER   PIC X(2)   VALUE "&a".

           03  LASER-PCL-DEGREES
                        PIC 9(3)   VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE "P".

      *--------------------------------------------------------{ ec%#A }
       01  LASER-PCL-MODE-CODE.

           03  FILLER   PIC X(1)   VALUE HEX-ESCAPE.
           03  FILLER   PIC X(3)   VALUE "%0A".

      *----------------------------------------------------------{ ecE }
       01  LASER-PCL-PAGE-BREAK-CODE.

           03  FILLER   PIC X(1)   VALUE HEX-EC.
           03  FILLER   PIC X(1)   VALUE HEX-UPPER-E.

       01  LASER-EJECT-PAGE.

           03  FILLER   PIC X(1)   VALUE HEX-EC.
           03  FILLER   PIC X(1)   VALUE HEX-UPPER-E.


      *--------------------------------------------------------{ ec%#A }
       01  LASER-PCL-PAGE-COPIES-CODE.

           03  FILLER   PIC X(1)   VALUE HEX-ESCAPE.
           03  FILLER   PIC X(2)   VALUE "&l".

           03  LASER-PCL-PAGE-COPIES
                        PIC 9(2)   VALUE 01.

           03  FILLER   PIC X(1)   VALUE "X".

      *-----------------------------------------------------------------
       01  LASER-PCL-POSITION-CODE.

           03  FILLER   PIC X(1)   VALUE HEX-EC.
           03  FILLER   PIC X(2)   VALUE "*p".
           03  LASER-PCL-POSITION-DOTS-X
                        PIC 9(4)   VALUE ZEROES.
           03  FILLER   PIC X(1)   VALUE "X".
           03  FILLER   PIC X(1)   VALUE HEX-EC.
           03  FILLER   PIC X(2)   VALUE "*p".
           03  LASER-PCL-POSITION-DOTS-Y
                        PIC 9(4)   VALUE ZEROES.
           03  FILLER   PIC X(1)   VALUE "Y".

           03  LASER-PCL-POSITION-TEXT
                        PIC x(250)  VALUE SPACES.

      *-----------------------------------------------------------------
       78  LASER-PCL-ORIENTATION-LANDSCAP     VALUE 1.
       78  LASER-PCL-ORIENTATION-PORTRAIT     VALUE 0.
       01  LASER-PCL-ORIENTATION-CODE.
           03  FILLER   PIC X(1)   VALUE HEX-EC.
           03  FILLER   PIC X(2)   VALUE "&l".
           03  LASER-PCL-ORIENTATION-TYPE
                        PIC 9(1)   VALUE ZEROES.
           03  FILLER   PIC X(1)   VALUE "O".

      ******************************************************************
      *
      *    L A S E R - P C L - A R E A - W O R K E R S
      *
      *=================================================================
      *
      *    HORIZ   = HORIZONITAL LINE
      *    VERT    = VERTICAL    LINE
      *    D-LEFT  = DIAGONAL LINE LEFT 
      *    D-RIGHT = DIAGONAL LINE RIGHT
      *    SQUARE  = SQUARE GRID
      *    GRID    = DIAGONAL GRID
      *
      *=================================================================
       78  LASER-PCL-AREA-FILL-PATTERN                    VALUE 3.
       78  LASER-PCL-AREA-FILL-SHADING                    VALUE 2.

       78  LASER-PCL-AREA-PATTERN-HORIZ                   VALUE 01.
       78  LASER-PCL-AREA-PATTERN-VERT                    VALUE 02.
       78  LASER-PCL-AREA-PATTERN-D-LEFT                  VALUE 03.
       78  LASER-PCL-AREA-PATTERN-D-RIGHT                 VALUE 04.
       78  LASER-PCL-AREA-PATTERN-SQUARE                  VALUE 05.
       78  LASER-PCL-AREA-PATTERN-GRID                    VALUE 06.

       77  LASER-PCL-AREA-PATTERN              PIC 9(2)   VALUE ZEROES.
       77  LASER-PCL-AREA-SHADING              PIC 9(2)   VALUE ZEROES.

      *-----------------------------------------------------------------
       01  LASER-PCL-AREA-CODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEG 'X'  }
           03  FILLER   PIC X(1)   VALUE HEX-ESCAPE.
           03  FILLER   PIC X(2)   VALUE "*p".

           03  LASER-PCL-AREA-BEG-DOTS-X
                        PIC 9(4)   VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE "X".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ BEG 'Y'  }
           03  FILLER   PIC X(1)   VALUE HEX-ESCAPE.
           03  FILLER   PIC X(2)   VALUE "*p".

           03  LASER-PCL-AREA-BEG-DOTS-Y
                        PIC 9(4)   VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE "Y".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ END 'X'  }
           03  FILLER   PIC X(1)   VALUE HEX-ESCAPE.
           03  FILLER   PIC X(2)   VALUE "*c".

           03  LASER-PCL-AREA-END-DOTS-X
                        PIC 9(4)   VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE "A".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ END 'Y'  }
           03  FILLER   PIC X(1)   VALUE HEX-ESCAPE.
           03  FILLER   PIC X(2)   VALUE "*c".

           03  LASER-PCL-AREA-END-DOTS-Y
                        PIC 9(4)   VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE "B".

      *- - - - - - - - - - - - - - - - - - - - -{ SELECT SHADE/PATTERN }
           03  FILLER   PIC X(1)   VALUE HEX-ESCAPE.
           03  FILLER   PIC X(2)   VALUE "*c".

           03  LASER-PCL-AREA-FILL-NO
                        PIC 9(2)   VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE "G".

      *- - - - - - - - - - - - - - - - - - - - - -{ FILL SHADE/PATTERN }
           03  FILLER   PIC X(1)   VALUE HEX-ESCAPE.
           03  FILLER   PIC X(2)   VALUE "*c".

           03  LASER-PCL-AREA-FILL-TYPE
                        PIC 9(1)   VALUE ZEROES.

           03  FILLER   PIC X(1)   VALUE "P".

      ******************************************************************
      *
      *    L A S E R - P C L - F O N T - W O R K E R S
      *
      ******************************************************************

      *-----------------------------------------------------{ 78 - SET }
       78  LASER-PCL-FONT-SET-BAR-CODES                   VALUE 01.
       78  LASER-PCL-FONT-SET-LINE-PRINTR                 VALUE 02.
       78  LASER-PCL-FONT-SET-OCR-A                       VALUE 03.
       78  LASER-PCL-FONT-SET-OCR-B                       VALUE 04.
       78  LASER-PCL-FONT-SET-PC-8                        VALUE 05.
       78  LASER-PCL-FONT-SET-ROMAN-8                     VALUE 06.
       78  LASER-PCL-FONT-SET-SYMBOL                      VALUE 07.
       78  LASER-PCL-FONT-SET-WINGDINGS                   VALUE 08.

       78  LASER-PCL-FONT-SET-MAX                         VALUE 08.

      *----------------------------------------------------{ 78 - TYPE }
       78  LASER-PCL-ALBERTUS-EXTRA-BOLD                  VALUE 01.
       78  LASER-PCL-ALBERTUS-MEDIUM                      VALUE 02.
       78  LASER-PCL-ANTIQUE-OLIVE                        VALUE 03.
       78  LASER-PCL-ANTIQUE-OLIVE-BOLD                   VALUE 04.
       78  LASER-PCL-ANTIQUE-OLIVE-ITALIC                 VALUE 05.
       78  LASER-PCL-ARIAL                                VALUE 06.
       78  LASER-PCL-ARIAL-BOLD                           VALUE 07.
       78  LASER-PCL-ARIAL-BOLD-ITALIC                    VALUE 08.
       78  LASER-PCL-ARIAL-ITALIC                         VALUE 09.
       78  LASER-PCL-BAR-CODES                            VALUE 10.
       78  LASER-PCL-CG-OMEGA                             VALUE 11.
       78  LASER-PCL-CG-OMEGA-BOLD                        VALUE 12.
       78  LASER-PCL-CG-OMEGA-BOLD-ITALIC                 VALUE 13.
       78  LASER-PCL-CG-OMEGA-ITALIC                      VALUE 14.
       78  LASER-PCL-CG-TIMES                             VALUE 15.
       78  LASER-PCL-CG-TIMES-BOLD                        VALUE 16.
       78  LASER-PCL-CG-TIMES-BOLD-ITALIC                 VALUE 17.
       78  LASER-PCL-CG-TIMES-ITALIC                      VALUE 18.
       78  LASER-PCL-CLARENDON-CONDENSED                  VALUE 19.
       78  LASER-PCL-CORONET                              VALUE 20.
       78  LASER-PCL-COURIER                              VALUE 21.
       78  LASER-PCL-COURIER-BOLD                         VALUE 22.
       78  LASER-PCL-COURIER-BOLD-ITALIC                  VALUE 23.
       78  LASER-PCL-COURIER-ITALIC                       VALUE 24.
       78  LASER-PCL-GARAMOND-ANTIQUA                     VALUE 25.
       78  LASER-PCL-GARAMOND-HALBFETT                    VALUE 26.
       78  LASER-PCL-GARAMOND-KURSIV                      VALUE 27.
       78  LASER-PCL-GARAMOND-KURSIV-HALB                 VALUE 28.
       78  LASER-PCL-LETTER-GOTHIC                        VALUE 29.
       78  LASER-PCL-LETTER-GOTHIC-BOLD                   VALUE 30.
       78  LASER-PCL-LETTER-GOTHIC-ITALIC                 VALUE 31.
       78  LASER-PCL-LINE-PRINTER                         VALUE 32.
       78  LASER-PCL-MARIGOLD                             VALUE 33.
       78  LASER-PCL-OCR-A                                VALUE 34.
       78  LASER-PCL-OCR-B                                VALUE 35.
       78  LASER-PCL-SYMBOL                               VALUE 36.
       78  LASER-PCL-TIMES-NEW                            VALUE 37.
       78  LASER-PCL-TIMES-NEW-BOLD                       VALUE 38.
       78  LASER-PCL-TIMES-NEW-BOLD-ITALI                 VALUE 39.
       78  LASER-PCL-TIMES-NEW-ITALIC                     VALUE 40.
       78  LASER-PCL-UNIVERS                              VALUE 41.
       78  LASER-PCL-UNIVERS-BOLD                         VALUE 42.
       78  LASER-PCL-UNIVERS-BOLD-ITALIC                  VALUE 43.
       78  LASER-PCL-UNIVERS-C-MED                        VALUE 44.
       78  LASER-PCL-UNIVERS-C-BLD                        VALUE 45.
       78  LASER-PCL-UNIVERS-C-MED-ITALIC                 VALUE 46.
       78  LASER-PCL-UNIVERS-C-BLD-ITALIC                 VALUE 47.
       78  LASER-PCL-UNIVERS-ITALIC                       VALUE 48.
       78  LASER-PCL-WINGDINGS                            VALUE 49.

       78  LASER-PCL-FONT-TYPE-MAX                        VALUE 49.

      *-----------------------------------------------{ 78 - UNDERLINE }
       78  LASER-PCL-FONT-UNDERLINE-OFF                   VALUE 01.
       78  LASER-PCL-FONT-UNDERLINE-FIXED                 VALUE 02.
       78  LASER-PCL-FONT-UNDERLINE-FX-01                 VALUE 03.
       78  LASER-PCL-FONT-UNDERLINE-FX-02                 VALUE 04.
       78  LASER-PCL-FONT-UNDERLINE-FL-01                 VALUE 05.
       78  LASER-PCL-FONT-UNDERLINE-FL-02                 VALUE 06.

       78  LASER-PCL-FONT-UNDERLINE-MAX                   VALUE 06.

      *-----------------------------------------------------------------
       77  LASER-PCL-FONT-CODE                 PIC X(80)  VALUE SPACES.
       77  LASER-PCL-FONT-SET                  PIC 9(02)  VALUE ZEROES.
       77  LASER-PCL-FONT-SETX                 PIC X(10)  VALUE SPACES.
       77  LASER-PCL-FONT-SIZE                 PIC 99.99  VALUE ZEROES.
       77  LASER-PCL-FONT-TYPE                 PIC 9(02)  VALUE ZEROES.
       77  LASER-PCL-FONT-TYPEX                PIC X(30)  VALUE SPACES.
       77  LASER-PCL-FONT-UNDERLINE            PIC 9(02)  VALUE ZEROES.
       77  LASER-PCL-FONT-UNDERLINEX           PIC X(05)  VALUE SPACES.

      *=====================================================[ FONT SET ]
       01  LASER-PCL-FONT-SET-TABLE.

      *- - - - - - - - - - - - - - - - - - - - - - - -{ BAR-CODES }-{01}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(09) VALUE "(4       ".

      *- - - - - - - - - - - - - - - - - - - - - -{ LINE-PRINTER  }-{02}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(09) VALUE "(66      ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ OCR-A }-{03}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(09) VALUE "(0O      ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ OCR-B }-{04}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(09) VALUE "(1O      ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ PC-8  }-{05}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(09) VALUE "(10U     ".

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ ROMAN-8 }-{06}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(09) VALUE "(8U      ".

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ SYMBOL  }-{07}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(09) VALUE "(19M     ".

      *- - - - - - - - - - - - - - - - - - - - - - - -{ WINGDINGS }-{08}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(09) VALUE "(579L    ".

      *-----------------------------------------------------------------
       01  FILLER REDEFINES LASER-PCL-FONT-SET-TABLE.

           03  LASER-PCL-FONT-SET-ELE PIC X(10)
                                          OCCURS LASER-PCL-FONT-SET-MAX.

      *====================================================[ FONT TYPE ]
       01  LASER-PCL-FONT-TYPE-TABLE.

      *- - - - - - - - - - - - - - - - - -{ ALBERTUS-EXTRA-BOLD   }-{01}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s1b4362T           ".

      *- - - - - - - - - - - - - - - - - -{ ALBERTUS-MEDIUM       }-{02}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s4b4362T           ".

      *- - - - - - - - - - - - - - - - - -{ ANTIQUE-OLIVE         }-{03}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s0b4168T           ".

      *- - - - - - - - - - - - - - - - - -{ ANTIQUE-OLIVE-BOLD    }-{04}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s3b4168T           ".

      *- - - - - - - - - - - - - - - - - -{ ANTIQUE-OLIVE-ITALIC  }-{05}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v1s0b4168T           ".

      *- - - - - - - - - - - - - - - - - -{ ARIAL                 }-{06}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s0b16602T          ".

      *- - - - - - - - - - - - - - - - - -{ ARIAL-BOLD            }-{07}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s3b16602T          ".

      *- - - - - - - - - - - - - - - - - -{ ARIAL-BOLD-ITALIC     }-{08}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v1s3b16602T          ".

      *- - - - - - - - - - - - - - - - - -{ ARIAL-ITALIC          }-{09}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v1s0b16602T          ".

      *- - - - - - - - - - - - - - - - - -{ BAR-CODES             }-{10}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p64.8v0s0b65535T           ".

      *- - - - - - - - - - - - - - - - - -{ CG-OMEGA              }-{11}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s0b4113T           ".

      *- - - - - - - - - - - - - - - - - -{ CG-OMEGA-BOLD         }-{12}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s3b4113T           ".

      *- - - - - - - - - - - - - - - - - -{ CG-OMEGA-BOLD-ITALIC  }-{13}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v1s3b4113T           ".

      *- - - - - - - - - - - - - - - - - -{ CG-OMEGA-ITALIC       }-{14}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v1s0b4113T           ".

      *- - - - - - - - - - - - - - - - - -{ CG-TIMES              }-{15}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s0b4101T           ".

      *- - - - - - - - - - - - - - - - - -{ CG-TIMES-BOLD         }-{16}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s3b4101T           ".

      *- - - - - - - - - - - - - - - - - -{ CG-TIMES-BOLD-ITALIC  }-{17}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v1s3b4101T           ".

      *- - - - - - - - - - - - - - - - - -{ CG-TIMES-ITALIC       }-{18}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v1s0b4101T           ".

      *- - - - - - - - - - - - - - - - - -{ CLARENDON-CONDENSED   }-{19}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v4s3b4140T           ".

      *- - - - - - - - - - - - - - - - - -{ CORONET               }-{20}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v1s0b4116T           ".

      *- - - - - - - - - - - - - - - - - -{ COURIER               }-{21}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s0p#####h0s0b4099T           ".

      *- - - - - - - - - - - - - - - - - -{ COURIER-BOLD          }-{22}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s0p#####h0s3b4099T           ".

      *- - - - - - - - - - - - - - - - - -{ COURIER-BOLD-ITALIC   }-{23}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s0p#####h1s3b4099T           ".

      *- - - - - - - - - - - - - - - - - -{ COURIER-ITALIC        }-{24}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s0p#####h1s0b4099T           ".

      *- - - - - - - - - - - - - - - - - -{ GARAMOND-ANTIQUA      }-{25}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s0b4197T           ".

      *- - - - - - - - - - - - - - - - - -{ GARAMOND-HALBFETT     }-{26}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s3b4197T           ".

      *- - - - - - - - - - - - - - - - - -{ GARAMOND-KURSIV       }-{27}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v1s0b4197T           ".

      *- - - - - - - - - - - - - - - - - -{ GARAMOND-KURSIV-HALB  }-{28}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v1s3b4197T           ".

      *- - - - - - - - - - - - - - - - - -{ LETTER-GOTHIC         }-{29}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s0p#####h0s0b4102T           ".

      *- - - - - - - - - - - - - - - - - -{ LETTER-GOTHIC-BOLD    }-{30}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s0p#####h0s3b4102T           ".

      *- - - - - - - - - - - - - - - - - -{ LETTER-GOTHIC-ITALIC  }-{31}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s0p#####h1s0b4102T           ".

      *- - - - - - - - - - - - - - - - - -{ LINE-PRINTER          }-{32}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s0p16.67h8.5v0s0b0T          ".

      *- - - - - - - - - - - - - - - - - -{ MARIGOLD              }-{33}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s0b4297T           ".

      *- - - - - - - - - - - - - - - - - -{ OCR-A                 }-{34}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s0p10.00h12.0v0s0b104T       ".

      *- - - - - - - - - - - - - - - - - -{ OCR-B                 }-{35}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s0p10.00h12.0v0s0b110T       ".

      *- - - - - - - - - - - - - - - - - -{ SYMBOL                }-{36}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s0b16686T          ".

      *- - - - - - - - - - - - - - - - - -{ TIMES-NEW             }-{37}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s0b16901T          ".

      *- - - - - - - - - - - - - - - - - -{ TIMES-NEW-BOLD        }-{38}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s3b16901T          ".

      *- - - - - - - - - - - - - - - - - -{ TIMES-NEW-BOLD-ITALIC }-{39}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v1s3b16901T          ".

      *- - - - - - - - - - - - - - - - - -{ TIMES-NEW-ITALIC      }-{40}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v1s0b16901T          ".

      *- - - - - - - - - - - - - - - - - -{ UNIVERS               }-{41}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s0b4148T           ".

      *- - - - - - - - - - - - - - - - - -{ UNIVERS-BOLD          }-{42}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s3b4148T           ".

      *- - - - - - - - - - - - - - - - - -{ UNIVERS-BOLD-ITALIC   }-{43}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v1s3b4148T           ".

      *- - - - - - - - - - - - - - - - - -{ UNIVERS-C-MED         }-{44}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v4s0b4148T           ".

      *- - - - - - - - - - - - - - - - - -{ UNIVERS-C-BLD         }-{45}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v4s3b4148T           ".

      *- - - - - - - - - - - - - - - - - -{ UNIVERS-C-MED-ITALIC  }-{46}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v5s0b4148T           ".

      *- - - - - - - - - - - - - - - - - -{ UNIVERS-C-BLD-ITALIC  }-{47}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v5s3b4148T           ".

      *- - - - - - - - - - - - - - - - - -{ UNIVERS-ITALIC        }-{48}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v1s0b4148T           ".

      *- - - - - - - - - - - - - - - - - -{ WINGDINGS             }-{49}

           03  FILLER PIC X(01) VALUE HEX-ESCAPE.
           03  FILLER PIC X(30) VALUE "(s1p#####v0s0b31402T          ".

      *-----------------------------------------------------------------
       01  FILLER REDEFINES LASER-PCL-FONT-TYPE-TABLE.

           03  LASER-PCL-FONT-TYPE-ELE PIC X(31)
                                         OCCURS LASER-PCL-FONT-TYPE-MAX.

      *===============================================[ FONT UNDERLINE ]
       01  LASER-PCL-FONT-UNDERLINE-TABLE.

      *- - - - - - - - - - - - - - - - - - - - - -{ OFF }- - -{ ec&d@  }

           03  FILLER   PIC X(1)   VALUE HEX-ESCAPE.
           03  FILLER   PIC X(4)   VALUE "&d@".

      *- - - - - - - - - - - - - - - - - - - - -{ FIXED }- - -{ ec&d0D }

           03  FILLER   PIC X(1)   VALUE HEX-ESCAPE.
           03  FILLER   PIC X(4)   VALUE "&d0D".

      *- - - - - - - - - - - - - - - -{ FIXED - SINGLE  }- - -{ ec&d1D }

           03  FILLER   PIC X(1)   VALUE HEX-ESCAPE.
           03  FILLER   PIC X(4)   VALUE "&d1D".

      *- - - - - - - - - - - - - - - -{ FIXED - DOUBLE  }- - -{ ec&d2D }

           03  FILLER   PIC X(1)   VALUE HEX-ESCAPE.
           03  FILLER   PIC X(4)   VALUE "&d2D".

      *- - - - - - - - - - - - - - - -{ FLOAT - SINGLE  }- - -{ ec&d3D }

           03  FILLER   PIC X(1)   VALUE HEX-ESCAPE.
           03  FILLER   PIC X(4)   VALUE "&d3D".

      *- - - - - - - - - - - - - - - -{ FLOAT - DOUBLE  }- - -{ ec&d4D }

           03  FILLER   PIC X(1)   VALUE HEX-ESCAPE.
           03  FILLER   PIC X(4)   VALUE "&d4D".

      *-----------------------------------------------------------------
       01  FILLER              REDEFINES LASER-PCL-FONT-UNDERLINE-TABLE.
           03  LASER-PCL-FONT-UNDERLINE-ELE
                        PIC X(05) OCCURS LASER-PCL-FONT-UNDERLINE-MAX.

      ******************************************************************
      *
      *    L A S E R - P C L - L I N E - S P A C I N G - W O R K E R S
      *
      ******************************************************************
       01  LASER-PCL-LINE-SPACING-CODE.

           03  FILLER                  PIC X(01)     VALUE HEX-ESCAPE.
           03  FILLER                  PIC X(02)     VALUE "&l".

           03  LASER-PCL-LINE-SPACING-SIZE
                                       PIC 9(02)     VALUE 10.

           03  FILLER                  PIC X(01)     VALUE "D".

      *-----------------------------------------------------------------
      *    0 - CR=CR; LF=LF; FF=FF
      *    1 - CR=CR+LF; LF=LF; FF=FF
      *    2 - CR=CR; LF=CR+LF; FF=CR+FF
      *    3 - CR=CR+LF; LF=CR+LF; FF=CR+FF
      *-----------------------------------------------------------------
       01  LASER-PCL-LINE-TERMINATE-CODE.
           03  FILLER                  PIC X(01)     VALUE HEX-ESCAPE.
           03  FILLER                  PIC X(02)     VALUE "&k".
           03  LASER-PCL-LINE-TERMINATE-TYPE
                                       PIC 9(01)     VALUE 2.
           03  FILLER                  PIC X(01)     VALUE "G".
