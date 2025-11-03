      * COPYMEMBER: LIBFP/FPWSPST
      ******************************************************************
      *
      * IDENTIFICATION:  LIBFP/FPWSPST
      *
      * DESCRIPTION   :  'WS' FOR PST-FILE
      *
      * **** NOTE ****:  THIS ALSO CONTAINS LIBFP/FPWSPST.CMD
      *
      *=================================================================
      * REV:
      * KEC 2011.0923 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ***NEW***
      *
      * KEC 2013.0621 {PR#04271} NEWPORT <A15> [JAY CISZEWSKI]
      *               MOVED PST-WS-FONT-WORKERS TO LIBLP/LPWSPST.CMD
      *
      * KEC 2016.0519 {PR#05076} PARADATA <A15> [JUDY REIFSTECK]
      *               IN DOING PR#05076, I NOTICED A NEED FROM
      *                PREVIOUS PR#.  LOGIC FROM PR#5076 WAS NOT ADDED
      *                TO LPWSPST.CMD.  PST LASER FORMS WAS NEW AT THE
      *                TIME OF THIS PR# WHICH IS WHY IT WAS SKIPPED.
      *                THIS WILL BE REQUIRED IN THE FUTURE.  SO ADDED
      *                PST-WS-TAG-TYPE, PST-WS-TAG-NUMBER AND
      *                PST-WS-TAG-DATE.
      *                
      * KEC 2020-1020 {PR#00000} WORLD [JAY CISZEWSKI]
      *               ADDED PST-TIME-NOW FOR DEVELOPMENT OF CREATING
      *               PDF FILES WITH GHOSTSCRIPT (PS2PDF).
      *
      ******************************************************************
       01  PST-REC-BYTE-SIZE                PIC 9(03) VALUE ZEROES.

      *=================================================================
       01  PST-WS-ELE                       PIC 9(03) VALUE ZEROES.
       01  PST-WS-MAX                       PIC 9(03) VALUE ZEROES.

      *-----------------------------------------------------------------
       01  PST-WS-STAT                      PIC X(02) VALUE SPACES.
           88  PST-WS-STAT-GOOD                 VALUE "00".
           88  PST-WS-STAT-BAD                  VALUE "  " THRU "//"
                                                      "01" THRU "~~".

      *======================================================[ JUSTIFY ]
       78  PST-WS-JUSTIFY-CENTER                     VALUE "C".
       78  PST-WS-JUSTIFY-LEFT                       VALUE "L".
       78  PST-WS-JUSTIFY-RIGHT                      VALUE "R".

      *==========================================================[ TAG ]
       78  PST-WS-TAG-MODE---OFF                     VALUE "OFF".
       78  PST-WS-TAG-MODE---ON                      VALUE "ON ".
       77  PST-WS-TAG-MODE             PIC X(03)     VALUE "ON ".
           88  PST-WS-TAG-MODE-OFF                   VALUE "OFF".
           88  PST-WS-TAG-MODE-ON                    VALUE "ON ".

       78  PST-WS-TAG-TYPE---LN                      VALUE "LN".
       78  PST-WS-TAG-TYPE---OT                      VALUE "OT".
       77  PST-WS-TAG-TYPE             PIC X(02)     VALUE "LN".
           88  PST-WS-TAG-TYPE-LN                    VALUE "LN".
           88  PST-WS-TAG-TYPE-OT                    VALUE "OT".

      *-----------------------------------------------------------------
       01  PST-WS-TAG-DATE             PIC 9(06)     VALUE ZEROES.
       01  PST-WS-TAG-NUMBER           PIC 9(06)     VALUE ZEROES.
       01  PST-WS-TAG-JUSTIFY          PIC X(01)     VALUE
                                                    PST-WS-JUSTIFY-LEFT.
       01  PST-WS-TAG-PAGE             PIC 9(02)     VALUE ZEROES.
       01  PST-WS-TAG-TEXT             PIC X(80)     VALUE SPACES.

      *=========================================================[ TIME ]
       01  PST-TIME-NOW.
           03  PST-TIME-NOW-HH         PIC 99        VALUE ZEROES.
           03  PST-TIME-NOW-MM         PIC 99        VALUE ZEROES.
           03  PST-TIME-NOW-SS         PIC 99        VALUE ZEROES.
           03  PST-TIME-NOW-HD         PIC 99        VALUE ZEROES.

      *=================================================================
       01  PST-START-STAT                   PIC X(02) VALUE SPACES.
           88  PST-START-STAT-GOOD              VALUE "00".
           88  PST-START-STAT-BAD               VALUE "  " THRU "//"
                                                      "01" THRU "~~".

      ******************************************************************
      *
      *    P S T - M E R G E - W O R K E R S
      *
      *=================================================================
      *    DESC:  USED TO MERGE TWO ITEMS INTO ONE ITEM.
      *           USED PRIMARILY FOR THE PST-WRITE-FILE ROUTINE.
      ******************************************************************
       78  PST-MERGE---MAX                             VALUE 500.
       77  PST-MERGE-ELE                    PIC 9(03)  VALUE ZEROES.
       77  PST-MERGE-SUB-1                  PIC 9(03)  VALUE ZEROES.
       77  PST-MERGE-SUB-2                  PIC 9(03)  VALUE ZEROES.
      *-----------------------------------------------------------------
       01  PST-MERGE-SOURCE-1               PIC X(PST-MERGE---MAX).
       01  PST-MERGE-SOURCE-1-ELE REDEFINES PST-MERGE-SOURCE-1
                                            PIC X(01)
                                            OCCURS PST-MERGE---MAX.

       01  PST-MERGE-SOURCE-2               PIC X(PST-MERGE---MAX).
       01  PST-MERGE-SOURCE-2-ELE REDEFINES PST-MERGE-SOURCE-2
                                            PIC X(01)
                                            OCCURS PST-MERGE---MAX.

       01  PST-MERGE-TARGET                 PIC X(PST-MERGE---MAX).
       01  PST-MERGE-TARGET-ELE   REDEFINES PST-MERGE-TARGET
                                            PIC X(01)
                                            OCCURS PST-MERGE---MAX.

      *-----------------------------------------------------------------
       01  PST-MERGE-STAT                   PIC X(02) VALUE SPACES.
           88  PST-MERGE-STAT-GOOD              VALUE "00".
           88  PST-MERGE-STAT-BAD               VALUE "  " THRU "//"
                                                      "01" THRU "~~".

      ******************************************************************
      *
      *    P S T - P A G E - W O R K E R S
      *
      *=================================================================
      *    DESC: USED TO SETTING & TRACKING THE DOTS ON A PAGE.
      *          'DPI' = DOTS PER INCH
      *          IT IS 72 DPI
      *          LEGAL  SIZE (8.5 X 14) IS 612 X 1008 DPI.
      *          LETTER SIZE (8.5 X 11) IS 612 X 792  DPI.
      *
      *=================================================================
      *          'X' IS FOR LEFT-RIGHT POSITIONING.
      *          'Y' IS FOR TOP-BOTTOM POSITIONING.
      *
      *          'CURRENT' IS THE CURRENT POSITION (X/Y) ON PAGE.
      *          'MAX' IS FOR THE CURRENT POSITION (X/Y) ON PAGE.
      ******************************************************************
       77  PST-PAGE-DPI                     PIC 9(04)  VALUE 72.

      *-----------------------------------------------------------------
       77  PST-PAGE-X                       PIC 9(04)-     VALUE ZEROES.
       77  PST-PAGE-X-CALC                  PIC S9(4)V999  VALUE ZEROES.
       77  PST-PAGE-X-CURRENT               PIC 9(04)-     VALUE ZEROES.
       77  PST-PAGE-X-SPACING               PIC 9(04)V999  VALUE ZEROES.
       77  PST-PAGE-X-START-LEGAL           PIC 9(04)      VALUE 12.
       77  PST-PAGE-X-START-LETTER          PIC 9(04)      VALUE 12.

      *-----------------------------------------------------------------
       77  PST-PAGE-Y                       PIC 9(04)-     VALUE ZEROES.
       77  PST-PAGE-Y-CALC                  PIC S9(4)V999  VALUE ZEROES.
       77  PST-PAGE-Y-CURRENT               PIC 9(04)-     VALUE ZEROES.
       77  PST-PAGE-Y-SPACING               PIC 9(04)V999  VALUE ZEROES.
       77  PST-PAGE-Y-START-LEGAL           PIC 9(04)      VALUE 984.
       77  PST-PAGE-Y-START-LETTER          PIC 9(04)      VALUE 768.

      *-----------------------------------------------------------------
       78  PST-PAGE-SIZE---LETTER                     VALUE ZEROES.
       78  PST-PAGE-SIZE---LEGAL                      VALUE 1.
       77  PST-PAGE-SIZE                    PIC 9(01) VALUE ZEROES.
           88  PST-PAGE-SIZE-LETTER                   VALUE ZEROES.
           88  PST-PAGE-SIZE-LEGAL                    VALUE 1.

      ******************************************************************
       COPY "LIBFP/FPWSPST_CMD.CPY".

