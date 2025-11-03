       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GRTOTL.
       DATE-WRITTEN.    032696.
      ******************************************************************
      *
      *    GROUP-TOTAL-TABLE --- ENTER-PARAMETER SECTION
      *
      *-----------------------------------------------------------------
      *
      *   DESC:  FOR TOTAL-LEVEL-TABLE; ENTERING YES OR NO IF YOU WANT
      *          A TOTALS BREAK DOWN ON A CERTAIN GROUP LEVEL.
      *
      *   IN:  ENT-GROUP
      *        TOTAL-LEVEL-TABLE(MAX)  MAX = 10
      *
      *=================================================================
      * REV:
      * BLM 081110 CHECK FOR GRTOTL-TOT-LEV-TAB(SUB) = "X", IF X, DON'T
      *             INCLUDE THIS LEVEL WHEN ASKING TOTAL LEVEL QUESTION,
      *             NEEDED FOR SP/SUPBON, WORLD PR# 598
      *
      * KEC 2016.0311 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-LV???? TO CD-LV-????
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/GRTOTL S35 06/05/23-09:59:02 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  GRTOTL-LINES                 PIC 99    VALUE 13.
       01  GRTOTL-COLUMNS               PIC 99    VALUE 55.
       01  GRTOTL-BEGIN-Y               PIC 99    VALUE 6.
       01  GRTOTL-BEGIN-X               PIC 99    VALUE 12.
       01  ERRCD           PIC X(1)    VALUE SPACES.
       01  SELECT-STAT     PIC X(2)    VALUE SPACES.

       01  MESSAGE-01      PIC X(47)   VALUE
           "F1 - RESET, ENTER (Y)ES OR (N)O               :".
       01  MESSAGE-02      PIC X(43)   VALUE
           "F1 - RESET, ENTER (Y)ES OR (N)O TO ACCEPT?:".

      *-----------------------------------------------------------------
      *    L E V E L - Q U E S T I O N
      *-----------------------------------------------------------------
       01  LEVEL-QUESTION.
           03  FILLER           PIC X(17) VALUE
               "PRINT TOTALS FOR ".
           03  LEVEL-DESC       PIC X(21).
           03  LEVEL-QUES2      PIC X(7).

      *-----------------------------------------------------------------
       01  SV-TOT-LEV-TABLE.
           03  SV-TOT-LEV-TAB   PIC X(1)  OCCURS 10.
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/HELPLINK.CPY".
       01  GRTOTL-WINDOW-HANDLE         PIC X(10).

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/GRTOTL_DEF.CPY".
       COPY "LIBWI/GRTOTL_WKS.CPY".


      *-----------------------------------------------------------------
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/GRTOTL_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME GRTOTL-BUF EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE GRTOTL-TOT-LEV-TABLE TO SV-TOT-LEV-TABLE.
           MOVE "GRTOTL"             TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE GRTOTL-BEGIN-Y
                   AT COL  GRTOTL-BEGIN-X
                   SIZE    GRTOTL-COLUMNS + VDU-WIN-W-MOD
                   LINES   GRTOTL-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS GRTOTL-WINDOW-HANDLE.

       RESET-DISPLAY.
      * ???? WHY IS THE FORM RESET COMMENTED OUT
      *    PERFORM FORM-RESET.
           MOVE MESSAGE-01 TO MESS.
           PERFORM SEND-LEGEND.
           MOVE SV-TOT-LEV-TABLE TO GRTOTL-TOT-LEV-TABLE.
           PERFORM DISPLAY-LEVELS.

       CHECK-LEV-PRT.

           MOVE 0  TO GTT-TOTAL-SUB.
           MOVE 10 TO GTT-LEVEL-SUB.

       ASK-LEVEL.

           SUBTRACT 1 FROM GTT-LEVEL-SUB.

           IF ( GTT-LEVEL-SUB = 0 )
              GO TO EXIT-ASK-LEVEL.

           IF ( WGR-LEVEL-TABLE(GTT-LEVEL-SUB) NOT = "Y" )
              MOVE SPACES TO GRTOTL-TOT-LEV-TAB(GTT-LEVEL-SUB)
              GO TO ASK-LEVEL.

           IF ( GRTOTL-TOT-LEV-TAB(GTT-LEVEL-SUB) = "X" )
              MOVE SPACES TO GRTOTL-TOT-LEV-TAB(GTT-LEVEL-SUB)
              GO TO ASK-LEVEL.

           ADD 1 TO GTT-TOTAL-SUB.

       ASK-LEVEL-QUEST.

           MOVE "E  A010A00." TO VDU.
           MOVE GTT-TOTAL-SUB TO VDUCOL.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              GO TO RESET-DISPLAY.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ASK-LEVEL-QUEST.

           IF ( VDUBUF NOT = "Y" )
              IF ( VDUBUF NOT = "N" )
                 MOVE "PLEASE ENTER Y OR N" TO MESS
                 PERFORM SEND-MESS
                 GO TO ASK-LEVEL-QUEST.

           MOVE VDUBUF TO GRTOTL-TOT-LEV-TAB(GTT-LEVEL-SUB).
           GO TO ASK-LEVEL.

       EXIT-ASK-LEVEL.

           MOVE MESSAGE-02 TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "S  A010ACCEPT." TO VDU.
           MOVE SPACES           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              GO TO RESET-DISPLAY.

           IF ( VDUSTATUS NOT = "00" )
              GO TO EXIT-ASK-LEVEL.

           IF ( VDUBUF NOT = "Y" )
              IF ( VDUBUF NOT = "N" )
                 GO TO EXIT-ASK-LEVEL.

           IF ( VDUBUF = "N" )
              GO TO CHECK-LEV-PRT.

       END-ROUTINE.
           GO TO END-PROGRAM.

      ******************************************************************
       DISPLAY-LEVELS SECTION.
      ******************************************************************

           PERFORM OPEN-GR1-FILE.
           MOVE GRTOTL-ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *  SEARCH-GR-INIT NOTES WHICH LEVELS ARE PRESENT WITHIN THE
      *  HIERARCHY OF SELECTED GROUP BY PUTTING "Y" IN WGR-LEVEL-TABLE
      *  FOR THAT LEVEL, SO WE MAY ASK WHICH LEVELS SHOULD PRINT

           PERFORM OPEN-CD1-FILE.
           MOVE 0  TO GTT-TOTAL-SUB.
           MOVE 10 TO GTT-LEVEL-SUB.

       DISPLAY-ASK-LEVEL.

           SUBTRACT 1 FROM GTT-LEVEL-SUB.

           IF ( GTT-LEVEL-SUB = 0 )
              GO TO EXIT-DISPLAY-LEVELS.

           IF ( WGR-LEVEL-TABLE(GTT-LEVEL-SUB) NOT = "Y" )
              GO TO DISPLAY-ASK-LEVEL.

           IF ( GRTOTL-TOT-LEV-TAB(GTT-LEVEL-SUB) = "X" )
              GO TO DISPLAY-ASK-LEVEL.

           ADD 1              TO GTT-TOTAL-SUB.
           MOVE "S  A000Q00." TO VDU.
           MOVE GTT-TOTAL-SUB TO VDUCOL.

           MOVE "LV"          TO CD-TYPE.
           MOVE GTT-LEVEL-SUB TO CD-LV-LEVEL.
           MOVE SPACES        TO CD-LV-FILLER.
           PERFORM READ-CD1-FILE.
           IF ( IO-FG = 0 )
              MOVE CD-DESC       TO LEVEL-DESC
           ELSE
              MOVE GTT-LEVEL-SUB TO LEVEL-DESC.

           MOVE SPACES TO LEVEL-QUES2.

           INSPECT LEVEL-QUESTION REPLACING FIRST "  " BY " &".
           INSPECT LEVEL-QUESTION REPLACING FIRST "&     "
                   BY "LEVEL?" AFTER INITIAL " ".

           MOVE LEVEL-QUESTION TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-ASK-LEVEL-QUEST.

           MOVE "S  A010A00." TO VDU.
           MOVE GTT-TOTAL-SUB TO VDUCOL.

           IF ( GRTOTL-ENT-GROUP NOT = GRTOTL-SV-ENT-GROUP )
              MOVE SPACES                        TO VDUBUF
           ELSE
              MOVE SV-TOT-LEV-TAB(GTT-LEVEL-SUB) TO VDUBUF.

           PERFORM SCREENIO.

           GO TO DISPLAY-ASK-LEVEL.

       EXIT-DISPLAY-LEVELS.
           EXIT.

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           DISPLAY GRTOTL-SCREEN UPON GRTOTL-WINDOW-HANDLE.
           MOVE GRTOTL-FORM-FIELDS TO WR-BUF.
           MOVE GRTOTL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE GRTOTL-HELP--FILE TO HELP-LINK-FILE.
           MOVE GRTOTL-HELP--DIR  TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/GRTOTL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/DATER.CPY".

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
       COPY "LIBGB/ACCESS.CPY".
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A470WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GRTOTL-SCREEN.
           DESTROY GRTOTL-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
