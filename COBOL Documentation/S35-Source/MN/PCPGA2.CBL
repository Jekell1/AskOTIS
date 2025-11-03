       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PCPGA2.
       AUTHOR.        KEC.
       DATE-WRITTEN.  06-30-2006.
      ******************************************************************
      *
      * IDENTIFICATION:  MN/PCPGA2.C
      *
      * DESCRIPTION   :  SCRATCH PAD FILE (PC) FILE MAINTENANCE
      *                  --- PC-ID = "A" (AUTO INS.) PAGE 2
      *
      *=================================================================
      * REV:
      * KEC 2006.0622 {PR#00127} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *               ***NEW***
      *
      * MJD 131203 A30 CONVERSION COMPLETED
      *
      * KEC 2017.0324 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED COMP-3 FROM PC1-KEY.
      *  CS 170503 ACTIVATE 8 DIGIT DATES
      *  CS 200130 REMOVE BRANCH FROM KEY ENTRY, ITS LOCAL IN NATURE SO
      *            FORCING EXT-FILPTH-BRNO TO THE BRANCH
      * BAH 20200617 CHANGED PC-LT-RECORD TO PC-LTA-RECORD
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *=================================================================
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/PCPGA2.CBL S34 06/06/21-16:16:01 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01PC.CPY".
       COPY "LIBLP/LP01PC_SQL.CPY".
       COPY "LIBLP/LPWSPC.CPY".
       COPY "LIBLP/LP01WK.CPY".

       01  HOLD-PC1-KEY.
           03  HOLD-PC-ID              PIC X(01).
           03  HOLD-PC-BRANCH          PIC 9(04).
           03  HOLD-PC-SOC-SEC         PIC 9(10).

       01  ORIGINAL-PC-REC         PIC X(PC-SIZE).
       01  CHANGED-PC-REC          PIC X(PC-SIZE).

      ******************************************************************
      *
      *    D I S P L A Y - W O R K E R S
      *
      ******************************************************************
       01  DISPLAY-BUF.
           03  F01           PIC X(25).
           03  F02           PIC X(25).
           03  F03           PIC X(25).
           03  F04           PIC X(25).
           03  F05           PIC X(25).
           03  F06           PIC X(15).
           03  F07           PIC X(2).
           03  F08           PIC X(10).
           03  F09           PIC X(12).
           03  F10           PIC X(25).
           03  F11           PIC X.
           03  F12           PIC X(25).
           03  F13           PIC X.
           03  F14           PIC 99/99/99 BLANK ZERO.
           03  F15           PIC 99/99/99 BLANK ZERO.
           03  F16           PIC 99/99/99 BLANK ZERO.
           03  F17           PIC 99/99/99 BLANK ZERO.
           03  F18           PIC Z,ZZZ,ZZ9.99- BLANK ZERO.
           03  F19           PIC Z,ZZZ,ZZ9.99- BLANK ZERO.
           03  F20           PIC Z,ZZZ,ZZ9.99- BLANK ZERO.
           03  F21           PIC Z,ZZZ,ZZ9.99- BLANK ZERO.
           03  F22           PIC Z,ZZZ,ZZ9.99- BLANK ZERO.
           03  F23           PIC Z,ZZZ,ZZ9.99- BLANK ZERO.
           03  F24           PIC ZZ9 BLANK ZERO.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(48) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(40) VALUE
           "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER      PIC X(16) VALUE
           "210 220 230 240.".

      *=================================================================
       01  DISPLAY-KEY-BUF.
           03  DISPLAY-KEY-ID          PIC X(01)          VALUE SPACES.
      *    03  DISPLAY-KEY-BRANCH      PIC 9(04)          VALUE ZEROES.
           03  DISPLAY-KEY-SSNO        PIC 9(10)          VALUE ZEROES.

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-LIST.
           03  FILLER PIC X(07) VALUE "KEY-ID ".
      *    03  FILLER PIC X(11) VALUE "KEY-BRANCH ".
           03  FILLER PIC X(09) VALUE "KEY-SSNO.".

      ******************************************************************
      *
      *    E L E M E N T - W O R K E R S
      *
      ******************************************************************
       78  MAX                                       VALUE 24.
       77  ONE                         PIC S9(1)     VALUE ZEROES.
       77  ELE                         PIC 9(02)     VALUE ZEROES.
           88  ELE-VALID                             VALUE 01 THRU MAX.

      *=================================================================
       01  ELEMENT-TABLE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{01 THRU 10}
           03  FILLER PIC X(08) VALUE "E  A250 ".
           03  FILLER PIC X(08) VALUE "E  A250 ".
           03  FILLER PIC X(08) VALUE "E  A250 ".
           03  FILLER PIC X(08) VALUE "E  A250 ".
           03  FILLER PIC X(08) VALUE "E  A250 ".
           03  FILLER PIC X(08) VALUE "E  A150 ".
           03  FILLER PIC X(08) VALUE "E  A020 ".
           03  FILLER PIC X(08) VALUE "E  A100 ".
           03  FILLER PIC X(08) VALUE "E  A120 ".
           03  FILLER PIC X(08) VALUE "E  A250 ".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{11 THRU 20}
           03  FILLER PIC X(08) VALUE "E  A250 ".
           03  FILLER PIC X(08) VALUE "E  A250 ".
           03  FILLER PIC X(08) VALUE "E  A010 ".
           03  FILLER PIC X(08) VALUE "ERNM060 ".
           03  FILLER PIC X(08) VALUE "ERNM060 ".
           03  FILLER PIC X(08) VALUE "ERNM060 ".
           03  FILLER PIC X(08) VALUE "ERNM060 ".
           03  FILLER PIC X(08) VALUE "E NS072 ".
           03  FILLER PIC X(08) VALUE "E NS072 ".
           03  FILLER PIC X(08) VALUE "E NS072 ".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{21 THRU 24}
           03  FILLER PIC X(08) VALUE "E NS072 ".
           03  FILLER PIC X(08) VALUE "E NS072 ".
           03  FILLER PIC X(08) VALUE "E NS072 ".
           03  FILLER PIC X(08) VALUE "ENNN030 ".

      *=================================================================
       01  ELEMENT-TABLE-RDF           REDEFINES ELEMENT-TABLE.
           03  ELEMENT-TABLE-OCCURS    OCCURS MAX.
               05  ELEMENT-VDU         PIC X(07).
               05  ELEMENT-LOCK        PIC X(01).

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(74)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-ENTER-ELE.
           03  FILLER PIC X(30) VALUE "F1-RESET, ENTER FIELD INFORMAT".
           03  FILLER PIC X(30) VALUE "ION                           ".
           03  FILLER PIC X(14) VALUE "              ".

       01  LEGEND-LOAD-KEY-ID.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ID 'C".
           03  FILLER PIC X(30) VALUE "' OR 'A'                      ".
           03  FILLER PIC X(14) VALUE "              ".

       01  LEGEND-LOAD-KEY-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER BRANC".
           03  FILLER PIC X(30) VALUE "H NUMBER                      ".
           03  FILLER PIC X(14) VALUE "              ".

       01  LEGEND-LOAD-KEY-SSNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER SOCIA".
           03  FILLER PIC X(30) VALUE "L SECURITY NUMBER             ".
           03  FILLER PIC X(14) VALUE "              ".

       01  LEGEND-MODIFY-ROUTINE.
           03  FILLER PIC X(30) VALUE "                 F1-RESET, ENT".
           03  FILLER PIC X(30) VALUE "ER-SAVE ,P1 , ENTER FIELD NUMB".
           03  FILLER PIC X(14) VALUE "ER TO CHANGE: ".

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBMN/COMMONW.CPY".
       COPY "LIBMN/PCPGRMW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/PCPGA2_DEF.CPY".
       COPY "LIBMN/PCPGA2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/PCPGA2_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

       MAIN-MODULE.
      D    STOP MESS.
           PERFORM LOAD-MODULE.

           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO MAIN-PROGRAM-EXIT.

       MAIN-MODULE-CONTINUE.

           PERFORM ENTRY-MODULE.

           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM SAVE-MODULE.
           IF IO-TYPE = "EDT"
              GO TO MAIN-MODULE-CONTINUE.

           IF ( NEW-RECORD = "Y" )
              MOVE "N"                     TO NEW-RECORD
              MOVE PC-ID                   TO PROGRAM-FORM-TYPE
              MOVE 01                      TO PROGRAM-FORM-PGNO.

           IF ( PROGRAM-FORM-PAGE-VALID )
              MOVE PC1-KEY                 TO SAVEKEY-PC1-KEY
              MOVE PROGRAM-FORM-PAGE       TO PAGE-SELECTION
              GO TO MAIN-PROGRAM-EXIT.

           MOVE "N" TO NEW-RECORD.

           GO TO MAIN-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE " " TO IO-TYPE.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PCPGA2" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO PCPGRM-LINK-BUF.

           PERFORM GET-LOCK-KEY.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       CREATE-DRIVER SECTION.
      ******************************************************************

           MOVE ZEROES                     TO ELE.
           MOVE SET-RECORD-NEW-ENTRY       TO RECORD-STATUS.

           MOVE "S  A000NEW."              TO VDU.
           MOVE "*** NEW RECORD ***"       TO VDUBUF.
           PERFORM SCREENIO.

       CREATE-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-DRIVER SECTION.
      ******************************************************************

           MOVE SPACES                     TO DISPLAY-BUF.

           MOVE WK-LTA-INS-CO             TO F01.
           MOVE WK-LTA-AGENCY             TO F02.
           MOVE WK-LTA-AGENT              TO F03.
           MOVE WK-LTA-ADR1               TO F04.
           MOVE WK-LTA-ADR2               TO F05.
           MOVE WK-LTA-CITY               TO F06.
           MOVE WK-LTA-STATE              TO F07.
           MOVE WK-LTA-ZIP                TO F08.

           IF ( WK-LTA-PHONE NOT = ZEROES )
              MOVE WK-LTA-PHONE           TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT                TO F09.

           MOVE WK-LTA-POLICY-NO          TO F10.
           MOVE WK-LTA-COV-TYPE           TO F11.
           MOVE WK-LTA-LOSS-PAYEE         TO F12.
           MOVE WK-LTA-DLR-REQ            TO F13.

           MOVE WK-LTA-EFF-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO F14.

           MOVE WK-LTA-EXP-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO F15.

           MOVE WK-LTA-CAN-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO F16.

           MOVE WK-LTA-CONF-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO F17.

           MOVE WK-LTA-BIL-COV            TO F18.
           MOVE WK-LTA-PDL-COV            TO F19.
           MOVE WK-LTA-PIP-COV            TO F20.
           MOVE WK-LTA-UMB-COV            TO F21.
           MOVE WK-LTA-COL-COV            TO F22.
           MOVE WK-LTA-COM-COV            TO F23.
           MOVE WK-LTA-LAST-FORM          TO F24.

       DISPLAY-DRIVER-END.

           PERFORM SEND-DISPLAY.

       DISPLAY-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-KEY-DRIVER SECTION.
      ******************************************************************

           MOVE SPACES                     TO DISPLAY-KEY-BUF.

           MOVE PC-ID                      TO DISPLAY-KEY-ID
           MOVE PC-SOC-SEC                 TO DISPLAY-KEY-SSNO

           PERFORM SEND-DISPLAY-KEY.

       DISPLAY-KEY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *    RECORD-STATUS = 0 - MODIFY EXISTING RECORD
      *                    1 - INITIAL NEW RECORD ENTRY
      *                    2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.

           MOVE " "     TO IO-TYPE.
           MOVE 1       TO ONE.
           MOVE ZEROES  TO ELE.

       CREATE-ROUTINE.

           IF ( NOT RECORD-NEW-ENTRY ) GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE < 1   ) MOVE 1 TO ELE ONE.
           IF ( ELE-VALID ) GO TO ENTER-ELE.

           MOVE SET-RECORD-NEW-MODIFY      TO RECORD-STATUS.

       MODIFY-ROUTINE.

           MOVE LEGEND-MODIFY-ROUTINE      TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL."              TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTRY-MODULE-END.
           IF ( NOT ENTER-KEY ) GO TO MODIFY-ROUTINE.

           MOVE VDUBUF                     TO SEL-ENTRY.

           IF ( SEL-ENTRY = SPACES )
              MOVE "RWR" TO IO-TYPE
              GO TO ENTRY-MODULE-END.

       ENTRY-PAGE-CHANGE.

           MOVE SPACES                     TO PROGRAM-FORM-PAGE.

           IF NOT (( SEL-SINGLE-PAGE-KEY = SPACES   )  AND
                   ( SEL-SINGLE-PAGE-P   = "P"      )  AND
                   ( SEL-SINGLE-PAGE-NUMBER NUMERIC ))
              GO TO CHECK-SEL-ELE.

           IF ( SEL-SINGLE-PAGE-NUMBER = 01 )
              MOVE ZEROES                  TO PROGRAM-FORM-TYPE
              MOVE SEL-SINGLE-PAGE-NUMBER  TO PROGRAM-FORM-PGNO
           ELSE
              MOVE PC-ID                   TO PROGRAM-FORM-TYPE
              MOVE SEL-SINGLE-PAGE-NUMBER  TO PROGRAM-FORM-PGNO.

           MOVE FORM-PATHNAME              TO FORM-PATH.

           IF ( FORM-NAM NOT = PROGRAM-FORM ) AND
              ( PROGRAM-FORM-PAGE-VALID     )
              MOVE "RWR" TO IO-TYPE
              GO TO ENTRY-MODULE-END
           ELSE
              GO TO MODIFY-ROUTINE.

       CHECK-SEL-ELE.

           INSPECT SEL-ENTRY REPLACING ALL SPACES BY ZEROES.

           IF ( SEL-ELE NOT NUMERIC ) GO TO MODIFY-ROUTINE.
           IF ( SEL-ELE = ZEROES    ) GO TO MODIFY-ROUTINE.
           IF ( SEL-ELE > MAX       ) GO TO MODIFY-ROUTINE.

           MOVE SEL-ELE                    TO ELE.

       ENTER-ELE.

           MOVE LEGEND-ENTER-ELE           TO LEGEND.
           PERFORM SEND-LEGEND.

           IF ( SEL-LOCK-KEY  NOT = ZEROES ) AND
              ( ELEMENT-LOCK(ELE) = SPACES ) AND
              ( NOT RECORD-NEW-ENTRY       )
              GO TO MODIFY-ROUTINE.

           MOVE ELEMENT-VDU(ELE)           TO VDU.
           MOVE ELE                        TO VDUROW.
           MOVE ZEROES                     TO VDUCOL     CURRENT-VDUCOL.
           MOVE "."                        TO VDUDECIMAL.
           PERFORM SCREENIO.

       ENTRY-UP-KEY.

           IF ( UP-KEY           ) AND
              ( RECORD-NEW-ENTRY )
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

       ENTRY-F1-KEY.

           IF ( F1-KEY )
              GO TO ENTRY-MODULE-END.

       ENTRY-ENTER-KEY.

           IF ( NOT ENTER-KEY )
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

       GOTO-FIELD.

           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24
                 DEPENDING ON ELE.

           GO TO ENTRY-MODULE-END.

       F-01.
           MOVE VDUBUF TO WK-LTA-INS-CO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO WK-LTA-AGENCY.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO WK-LTA-AGENT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO WK-LTA-ADR1.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO WK-LTA-ADR2.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUBUF TO WK-LTA-CITY.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUBUF TO WK-LTA-STATE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDUBUF TO WK-LTA-ZIP.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO WK-LTA-PHONE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE VDUBUF TO WK-LTA-POLICY-NO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE VDUBUF TO WK-LTA-COV-TYPE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
           MOVE VDUBUF TO WK-LTA-LOSS-PAYEE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
           MOVE VDUBUF TO WK-LTA-DLR-REQ.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-14.
           MOVE VDU-DATE-YYYYMMDD TO WK-LTA-EFF-DATE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-15.
           MOVE VDU-DATE-YYYYMMDD TO WK-LTA-EXP-DATE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           MOVE VDU-DATE-YYYYMMDD TO WK-LTA-CAN-DATE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDU-DATE-YYYYMMDD TO WK-LTA-CONF-DATE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-18.
           MOVE VDUNUM2 TO WK-LTA-BIL-COV.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-19.
           MOVE VDUNUM2 TO WK-LTA-PDL-COV.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-20.
           MOVE VDUNUM2 TO WK-LTA-PIP-COV.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-21.
           MOVE VDUNUM2 TO WK-LTA-UMB-COV.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-22.
           MOVE VDUNUM2 TO WK-LTA-COL-COV.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-23.
           MOVE VDUNUM2 TO WK-LTA-COM-COV.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-24.
           MOVE VDUNUM0 TO WK-LTA-LAST-FORM.
           GO TO CREATE-ROUTINE.

       ENTRY-MODULE-END.

           IF ( F1-KEY ) MOVE "CLR" TO IO-TYPE.

       ENTRY-MODULE-EXIT.
           EXIT.

      ******************************************************************
       LOAD-MODULE SECTION.
      ******************************************************************

           MOVE SPACES          TO IO-TYPE.

           MOVE SET-RECORD-EXIST-MODIFY    TO RECORD-STATUS.

           PERFORM FORM-RESET.

           IF ( SAVEKEY-PC1-KEY NOT = SPACES )
              MOVE SAVEKEY-PC1-KEY         TO PC1-KEY   HOLD-PC1-KEY
              MOVE SPACES                  TO SAVEKEY-PC1-KEY
              PERFORM DISPLAY-KEY-DRIVER
              GO TO LOAD-RECORD-FROM-KEY.

           MOVE EXT-FILPATH-BRNO TO BR-NO PC-BRANCH HOLD-PC-BRANCH.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              MOVE "INVALID: BRANCH NUMBER IS NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO LOAD-MODULE-END.

       LOAD-KEY-ID.

           MOVE "S  A010KEY-ID." TO VDU.
           MOVE "A"              TO VDUBUF PC-ID HOLD-PC-ID.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       LOAD-KEY-SSNO.

           MOVE "ENNN100KEY-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO LOAD-KEY-SSNO.
           IF (        F1-KEY ) GO TO LOAD-MODULE-END.
           IF (        F7-KEY ) GO TO LOAD-MODULE-END.
           IF ( NOT ENTER-KEY ) GO TO LOAD-KEY-SSNO.

           IF ( VDUNUM0 = ZEROES )
              GO TO LOAD-KEY-SSNO.

           MOVE VDUNUM0 TO PC-SOC-SEC HOLD-PC-SOC-SEC.

       LOAD-RECORD-FROM-KEY.

           PERFORM DISPLAY-KEY-DRIVER.

           PERFORM OPEN-PC1-FILE.
           PERFORM READ-PC1-FILE.
           PERFORM CLOSE-PC1-FILE.
           IF ( IO-GOOD )
              MOVE PC-REC                  TO ORIGINAL-PC-REC
              MOVE PC-LTA-AUTO-RECORD      TO WK-LTA-RECORD
              PERFORM DISPLAY-DRIVER
            ELSE
              MOVE "ERROR: CAN ONLY CREATE NEW RECORD ON PAGE 1" TO MESS
              PERFORM SEND-MESS
              MOVE VDU---F1-KEY TO VDUSTATUS.

           IF ( NEW-RECORD = "Y" )
              PERFORM CREATE-DRIVER.

       LOAD-MODULE-END.

           IF ( F1-KEY ) MOVE "CLR" TO IO-TYPE.
           IF ( F7-KEY ) MOVE "END" TO IO-TYPE.

       LOAD-MODULE-EXIT.
           EXIT.

      ******************************************************************
       SAVE-MODULE SECTION.
      ******************************************************************

           MOVE SPACES          TO IO-TYPE.

           PERFORM OPEN-PC1-FILE.
           MOVE WK-LTA-RECORD             TO PC-LTA-AUTO-RECORD.
           MOVE PC-REC                    TO CHANGED-PC-REC.

           IF ( ORIGINAL-PC-REC = CHANGED-PC-REC )
              GO TO SAVE-MODULE-END.

           PERFORM READ-PC1-FILE.
           IF ( IO-BAD )
              PERFORM MESSAGE-UNDERGONE-CHANGE
              PERFORM MESSAGE-RECORD-DELETED
              GO TO SAVE-MODULE-END.

           IF ( PC-REC NOT = ORIGINAL-PC-REC )
              PERFORM MESSAGE-UNDERGONE-CHANGE
              PERFORM MESSAGE-CHANGE-AGAIN
              GO TO SAVE-MODULE-END.

           MOVE CHANGED-PC-REC             TO PC-REC.
           PERFORM REWRITE-PC1-FILE.

       SAVE-MODULE-END.

           PERFORM CLOSE-PC1-FILE.
           MOVE "CLR" TO IO-TYPE.
       SAVE-MODULE-EXIT.
           EXIT.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO PCPGA2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PCPGA2-SYSDATE.
           DISPLAY SPACES UPON PCPGRM-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PCPGA2-SCREEN.
           DISPLAY PCPGA2-SCREEN UPON PCPGRM-WINDOW-HANDLE.
           MOVE PCPGA2-FORM-FIELDS TO WR-BUF.
           MOVE PCPGA2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE PCPGA2-HELP--FILE TO HELP-LINK-FILE.
           MOVE PCPGA2-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/PCPGA2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBMN/COMMON.CPY".

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-DISPLAY.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-DISPLAY-KEY.
           MOVE DISPLAY-KEY-BUF TO WR-BUF.
           MOVE DISPLAY-KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-PC1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPPCGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPPC1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PCPGA2-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE PCPGRM-LINK-BUF TO FORM-LINK-AREA.
           EXIT PROGRAM.
