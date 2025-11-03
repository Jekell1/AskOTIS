       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PCPGC2.
       AUTHOR.        KEC.
       DATE-WRITTEN.  06-30-2006.
      ******************************************************************
      *
      * IDENTIFICATION:  MN/PCPGC2.C
      *
      * DESCRIPTION   :  SCRATCH PAD FILE (PC) FILE MAINTENANCE
      *                  --- PC-ID = "C" (COLLATERAL) PAGE 2
      *
      *=================================================================
      * REV:
      * KEC 2006.0622 {PR#00127} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *               ***NEW***
      * MJD 131203 A30 CONVERSION COMPLETED
      *
      * KEC 2017.0324 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED COMP-3 FROM PC1-KEY.
      * CS 20200130 REMOVED BRANCH FROM KEY  AND FORCED TO EXT-FILPATH-BRNO
      * BAH 20200617 CHANGED PC-LT-RECORD TO PC-LTC-RECORD
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
           "@(#)MN/PCPGC2.CBL S35 03/07/24-09:15:12 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01IX.CPY".
       COPY "LIBLP/LP01IX_SQL.CPY".
       COPY "LIBLP/LPWSIX.CPY".
       COPY "LIBLP/LP01PC.CPY".
       COPY "LIBLP/LP01PC_SQL.CPY".
       COPY "LIBLP/LPWSPC.CPY".
       COPY "LIBLP/LP01WK.CPY".


      *-----------------------------------------------------------------
       01  HOLD-PC1-KEY.
           03  HOLD-PC-ID              PIC X(01).
           03  HOLD-PC-BRANCH          PIC 9(04).
           03  HOLD-PC-SOC-SEC         PIC 9(10).

       01  RECORD-SIZES-WORKERS.
           03  ORIGINAL-PC-REC         PIC X(PC-SIZE).
           03  CHANGED-PC-REC          PIC X(PC-SIZE).

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  SUBSCRIPT-WORKERS.
           03  SUB                     PIC 9(03)     VALUE ZEROES.
           03  S1                      PIC 9(03)     VALUE ZEROES.
           03  S2                      PIC 9(03)     VALUE ZEROES.

       01  OUT-SUB                     PIC 9(02)     COMP-3 VALUE 0.
       01  OUT-VIN                     PIC X(06)     VALUE SPACES.
       01  OUT-VINX          REDEFINES OUT-VIN.
           03  OUT-VIN-BYTE            PIC X(01)     OCCURS 6.

       01  WS-ACCTNO                   PIC 9(08).
       01  WS-ACCTNOX        REDEFINES WS-ACCTNO.
           03  WS-NUMBER               PIC 9(06).
           03  FILLER                  PIC 9(02).

      ******************************************************************
      *
      *    D I S P L A Y - W O R K E R S
      *
      ******************************************************************
       01  DISPLAY-BUF.

           03  DISPLAY-AUTO-1-BUF.
               05  F01                 PIC X(10).
               05  F02                 PIC X(16).
               05  F03                 PIC X(16).
               05  F04                 PIC X(04).
               05  F05                 PIC X(08).
               05  F06                 PIC X(08).
               05  F07                 PIC X(20).
               05  F08                 PIC X(08).
               05  F09                 PIC Z(07)-     BLANK ZERO.
               05  F10                 PIC Z(05).99   BLANK ZERO.
               05  F11                 PIC Z(05).99   BLANK ZERO.
               05  F12                 PIC Z(05).99-  BLANK ZERO.

           03  DISPLAY-AUTO-2-BUF.
               05  F13                 PIC X(10).
               05  F14                 PIC X(16).
               05  F15                 PIC X(16).
               05  F16                 PIC X(04).
               05  F17                 PIC X(08).
               05  F18                 PIC X(08).
               05  F19                 PIC X(20).
               05  F20                 PIC X(08).
               05  F21                 PIC Z(07)-     BLANK ZERO.
               05  F22                 PIC Z(05).99   BLANK ZERO.
               05  F23                 PIC Z(05).99   BLANK ZERO.
               05  F24                 PIC Z(05).99-  BLANK ZERO.

           03  F25                     PIC X(80).
           03  F26                     PIC X(80).

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.

           03  DISPLAY-AUTO-1-LIST.
               05  FILLER PIC X(20) VALUE "010 020 030 040 050 ".
               05  FILLER PIC X(20) VALUE "060 070 080 090 100 ".
               05  FILLER PIC X(07) VALUE "110 120".
               05  DISPLAY-AUTO-1-LIST-END
                          PIC X(01) VALUE SPACES.

           03  DISPLAY-AUTO-2-LIST.
               05  FILLER PIC X(20) VALUE "130 140 150 160 170 ".
               05  FILLER PIC X(20) VALUE "180 190 200 210 220 ".
               05  FILLER PIC X(07) VALUE "230 240".
               05  DISPLAY-AUTO-2-LIST-END
                          PIC X(01) VALUE SPACES.

           03  FILLER PIC X(08) VALUE "250 260.".

      *=================================================================
       01  DISPLAY-KEY-BUF.
           03  DISPLAY-KEY-ID          PIC X(01)          VALUE SPACES.
      *    03  DISPLAY-KEY-BRANCH      PIC 9(04)          VALUE ZEROES.
           03  DISPLAY-KEY-SSNO        PIC 9(10)          VALUE ZEROES.

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-LIST.
           03  FILLER PIC X(07) VALUE "KEY-ID ".
           03  FILLER PIC X(11) VALUE "KEY-BRANCH ".
           03  FILLER PIC X(09) VALUE "KEY-SSNO.".

      ******************************************************************
      *
      *    E L E M E N T - W O R K E R S
      *
      ******************************************************************
       78  MAX                                       VALUE 26.
       78  MAX---BEG-AUTO-1                          VALUE 01.
       78  MAX---END-AUTO-1                          VALUE 12.
       78  MAX---BEG-AUTO-2                          VALUE 13.
       78  MAX---END-AUTO-2                          VALUE 24.
       77  ONE                         PIC S9(1)     VALUE ZEROES.
       77  ELE                         PIC 9(02)     VALUE ZEROES.
           88  ELE-VALID                             VALUE 01 THRU MAX.

      *=================================================================
       01  ELEMENT-TABLE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{01 THRU 10}
           03  FILLER                  PIC X(8) VALUE "E  A100 ".
           03  FILLER                  PIC X(8) VALUE "E  A160 ".
           03  FILLER                  PIC X(8) VALUE "E  A160 ".
           03  FILLER                  PIC X(8) VALUE "E  A040 ".
           03  FILLER                  PIC X(8) VALUE "E  A080 ".
           03  FILLER                  PIC X(8) VALUE "E  A080 ".
           03  FILLER                  PIC X(8) VALUE "E  A200 ".
           03  FILLER                  PIC X(8) VALUE "E  A080 ".
           03  FILLER                  PIC X(8) VALUE "ENNS070 ".
           03  FILLER                  PIC X(8) VALUE "ENNN052 ".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{11 THRU 20}
           03  FILLER                  PIC X(8) VALUE "ENNN052 ".
           03  FILLER                  PIC X(8) VALUE "ENNS052 ".
           03  FILLER                  PIC X(8) VALUE "E  A100 ".
           03  FILLER                  PIC X(8) VALUE "E  A160 ".
           03  FILLER                  PIC X(8) VALUE "E  A160 ".
           03  FILLER                  PIC X(8) VALUE "E  A040 ".
           03  FILLER                  PIC X(8) VALUE "E  A080 ".
           03  FILLER                  PIC X(8) VALUE "E  A080 ".
           03  FILLER                  PIC X(8) VALUE "E  A200 ".
           03  FILLER                  PIC X(8) VALUE "E  A080 ".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{21 THRU 26}
           03  FILLER                  PIC X(8) VALUE "ENNS070 ".
           03  FILLER                  PIC X(8) VALUE "ENNN052 ".
           03  FILLER                  PIC X(8) VALUE "ENNN052 ".
           03  FILLER                  PIC X(8) VALUE "ENNS052 ".
           03  FILLER                  PIC X(8) VALUE "E  A800 ".
           03  FILLER                  PIC X(8) VALUE "E  A800 ".

      *=================================================================
       01  ELEMENT-TABLE-RDF           REDEFINES ELEMENT-TABLE.
           03  ELEMENT-TABLE-OCCURS    OCCURS MAX.
               05  ELEMENT-VDU         PIC X(07).
               05  ELEMENT-LOCK        PIC X(01).

      *=================================================================
       78  ELEMENT---SWITCH-LOCK                     VALUE "N".
       78  ELEMENT---SWITCH-UNLOCK                   VALUE SPACES.
      *-----------------------------------------------------------------
       77  ELEMENT-SWITCH              PIC X(01)     VALUE SPACES.
           88  ELEMENT-SWITCH-LOCK                   VALUE "N".
           88  ELEMENT-SWITCH-UNLOCK                 VALUE SPACES.

       77  ELEMENT-SWITCH-AUTO         PIC 9(01)     VALUE ZEROES.
           88  ELEMENT-SWITCH-AUTO-VALID             VALUE 1 2.

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
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBMN/COMMONW.CPY".
       COPY "LIBMN/PCPGRMW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/PCPGC2_DEF.CPY".
       COPY "LIBMN/PCPGC2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/PCPGC2_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           PERFORM INITIALIZATION.

       MAIN-MODULE.

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
           MOVE "PCPGC2" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO PCPGRM-LINK-BUF.

           PERFORM GET-LOCK-KEY.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       CREATE-DRIVER SECTION.
      ******************************************************************

           MOVE ZEROES                     TO ELE.
           MOVE SET-RECORD-NEW-ENTRY       TO RECORD-STATUS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A000NEW."              TO VDU.
           MOVE "*** NEW RECORD ***"       TO VDUBUF.
           PERFORM SCREENIO.

       CREATE-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-DRIVER SECTION.
      ******************************************************************

           MOVE SPACES                     TO DISPLAY-BUF.

           PERFORM DISPLAY-AUTO-1-DRIVER.
           PERFORM DISPLAY-AUTO-2-DRIVER.

           MOVE WK-HHGS-1             TO F25.
           MOVE WK-HHGS-2             TO F26.

       DISPLAY-DRIVER-END.

           PERFORM SEND-DISPLAY.

       DISPLAY-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-AUTO-1-DRIVER SECTION.
      ******************************************************************

           MOVE SPACES                     TO DISPLAY-AUTO-1-BUF.

           MOVE WK-DESC-1                  TO F01.
           MOVE WK-MAKE-1                  TO F02.
           MOVE WK-MODEL-1                 TO F03.
           MOVE WK-YEAR-1                  TO F04.
           MOVE WK-STYLE-1                 TO F05.
           MOVE WK-CLASS-1                 TO F06.
           MOVE WK-VIN-1                    TO F07.
           MOVE WK-TITLE-NO-1              TO F08.
           MOVE WK-MILEAGE-1               TO F09.
           MOVE WK-VALUE-1                 TO F10.
           MOVE WK-SELLPR-1                TO F11.
           MOVE WK-MISC-AMT-1              TO F12.

       DISPLAY-AUTO-1-DRIVER-END.

           PERFORM SEND-DISPLAY-AUTO-1.

       DISPLAY-AUTO-1-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-AUTO-2-DRIVER SECTION.
      ******************************************************************

           MOVE SPACES                     TO DISPLAY-AUTO-2-BUF.

           MOVE WK-DESC-2                  TO F13.
           MOVE WK-MAKE-2                  TO F14.
           MOVE WK-MODEL-2                 TO F15.
           MOVE WK-YEAR-2                  TO F16.
           MOVE WK-STYLE-2                 TO F17.
           MOVE WK-CLASS-2                 TO F18.
           MOVE WK-VIN-2                   TO F19.
           MOVE WK-TITLE-NO-2              TO F20.
           MOVE WK-MILEAGE-2               TO F21.
           MOVE WK-VALUE-2                 TO F22.
           MOVE WK-SELLPR-2                TO F23.
           MOVE WK-MISC-AMT-2              TO F24.

       DISPLAY-AUTO-2-DRIVER-END.

           PERFORM SEND-DISPLAY-AUTO-2.

       DISPLAY-AUTO-2-DRIVER-EXIT.
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
       ELEMENT-SWITCH-DRIVER SECTION.
      ******************************************************************

           IF ( ELEMENT-SWITCH-AUTO = 01 )
              MOVE MAX---BEG-AUTO-1        TO S1
              MOVE MAX---END-AUTO-1        TO S2
           ELSE
              MOVE MAX---BEG-AUTO-2        TO S1
              MOVE MAX---END-AUTO-2        TO S2.

           PERFORM VARYING SUB FROM S1 BY 1 UNTIL SUB > S2
                   MOVE ELEMENT-SWITCH     TO ELEMENT-LOCK(SUB)
           END-PERFORM.

       ELEMENT-SWITCH-ROUT-EXIT.
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

           MOVE " "                        TO IO-TYPE.
           MOVE 1                          TO ONE.
           MOVE ZEROES                     TO ELE.

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
                 F-21 F-22 F-23 F-24 F-25 F-26
                 DEPENDING ON ELE.

           GO TO ENTRY-MODULE-END.

       F-01.

           IF ( RECORD-NEW-ENTRY ) AND
              ( VDUBUF = SPACES  )
              MOVE SPACES                  TO WK-AUTO-ALP-1
              MOVE ZEROES                  TO WK-MILEAGE-1
                                              WK-VALUE-1
                                              WK-SELLPR-1
                                              WK-MISC-AMT-1
                                              WK-VIN-CHG-DATE-1
              MOVE ELEMENT---SWITCH-LOCK   TO ELEMENT-SWITCH
              MOVE 01                      TO ELEMENT-SWITCH-AUTO
              PERFORM ELEMENT-SWITCH-DRIVER
              MOVE 12                      TO ELE
              GO TO CREATE-ROUTINE.

           MOVE VDUBUF                     TO WK-DESC-1.

           IF ( VDUBUF = SPACES )
              MOVE SPACES                  TO WK-AUTO-ALP-1
              MOVE ZEROES                  TO WK-MILEAGE-1
                                              WK-VALUE-1
                                              WK-SELLPR-1
                                              WK-MISC-AMT-1
                                              WK-VIN-CHG-DATE-1
              MOVE ELEMENT---SWITCH-LOCK   TO ELEMENT-SWITCH
              MOVE 01                      TO ELEMENT-SWITCH-AUTO
              PERFORM ELEMENT-SWITCH-DRIVER
              PERFORM DISPLAY-AUTO-1-DRIVER
              GO TO CREATE-ROUTINE.

           MOVE ELEMENT---SWITCH-UNLOCK    TO ELEMENT-SWITCH
           MOVE 01                         TO ELEMENT-SWITCH-AUTO
           PERFORM ELEMENT-SWITCH-DRIVER

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO WK-MAKE-1.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO WK-MODEL-1.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO WK-YEAR-1.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO WK-STYLE-1.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUBUF TO WK-CLASS-1.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.

           MOVE VDUBUF                     TO WK-VIN-1.

           IF ( VDUBUF NOT = SPACES )
              IF ( BR-COLL-ID-SCAN = "Y" OR "S" )
                 MOVE WK-VIN-1              TO IX-VIN
                 PERFORM VIN-DRIVER
                 IF ( IO-GOOD )
                    MOVE IX-ACCTNO   TO WS-ACCTNO
                    STRING "VIN ALREADY EXIST, ACCOUNT: ", WS-NUMBER
                           DELIMITED BY "  " INTO MESS
                    PERFORM SEND-MESS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDUBUF TO WK-TITLE-NO-1.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUNUM0 TO WK-MILEAGE-1.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE VDUNUM2 TO WK-VALUE-1.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE VDUNUM2 TO WK-SELLPR-1.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
           MOVE VDUNUM2 TO WK-MISC-AMT-1.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.

           IF ( RECORD-NEW-ENTRY ) AND
              ( VDUBUF = SPACES  )
              MOVE SPACES                  TO WK-AUTO-ALP-2
              MOVE ZEROES                  TO WK-MILEAGE-2
                                              WK-VALUE-2
                                              WK-SELLPR-2
                                              WK-MISC-AMT-2
                                              WK-VIN-CHG-DATE-2
              MOVE ELEMENT---SWITCH-LOCK   TO ELEMENT-SWITCH
              MOVE 02                      TO ELEMENT-SWITCH-AUTO
              PERFORM ELEMENT-SWITCH-DRIVER
              MOVE 24                      TO ELE
              GO TO CREATE-ROUTINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO WK-DESC-2.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUBUF = SPACES )
              MOVE SPACES                  TO WK-AUTO-ALP-2
              MOVE ZEROES                  TO WK-MILEAGE-2
                                              WK-VALUE-2
                                              WK-SELLPR-2
                                              WK-MISC-AMT-2
                                              WK-VIN-CHG-DATE-2
              MOVE ELEMENT---SWITCH-LOCK   TO ELEMENT-SWITCH
              MOVE 02                      TO ELEMENT-SWITCH-AUTO
              PERFORM ELEMENT-SWITCH-DRIVER
              PERFORM DISPLAY-AUTO-2-DRIVER
              GO TO CREATE-ROUTINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ELEMENT---SWITCH-UNLOCK    TO ELEMENT-SWITCH
           MOVE 02                         TO ELEMENT-SWITCH-AUTO
           PERFORM ELEMENT-SWITCH-DRIVER

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-14.
           MOVE VDUBUF TO WK-MAKE-2.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-15.
           MOVE VDUBUF TO WK-MODEL-2.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           MOVE VDUBUF TO WK-YEAR-2.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDUBUF TO WK-STYLE-2.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-18.
           MOVE VDUBUF TO WK-CLASS-2.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-19.

           MOVE VDUBUF                     TO WK-VIN-2.

           IF ( VDUBUF NOT = SPACES )
              IF ( BR-COLL-ID-SCAN = "Y" OR "S" )
                 MOVE WK-VIN-2              TO IX-VIN
                 PERFORM VIN-DRIVER
                 IF ( IO-GOOD )
                    MOVE IX-ACCTNO   TO WS-ACCTNO
                    STRING "VIN ALREADY EXIST, ACCOUNT: ", WS-NUMBER
                           DELIMITED BY "  " INTO MESS
                    PERFORM SEND-MESS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-20.
           MOVE VDUBUF TO WK-TITLE-NO-2.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-21.
           MOVE VDUNUM0 TO WK-MILEAGE-2.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-22.
           MOVE VDUNUM2 TO WK-VALUE-2.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-23.
           MOVE VDUNUM2 TO WK-SELLPR-2.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-24.
           MOVE VDUNUM2 TO WK-MISC-AMT-2.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-25.
           MOVE VDUBUF TO WK-HHGS-1.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-26.
           MOVE VDUBUF TO WK-HHGS-2.
           GO TO CREATE-ROUTINE.

       ENTRY-MODULE-END.

           IF ( F1-KEY ) MOVE "CLR" TO IO-TYPE.

       ENTRY-MODULE-EXIT.
           EXIT.

      ******************************************************************
       LOAD-MODULE SECTION.
      ******************************************************************

           MOVE " " TO IO-TYPE.

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
      *=================================================================
       LOAD-KEY-ID.

           MOVE "S  A010KEY-ID." TO VDU.
           MOVE "C"              TO VDUBUF PC-ID HOLD-PC-ID.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
      *LOAD-KEY-BRANCH.

      *    MOVE LEGEND-LOAD-KEY-BRANCH    TO LEGEND.
      *    PERFORM SEND-LEGEND.

      *    MOVE "ENNN040KEY-BRANCH." TO VDU.
      *    PERFORM SCREENIO.

      *    IF (        F1-KEY ) GO TO LOAD-MODULE-END.
      *    IF (        F7-KEY ) GO TO LOAD-MODULE-END.
      *    IF ( NOT ENTER-KEY ) GO TO LOAD-KEY-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF ( VDUNUM0 = ZEROES )
      *       GO TO LOAD-KEY-BRANCH.

      *    MOVE VDUNUM0 TO BR-NO PC-BRANCH HOLD-PC-BRANCH.

      *    PERFORM OPEN-BR-FILE.
      *    PERFORM READ-BR-FILE.
      *    PERFORM CLOSE-BR-FILE.

      *    IF ( IO-BAD )
      *       MOVE "INVALID: BRANCH NUMBER IS NOT ON FILE" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO LOAD-KEY-BRANCH.

      *-----------------------------------------------------------------
       LOAD-KEY-SSNO.

           MOVE "ENNN100KEY-SSNO." TO VDU.
           PERFORM SCREENIO.

      *    IF (        UP-KEY ) GO TO LOAD-KEY-SSNO.
           IF (        F1-KEY ) GO TO LOAD-MODULE-END.
           IF (        F7-KEY ) GO TO LOAD-MODULE-END.
           IF ( NOT ENTER-KEY ) GO TO LOAD-KEY-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 = ZEROES )
              GO TO LOAD-KEY-SSNO.

           MOVE VDUNUM0 TO PC-SOC-SEC HOLD-PC-SOC-SEC.

      *=================================================================
       LOAD-RECORD-FROM-KEY.

           PERFORM DISPLAY-KEY-DRIVER.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM OPEN-PC1-FILE.
           PERFORM READ-PC1-FILE.
           PERFORM CLOSE-PC1-FILE.

           IF ( IO-GOOD )
              MOVE PC-REC                  TO ORIGINAL-PC-REC
              MOVE PC-LTC-HHGS(1) TO WK-HHGS-1
              MOVE PC-LTC-HHGS(2) TO WK-HHGS-2
              MOVE PC-LTC-HHGS(3) TO WK-HHGS-3
              MOVE PC-LTC-AUTO(1) TO WK-AUTO-1
              MOVE PC-LTC-AUTO(2) TO WK-AUTO-2
              MOVE PC-LTC-AUTO(3) TO WK-AUTO-3
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

           MOVE " " TO IO-TYPE.

           PERFORM OPEN-PC1-FILE.

           MOVE WK-HHGS-1    TO PC-LTC-HHGS(1).
           MOVE WK-HHGS-2    TO PC-LTC-HHGS(2).
           MOVE WK-HHGS-3    TO PC-LTC-HHGS(3).
           MOVE WK-AUTO-1    TO PC-LTC-AUTO(1).
           MOVE WK-AUTO-2    TO PC-LTC-AUTO(2).
           MOVE WK-AUTO-3    TO PC-LTC-AUTO(3).

           MOVE PC-REC                     TO CHANGED-PC-REC.

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

      ******************************************************************
       VIN-DRIVER SECTION.
      ******************************************************************

           IF ( BR-COLL-ID-SCAN NOT = "S" )
              GO TO VIN-START-IX2.

           MOVE SPACES                     TO OUT-VIN.
           MOVE 20                         TO SUB.
           MOVE 06                         TO OUT-SUB.

       VIN-READ-NEXT-IX2.

           IF ( VDUTAB(SUB) NOT = SPACES )
              MOVE VDUTAB(SUB)             TO OUT-VIN-BYTE(OUT-SUB)
              SUBTRACT 1 FROM OUT-SUB.

           IF ( OUT-SUB > 0 )
              IF ( SUB > 1 )
                 SUBTRACT 1 FROM SUB
                 GO TO VIN-READ-NEXT-IX2.

           MOVE OUT-VIN                    TO IX-VIN.    

       VIN-START-IX2.

           PERFORM OPEN-IX2-FILE.

           MOVE IX-PATH-OWNBR TO IX-BRNO.

           PERFORM START-IX2-FILE.
           PERFORM READ-IX2-FILE-NEXT.
           IF ( IO-GOOD )
              IF ( BR-COLL-ID-SCAN = "S" )
                 IF ( IX-VIN NOT = OUT-VIN ) OR
                     (IX-BRNO NOT = IX-PATH-OWNBR)
                    MOVE 9 TO IO-FG
                 ELSE
                    NEXT SENTENCE
              ELSE
                 IF ( IX-VIN NOT = VDUBUF ) OR
                     (IX-BRNO NOT = IX-PATH-OWNBR)
                    MOVE 9 TO IO-FG.


           PERFORM CLOSE-IX2-FILE.

       VIN-DRIVER-END.
           EXIT.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO PCPGC2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PCPGC2-SYSDATE.
           DISPLAY SPACES UPON PCPGRM-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PCPGC2-SCREEN.
           DISPLAY PCPGC2-SCREEN UPON PCPGRM-WINDOW-HANDLE.
           MOVE PCPGC2-FORM-FIELDS TO WR-BUF.
           MOVE PCPGC2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE PCPGC2-HELP--FILE TO HELP-LINK-FILE.
           MOVE PCPGC2-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/PCPGC2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBMN/COMMON.CPY".
       COPY "LIBGB/DATER.CPY".

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-DISPLAY.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-DISPLAY-AUTO-1.
           MOVE DISPLAY-AUTO-1-BUF TO WR-BUF.
           MOVE DISPLAY-AUTO-1-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SPACES         TO DISPLAY-AUTO-1-LIST-END.
       SEND-DISPLAY-AUTO-2.
           MOVE DISPLAY-AUTO-2-BUF TO WR-BUF.
           MOVE DISPLAY-AUTO-2-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SPACES         TO DISPLAY-AUTO-2-LIST-END.
       SEND-DISPLAY-KEY.
           MOVE DISPLAY-KEY-BUF TO WR-BUF.
           MOVE DISPLAY-KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-LEGEND.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-PC1-FILE.
           PERFORM CLOSE-IX2-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPPCGS_SQL.CPY".
       COPY "LIBLP/LPIXGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPIX2RN.CPY".
       COPY "LIBLP/LPPC1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.
       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PCPGC2-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE PCPGRM-LINK-BUF TO FORM-LINK-AREA.
           EXIT PROGRAM.
