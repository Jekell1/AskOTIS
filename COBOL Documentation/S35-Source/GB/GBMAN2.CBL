       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GBMAN2.
      ******************************************************************
      *
      *  DESC: GLOBAL FILE MAINTENANCE - PAGE 2
      *        2019-0918 GB/GBMNA1 REPLACED GB/GBMAN1 SINCE IT NOW
      *        NEEDS TO ALWAYS ASK FOR BRANCH NUMBER.
      *
      *=================================================================
      * REV:
      * 190708 NEW
      * 190712 ADD GB-EOM-UPDATE-DATE,GB-EOM-UPDATE-TIME,GB-EOM-UPDATE-USERID
      * 190725 IN DISPLAY FIELDS, FOUND A PROBLEM WITH DISPLAY OF EOY 
      *         CALENDAR
      *NEW PAGE 2
      * 190726 ADD NEW PAGE 2, ADDED GB/EOM-TAG-CCYYMM,GB-EOM-UPDATE-DATE
      *         GB-EOM-UPDATE-TIME,GB-EOM-UPDATE-USERID, GB-FISCAL-EOY-TAG-CCYY
      *         -MM,GB-EOYF-UPDATE-DATE,GB-EOYF-UPDAT-TIME,GB-EOYF-UPDATE-
      *         USERID, GB-CALENDAR-EOY-TAG-CCYYMM, GB-EOYC-UPDATE-DATE,
      *         GB-EOYC-UPDATE-TIME,GB-EOYC-USERID
      * 190801 FIXED FOR MULTI PAGE MAINTENANCE
      * 190805 DONT ALLOW LEAVING SCREEN WITHOUT THE DATE FIELD YYYYMM
      *         BEING COMPLETED (GB-EOM-CCYYMM, GB-FISCAL-EOY-TAG-CCYYMM
      *         GB-CALENDAR-EOY-TAG-CCYYMM
      * 190806 VALIDATE THE 8 DIGIT DATE ENTRIES
      * BAH 190918 REMOVED ACCESS-CALL ON GBFILE
      *  CS 190920 ADD CHECK ON FIELDS 1,2,3 CAN NOT BE 0
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/GBMAN2.CBL S34 05/21/21-15:00:15 >".

       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

      *01  ERRCD                               PIC X       VALUE SPACES.
       01  ACTION.
           03  CHKEY                           PIC X(4).
           03  CHNO                            PIC 9(2).

       01  FORM-NUM REDEFINES ACTION.
           03  FORM-KEY    PIC X(4).
           03  FORM-P      PIC X.
           03  FORM-SEL    PIC 9.
       
       01  CHECK-DATE-FG                       PIC X VALUE SPACES.

       01  DATE-SW                             PIC XXX     VALUE "   ".
           88  BAD-DATE                                    VALUE "BAD".

      * USED TO VALIDATE MMDD FOR HOLIDAY TABLE AND ALSO FOR MMYY ACCRUAL
       01  WS-DATE                             PIC 9(6).
       01  WS-DATE-R                 REDEFINES WS-DATE.
           03  FILLER                          PIC XX.
           03  WS-MM                           PIC 99.
           03  WS-DD                           PIC 99.

       01  ELE                                 PIC 9(2).
       01  IO-TYPE                             PIC X(3).
       01  NEW-FLAG                            PIC 9.
       01  ONE                                 PIC S9.
       01  SAVE-LOGNAME                        PIC X(10).
       01  TODAYS-DATE                         PIC 9(8).

      * ORIGINAL RECORD GBOR-REC
       COPY "LIBGB/GB01GB.CPY" REPLACING LEADING "GB" BY "GBOR".
      * CHANGED RECORD GBCH-REC
       COPY "LIBGB/GB01GB.CPY" REPLACING LEADING "GB" BY "GBCH".

      *=================================================================
      *    DESC: LOG-MAINTENANCE-CHGS WORKERS
      ******************************************************************
       01  LMC-DETAIL                  PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES LMC-DETAIL.
           03  LMC-BRNO                PIC 9(04).
           03  FILLER                  PIC X(01).
           03  LMC-FIELD               PIC X(03).
           03  FILLER                  PIC X(01).
           03  LMC-DESC                PIC X(09).
           03  FILLER                  PIC X(01).

           03  LMC-ORG-TEXT            PIC X(16).
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-ORG-DATE            REDEFINES LMC-ORG-TEXT
                                       PIC 99/99/99.
           03  LMC-ORG-DLNO            REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-ORG-GLNO            REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-ORG-PHONE           REDEFINES LMC-ORG-TEXT
                                       PIC 999/999/9999.
           03  LMC-ORG-SSNO            REDEFINES LMC-ORG-TEXT
                                       PIC 999/99/9999.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(16).
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-CHG-DATE            REDEFINES LMC-CHG-TEXT
                                       PIC 99/99/99.
           03  LMC-CHG-DLNO            REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-CHG-GLNO            REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-CHG-PHONE           REDEFINES LMC-CHG-TEXT
                                       PIC 999/999/9999.
           03  LMC-CHG-SSNO            REDEFINES LMC-CHG-TEXT
                                       PIC 999/99/9999.
       01 GBMNAL-LINK.
          03 SAVEKEY.           
             05 SAVE-BRNO         PIC 9(4).
             05 SAVE-NO           PIC 9.
          03 NEW-REC              PIC X.
          03 ERRCD                PIC X.
          03 FORMNUM              PIC 9.
          03 GBMAIN-WINDOW-HANDLE PIC X(10).

      *=================================================================
      *
      *    SCREEN DISPLAY BUFFER
      *
      *=================================================================
       01  DISPLAY-BUF                  VALUE SPACES.
           03  COMPANY-INFORMATION.
               05  F01                  PIC 9(4).
               05  F011                 PIC ZZ/ZZ/ZZ.
               05  F012                 PIC 9(6).
               05  F013                 PIC X(10).
               05  F02                  PIC 9(4).
               05  F021                 PIC ZZ/ZZ/ZZ.
               05  F022                 PIC 9(6).
               05  F023                 PIC X(10).
               05  F03                  PIC 9(4).
               05  F031                 PIC ZZ/ZZ/ZZ.
               05  F032                 PIC 9(6).
               05  F033                 PIC X(10).

       01  DISPLAY-LIST.
           03  FILLER                   PIC X(48) VALUE
           "010 011 012 013 020 021 022 023 030 031 032 033.".


      *=================================================================
      *
      *    ELEMENT SPECIFICATIONS
      *
      *=================================================================
       01  MAX                                PIC 99   VALUE 3.   
       01  SPEC-TABLE.
      *01. END OF MONTH MMYY INPUT
           03  FILLER                         PIC X(8) VALUE "ENNN040N".
      *02. END OF YEAR FISCAL MMYY
           03  FILLER                         PIC X(8) VALUE "ENNN040N".
      *03. END OF YEAR CALENDAR  MMYY
           03  FILLER                         PIC X(8) VALUE "ENNN040N".

      *-----------------------------------------------------------------
       01  SPEC-TABLE2              REDEFINES SPEC-TABLE.
           03  SPEC-REC                       OCCURS 3 TIMES.
               05  SPEC                       PIC X(7).
               05  CHFG                       PIC X.



      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/TIMEWK.CPY".
      *01  GBMAIN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/GBMAN2_DEF.CPY".
       COPY "LIBGB/GBMAN2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBGB/GBMAN2_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E    D I V I S I O N
      *
      ******************************************************************

       PROCEDURE DIVISION
              USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GBMAN2" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO GBMNAL-LINK.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TODAYS-DATE.

           MOVE "LOGNAME" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO SAVE-LOGNAME.

       MAIN-MODULE.

      D    STOP MESS.
           PERFORM FORM-RESET.

           PERFORM REC-GB-MODULE.
           IF ( IO-TYPE = "END" )
              MOVE "X"  TO ERRCD
              GO TO END-ROUTINE.
           
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF ( IO-TYPE = "RWR" )
              PERFORM WRITE-MODULE.

           IF NEW-REC = "Y"
              MOVE "N" TO NEW-REC
              MOVE "P" TO FORM-P
              MOVE 1 TO FORM-SEL.

           IF FORM-SEL NOT = 0
              IF FORM-P = "P"
                 MOVE GB-KEY TO SAVEKEY
                 MOVE FORM-SEL TO FORMNUM
                 GO TO END-ROUTINE.

           MOVE "N" TO NEW-REC.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-GB-MODULE SECTION.
      ******************************************************************
       BEGIN-REC-GB.              
           MOVE 0 TO GB-BRNO GB-NO.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO GB-KEY 
              MOVE SAVE-BRNO TO BR-NO
              MOVE SPACES TO SAVEKEY
              MOVE "S  A040BRNO." TO VDU
              MOVE BR-NO  TO VDUBUF
              PERFORM SCREENIO
              PERFORM LOAD-BRANCH-MODULE
              IF BR-PATH = SPACES
                  MOVE "INVALID BRANCH" TO MESS
                  PERFORM SEND-MESS
                  GO TO ENTER-BRANCH
              END-IF
              GO TO READ-GLOBAL. 

            MOVE 0 TO NEW-FLAG.

       ENTER-BRANCH.
           MOVE "EN N040BRNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY 
              IF IO-TYPE = "CLR"
                 GO TO BEGIN-REC-GB
              ELSE
                 MOVE BR-NO TO VDUNUM0.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GB-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-BRANCH.

           IF VDUNUM0 = 0
              GO TO ENTER-BRANCH.

           MOVE VDUNUM0 TO BR-NO.
           PERFORM LOAD-BRANCH-MODULE.
       READ-GLOBAL.
           MOVE  1 TO GB-NO.
           PERFORM LOAD-BR-FILE.

           PERFORM OPEN-GB-FILE.
          
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO GB-PATH-OVERRIDE. 

           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
               MOVE "CLR" TO IO-TYPE
               MOVE "NO GLOBAL RECORD FOR THIS BRANCH" TO MESS
               PERFORM SEND-MESS 
               GO TO BEGIN-REC-GB.              

           MOVE GB-REC TO GBOR-REC.
           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.
           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG.
       END-REC-GB-MODULE.

       EXIT-REC-GB-MODULE.
           EXIT.

      ****************************************
       LOAD-BRANCH-MODULE SECTION.
      ****************************************
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
                MOVE SPACES                  TO BR-PATH
                GO TO LOAD-BRANCH-MODULE-EXIT.
 
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.
 
            MOVE "B"                        TO GB-PATH-OVERRIDE.
 
       LOAD-BRANCH-MODULE-EXIT.
            EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.

           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.

       CREATE-ROUTINE.

           IF ( NEW-FLAG NOT = 1 )
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE < 1 )
              MOVE 1 TO ELE ONE.

           IF ( ELE NOT > MAX )
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY ) AND
                (NEW-FLAG = 0)
              PERFORM CHECK-EOM-EOY-DATES
              IF CHECK-DATE-FG NOT = SPACES
                   GO TO MODIFY-ROUTINE
              ELSE
                   MOVE "CLR" TO IO-TYPE
                   GO TO END-ENTRY-MODULE.

           IF ( NOT ENTER-KEY )
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ACTION = SPACES 
              PERFORM CHECK-EOM-EOY-DATES
              IF CHECK-DATE-FG NOT = SPACES
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE.

           IF FORM-KEY = " " AND FORM-P = "P" AND FORM-SEL NUMERIC
              PERFORM CHECK-EOM-EOY-DATES
              IF CHECK-DATE-FG NOT = SPACES
                 GO TO MODIFY-ROUTINE
              END-IF
      *FUTURE IF ( FORM-SEL < 1 OR FORM-SEL = 2 OR FORM-SEL > ?? )
              IF ( FORM-SEL NOT = 1 )
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.

           IF ( CHNO NOT NUMERIC )
              GO TO MODIFY-ROUTINE.

           IF ( CHNO = ZEROES )
              GO TO MODIFY-ROUTINE.

           IF ( CHNO > MAX )
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       ENTER-ELE.
      * ONLY ALLOW ENTRY TO 1,2 AND 3 IF NEW-FLAG = 1 OR 2
           IF ( NEW-FLAG NOT = 0 )
              MOVE SPACES TO CHFG(1) CHFG(2) CHFG(3)
           ELSE
              MOVE "N"    TO CHFG(1) CHFG(2) CHFG(3).

           IF ( CHKEY NOT = ZEROES )
              IF ( CHFG (ELE) = SPACES )
                 IF ( NEW-FLAG NOT = 1 )
                    GO TO MODIFY-ROUTINE.

           IF ( CHFG(ELE) = "N" )
              IF ( NEW-FLAG = 1 )
                 GO TO CREATE-ROUTINE
            ELSE
              IF ( CHKEY NOT = EXT-CONAME-DAILY-PASSWORD )
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE ZEROES    TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       TEST-F1.
           IF ( F1-KEY ) AND
              ( NEW-FLAG = 0 )
              PERFORM CHECK-EOM-EOY-DATES
              IF CHECK-DATE-FG NOT = SPACES
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "CLR" TO IO-TYPE
                 MOVE "N"   TO NEW-REC
                 GO TO END-ENTRY-MODULE.



      *-----------------------------------------------------------------
       TEST-UP-KEY.

           IF ( UP-KEY )
              IF ( NEW-FLAG = 1 )
                 MOVE -1 TO ONE
                 GO TO GOTO-FIELD.

      *-----------------------------------------------------------------
       TEST-DOWN-KEY.

           IF ( DOWN-KEY )
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

      *-----------------------------------------------------------------
       TEST-ENTER-KEY.

           IF ( NOT ENTER-KEY )
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD.
           GO TO F-01 F-02 F-03
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
       F-01.
      *EOMALL SHOULD UPDATE THIS DATE
           PERFORM DATE-CHECK-MMYY.
           IF BAD-DATE
                 GO TO ENTER-ELE.
           IF VDUNUM0 = 0
                 GO TO ENTER-ELE.

           MOVE VDUNUM0 TO DATE-MMYY.
           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM    TO GB-EOM-TAG-CCYYMM.
           MOVE "S  8080011." TO VDU
           MOVE TODAYS-DATE TO GB-EOM-UPDATE-DATE
                                   VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           PERFORM GET-TIME.
           MOVE "SNNN060012." TO VDU.
           MOVE HH-MM-SS    TO GB-EOM-UPDATE-TIME VDUNUM0.

           PERFORM SCREENIO.

           MOVE "S  A100013." TO VDU.
           MOVE SAVE-LOGNAME TO GB-EOM-UPDATE-USERID
                                         VDUBUF.
           PERFORM SCREENIO.


           GO TO CREATE-ROUTINE.


       F-02.
      *EOY FISCAL WILL UPDATE
           PERFORM DATE-CHECK-MMYY.
           IF BAD-DATE
                 GO TO ENTER-ELE.

           IF VDUNUM0 = 0
                 GO TO ENTER-ELE.
           MOVE VDUNUM0 TO DATE-MMYY.
           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM    TO GB-FISCAL-EOY-TAG-CCYYMM.

           MOVE "S  8080021." TO VDU.
           MOVE TODAYS-DATE TO GB-EOYF-UPDATE-DATE 
                                  VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           PERFORM GET-TIME.
           MOVE "SNNN060022." TO VDU.
           MOVE HH-MM-SS    TO GB-EOYF-UPDATE-TIME
                                          VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A100023." TO VDU.
           MOVE SAVE-LOGNAME TO GB-EOYF-UPDATE-USERID
                                           VDUBUF.
           PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.
           
       F-03.
      *END OF YEAR CALENDAR WILL UPDATE
           PERFORM DATE-CHECK-MMYY.
           IF BAD-DATE
              GO TO ENTER-ELE.

           IF VDUNUM0 = 0
                 GO TO ENTER-ELE.
           MOVE VDUNUM0 TO DATE-MMYY.
           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM    TO GB-CALENDAR-EOY-TAG-CCYYMM.
           MOVE "S  8080031." TO VDU
           MOVE TODAYS-DATE TO GB-EOYC-UPDATE-DATE
                                   VDU-DATE-YYYYMMDD
           PERFORM SCREENIO.
           PERFORM GET-TIME.
           MOVE "SNNN060032." TO VDU.
           MOVE HH-MM-SS    TO GB-EOYC-UPDATE-TIME
                                          VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A100033." TO VDU.
           MOVE SAVE-LOGNAME TO GB-EOYC-UPDATE-USERID
                                           VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL.
       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       DATE-CHECK-MMYY SECTION.
      ******************************************************************

           MOVE SPACES TO DATE-SW.
           MOVE VDU-NUM TO WS-DATE.

           IF ( WS-MM = ZEROES ) AND ( WS-DD = ZEROES )
              GO TO DATE-CHECK-MMYY-EXIT.

           IF ( WS-MM < 1 ) OR ( WS-MM > 12 )
              MOVE "BAD MONTH" TO MESS
              PERFORM SEND-MESS
              MOVE "BAD" TO DATE-SW.

       DATE-CHECK-MMYY-EXIT.
           EXIT.

      ********************************************************
       CHECK-EOM-EOY-DATES SECTION.
      *****************************************************
           MOVE SPACES TO CHECK-DATE-FG.
           IF GB-EOM-TAG-CCYYMM = 0 OR
               GB-FISCAL-EOY-TAG-CCYYMM = 0 OR
                   GB-CALENDAR-EOY-TAG-CCYYMM = 0
                      MOVE
                 "EOM MMYY DATE, EOY FISCAL MMYY,EOY CALENDAR MMYY"
                                       TO MESS
                     PERFORM SEND-MESS
                     MOVE
                 "CAN NOT BE 0" TO MESS
                     PERFORM SEND-MESS
                 MOVE "X" TO CHECK-DATE-FG.
       CHECK-EOM-EOY-DATES-EXIT.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-GB-FILE.

           MOVE GB-REC TO GBCH-REC.

           IF GBOR-REC = GBCH-REC
              GO TO END-WRITE-MODULE.

           MOVE 1 TO GB-NO.
           PERFORM READ-GB-FILE.
           IF ( IO-BAD )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF ( GB-REC NOT = GBOR-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE GBCH-REC TO GB-REC.
           PERFORM REWRITE-GB-FILE.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-GB-FILE.
           IF ( IO-BAD )
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-GB-FILE.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE SPACES                          TO LMC-DETAIL.
           MOVE GB-BRNO                         TO LMC-BRNO.
           MOVE " TO "                          TO LMC-TO-X.

           IF GBOR-EOM-TAG-CCYYMM NOT = GBCH-EOM-TAG-CCYYMM
              MOVE "01 "                        TO LMC-FIELD
              MOVE "EOM MMYY DATE"              TO LMC-DESC
              MOVE GBOR-EOM-TAG-CCYYMM          TO LMC-ORG-NUM
              MOVE GBCH-EOM-TAG-CCYYMM          TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

           IF GBOR-EOM-UPDATE-DATE NOT = GBCH-EOM-UPDATE-DATE
              MOVE "01 "                        TO LMC-FIELD
              MOVE "EOM MMYY DATE (DATE)"       TO LMC-DESC
              MOVE GBOR-EOM-UPDATE-DATE         TO LMC-ORG-NUM
              MOVE GBCH-EOM-UPDATE-DATE         TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

           IF GBOR-EOM-UPDATE-TIME NOT = GBCH-EOM-UPDATE-TIME
              MOVE "01 "                        TO LMC-FIELD
              MOVE "EOM MMYY DATE (TIME)"       TO LMC-DESC
              MOVE GBOR-EOM-UPDATE-TIME         TO LMC-ORG-NUM
              MOVE GBCH-EOM-UPDATE-TIME         TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

           IF GBOR-EOM-UPDATE-USERID NOT = GBCH-EOM-UPDATE-USERID
              MOVE "01 "                        TO LMC-FIELD
              MOVE "EOM MMYY DATE (USERID)"     TO LMC-DESC
              MOVE GBOR-EOM-UPDATE-USERID       TO LMC-ORG-NUM
              MOVE GBCH-EOM-UPDATE-USERID       TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

           IF GBOR-FISCAL-EOY-TAG-CCYYMM NOT =
              GBCH-FISCAL-EOY-TAG-CCYYMM
              MOVE "02 "                        TO LMC-FIELD
              MOVE "EOY FISCAL MMYY"            TO LMC-DESC
              MOVE GBOR-FISCAL-EOY-TAG-CCYYMM   TO LMC-ORG-NUM
              MOVE GBCH-FISCAL-EOY-TAG-CCYYMM   TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

           IF GBOR-EOYF-UPDATE-DATE NOT = GBCH-EOYF-UPDATE-DATE
              MOVE "02 "                        TO LMC-FIELD
              MOVE "EOM FISCAL DATE (DATE)"     TO LMC-DESC
              MOVE GBOR-EOYF-UPDATE-DATE        TO LMC-ORG-NUM
              MOVE GBCH-EOYF-UPDATE-DATE        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

           IF GBOR-EOYF-UPDATE-TIME NOT = GBCH-EOYF-UPDATE-TIME
              MOVE "02 "                        TO LMC-FIELD
              MOVE "EOM FISCAL DATE (TIME)"     TO LMC-DESC
              MOVE GBOR-EOYF-UPDATE-TIME        TO LMC-ORG-NUM
              MOVE GBCH-EOYF-UPDATE-TIME        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

           IF GBOR-EOYF-UPDATE-USERID NOT = GBCH-EOYF-UPDATE-USERID
              MOVE "02 "                        TO LMC-FIELD
              MOVE "EOM FISCAL DATE (USERID)"   TO LMC-DESC
              MOVE GBOR-EOYF-UPDATE-USERID      TO LMC-ORG-NUM
              MOVE GBCH-EOYF-UPDATE-USERID      TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

           IF GBOR-CALENDAR-EOY-TAG-CCYYMM NOT =
              GBCH-CALENDAR-EOY-TAG-CCYYMM
              MOVE "03 "                        TO LMC-FIELD
              MOVE "EOY CALENDAR MMYY"          TO LMC-DESC
              MOVE GBOR-CALENDAR-EOY-TAG-CCYYMM TO LMC-ORG-NUM
              MOVE GBCH-CALENDAR-EOY-TAG-CCYYMM TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

           IF GBOR-EOYC-UPDATE-DATE NOT = GBCH-EOYC-UPDATE-DATE
              MOVE "03 "                        TO LMC-FIELD
              MOVE "EOM CALENDAR DATE (DATE)"   TO LMC-DESC
              MOVE GBOR-EOYC-UPDATE-DATE        TO LMC-ORG-NUM
              MOVE GBCH-EOYC-UPDATE-DATE        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

           IF GBOR-EOYC-UPDATE-TIME NOT = GBCH-EOYC-UPDATE-TIME
              MOVE "03 "                        TO LMC-FIELD
              MOVE "EOM CALENDAR DATE (TIME)"   TO LMC-DESC
              MOVE GBOR-EOYC-UPDATE-TIME        TO LMC-ORG-NUM
              MOVE GBCH-EOYC-UPDATE-TIME        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

           IF GBOR-EOYC-UPDATE-USERID NOT = GBCH-EOYC-UPDATE-USERID
              MOVE "03 "                        TO LMC-FIELD
              MOVE "EOM CALENDAR DATE (USERID)" TO LMC-DESC
              MOVE GBOR-EOYC-UPDATE-USERID      TO LMC-ORG-NUM
              MOVE GBCH-EOYC-UPDATE-USERID      TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.

      ******************************************************************
       LOG-FIELD-CHANGE SECTION.
      ******************************************************************

           MOVE "F"           TO ERRLOGW-ACTION.
           MOVE LMC-DETAIL    TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       LOG-FIELD-CHANGE-EXIT.
           EXIT.


      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES                TO DISPLAY-BUF.
         
           MOVE GB-EOM-TAG-CCYYMM   TO DATE-YYYYMM
           PERFORM CONVERT-YYYYMM-TO-MMDDYY
           PERFORM CONVERT-MMDDYY-TO-MMYY
           MOVE DATE-MMYY             TO F01.

           MOVE GB-EOM-UPDATE-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO F011.

           MOVE GB-EOM-UPDATE-TIME TO F012.
           MOVE GB-EOM-UPDATE-USERID  TO F013.

           MOVE GB-FISCAL-EOY-TAG-CCYYMM TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY                TO F02.
           MOVE GB-EOYF-UPDATE-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO F021.
           MOVE GB-EOYF-UPDATE-TIME      TO F022.
           MOVE GB-EOYF-UPDATE-USERID    TO F023.

           MOVE GB-CALENDAR-EOY-TAG-CCYYMM TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY                 TO F03.
           MOVE GB-EOYC-UPDATE-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY          TO    F031.
           MOVE GB-EOYC-UPDATE-TIME        TO F032.
           MOVE GB-EOYC-UPDATE-USERID      TO F033.

           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

       EXIT-DISPLAY-FIELDS.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************

           MOVE SPACES TO IO-TYPE.
           MOVE " ENTER BRANCH # OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-BR-FILE.
           MOVE 1 TO BR-NO.
           PERFORM START-BR-FILE.

       ENTER-KEY.
           MOVE "EN N040SCAN." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCAN.

           IF F6-KEY
              GO TO READ-FILES-NEXT.

           IF NOT ENTER-KEY
              GO TO ENTER-KEY.

           IF VDUNUM0 = 0
              GO TO CLOSE-SCAN.

           MOVE VDUNUM0 TO BR-NO.
           PERFORM START-BR-FILE.

      *----------------------------------------------------------------
       READ-FILES-NEXT.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCAN.

           MOVE "SN N040BRNO." TO VDU.
           MOVE BR-NO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO GB-PATH-OVERRIDE.

           PERFORM OPEN-GB-FILE.
           MOVE BR-NO TO GB-BRNO.
           MOVE 1     TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-GOOD
              PERFORM DISPLAY-FIELDS
           ELSE
              MOVE SPACES TO DISPLAY-BUF
      *       MOVE "NO GLOBAL FOR THIS BRANCH!!" TO F01
              MOVE DISPLAY-LIST TO WR-LIST
              MOVE DISPLAY-BUF TO WR-BUF
              PERFORM CALL-WRFORM 
              MOVE "NO GLOBAL FOR THIS BRANCH!!" TO MESS
              PERFORM SEND-MESS.

           GO TO ENTER-KEY.

      *----------------------------------------------------------------
       CLOSE-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
           PERFORM CLOSE-BR-FILE.

       END-SCAN.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO GBMAN2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GBMAN2-SYSDATE.
           DISPLAY GBMAN2-SCREEN UPON GBMAIN-WINDOW-HANDLE.
           MOVE GBMAN2-FORM-FIELDS TO WR-BUF.
           MOVE GBMAN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE GBMAN2-HELP--FILE TO HELP-LINK-FILE.
           MOVE GBMAN2-HELP--DIR TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/GBMAN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/PHONEIO.CPY".
      *-----------------------------------------------------------------
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBIN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GBMAN2-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE GBMNAL-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
