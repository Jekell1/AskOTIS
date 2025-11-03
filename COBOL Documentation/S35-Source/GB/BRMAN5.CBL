       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BRMAN5.
      *******************************************************************
      *  DIR :   GB
      *  DESC:   BRANCH FILE MAINTENANCE PAGE 5
      *  REV :
      *  MJD 112197 EXPANDED BR-REC TO 850 BYTES
      *  KEC 121797 ADDED PAGE 7.
      *  MJD 031398 ADDED 78 LEVEL: BR-SIZE
      *  BAH 990413 ADDED CALL TO CREATE CHANGES FILE AFTER REWRITE
      *  MG  991005 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BLV 031120 IF I-AM-WORLD, DON'T ENTER OR DISPLAY BR-STMNT2 -
      *             THEY USE REDEF OF FIELD ENTERED IN BRMAN7, FIELD 31
      *  BAH 040116 ADDED NEW BRANCH TRAILER, B2FILE, REGACC PR# 47
      *             BR-COLLECT-MANAGER, PHONE AND EXT
      *   CS 061211 ADDED BR-SUPERVISOR, BR-AREA-MANAGER PR#159 REGACC
      *  BAH 070220 ADDED BR-CC FIELDS FOR COLLECTION CENTER INFO
      *                                                 REGACC PR# 164
      *  AMM 070308 CREATED A LOG FIELD CHANGE ROUTINE
      *  BAH 080625 IF I-AM-WORLD BUT IN "MX", ALLOW STMNT2 TO BE ENTERED
      *             NEEDED FOR MEXICO STATEMENT PRINT, JIMR
      *  BAH 120514 REMOVED SPECIAL-FG
      *  BAH 140911 FIXED PHONE ENTRY
      *  BAH 151021 REMOVED B2FILE
      *  BAH 20211116 LIMIT ACCESS BY PASSWORD TO ALL FIELDS, #1524
      *  BAH 20220921 ADDED 9. VERITEC LICENSE, BR-VERITEC-LICENSE #1570
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
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)GB/BRMAN5 S35 10/18/22-10:39:00 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

      * ORIGINAL LOAN RECORD
       COPY "LIBGB/GB01BR.CPY" REPLACING LEADING "BR" BY "BROR".
      * CHANGED LOAN RECORD
       COPY "LIBGB/GB01BR.CPY" REPLACING LEADING "BR" BY "BRCH".

      ******************************************************************
      *
      *    L M C - W O R K E R S
      *
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

      ******************************************************************

       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  FORM-NUM REDEFINES ACTION.
           03  FORM-KEY    PIC X(4).
           03  FORM-P      PIC X.
           03  FORM-SEL    PIC 9.

       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

       01  ORIGINAL-BR-REC    PIC X(BR-SIZE).
       01  CHANGED-BR-REC     PIC X(BR-SIZE).

       01  UPDATE-BR-STAT            PIC X(02) VALUE SPACES.
           88  UPDATE-BR-STAT-GOOD             VALUE "00".
           88  UPDATE-BR-STAT-BAD              VALUE "  " THRU "//"
                                                     "01" THRU "~~".

       01  BRMAIN-LINK       VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(4).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMNUM       PIC 9.
           03  BRMAIN-WINDOW-HANDLE PIC X(10).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF          VALUE SPACES.
           03  F01              PIC X(30).
           03  F011             PIC X(30).
           03  F012             PIC X(30).
           03  F02              PIC X(30).
           03  F021             PIC X(30).
           03  F03              PIC X(60).
           03  F031             PIC X(60).
           03  F04              PIC X(25).
           03  F05              PIC X(12).
           03  F051             PIC Z(4).
           03  F06              PIC X(25).
           03  F07              PIC X(25).
           03  F08              PIC X.
           03  F081             PIC X(24).
           03  F082             PIC X(15).
           03  F083             PIC X(2).
           03  F084             PIC X(10).
           03  F09              PIC 9(7).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(48) VALUE
           "010 011 012 020 021 030 031 040 050 051 060 070 ".
           03  FILLER      PIC X(24) VALUE
           "080 081 082 083 084 090.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS #1
      *    **********************************
       01  MAX            PIC 99   VALUE 9.
       01  SPEC-TABLE.
      * 1. REMIT ADDRESS
           03  FILLER     PIC X(8) VALUE "E  A300 ".
      * 2. RECEIPT MESSAGE
           03  FILLER     PIC X(8) VALUE "E  A300 ".
      * 3. STATEMENT MESSAGE
           03  FILLER     PIC X(8) VALUE "E  A600 ".
      * 4. COLLECTION MANAGER
           03  FILLER     PIC X(8) VALUE "E  A250 ".
      * 5. PHONE
           03  FILLER     PIC X(8) VALUE "E  A120 ".
      * 6. SUPERVISOR NAME
           03  FILLER     PIC X(8) VALUE "E  A250 ".
      * 7. AREA MANAGER NAME
           03  FILLER     PIC X(8) VALUE "E  A250 ".
      * 8. COLLECTION CENTER INFO
           03  FILLER     PIC X(8) VALUE "E  A010 ".
      * 9. VERITEC LICENSE
           03  FILLER     PIC X(8) VALUE "EN N070 ".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 9 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  FIELDS-SEQ                VALUE 0.
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/BRMAN5_DEF.CPY".
       COPY "LIBGB/BRMAN5_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBGB/BRMAN5_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ***************************************
       MAIN-PROGRAM SECTION.
      ***************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE FORM-LINK-AREA TO BRMAIN-LINK.
           MOVE "BRMAN5" TO VDU-FORM-NAME.

       MAIN-MODULE.
           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           PERFORM FORM-RESET.

           PERFORM REC-BR-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P
              MOVE 6 TO FORM-SEL.

           IF FORM-SEL NOT = 0
              IF FORM-P = "P"
                 MOVE BR-NO TO SAVEKEY
                 MOVE FORM-SEL TO FORMNUM
                 GO TO END-ROUTINE.

           MOVE "N" TO NEW-REC.
           GO TO MAIN-MODULE.

      *******************************
       REC-BR-MODULE SECTION.
      *******************************
       REC-BR-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO BR-NO
              MOVE " " TO SAVEKEY
              MOVE "SN N040BRANCH." TO VDU
              MOVE BR-NO TO VDUNUM0
              PERFORM SCREENIO
              GO TO READ-BRANCH.

           MOVE 0 TO NEW-FLAG.
           MOVE "EN N040BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BR-MODULE.
           IF NOT ENTER-KEY
              GO TO REC-BR-MODULE-NO.
           IF VDUNUM0 = 0
              GO TO REC-BR-MODULE-NO.
           MOVE VDUNUM0 TO BR-NO.
       READ-BRANCH.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              GO TO REC-BR-MODULE-NO.

           MOVE BR-REC TO ORIGINAL-BR-REC.
           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.
           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG.
       END-REC-BR-MODULE.
           EXIT.

      *******************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      *******************************
       ENTRY-MODULE SECTION.

       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF NOT ENTER-KEY
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF FORM-KEY = " " AND FORM-P = "P" AND FORM-SEL NUMERIC
              IF NOT (FORM-SEL = 1 OR 2 OR 3 OR 4 OR 6 OR 7)
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.

      * LIMIT ACCESS TO ALL FIELDS 
           IF NEW-FLAG = 0
              MOVE FIELDS-SEQ                    TO PASSWDW-SEQ
              MOVE "TO CHANGE THIS FIELD......." TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.

       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF UP-KEY
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF DOWN-KEY
              MOVE 1 TO ONE
              GO TO GOTO-FIELD1.
       TEST-F1.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF NOT ENTER-KEY
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD1.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08
                 F-09
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUBUF TO BR-REMIT1.
       F-011.
           MOVE "E  A300011." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT ENTER-KEY
              GO TO F-011.
           MOVE VDUBUF TO BR-REMIT2.
       F-012.
           MOVE "E  A300012." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO F-011.
           IF NOT ENTER-KEY
              GO TO F-012.
           MOVE VDUBUF TO BR-REMIT3.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO BR-RECP1.
       F-021.
           MOVE "E  A600021." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT ENTER-KEY
              GO TO F-021.
           MOVE VDUBUF TO BR-RECP2.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO BR-STMNT1.
       F-031.
           IF BR-STATE NOT = "MX"
              GO TO CREATE-ROUTINE.
           MOVE "E  A600031." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT ENTER-KEY
              GO TO F-031.
           MOVE VDUBUF TO BR-STMNT2.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO BR-COLLECT-MANAGER.
           GO TO CREATE-ROUTINE.
       F-05.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO BR-COLLECT-PHONE.
           PERFORM SEND-PHONE.
           MOVE "S  A120050." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
       F-051.
           MOVE "ENNN040051." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT ENTER-KEY
              GO TO F-051.
           MOVE VDUNUM0 TO BR-COLLECT-EXT.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO BR-SUPERVISOR.
           GO TO CREATE-ROUTINE.

       F-07.
           MOVE VDUBUF TO BR-AREA-MANAGER.
           GO TO CREATE-ROUTINE.

       F-08.
           MOVE VDUBUF TO BR-CC-FLAG.
           IF BR-CC-FLAG NOT = "Y" AND NOT = "N"
              GO TO ENTER-ELE.
       F-081.
           MOVE "E  A240081." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT ENTER-KEY
              GO TO F-081.
           MOVE VDUBUF TO BR-CC-ADR1.
       F-082.
           MOVE "E  A150082." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO F-081.
           IF NOT ENTER-KEY
              GO TO F-082.
           MOVE VDUBUF TO BR-CC-CITY.
       F-083.
           MOVE "E  A020083." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO F-082.
           IF NOT ENTER-KEY
              GO TO F-083.
           MOVE VDUBUF TO BR-CC-STATE.
       F-084.
           MOVE "E  A100084." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO F-083.
           IF NOT ENTER-KEY
              GO TO F-084.
           MOVE VDUBUF TO BR-CC-ZIP.

           GO TO CREATE-ROUTINE.

       F-09.
           MOVE VDUNUM0 TO BR-VERITEC-LICENSE.
           GO TO CREATE-ROUTINE.


       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL.
       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " "                          TO PASSWDW-STAT.
           MOVE "BRMAN5"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       EXIT-VERIFY-PASSWORD.
           EXIT.

      *****************************************
       WRITE-MODULE SECTION.
      *****************************************

           MOVE STAT---BAD TO UPDATE-BR-STAT.

       REWRITE-RECORD.
           PERFORM OPEN-BR-FILE.
           MOVE BR-REC TO CHANGED-BR-REC.
           IF ORIGINAL-BR-REC = CHANGED-BR-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF BR-REC NOT = ORIGINAL-BR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-BR-REC TO BR-REC.
           PERFORM REWRITE-BR-FILE.
           MOVE STAT---GOOD TO UPDATE-BR-STAT.

      * CODE TO WRITE CHANGE FILE.
      *    MOVE "C" TO ERRLOGW-ACTION.
      *    MOVE BR-NO TO ERRLOGW-KEY.
      *    MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
      *    MOVE "CL/ERRLOG" TO FORM-NAM.
      *    CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
      *    CANCEL FORM-PROGX.

       END-WRITE-MODULE.
           PERFORM CLOSE-BR-FILE.

           IF ( UPDATE-BR-STAT-GOOD )
              PERFORM LOG-MAINTENANCE-CHGS.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE ORIGINAL-BR-REC               TO BROR-REC.
           MOVE CHANGED-BR-REC                TO BRCH-REC.

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE BR-NO                      TO LMC-BRNO.
           MOVE " TO "                     TO LMC-TO-X.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BROR-REMIT1 NOT = BRCH-REMIT1
             MOVE "01 "                    TO LMC-FIELD
             MOVE "REMIT NAM"              TO LMC-DESC
             MOVE BROR-REMIT1              TO LMC-ORG-TEXT
             MOVE BRCH-REMIT1              TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-REMIT2 NOT = BRCH-REMIT2
             MOVE "011"                    TO LMC-FIELD
             MOVE "REMIT ADR"              TO LMC-DESC
             MOVE BROR-REMIT2              TO LMC-ORG-TEXT
             MOVE BRCH-REMIT2              TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-REMIT3 NOT = BRCH-REMIT3
             MOVE "012"                    TO LMC-FIELD
             MOVE "REMIT CSZ"              TO LMC-DESC
             MOVE BROR-REMIT3              TO LMC-ORG-TEXT
             MOVE BRCH-REMIT3              TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-RECP1 NOT = BRCH-RECP1
             MOVE "02 "                    TO LMC-FIELD
             MOVE "REC MESS1"              TO LMC-DESC
             MOVE BROR-RECP1               TO LMC-ORG-TEXT
             MOVE BRCH-RECP1               TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-RECP2 NOT = BRCH-RECP2
             MOVE "021"                    TO LMC-FIELD
             MOVE "REC MESS2"              TO LMC-DESC
             MOVE BROR-RECP2               TO LMC-ORG-TEXT
             MOVE BRCH-RECP2               TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-STMNT1 NOT = BRCH-STMNT1
             MOVE "03 "                    TO LMC-FIELD
             MOVE "STM MESS1"              TO LMC-DESC
             MOVE BROR-STMNT1              TO LMC-ORG-TEXT
             MOVE BRCH-STMNT1              TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-STMNT2 NOT = BRCH-STMNT2
             MOVE "031"                    TO LMC-FIELD
             MOVE "STM MESS2"              TO LMC-DESC
             MOVE BROR-STMNT2              TO LMC-ORG-TEXT
             MOVE BRCH-STMNT2              TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-COLLECT-MANAGER NOT = BRCH-COLLECT-MANAGER
             MOVE "04 "                    TO LMC-FIELD
             MOVE "COLL MNGR"              TO LMC-DESC
             MOVE BROR-COLLECT-MANAGER     TO LMC-ORG-TEXT
             MOVE BRCH-COLLECT-MANAGER     TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-COLLECT-PHONE NOT = BRCH-COLLECT-PHONE
             MOVE "05 "                    TO LMC-FIELD
             MOVE "COL PHONE"              TO LMC-DESC
             MOVE BROR-COLLECT-PHONE       TO LMC-ORG-PHONE
             INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
             MOVE BRCH-COLLECT-PHONE       TO LMC-CHG-PHONE
             INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-COLLECT-EXT NOT = BRCH-COLLECT-EXT
             MOVE "051"                    TO LMC-FIELD
             MOVE "COLL EXT "              TO LMC-DESC
             MOVE BROR-COLLECT-EXT         TO LMC-ORG-NUM
             MOVE BRCH-COLLECT-EXT         TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-SUPERVISOR NOT = BRCH-SUPERVISOR
             MOVE "06 "                    TO LMC-FIELD
             MOVE "SUPERVISR"              TO LMC-DESC
             MOVE BROR-SUPERVISOR          TO LMC-ORG-TEXT
             MOVE BRCH-SUPERVISOR          TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-AREA-MANAGER NOT = BRCH-AREA-MANAGER
             MOVE "07 "                    TO LMC-FIELD
             MOVE "MANAGER  "              TO LMC-DESC
             MOVE BROR-AREA-MANAGER        TO LMC-ORG-TEXT
             MOVE BRCH-AREA-MANAGER        TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-CC-FLAG NOT = BRCH-CC-FLAG
             MOVE "08 "                    TO LMC-FIELD
             MOVE "CC FLAG  "              TO LMC-DESC
             MOVE BROR-CC-FLAG             TO LMC-ORG-TEXT
             MOVE BRCH-CC-FLAG             TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-CC-ADR1 NOT = BRCH-CC-ADR1
             MOVE "081"                    TO LMC-FIELD
             MOVE "CC ADDRES"              TO LMC-DESC
             MOVE BROR-CC-ADR1             TO LMC-ORG-TEXT
             MOVE BRCH-CC-ADR1             TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-CC-CITY NOT = BRCH-CC-CITY
             MOVE "082"                    TO LMC-FIELD
             MOVE "CC CITY  "              TO LMC-DESC
             MOVE BROR-CC-CITY             TO LMC-ORG-TEXT
             MOVE BRCH-CC-CITY             TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-CC-STATE NOT = BRCH-CC-STATE
             MOVE "083"                    TO LMC-FIELD
             MOVE "CC STATE "              TO LMC-DESC
             MOVE BROR-CC-STATE            TO LMC-ORG-TEXT
             MOVE BRCH-CC-STATE            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-CC-ZIP NOT = BRCH-CC-ZIP
             MOVE "084"                    TO LMC-FIELD
             MOVE "CC ZIP   "              TO LMC-DESC
             MOVE BROR-CC-ZIP              TO LMC-ORG-TEXT
             MOVE BRCH-CC-ZIP              TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-VERITEC-LICENSE NOT = BRCH-VERITEC-LICENSE
             MOVE "09 "                    TO LMC-FIELD
             MOVE "VERITEC LICENSE"        TO LMC-DESC
             MOVE BROR-VERITEC-LICENSE     TO LMC-ORG-TEXT
             MOVE BRCH-VERITEC-LICENSE     TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

       LOG-MAINTENANCE-CHGS-EXIT.
           EXIT.

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
           MOVE SPACES TO DISPLAY-BUF.
           MOVE BR-REMIT1 TO F01.
           MOVE BR-REMIT2 TO F011.
           MOVE BR-REMIT3 TO F012.
           MOVE BR-RECP1 TO F02.
           MOVE BR-RECP2 TO F021.
           MOVE BR-STMNT1 TO F03.
           IF BR-STATE NOT = "MX"
              MOVE SPACES    TO F031
           ELSE
              MOVE BR-STMNT2 TO F031.

           MOVE BR-COLLECT-MANAGER TO F04.
           MOVE BR-COLLECT-PHONE TO TEL-BUF.

           PERFORM SEND-PHONE.
           MOVE TEL-EDIT TO F05.
           MOVE BR-COLLECT-EXT TO F051.

           MOVE BR-SUPERVISOR   TO F06.
           MOVE BR-AREA-MANAGER TO F07.

           MOVE BR-CC-FLAG TO F08.
           MOVE BR-CC-ADR1 TO F081.
           MOVE BR-CC-CITY TO F082.
           MOVE BR-CC-STATE TO F083.
           MOVE BR-CC-ZIP TO F084.

           MOVE BR-VERITEC-LICENSE TO F09.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO BRMAN5-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BRMAN5-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BRMAN5-SCREEN.
           DISPLAY BRMAN5-SCREEN UPON BRMAIN-WINDOW-HANDLE.
           MOVE BRMAN5-FORM-FIELDS TO WR-BUF.
           MOVE BRMAN5-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE BRMAN5-HELP--FILE TO HELP-LINK-FILE.
           MOVE BRMAN5-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/BRMAN5_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************
       MISC-PARAGRAPHS SECTION.
      ************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/PHONEIO.CPY".
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      *******************************
       IO-ROUTINES SECTION.
      *******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRIN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *****************************
       END-ROUTINE SECTION.
      *****************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BRMAN5-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE BRMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
