       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BRMAN6.
      ******************************************************************
      *
      *  DIR :   GB
      *  DESC:   BRANCH FILE MAINTENANCE PAGE 6
      *  REV :
      *  BAH 111897  ADDED BR-CONVERT-AUTO-ACCTNO, FIELD WAS IN BR-REC
      *  MJD 112197 EXPANDED BR-REC TO 850 BYTES
      *  JTG 120197 ADDED WORLD BR-ADD-INS-FEECODE AD&D INSURANCE GA
      *                         BR-ROAD-G-SGL-FEECODE FOR VOYAGER
      *                         BR-ROAD-G-FAM-FEECODE
      *  KEC 121797 REVISED BACK TO ORIGINAL STATE PRIOR TO 07/22/97;
      *              ALL CHANGES MADE AFTER 07/22/97 WERE MOVED TO
      *              PAGE 7.
      *  MJD 031398 ADDED 78 LEVEL: BR-SIZE
      *  CS  111798 ADDED BR-ALPHA-DRAWERS TO FORCE CASH DRAWERS 1 - 9
      *  BAH 990413 ADDED CALL TO CREATE CHANGES FILE AFTER REWRITE
      *  MG  991005 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  AMM 070308 CREATED A LOG FIELD CHANGE ROUTINE
      *  BLM 100304 ADD FIELDS 13 & 14, BR-GL-ESCROW-BALANCE &
      *             BR-GL-ESCROW-PAYABLE, REGWAN PR# 3506
      *  BAH 120514 REMOVED SPECIAL-FG
      *  BAH 151021 REMOVED B2FILE
      *  BAH 20211116 LIMIT ACCESS BY PASSWORD TO ALL FIELDS EXCEPT
      *               FIELDS 1-10, #1524
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
      *      W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/BRMAN6.CBL S35 09/20/22-12:46:14 >".

      ******************************************************************
      *
      *      P R O G R A M     W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ORIGINAL LOAN RECORD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/GB01BR.CPY" REPLACING
            LEADING "BR" BY "BROR".
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHANGED LOAN RECORD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/GB01BR.CPY" REPLACING
            LEADING "BR" BY "BRCH".

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

       01  SUB             PIC 999     VALUE 0.
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

       01  WORK-GL         PIC 9(11).
       01  WORK-NUMBER REDEFINES WORK-GL.
           03  WORK-BR     PIC 9(4).
           03  WORK-NO     PIC 9(7).
      * USED FOR GL WINDOW SCAN
       01  GL-SCAN-ERR         PIC X                 VALUE "N".

       01  BRMAIN-LINK       VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(4).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMNUM       PIC 9.
           03  BRMAIN-WINDOW-HANDLE PIC X(10).

      ******************************************************************
      *    S C R E E N    D I S P L A Y   B U F F E R
      ******************************************************************
       01  DISPLAY-BUF          VALUE SPACES.
           03  F01              PIC X.
           03  DISPLAY-BALANCES OCCURS 9 TIMES.
               05  F02          PIC Z(6)9.99-.
           03  F11              PIC X(5).
           03  F111             PIC X(5).
           03  F112             PIC X(5).
           03  F120             PIC X.
           03  F130             PIC 9(4).
           03  F131             PIC 9(7).
           03  F132             PIC X(36).
           03  F140             PIC 9(4).
           03  F141             PIC 9(7).
           03  F142             PIC X(36).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(40) VALUE
           "110 111 112 120 130 131 132 140 141 142.".

      ******************************************************************
      *       ELEMENT SPECIFICATIONS #1
      ******************************************************************
       01  MAX            PIC 99   VALUE 14.
       01  SPEC-TABLE.
      * 1.  BR-AUTO-OPEN-BAL
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 2.  BR-OPEN-BAL(1)
           03  FILLER     PIC X(8) VALUE "ENNS072N".
      * 3.  BR-OPEN-BAL(2)
           03  FILLER     PIC X(8) VALUE "ENNS072N".
      * 4.  BR-OPEN-BAL(3)
           03  FILLER     PIC X(8) VALUE "ENNS072N".
      * 5.  BR-OPEN-BAL(4)
           03  FILLER     PIC X(8) VALUE "ENNS072N".
      * 6.  BR-OPEN-BAL(5)
           03  FILLER     PIC X(8) VALUE "ENNS072N".
      * 7.  BR-OPEN-BAL(6)
           03  FILLER     PIC X(8) VALUE "ENNS072N".
      * 8.  BR-OPEN-BAL(7)
           03  FILLER     PIC X(8) VALUE "ENNS072N".
      * 9.  BR-OPEN-BAL(8)
           03  FILLER     PIC X(8) VALUE "ENNS072N".
      * 10. BR-OPEN-BAL(9)
           03  FILLER     PIC X(8) VALUE "ENNS072N".
      * 11. BR-TENDER-CD
           03  FILLER     PIC X(8) VALUE "E  A050 ".
      * 12. BR-ALPHA-DRAWERS
           03  FILLER     PIC X(8) VALUE "E  A010 ".
      * 13. BR-GL-ESCROW-BALANCE
           03  FILLER     PIC X(8) VALUE "ENNN040 ".
      * 14. BR-GL-ESCROW-PAYABLE
           03  FILLER     PIC X(8) VALUE "ENNN040 ".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 14 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

       COPY "LIBGB/PASSWDW.CPY".
       78  FIELDS-SEQ                VALUE 0.
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGL/GLSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/BRMAN6_DEF.CPY".
       COPY "LIBGB/BRMAN6_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBGB/BRMAN6_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E    D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************

           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE FORM-LINK-AREA TO BRMAIN-LINK.
           MOVE "BRMAN6" TO VDU-FORM-NAME.

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
              MOVE 7 TO FORM-SEL.

           IF FORM-SEL NOT = 0
              IF FORM-P = "P"
                 MOVE BR-NO TO SAVEKEY
                 MOVE FORM-SEL TO FORMNUM
                 GO TO END-ROUTINE.

           MOVE "N" TO NEW-REC.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-BR-MODULE SECTION.
      ******************************************************************
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

      *----------------------------------------------------------------
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

      ******************************************************************
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *
      ******************************************************************
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

      *----------------------------------------------------------------
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
              IF NOT (FORM-SEL = 1 OR 2 OR 3 OR 4 OR 5 OR 7)
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

      * LIMIT ACCESS TO ALL FIELDS EXCEPT 1-10
           IF ELE > 10
              IF NEW-FLAG = 0
                 MOVE FIELDS-SEQ                    TO PASSWDW-SEQ
                 MOVE "TO CHANGE THIS FIELD......." TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       TEST-F4.
           IF UP-KEY
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       TEST-F5.
           IF DOWN-KEY
              MOVE 1 TO ONE
              GO TO GOTO-FIELD1.

      *----------------------------------------------------------------
       TEST-F6.
           IF NOT F6-KEY
              GO TO TEST-F1.
           IF ELE NOT = 13
              IF ELE NOT = 14
                 GO TO ENTER-ELE.
           PERFORM SCAN-GL-FILE.
           IF GL-SCAN-ERR = "Y"
              GO TO ENTER-ELE.
           GO TO GOTO-FIELD1.
      *----------------------------------------------------------------
       TEST-F1.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *----------------------------------------------------------------
       GOTO-FIELD1.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *****************************************************************
      *    S E T   R E C O R D   E L E M E N T S
      *****************************************************************
       F-01.
           MOVE VDUBUF TO BR-AUTO-OPEN-BAL.

           IF BR-AUTO-OPEN-BAL NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-02.
           MOVE VDUNUM2 TO BR-OPEN-BAL(1).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-03.
           MOVE VDUNUM2 TO BR-OPEN-BAL(2).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-04.
           MOVE VDUNUM2 TO BR-OPEN-BAL(3).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-05.
           MOVE VDUNUM2 TO BR-OPEN-BAL(4).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-06.
           MOVE VDUNUM2 TO BR-OPEN-BAL(5).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-07.
           MOVE VDUNUM2 TO BR-OPEN-BAL(6).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-08.
           MOVE VDUNUM2 TO BR-OPEN-BAL(7).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-09.
           MOVE VDUNUM2 TO BR-OPEN-BAL(8).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-10.
           MOVE VDUNUM2 TO BR-OPEN-BAL(9).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-11.
           IF VDUBUF = " "
              GO TO ENTER-ELE.

           MOVE VDUBUF TO BR-TENDER-CD(1).

      *----------------------------------------------------------------
       F-111.
           MOVE "E  A050111." TO VDU.
           PERFORM SCREENIO.

           IF UP-KEY
              GO TO ENTER-ELE.

           IF NOT ENTER-KEY
              GO TO F-111.

           IF VDUBUF = " "
              GO TO F-111.

           MOVE VDUBUF TO BR-TENDER-CD(2).

      *----------------------------------------------------------------
       F-112.
           MOVE "E  A050112." TO VDU.
           PERFORM SCREENIO.

           IF UP-KEY
              GO TO F-111.

           IF NOT ENTER-KEY
              GO TO F-112.

           IF VDUBUF = " "
              GO TO F-112.

           MOVE VDUBUF TO BR-TENDER-CD(3).
           GO TO CREATE-ROUTINE.
      *---------------------------------------------------------------
       F-12.
           MOVE VDUBUF TO BR-ALPHA-DRAWERS.

           IF  BR-ALPHA-DRAWERS NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-13.
           IF F6-KEY
              MOVE WORK-GL TO BR-GL-ESCROW-BALANCE
              GO TO F-13-CHECK-GL.
           MOVE VDUNUM0 TO GL-BRANCH WORK-BR.
       F-131.
           MOVE "ENNN070131." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-131.
           MOVE VDUNUM0 TO GL-ACCOUNT WORK-NO.
           MOVE WORK-GL TO BR-GL-ESCROW-BALANCE.
       F-13-CHECK-GL.
           IF BR-GL-ESCROW-BALANCE NOT = 0
              MOVE BR-GL-ESCROW-BALANCE TO GL-BR-ACCOUNT
              PERFORM GET-GL
              IF IO-BAD
                 MOVE "NOT FOUND" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 MOVE "S  A000132." TO VDU
                 MOVE GL-DESCRIPTION TO VDUBUF
                 PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.
      *----------------------------------------------------------------
       F-14.
           IF F6-KEY
              MOVE WORK-GL TO BR-GL-ESCROW-PAYABLE
              GO TO F-14-CHECK-GL.
           MOVE VDUNUM0 TO GL-BRANCH WORK-BR.
       F-141.
           MOVE "ENNN070141." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-141.
           MOVE VDUNUM0 TO GL-ACCOUNT WORK-NO.
           MOVE WORK-GL TO BR-GL-ESCROW-PAYABLE.

       F-14-CHECK-GL.
           IF BR-GL-ESCROW-PAYABLE NOT = 0
              MOVE BR-GL-ESCROW-PAYABLE TO GL-BR-ACCOUNT
              PERFORM GET-GL
              IF IO-BAD
                 MOVE "NOT FOUND" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 MOVE "S  A000142." TO VDU
                 MOVE GL-DESCRIPTION TO VDUBUF
                 PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL.

       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " "                          TO PASSWDW-STAT.
           MOVE "BRMAN6"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       EXIT-VERIFY-PASSWORD.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

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

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE CHANGED-BR-REC TO BR-REC.
           PERFORM REWRITE-BR-FILE.

      * CODE TO WRITE CHANGE FILE.
      *    MOVE "C" TO ERRLOGW-ACTION.
      *    MOVE BR-NO TO ERRLOGW-KEY.
      *    MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
      *    MOVE "CL/ERRLOG" TO FORM-NAM.
      *    CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
      *    CANCEL FORM-PROGX.

       END-WRITE-MODULE.
           PERFORM CLOSE-BR-FILE.


      *****************************************************************
       SCAN-GL-FILE SECTION.
      *****************************************************************
           MOVE BR-NO  TO GLSCAN-BRANCH.
           MOVE SPACES TO GLSCAN-KEY.

           MOVE "WI/GLSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF GLSCAN-KEY = SPACES
              MOVE "Y" TO GL-SCAN-ERR
              GO TO EXIT-SCAN-GL-FILE.

           MOVE "SNNN040  0." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE GLSCAN-BRANCH TO VDUNUM0 WORK-BR GL-BRANCH.
           PERFORM SCREENIO.

           MOVE "SNNN070  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE GLSCAN-ACCOUNT TO VDUNUM0 WORK-NO GL-ACCOUNT.
           PERFORM SCREENIO.

           MOVE "SNNN070  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE GLSCAN-DESC TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "N" TO GL-SCAN-ERR.
           MOVE "1<" TO VDUSTATUS.

       EXIT-SCAN-GL-FILE.
           EXIT.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE ORIGINAL-BR-REC               TO BROR-REC.
           MOVE CHANGED-BR-REC                TO BRCH-REC.

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE BR-NO                      TO LMC-BRNO.
           MOVE " TO "                     TO LMC-TO-X.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BROR-AUTO-OPEN-BAL NOT = BRCH-AUTO-OPEN-BAL
             MOVE "01 "                   TO LMC-FIELD
             MOVE "AUTO OPEN"             TO LMC-DESC
             MOVE BROR-AUTO-OPEN-BAL      TO LMC-ORG-TEXT
             MOVE BRCH-AUTO-OPEN-BAL      TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-OPEN-BAL(1) NOT = BRCH-OPEN-BAL(1)
             MOVE "02 "                   TO LMC-FIELD
             MOVE "OPEN BAL1"             TO LMC-DESC
             MOVE BROR-OPEN-BAL(1)        TO LMC-ORG-AMT
             MOVE BRCH-OPEN-BAL(1)        TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-OPEN-BAL(2) NOT = BRCH-OPEN-BAL(2)
             MOVE "03 "                   TO LMC-FIELD
             MOVE "OPEN BAL2"             TO LMC-DESC
             MOVE BROR-OPEN-BAL(2)        TO LMC-ORG-AMT
             MOVE BRCH-OPEN-BAL(2)        TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-OPEN-BAL(3) NOT = BRCH-OPEN-BAL(3)
             MOVE "04 "                   TO LMC-FIELD
             MOVE "OPEN BAL3"             TO LMC-DESC
             MOVE BROR-OPEN-BAL(3)        TO LMC-ORG-AMT
             MOVE BRCH-OPEN-BAL(3)        TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-OPEN-BAL(4) NOT = BRCH-OPEN-BAL(4)
             MOVE "05 "                   TO LMC-FIELD
             MOVE "OPEN BAL4"             TO LMC-DESC
             MOVE BROR-OPEN-BAL(4)        TO LMC-ORG-AMT
             MOVE BRCH-OPEN-BAL(4)        TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-OPEN-BAL(5) NOT = BRCH-OPEN-BAL(5)
             MOVE "06 "                   TO LMC-FIELD
             MOVE "OPEN BAL5"             TO LMC-DESC
             MOVE BROR-OPEN-BAL(5)        TO LMC-ORG-AMT
             MOVE BRCH-OPEN-BAL(5)        TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-OPEN-BAL(6) NOT = BRCH-OPEN-BAL(6)
             MOVE "07 "                   TO LMC-FIELD
             MOVE "OPEN BAL6"             TO LMC-DESC
             MOVE BROR-OPEN-BAL(6)        TO LMC-ORG-AMT
             MOVE BRCH-OPEN-BAL(6)        TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-OPEN-BAL(7) NOT = BRCH-OPEN-BAL(7)
             MOVE "08 "                   TO LMC-FIELD
             MOVE "OPEN BAL7"             TO LMC-DESC
             MOVE BROR-OPEN-BAL(7)        TO LMC-ORG-AMT
             MOVE BRCH-OPEN-BAL(7)        TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-OPEN-BAL(8) NOT = BRCH-OPEN-BAL(8)
             MOVE "08 "                   TO LMC-FIELD
             MOVE "OPEN BAL8"             TO LMC-DESC
             MOVE BROR-OPEN-BAL(8)        TO LMC-ORG-AMT
             MOVE BRCH-OPEN-BAL(8)        TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-OPEN-BAL(9) NOT = BRCH-OPEN-BAL(9)
             MOVE "10 "                   TO LMC-FIELD
             MOVE "OPEN BAL9"             TO LMC-DESC
             MOVE BROR-OPEN-BAL(9)        TO LMC-ORG-AMT
             MOVE BRCH-OPEN-BAL(9)        TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-TENDER-CD(1) NOT = BRCH-TENDER-CD(1)
             MOVE "11 "                   TO LMC-FIELD
             MOVE "TENDER 01"             TO LMC-DESC
             MOVE BROR-TENDER-CD(1)       TO LMC-ORG-TEXT
             MOVE BRCH-TENDER-CD(1)       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-TENDER-CD(2) NOT = BRCH-TENDER-CD(2)
             MOVE "111"                   TO LMC-FIELD
             MOVE "TENDER 02"             TO LMC-DESC
             MOVE BROR-TENDER-CD(2)       TO LMC-ORG-TEXT
             MOVE BRCH-TENDER-CD(2)       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-TENDER-CD(3) NOT = BRCH-TENDER-CD(3)
             MOVE "112"                   TO LMC-FIELD
             MOVE "TENDER 03"             TO LMC-DESC
             MOVE BROR-TENDER-CD(3)       TO LMC-ORG-TEXT
             MOVE BRCH-TENDER-CD(3)       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ALPHA-DRAWERS NOT = BRCH-ALPHA-DRAWERS
             MOVE "12 "                   TO LMC-FIELD
             MOVE "ALP DRAWR"             TO LMC-DESC
             MOVE BROR-ALPHA-DRAWERS      TO LMC-ORG-TEXT
             MOVE BRCH-ALPHA-DRAWERS      TO LMC-CHG-TEXT
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
           MOVE SPACES           TO DISPLAY-BUF.

           MOVE BR-AUTO-OPEN-BAL TO F01.
           MOVE 1                TO SUB.

      *----------------------------------------------------------------
       SETUP-NEXT.
           MOVE BR-OPEN-BAL(SUB) TO F02(SUB).
           ADD 1 TO SUB.

           IF SUB NOT > 9
              GO TO SETUP-NEXT.

           MOVE BR-TENDER-CD(1) TO F11.
           MOVE BR-TENDER-CD(2) TO F111.
           MOVE BR-TENDER-CD(3) TO F112.
           MOVE BR-ALPHA-DRAWERS TO F120.

           MOVE BR-GL-ESCROW-BALANCE TO WORK-GL GL-BR-ACCOUNT.
           MOVE WORK-BR TO F130.
           MOVE WORK-NO TO F131.
           PERFORM GET-GL.
           IF IO-GOOD
              MOVE GL-DESCRIPTION TO F132.
           MOVE BR-GL-ESCROW-PAYABLE TO WORK-GL GL-BR-ACCOUNT.
           MOVE WORK-BR TO F140.
           MOVE WORK-NO TO F141.
           PERFORM GET-GL.
           IF IO-GOOD
              MOVE GL-DESCRIPTION TO F142.

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
           MOVE EXT-CONAME-CONAME TO BRMAN6-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BRMAN6-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BRMAN6-SCREEN.
           DISPLAY BRMAN6-SCREEN UPON BRMAIN-WINDOW-HANDLE.
           MOVE BRMAN6-FORM-FIELDS TO WR-BUF.
           MOVE BRMAN6-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE BRMAN6-HELP--FILE TO HELP-LINK-FILE.
           MOVE BRMAN6-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/BRMAN6_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
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
           MOVE "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                                               TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       GET-GL.
           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           PERFORM CLOSE-GL1-FILE.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BRMAN6-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE BRMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
