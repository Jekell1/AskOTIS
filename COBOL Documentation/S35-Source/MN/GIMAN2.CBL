       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GIMAN2.
      ******************************************************************
      *
      *   DESC:  GENERAL LEDGER CONTROLS MAINT - PROG 2
      *
      *=================================================================
      * REV:
      * JTG 121197 CHANGED TO USE ( 78  GI-SIZE           VALUE 750 )
      * JTG 111998 ADDED SELECT OF PAGE 6, REVOLVING OTHBAL TABLE
      * MJD 000929 ADDED GI-PAYLBOX (OFFSET=117) (MFC PR#237)
      * JTG 010605 ADDED GP-TRUE-NEXT-BATCH               (MERC PR#243/251)
      *            GI-PAYTPAY
      *            GI-PAYTCOLL
      *            GI-PAYMGRAM
      * JTG 010613 CORRECTED GP-TRUE-NEXT-BATCH           (MERC PR#243/251)
      * MG  030602 ADDED AN F9-CALL TO THE MN/GLDSMN PER JOHN
      * BAH 031211 WRITE CHANGES AND DELETE TO CHANGES LOG FILE
      * AMM 070309 CREATED A COPY MEMBER TO LOG FIELD CHANGE
      * BLM 150205 REWRITE TO USE DESCRIPTIVE FIELD NAMES, NOT TABLE. 
      *            ADD FIELDS FOR EXPANDED INSURANCE TABLE, ADD
      *            F2-DISPLAY DESC
      *
      * KEC 2017.0214 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'GI' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *          FIXED THE SCANKEY; WAS NOT WORKING FROM A15.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/GIMAN2.CBL S34 06/03/21-20:57:16 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".

       COPY "LIBMN/MNLGGIW.CPY".

      *=================================================================
       01  CHANGES-GI1-KEY.
           03  CHANGES-GI-BRANCH       PIC 9(04)     VALUE ZEROES.
           03  CHANGES-GI-CLASS        PIC 9(03)     VALUE ZEROES.

       01  HOLD-GI1-KEY                PIC 9(07)     VALUE ZEROES.

       01  CHANGED-GI-REC              PIC X(GI-SIZE)   VALUE SPACES.
       01  ORIGINAL-GI-REC             PIC X(GI-SIZE)   VALUE SPACES.

      ******************************************************************
       01  ONE               PIC S9.
       01  ELE               PIC 99     VALUE 0.
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).
       01  FORM-NUM1     REDEFINES ACTION.
           03  FORM-KEY1     PIC X(4).
           03  FORM-P1       PIC X.
           03  FORM-SEL1     PIC 9.

       01  SPLIT-BR-ACCT     PIC 9(11)  VALUE 0.
       01  SPLIT-ACCT        REDEFINES SPLIT-BR-ACCT.
           03  GI-BR         PIC 9(4).
           03  GI-NUM        PIC 9(7).

       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.

       01  CHAN              PIC X(4).
       01  GIMAIN-LINK       VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC 9(7).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  GIMAIN-WINDOW-HANDLE  PIC X(10).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER1
      *    **********************************
       01  DISPLAY-BUF      VALUE SPACES.
           03  DIS-BUF OCCURS 25 TIMES.
               05  DIS-BR   PIC Z(4).
               05  DIS-NUM  PIC 9(7).

       01  DISPLAY-LIST.
           03  FILLER       PIC X(48) VALUE
           "010 011 020 021 030 031 040 041 050 051 060 061 ".
           03  FILLER       PIC X(48) VALUE
           "070 071 080 081 090 091 100 101 110 111 120 121 ".
           03  FILLER       PIC X(48) VALUE
           "130 131 140 141 150 151 160 161 170 171 180 181 ".
           03  FILLER       PIC X(48) VALUE
           "190 191 200 201 210 211 220 221 230 231 240 241 ".
           03  FILLER       PIC X(08) VALUE
           "250 251.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       78  MAX                      VALUE 25.
       01  SPEC-TABLE.
           03  FILLER        PIC X(8) VALUE "ENNN040 ".
       01  SPEC-TABLE2 REDEFINES SPEC-TABLE.
           03  SPEC-REC.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

       COPY "LIBGL/GLSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/GIMAN2_DEF.CPY".
       COPY "LIBMN/GIMAN2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/GIMAN2_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ********************************************
       MAIN-PROGRAM SECTION.
      ********************************************
           PERFORM SQL-CONNECT.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME  TO FORM-PATH.

           MOVE FORM-LINK-AREA TO GIMAIN-LINK.

           MOVE "GIMAN2"       TO VDU-FORM-NAME.

       MAIN-MODULE.
      D    STOP MESS.
           PERFORM FORM-RESET.

           PERFORM REC-GI-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.

           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P1
              MOVE 3 TO FORM-SEL1.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE GI1-KEY TO SAVEKEY
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.
           MOVE "N" TO NEW-REC.
           GO TO MAIN-MODULE.

      ********************************************
       REC-GI-MODULE SECTION.
      ********************************************
       REC-GI-MODULE-NO.
           MOVE 0 TO GI-BRANCH GI-CLASS.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = 0000000
              MOVE SAVEKEY TO GI1-KEY
              MOVE ALL "0" TO SAVEKEY
              PERFORM DISPLAY-KEY
              GO TO READ-GIFILE.
           MOVE 0 TO NEW-FLAG.
           MOVE "ENTER BRANCH, F6-SCAN, F2-DISPLAY DESC" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N040BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-GI-MODULE.
           IF VDUSTATUS = "1>"
              MOVE SPACES TO MESS
              PERFORM SEND-LEGEND
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GI-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-GI-MODULE-NO.
           MOVE VDUNUM0 TO GI-BRANCH.
           PERFORM VERIFY-BRNO.
           IF IO-FG NOT = 0
              GO TO REC-GI-MODULE-NO.
       REC-GI-CLASS.
           MOVE "ENTER CLASS, F6-SCAN" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N030CLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-GI-MODULE-NO.
           IF VDUSTATUS = "1U"
              GO TO REC-GI-MODULE-NO.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-GI-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-GI-CLASS.
           MOVE VDUNUM0 TO GI-CLASS.
           PERFORM VERIFY-CLASS-CODE.
           IF IO-FG NOT = 0
              GO TO REC-GI-CLASS.
       READ-GIFILE.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
           MOVE GI1-KEY TO HOLD-GI1-KEY.
           PERFORM OPEN-GI1-FILE.
           PERFORM READ-GI1-FILE.
           PERFORM CLOSE-GI1-FILE.
           IF IO-FG NOT = 0
              GO TO REC-GI-MODULE-NO.
           MOVE GI-REC TO ORIGINAL-GI-REC.
           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG
              MOVE "S  A000NEW." TO VDU
              MOVE "*** NEW RECORD ***" TO VDUBUF
              PERFORM SCREENIO
              GO TO END-REC-GI-MODULE.
           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.
       END-REC-GI-MODULE.
           EXIT.

      ********************************************
       DISPLAY-KEY SECTION.
      ********************************************
           MOVE "SN N040BRANCH." TO VDU.
           MOVE GI-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM VERIFY-BRNO.
           MOVE "S  A000BRNAME." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN N030CLASS." TO VDU.
           MOVE GI-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM VERIFY-CLASS-CODE.
           MOVE "S  A000CLASSDESC." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.

      ********************************************
       ENTER-SCANKEY SECTION.
      ********************************************
           MOVE "F1-RESET, F6-SCAN, <ENTER> TO ACCEPT" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-GI1-FILE.
       ENTER-KEY.
           MOVE "EN N060SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-GIFILE-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUNUM0 = 0
              IF FIRST-SCAN NOT = 0
                 MOVE GI1-KEY TO HOLD-GI1-KEY
                 MOVE GI-REC TO ORIGINAL-GI-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
           MOVE VDUNUM0         TO HOLD-GI1-KEY.
           MOVE HOLD-GI1-KEY    TO GI1-KEY.
           PERFORM START-GI1-FILE.
       READ-GIFILE-NEXT.
           IF FIRST-SCAN = 0
              PERFORM START-GI1-FILE
              MOVE 1 TO FIRST-SCAN.
           PERFORM READ-GI1-FILE-NEXT.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
      *    UNLOCK GI-FILE.
           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.
           GO TO ENTER-KEY.
       DISPLAY-SCAN-KEY.
           MOVE "SN N040BRANCH." TO VDU.
           MOVE GI-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM VERIFY-BRNO.
           MOVE "SN N030CLASS." TO VDU.
           MOVE GI-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM VERIFY-CLASS-CODE.
       CLOSE-SCANKEY.
           PERFORM CLOSE-GI1-FILE.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       END-SCANKEY.
           EXIT.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
           PERFORM OPEN-GL1-FILE.
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
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "1("
              PERFORM CALL-PROGRAM
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
      * FORM-NUM1 CHECKS FOR PAGES 1,3,4,5
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 = 0 OR FORM-SEL1 = 2 OR FORM-SEL1 > 5
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
           GO TO CHECK-CHNO.
       CHECK-PAGE.
           MOVE "RWR" TO IO-TYPE.
           GO TO EXIT-ENTRY-MODULE.
       CHECK-CHNO.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                IF CHKEY NOT = CHAN
                   GO TO MODIFY-ROUTINE.
           MOVE "F6-GL SCAN, <ENTER>-DEFAULT BRANCH, F2-DISPLAY DESC"
             TO MESS.
           PERFORM CALL-TMMSG.
           MOVE SPEC TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F6.
           IF VDUSTATUS NOT = "1<"
              GO TO TEST-F2.

           MOVE GI-BRANCH TO GLSCAN-BRANCH.
           MOVE 0         TO GLSCAN-ACCOUNT.

           MOVE "WI/GLSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF GLSCAN-ACCOUNT = 0
              GO TO ENTER-ELE.

           MOVE "SN N040000." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE GLSCAN-BRANCH TO VDUNUM0 GL-BRANCH GI-BR.
           PERFORM SCREENIO.
           MOVE "SN N070001." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE GLSCAN-ACCOUNT TO VDUNUM0 GL-ACCOUNT GI-NUM.
           PERFORM SCREENIO.
           MOVE "1<" TO VDUSTATUS.
           GO TO GOTO-FIELD.
       TEST-F2.
           IF F2-KEY AND NEW-FLAG NOT = 1
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08
                 F-09 F-10 F-11 F-12 F-13 F-14 F-15 F-16
                 F-17 F-18 F-19 F-20 F-21 F-22 F-23 F-24
                 F-25
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
      *-----------------------------------------------------------------
      *    SET RECORD ELEMENTS
      *----------------------------------------------------------------
       F-01.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-PAYCL1
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-PAYCL1 TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-01-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-PAYCL1.
           GO TO CREATE-ROUTINE.
       F-02.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-PAYCL2
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-PAYCL2 TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-02-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-PAYCL2.
           GO TO CREATE-ROUTINE.
       F-03.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-PAYAH
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-PAYAH TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-03-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-PAYAH.
           GO TO CREATE-ROUTINE.
       F-04.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-PAYPP
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-PAYPP TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-04-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-PAYPP.
           GO TO CREATE-ROUTINE.
       F-05.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-PAYO1
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-PAYO1 TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-05-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-PAYO1.
           GO TO CREATE-ROUTINE.
       F-06.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-PAYO2
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-PAYO2 TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-06-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-PAYO2.
           GO TO CREATE-ROUTINE.
       F-07.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-PAYO3
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-PAYO3 TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-07-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-PAYO3.
           GO TO CREATE-ROUTINE.
       F-08.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-PAYO4
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-PAYO4 TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-08-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-PAYO4.
           GO TO CREATE-ROUTINE.
       F-09.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-PAYO5
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-PAYO5 TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-09-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-PAYO5.
           GO TO CREATE-ROUTINE.
       F-10.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-PAYO6
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-PAYO6 TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-10-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-PAYO6.
           GO TO CREATE-ROUTINE.
       F-11.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-PAYO7
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-PAYO7 TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-11-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-PAYO7.
           GO TO CREATE-ROUTINE.
       F-12.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-DLPAYABLE
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-DLPAYABLE TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-12-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-DLPAYABLE.
           GO TO CREATE-ROUTINE.
       F-13.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-DLRVPRIN-1
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-DLRVPRIN-1 TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-13-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-DLRVPRIN-1.
           GO TO CREATE-ROUTINE.
       F-14.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-DLRVPRIN-2
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-DLRVPRIN-2 TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-14-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-DLRVPRIN-2.
           GO TO CREATE-ROUTINE.
       F-15.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-DLRVPRIN-3
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-DLRVPRIN-3 TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-15-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-DLRVPRIN-3.
           GO TO CREATE-ROUTINE.
       F-16.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-DLRVCHGS-1
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-DLRVCHGS-1 TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-16-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-DLRVCHGS-1.
           GO TO CREATE-ROUTINE.
       F-17.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-DLRVCHGS-2
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-DLRVCHGS-2 TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-17-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-DLRVCHGS-2.
           GO TO CREATE-ROUTINE.
       F-18.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-DLRVCHGS-3
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-DLRVCHGS-3 TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-18-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-DLRVCHGS-3.
           GO TO CREATE-ROUTINE.
       F-19.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-DLPARV
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-DLPARV TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-19-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-DLPARV.
           GO TO CREATE-ROUTINE.
       F-20.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-PAYALLOT
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-PAYALLOT TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-20-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-PAYALLOT.
           GO TO CREATE-ROUTINE.
       F-21.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-DIRECT-PAY
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-DIRECT-PAY TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-21-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-DIRECT-PAY.
           GO TO CREATE-ROUTINE.
       F-22.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-PAYLBOX
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-PAYLBOX TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-22-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-PAYLBOX.
           GO TO CREATE-ROUTINE.
       F-23.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-PAYTPAY
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-PAYTPAY TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-23-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-PAYTPAY.
           GO TO CREATE-ROUTINE.
       F-24.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-PAYTCOLL
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-PAYTCOLL TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-24-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-PAYTCOLL.
           GO TO CREATE-ROUTINE.
       F-25.
           IF F6-KEY
              MOVE SPLIT-BR-ACCT TO GI-PAYMGRAM
              GO TO CREATE-ROUTINE.
           IF F2-KEY
              MOVE GI-PAYMGRAM TO GL-BR-ACCOUNT
              PERFORM DISPLAY-GL-DESC
              GO TO ENTER-ELE.
           IF VDUNUM0 = 0
              PERFORM SEND-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH GI-BR.
       F-25-ACCT.
           PERFORM ENTER-ACCOUNT.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE SPLIT-BR-ACCT TO GI-PAYMGRAM.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1.
           PERFORM CLOSE-GL1-FILE.
       EXIT-ENTRY-MODULE.
           EXIT.

      ********************************************
       SEND-BRANCH SECTION.
      ********************************************
           MOVE "SNNN040001. " TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE GI-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      ********************************************
       ENTER-ACCOUNT SECTION.
      ********************************************
           MOVE "ENNN070   ." TO VDU.
           MOVE ELE TO VDUROW
           MOVE 1 TO VDUCOL.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE 9 TO IO-FG
              GO TO EXIT-ENTER-ACCOUNT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-ACCOUNT.
           MOVE VDUNUM0 TO GL-ACCOUNT GI-NUM.
           PERFORM READ-GL1-FILE.
           IF IO-BAD
              MOVE "INVALID ACCOUNT" TO MESS
              PERFORM SEND-MESS.
       EXIT-ENTER-ACCOUNT.
           EXIT.

      ********************************************
       DISPLAY-GL-DESC SECTION.
      ********************************************
           PERFORM READ-GL1-FILE.
           IF IO-GOOD
             MOVE "GL DESCRIPTION: &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"
                TO MESS
             INSPECT MESS REPLACING FIRST
                "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&" BY GL-DESCRIPTION
           ELSE
              MOVE "INVALID ACCOUNT" TO MESS
           END-IF
           PERFORM SEND-MESS.

      ********************************************
       WRITE-MODULE SECTION.
      ********************************************
           PERFORM OPEN-GI1-FILE.
           MOVE GI-REC TO CHANGED-GI-REC.
           IF ORIGINAL-GI-REC = CHANGED-GI-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-GI1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF GI-REC NOT = ORIGINAL-GI-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM MNLGGI-MODULE.

           MOVE CHANGED-GI-REC TO GI-REC.
           PERFORM REWRITE-GI1-FILE.
           GO TO END-WRITE-MODULE.
       END-WRITE-MODULE.
           PERFORM CLOSE-GI1-FILE.

      ********************************************
       DELETE-MODULE SECTION.
      ********************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-GI1-FILE.
           PERFORM READ-GI1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF GI-REC NOT = ORIGINAL-GI-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-GI1-FILE.
           PERFORM WRITE-CHANGES-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-GI1-FILE.

      ********************************************
       DISPLAY-FIELDS SECTION.
      ********************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE GI-PAYCL1         TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(1).
           MOVE GI-NUM            TO DIS-NUM(1).
           MOVE GI-PAYCL2         TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(2).
           MOVE GI-NUM            TO DIS-NUM(2).
           MOVE GI-PAYAH          TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(3).
           MOVE GI-NUM            TO DIS-NUM(3).
           MOVE GI-PAYPP          TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(4).
           MOVE GI-NUM            TO DIS-NUM(4).
           MOVE GI-PAYO1          TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(5).
           MOVE GI-NUM            TO DIS-NUM(5).
           MOVE GI-PAYO2          TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(6).
           MOVE GI-NUM            TO DIS-NUM(6).
           MOVE GI-PAYO3          TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(7).
           MOVE GI-NUM            TO DIS-NUM(7).
           MOVE GI-PAYO4          TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(8).
           MOVE GI-NUM            TO DIS-NUM(8).
           MOVE GI-PAYO5          TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(9).
           MOVE GI-NUM            TO DIS-NUM(9).
           MOVE GI-PAYO6          TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(10).
           MOVE GI-NUM            TO DIS-NUM(10).
           MOVE GI-PAYO7          TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(11).
           MOVE GI-NUM            TO DIS-NUM(11).
           MOVE GI-DLPAYABLE      TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(12).
           MOVE GI-NUM            TO DIS-NUM(12).
           MOVE GI-DLRVPRIN-1     TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(13).
           MOVE GI-NUM            TO DIS-NUM(13).
           MOVE GI-DLRVPRIN-2     TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(14).
           MOVE GI-NUM            TO DIS-NUM(14).
           MOVE GI-DLRVPRIN-3     TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(15).
           MOVE GI-NUM            TO DIS-NUM(15).
           MOVE GI-DLRVCHGS-1     TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(16).
           MOVE GI-NUM            TO DIS-NUM(16).
           MOVE GI-DLRVCHGS-2     TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(17).
           MOVE GI-NUM            TO DIS-NUM(17).
           MOVE GI-DLRVCHGS-3     TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(18).
           MOVE GI-NUM            TO DIS-NUM(18).
           MOVE GI-DLPARV         TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(19).
           MOVE GI-NUM            TO DIS-NUM(19).
           MOVE GI-PAYALLOT       TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(20).
           MOVE GI-NUM            TO DIS-NUM(20).
           MOVE GI-DIRECT-PAY     TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(21).
           MOVE GI-NUM            TO DIS-NUM(21).
           MOVE GI-PAYLBOX        TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(22).
           MOVE GI-NUM            TO DIS-NUM(22).
           MOVE GI-PAYTPAY        TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(23).
           MOVE GI-NUM            TO DIS-NUM(23).
           MOVE GI-PAYTCOLL       TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(24).
           MOVE GI-NUM            TO DIS-NUM(24).
           MOVE GI-PAYMGRAM       TO SPLIT-BR-ACCT.
           MOVE GI-BR             TO DIS-BR(25).
           MOVE GI-NUM            TO DIS-NUM(25).
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      ********************************************
       CALL-PROGRAM SECTION.
      ********************************************

           MOVE "MN/GLDSMN"  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH.

       CALL-PROGRAM-EXIT.
           EXIT.

       COPY "LIBMN/MNLGGI.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO GIMAN2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GIMAN2-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
           DISPLAY SPACES UPON GIMAIN-WINDOW-HANDLE
             LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GIMAN2-SCREEN UPON GIMAIN-WINDOW-HANDLE.
           MOVE GIMAN2-FORM-FIELDS TO WR-BUF.
           MOVE GIMAN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE GIMAN2-HELP--FILE TO HELP-LINK-FILE.
           MOVE GIMAN2-HELP--DIR TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/GIMAN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************************************
       MISC-PARAGRAPHS SECTION.
      *******************************************************
       COPY "LIBGB/ACCESS.CPY".
       WRITE-CHANGES-FILE.
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE GI-BRANCH TO CHANGES-GI-BRANCH.
           MOVE GI-CLASS TO CHANGES-GI-CLASS.
           MOVE CHANGES-GI1-KEY TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.
       VERIFY-BRNO.
           MOVE GI-BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO BR-NAME.
           MOVE "S  A000BRNAME." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.
       VERIFY-CLASS-CODE.
           MOVE GI-CLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE * " TO LC-DESC.
           MOVE "S  A000CLASSDESC." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           PERFORM CALL-TMMSG.

      *******************************************************
       IO-ROUTINES SECTION.
      *******************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGL/GLGI1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *******************************************************
       END-ROUTINE SECTION.
      *******************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GIMAN2-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE GIMAIN-LINK TO FORM-LINK-AREA.
       STOP-RUN.
           EXIT PROGRAM.
