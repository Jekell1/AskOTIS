       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BROPM1.
      *************************************************************************
      *
      *  DIR :   GB
      *  DESC:   COMENU HIDDEN BRANCH FILE MAINT PAGE 1
      *
      * REV :
      * JTG 033095 ADDED BR-UNITPER-FG & BR-UNITPER-Y-D-FG
      * KEC 020796 ADDED BR-COLLECTOR-QUEUE.
      * BAH 021996 ADDED BR-PCNI-LEDGER = "R" FOR RELIANCE
      * JTG 071596 REMOVED CHANGE FLAG ON BR-UNITPER-FG
      * BAH 082696 ADDED BR-A50-CPCORP FOR INSTANT (#368)
      * BAH 111496 ADDED BR-AVAIL-CREDIT OPTION = "S" FOR WDT (#335)
      * MJD 071797 ADDED BR-CAN-GROUP-ID AND DESCRIPTION. ADDED CAN GROUP
      *            ID SCAN. ADDED BR-STATE FOR REFERENCE ONLY.
      * MJD 072897 ADDED BR-LONPF-OPT-DISPLAY
      * KEC 082797 ***CONVERSION-A90-TO-A10***
      *              ADDED BR-GIFTNET.
      * JTG 092597 ADDED BRANCH OPTION BR-BALLOON-LOANS-FG, CASTLE FINANCE
      * MJD 112197 EXPANDED BR-REC TO 850 BYTES
      * BAH 022698 REMOVED BR-MEDIA-PURGE
      * BAH 040298 ADDED BR-GIFTNET-LEDGER-LINE FROM A90, FOR "TN" WHO USES
      *            THE BIG PARADATA CARDS FOR READL LOANS (LEDGER=Y) AND
      *            USES TINY NORWEST CARDS FOR GIFTNET (LEDGER=W)
      * BAH 052998 ADDED BR-CHECK-EOM-FG, FIELD # 7
      * BAH 061598 ADDED BR-FXPRESS-OPTION, MOVED FROM BRMAN7
      * JTG 101298 ADDED BR-REVOLVING-SF-FG AND ADDED BR-REVOLVING-LN-FG
      * GLF 101298 ADDED BR-LATECHG-ON-ALLOTMENTS, TFC #18
      * BAH 990413 ADDED CALL TO CREATE CHANGES FILE AFTER REWRITE
      * JTG 101298 ALLOWED BR-REVOLVING-LN-FG TO BE INPUTED
      * JTG 990616 ADDED TEST FOR BR-REB-METHOD-2(1) O3, NOT = TO 4 OR 5
      *            WHEN SELECTING BR-INS-ADDON = 'Y' OR 'O'
      * JTG 990618 ADDED GP-I-AM-REGMGM, REGIONAL MANAGEMENT, SC
      *            WHICH WILL ALLOW SELECTING BR-INS-ADDON 'Y' OR 'O'
      *            WHEN BR-REB-METHOD-.() = 4 OR 5
      * BAH 990623 CHANGED BR-PCNI-LEDGER = "R" FOR RELIANCE, TO BE FOR
      *            REGIONAL MANAGEMENT LEDGERS
      * JTG 990618 FIXED TO ALLOW 'Y'
      *            ADDED GP-I-AM-REGMGM, REGIONAL MANAGEMENT, SC
      *            WHICH WILL ALLOW SELECTING BR-INS-ADDON 'Y' OR 'O'
      *            WHEN BR-REB-METHOD-.() = 4 OR 5
      * GLB 990715 ADD BR-PASSWORD-DRAWER FLAG, PR# 901 REGMGM
      * BAH 990723 REMOVED FIELD 28, FORMS EXPRESS OPTION
      * BLV 990803 ALLOW VALUE OF C IN BR-PRIO-MEMO TO DISPLAY
      *            COLLATERAL INSTEAD OF BORROWER PRIORITY MEMO
      *  MG 991005 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * JTG 991129 ADDED BR-ONESTEP-RN-FG, TO ALLOW ONE STEP RENEWALS, WORLD
      * MJD 000223 ADDED BR-HIDE-END-BAL (WORLD PR#219)
      * MJD 000911 ADDED BR-LATECHG-ON-LBOX (MFC PR#227)
      * JTG 001117 STOPPED BR-ONESTEP-RN-FG = 'Y', UNTIL THE PROGRAMMING IS
      *            COMPLETED, WORLD
      * MJD 001120 ADDED BR-GLOBAL-QUEUE (TFC PR#60)
      * BAH 020405 CHANGED BROPMN TO BROPM1, HAD TO ADD PAGE 2
      *            MOVED BR-GLOBAL-QUEUE TO PAGE 2 ALSO....
      * BAH 060413 ADDED AURORA FINANCE LEDGER LINE TYPE "A", PR# 2762
      * AMM 070308 CREATED A LOG FIELD CHANGE ROUTINE
      * BAH 120514 REMOVED MAXPRU NONDISPLAY
      * BAH 130125 A30 CONVERSION
      *  CS 130318 REMOVED MISC CALL-TMMSG
      * BAH 130903 ADDED BR-AVAIL-CREDIT OPTION = "B" FOR HONOR PR #4358
      *  CS 160121 REMOVED BR-REB-METHOD-2, CL-LEVL IS NOW BR-REB-METHOD(7)
      *            O3 IS NOT BR-REB-METHOD(6)
      * KEC 2016.0309 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-CL???? TO BE CD-CL-????.
      * KEC 2016.0323 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-CD-??-KEY (NEW).
      * KEC 181009 PL#989 PARADATA A15 JAY CISZEWSKI
      *            FIXED AN ISSUE WITH ENTER-SCANKEY SECTION WHEN USING THE
      *            F1-KEY WHEN FIRST COMING INTO IT (BEFORE DOING A READ-NEXT).
      *            IT WOULD FLOW INTO ENTRY-MODULE WITHOUT HAVE READ A GP-REC.
      *            IN MAIN-MODULE, ADDED IF CONDITION ON IO-TYPE FOR "CLR"
      *            AFTER 'PERFORM REC-BR-MODULE' WHICH IF TRUE THEN RESET.
      * KEC 190220 A30 & A32 - ERROR OCCURS IN 'ENTER-SCANKEY SECTION'.
      *            FOUND THE ERROR HERE:
      *               IF VDUNUM0 = 0
      *                  MOVE BR-REC TO BROR-REC  <-- MISSING
      *                  GO TO CLOSE-SCAN.
      *            ADDED IT BACK TO GET IT TO WORK.
      * BAH 20190625 REMOVED BR-REVOLVING-SF-FG BR-REVOLVING-LN-FG PT0153
      * BAH 20190730 ADDED BR-PURGE-NOW-FLAG TO REPLACE THE "PURGEME" TAG
      *              IN LNARCH. LNARCH GETS PUT IN A BATCH AND ONLY THE
      *              BRANCHES WITH THIS FLAG SET WILL ACTUALLY PURGE
      * BAH 20200624 REMOVED OPEN-CD1 AND CLOSE FROM TSTCANID COPY MEMBER,
      *              REMOVED OPEN-CD1 AND CLOSE FROM GET-CD MISC ROUTINE
      *                                                            #TST001
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/BROPM1.CBL S35 05/06/24-15:06:10 >".

      ******************************************************************
      *
      *    P R O G R A M - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ORIGINAL RECORD BROR-REC
      *    CHANGED  RECORD BRCH-REC
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/GB01BR.CPY" REPLACING LEADING "BR" BY "BROR".
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

      *-----------------------------------------------------------------

       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  FORM-NUM REDEFINES ACTION.
           03  FORM-KEY    PIC X(4).
           03  FORM-P      PIC X.
           03  FORM-SEL    PIC 9.
       01  IO-TYPE         PIC X(3)    VALUE SPACES.
       01  NEW-FLAG        PIC 9       VALUE 0.
       01  STATE-WORKER    PIC X(4).

       01  BROPMN-LINK     VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(4).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMNUM       PIC 9.
           03  BROPMN-WINDOW-HANDLE PIC X(10).


      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  BRNAME      PIC X(20).
           03  BRSTATE     PIC X(2).
           03  F01         PIC X.
           03  F02         PIC X.
           03  F03         PIC X.
           03  F04         PIC X.
           03  F05         PIC X.
           03  F06         PIC X.
           03  F07         PIC X.
           03  F08         PIC X.
           03  F09         PIC X.
           03  F10         PIC X.
           03  F11         PIC X.
           03  F12         PIC X.
           03  F13         PIC X.
           03  F14         PIC X.
           03  F15         PIC X.
           03  F16         PIC X.
           03  F17         PIC X.
           03  F18         PIC X.
           03  F181        PIC X(4).
           03  F19         PIC X.
           03  F20         PIC X.
           03  F21         PIC X.
           03  F22         PIC X.
           03  F23         PIC X.
           03  F24         PIC X.
           03  F25         PIC X.
           03  F26         PIC X.
           03  F27         PIC X.
           03  F28         PIC X.
           03  F29         PIC X.
           03  F30         PIC X.
           03  F31         PIC X.
           03  F32         PIC X.
           03  F33         PIC X.
           03  F34         PIC X.

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER      PIC X(43) VALUE
           "BRNAME BRSTATE 010 020 030 040 050 060 070 ".
           03  FILLER      PIC X(36) VALUE
           "080 090 100 110 120 130 140 150 160 ".
           03  FILLER      PIC X(52) VALUE
           "170 180 181 190 200 210 220 230 240 250 260 270 280 ".
           03  FILLER      PIC X(24) VALUE
           "290 300 310 320 330 340.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S - # 1
      *
      ******************************************************************
       78  MAX                      VALUE 34.
      *-----------------------------------------------------------------
       01  SPEC-TABLE.
      * 1. COLL-ID-SCAN
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 2. BR-PCNI-LEDGER
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 3. BR-EMLNRG-PHONE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 4. BR-PRIO-MEMO-FLAG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 5. BR-AUTO-INS-EARN
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 6. BR-OTH-INS-LEVELS
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 7. BR-CHECK-EOM-FG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 8. BR-AVAIL-CREDIT
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 9. BR-SALES-SMALL
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *10. BR-ALDEL-FLAG.
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *11. BR-INS-ADDON.
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *12. BR-TRAILER-REC.
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *13. BR-AUTO-CHECK
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *14. BR-LC-CODE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *15. BR-VALIDATE-PRUS
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *16. BR-PREBILL-PD-JD-OPT
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *17. BR-NEW-COLL-INQUIRY
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *18. BR-CAN-LOANS-FG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *19. BR-POST-AHEAD-FG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *20. BR-UNITPER-FG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *21. BR-UNITPER-Y-D-FG
           03  FILLER      PIC X(8) VALUE "E  A010N".
      *22. BR-COLLECTOR-QUEUE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *23. BR-A50-CPCORP
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *24. BR-LONPF-OPT-DISPLAY
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *25. BR-GIFTNET
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *26. BR-BALLOON-LOANS-FG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *27. BR-GIFTNET-LEDGER-LINE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *28. BR-ONESTEP-RN-FG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *29. BR-PURGE-NOW-FLAG
           03  FILLER      PIC X(8) VALUE "E  A010N".
      *30. NA
           03  FILLER      PIC X(8) VALUE "E  A010N".
      *31. BR-LATECHG-ON-ALLOTMENTS
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *32. BR-LATECHG-ON-LBOX
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *33. BR-PASSWORD-DRAWER
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *34. BR-HIDE-END-BAL
           03  FILLER      PIC X(8) VALUE "E  A010 ".

      *-----------------------------------------------------------------
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TSTCANIDW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/BROPM1_DEF.CPY".
       COPY "LIBGB/BROPM1_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBGB/BROPM1_SCN.CPY".

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

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BROPM1" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO BROPMN-LINK.

           PERFORM OPEN-CD1-FILE.

       MAIN-MODULE.

           PERFORM FORM-RESET.

           PERFORM REC-BR-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.

           IF ( IO-TYPE = "CLR" )
              GO TO MAIN-MODULE.

           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF FORM-SEL NOT = 0
              IF FORM-P = "P"
                 MOVE BR-NO TO SAVEKEY
                 MOVE FORM-SEL TO FORMNUM
                 GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-BR-MODULE SECTION.
      ******************************************************************
       REC-BR-MODULE-NO.

           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO BR-NO
              MOVE " " TO SAVEKEY
              MOVE "S  A040BRANCH." TO VDU
              MOVE BR-NO TO VDUBUF
              PERFORM SCREENIO
              GO TO READ-BRANCH.

           MOVE 0 TO NEW-FLAG.
           MOVE "EN N040BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF F6-KEY
              PERFORM ENTER-SCANKEY
              GO TO END-REC-BR-MODULE.

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
           IF IO-GOOD
              MOVE BR-REC TO BROR-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
            ELSE
              PERFORM BRANCH-NOT-FOUND
              GO TO REC-BR-MODULE-NO.

       END-REC-BR-MODULE.
           EXIT.

      ******************************************************************
      *
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
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF FORM-KEY = " " AND FORM-P = "P" AND FORM-SEL NUMERIC
              IF NOT (FORM-SEL = 2 )
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.

           IF CHNO = 0
              GO TO MODIFY-ROUTINE.

           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
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

           IF ELE = 21
              GO TO CREATE-ROUTINE.

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
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

       GOTO-FIELD1.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25 F-26 F-27 F-28 F-29 F-30
                 F-31 F-32 F-33 F-34
           DEPENDING ON ELE.

       F-01.
           MOVE VDUBUF TO BR-COLL-ID-SCAN.

           IF NOT (BR-COLL-ID-SCAN = "Y" OR "N" OR "S")
              MOVE "MUST BE 'Y' OR 'N' OR 'S'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO BR-PCNI-LEDGER.

           IF BR-PCNI-LEDGER NOT = "Y" AND
              BR-PCNI-LEDGER NOT = "N" AND
              BR-PCNI-LEDGER NOT = "G" AND
              BR-PCNI-LEDGER NOT = "M" AND
              BR-PCNI-LEDGER NOT = "R" AND
              BR-PCNI-LEDGER NOT = "W" AND
              BR-PCNI-LEDGER NOT = "A"
              MOVE "ENTER: 'Y', 'N', 'G', 'M', 'R', 'W' OR 'A'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO BR-EMLNRG-PHONE.

           IF BR-EMLNRG-PHONE NOT = "Y" AND
              BR-EMLNRG-PHONE NOT = "N"
                 PERFORM SEND-Y-OR-N
                 GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO BR-PRIO-MEMO-FLAG.

           IF BR-PRIO-MEMO-FLAG NOT = "Y" AND
               BR-PRIO-MEMO-FLAG NOT = " " AND
                BR-PRIO-MEMO-FLAG NOT = "C" AND
                 BR-PRIO-MEMO-FLAG NOT = "I"
                    MOVE "MUST BE Y, C, I OR SPACE" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO BR-AUTO-INS-EARN.

           IF BR-AUTO-INS-EARN NOT = "Y" AND
              BR-AUTO-INS-EARN NOT = "N"
                 PERFORM SEND-Y-OR-N
                 GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUBUF TO BR-OTH-INS-LEVELS.

           IF BR-OTH-INS-LEVELS NOT = "Y" AND
              BR-OTH-INS-LEVELS NOT = "N"
                 PERFORM SEND-Y-OR-N
                 GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-CHECK-EOM-FG.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDUBUF TO BR-AVAIL-CREDIT.

           IF BR-AVAIL-CREDIT NOT = "Y" AND
              BR-AVAIL-CREDIT NOT = "N" AND
              BR-AVAIL-CREDIT NOT = "S" AND
              BR-AVAIL-CREDIT NOT = "B"
                 MOVE "ENTER 'Y', 'N', 'S' OR 'B'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUBUF TO BR-SALES-SMALL.

           IF BR-SALES-SMALL NOT = "Y" AND NOT = "N"
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE VDUBUF TO BR-ALDEL-FLAG.

           IF BR-ALDEL-FLAG NOT = "Y" AND NOT = "N"
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE VDUBUF TO BR-INS-ADDON.

           IF NOT BR-INS-ADDON-VALID
              MOVE "ENTER 'Y-(NEW)' OR 'N-(NONE)' OR 'O-(ORIG)' ONLY"
                     TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF ( BR-INS-ADDON = "Y" OR "O"        ) AND
              ( (BR-REB-METHOD(1)   = 4 OR 5) OR
                (BR-REB-METHOD(2)   = 4 OR 5) OR
                (BR-REB-METHOD(3)   = 4 OR 5) OR
                (BR-REB-METHOD(4)   = 4 OR 5) OR
                (BR-REB-METHOD(5)   = 4 OR 5) OR
                (BR-REB-METHOD(6) = 4 OR 5)
              )
                MOVE "ADDONS NOT ALLOWED WITH INS. CASH EARN METHODS"
                                                      TO MESS
                PERFORM SEND-MESS
                GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
           MOVE VDUBUF TO BR-TRAILER-REC.

           IF BR-TRAILER-REC NOT = "Y" AND NOT = "N"
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
           MOVE VDUBUF TO BR-AUTO-CHECK.

           IF BR-AUTO-CHECK NOT = "Y" AND NOT = "N"
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-14.
           MOVE VDUBUF TO BR-LC-CODE.

           IF BR-LC-CODE NOT = "Y" AND NOT = "N"
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-15.
           MOVE VDUBUF TO BR-VALIDATE-PRUS.

           IF BR-VALIDATE-PRUS NOT = "Y" AND NOT = "N"
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           MOVE VDUBUF TO BR-PREBILL-PL-JD-OPT

           IF BR-PREBILL-PL-JD-OPT NOT = "Y" AND NOT = "N"
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDUBUF TO BR-NEW-COLL-INQUIRY

           IF BR-NEW-COLL-INQUIRY NOT = "Y" AND NOT = "N"
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-18.
           MOVE VDUBUF TO BR-CAN-LOANS-FG

           IF NOT (BR-CAN-LOANS-FG = "Y" OR "N" OR "O")
              PERFORM SEND-Y-OR-N-OR-O
              GO TO ENTER-ELE.
           IF BR-CAN-LOANS-FG = "N"
              MOVE "S  A026181P." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A040181." TO VDU
              MOVE SPACES TO VDUBUF BR-CAN-GROUP-ID
              PERFORM SCREENIO
              MOVE "S  A012181P2:S." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A020181D." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
       F-181.
           MOVE "S  A026181P:S." TO VDU.
           MOVE "CANNED LOAN GROUP ID     :" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A012181P2:S." TO VDU.
           MOVE "DESCRIPTION:" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "E  A040181." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF F6-KEY
              PERFORM CODE-SCAN
              IF CDSCAN-STATUS NOT = "00"
                 GO TO F-181
              ELSE
                 MOVE CDSCAN-CODE TO VDUBUF
                 MOVE "00" TO VDUSTATUS.
           IF NOT ENTER-KEY
              GO TO F-181.
           IF VDUBUF = SPACES
              MOVE "MUST ENTER" TO MESS
              PERFORM SEND-MESS
              GO TO F-181.

           MOVE VDUBUF TO TCAN-WORD.
           MOVE "N"    TO TCAN-SKIP-CD-VERIFY.

           PERFORM TEST-CAN-GROUP.

           MOVE TCAN-WORD     TO BR-CAN-GROUP-ID.
           MOVE "S  A040181." TO VDU.
           MOVE TCAN-WORD     TO VDUBUF.
           PERFORM SCREENIO.
           IF TCAN-ERR NOT = 0
              MOVE "CAN GROUP ID NOT DEFINED" TO MESS
              PERFORM SEND-MESS
              GO TO F-181.

           MOVE "S  A020181D." TO VDU.
           MOVE CD-DESC        TO VDUBUF.
           PERFORM SCREENIO.
           IF TCAN-WORD NUMERIC
              IF TCAN-NUMB NOT = BR-NO
                 STRING "THIS WILL GIVE BRANCH " BR-NO
                   " ACCESS TO " TCAN-WORD "'S CAN LOANS"
                   DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS
              END-IF
           ELSE
              MOVE BR-STATE TO STATE-WORKER
              IF TCAN-WORD NOT = STATE-WORKER
      *JTG       STRING "THIS WILL GIVE THE STATE OF " BR-STATE
                 STRING "THIS WILL GIVE BRANCH " BR-NO
                   " ACCESS TO " TCAN-WORD "'S CAN LOANS"
                   DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS
              END-IF.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-19.
           MOVE VDUBUF TO BR-POST-AHEAD-FG.

           IF NOT (BR-POST-AHEAD-FG = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-20.
           MOVE VDUBUF TO BR-UNITPER-FG.

           IF NOT (BR-UNITPER-FG = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-21.
           GO TO CREATE-ROUTINE.
           MOVE VDUBUF TO BR-UNITPER-Y-D-FG.

           IF NOT (BR-UNITPER-Y-D-FG = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-22.
           MOVE VDUBUF TO BR-COLLECTOR-QUEUE.

           IF NOT (BR-COLLECTOR-QUEUE = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-23.
           MOVE VDUBUF TO BR-A50-CPCORP.
           IF NOT (BR-A50-CPCORP = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-24.
           MOVE VDUBUF TO BR-LONPF-OPT-DISPLAY.
           IF NOT (BR-LONPF-OPT-DISPLAY = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-25.
           MOVE VDUBUF TO BR-GIFTNET.

           IF NOT ( BR-GIFTNET = "Y" OR "N" )
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-26.
           MOVE VDUBUF TO BR-BALLOON-LOANS-FG.

           IF NOT ( BR-BALLOON-LOANS-FG = "Y" OR "N" )
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-27.
           MOVE VDUBUF TO BR-GIFTNET-LEDGER-LINE.

           IF BR-GIFTNET-LEDGER-LINE NOT = "Y" AND
              BR-GIFTNET-LEDGER-LINE NOT = "N" AND
              BR-GIFTNET-LEDGER-LINE NOT = "G" AND
              BR-GIFTNET-LEDGER-LINE NOT = "M" AND
              BR-GIFTNET-LEDGER-LINE NOT = "W"
               MOVE "ENTER: 'Y', 'N', 'G', 'M' OR 'W' ONLY" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-28.
      *    TEMPORARY STOP UNTIL PROGRAMMING IS COMPLETED! JTG 001117
           MOVE "S  A010280." TO VDU.
           MOVE "N"           TO VDUBUF
                                 BR-ONESTEP-RN-FG.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

           MOVE VDUBUF TO BR-ONESTEP-RN-FG.
           IF NOT (BR-ONESTEP-RN-FG = "Y" OR "N")
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *--------------------------L--------------------------------------
       F-29.
           MOVE VDUBUF TO BR-PURGE-NOW-FLAG.
           IF NOT (BR-PURGE-NOW-FLAG = "Y" OR "N")
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-30.
           GO TO CREATE-ROUTINE.
      *----------------------------------------------------------------
       F-31.
           MOVE VDUBUF TO BR-LATECHG-ON-ALLOTMENTS.

           IF NOT (BR-LATECHG-ON-ALLOTMENTS = "Y" OR "N")
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *----------------------------------------------------------------
       F-32.
           MOVE VDUBUF TO BR-LATECHG-ON-LBOX.

           IF NOT (BR-LATECHG-ON-LBOX = "Y" OR "N")
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-33.
           MOVE VDUBUF TO BR-PASSWORD-DRAWER.

           IF NOT (BR-PASSWORD-DRAWER = "Y" OR "N")
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-34.
           MOVE VDUBUF TO BR-HIDE-END-BAL.

           IF NOT (BR-HIDE-END-BAL = "Y" OR "N")
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-BR-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE BR-REC TO BRCH-REC.

           IF BROR-REC = BRCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-BR-FILE.
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF BR-REC NOT = BROR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE BRCH-REC TO BR-REC.
           MOVE "N" TO BR-UNITPER-Y-D-FG.
           PERFORM REWRITE-BR-FILE.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           MOVE BR-NO TO BR-NO.
           MOVE "N" TO BR-UNITPER-FG
                       BR-UNITPER-Y-D-FG.
           PERFORM WRITE-BR-FILE.
           IF IO-BAD
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-BR-FILE.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE BR-NO                      TO LMC-BRNO.
           MOVE " TO "                     TO LMC-TO-X.

           IF BROR-COLL-ID-SCAN NOT = BRCH-COLL-ID-SCAN
             MOVE "01 "                   TO LMC-FIELD
             MOVE "COLL SCAN"             TO LMC-DESC
             MOVE BROR-COLL-ID-SCAN       TO LMC-ORG-TEXT
             MOVE BRCH-COLL-ID-SCAN       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-PCNI-LEDGER NOT = BRCH-PCNI-LEDGER
             MOVE "02 "                   TO LMC-FIELD
             MOVE "LEDGER LN"             TO LMC-DESC
             MOVE BROR-PCNI-LEDGER        TO LMC-ORG-TEXT
             MOVE BRCH-PCNI-LEDGER        TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-EMLNRG-PHONE NOT = BRCH-EMLNRG-PHONE
             MOVE "03 "                   TO LMC-FIELD
             MOVE "XTR PHONE"             TO LMC-DESC
             MOVE BROR-EMLNRG-PHONE       TO LMC-ORG-TEXT
             MOVE BRCH-EMLNRG-PHONE       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-PRIO-MEMO-FLAG NOT = BRCH-PRIO-MEMO-FLAG
             MOVE "04 "                   TO LMC-FIELD
             MOVE "PRIO MEMO"             TO LMC-DESC
             MOVE BROR-PRIO-MEMO-FLAG     TO LMC-ORG-TEXT
             MOVE BRCH-PRIO-MEMO-FLAG     TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-AUTO-INS-EARN NOT = BRCH-AUTO-INS-EARN
             MOVE "05 "                   TO LMC-FIELD
             MOVE "INS EARN "             TO LMC-DESC
             MOVE BROR-AUTO-INS-EARN      TO LMC-ORG-TEXT
             MOVE BRCH-AUTO-INS-EARN      TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-OTH-INS-LEVELS NOT = BRCH-OTH-INS-LEVELS
             MOVE "06 "                   TO LMC-FIELD
             MOVE "ALLOW OTH"             TO LMC-DESC
             MOVE BROR-OTH-INS-LEVELS     TO LMC-ORG-TEXT
             MOVE BRCH-OTH-INS-LEVELS     TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-CHECK-EOM-FG   NOT = BRCH-CHECK-EOM-FG
             MOVE "07 "                   TO LMC-FIELD
             MOVE "OPEN DAY "             TO LMC-DESC
             MOVE BROR-CHECK-EOM-FG       TO LMC-ORG-TEXT
             MOVE BRCH-CHECK-EOM-FG       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-AVAIL-CREDIT NOT = BRCH-AVAIL-CREDIT
             MOVE "08 "                   TO LMC-FIELD
             MOVE "AVAL CRED"             TO LMC-DESC
             MOVE BROR-AVAIL-CREDIT       TO LMC-ORG-TEXT
             MOVE BRCH-AVAIL-CREDIT       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-SALES-SMALL NOT = BRCH-SALES-SMALL
             MOVE "09 "                   TO LMC-FIELD
             MOVE "SALES FIN"             TO LMC-DESC
             MOVE BROR-SALES-SMALL        TO LMC-ORG-TEXT
             MOVE BRCH-SALES-SMALL        TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ALDEL-FLAG NOT = BRCH-ALDEL-FLAG
             MOVE "10 "                   TO LMC-FIELD
             MOVE "ALOW DELQ"             TO LMC-DESC
             MOVE BROR-ALDEL-FLAG         TO LMC-ORG-TEXT
             MOVE BRCH-ALDEL-FLAG         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-INS-ADDON NOT = BRCH-INS-ADDON
             MOVE "11 "                   TO LMC-FIELD
             MOVE "ADDON INS"             TO LMC-DESC
             MOVE BROR-INS-ADDON          TO LMC-ORG-TEXT
             MOVE BRCH-INS-ADDON          TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-TRAILER-REC NOT = BRCH-TRAILER-REC
             MOVE "12 "                   TO LMC-FIELD
             MOVE "EXP COLL "             TO LMC-DESC
             MOVE BROR-TRAILER-REC        TO LMC-ORG-TEXT
             MOVE BRCH-TRAILER-REC        TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-AUTO-CHECK NOT = BRCH-AUTO-CHECK
             MOVE "13 "                   TO LMC-FIELD
             MOVE "CHECK NUM"             TO LMC-DESC
             MOVE BROR-AUTO-CHECK         TO LMC-ORG-TEXT
             MOVE BRCH-AUTO-CHECK         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-LC-CODE NOT = BRCH-LC-CODE
             MOVE "14 "                   TO LMC-FIELD
             MOVE "DLR LCODE"             TO LMC-DESC
             MOVE BROR-LC-CODE            TO LMC-ORG-TEXT
             MOVE BRCH-LC-CODE            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-VALIDATE-PRUS NOT = BRCH-VALIDATE-PRUS
             MOVE "15 "                   TO LMC-FIELD
             MOVE "VALID PRU"             TO LMC-DESC
             MOVE BROR-VALIDATE-PRUS      TO LMC-ORG-TEXT
             MOVE BRCH-VALIDATE-PRUS      TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-PREBILL-PL-JD-OPT NOT = BRCH-PREBILL-PL-JD-OPT
             MOVE "16 "                   TO LMC-FIELD
             MOVE "PREBILL  "             TO LMC-DESC
             MOVE BROR-PREBILL-PL-JD-OPT  TO LMC-ORG-TEXT
             MOVE BRCH-PREBILL-PL-JD-OPT  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-NEW-COLL-INQUIRY NOT = BRCH-NEW-COLL-INQUIRY
             MOVE "17 "                   TO LMC-FIELD
             MOVE "NEW COLL "             TO LMC-DESC
             MOVE BROR-NEW-COLL-INQUIRY   TO LMC-ORG-TEXT
             MOVE BRCH-NEW-COLL-INQUIRY   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-CAN-LOANS-FG NOT = BRCH-CAN-LOANS-FG
             MOVE "18 "                   TO LMC-FIELD
             MOVE "CAN LOAN "             TO LMC-DESC
             MOVE BROR-CAN-LOANS-FG       TO LMC-ORG-TEXT
             MOVE BRCH-CAN-LOANS-FG       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-CAN-GROUP-ID NOT = BRCH-CAN-GROUP-ID
             MOVE "181"                   TO LMC-FIELD
             MOVE "GROUP ID "             TO LMC-DESC
             MOVE BROR-CAN-GROUP-ID       TO LMC-ORG-TEXT
             MOVE BRCH-CAN-GROUP-ID       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-POST-AHEAD-FG NOT = BRCH-POST-AHEAD-FG
             MOVE "19 "                   TO LMC-FIELD
             MOVE "POST TRAN"             TO LMC-DESC
             MOVE BROR-POST-AHEAD-FG      TO LMC-ORG-TEXT
             MOVE BRCH-POST-AHEAD-FG      TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-UNITPER-FG NOT = BRCH-UNITPER-FG
             MOVE "20 "                   TO LMC-FIELD
             MOVE "WEEK ALOW"             TO LMC-DESC
             MOVE BROR-UNITPER-FG         TO LMC-ORG-TEXT
             MOVE BRCH-UNITPER-FG         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-UNITPER-Y-D-FG NOT = BRCH-UNITPER-Y-D-FG
             MOVE "21 "                   TO LMC-FIELD
             MOVE "DAY & YR "             TO LMC-DESC
             MOVE BROR-UNITPER-Y-D-FG     TO LMC-ORG-TEXT
             MOVE BRCH-UNITPER-Y-D-FG     TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-COLLECTOR-QUEUE NOT = BRCH-COLLECTOR-QUEUE
             MOVE "22 "                   TO LMC-FIELD
             MOVE "COL QUEUE"             TO LMC-DESC
             MOVE BROR-COLLECTOR-QUEUE    TO LMC-ORG-TEXT
             MOVE BRCH-COLLECTOR-QUEUE    TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-A50-CPCORP NOT = BRCH-A50-CPCORP
             MOVE "23 "                   TO LMC-FIELD
             MOVE "USE A50  "             TO LMC-DESC
             MOVE BROR-A50-CPCORP         TO LMC-ORG-TEXT
             MOVE BRCH-A50-CPCORP         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-LONPF-OPT-DISPLAY NOT = BRCH-LONPF-OPT-DISPLAY
             MOVE "24 "                   TO LMC-FIELD
             MOVE "OPT DISP "             TO LMC-DESC
             MOVE BROR-LONPF-OPT-DISPLAY  TO LMC-ORG-TEXT
             MOVE BRCH-LONPF-OPT-DISPLAY  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-GIFTNET NOT = BRCH-GIFTNET
             MOVE "25 "                   TO LMC-FIELD
             MOVE "WRLD CLAS"             TO LMC-DESC
             MOVE BROR-GIFTNET            TO LMC-ORG-TEXT
             MOVE BRCH-GIFTNET            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-BALLOON-LOANS-FG NOT = BRCH-BALLOON-LOANS-FG
             MOVE "26 "                   TO LMC-FIELD
             MOVE "LN BALOON"             TO LMC-DESC
             MOVE BROR-BALLOON-LOANS-FG   TO LMC-ORG-TEXT
             MOVE BRCH-BALLOON-LOANS-FG   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-GIFTNET-LEDGER-LINE NOT = BRCH-GIFTNET-LEDGER-LINE
             MOVE "27 "                    TO LMC-FIELD
             MOVE "WCBC LEDG"              TO LMC-DESC
             MOVE BROR-GIFTNET-LEDGER-LINE TO LMC-ORG-TEXT
             MOVE BRCH-GIFTNET-LEDGER-LINE TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ONESTEP-RN-FG NOT = BRCH-ONESTEP-RN-FG
             MOVE "28 "                   TO LMC-FIELD
             MOVE "1ST RENEW"             TO LMC-DESC
             MOVE BROR-ONESTEP-RN-FG      TO LMC-ORG-TEXT
             MOVE BRCH-ONESTEP-RN-FG      TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-LATECHG-ON-ALLOTMENTS NOT =
              BRCH-LATECHG-ON-ALLOTMENTS
             MOVE "31 "                      TO LMC-FIELD
             MOVE "LCHG ALOT"                TO LMC-DESC
             MOVE BROR-LATECHG-ON-ALLOTMENTS TO LMC-ORG-TEXT
             MOVE BRCH-LATECHG-ON-ALLOTMENTS TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-LATECHG-ON-LBOX NOT = BRCH-LATECHG-ON-LBOX
             MOVE "32 "                   TO LMC-FIELD
             MOVE "LCHG LBOX"             TO LMC-DESC
             MOVE BROR-LATECHG-ON-LBOX    TO LMC-ORG-TEXT
             MOVE BRCH-LATECHG-ON-LBOX    TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-PASSWORD-DRAWER NOT = BRCH-PASSWORD-DRAWER
             MOVE "33 "                   TO LMC-FIELD
             MOVE "PWD DRAWR"             TO LMC-DESC
             MOVE BROR-PASSWORD-DRAWER    TO LMC-ORG-TEXT
             MOVE BRCH-PASSWORD-DRAWER    TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-HIDE-END-BAL NOT = BRCH-HIDE-END-BAL
             MOVE "34 "                   TO LMC-FIELD
             MOVE "HIDE EBAL"             TO LMC-DESC
             MOVE BROR-HIDE-END-BAL       TO LMC-ORG-TEXT
             MOVE BRCH-HIDE-END-BAL       TO LMC-CHG-TEXT
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
       ENTER-SCANKEY SECTION.
      ******************************************************************

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
              MOVE BR-REC TO BROR-REC
              GO TO CLOSE-SCAN.

           MOVE VDUNUM0 TO BR-NO.
           PERFORM START-BR-FILE.

       READ-FILES-NEXT.

           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCAN.

           MOVE "SN N040BRANCH." TO VDU.
           MOVE BR-NO TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

           GO TO ENTER-KEY.

       CLOSE-SCAN.

           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
           PERFORM CLOSE-BR-FILE.

       END-SCAN.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE SPACES                 TO DISPLAY-BUF.
           MOVE BR-NAME                TO BRNAME.
           MOVE BR-STATE               TO BRSTATE.
           MOVE BR-COLL-ID-SCAN        TO F01.
           MOVE BR-PCNI-LEDGER         TO F02.
           MOVE BR-EMLNRG-PHONE        TO F03.
           MOVE BR-PRIO-MEMO-FLAG      TO F04.
           MOVE BR-AUTO-INS-EARN       TO F05.
           MOVE BR-OTH-INS-LEVELS      TO F06.
           MOVE BR-CHECK-EOM-FG        TO F07.
           MOVE BR-AVAIL-CREDIT        TO F08.
           MOVE BR-SALES-SMALL         TO F09.
           MOVE BR-ALDEL-FLAG          TO F10.
           MOVE BR-INS-ADDON           TO F11.
           MOVE BR-TRAILER-REC         TO F12.
           MOVE BR-AUTO-CHECK          TO F13.
           MOVE BR-LC-CODE             TO F14.
           MOVE BR-VALIDATE-PRUS       TO F15.
           MOVE BR-PREBILL-PL-JD-OPT   TO F16.
           MOVE BR-NEW-COLL-INQUIRY    TO F17.
           MOVE BR-CAN-LOANS-FG        TO F18.
           MOVE BR-CAN-GROUP-ID        TO F181.
           MOVE BR-POST-AHEAD-FG       TO F19.
           MOVE BR-UNITPER-FG          TO F20.
           MOVE BR-UNITPER-Y-D-FG      TO F21.
           MOVE BR-COLLECTOR-QUEUE     TO F22.
           MOVE BR-A50-CPCORP          TO F23.
           MOVE BR-LONPF-OPT-DISPLAY   TO F24.
           MOVE BR-GIFTNET             TO F25.
           MOVE BR-BALLOON-LOANS-FG    TO F26.
           MOVE BR-GIFTNET-LEDGER-LINE TO F27.
           MOVE BR-ONESTEP-RN-FG       TO F28.
           MOVE BR-PURGE-NOW-FLAG      TO F29.
           MOVE SPACES                 TO F30.
           MOVE BR-LATECHG-ON-ALLOTMENTS TO F31.
           MOVE BR-LATECHG-ON-LBOX     TO F32.
           MOVE BR-PASSWORD-DRAWER     TO F33.
           MOVE BR-HIDE-END-BAL        TO F34.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF BR-CAN-LOANS-FG NOT = "Y"
              AND BR-CAN-LOANS-FG NOT = "O"
              MOVE "S  A026181P." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A012181P2:S." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A020181D." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              PERFORM GET-CD
              MOVE "S  A026181P:S." TO VDU
              MOVE "CANNED LOAN GROUP ID     :" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A012181P2:S." TO VDU
              MOVE "DESCRIPTION:" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A020181D." TO VDU
              MOVE CD-DESC TO VDUBUF
              PERFORM SCREENIO.

      *******************************************
       CODE-SCAN SECTION.
      *******************************************
           MOVE SPACES TO CDSCAN-WINDOW-BUF.
           MOVE "CL"   TO CDSCAN-TYPE.
           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
       EXIT-CODE-SCAN.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TSTCANID.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO BROPM1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BROPM1-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BROPM1-SCREEN.
           DISPLAY BROPM1-SCREEN UPON BROPMN-WINDOW-HANDLE.
           MOVE BROPM1-FORM-FIELDS TO WR-BUF.
           MOVE BROPM1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE BROPM1-HELP--FILE TO HELP-LINK-FILE.
           MOVE BROPM1-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/BROPM1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       MISC-PARAGRAPHS SECTION.
      ***********************************************
       COPY "LIBGB/ACCESS.CPY".
       GET-CD.
           MOVE "CL"            TO CD-TYPE.
           MOVE BR-CAN-GROUP-ID TO CD-CODE.
           PERFORM READ-CD1-FILE.
           IF IO-BAD
              MOVE "NOT FOUND " TO  CD-DESC.
       SEND-Y-OR-N.
           MOVE "MUST BE 'Y' OR 'N'" TO MESS.
           PERFORM SEND-MESS.
       SEND-Y-OR-N-OR-O.
           MOVE "MUST BE 'Y' OR 'N' OR 'O'-ONLY" TO MESS.
           PERFORM SEND-MESS.
       BRANCH-NOT-FOUND.
           MOVE "BRANCH NOT FOUND!" TO MESS.
           PERFORM SEND-MESS.
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
           MOVE "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                                                   TO MESS.
           PERFORM SEND-MESS.

      **************************************
       IO-ROUTINES SECTION.
      **************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      **************************************
       END-ROUTINE SECTION.
      **************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BROPM1-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE BROPMN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
