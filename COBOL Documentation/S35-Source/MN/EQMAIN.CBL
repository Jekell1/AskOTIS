       IDENTIFICATION DIVISION.
       PROGRAM-ID.  EQMAIN.
      ******************************************************************
      *
      *                 (MN)
      *  DESCRIPTION:   COLLECTOR QUEUE EXTRACTION FILE MAINTENANCE
      *         NOTE:   THIS MAINTENANCE CREATES/CHANGES EAFILE FOR
      *                 RECORDS WITH EA-CQ-FG = "Y"; OTHER RECORDS
      *                 MAINTAINED BY EAMAIN.  SOME FIELDS INITIALIZED
      *                 TO "ALL" RANGES - SEE NEW-REC SECTION
      *
      *    USING FORM-PATHNAME EXIT-PATHNAME CHANGE-BRNO-FG.
      *       CHANGE-BRNO-FG = "Y" FROM CPMNMU(3. FROM 1ST MAINTENANCE MENU)
      *                                 #8. SET-UP COLLECTOR QUEUES 
      *       CHANGE-BRNO-FG = "N" FROM LPCPMU(4. COLLETION PROCESSING MENU)
      *                                 #12. COLLECTOR QUEUE SET-UP
      *
      *  REV:
      *  KEC 062496 ADDED EA-PL-IN-ACTIVE-BOTH; REVISED FORM.
      *  CLS 030597 ADDED EA-CONFIDENTIAL,EA-1ST-PMT-MISSES.
      *  BLV 032097 CHANGE TO USE OLD SCAN ROUTINE
      *   CS 070197 ADDED EA-LEGAL,EA-DEBTAID,EA-JUDGEMENT, EA-PROM-BREAK1
      *             EA-PROM-BREAK2
      *  BLV 070297 ADD BROKEN PROMISE OPTION & RANGE TO BREAK OUT BROKEN
      *             PROMISES & HAVE THEM FIRST IN COLLECTOR QUEUE
      *  MJD 112597 EXPANDED EA-REC TO 222 BYTES
      *   CS 042098 ADDED EA-ORIGIN, CHANGED FORM
      * KEC 2000.0427 {PR#01138} REGIONAL ACCEPTANCE; KEVIN MULLEN
      *                ADDED EA-PL-BEG-DATE AND EA-PL-END-DATE
      * KEC 2000.0620 {PR#01138} REGIONAL ACCEPTANCE [KEVIN MULLEN]
      *                HAD TO FIX A PROBLEM WITH EA-PL-BEG-DATE AND
      *                EA-PL-END-DATE.  BARB FOUND A PROBLEM WHEN
      *                DISPLAYING THE REC; INTERFERES WITH ORIGINAL-REC.
      *  BLV 001017 ADD EA-RCPOTS, EA-MISPAYS, BUMP FIELDS 21-33 TO
      *             23-35, WORLD PR# 244
      *  BLV 001206 VERIFY IF BRANCH IS GLOBAL QUEUING, SELECT CD-EA-GROUP
      *             DESIGNATED AS GLOBAL QUEUING, AND VICE VERSA.  ALSO,
      *             IF GLOBAL QUEUING, USE GLOBAL CP-PATH; TFC PR# 60
      *   CS 010507 ADDED EA-ALLOTMENTS
      *  BLV 011009 ADDED EA-LNDATE-MONTHS1, EA-LNDATE-MONTHS2,
      *             EA-MATURE-MONTHS1, EA-MATURE-MONTHS2; HAD TO COMPLETELY
      *             REVISE SCREEN TO FIT MORE FIELDS, REGWAN PR# 1587
      *  BLV 011011 CHANGED MONTHS SINCE LOAN DATE & MONTHS TIL MATURITY
      *             TO BE DAYS, REGWAN PR# 1587
      *  BLV 021029 ADDED DAY OF MONTH DUE RANGE AS FIELD 11, SHIFTED
      *             OTHER FIELDS DOWN, ALSO CHANGED CLEAR-EQFILE TO
      *             USE COPY MEMBER, REGACC PR# 0009
      *  BLV 021030 NOTE, CURRENTLY CHECKING FOR RANGE OF 20-20 FOR NEW
      *             DAY OF MONTH DUE RANGE IN CPRUN, CQRUN - SO WILL GIVE
      *             A WARNING HERE THAT IT WILL NOT WORK
      *  BLV 030630 MAKE VARIABLE PROMPTS FOR FIELDS 9 & 10, REGACC WILL
      *             USE FOR TRIAD PARAMETERS, REGACC PR# 0033
      *  BLF 060228 ADD EA-INCL-COLLECTOR AS FIELD 14, BUMP OTHER
      *             FIELDS DOWN, LENDMARK PR# 2719
      *  BAH 090211 THE FIELD SIZES FOR FIELD-291,292 AND 293 WERE WRONG
      *             CAUSING THE SCREEN TO DISPLAY JUNK, KEVIN/REGACC
      * MJD 131209 FIXED EXIT, WOULD NOT EXIT BACK TO MENU, MN/EQMAIN WAS IN
      *            FORM-NAM SO IT KEPT RELOADING.
      *
      * KEC 2016.0226 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED EA-CLASS FROM 2 TO 3 BYTES.
      *         FIXED KEYS & RECS FOR EA-REC (HOLD/ORIG/SAVE/WK).
      *
      * KEC 2016.0310 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED CD-EA-FILLER FOR CHANGE TO CD-EA-REC.
      * KEC 2016.0325 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      *  CS 180412 CHANGED ENTRY OF EA-ALPNAME1/EA-ALPNAME2 TO X(4)
      * KEC 2018-1026 CONVERTED CPFILE FOR A32 & A34
      *               CP-PATH-OVERRIDE TO "G" IF GLOBAL QUEUEING.
      * CS  191111 FIXED SCANKEY ENTRY
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/EQMAIN.CBL S35 02/21/24-12:34:44 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01EA.CPY".
       COPY "LIBLP/LP01EA_SQL.CPY".
       COPY "LIBLP/LPWSEA.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBSP/SP01CP.CPY".
       COPY "LIBSP/SP01CP_SQL.CPY".
       COPY "LIBSP/SPWSCP.CPY".

       01  ERRCD           PIC X       VALUE SPACES.
       01  CHAN            PIC X(4)    VALUE "CHAN".
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-SCAN      PIC 9       VALUE 0.
       01  BRANCH-NO       PIC 9(4)    VALUE 0.
       01  HOLD-EA-GQ-FG   PIC X       VALUE "N".

       01  HOLD-EA1-KEY                VALUE SPACES.
           03  HOLD-EA1-BRANCH         PIC 9(4).
           03  HOLD-EA1-CLASS          PIC 9(3).
           03  HOLD-EA1-GROUP-ID       PIC X(6).
           03  HOLD-EA1-SEQNO          PIC 99.

       01  HOLD-EA-PL-BEG-DATE       PIC 9(08).
       01  HOLD-EA-PL-END-DATE       PIC 9(08).
       01  D-KEY-LIST                PIC X(24) VALUE
           "BRNO CLS GROUP-ID SEQNO.".

      *01  WORK-KEY        PIC 9(8).
      *01  WORK-KEYX REDEFINES WORK-KEY PIC X(8).

       01  WORK-EA1-KEY                PIC X(15).
       01  WORK-EA1-KEYX               REDEFINES WORK-EA1-KEY.
           03  WORK-EA-BRANCH          PIC 9(4).
      *    03  WORK-EA-CLASS           PIC 9(3).
           03  WORK-EA-CLASS           PIC 9(2).
           03  WORK-EA-GROUP-ID        PIC X(6).
           03  WORK-EA-SEQNO           PIC 99.

      *-----------------------------------------------------------------

       78  FIELD-291X-FILLER         VALUE "ACT,INACT,BOTH:".
       78  FIELD-292X-FILLER         VALUE "BEG DATE:".
       78  FIELD-293X-FILLER         VALUE "END DATE:".

       01  FIELDS-29X-BUF.
           03  FIELD-291X            PIC X(15)   VALUE SPACES.
           03  FIELD-291             PIC X(01)   VALUE SPACES.
           03  FIELD-292X            PIC X(10)   VALUE SPACES.
           03  FIELD-292             PIC X(08)   VALUE SPACES.
           03  FIELD-292-DATE        REDEFINES   FIELD-292
                                     PIC 99/99/99.
           03  FIELD-293X            PIC X(10)   VALUE SPACES.
           03  FIELD-293             PIC X(08)   VALUE SPACES.
           03  FIELD-293-DATE        REDEFINES   FIELD-293
                                     PIC 99/99/99.

       01  FIELDS-29X-LIST.
           03  FILLER                PIC X(14)   VALUE "PROMPT291 291 ".
           03  FILLER                PIC X(14)   VALUE "PROMPT292 292 ".
           03  FILLER                PIC X(14)   VALUE "PROMPT293 293.".

       01  ORIGINAL-EA-REC             PIC X(EA-SIZE)   VALUE SPACES.
       01  CHANGED-EA-REC              PIC X(EA-SIZE)   VALUE SPACES.


      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC ZZZ9.
           03  F011        PIC ZZZ9.
           03  F02         PIC ZZZZ.99.
           03  F021        PIC ZZZZ.99.
           03  F03         PIC ZZZZZZ.99.
           03  F031        PIC ZZZZZZ.99.
           03  F04         PIC ZZZ9-.
           03  F041        PIC ZZZ9-.
           03  F05         PIC ZZZ9.
           03  F051        PIC ZZZ9.
           03  F06         PIC XXXX.
           03  F061        PIC XXXX.
           03  F07         PIC ZZZ9-.
           03  F071        PIC ZZZ9-.
           03  F08         PIC ZZZ9.99.
           03  F081        PIC ZZZ9.99.
           03  F09         PIC ZZZ9.
           03  F091        PIC ZZZ9.
           03  F10         PIC ZZZ9.
           03  F101        PIC ZZZ9.
           03  F11         PIC Z9.
           03  F111        PIC Z9.
           03  F12         PIC XXXX.
           03  F13         PIC XXXX.
           03  F14         PIC XXX.
           03  F15         PIC X.
           03  F16         PIC X.
           03  F17         PIC X.
           03  F18         PIC X.
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
           03  F291X       PIC X(15).
           03  F291        PIC X.
           03  F292X       PIC X(10).
           03  F292        PIC X(08).
           03  F292-DATE   REDEFINES F292
                           PIC 99/99/99.
           03  F293X       PIC X(10).
           03  F293        PIC X(08).
           03  F293-DATE   REDEFINES F293
                           PIC 99/99/99.
           03  F30         PIC X.
           03  FPROMPT301  PIC X(9).
           03  F301        PIC ZZZ9-.
           03  FPROMPT302  PIC X(9).
           03  F302        PIC ZZZ9-.
           03  F31         PIC XXX.
           03  F32         PIC X(60).


      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER      PIC X(40)   VALUE
           "010 011 020 021 030 031 040 041 050 051 ".
           03  FILLER      PIC X(40)   VALUE
           "060 061 070 071 080 081 090 091 100 101 ".
           03  FILLER      PIC X(44)   VALUE
           "110 111 120 130 140 150 160 170 180 190 200 ".
           03  FILLER      PIC X(36)   VALUE
           "210 220 230 240 250 260 270 280 290 ".
           03  FILLER      PIC X(42)   VALUE
           "PROMPT291 291 PROMPT292 292 PROMPT293 293 ".
           03  FILLER      PIC X(40)   VALUE
           "300 PROMPT301 301 PROMPT302 302 310 320.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      ******************************************************************
       01  MAX             PIC 99      VALUE 32.
       01  SPEC-TABLE.
      * 1. BEG PAST-DUE DAYS
           03  FILLER      PIC X(8)    VALUE "ENNN040 ".
      * 2. BEG PAST-DUE AMT
           03  FILLER      PIC X(8)    VALUE "ENNN042 ".
      * 3. BEG BALANCE DUE
           03  FILLER      PIC X(8)    VALUE "ENNN062 ".
      * 4. BEG PAST PROMISE
           03  FILLER      PIC X(8)    VALUE "ENNS040 ".
      * 5. BEG RECENCY DAYS
           03  FILLER      PIC X(8)    VALUE "ENNN040 ".
      * 6. BEG LAST NAME
           03  FILLER      PIC X(8)    VALUE "E NA040 ".
      * 7. BEGIN CONTRACTUAL
           03  FILLER      PIC X(8)    VALUE "ENNS040 ".
      * 8. BEGIN DLNO
           03  FILLER      PIC X(8)    VALUE "ENNN042 ".
      * 9. BEGIN DAYS SINCE LOAN DATE
           03  FILLER      PIC X(8)    VALUE "ENNN040 ".
      *10. BEGIN DAYS UNTIL MATURITY
           03  FILLER      PIC X(8)    VALUE "ENNN040 ".
      *11. BEGIN DAY OF THE MONTH DUE
           03  FILLER      PIC X(8)    VALUE "ENNN020 ".
      *12. ACTION CODE
           03  FILLER      PIC X(8)    VALUE "E NA040 ".
      *13. ORIGIN
           03  FILLER      PIC X(8)    VALUE "E NA040 ".
      *14. INCL COLLECTOR
           03  FILLER      PIC X(8)    VALUE "E NA040 ".
      *15. INCLUDE 0 RECENCY
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      *16. INCLUDE 30 DAY CONTRACTUAL POTS (I,S,O)
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      *17. INCLUDE 30 DAY RECENCY POTS (I,S,O)
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      *18. INCLUDE MISSED PAYS(I,S,O)
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      *19. MATURE ACCTS
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      *20. PROMISED ACCTS
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      *21. INCLUDE REPO ACCTS
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      *22. INCLUDE LEGAL ACCOUNTS
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      *23. INCLUDE DEBTAID ACCOUNTS
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      *24. INCLUDE BANKRUPT ACCOUNTS
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      *25. INCLUDE P & L ACCTS
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      *26. INCLUDE JUDGEMENT ACCOUNTS
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      *27. INCLUDE CONFIDENTIAL ACCOUNTS
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      *28. INCLUDE FIRST PAYMENT MISSES
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      *29. INCLUDE ALLOTMENTS
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      *30. BREAK OUT PROMISES
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      *31. COLLECTOR QUEUE POOL
           03  FILLER      PIC X(8)    VALUE "E NA030 ".
      *32. DESCRIPTION
           03  FILLER      PIC X(8)    VALUE "E NA600 ".

      *-----------------------------------------------------------------
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 32.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/EASCANW.CPY".
       COPY "LIBWI/CPSCANW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  EQMAIN-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/EQMAIN_DEF.CPY".
       COPY "LIBMN/EQMAIN_WKS.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  CHANGE-BRNO-FG  PIC X       VALUE "N".

       SCREEN SECTION.
       COPY "LIBMN/EQMAIN_SCN.CPY".


      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME CHANGE-BRNO-FG.

      ******************************************************************
      *
      *     M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EQMAIN" TO VDU-FORM-NAME.

      * IF THIS WAS STANDARD, IT WOULD NOT CLEAR PREVIOUS MENU
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
                   HANDLE IS EQMAIN-WINDOW-HANDLE.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
           MOVE EXT-FILPATH-BRANCH TO BRANCH-NO.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-EA-KEY-MODULE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-EA-KEY-MODULE SECTION.
      ******************************************************************
       REC-EA-KEY-MODULE-NO.

           MOVE 0 TO EA-BRANCH EA-CLASS EA-SEQNO.
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.


      *       CHANGE-BRNO-FG = "Y" FROM CPMNMU(3. FROM 1ST MAINTENANCE MENU)
      *                                 #8. SET-UP COLLECTOR QUEUES 
      *       CHANGE-BRNO-FG = "N" FROM LPCPMU(4. COLLETION PROCESSING MENU)
      *                                 #12. COLLECTOR QUEUE SET-UP

      * WHETHER OR NOT TO ALLOW ENTERING DIFFERENT BRANCH
      * NUMBERS; IN COLLECTION MENU, YOU CAN'T CHANGE BRNO; BUT IN
      * MAINTENANCE MENU, YOU CAN:
           IF CHANGE-BRNO-FG = "N"
              MOVE "SN N040BRNO." TO VDU
              MOVE BRANCH-NO TO EA-BRANCH VDUNUM0
              PERFORM SCREENIO
              PERFORM VERIFY-BRNO
              GO TO REC-EA-CLASS.

       ENTER-BRANCH.
           MOVE "EN N040BRNO." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1<" )
              PERFORM ENTER-SCANKEY
              GO TO END-REC-EA-KEY-MODULE.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-EA-KEY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-EA-KEY-MODULE-NO.

           MOVE VDUNUM0 TO EA-BRANCH.
           PERFORM VERIFY-BRNO.

           IF IO-FG NOT = 0
              GO TO REC-EA-KEY-MODULE-NO.

      *-----------------------------------------------------------------
       REC-EA-CLASS.

           MOVE "S  A200CLSDESC." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "EN N030CLS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" OR "1U"
              GO TO REC-EA-KEY-MODULE-NO.

           IF ( VDUSTATUS = "1<" )
              PERFORM ENTER-SCANKEY
              IF IO-TYPE = SPACES
                 PERFORM VERIFY-GROUP
                 IF IO-FG = 0
                    GO TO END-REC-EA-KEY-MODULE
                 ELSE
                    GO TO REC-EA-KEY-MODULE-NO
              ELSE
                 GO TO END-REC-EA-KEY-MODULE.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-EA-KEY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-EA-CLASS.

           MOVE VDUNUM0 TO EA-CLASS.
           PERFORM VERIFY-CLASS-CODE.

           IF IO-FG NOT = 0
              GO TO REC-EA-CLASS.

      *-----------------------------------------------------------------
       REC-EA-GROUP-ID.

           MOVE "E  A060GROUP-ID." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-REC-EA-KEY-MODULE.

           IF ( VDUSTATUS = "1U" )
              GO TO REC-EA-CLASS.

           IF ( VDUSTATUS = "1<" )
              PERFORM EA-GROUP-SCAN
              IF ( IO-FG = 9 )
                 GO TO REC-EA-GROUP-ID
              ELSE
                 MOVE CDSCAN-CODE TO VDUBUF EA-GROUP-ID
                 GO TO TEST-GROUP.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-EA-KEY-MODULE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUSTATUS NOT = "00" )
              GO TO REC-EA-GROUP-ID.

           MOVE VDUBUF TO EA-GROUP-ID.

      *- - - - - - - - - - - - - - - - TEST IF VALID GROUP ID  - - - - -
       TEST-GROUP.
           PERFORM VERIFY-GROUP.

           IF ( IO-FG NOT = 0 )
              GO TO REC-EA-GROUP-ID.

      *-----------------------------------------------------------------
       REC-EA-SEQNO.

           MOVE "EN N020SEQNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-REC-EA-KEY-MODULE.

           IF VDUSTATUS = "1U"
              GO TO REC-EA-GROUP-ID.

           IF ( VDUSTATUS = "1<" )
              PERFORM EXTRACTION-SCAN
              IF EASCAN-STATUS NOT = "00"
                 GO TO REC-EA-SEQNO
              ELSE
                 GO TO READ-EAFILE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-EA-SEQNO.

           MOVE VDUNUM0 TO EA-SEQNO.

           IF EA-SEQNO = 0
              PERFORM EA-NEXT-AVAIL-SEQNO.

       READ-EAFILE.

           MOVE EA1-KEY TO HOLD-EA1-KEY.

           PERFORM OPEN-EA1-FILE.
           PERFORM READ-EA1-FILE.
           PERFORM CLOSE-EA1-FILE.
           IF IO-FG = 0
              MOVE EA-REC TO ORIGINAL-EA-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-EA-KEY-MODULE.
           EXIT.

      ******************************************************************
       EA-NEXT-AVAIL-SEQNO SECTION.
      ******************************************************************

           PERFORM OPEN-EA1-FILE.

       EA-NEXT-AVAIL-CONT.

           ADD 1 TO EA-SEQNO.
           PERFORM READ-EA1-FILE.
           IF IO-FG = 0
              GO TO EA-NEXT-AVAIL-CONT.

           PERFORM CLOSE-EA1-FILE.

           MOVE "SN N020SEQNO." TO VDU.
           MOVE EA-SEQNO TO VDUNUM0.
           PERFORM SCREENIO.

       EA-NEXT-AVAIL-EXIT.
           EXIT.

      ******************************************************************
       EXTRACTION-SCAN SECTION.
      ******************************************************************
           MOVE BRANCH-NO   TO EASCAN-BRNO.
           MOVE EA-CLASS    TO EASCAN-CLASS.
           MOVE EA-GROUP-ID TO EASCAN-GROUP.
           MOVE "Y"         TO EASCAN-CQ-FG.

           MOVE "WI/EASCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EASCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF EASCAN-STATUS NOT = "00"
              GO TO EXIT-EXTRACTION-SCAN.

           MOVE EASCAN-KEY TO HOLD-EA1-KEY EA1-KEY.

           MOVE D-KEY-LIST   TO WR-LIST.
           MOVE HOLD-EA1-KEY TO WR-BUF.
           PERFORM CALL-WRFORM.

       EXIT-EXTRACTION-SCAN.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************

           PERFORM OPEN-EA1-FILE.

       ENTER-KEY.

           MOVE "E  A140SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           IF VDUSTATUS = "1<"
              GO TO READ-EAFILE-NEXT.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.

           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE EA1-KEY TO HOLD-EA1-KEY
                 MOVE EA-REC TO ORIGINAL-EA-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUBUF    TO WORK-EA1-KEY.
           IF CHANGE-BRNO-FG = "N"
              MOVE BRANCH-NO TO WORK-EA-BRANCH.
      *    MOVE WORK-EA1-KEYX TO EA1-KEY.

           MOVE WORK-EA-BRANCH   TO EA-BRANCH.
           MOVE WORK-EA-CLASS    TO EA-CLASS.
           MOVE WORK-EA-GROUP-ID TO EA-GROUP-ID.
           MOVE WORK-EA-SEQNO    TO EA-SEQNO.
           IF EA-BRANCH NOT NUMERIC
              MOVE 0 TO EA-BRANCH.
           IF EA-CLASS NOT NUMERIC
              MOVE 0 TO EA-CLASS.
           IF EA-SEQNO NOT NUMERIC
              MOVE 0 TO EA-SEQNO.

           PERFORM START-EA1-FILE.

      *-----------------------------------------------------------------
       READ-EAFILE-NEXT.

           IF FIRST-SCAN = 0
              PERFORM START-EA1-FILE
              MOVE 1 TO FIRST-SCAN.

           PERFORM READ-EA1-FILE-NEXT.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

      *    UNLOCK EA-FILE.

           IF CHANGE-BRNO-FG = "N"
              IF EA-BRANCH NOT = BRANCH-NO
                 MOVE "CLR" TO IO-TYPE
                 GO TO CLOSE-SCANKEY.

           IF EA-CQ-FG NOT = "Y"
              GO TO READ-EAFILE-NEXT.

           PERFORM DISPLAY-SCAN-KEY.

           PERFORM DISPLAY-FIELDS.
           GO TO ENTER-KEY.

      *-----------------------------------------------------------------
       DISPLAY-SCAN-KEY.

           MOVE "SN N040BRNO." TO VDU.
           MOVE EA-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM VERIFY-BRNO.

           MOVE "S  N030CLS." TO VDU.
           MOVE EA-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM VERIFY-CLASS-CODE.

           MOVE "S  A060GROUP-ID." TO VDU.
           MOVE EA-GROUP-ID TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  N020SEQNO." TO VDU.
           MOVE EA-SEQNO TO VDUNUM0.
           PERFORM SCREENIO.

       CLOSE-SCANKEY.
           PERFORM CLOSE-EA1-FILE.
           MOVE 0 TO FIRST-SCAN.

       END-SCANKEY.
           EXIT.

      ********************************************
       VERIFY-GROUP SECTION.
      ********************************************

           PERFORM OPEN-CD1-FILE.
           MOVE "EA"        TO CD-TYPE.
           MOVE EA-GROUP-ID TO CD-CODE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF ( IO-FG NOT = 0 )
              GO TO EXIT-VERIFY-GROUP.

           IF CD-EA-CQ-FG NOT = "Y"
              MOVE "PLEASE EDIT THIS GROUP IN EXTRACTION ACTION MAINT"
                                  TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO EXIT-VERIFY-GROUP.

           IF BR-GLOBAL-QUEUE = "Y"
              IF CD-EA-GQ-FG NOT = "Y"
                 MOVE "THIS GROUP IS NOT VALID FOR GLOBAL QUEUING!"
                    TO MESS
                 MOVE 9 TO IO-FG
                 GO TO EXIT-VERIFY-GROUP.

           IF BR-GLOBAL-QUEUE NOT = "Y"
              IF CD-EA-GQ-FG = "Y"
                 MOVE "THIS GROUP IS NOT VALID FOR LOCAL QUEUING!"
                    TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO EXIT-VERIFY-GROUP.

           MOVE CD-EA-GQ-FG TO HOLD-EA-GQ-FG.

       PASSWORD-REQUIRED.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE
           "EDITING THIS EXTRACTION IS NOT ALLOWED WITHOUT PASSWORD!!!"
                                                TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG.
       EXIT-VERIFY-GROUP.
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
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
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

       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

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
           IF ( VDUSTATUS = "1<" ) AND ( ELE = 31 )
              PERFORM POOL-SCAN
              IF ( IO-FG NOT = 0 )
                 GO TO ENTER-ELE
              ELSE
                 MOVE CPSCAN-KEY     TO VDUBUF
                 MOVE "00"           TO VDUSTATUS.
       
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.

       
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06
                 F-07 F-08 F-09 F-10 F-11 F-12
                 F-13 F-14 F-15 F-16 F-17 F-18
                 F-19 F-20 F-21 F-22 F-23 F-24
                 F-25 F-26 F-27 F-28 F-29 F-30
                 F-31 F-32
                 DEPENDING ON ELE.

       F-01.
           MOVE VDUNUM0 TO EA-PASTDUE-DAY1.
       F-011.
           MOVE "ENNN040011." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-011.
           MOVE VDUNUM0 TO EA-PASTDUE-DAY2.
           IF EA-PASTDUE-DAY1 NOT = 0
              IF EA-PASTDUE-DAY2 < EA-PASTDUE-DAY1
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-02.
           MOVE VDUNUM2 TO EA-PASTDUE-AMT1.
       F-021.
           MOVE "ENNN042021." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-021.
           MOVE VDUNUM2 TO EA-PASTDUE-AMT2.
           IF EA-PASTDUE-AMT1 NOT = 0
              IF EA-PASTDUE-AMT2 < EA-PASTDUE-AMT1
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-03.
           MOVE VDUNUM2 TO EA-ACCT-BAL1.
       F-031.
           MOVE "ENNN062031." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-031.
           MOVE VDUNUM2 TO EA-ACCT-BAL2.
           IF EA-ACCT-BAL1 NOT = 0
              IF EA-ACCT-BAL2 < EA-ACCT-BAL1
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF EA-DAY-OF-MO-DUE1 = 20
              IF EA-DAY-OF-MO-DUE2 = 20
                 MOVE "WARNING, THIS RANGE WILL NOT WORK" TO MESS
                 PERFORM SEND-MESS.
           GO TO CREATE-ROUTINE.
       
       F-04.
           MOVE VDUNUM0 TO EA-PASTPROM-DAY1.
       F-041.
           MOVE "ENNS040041." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-041.
           MOVE VDUNUM0 TO EA-PASTPROM-DAY2.
           IF EA-PASTPROM-DAY1 NOT = 0
              IF EA-PASTPROM-DAY2 < EA-PASTPROM-DAY1
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-05.
           MOVE VDUNUM0 TO EA-RECENCY-DAY1.
       F-051.
           MOVE "ENNN040051." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-051.
           MOVE VDUNUM0 TO EA-RECENCY-DAY2.
           IF EA-RECENCY-DAY1 NOT = 0
              IF EA-RECENCY-DAY2 < EA-RECENCY-DAY1
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-06.
           MOVE VDUBUF TO EA-ALPNAME1.
       F-061.
           MOVE "E NA040061." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-061.
           MOVE VDUBUF TO EA-ALPNAME2.
           IF EA-ALPNAME2 < EA-ALPNAME1
              MOVE " INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-07.
           MOVE VDUNUM0 TO EA-CONTRACTUAL-DAY1.
       F-071.
           MOVE "ENNS040071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-071.
           MOVE VDUNUM0 TO EA-CONTRACTUAL-DAY2.
           IF EA-CONTRACTUAL-DAY1 NOT = 0
              IF EA-CONTRACTUAL-DAY1 > EA-CONTRACTUAL-DAY2
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-08.
           MOVE VDUNUM2 TO EA-DLNO1.
       F-081.
           MOVE "ENNN042081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-081.
           MOVE VDUNUM2 TO EA-DLNO2.
           IF EA-DLNO1 NOT = 0
              IF EA-DLNO1 > EA-DLNO2
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-09.
           MOVE VDUNUM0 TO EA-LNDATE-DAYS1.
       F-091.
           MOVE "ENNN040091." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-091.
           MOVE VDUNUM0 TO EA-LNDATE-DAYS2.
           IF EA-LNDATE-DAYS2 NOT = 0
              IF EA-LNDATE-DAYS1 > EA-LNDATE-DAYS2
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-10.
           MOVE VDUNUM0 TO EA-MATURE-DAYS1.
       F-101.
           MOVE "ENNN040101." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-101.
           MOVE VDUNUM0 TO EA-MATURE-DAYS2.
           IF EA-MATURE-DAYS2 NOT = 0
              IF EA-MATURE-DAYS1 > EA-MATURE-DAYS2
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-11.
           MOVE VDUNUM0 TO EA-DAY-OF-MO-DUE1.
           IF EA-DAY-OF-MO-DUE1 > 31
                 MOVE "ENTER A DAY OF THE MONTH" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
       F-111.
           MOVE "ENNN020111." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-111.
           MOVE VDUNUM0 TO EA-DAY-OF-MO-DUE2.
           IF EA-DAY-OF-MO-DUE2 > 31
                 MOVE "ENTER A DAY OF THE MONTH" TO MESS
                 PERFORM SEND-MESS
                 GO TO F-111.
           IF EA-DAY-OF-MO-DUE1 > EA-DAY-OF-MO-DUE2
              MOVE " INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-12.
           MOVE VDUBUF TO EA-ACTION.
           GO TO CREATE-ROUTINE.
       
       F-13.
           MOVE VDUBUF TO EA-ORIGIN.
           GO TO CREATE-ROUTINE.
       
       F-14.
           MOVE VDUBUF TO EA-INCL-COLLECTOR.
           IF VDUBUF = SPACES
              MOVE "ENTER `???' TO INCLUDE ALL" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-15.
           MOVE VDUBUF TO EA-RECENCYDATE.
           IF VDUBUF NOT = "Y" AND NOT = "N"
              MOVE " ENTER: 'Y'-LOANDATE, 'N'-1STPYDATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-16.
           MOVE VDUBUF TO EA-CPOTS.
           IF EA-CPOTS NOT = "I"
            IF EA-CPOTS NOT = "S"
             IF EA-CPOTS NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-17.
           MOVE VDUBUF TO EA-RPOTS.
           IF EA-RPOTS NOT = "I"
            IF EA-RPOTS NOT = "S"
             IF EA-RPOTS NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-18.
           MOVE VDUBUF TO EA-MISPAYS.
           IF EA-MISPAYS NOT = "I"
            IF EA-MISPAYS NOT = "S"
             IF EA-MISPAYS NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-19.
           MOVE VDUBUF TO EA-MATURED.
           IF EA-MATURED NOT = "I"
            IF EA-MATURED NOT = "S"
             IF EA-MATURED NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-20.
           MOVE VDUBUF TO EA-PROMISED.
           IF EA-PROMISED NOT = "I"
            IF EA-PROMISED NOT = "S"
             IF EA-PROMISED NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-21.
           MOVE VDUBUF TO EA-REPO.
           IF EA-REPO NOT = "I"
            IF EA-REPO NOT = "S"
             IF EA-REPO NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-22.
           MOVE VDUBUF TO EA-LEGAL.
           IF EA-LEGAL NOT = "I"
            IF EA-LEGAL NOT = "S"
             IF EA-LEGAL NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-23.
           MOVE VDUBUF TO EA-DEBTAID.
           IF EA-DEBTAID NOT = "I"
            IF EA-DEBTAID NOT = "S"
             IF EA-DEBTAID NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-24.
           MOVE VDUBUF TO EA-BANKRUPT.
           IF EA-BANKRUPT NOT = "I"
            IF EA-BANKRUPT NOT = "S"
             IF EA-BANKRUPT NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-25.
           MOVE VDUBUF TO EA-JUDGEMENT.
           IF EA-JUDGEMENT NOT = "I"
            IF EA-JUDGEMENT NOT = "S"
             IF EA-JUDGEMENT NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-26.
           MOVE VDUBUF TO EA-CONFIDENTIAL-FG
           IF EA-CONFIDENTIAL-FG NOT  = "I"
            IF EA-CONFIDENTIAL-FG NOT = "S"
             IF EA-CONFIDENTIAL-FG NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-27.
           MOVE VDUBUF TO EA-1ST-PMT-MISSES
           IF EA-1ST-PMT-MISSES NOT = "I"
            IF EA-1ST-PMT-MISSES NOT = "S"
             IF EA-1ST-PMT-MISSES NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-28.
           MOVE VDUBUF TO EA-ALLOTMENTS
           IF EA-ALLOTMENTS NOT = "I"
            IF EA-ALLOTMENTS NOT = "S"
             IF EA-ALLOTMENTS NOT = "O"
              MOVE "ENTER : 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-29.
           MOVE VDUBUF TO EA-PL.
           IF ( EA-PL NOT = "I" ) AND
              ( EA-PL NOT = "S" ) AND
              ( EA-PL NOT = "O" )
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
      *- - - - - - - - - - - - - - - - - - - - -{ DISPLAY '28X' FIELDS }
           PERFORM DISPLAY-FIELDS-29X.
           IF ( EA-PL = "S" )
              MOVE SPACES TO EA-PL-IN-ACTIVE-BOTH
              MOVE ZEROES TO EA-PL-BEG-DATE
              MOVE ZEROES TO EA-PL-END-DATE
              GO TO CREATE-ROUTINE.
       F-291.
           MOVE "E  A010291." TO VDU.
           PERFORM SCREENIO.
           IF (        F1-KEY ) MOVE "CLR" TO IO-TYPE
                                GO TO END-ENTRY-MODULE.
           IF (        UP-KEY ) GO TO ENTER-ELE.
           IF ( NOT ENTER-KEY ) GO TO F-291.
           IF NOT ( VDUBUF = "A" OR "B" OR "I" )
              MOVE " ENTER: 'I'-INACTIVE, 'A'-ACTIVE, 'B'-BOTH" TO MESS
              PERFORM SEND-MESS
              GO TO F-291.
           MOVE VDUBUF TO EA-PL-IN-ACTIVE-BOTH.
       
       F-292.
           MOVE "ERNM060292." TO VDU.
           PERFORM SCREENIO.
           IF (        F1-KEY ) MOVE "CLR" TO IO-TYPE
           IF (        UP-KEY ) GO TO F-291.
           IF ( NOT ENTER-KEY ) GO TO F-292.
           IF ( VDUNUM0 = ZEROES )
              GO TO F-292.
           MOVE VDU-DATE-YYYYMMDD TO EA-PL-BEG-DATE.
       
       F-293.
           MOVE "ERNM060293." TO VDU.
           PERFORM SCREENIO.
           IF (        F1-KEY ) MOVE "CLR" TO IO-TYPE
           IF (        UP-KEY ) GO TO F-292.
           IF ( NOT ENTER-KEY ) GO TO F-293.
           IF ( VDUNUM0 = ZEROES )
              GO TO F-293.
           MOVE VDU-DATE-YYYYMMDD TO EA-PL-END-DATE.
           MOVE EA-PL-BEG-DATE TO NUM-DATE.
           MOVE EA-PL-END-DATE TO SYS-DATE.
           IF EA-PL-END-DATE < EA-PL-BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO F-292.
           GO TO CREATE-ROUTINE.
       
       F-30.
           MOVE VDUBUF TO EA-BROKEN-PROM-FG.
           IF EA-BROKEN-PROM-FG NOT = "Y" AND NOT = "N"
              MOVE " ENTER 'Y' OR 'N' " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF EA-BROKEN-PROM-FG = "N"
              MOVE "S  A000PROMPT301." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000301." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000PROMPT302." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000302." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
       F-301.
           MOVE "S  A000PROMPT301." TO VDU.
           MOVE "BEG PROM DAYS :" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "ENNS040301." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1U" )
              GO TO F-30.
           IF ( VDUSTATUS NOT = "00" AND NOT = "1D" )
              GO TO F-301.
           MOVE VDUNUM0 TO EA-PROM-BREAK1.
       F-302.
           MOVE "S  A000PROMPT302." TO VDU.
           MOVE "END PROM DAYS :" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "ENNS040302." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1U" )
              GO TO F-301.
           IF ( VDUSTATUS NOT = "00" AND NOT = "1D" )
              GO TO F-302.
           MOVE VDUNUM0 TO EA-PROM-BREAK2.
           GO TO CREATE-ROUTINE.
       
       F-31.
           MOVE VDUBUF TO EA-COL-POOL.
           IF ( EA-LN-COL-ID NOT = "===" )
              GO TO CREATE-ROUTINE.
           IF HOLD-EA-GQ-FG = "Y"
              MOVE EXT-FILPATH-BASE    TO CP-GLOBAL-BASE
              MOVE EXT-FILPATH-MACHINE TO CP-GLOBAL-MACHINE
              MOVE "G"                 TO CP-PATH-OVERRIDE 
              MOVE CP-GLOBAL-PATH      TO CP-PATH
           ELSE
              MOVE " "                 TO CP-PATH-OVERRIDE.

           MOVE EA-COL-POOL TO CP-ID.
           PERFORM OPEN-CP1-FILE.
           PERFORM READ-CP1-FILE.
           PERFORM CLOSE-CP1-FILE.
           IF IO-FG NOT = 0
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       
       F-32.
           MOVE VDUBUF TO EA-DESC.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      ************************************
       WRITE-MODULE SECTION.
      ************************************
           PERFORM OPEN-EA1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.
           MOVE EA-REC TO CHANGED-EA-REC.
           IF ORIGINAL-EA-REC = CHANGED-EA-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-EA1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF EA-REC NOT = ORIGINAL-EA-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-EA-REC TO EA-REC.
           PERFORM REWRITE-EA1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-EA1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-EA1-FILE.

      ************************************
       DELETE-MODULE SECTION.
      ************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-EA1-FILE.
           PERFORM READ-EA1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF EA-REC NOT = ORIGINAL-EA-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-EA1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-EA1-FILE.

      ************************************
       NEW-RECORD-SETUP SECTION.
      ************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-EA-FILE.

           MOVE EXT-JULIAN-BEG TO EA-LSTFM-DATE1.
           MOVE EXT-JULIAN-END TO EA-LSTFM-DATE2.
           MOVE 0 TO EA-LSTFM-NO1.
           MOVE 9999 TO EA-LSTFM-NO2.
           MOVE "N" TO EA-INC-CNT EA-SET-LN-COL.
           MOVE "====" TO EA-SETACT-CD.
           MOVE "===" TO EA-LN-COL-ID.
           MOVE "Y" TO EA-CQ-FG.
           MOVE HOLD-EA1-KEY TO EA1-KEY.
           MOVE 0 TO ELE.

      *********************************************
       DISPLAY-FIELDS SECTION.
      *********************************************

           MOVE EA-PASTDUE-DAY1     TO F01.
           MOVE EA-PASTDUE-DAY2     TO F011.
           MOVE EA-PASTDUE-AMT1     TO F02.
           MOVE EA-PASTDUE-AMT2     TO F021.
           MOVE EA-ACCT-BAL1        TO F03.
           MOVE EA-ACCT-BAL2        TO F031.
           MOVE EA-PASTPROM-DAY1    TO F04.
           MOVE EA-PASTPROM-DAY2    TO F041.
           MOVE EA-RECENCY-DAY1     TO F05.
           MOVE EA-RECENCY-DAY2     TO F051.
           MOVE EA-ALPNAME1         TO F06.
           MOVE EA-ALPNAME2         TO F061.
           MOVE EA-CONTRACTUAL-DAY1 TO F07.
           MOVE EA-CONTRACTUAL-DAY2 TO F071.
           MOVE EA-DLNO1            TO F08.
           MOVE EA-DLNO2            TO F081.
           MOVE EA-LNDATE-DAYS1     TO F09.
           MOVE EA-LNDATE-DAYS2     TO F091.
           MOVE EA-MATURE-DAYS1     TO F10.
           MOVE EA-MATURE-DAYS2     TO F101.
           MOVE EA-DAY-OF-MO-DUE1   TO F11.
           MOVE EA-DAY-OF-MO-DUE2   TO F111.
           MOVE EA-ACTION           TO F12.
           MOVE EA-ORIGIN           TO F13.
           MOVE EA-INCL-COLLECTOR   TO F14.
           MOVE EA-RECENCYDATE      TO F15.
           MOVE EA-CPOTS            TO F16.
           MOVE EA-RPOTS            TO F17.
           MOVE EA-MISPAYS          TO F18.
           MOVE EA-MATURED          TO F19.
           MOVE EA-PROMISED         TO F20.
           MOVE EA-REPO             TO F21.
           MOVE EA-LEGAL            TO F22.
           MOVE EA-DEBTAID          TO F23.
           MOVE EA-BANKRUPT         TO F24.
           MOVE EA-JUDGEMENT        TO F25.
           MOVE EA-CONFIDENTIAL-FG  TO F26.
           MOVE EA-1ST-PMT-MISSES   TO F27.
           MOVE EA-ALLOTMENTS       TO F28.
           MOVE EA-PL               TO F29.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EA-PL-BEG-DATE NOT NUMERIC ) OR
              ( EA-PL-BEG-DATE = 02020202    ) OR
              ( EA-PL-BEG-DATE = 20202020    )
              MOVE ZEROES         TO HOLD-EA-PL-BEG-DATE
                                     HOLD-EA-PL-END-DATE
           ELSE
              MOVE EA-PL-BEG-DATE TO HOLD-EA-PL-BEG-DATE
              MOVE EA-PL-END-DATE TO HOLD-EA-PL-END-DATE.

           IF ( EA-PL = "S" )
              MOVE SPACES                  TO F291X
              MOVE SPACES                  TO F291
              MOVE SPACES                  TO F292X
              MOVE SPACES                  TO F292
              MOVE SPACES                  TO F293X
              MOVE SPACES                  TO F293
           ELSE
              MOVE FIELD-291X-FILLER       TO F291X
              MOVE EA-PL-IN-ACTIVE-BOTH    TO F291
              MOVE FIELD-292X-FILLER       TO F292X
              MOVE HOLD-EA-PL-BEG-DATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY             TO F292-DATE
              MOVE FIELD-293X-FILLER       TO F293X
              MOVE HOLD-EA-PL-END-DATE     TO F293-DATE 
              MOVE HOLD-EA-PL-END-DATE     TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY             TO F293-DATE.
        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EA-BROKEN-PROM-FG TO F30.
           IF EA-BROKEN-PROM-FG = "Y"
                MOVE "BEG PROM DAYS :" TO FPROMPT301
                MOVE EA-PROM-BREAK1 TO F301
                MOVE "END PROM DAYS :" TO FPROMPT302
                MOVE EA-PROM-BREAK2 TO F302.
           MOVE EA-COL-POOL TO F31.
           MOVE EA-DESC     TO F32.

       
           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

      *********************************************
       DISPLAY-FIELDS-29X SECTION.
      *********************************************

           IF ( EA-PL = "S" )
              MOVE SPACES TO FIELD-291X
              MOVE SPACES TO FIELD-291
              MOVE SPACES TO FIELD-292X
              MOVE SPACES TO FIELD-292
              MOVE SPACES TO FIELD-293X
              MOVE SPACES TO FIELD-293
              GO TO DISPLAY-FIELDS-29X-SEND.

      *- - - - - - - - - - - - - - - - - - - - -{ (I)NCLUDE OR (B)OTH  }
           IF ( EA-PL-BEG-DATE NOT NUMERIC ) OR
              ( EA-PL-BEG-DATE = 02020202    ) OR
              ( EA-PL-BEG-DATE = 20202020    )
              MOVE ZEROES         TO HOLD-EA-PL-BEG-DATE
                                     HOLD-EA-PL-END-DATE
           ELSE
              MOVE EA-PL-BEG-DATE TO HOLD-EA-PL-BEG-DATE
              MOVE EA-PL-END-DATE TO HOLD-EA-PL-END-DATE.

           MOVE FIELD-291X-FILLER    TO FIELD-291X.
           MOVE EA-PL-IN-ACTIVE-BOTH TO FIELD-291.
           MOVE FIELD-292X-FILLER    TO FIELD-292X.
           MOVE HOLD-EA-PL-BEG-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY          TO FIELD-292-DATE.
           MOVE FIELD-293X-FILLER    TO FIELD-293X.
           MOVE HOLD-EA-PL-END-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY          TO FIELD-293-DATE.
       
       DISPLAY-FIELDS-29X-SEND.

           MOVE FIELDS-29X-LIST TO WR-LIST.
           MOVE FIELDS-29X-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-29X-EXIT.
           EXIT.

      *********************************************
       EA-GROUP-SCAN SECTION.
      *********************************************
           MOVE "EA"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.
           MOVE "Y"    TO CDSCAN-EA-CQ-FG.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
               MOVE 9 TO IO-FG
               GO TO EXIT-GROUP-SCAN.

           MOVE 0 TO IO-FG.
           MOVE "S  A060GROUP-ID." TO VDU.
           MOVE CDSCAN-CODE        TO VDUBUF.
           PERFORM SCREENIO.

       EXIT-GROUP-SCAN.
           EXIT.

      *********************************************
       POOL-SCAN SECTION.
      *********************************************

           MOVE EA-BRANCH TO CPSCAN-BRNO.
           IF HOLD-EA-GQ-FG = "Y"
              MOVE "G"    TO CPSCAN-TYPE
           ELSE
              MOVE " "    TO CPSCAN-TYPE.

           MOVE "WI/CPSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CPSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CPSCAN-STATUS NOT = "00"
              MOVE 9 TO IO-FG
              GO TO EXIT-POOL-SCAN.

           MOVE 0 TO IO-FG.
           MOVE "S  A030310." TO VDU.
           MOVE CPSCAN-KEY TO VDUBUF.
           PERFORM SCREENIO.
       EXIT-POOL-SCAN.
           EXIT.

      *********************************************
       VERIFY-PASSWORD SECTION.
      *********************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE EA-GROUP-ID TO PASSWDW-PGM.
           MOVE 0 TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO EDIT THIS EXTRACTION:" TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBLP/LPEACL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO EQMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EQMAIN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY EQMAIN-SCREEN UPON EQMAIN-WINDOW-HANDLE.
           MOVE EQMAIN-FORM-FIELDS TO WR-BUF.
           MOVE EQMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/EQMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************
       SETUP-LINK-INFO SECTION.
      ****************************************
           MOVE EQMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE EQMAIN-HELP--DIR TO HELP-LINK-DIR.

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       VERIFY-BRNO.
           MOVE EA-BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO BR-NAME.
           MOVE "S  A200BRDESC." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.
       VERIFY-CLASS-CODE.
           MOVE EA-CLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO LC-DESC.
           MOVE "S  A020CLSDESC." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.
       GET-FD-RECORD.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
           IF IO-FG NOT = 0
              MOVE " INVALID FORM NUMBER" TO MESS
              PERFORM SEND-MESS.

      ***************************************
       IO-ROUTINES SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CP1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-EA1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBSP/SPCPGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPEAGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPEA1IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBSP/SPCP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN  
           DESTROY EQMAIN-SCREEN.        
           DESTROY EQMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
