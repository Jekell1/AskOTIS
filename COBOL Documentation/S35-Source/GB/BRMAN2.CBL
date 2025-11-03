       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BRMAN2.
      *********************************************************
      *  DIR :   GB
      *  DESC:   BRANCH FILE MAINTENANCE PAGE 2
      *
      * REV:
      *  JTG 071395 ADDED BR-DEFAULT-SC-SUBCLASS
      *  BAH 083195 REMOVED BR-CASH-METHOD-ONLY AND IB-ACCRUALS
      *             (MOVED TO GP RECORD)
      * MJD 110295  ADDED OPTION TO PRINT SUBCLASS INSTEAD OF LAW
      *              CODE (OPTION "C")
      * MJD 011296 ADDED BR-NO-OUT-DLR-RSV.
      * KEC 112096 ADDED BR-LEDGER-JUST-INTCHG FOR QUICK CREDIT,
      *             FOR "MQ" AND "MR" TO ONLY PRINT INTCHG IN THE
      *             FINANCE CHARGE BOX.  FOR COPYING LP/LONPMQ FROM A48.
      * MJD 120996 ADDED "O" OPTION TO BR-LOAN-NUM-FLAG.
      *            - DISPLAY BW-SOLICIT IN FIRST POSITION OF LPSTAT.
      *  MJD 112197 EXPANDED GB-REC TO 850 BYTES
      *  KEC 121797 ADDED PAGE 7.
      * BAH 022698 ADDED BR-ADDON-FEECD-O3 PIC XX OCCURS 2
      *  MJD 031398 ADDED 78 LEVEL: BR-SIZE
      *  BAH 042998 REMOVED BR-ADDON-FEECD(1) AND (2) AND RENAMED (3) AND (4)
      *             TO BR-ADDON-FEECD-O2(1) AND (2)
      *  BAH 990413 ADDED CALL TO CREATE CHANGES FILE AFTER REWRITE
      *  MG  991005 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BLV 991124 CHANGE BR-CPCORP-ORDER, SEQ 4 FOR I-AM-WORLD ONLY
      *             FOR RENEWABLE BY CASH AVAILABLE, WORLD PR# 214
      *  BAH 011016 ADD BR-CPCORP-ORDER 7 SMARTCHOICE(FFG), PR# 1613
      *             BY DEALER NUMBER, LAST NAME
      *  JTG 031103 NEW ENTRY FOR 'BR-INC-INS = L' WHICH WILL MEAN
      *             TO PROMPT TO KEEP INSURANCE IN FORCE FOR
      *             CREDIT LIFE ONLY. ALSO ASK QUESTION NOT ONLY
      *             IN PAYOFF POSTING BUT ALSO IN INQUIRY     FIN.MANMENT #2136
      *  AMM 070307 CREATED A LOG FIELD CHANGE ROUTINE
      *  BAH 120514 REMOVED SPECIAL-FG
      *  BAH 20211116 LIMIT ACCESS BY PASSWORD TO ALL FIELDS, #1524
      *************************************************************************
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
           "@(#)GB/BRMAN2.CBL S35 09/20/22-12:49:33 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ORIGINAL BRANCH RECORD
      *    CHANGED BRANCH RECORD
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
       01  ELE             PIC 9(2)    VALUE 0.
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  FORM-NUM REDEFINES ACTION.
           03  FORM-KEY    PIC X(4).
           03  FORM-P      PIC X.
           03  FORM-SEL    PIC 9.

       01  IO-TYPE         PIC X(3)   VALUE SPACES.
       01  NEW-FLAG        PIC 9      VALUE 0.

       01  ORIGINAL-BR-REC    PIC X(BR-SIZE).
       01  CHANGED-BR-REC     PIC X(BR-SIZE).

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
       01  DISPLAY-BUF    VALUE SPACES.
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
           03  F19         PIC X.
           03  F20         PIC X.
           03  F21         PIC X.
           03  F22         PIC XX.
           03  F221        PIC XX.
           03  F222        PIC XX.
           03  F223        PIC XX.
           03  F23         PIC X.
           03  F24         PIC X.
           03  F25         PIC X.
           03  F26         PIC X.
           03  F27         PIC X.
           03  F28         PIC X.
           03  F29         PIC X.
           03  F30         PIC X.
           03  F31         PIC X.
           03  F32         PIC Z9.
           03  F33         PIC Z9.
           03  F34         PIC X.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(40) VALUE
           "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER      PIC X(40) VALUE
           "210 220 221 222 223 230 240 250 260 270 ".
           03  FILLER      PIC X(28) VALUE
           "280 290 300 310 320 330 340.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS #1
      *    **********************************
       01  MAX             PIC 99   VALUE 34.
       01  SPEC-TABLE.
      * 1. BR-LCARDFG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 2. BR-DLRCHK
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 3. NOT USED
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 4. BR-NEGDLRSV
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 5. NOT USED
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 6. BR-ALLOTMNTS
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 7. BR-EXTRACT-AUTO-UPD
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 8. BR-ACCNNT-NUM-SEQ
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      * 9. BR-LOAN-NUM-FLAG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *10. BR-DOWNPY-FLAG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *11. BR-UNER-DLANRP
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *12. BR-SECOND-PROMISE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *13. BR-POST-GL
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *14. BR-OTHER-BRANCH-GL
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *15. BR-INC-WAIVERS
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *16. BR-INC-INS
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *17. BR-AGE-ERN
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *18. BR-CHANGE-MEMO
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *19. BR-IB-LCHG-MESS
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *20. BR-FRON-END-FG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *21. BR-DLPARV-ADJ-FG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *22. BR-ADDON-FEECODES
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      *23. BR-RECEIPT-PROMDATE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *24. BR-RECEIPT-INTPDS
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *25. BR-OUR-FEE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *26. BR-STOP-ON-COLL
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *27. BR-LEDGER-JUST-INTCHG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *28. NOT USED
           03  FILLER      PIC X(8) VALUE "E  A010N".
      *29. BR-VOLUME-DLANRP
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *30. BR-PDTH-NXDUE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *31. BR-OVERRIDE-BR-STATE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *32. BR-DEFAULT-SC-CLASS
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *33. BR-DEFAULT-SC-SUBCLASS
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *34. BR-RECDEL-NOT REC
           03  FILLER      PIC X(8) VALUE "E  A010 ".

       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 34 TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

       COPY "LIBGB/PASSWDW.CPY".
       78  FIELDS-SEQ                VALUE 0.
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/BRMAN2_DEF.CPY".
       COPY "LIBGB/BRMAN2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBGB/BRMAN2_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *************************************
       MAIN-PROGRAM SECTION.
      *************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BRMAN2" TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO BRMAIN-LINK.

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
              MOVE 3 TO FORM-SEL.
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
              MOVE "S  A040BRANCH." TO VDU
              MOVE BR-NO TO VDUBUF
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

      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
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
              IF NOT (FORM-SEL = 1 OR 3 OR 4 OR 5 OR 6 OR 7)
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
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07
                 F-08 F-09 F-10 F-11 F-12 F-13 F-14
                 F-15 F-16 F-17 F-18 F-19 F-20 F-21
                 F-22 F-23 F-24 F-25 F-26 F-27 F-28
                 F-29 F-30 F-31 F-32 F-33 F-34
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-LCARDFG.
           GO TO CREATE-ROUTINE.
       F-02.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-DLRCHK.
           GO TO CREATE-ROUTINE.
       F-03.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-NO-OUT-DLR-RSV.
           GO TO CREATE-ROUTINE.
       F-04.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-NEGDLRSV.
           GO TO CREATE-ROUTINE.
       F-05.
           GO TO CREATE-ROUTINE.
       F-06.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-ALLOTMNTS.
           GO TO CREATE-ROUTINE.
       F-07.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-EXTRACT-AUTO-UPD.
           GO TO CREATE-ROUTINE.
       F-08.
           IF VDUBUF NOT = "1" AND
              VDUBUF NOT = "2" AND
              VDUBUF NOT = "3" AND
              VDUBUF NOT = "4" AND
              VDUBUF NOT = "5" AND
              VDUBUF NOT = "6" AND
              VDUBUF NOT = "7"
                 MOVE "ENTER A VALUE FROM 1 TO 7 " TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
           ELSE
              MOVE VDUBUF TO BR-CPCORP-ORDER
              PERFORM SEND-CPCORP-TYPE
              GO TO CREATE-ROUTINE.

       F-09.
           IF VDUBUF NOT = "L" AND
                     NOT = "S" AND
                     NOT = "C" AND
                     NOT = "O" AND
                     NOT = "N"
              STRING "L=LAWCODE, " "S=SEQ NUMBER, " "C=SUBCLASS, "
                     "O=SOLICIT, " "N=NEITHER"
                    DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-LOAN-NUM-FLAG.
           GO TO CREATE-ROUTINE.
       F-10.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-DOWNPY-FLAG.
           GO TO CREATE-ROUTINE.
       F-11.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-UNER-DLANRP.
           GO TO CREATE-ROUTINE.
       F-12.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-SECOND-PROMISE.
           GO TO CREATE-ROUTINE.
       F-13.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-POST-GL.
           GO TO CREATE-ROUTINE.
       F-14.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-OTHER-BRANCH-GL.
           GO TO CREATE-ROUTINE.
       F-15.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-INC-WAIVERS.
           GO TO CREATE-ROUTINE.
       F-16.
           IF NOT (VDUBUF = "Y" OR "N" OR "L")
              PERFORM Y-OR-N-OR-L
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-INC-INS.
           GO TO CREATE-ROUTINE.
       F-17.
           IF VDUBUF NOT = "T" AND VDUBUF NOT = "U"
              MOVE "MUST BE 'T' OR 'U'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-AGE-ERN.
           GO TO CREATE-ROUTINE.
       F-18.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-CHANGE-MEMO.
           GO TO CREATE-ROUTINE.
       F-19.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-IB-LCHG-MESS.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUBUF TO BR-FRONT-END-FG.
           IF BR-FRONT-END-FG NOT = "Y" AND
              BR-FRONT-END-FG NOT = "N"
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUBUF TO BR-DLPARV-ADJ-FG.
           IF NOT (BR-DLPARV-ADJ-FG = "C" OR "D" OR "N")
              MOVE "ENTER 'C' OR 'D' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUBUF TO BR-ADDON-FEECD-O2(1).
       F-221.
           MOVE "E  A020221." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT ENTER-KEY
              GO TO F-221.
           MOVE VDUBUF TO BR-ADDON-FEECD-O2(2).

       F-222.
           MOVE "E  A020222." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO F-221.
           IF NOT ENTER-KEY
              GO TO F-222.
           MOVE VDUBUF TO BR-ADDON-FEECD-O3(1).

       F-223.
           MOVE "E  A020223." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO F-222.
           IF NOT ENTER-KEY
              GO TO F-223.
           MOVE VDUBUF TO BR-ADDON-FEECD-O3(2).

           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDUBUF TO BR-RECEIPT-PROMDATE.
           IF NOT (BR-RECEIPT-PROMDATE = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDUBUF TO BR-RECEIPT-INTPD.
           IF NOT (BR-RECEIPT-INTPD = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE VDUBUF TO BR-OUR-FEE.
           IF NOT (BR-OUR-FEE = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-26.
           MOVE VDUBUF TO BR-STOP-ON-COLL.
           IF NOT (BR-STOP-ON-COLL = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-27.
           MOVE VDUBUF TO BR-LEDGER-JUST-INTCHG.
           IF NOT (BR-LEDGER-JUST-INTCHG = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-28.
      *    MOVE VDUBUF TO BR-IB-ACCRUALS-DONE.
      *    IF NOT (BR-IB-ACCRUALS-DONE = "Y" OR "N")
      *       PERFORM Y-OR-N
      *       GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-29.
           MOVE VDUBUF TO BR-VOLUME-DLANRP.
           IF NOT (BR-VOLUME-DLANRP = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-30.
           MOVE VDUBUF TO BR-PDTH-NXDUE.
           IF NOT (BR-PDTH-NXDUE = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-31.
           MOVE VDUBUF TO BR-OVERRIDE-BR-STATE.
           IF NOT (BR-OVERRIDE-BR-STATE = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-32.
           MOVE VDUNUM0 TO BR-DEFAULT-SC-CLASS.
           GO TO CREATE-ROUTINE.
       F-33.
           MOVE VDUNUM0 TO BR-DEFAULT-SC-SUBCLASS.
           GO TO CREATE-ROUTINE.
       F-34.
           MOVE VDUBUF TO BR-RECDEL-NOT-REC
           IF NOT (BR-RECDEL-NOT-REC = "Y" OR "N")
              PERFORM Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL.
       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " "                          TO PASSWDW-STAT.
           MOVE "BRMAN2"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       EXIT-VERIFY-PASSWORD.
           EXIT.

      *************************************************
       WRITE-MODULE SECTION.
      *************************************************
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

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE CHANGED-BR-REC TO BR-REC.
           PERFORM REWRITE-BR-FILE.

       END-WRITE-MODULE.
           PERFORM CLOSE-BR-FILE.

      *************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      *************************************************

           MOVE ORIGINAL-BR-REC               TO BROR-REC.
           MOVE CHANGED-BR-REC                TO BRCH-REC.

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE BR-NO                      TO LMC-BRNO.
           MOVE " TO "                     TO LMC-TO-X.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BROR-LCARDFG NOT = BRCH-LCARDFG
             MOVE "01 "                   TO LMC-FIELD
             MOVE "LEDG CARD"             TO LMC-DESC
             MOVE BROR-LCARDFG            TO LMC-ORG-TEXT
             MOVE BRCH-LCARDFG            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-DLRCHK NOT = BRCH-DLRCHK
             MOVE "02 "                   TO LMC-FIELD
             MOVE "DLR CHECK"             TO LMC-DESC
             MOVE BROR-DLRCHK             TO LMC-ORG-TEXT
             MOVE BRCH-DLRCHK             TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-NO-OUT-DLR-RSV NOT = BRCH-NO-OUT-DLR-RSV
             MOVE "03 "                   TO LMC-FIELD
             MOVE "DLR RESRV"             TO LMC-DESC
             MOVE BROR-NO-OUT-DLR-RSV     TO LMC-ORG-TEXT
             MOVE BRCH-NO-OUT-DLR-RSV     TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-NEGDLRSV NOT = BRCH-NEGDLRSV
             MOVE "04 "                   TO LMC-FIELD
             MOVE "NEG RESRV"             TO LMC-DESC
             MOVE BROR-NEGDLRSV           TO LMC-ORG-TEXT
             MOVE BRCH-NEGDLRSV           TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ALLOTMNTS NOT = BRCH-ALLOTMNTS
             MOVE "06 "                   TO LMC-FIELD
             MOVE "ALLOTMNTS"             TO LMC-DESC
             MOVE BROR-ALLOTMNTS          TO LMC-ORG-TEXT
             MOVE BRCH-ALLOTMNTS          TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-EXTRACT-AUTO-UPD NOT = BRCH-EXTRACT-AUTO-UPD
             MOVE "07 "                   TO LMC-FIELD
             MOVE "AUTO UPDT"             TO LMC-DESC
             MOVE BROR-EXTRACT-AUTO-UPD   TO LMC-ORG-TEXT
             MOVE BRCH-EXTRACT-AUTO-UPD   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-CPCORP-ORDER NOT = BRCH-CPCORP-ORDER
             MOVE "08 "                   TO LMC-FIELD
             MOVE "PRNT ORDR"             TO LMC-DESC
             MOVE BROR-CPCORP-ORDER       TO LMC-ORG-TEXT
             MOVE BRCH-CPCORP-ORDER       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-LOAN-NUM-FLAG NOT = BRCH-LOAN-NUM-FLAG
             MOVE "09 "                   TO LMC-FIELD
             MOVE "LOAN NUM "             TO LMC-DESC
             MOVE BROR-LOAN-NUM-FLAG      TO LMC-ORG-TEXT
             MOVE BRCH-LOAN-NUM-FLAG      TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-DOWNPY-FLAG NOT = BRCH-DOWNPY-FLAG
             MOVE "10 "                   TO LMC-FIELD
             MOVE "DOWNPAY  "             TO LMC-DESC
             MOVE BROR-DOWNPY-FLAG        TO LMC-ORG-TEXT
             MOVE BRCH-DOWNPY-FLAG        TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-UNER-DLANRP NOT = BRCH-UNER-DLANRP
             MOVE "11 "                   TO LMC-FIELD
             MOVE "TRUE UNER"             TO LMC-DESC
             MOVE BROR-UNER-DLANRP        TO LMC-ORG-TEXT
             MOVE BRCH-UNER-DLANRP        TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-SECOND-PROMISE NOT = BRCH-SECOND-PROMISE
             MOVE "12 "                   TO LMC-FIELD
             MOVE "2ND PROM "             TO LMC-DESC
             MOVE BROR-SECOND-PROMISE     TO LMC-ORG-TEXT
             MOVE BRCH-SECOND-PROMISE     TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-POST-GL NOT = BRCH-POST-GL
             MOVE "13 "                   TO LMC-FIELD
             MOVE "OWNBR GL "             TO LMC-DESC
             MOVE BROR-POST-GL            TO LMC-ORG-TEXT
             MOVE BRCH-POST-GL            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-OTHER-BRANCH-GL NOT = BRCH-OTHER-BRANCH-GL
             MOVE "14 "                   TO LMC-FIELD
             MOVE "DISBURSE "             TO LMC-DESC
             MOVE BROR-OTHER-BRANCH-GL    TO LMC-ORG-TEXT
             MOVE BRCH-OTHER-BRANCH-GL    TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-INC-WAIVERS NOT = BRCH-INC-WAIVERS
             MOVE "15 "                   TO LMC-FIELD
             MOVE "WAIVERS  "             TO LMC-DESC
             MOVE BROR-INC-WAIVERS        TO LMC-ORG-TEXT
             MOVE BRCH-INC-WAIVERS        TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-INC-INS NOT = BRCH-INC-INS
             MOVE "16 "                   TO LMC-FIELD
             MOVE "KEEP INS "             TO LMC-DESC
             MOVE BROR-INC-INS            TO LMC-ORG-TEXT
             MOVE BRCH-INC-INS            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-AGE-ERN NOT = BRCH-AGE-ERN
             MOVE "17 "                   TO LMC-FIELD
             MOVE "DISP UNER"             TO LMC-DESC
             MOVE BROR-AGE-ERN            TO LMC-ORG-TEXT
             MOVE BRCH-AGE-ERN            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-CHANGE-MEMO NOT = BRCH-CHANGE-MEMO
             MOVE "18 "                   TO LMC-FIELD
             MOVE "DATE MEMO"             TO LMC-DESC
             MOVE BROR-CHANGE-MEMO        TO LMC-ORG-TEXT
             MOVE BRCH-CHANGE-MEMO        TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-IB-LCHG-MESS NOT = BRCH-IB-LCHG-MESS
             MOVE "19 "                   TO LMC-FIELD
             MOVE "DISP MESS"             TO LMC-DESC
             MOVE BROR-IB-LCHG-MESS       TO LMC-ORG-TEXT
             MOVE BRCH-IB-LCHG-MESS       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-FRONT-END-FG NOT = BRCH-FRONT-END-FG
             MOVE "20 "                   TO LMC-FIELD
             MOVE "FRONT END"             TO LMC-DESC
             MOVE BROR-FRONT-END-FG       TO LMC-ORG-TEXT
             MOVE BRCH-FRONT-END-FG       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-DLPARV-ADJ-FG NOT = BRCH-DLPARV-ADJ-FG
             MOVE "21 "                   TO LMC-FIELD
             MOVE "DLRPARTIC"             TO LMC-DESC
             MOVE BROR-DLPARV-ADJ-FG      TO LMC-ORG-TEXT
             MOVE BRCH-DLPARV-ADJ-FG      TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ADDON-FEECD-O2(1) NOT = BRCH-ADDON-FEECD-O2(1)
             MOVE "22 "                   TO LMC-FIELD
             MOVE "ADON O2 1"             TO LMC-DESC
             MOVE BROR-ADDON-FEECD-O2(1)  TO LMC-ORG-TEXT
             MOVE BRCH-ADDON-FEECD-O2(1)  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ADDON-FEECD-O2(2) NOT = BRCH-ADDON-FEECD-O2(2)
             MOVE "221"                   TO LMC-FIELD
             MOVE "ADON O2 2"             TO LMC-DESC
             MOVE BROR-ADDON-FEECD-O2(2)  TO LMC-ORG-TEXT
             MOVE BRCH-ADDON-FEECD-O2(2)  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ADDON-FEECD-O3(1) NOT = BRCH-ADDON-FEECD-O3(1)
             MOVE "222"                   TO LMC-FIELD
             MOVE "ADON O3 1"             TO LMC-DESC
             MOVE BROR-ADDON-FEECD-O3(1)  TO LMC-ORG-TEXT
             MOVE BRCH-ADDON-FEECD-O3(1)  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ADDON-FEECD-O3(2) NOT = BRCH-ADDON-FEECD-O3(2)
             MOVE "223"                   TO LMC-FIELD
             MOVE "ADON O3 2"             TO LMC-DESC
             MOVE BROR-ADDON-FEECD-O3(2)  TO LMC-ORG-TEXT
             MOVE BRCH-ADDON-FEECD-O3(2)  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-RECEIPT-PROMDATE NOT = BRCH-RECEIPT-PROMDATE
             MOVE "23 "                   TO LMC-FIELD
             MOVE "RCPT DATE"             TO LMC-DESC
             MOVE BROR-RECEIPT-PROMDATE   TO LMC-ORG-TEXT
             MOVE BRCH-RECEIPT-PROMDATE   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-RECEIPT-INTPD NOT = BRCH-RECEIPT-INTPD
             MOVE "24 "                   TO LMC-FIELD
             MOVE "RCPT INT "             TO LMC-DESC
             MOVE BROR-RECEIPT-INTPD      TO LMC-ORG-TEXT
             MOVE BRCH-RECEIPT-INTPD      TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-OUR-FEE NOT = BRCH-OUR-FEE
             MOVE "25 "                   TO LMC-FIELD
             MOVE "SF OURFEE"             TO LMC-DESC
             MOVE BROR-OUR-FEE            TO LMC-ORG-TEXT
             MOVE BRCH-OUR-FEE            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-STOP-ON-COLL NOT = BRCH-STOP-ON-COLL
             MOVE "26 "                   TO LMC-FIELD
             MOVE "STOP COLL"             TO LMC-DESC
             MOVE BROR-STOP-ON-COLL       TO LMC-ORG-TEXT
             MOVE BRCH-STOP-ON-COLL       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-LEDGER-JUST-INTCHG NOT = BRCH-LEDGER-JUST-INTCHG
             MOVE "27 "                   TO LMC-FIELD
             MOVE "LEDG PRNT"             TO LMC-DESC
             MOVE BROR-LEDGER-JUST-INTCHG TO LMC-ORG-TEXT
             MOVE BRCH-LEDGER-JUST-INTCHG TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-VOLUME-DLANRP NOT = BRCH-VOLUME-DLANRP
             MOVE "29 "                   TO LMC-FIELD
             MOVE "PRNT VOL "             TO LMC-DESC
             MOVE BROR-VOLUME-DLANRP      TO LMC-ORG-TEXT
             MOVE BRCH-VOLUME-DLANRP      TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-PDTH-NXDUE NOT = BRCH-PDTH-NXDUE
             MOVE "30 "                   TO LMC-FIELD
             MOVE "PDTH DATE"             TO LMC-DESC
             MOVE BROR-PDTH-NXDUE         TO LMC-ORG-TEXT
             MOVE BRCH-PDTH-NXDUE         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-OVERRIDE-BR-STATE NOT = BRCH-OVERRIDE-BR-STATE
             MOVE "31 "                   TO LMC-FIELD
             MOVE "OVR STATE"             TO LMC-DESC
             MOVE BROR-OVERRIDE-BR-STATE  TO LMC-ORG-TEXT
             MOVE BRCH-OVERRIDE-BR-STATE  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-DEFAULT-SC-CLASS NOT = BRCH-DEFAULT-SC-CLASS
             MOVE "32 "                   TO LMC-FIELD
             MOVE "DEF CLASS"             TO LMC-DESC
             MOVE BROR-DEFAULT-SC-CLASS   TO LMC-ORG-NUM
             MOVE BRCH-DEFAULT-SC-CLASS   TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-DEFAULT-SC-SUBCLASS NOT = BRCH-DEFAULT-SC-SUBCLASS
             MOVE "33 "                    TO LMC-FIELD
             MOVE "DEF SUBCL"              TO LMC-DESC
             MOVE BROR-DEFAULT-SC-SUBCLASS TO LMC-ORG-NUM
             MOVE BRCH-DEFAULT-SC-SUBCLASS TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-RECDEL-NOT-REC NOT = BRCH-RECDEL-NOT-REC
             MOVE "34 "                   TO LMC-FIELD
             MOVE "COLL RPT "             TO LMC-DESC
             MOVE BROR-RECDEL-NOT-REC     TO LMC-ORG-TEXT
             MOVE BRCH-RECDEL-NOT-REC     TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

       LOG-MAINTENANCE-CHGS-EXIT.
           EXIT.

      *************************************************
       LOG-FIELD-CHANGE SECTION.
      *************************************************

           MOVE "F"           TO ERRLOGW-ACTION.
           MOVE LMC-DETAIL    TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       LOG-FIELD-CHANGE-EXIT.
           EXIT.

      ************************************************
       DISPLAY-FIELDS SECTION.
      ************************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE BR-LCARDFG TO F01.
           MOVE BR-DLRCHK TO F02.
           MOVE BR-NO-OUT-DLR-RSV TO F03.
           MOVE BR-NEGDLRSV TO F04.
           MOVE SPACES TO F05.
           MOVE BR-ALLOTMNTS TO F06.
           MOVE BR-EXTRACT-AUTO-UPD TO F07.
           MOVE BR-CPCORP-ORDER TO F08.
           MOVE BR-LOAN-NUM-FLAG TO F09.
           MOVE BR-DOWNPY-FLAG TO F10.
           MOVE BR-UNER-DLANRP TO F11.
           MOVE BR-SECOND-PROMISE TO F12.
           MOVE BR-POST-GL TO F13.
           MOVE BR-OTHER-BRANCH-GL TO F14.
           MOVE BR-INC-WAIVERS TO F15.
           MOVE BR-INC-INS TO F16.
           MOVE BR-AGE-ERN TO F17.
           MOVE BR-CHANGE-MEMO TO F18.
           MOVE BR-IB-LCHG-MESS TO F19.
           MOVE BR-FRONT-END-FG TO F20.
           MOVE BR-DLPARV-ADJ-FG TO F21.
           MOVE BR-ADDON-FEECD-O2(1) TO F22.
           MOVE BR-ADDON-FEECD-O2(2) TO F221.
           MOVE BR-ADDON-FEECD-O3(1) TO F222.
           MOVE BR-ADDON-FEECD-O3(2) TO F223.
           MOVE BR-RECEIPT-PROMDATE TO F23.
           MOVE BR-RECEIPT-INTPD TO F24.
           MOVE BR-OUR-FEE TO F25.
           MOVE BR-STOP-ON-COLL TO F26.
           MOVE BR-LEDGER-JUST-INTCHG TO F27.
      *    MOVE BR-IB-ACCRUALS-DONE TO F28.
           MOVE SPACES TO F28.
           MOVE BR-VOLUME-DLANRP TO F29.
           MOVE BR-PDTH-NXDUE TO F30.
           MOVE BR-OVERRIDE-BR-STATE TO F31.
           MOVE BR-DEFAULT-SC-CLASS TO F32.
           MOVE BR-DEFAULT-SC-SUBCLASS TO F33.
           MOVE BR-RECDEL-NOT-REC TO F34.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM SEND-CPCORP-TYPE.

       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO BRMAN2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BRMAN2-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BRMAN2-SCREEN.
           DISPLAY BRMAN2-SCREEN UPON BRMAIN-WINDOW-HANDLE.
           MOVE BRMAN2-FORM-FIELDS TO WR-BUF.
           MOVE BRMAN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE BRMAN2-HELP--FILE TO HELP-LINK-FILE.
           MOVE BRMAN2-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/BRMAN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARAGRAPHS SECTION.
      **************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/PHONEIO.CPY".
       SEND-CPCORP-TYPE.
           MOVE "S  A150CPTYPE." TO VDU.
           IF BR-CPCORP-ORDER = "1"
              MOVE "ACCOUNT NUMBER" TO VDUBUF
           ELSE
              IF BR-CPCORP-ORDER = "2"
                 MOVE "AGEING" TO VDUBUF
              ELSE
                 IF BR-CPCORP-ORDER = "3"
                    MOVE "ALPHA-LAST NAME" TO VDUBUF
                 ELSE
                    IF BR-CPCORP-ORDER = "4"
                       MOVE "RENEWABLE/AVAIL" TO VDUBUF
                    ELSE
                       IF BR-CPCORP-ORDER = "5"
                          MOVE "RECENCY" TO VDUBUF
                       ELSE
                       IF BR-CPCORP-ORDER = "6"
                          MOVE "AGEING,ALPHA" TO VDUBUF
                       ELSE
                       IF BR-CPCORP-ORDER = "7"
                          MOVE "DEALER NO,LNAME" TO VDUBUF.
           PERFORM SCREENIO.
       INVALID-LOCATION.
           MOVE "BRANCH NO. EXCEEDS GLOBAL NO. OF LOCATIONS!" TO MESS.
           PERFORM SEND-MESS.
       Y-OR-N-OR-L.
           MOVE "ENTER 'Y' OR 'N' OR 'L'-ONLY" TO MESS.
           PERFORM SEND-MESS.
       Y-OR-N-OR-O.
           MOVE "ENTER 'Y' OR 'N' OR 'O'-ONLY" TO MESS.
           PERFORM SEND-MESS.
       Y-OR-N.
           MOVE "ENTER 'Y' OR 'N'" TO MESS.
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
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      *********************************
       IO-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BRMAN2-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE BRMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
