       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BRLIST.
       DATE-WRITTEN.    01-07-88.
      ******************************************************************
      *
      *  PROGRAM NAME:  GB/BRLIST.C
      *
      *  DESCRIPTION :  BRANCH FILE LIST
      *
      *-----------------------------------------------------------------
      * REV:
      * MJD 063095 ADDED BR-BANK-CLEARING-GL
      *                  BR-BANK-ROUTING-NO
      *                  BR-BANK-ACCTNO.
      * BAH 083195 REMOVED BR-CASH-METHOD-ONLY AND BR-IB-ACCRUALS
      *            (MOVED TO GP RECORD)
      * BAH 111397 NOTHING FROM PAGE 4 WAS PRINTING...PL# 154
      * BAH 022698 ADDED BR-ADDON-FEECD-O3
      * KEC 032598 ADDED PRINT OF MAINTENANCE PAGE 7 (OPTIONS).
      * BAH 042998 REMOVED BR-ADDON-FEECD(1) AND (2) AND RENAMED (3) AND (4)
      *            TO BR-ADDON-FEECD-O2(1) AND (2)
      * BAH 061598 REMOVED BR-FXPRESS-OPTION, MOVED TO BROPMN
      * BAH 090298 REMOVED BR-EOM1-PROCEDURE, BR-EOM2-PROCEDURE
      * CS  101098 ADDED BR-FAX-NUMBER
      * MG  991004 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * MJD 00914 ADDED BR-LBOX (MFC PR#227)
      * JTG 010507 ALLOWED ENTRY OF 'TRADEIN' RE: SALE PRICE-DP-TI
      *            PAACO PR#1467
      * MG  030718 UPDATED THE FIELDS AND CHANGED THE LAYOUT TO BE IN
      *            ORDER {PL#0387}
      * BAH 040119 ADDED NEW BRANCH TRAILER FIELDS, B2FILE
      *            BR-COLLECT-MANAGER, PHONE AND EXT REGACC PR# 47
      * AMM 070126 UPDATED THE LISTING
      * BAH 070220 ADDED BR-CC FIELDS FOR REGACC PR# 164
      * BAH 110812 REMOVED RRFORMS AND ADDED SCREEN
      * BAH 151021 REMOVED B2FILE, PD0003
      *  CS 160121 REMOVED BR-REB-METHOD-2. CL-LVL NOW BR-REB-METHOD(7)
      *            O3 NOW BR-REB-METHOD(6)
      *    CL(DEC)  -  1      O2       - 5     
      *    AH       -  2      O3       - 6     O5     -  9 
      *    PP       -  3      CL(LEV)  - 7     O6     - 10      
      *    O1       -  4      O4       - 8     O7     - 11
      * AMM 161031 ADDED LASER PRINT OPTION FG TO LIST WORLD PR#1200
      * AMM 181113 SDP-002-T02 REMOVED CREDIT BUREAU FIELDS
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./GB/BRLIST.CBL S35 09/20/22-12:41:07 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".


       01  FIRST-TIME                      PIC X(01)     VALUE "Y".
       01  MESS-LIST                       PIC X(05)     VALUE "MESS.".

       01  RECORDS-FOUND                   PIC X(01)     VALUE SPACES.
           88  NO-RECORDS-FOUND                          VALUE "N".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LN-WORKERS.
           03  LN-FEED                     PIC 9(02)     VALUE ZEROES.
           03  LN-COUNT                    PIC 9(03)     VALUE 99.

       01  PAGE-NUMBER                     PIC 9(04)     VALUE ZEROES.

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR                   PIC 9(4).
           03  END-BR                   PIC 9(4).

       01  VERSION-CONTROL              PIC X VALUE "A".

      *=================================================================
      *
      *    H E A D E R - B U F F E R S
      *
      *=================================================================
       01  HEAD1.
           03  H-DATE                    PIC X(08)     VALUE SPACES.
           03  FILLER                    PIC X(02)     VALUE SPACES.
           03  H-HR                      PIC 9(02)     VALUE ZEROES.
           03  FILLER                    PIC X(01)     VALUE ":".
           03  H-MIN                     PIC 9(02)     VALUE ZEROES.
           03  FILLER                    PIC X(31)     VALUE SPACES.
           03  H-NAME                    PIC X(40)     VALUE SPACES.
           03  FILLER                    PIC X(37)     VALUE SPACES.
           03  FILLER                    PIC X(05)     VALUE "PAGE ".
           03  H-PAGE-NO                 PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER                    PIC X(10)     VALUE "USER ID:".
           03  H-USERID                  PIC X(10).
           03  FILLER                    PIC X(19)     VALUE " ".
           03  H-TITLE                   PIC X(44)     VALUE
               "         - - - - BRANCH FILE LISTING - - - -".
           03  FILLER                    PIC X(52)     VALUE " ".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER                    PIC X(09)     VALUE
               "BRANCH : ".
           03  H-BRNO                    PIC 9(04).
           03  FILLER                    PIC X(119)    VALUE SPACES.

      *=================================================================
      *
      *    D E T A I L - L I N E - B U F F E R S
      *
      *=================================================================
       01  DETAIL-LINE                   PIC X(132)    VALUE SPACES.

       01  D-LINE1             REDEFINES DETAIL-LINE.
           03  D-TAG1                    PIC X(18).
           03  D-NAME-ADR                PIC X(24).

           03  D-X             REDEFINES D-NAME-ADR.
               05  D-PHONE               PIC 999/999/9999.
               05  FILLER                PIC X(12).

           03  D-Z             REDEFINES D-NAME-ADR.
               05  D-REC                 PIC Z(5)9.
               05  FILLER                PIC X(5).
               05  FILLER                PIC X(13).

           03  D-PRINTERS      REDEFINES D-NAME-ADR.
               05  D-PRNUM               PIC 99.
               05  FILLER                PIC X.
               05  D-PRFLAG              PIC X.
               05  FILLER                PIC X(20).

           03  FILLER                    PIC X(9).
           03  D-TAG2                    PIC X(18).
           03  D-BAL                     PIC Z(9)9.99-.

           03  D-BANK-AC       REDEFINES D-BAL.
               05  FILLER                PIC X.
               05  D-GL                  PIC 9999/9999999.
               05  FILLER                PIC X.

           03  D-BANK-NUM      REDEFINES D-BAL.
               05  FILLER                PIC X(3).
               05  D-RT-NO               PIC Z(10).
               05  FILLER                PIC X.

           03  FILLER                    PIC X(22).

      *-----------------------------------------------------------------
       01  D-LINE3             REDEFINES DETAIL-LINE.
           03  D-OPTION-TAG              PIC X(34).

           03  FILLER                    REDEFINES D-OPTION-TAG.
               05  FILLER                PIC X(03).
               05  D-PP-TAG              PIC X(03).
               05  D-PP-AMT              PIC ZZZ9.99.
               05  FILLER                PIC X(04).
               05  D-O3-TAG              PIC X(03).
               05  D-O3-AMT              PIC ZZZ9.99.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-XX.
               05  D-FLAG                PIC X.
               05  FILLER                PIC X(5).

           03  D-XZ REDEFINES D-XX.
               05  D-FLAG3               PIC X(3).
               05  FILLER                PIC X.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-OPTION-TAG2             PIC X(34).

           03  D-OPT-TAG2X REDEFINES D-OPTION-TAG2.
               05  D-OPT-TAG2            PIC X(12).
               05  D-FEE1                PIC XX.
               05  FILLER                PIC X.
               05  D-FEE2                PIC XX.
               05  D-OPT-TAG2A           PIC X(3).
               05  D-FEE3                PIC XX.
               05  FILLER                PIC X.
               05  D-FEE4                PIC XX.
               05  D-OPT-TAG2B           PIC X(3).
               05  D-FEE5                PIC XX.
               05  FILLER                PIC X.
               05  D-FEE6                PIC XX.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-FLAG-NUM2.
               05  D-FLAG2               PIC X.
               05  FILLER                PIC X.

      *-----------------------------------------------------------------
       01  D-LINE4             REDEFINES DETAIL-LINE.
           03  FILLER                    PIC X(58).
           03  D-OPTTITLE                PIC X(19).
           03  FILLER                    PIC X(55).

      *-----------------------------------------------------------------
       01  D-LINE5             REDEFINES DETAIL-LINE.
           03  D-PAGE3-TAG1              PIC X(21).
           03  D-PAGE3-TAG2              PIC X(11).
           03  D-PAGE3-TAG3              PIC X(16).
           03  D-PAGE3-TAG4              PIC X(10).
           03  D-PAGE3-TAG5              PIC X(22).

      *-----------------------------------------------------------------
       01  D-LINE6             REDEFINES DETAIL-LINE.
           03  FILLER                    PIC X(24).
           03  D-EARNMETHOD              PIC Z.
           03  FILLER                    PIC X(11).
           03  D-REBDAYS                 PIC ZZ.
           03  FILLER                    PIC X(11).
           03  D-ACCMONTHS               PIC ZZZ.

      *-----------------------------------------------------------------
       01  D-LINE8             REDEFINES DETAIL-LINE.
           03  D-P5-TAG                   PIC X(20).
           03  D-P5-TEXT                  PIC X(60).
           03  D-P5-X REDEFINES D-P5-TEXT.
               05  D-P5-PHONE             PIC 999/999/9999.
               05  D-P5-EXT-TAG           PIC X(6).
               05  D-P5-EXT               PIC Z(4).
           03  FILLER                     PIC X(10).
           03  D-P5-MAIN-TEXT2.
               05  D-P5-TAG2              PIC X(7).
               05  D-P5-TEXT2             PIC X(27).
               05  D-P5-TEXT2X REDEFINES D-P5-TEXT2.
                   07  FILLER             PIC X(12).
                   07  D-P5-ZIP-TAG       PIC X(5).
                   07  D-P5-ZIP           PIC X(10).

      *-----------------------------------------------------------------
       01  D-LINE9             REDEFINES DETAIL-LINE.
           03  D-P6-TAG                   PIC X(26).
           03  FILLER                     PIC X(01).
           03  D-P6-TEXT                  PIC X(105).
           03  FILLER          REDEFINES D-P6-TEXT.
               05  D-P6-AMT              PIC Z(06)9.99-.
               05  FILLER                PIC X(94).
           03  FILLER          REDEFINES D-P6-TEXT.
               05  D-P6-REFNO1           PIC X(05).
               05  FILLER                PIC X(01).
               05  D-P6-REFNO2           PIC X(05).
               05  FILLER                PIC X(01).
               05  D-P6-REFNO3           PIC X(05).
               05  FILLER                PIC X(88).

      *-----------------------------------------------------------------
       01  D-LINE10            REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(40).
           03  D-P7-TAG                   PIC X(34).
           03  FILLER                     PIC X(01).
           03  D-P7-TEXT                  PIC X(57).
           03  FILLER          REDEFINES D-P7-TEXT.
               05  D-P7-DATE             PIC 99/99/99.
               05  FILLER                PIC X(49).
           03  FILLER          REDEFINES D-P7-TEXT.
               05  D-P7-FLAG             PIC X(01).
               05  FILLER                PIC X(56).
      *-----------------------------------------------------------------
       01  RANGE-LINE         REDEFINES DETAIL-LINE.
           03  FILLER                   PIC X(20).
           03  RANGE-SELECTION          PIC X(16).
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/BRLIST_DEF.CPY".
       COPY "LIBGB/BRLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGB/BRLIST_SCN.CPY".

      ******************************************************************
       PROCEDURE DIVISION
      ******************************************************************
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
       MAIN-MODULE SECTION.
      ******************************************************************
           PERFORM SQL-CONNECT.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BRLIST" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO END-PROGRAM.

       NEXT-BR.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD ) OR ( BR-NO > END-BR )
              GO TO END-ROUTINE.

           MOVE BR-NO TO H-BRNO.
           PERFORM HEAD-PAGE.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-BR.

       END-ROUTINE.

           IF ( FIRST-TIME = "Y" )
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE BRLIST-QUEUE-POS TO Q-POS.
           MOVE BRLIST-COPIES-POS TO C-POS.
           MOVE BRLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.
           MOVE BEG-BR TO BR-NO.
           PERFORM START-BR-FILE.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID  TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LISTING IN PROGRESS................." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
           IF VDUSTATUS = "1U"
              GO TO BR-02.

       BR-01.
           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUNUM0 TO BEG-BR.

       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.

           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           GO TO EXIT-PARM.

       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR TO VDUNUM0.
              PERFORM SCREENIO.
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR TO VDUNUM0.
              PERFORM SCREENIO.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************
       BEGIN-DETAIL-LINE-PROCESS.

      ******************************************************************
      *            BRANCH FILE MAINTENANCE - PAGE 1
      ******************************************************************

           MOVE "{PAGE 01}" TO D-TAG1.
           MOVE 02          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BRANCH NAME     : " TO D-TAG1.
           MOVE BR-NAME              TO D-NAME-ADR.
           MOVE "BANK BALANCE : "    TO D-TAG2.
           MOVE BR-BANKBAL           TO D-BAL.
           MOVE 02          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ADDRESS1        : " TO D-TAG1.
           MOVE BR-ADR1              TO D-NAME-ADR.
           MOVE "BANK    GL # : "    TO D-TAG2.
           MOVE BR-GLBANK            TO D-GL.
           INSPECT D-GL REPLACING ALL "/" BY "-".
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ADDRESS2        : " TO D-TAG1.
           MOVE BR-ADR2              TO D-NAME-ADR.
           MOVE "DEPOSIT GL # : "    TO D-TAG2.
           MOVE BR-GLBANK-DEPOSIT    TO D-GL.
           INSPECT D-GL REPLACING ALL "/" BY "-".
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CITY            : " TO D-TAG1.
           MOVE BR-CITY              TO D-NAME-ADR.
           MOVE "ONHAND  GL # : "    TO D-TAG2.
           MOVE BR-GLONHAND          TO D-GL.
           INSPECT D-GL REPLACING ALL "/" BY "-".
           PERFORM WRITE-DETAIL-LINE.

           MOVE "STATE           : " TO D-TAG1.
           MOVE BR-STATE             TO D-NAME-ADR.
           MOVE "OVER/SHORT # : "    TO D-TAG2.
           MOVE BR-GLOVERSHORT       TO D-GL.
           INSPECT D-GL REPLACING ALL "/" BY "-".
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ZIP             : " TO D-TAG1.
           MOVE BR-ZIP               TO D-NAME-ADR.
           MOVE "RETAINED   # : "    TO D-TAG2.
           MOVE BR-RETAIN            TO D-GL.
           INSPECT D-GL REPLACING ALL "/" BY "-".
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PHONE           : " TO D-TAG1.
           MOVE BR-PHONE             TO D-PHONE.
           INSPECT D-PHONE REPLACING ALL "/" BY "-".
           MOVE "BANK CLEARING: "    TO D-TAG2.
           MOVE BR-BANK-CLEARING-GL  TO D-GL.
           INSPECT D-GL REPLACING ALL "/" BY "-".
           PERFORM WRITE-DETAIL-LINE.

           MOVE "FAX             : " TO D-TAG1.
           MOVE BR-FAX-NUMBER        TO D-PHONE.
           INSPECT D-PHONE REPLACING ALL "/" BY "-".
           MOVE "ROUTING NO   : "    TO D-TAG2.
           MOVE BR-BANK-ROUTING-NO   TO D-RT-NO.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " - - PRINTERS - - " TO D-TAG1.
           MOVE "BANK ACCTNO  : "    TO D-TAG2.
           MOVE BR-BANK-ACCTNO       TO D-BANK-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "#1 - LINE/CUT   : " TO D-TAG1.
           MOVE BR-PRINTER(1)        TO D-PRNUM.
           MOVE BR-PRINTERFG(1)      TO D-PRFLAG.
           MOVE "DATA PATH    : "    TO D-TAG2.
           MOVE BR-DATA-PATH         TO D-BANK-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "#2 - LINE/CUT   : " TO D-TAG1.
           MOVE BR-PRINTER(2)        TO D-PRNUM.
           MOVE BR-PRINTERFG(2)      TO D-PRFLAG.
           MOVE "MACHINE      : "    TO D-TAG2.
           MOVE BR-MACHINE           TO D-BANK-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "#3 - LEDGER CARD: " TO D-TAG1.
           MOVE BR-PRINTER(3)        TO D-PRNUM.
           MOVE BR-PRINTERFG(3)      TO D-PRFLAG.
           MOVE "MANAGER      : "    TO D-TAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "#4 - CUT SHEET  : " TO D-TAG1.
           MOVE BR-PRINTER(4)        TO D-PRNUM.
           MOVE BR-PRINTERFG(4)      TO D-PRFLAG.
           MOVE BR-MANAGER           TO D-TAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NEXT CHECK NUMBER: " TO D-TAG1.
           MOVE BR-NEXT-CHECK         TO D-REC.
           MOVE "EOD BATCH ID   : "   TO D-TAG2.
           MOVE BR-EOD-PROCEDURE      TO D-BANK-NUM.
           MOVE 02        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "FEE GL BRANCH NO: "  TO D-TAG1.
           MOVE BR-FEE-GL-BRNO        TO D-REC.
           MOVE "REGION #       : "   TO D-TAG2.
           MOVE BR-REGION-NUMBER      TO D-BANK-NUM.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *            BRANCH FILE MAINTENANCE - PAGE 2 (OPTIONS PAGE #1)
      ******************************************************************
           MOVE "{PAGE 02}" TO D-TAG1.
           MOVE 02          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "- - - OPTIONS - - -" TO D-OPTTITLE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LEDGER CARDS                     : " TO D-OPTION-TAG.
           MOVE BR-LCARDFG                            TO D-FLAG.
           MOVE "DISPLAY UNEARNED ON AGED TRIAL    "  TO D-OPTION-TAG2.
           MOVE 02        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


           MOVE "EDIT S.F. CONVERSION DEALER CHECK:" TO D-OPTION-TAG.
           MOVE BR-DLRCHK                            TO D-FLAG.
           MOVE "OR TOTAL EARNED (U/T)            :" TO D-OPTION-TAG2.
           MOVE BR-AGE-ERN                           TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "WITHDRAW FROM DELAER RESERVES    :" TO D-OPTION-TAG.
           MOVE BR-NO-OUT-DLR-RSV                    TO D-FLAG.
           MOVE "CREATE DATED MEMO WITH CHANGES   :" TO D-OPTION-TAG2.
           MOVE BR-CHANGE-MEMO                       TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALLOW DEALER RESERVE TO GO NEG.  :" TO D-OPTION-TAG.
           MOVE BR-NEGDLRSV                          TO D-FLAG.
           MOVE "DISPLAY MESSAGE FOR L/C ON IB    :" TO D-OPTION-TAG2.
           MOVE BR-IB-LCHG-MESS                      TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NOT USED                         :" TO D-OPTION-TAG.
           MOVE "FRONT-END CALCULATIONS           :" TO D-OPTION-TAG2.
           MOVE BR-FRONT-END-FG TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ARE ALLOTMENT PLANS IN FORCE     :" TO D-OPTION-TAG.
           MOVE BR-ALLOTMNTS TO D-FLAG.
           MOVE "DEALER PARTIC. RECAPTURE         :" TO D-OPTION-TAG2.
           MOVE BR-DLPARV-ADJ-FG TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "AUTOMATIC EXTRACTION UPDATE      :" TO D-OPTION-TAG.
           MOVE BR-EXTRACT-AUTO-UPD                  TO D-FLAG.
           MOVE "ADDON FEE O2)"                      TO D-OPT-TAG2.
           MOVE BR-ADDON-FEECD-O2(1)                 TO D-FEE1.
           MOVE BR-ADDON-FEECD-O2(2)                 TO D-FEE2.
           MOVE " O3)"                               TO D-OPT-TAG2B.
           MOVE BR-ADDON-FEECD-O3(1)                 TO D-FEE3.
           MOVE BR-ADDON-FEECD-O3(2)                 TO D-FEE4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "COLLECTION REPORT PRINT ORDER    :" TO D-OPTION-TAG.
           MOVE BR-CPCORP-ORDER                      TO D-FLAG.
           MOVE "PRINT PROMISED DATE ON RECEIPT   :" TO D-OPTION-TAG2.
           MOVE BR-RECEIPT-PROMDATE                  TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT INTEREST PAID ON RECEIPT   :" TO D-OPTION-TAG2.
           MOVE BR-RECEIPT-INTPD                     TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LOAN # DISPLAY WITH OUT LAW CODE :" TO D-OPTION-TAG.
           MOVE BR-LOAN-NUM-FLAG                     TO D-FLAG.
           MOVE "SALES FINANCE FEES OURS (Y/N)    :" TO D-OPTION-TAG2.
           MOVE BR-OUR-FEE                           TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SF DOWNPY & TRADEIN ARE MEMO ONLY:" TO D-OPTION-TAG.
           MOVE BR-DOWNPY-FLAG                       TO D-FLAG.
           MOVE "STOP ON COLLATERAL AT INPUT      :" TO D-OPTION-TAG2.
           MOVE BR-STOP-ON-COLL                      TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT TRUE UNEARN ON DL.ANAL.RPT.:" TO D-OPTION-TAG.
           MOVE BR-UNER-DLANRP                       TO D-FLAG.
           MOVE "LEDGER PRINT ONLY INTCHG         :" TO D-OPTION-TAG2.
           MOVE BR-LEDGER-JUST-INTCHG                TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SECOND PROMISE DATE              :" TO D-OPTION-TAG.
           MOVE BR-SECOND-PROMISE                    TO D-FLAG.
           MOVE "                                 :" TO D-OPTION-TAG2.
           MOVE SPACES                               TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DOES BRANCH RUN THEIR OWN G/L    :" TO D-OPTION-TAG.
           MOVE BR-POST-GL                           TO D-FLAG.
           MOVE "PRINT VOLUME ON DL.ANAL.RPT.     :" TO D-OPTION-TAG2.
           MOVE BR-VOLUME-DLANRP                     TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DISBURSE TO OTHER BRANCH G/L     :" TO D-OPTION-TAG.
           MOVE BR-OTHER-BRANCH-GL                   TO D-FLAG.
           MOVE "USE PAID THRU DATE AS NEXT DUE   :" TO D-OPTION-TAG2.
           MOVE BR-PDTH-NXDUE                        TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE WAIVERS AT PAYOFF        :" TO D-OPTION-TAG.
           MOVE BR-INC-WAIVERS                       TO D-FLAG.
           MOVE "OVERRIDE ON THE BRANCH'S STATE   :" TO D-OPTION-TAG2.
           MOVE BR-OVERRIDE-BR-STATE                 TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "KEEP INSUR IN FORCE AT PAYOFF    :" TO D-OPTION-TAG.
           MOVE BR-INC-INS                           TO D-FLAG.
           MOVE "DEFAULT CLASS NO. FOR SC CONV    :" TO D-OPTION-TAG2.
           MOVE BR-DEFAULT-SC-CLASS                  TO D-FLAG-NUM2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SUBCLASS  FOR SC CONV            :" TO D-OPTION-TAG2.
           MOVE BR-DEFAULT-SC-SUBCLASS               TO D-FLAG-NUM2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "COLLECTION REPORT USE REC-DEL    :" TO D-OPTION-TAG2.
           MOVE BR-RECDEL-NOT-REC                    TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *            BRANCH FILE MAINTENANCE - PAGE 3
      ******************************************************************

           PERFORM HEAD-PAGE.

           MOVE "{PAGE 03}" TO D-TAG1.
           MOVE 02          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INSURANCE EARNING"     TO D-PAGE3-TAG1.
           MOVE "EARNING"               TO D-PAGE3-TAG2.
           MOVE "REBATE DAYS"           TO D-PAGE3-TAG3.
           MOVE "ACCRUAL"               TO D-PAGE3-TAG4.
           MOVE "---------------------" TO D-PAGE3-TAG5.
           MOVE 02        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "   REPORT OPTIONS:"    TO D-PAGE3-TAG1.
           MOVE " METHOD"               TO D-PAGE3-TAG2.
           MOVE "/FULL MONTH"           TO D-PAGE3-TAG3.
           MOVE "MONTHS"                TO D-PAGE3-TAG4.
           MOVE "EARNING METHOD CODES:" TO D-PAGE3-TAG5.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "   1. CL (DECR)  :"    TO D-PAGE3-TAG1.
           MOVE BR-REB-METHOD(1)        TO D-EARNMETHOD.
           MOVE BR-REBDAYS-MONTH(1)     TO D-REBDAYS.
           MOVE BR-ACCRUAL-MONTHS(1)    TO D-ACCMONTHS.
           MOVE "1. RULE OF 78THS"      TO D-PAGE3-TAG5.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "   2. CL (LEVL)  :"    TO D-PAGE3-TAG1.
           MOVE BR-REB-METHOD(7)        TO D-EARNMETHOD.
           MOVE BR-REBDAYS-MONTH(7)     TO D-REBDAYS.
           MOVE BR-ACCRUAL-MONTHS(7)    TO D-ACCMONTHS.
           MOVE "2. PRO RATA"           TO D-PAGE3-TAG5.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "   3. A & H      :"    TO D-PAGE3-TAG1.
           MOVE BR-REB-METHOD(2)        TO D-EARNMETHOD.
           MOVE BR-REBDAYS-MONTH(2)     TO D-REBDAYS.
           MOVE BR-ACCRUAL-MONTHS(2)    TO D-ACCMONTHS.
           MOVE "3. REFUND METHOD (SPR)" TO D-PAGE3-TAG5.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "   4. PERS PROP  :"     TO D-PAGE3-TAG1.
           MOVE BR-REB-METHOD(3)         TO D-EARNMETHOD.
           MOVE BR-REBDAYS-MONTH(3)      TO D-REBDAYS.
           MOVE BR-ACCRUAL-MONTHS(3)     TO D-ACCMONTHS.
           MOVE "-------------------------" TO D-PAGE3-TAG5.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "   5. OTHER 1    :"     TO D-PAGE3-TAG1.
           MOVE BR-REB-METHOD(4)            TO D-EARNMETHOD.
           MOVE BR-REBDAYS-MONTH(4)         TO D-REBDAYS.
           MOVE BR-ACCRUAL-MONTHS(4)        TO D-ACCMONTHS.
           MOVE "4. RULE 78THS CASH - 7" TO D-PAGE3-TAG5.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "   6. OTHER 2    :"     TO D-PAGE3-TAG1.
           MOVE BR-REB-METHOD(5)         TO D-EARNMETHOD.
           MOVE BR-REBDAYS-MONTH(5)      TO D-REBDAYS.
           MOVE BR-ACCRUAL-MONTHS(5)     TO D-ACCMONTHS.
           MOVE "5. RULE 78THS CASH - 8" TO D-PAGE3-TAG5.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "   7. OTHER 3    :"     TO D-PAGE3-TAG1.
           MOVE BR-REB-METHOD(6)         TO D-EARNMETHOD.
           MOVE BR-REBDAYS-MONTH(6)      TO D-REBDAYS.
           MOVE BR-ACCRUAL-MONTHS(6)     TO D-ACCMONTHS.
           MOVE "6. 78THS/PRORATA AVG"   TO D-PAGE3-TAG5.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "7. ACTUARIAL ACCRUAL"   TO D-PAGE3-TAG5.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "9. SECURITY"            TO D-PAGE3-TAG5.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *            BRANCH FILE MAINTENANCE - PAGE 4 (OPTIONS PAGE #2)
      ******************************************************************

           MOVE "{PAGE 04}" TO D-TAG1.
           MOVE 02          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "- - - OPTIONS - - -" TO D-OPTTITLE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO D-OPTION-TAG D-FLAG.
           MOVE "MUST ACCELERATE BEFORE P4P5      :"  TO D-OPTION-TAG2.
           MOVE BR-AC-BEFORE-P4P5                     TO D-FLAG2.
           MOVE 02        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "S.F. DEFAULT PAY PERIOD CODE     : " TO D-OPTION-TAG.
           MOVE BR-DEFAULT-SF-UNITPER-CD              TO D-FLAG.
           MOVE "COLLECTOR QUEUE CONT/REC         :"  TO D-OPTION-TAG2.
           MOVE BR-COLL-QUEUE-CON-REC                 TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "S.F. DEFAULT PURPOSE CODE        : " TO D-OPTION-TAG.
           MOVE BR-DEFAULT-SF-PURCD                   TO D-FLAG3.
           MOVE SPACES TO D-OPTION-TAG2 D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PU"                TO CD-TYPE.
           MOVE BR-DEFAULT-SF-PURCD TO CD-CODE.
           PERFORM GET-CD.
           IF IO-GOOD
              MOVE CD-DESC          TO D-OPTION-TAG.
           MOVE "APP. REQUIRED FOR LOAN INPUT     :"  TO D-OPTION-TAG2.
           MOVE BR-APP-REQUIRED-FOR-INPUT             TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LOAN DEFAULT PAY PERIOD CODE     : " TO D-OPTION-TAG.
           MOVE BR-DEFAULT-LN-UNITPER-CD              TO D-FLAG.
           MOVE "APP. EXPANDED INPUT              :"  TO D-OPTION-TAG2.
           MOVE BR-EXPAND-APP-INPUT                   TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

      * 5.
           MOVE "LOAN DEFAULT PURPOSE CODE        : " TO D-OPTION-TAG.
           MOVE BR-DEFAULT-LN-PURCD                   TO D-FLAG.
      * 22.
           MOVE "USE GLOBAL SCAN IN INTER BRANCH  :"  TO D-OPTION-TAG2.
           MOVE BR-INBR-BILNSC-FG                     TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-DEFAULT-LN-PURCD TO CD-CODE.
           MOVE "PU"                TO CD-TYPE.
           PERFORM GET-CD.
           IF IO-GOOD
              MOVE CD-DESC          TO D-OPTION-TAG.
           MOVE "BRIEF POSTING IN INTER-BRANCH    :"  TO D-OPTION-TAG2.
           MOVE BR-INBR-BRIEF-FG                      TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SET COLL CODE TO DEALER MFG CD   : " TO D-OPTION-TAG.
           MOVE BR-SET-COLLCD-DL-MFGCD                TO D-FLAG.
           MOVE "BRANCH STATS DEFAULT START PAGE  :"  TO D-OPTION-TAG2.
           MOVE BR-BYINQ-START-PAGE                   TO D-FLAG-NUM2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NO PROMPT FOR OTHER BR PAPER     : " TO D-OPTION-TAG.
           MOVE BR-NO-PROMPT-FOR-OTHBR                TO D-FLAG.
           MOVE "DEALER STATS DEFAULT START PAGE  :"  TO D-OPTION-TAG2.
           MOVE BR-DSINQ-START-PAGE                   TO D-FLAG-NUM2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALLOW TAP OF OTHER DEALER RSV    : " TO D-OPTION-TAG.
           MOVE BR-TAP-OTHER-DL                       TO D-FLAG.
           MOVE "ENFORCE MINIMUM CREDIT SCORING   :"  TO D-OPTION-TAG2.
           MOVE BR-CREDIT-SCORING-FG                  TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MUST WRITE CL TO WRITE A&H       : " TO D-OPTION-TAG.
           MOVE BR-AH-AND-CL                          TO D-FLAG.
           MOVE "COL QUEUE LOW TO HIGH DELINQ     :"  TO D-OPTION-TAG2.
           MOVE BR-COL-QUEUE-LOW-TO-HI                TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NO CASH PROCEEDS ON DISBURSE     : " TO D-OPTION-TAG.
           MOVE BR-NO-CASH-PROCEEDS                   TO D-FLAG.
           MOVE "AUTO OPEN LNSTAT WINDOW (CONV)   :"  TO D-OPTION-TAG2.
           MOVE BR-AUTO-LNSTAT-WINDOW                 TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MUST REBATE INT BEFORE REPO      : " TO D-OPTION-TAG.
           MOVE BR-RI-BEFORE-REPO                     TO D-FLAG.
           MOVE "TIME ZONE FACTOR                 :"  TO D-OPTION-TAG2.
           MOVE BR-TIME-ZONE-FACTOR                   TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MUST REBATE INT BEFORE P4 & P5   : " TO D-OPTION-TAG.
           MOVE BR-RI-BEFORE-P4P5                     TO D-FLAG.
           MOVE "DUE DATE CHANGE RESTRICT CODE    :"  TO D-OPTION-TAG2.
           MOVE BR-DUEDATE-CHANGE-CD                  TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REFUND INS. THATS NOT OURS       : " TO D-OPTION-TAG.
           MOVE BR-NOT-OURS-REFUND                    TO D-FLAG.
           MOVE "GL ENTRY TYPE ALLOWED            :"  TO D-OPTION-TAG2.
           MOVE BR-GL-TYPE-ALLOWED                    TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OPEN DISB. WINDOW FOR O3 INS.    : " TO D-OPTION-TAG.
           MOVE BR-SFDLO3-WINDOW-FG                   TO D-FLAG.
           MOVE "GL ENTRY DEFAULT                 :"  TO D-OPTION-TAG2.
           MOVE BR-GL-ENTRY-DEFAULT                   TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SALE FINANCE CALC INS. ADJUST    : " TO D-OPTION-TAG.
           MOVE BR-CALC-INS-ADJ                       TO D-FLAG.
           MOVE "MIDSTREAM 1ST PAY MISS DEFAULT   :"  TO D-OPTION-TAG2.
           MOVE BR-MIDSTREAM-1STPAY-FG                TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MUST ACCELERATE BEFORE REPO      : " TO D-OPTION-TAG.
           MOVE BR-AC-BEFORE-REPO                     TO D-FLAG.
           MOVE "NO DEFERMENTS FIRST 'X' MONTHS   :"  TO D-OPTION-TAG2.
           MOVE BR-NODEF-1ST-X-MONTHS                 TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ZERO DEFERMENT IS NOT ALLOWED    :"  TO D-OPTION-TAG2.
           MOVE BR-NODEF-IF-ZERO-AMT                  TO D-FLAG2.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *            BRANCH FILE MAINTENANCE - PAGE 5
      ******************************************************************

           MOVE "{PAGE 05}" TO D-TAG1.
           MOVE 02          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REMIT ADDRESS  : " TO D-P5-TAG.
           MOVE BR-REMIT1           TO D-P5-TEXT.
           MOVE "COLLECTION CENTER:" TO D-P5-MAIN-TEXT2.
           MOVE 02        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-REMIT2           TO D-P5-TEXT.
           MOVE "FLAG : " TO D-P5-TAG2.
           MOVE BR-CC-FLAG TO D-P5-TEXT2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-REMIT3           TO D-P5-TEXT.
           MOVE "ADDR : " TO D-P5-TAG2.
           MOVE BR-CC-ADR1 TO D-P5-TEXT2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CITY : " TO D-P5-TAG2.
           MOVE BR-CC-CITY TO D-P5-TEXT2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RECEIPT MESSAGE: " TO D-P5-TAG.
           MOVE BR-RECP1            TO D-P5-TEXT.
           MOVE "STATE: " TO D-P5-TAG2.
           MOVE BR-CC-STATE TO D-P5-TEXT2.
           MOVE "ZIP:" TO D-P5-ZIP-TAG.
           MOVE BR-CC-ZIP TO D-P5-ZIP.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-RECP2            TO D-P5-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "STATEMENT MESSG: " TO D-P5-TAG.
           MOVE BR-STMNT1           TO D-P5-TEXT.
           MOVE 02        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BR-STMNT2           TO D-P5-TEXT.
           PERFORM WRITE-DETAIL-LINE.


           MOVE "COLLECTION MANAGER: " TO D-P5-TAG.
           MOVE BR-COLLECT-MANAGER     TO D-P5-TEXT.
           MOVE 02        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "COLLECTION PHONE: " TO D-P5-TAG.
           MOVE BR-COLLECT-PHONE     TO D-P5-PHONE.
           INSPECT D-P5-PHONE REPLACING ALL "/" BY "-".
           MOVE " EXT: " TO D-P5-EXT-TAG.
           MOVE BR-COLLECT-EXT TO D-P5-EXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SUPERVISOR NAME   : " TO D-P5-TAG.
           MOVE BR-SUPERVISOR          TO D-P5-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "AREA MANAGER NAME : " TO D-P5-TAG.
           MOVE BR-AREA-MANAGER        TO D-P5-TEXT.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *            BRANCH FILE MAINTENANCE - PAGE 6
      ******************************************************************

           PERFORM HEAD-PAGE.

           MOVE "{PAGE 06}" TO D-TAG1.
           MOVE 02          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "AUTO OPENING CASH DRAWER : " TO D-P6-TAG.
           MOVE BR-AUTO-OPEN-BAL              TO D-P6-TEXT.
           MOVE 02        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OPEN BALANCE DRAWER 1    : " TO D-P6-TAG.
           MOVE BR-OPEN-BAL(1)                TO D-P6-AMT.
           MOVE 02        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OPEN BALANCE DRAWER 2    : " TO D-P6-TAG.
           MOVE BR-OPEN-BAL(2)                TO D-P6-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OPEN BALANCE DRAWER 3    : " TO D-P6-TAG.
           MOVE BR-OPEN-BAL(3)                TO D-P6-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OPEN BALANCE DRAWER 4    : " TO D-P6-TAG.
           MOVE BR-OPEN-BAL(4)                TO D-P6-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OPEN BALANCE DRAWER 5    : " TO D-P6-TAG.
           MOVE BR-OPEN-BAL(5)                TO D-P6-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OPEN BALANCE DRAWER 6    : " TO D-P6-TAG.
           MOVE BR-OPEN-BAL(6)                TO D-P6-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OPEN BALANCE DRAWER 7    : " TO D-P6-TAG.
           MOVE BR-OPEN-BAL(7)                TO D-P6-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OPEN BALANCE DRAWER 8    : " TO D-P6-TAG.
           MOVE BR-OPEN-BAL(8)                TO D-P6-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OPEN BALANCE DRAWER 9    : " TO D-P6-TAG.
           MOVE BR-OPEN-BAL(9)                TO D-P6-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REFNO'S FOR CASH TENDERED:" TO D-P6-TAG.
           MOVE BR-TENDER-CD(1)              TO D-P6-REFNO1.
           MOVE BR-TENDER-CD(2)              TO D-P6-REFNO2.
           MOVE BR-TENDER-CD(3)              TO D-P6-REFNO3.
           MOVE 02        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALLOW ALPHA CASH DRAWERS :" TO D-P6-TAG.
           MOVE BR-ALPHA-DRAWERS             TO D-P6-TEXT.
           MOVE 02        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *            BRANCH FILE MAINTENANCE - PAGE 7 (OPTIONS PAGE #3)
      ******************************************************************

           MOVE "{PAGE 07}" TO D-TAG1.
           MOVE 02          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "- - - OPTIONS - - -"                 TO D-OPTTITLE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ASSESS LATE CHGS IN THIS BRANCH  :"  TO D-P7-TAG.
           MOVE BR-ASSESS-LATE-CHGS                   TO D-P7-TEXT.
           MOVE 02        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NOT IN USE                       : " TO D-OPTION-TAG.
           MOVE SPACES                                TO D-FLAG.
           MOVE "GL JOURNAL ENTRY ALLOC SKIP FG   :"  TO D-P7-TAG.
           MOVE BR-JE-ALLOCATION-SKIP                 TO D-P7-FLAG.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NOT IN USE                       : " TO D-OPTION-TAG.
           MOVE SPACES                                TO D-FLAG.
           MOVE "BRANCH TYPE FLAG                 :"  TO D-P7-TAG.
           MOVE BR-WORLD-BRANCH-TYPE                  TO D-P7-FLAG.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NOT IN USE                       : " TO D-OPTION-TAG.
           MOVE SPACES                                TO D-FLAG.
           MOVE "ORDER QUEUE BY BEHAVIOR GRADE    :"  TO D-P7-TAG.
           MOVE BR-COL-QUEUE-BEHAVIOR-FG              TO D-P7-FLAG.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NOT IN USE                       : " TO D-OPTION-TAG.
           MOVE SPACES                                TO D-FLAG.
           MOVE "COUPONS OR STATEMENTS   (C/S)    :"  TO D-P7-TAG.
           MOVE BR-COUPON-STATEMENT                   TO D-P7-FLAG.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NOT IN USE                       : " TO D-OPTION-TAG.
           MOVE SPACES                                TO D-FLAG.
           MOVE "NOT IN USE                       :"  TO D-P7-TAG.
           MOVE SPACES                                TO D-P7-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "COLLECTION GROUP ID (LR)         :"  TO D-OPTION-TAG.
           MOVE BR-LR-GROUP-COLLID                    TO D-FLAG.
           MOVE "NOT IN USE                       :"  TO D-P7-TAG.
           MOVE SPACES                                TO D-P7-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "AUTO ACCOUNT NUMBER AT CONVERSION: " TO D-OPTION-TAG.
           MOVE BR-CONVERT-AUTO-ACCTNO                TO D-FLAG.
           MOVE "NOT IN USE                       :"  TO D-P7-TAG.
           MOVE SPACES                                TO D-P7-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "IMMEDIATE RECEIPT OVERRIDE       : " TO D-OPTION-TAG.
           MOVE BR-IMMEDIATE-RECEIPT                  TO D-FLAG.
           MOVE "NOT IN USE                       :"  TO D-P7-TAG.
           MOVE SPACES                                TO D-P7-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "GO DIRECTLY TO DOCUMENT PRINT    : " TO D-OPTION-TAG.
           MOVE BR-IMMEDIATE-DOC-PRINT                TO D-FLAG.
           MOVE "LASER PRINT OPTION               :"  TO D-P7-TAG.
           MOVE BR-OPTION-PST-LASER                   TO D-P7-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "AUTO REFUND/ACCELERATE P&L       : " TO D-OPTION-TAG.
           MOVE BR-AUTO-PL-REF-AC                     TO D-FLAG.
           MOVE "NOT IN USE                       :"  TO D-P7-TAG.
           MOVE SPACES                                TO D-P7-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALLOW SYS AS EFFECTIVE DATE      : " TO D-OPTION-TAG.
           MOVE BR-ALLOW-SYS-AS-EFFDATE               TO D-FLAG.
           MOVE "NOT IN USE                       :"  TO D-P7-TAG.
           MOVE SPACES                                TO D-P7-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LOAN INTERFACE (LI) SCAN WINDOW  : " TO D-OPTION-TAG.
           MOVE BR-LI-LISCAN-FG                       TO D-FLAG.
           MOVE "NOT IN USE                       :"  TO D-P7-TAG.
           MOVE SPACES                                TO D-P7-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ARE LOCK BOX PAYMENTS IN EFFECT  :" TO D-OPTION-TAG.
           MOVE BR-LBOX TO D-FLAG.
           MOVE "NOT IN USE                       :"  TO D-P7-TAG.
           MOVE "INSURANCE PRODUCER CODE          :"  TO D-P7-TAG.
           MOVE BR-INS-PRODUCER-NUM                   TO D-P7-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALLOW 'OV' TRAN CODE/NA LOCK BOX : " TO D-OPTION-TAG.
           MOVE BR-ALLOW-OV-TRCD                      TO D-FLAG.
           MOVE "                                  "  TO D-P7-TAG.
           MOVE SPACES                                TO D-P7-DATE.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ALLOW INTERBRNCH PMTS TO THIS BR : " TO D-OPTION-TAG.
           MOVE BR-ALLOW-INTERBR                      TO D-FLAG.
           MOVE "AD&D INSURANCE FEE CODE          : " TO D-P7-TAG.
           MOVE BR-ADD-INS-FEECODE                   TO D-P7-TEXT.

           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ORDER QUEUE BY PAST-DUE DAYS     : " TO D-OPTION-TAG.
           MOVE BR-COL-QUEUE-PAST-DUE                 TO D-FLAG.

           MOVE "SINGLE ROADGARD FEE CODE         :" TO D-P7-TAG.
           MOVE BR-ROAD-G-SGL-FEECODE                TO D-P7-TEXT.

           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "LI DLCOMM MAX PP / MAX 03        : " TO D-OPTION-TAG.
           MOVE SPACES                                TO D-FLAG.

           MOVE "FAMILY ROADGARD FEE CODE         :" TO D-P7-TAG.
           MOVE BR-ROAD-G-FAM-FEECODE                TO D-P7-TEXT.

           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PP "                                 TO D-PP-TAG.
           MOVE BR-LI-DLCOMM-CMSI-PP-MAX              TO D-PP-AMT.
           MOVE "O3 "                                 TO D-O3-TAG.
           MOVE BR-LI-DLCOMM-CMSI-O3-MAX              TO D-O3-AMT.

           PERFORM WRITE-DETAIL-LINE.

       END-DETAIL-LN-PROCESS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED      TO LN-COUNT.
           MOVE 1           TO LN-FEED.
           MOVE SPACES      TO DETAIL-LINE.

       EXIT-WRITE-DETAIL-LINE.
           EXIT.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           MOVE "N"         TO FIRST-TIME.
           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99    TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3 TO PR-REC.
           MOVE 2     TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 1 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************

           MOVE "BEGINNING BRANCH" TO RANGE-LINE.
           MOVE BEG-BR          TO RANGE-SELECTION.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING BRANCH" TO RANGE-LINE.
           MOVE END-BR       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO BRLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BRLIST-SYSDATE.
           DISPLAY BRLIST-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BRLIST-FORM-FIELDS TO WR-BUF.
           MOVE BRLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE BRLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE BRLIST-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/BRLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
      *-----------------------------------------------------------------
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF ( IO-BAD )
              MOVE "INVALID CODE" TO CD-DESC.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE!" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           IF EXT-ROUTE-BUF  = "00"
              MOVE "GB/GBMENU" TO FORM-NAM
              MOVE FORM-PATH   TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BRLIST-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
