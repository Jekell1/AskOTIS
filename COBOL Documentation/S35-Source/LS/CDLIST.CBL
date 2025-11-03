       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CDLIST.
       AUTHOR.        KEC.
       DATE-WRITTEN.  01/27/84
      ******************************************************************
      *
      *   PROGRAM NAME:  LS/CDLIST
      *
      *   DESCRIPTION :  CODE FILE LISTING
      *
      *-----------------------------------------------------------------
      *   REV:
      *   BAH 071197 ADDED CD-GLFEEFINCHG
      *   KEC 092298 ADDED NEW CODE TYPE OF 'PR';
      *               PRODUCT CODE (REVOLVING)
      *   KEC 100598 ADDED NEW CODE TYPE OF 'RP';
      *               REVOLVING PROGRAM CODE (REVOLVING)
      *   CS  011008 ADDED NEW CODE TYPE OF "TS" (TRW SPECIAL CODES)
      *   CS  011008 ADDED NEW CODE TYPE OF "{ " (ACTION CODE)
      *   BAH 020626 CHANGE OUTPUT NAME FOR TYPE OF "CH" OR IT DUPLICATES
      *              AUDIT/CHANGES REGISTER (CHLIST) REGACC PR# 1775
      *   BAH 030425 REMOVED ALL TRACES OF "GC" G/L FEE CODES
      *              HAS ITS OWN LISTING, TOO CONFUSING (LS/GCLIST)
      *   CS  040622 ADDED NEW CODE OF "RO" (REVOLVING OTHER CHARGES)
      *   BAH 110919 REPLACED RRFORM WITH SCREEN
      *   BAH 120904 ADDED CD-TITLE-HOLD-DAYS FOR "PO" REGACC PR# 319
      * KEC 2016.0309 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-ACTION???? TO BE CD-ACTION-????.
      *         CHANGED CD-GROUP, CD-SECURED & CD-TRW TO BE
      *         CD-CO-GROUP, CD-CO-SECURED & CD-CO-TRW.
      *         CHANGED CD-POOL???? TO BE CD-PL-????.
      * BAH 190204 SPLIT CDFILE
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/CDLIST.CBL S35 02/23/24-10:16:10 >".

       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  FIRST-TIME                  PIC X(01)     VALUE "Y".
       01  MESS-LIST                   PIC X(05)     VALUE "MESS.".

       01  HOLD-FORM-PATHNAME          PIC X(22).
       01  CODE-TYPE-LS                PIC X(02)  VALUE SPACES.
           88  CDTYPE-ACTION                      VALUE "AC".
           88  CDTYPE-AT                          VALUE "AT".
           88  CDTYPE-BS                          VALUE "BS".
           88  CDTYPE-BW                          VALUE "BW".
           88  CDTYPE-CH                          VALUE "CH".
           88  CDTYPE-CL                          VALUE "CL".
           88  CDTYPE-CO                          VALUE "CO".
           88  CDTYPE-DF                          VALUE "DF".
           88  CDTYPE-DI                          VALUE "DI".
           88  CDTYPE-DR                          VALUE "DR".
           88  CDTYPE-EA                          VALUE "EA".
           88  CDTYPE-EL                          VALUE "EL".
           88  CDTYPE-FC                          VALUE "FC".
           88  CDTYPE-FU                          VALUE "FU".
           88  CDTYPE-ID                          VALUE "ID".
           88  CDTYPE-JT                          VALUE "JT".
           88  CDTYPE-LO                          VALUE "LO".
           88  CDTYPE-LS                          VALUE "LS".
           88  CDTYPE-LV                          VALUE "LV".
           88  CDTYPE-ML                          VALUE "ML".
           88  CDTYPE-PA                          VALUE "PA".
           88  CDTYPE-PC                          VALUE "PC".
           88  CDTYPE-PH                          VALUE "PH".
           88  CDTYPE-PL                          VALUE "PL".
           88  CDTYPE-PO                          VALUE "PO".
           88  CDTYPE-PR                          VALUE "PR".
           88  CDTYPE-PS                          VALUE "PS".
           88  CDTYPE-PU                          VALUE "PU".
           88  CDTYPE-PX                          VALUE "PX".
           88  CDTYPE-RC                          VALUE "RC".
           88  CDTYPE-RE                          VALUE "RE".
           88  CDTYPE-RF                          VALUE "RF".
           88  CDTYPE-RL                          VALUE "RL".
           88  CDTYPE-RO                          VALUE "RO".
           88  CDTYPE-SR                          VALUE "SR".
           88  CDTYPE-TS                          VALUE "TS".
           88  CDTYPE-UW                          VALUE "UW".
           88  CDTYPE-FZ                          VALUE "FZ".
           88  CDTYPE-XL                          VALUE "XL".

      *=================================================================
      *
      *    S C R E E N - F I E L D S
      *
      *=================================================================
       01  ERRCD                   PIC X(01).

      *-----------------------------------------------------------------

       01  ACCEPT-BUF                  PIC X(01)     VALUE SPACES.

       01  LN-WORKERS.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.

       01  PAGE-NUMBER                 PIC 9(04)     VALUE ZEROES.

       01  HEAD1.
           03  H-DATE                  PIC X(08)     VALUE SPACES.
           03  FILLER                  PIC X(02)     VALUE SPACES.
           03  H-HR                    PIC 9(02)     VALUE ZEROES.
           03  FILLER                  PIC X(01)     VALUE ":".
           03  H-MIN                   PIC 9(02)     VALUE ZEROES.
           03  FILLER                  PIC X(31)     VALUE SPACES.
           03  H-NAME                  PIC X(40)     VALUE SPACES.
           03  FILLER                  PIC X(37)     VALUE SPACES.
           03  FILLER                  PIC X(05)     VALUE "PAGE ".
           03  H-PAGE-NO               PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER                  PIC X(10)     VALUE "USER ID:".
           03  H-USERID                PIC X(10).
           03  FILLER                  PIC X(23)     VALUE SPACES.
           03  H-TITLE                 PIC X(45)     VALUE SPACES.
           03  FILLER                  PIC X(44)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER                  PIC X(18)    VALUE SPACES.
           03  FILLER                  PIC X(04)    VALUE "CODE".
           03  FILLER                  PIC X(08)    VALUE SPACES.
           03  FILLER                  PIC X(11)    VALUE "DESCRIPTION".
           03  FILLER                  PIC X(16)    VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEAD-PR-LINE.
           03  FILLER                  PIC X(18)    VALUE SPACES.
           03  FILLER                  PIC X(05)    VALUE "CODE ".
           03  FILLER                  PIC X(03)    VALUE SPACES.
           03  FILLER                  PIC X(35)    VALUE "DESCRIPTION".
           03  FILLER                  PIC X(19)    VALUE SPACES.

      *=================================================================
      *
      *    D E T A I L - L I N E
      *
      *=================================================================
       01  DETAIL-LINE                 PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  D-LINE                      REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(18).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-KEY-AREA.
               10  D-CODE              PIC X(08).
               10  FILLER              PIC X(04).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  FILLER                  REDEFINES D-KEY-AREA.
               10  D-POOLID            PIC 9(05).
               10  FILLER              PIC X(07).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  DISPLAY-NON-GC.
               05  D-DESC              PIC X(35).
               05  FILLER              PIC X(08).
               05  D-TRWX              PIC X(04).
               05  D-TRW               PIC X(02).
               05  FILLER              PIC X(03).
               05  D-SECX              PIC X(08).
               05  D-SEC               PIC X(01).
               05  FILLER              PIC X(41).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  DISPLAY-PO-CODE          REDEFINES DISPLAY-NON-GC.
               05  FILLER              PIC X(40).
               05  D-PO-TAG            PIC X(16).
               05  D-TITLE-HOLD-DAYS   PIC Z9.
               05  FILLER              PIC X(44).


      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       01  CDLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".

       COPY "LIBLS/ACLIST_DEF.CPY".
       COPY "LIBLS/BSLIST_DEF.CPY".
       COPY "LIBLS/CDCHLS_DEF.CPY".
       COPY "LIBLS/COLIST_DEF.CPY".
       COPY "LIBLS/LSLIST_DEF.CPY".
       COPY "LIBLS/PLLIST_DEF.CPY".
       COPY "LIBLS/POLIST_DEF.CPY".
       COPY "LIBLS/PULIST_DEF.CPY".
       COPY "LIBLS/SRLIST_DEF.CPY".
       COPY "LIBLS/TSLIST_DEF.CPY".

       COPY "LIBLS/ACLIST_WKS.CPY".
       COPY "LIBLS/BSLIST_WKS.CPY".
       COPY "LIBLS/CDCHLS_WKS.CPY".
       COPY "LIBLS/COLIST_WKS.CPY".
       COPY "LIBLS/LSLIST_WKS.CPY".
       COPY "LIBLS/PLLIST_WKS.CPY".
       COPY "LIBLS/POLIST_WKS.CPY".
       COPY "LIBLS/PULIST_WKS.CPY".
       COPY "LIBLS/SRLIST_WKS.CPY".
       COPY "LIBLS/TSLIST_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       01  CDTYPE                  PIC X(02).
       01  CDTAG                   PIC X(40).

       SCREEN SECTION.
       COPY "LIBLS/ACLIST_SCN.CPY".
       COPY "LIBLS/BSLIST_SCN.CPY".
       COPY "LIBLS/CDCHLS_SCN.CPY".
       COPY "LIBLS/COLIST_SCN.CPY".
       COPY "LIBLS/LSLIST_SCN.CPY".
       COPY "LIBLS/PLLIST_SCN.CPY".
       COPY "LIBLS/POLIST_SCN.CPY".
       COPY "LIBLS/PULIST_SCN.CPY".
       COPY "LIBLS/SRLIST_SCN.CPY".
       COPY "LIBLS/TSLIST_SCN.CPY".


      ******************************************************************
       PROCEDURE DIVISION
      ******************************************************************
             USING FORM-PATHNAME EXIT-PATHNAME CDTYPE CDTAG.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO EXIT-PROG.                                                

           PERFORM INITIALIZATION.

           IF ( ERRCD NOT = SPACES )
              GO TO EXIT-MAIN-PROGRAM.

           PERFORM PROCESS-LISTING.

       EXIT-MAIN-PROGRAM.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE SPACES TO ERRCD.

      * SETS VDU-FORM-NAME
           PERFORM ACCESS-CODE-TYPE.
           IF STAT NOT = "00"
              GO TO ENDIT.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS CDLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       SETRP.
           IF ( CDTYPE-ACTION )
              MOVE ACLIST-QUEUE-POS TO Q-POS
              MOVE ACLIST-COPIES-POS TO C-POS
              MOVE ACLIST-PRU-POS TO P-POS
           ELSE
              MOVE BSLIST-QUEUE-POS TO Q-POS
              MOVE BSLIST-COPIES-POS TO C-POS
              MOVE BSLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.

           IF ( STAT NOT = "00" )
              GO TO ENDIT.

      *-----------------------------------------------------------------
       OK-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.

           IF ( ACCEPT-BUF     = "N"  ) GO TO ENDIT.
           IF ( ACCEPT-BUF NOT = "Y"  ) GO TO OK-YN.

       CONTINUE-INITIALIZATION.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE CDTAG              TO H-TITLE.

           PERFORM OPEN-CD1-FILE.

           MOVE CDTYPE TO CD-TYPE
                          QCD1-WBEG-TYPE
                          QCD1-WEND-TYPE.
           IF ( CDTYPE = "PL" )
               MOVE ZEROES TO CD-PL-POOLID
           ELSE
               MOVE SPACES TO CD-CODE.

           MOVE CD-CODE    TO QCD1-WBEG-CODE
           MOVE ALL "Z"    TO QCD1-WEND-CODE.

           PERFORM START-CD1-FILE.
           IF ( IO-BAD )
              GO TO ENDIT.

           MOVE "LISTING IN PROGRESS..............." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
      *
      *    A C C E S S - C O D E - T Y P E --- ( CODE DESCRIPTION FILE )
      *
      *-----------------------------------------------------------------
      *          TYPE: AC -  ACTION CODES
      *                AT -  ACTIVITY CODES
      *                BW -  BORROWER HOUSING CODES
      *                CH -  CHARGE OFF (P&L) REASON CODE
      *                CO -  COLLATERAL CODES
      *                DI -  DISPOSAL / HOLD COLLATERAL (REPO)
      *                EA -  EXTRACTION/ACTION GROUP ID
      *                EL -  EXTENDED LATE CHARGE INFO
      *                FU -  FOLLOW-UP CODES
      *                ID -  ID TYPE (KNOW YOUR CUSTOMER SCREENS)
      *                LS -  REPO REASON CODES
      *                LV -  GROUP LEVELS
      *                ML -  MAIL LIST SOURCE CODES
      *                PL -  POOL ID CODES
      *                PO -  PAYOFF CODES
      *                PS -  PAYMENT RESCHEDULE REASON
      *                PU -  PURPOSE CODES
      *                PX -  P&L CODES
      *                RE -  RESULT CODES
      *                RL -  RELATION CODES
      *                SR -  SOURCE CODES
      ******************************************************************
       ACCESS-CODE-TYPE SECTION.
      ******************************************************************

           MOVE FORM-PATHNAME TO HOLD-FORM-PATHNAME FORM-PATH.
           MOVE CDTYPE TO CODE-TYPE-LS.

           IF ( CDTYPE-ACTION )
              MOVE "LS/ACLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-BS )
              MOVE "LS/BSLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-CO )
              MOVE "LS/COLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-LV )
              MOVE "LS/LVLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-ML )
              MOVE "LS/MLLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-PC )
              MOVE "LS/PCLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-PH )
              MOVE "LS/PHLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-PL )
              MOVE "LS/PLLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-PO )
              MOVE "LS/POLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-PU )
              MOVE "LS/PULIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-PX )
              MOVE "LS/PXLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-RE )
              MOVE "LS/RELIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-RL )
              MOVE "LS/RLLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-SR )
              MOVE "LS/SRLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-LO )
              MOVE "LS/LOLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-TS )
              MOVE "LS/TSLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-LS )
              MOVE "LS/LSLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-CH )
              MOVE "LS/CDCHLS" TO FORM-NAM
           ELSE
           IF ( CDTYPE-UW )
              MOVE "LS/UWLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-FZ )
              MOVE "LS/FZLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-ID )
              MOVE "LS/IDLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-JT )
              MOVE "LS/JTLIST" TO FORM-NAM
           ELSE
           IF ( CDTYPE-PS )
              MOVE "LS/PSCDMN" TO FORM-NAM
           ELSE
           IF ( CDTYPE-XL )
              MOVE "LS/XLLIST" TO FORM-NAM
           ELSE
              MOVE "XX" TO STAT
              GO TO EXIT-ACCESS-CODE-TYPE.

       END-ACCESS-CODE-TYPE.
           MOVE FORM-PATH TO FORM-PATHNAME.

      * FORM-PROG WILL CREATE THE /USR/TMP WITH CORRECT PROGRAM NAME INSTEAD
      * OF THEM ALL BEING CDLIST I.E. ACLIST, ETC.
           MOVE FORM-PROGX TO VDU-FORM-NAME FORM-PROG.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE HOLD-FORM-PATHNAME TO FORM-PATHNAME FORM-PATH.

       EXIT-ACCESS-CODE-TYPE.
           EXIT.

      ******************************************************************
       PROCESS-LISTING SECTION.
      ******************************************************************

           MOVE SPACES TO DETAIL-LINE.

       NEXT-PROCESS-LISTING.
           PERFORM READ-CD1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO EXIT-PROCESS-LISTING.

           IF ( CD-TYPE NOT = CDTYPE )
               GO TO EXIT-PROCESS-LISTING.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-PROCESS-LISTING.

       EXIT-PROCESS-LISTING.
           EXIT.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************

           MOVE CD-CODE TO D-CODE.
           MOVE CD-DESC TO D-DESC.

           IF ( CD-TYPE = "PL" )
              MOVE CD-PL-POOLID TO D-POOLID.

           IF ( CD-TYPE = "CO" )
              MOVE "TRW-"        TO D-TRWX
              MOVE CD-CO-TRW     TO D-TRW
              MOVE "SECURED "    TO D-SECX
              MOVE CD-CO-SECURED TO D-SEC.

           IF ( CD-TYPE = "PO" )
              MOVE "TITLE HOLD DAYS "    TO D-PO-TAG
              MOVE CD-PO-TITLE-HOLD-DAYS TO D-TITLE-HOLD-DAYS.

           GO TO PRINTING-DETAIL-LINE.

       PRINTING-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

       EXIT-DETAIL-LINE-PROCESS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( LN-WK > EXT-CONAME-RPT-LINES )
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED      TO LN-COUNT.
           MOVE 1           TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

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

      *-----------------------------------------------------------------
       PRINT-HEAD-PAGE-LINE-01.

           MOVE HEAD1 TO PR-REC.
           MOVE 99    TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *-----------------------------------------------------------------
       PRINT-HEAD-PAGE-LINE-02.

           MOVE HEAD2 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *-----------------------------------------------------------------
       PRINT-HEAD-PAGE-LINE-03.

           IF ( CDTYPE = "PR" )
              MOVE HEAD-PR-LINE   TO PR-REC
           ELSE
              MOVE HEAD3          TO PR-REC.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *-----------------------------------------------------------------
       PRINT-HEAD-PAGE-LINE-06.

           MOVE ALL "=" TO PR-REC
           MOVE 1       TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *=================================================================
       END-HEAD-PAGE.

           IF ( CDTYPE = "RP" )
              MOVE 7 TO LN-COUNT
           ELSE
              MOVE 4 TO LN-COUNT.

           MOVE 2 TO LN-FEED.

       EXIT-HEAD-PAGE.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           EVALUATE CODE-TYPE-LS
              WHEN "AC " MOVE EXT-CONAME-CONAME TO ACLIST-CONAME
                        MOVE EXT-CONAME-SYSDATE TO ACLIST-SYSDATE
                        DISPLAY ACLIST-SCREEN UPON CDLIST-WINDOW-HANDLE
                        MOVE ACLIST-FORM-FIELDS TO WR-BUF 
                        MOVE ACLIST-FORM-LIST   TO WR-LIST 
              WHEN "BS" MOVE EXT-CONAME-CONAME TO BSLIST-CONAME
                        MOVE EXT-CONAME-SYSDATE TO BSLIST-SYSDATE
                        DISPLAY BSLIST-SCREEN UPON CDLIST-WINDOW-HANDLE
                        MOVE BSLIST-FORM-FIELDS TO WR-BUF 
                        MOVE BSLIST-FORM-LIST   TO WR-LIST 
              WHEN "CH" MOVE EXT-CONAME-CONAME  TO CDCHLS-CONAME
                        MOVE EXT-CONAME-SYSDATE TO CDCHLS-SYSDATE
                        DISPLAY CDCHLS-SCREEN UPON CDLIST-WINDOW-HANDLE
                        MOVE CDCHLS-FORM-FIELDS TO WR-BUF 
                        MOVE CDCHLS-FORM-LIST   TO WR-LIST 
              WHEN "CO" MOVE EXT-CONAME-CONAME  TO COLIST-CONAME
                        MOVE EXT-CONAME-SYSDATE TO COLIST-SYSDATE
                        DISPLAY COLIST-SCREEN UPON CDLIST-WINDOW-HANDLE
                        MOVE COLIST-FORM-FIELDS TO WR-BUF 
                        MOVE COLIST-FORM-LIST   TO WR-LIST 
              WHEN "LS" MOVE EXT-CONAME-CONAME  TO LSLIST-CONAME
                        MOVE EXT-CONAME-SYSDATE TO LSLIST-SYSDATE
                        DISPLAY LSLIST-SCREEN UPON CDLIST-WINDOW-HANDLE
                        MOVE LSLIST-FORM-FIELDS TO WR-BUF 
                        MOVE LSLIST-FORM-LIST   TO WR-LIST 
              WHEN "PL" MOVE EXT-CONAME-CONAME  TO PLLIST-CONAME
                        MOVE EXT-CONAME-SYSDATE TO PLLIST-SYSDATE
                        DISPLAY PLLIST-SCREEN UPON CDLIST-WINDOW-HANDLE
                        MOVE PLLIST-FORM-FIELDS TO WR-BUF 
                        MOVE PLLIST-FORM-LIST   TO WR-LIST 
              WHEN "PO" MOVE EXT-CONAME-CONAME  TO POLIST-CONAME
                        MOVE EXT-CONAME-SYSDATE TO POLIST-SYSDATE
                        DISPLAY POLIST-SCREEN UPON CDLIST-WINDOW-HANDLE
                        MOVE POLIST-FORM-FIELDS TO WR-BUF 
                        MOVE POLIST-FORM-LIST   TO WR-LIST 
              WHEN "PU" MOVE EXT-CONAME-CONAME  TO PULIST-CONAME
                        MOVE EXT-CONAME-SYSDATE TO PULIST-SYSDATE
                        DISPLAY PULIST-SCREEN UPON CDLIST-WINDOW-HANDLE
                        MOVE PULIST-FORM-FIELDS TO WR-BUF 
                        MOVE PULIST-FORM-LIST   TO WR-LIST 
              WHEN "SR" MOVE EXT-CONAME-CONAME  TO SRLIST-CONAME
                        MOVE EXT-CONAME-SYSDATE TO SRLIST-SYSDATE
                        DISPLAY SRLIST-SCREEN UPON CDLIST-WINDOW-HANDLE
                        MOVE SRLIST-FORM-FIELDS TO WR-BUF 
                        MOVE SRLIST-FORM-LIST   TO WR-LIST 
              WHEN "TS" MOVE EXT-CONAME-CONAME  TO TSLIST-CONAME
                        MOVE EXT-CONAME-SYSDATE TO TSLIST-SYSDATE
                        DISPLAY TSLIST-SCREEN UPON CDLIST-WINDOW-HANDLE
                        MOVE TSLIST-FORM-FIELDS TO WR-BUF 
                        MOVE TSLIST-FORM-LIST   TO WR-LIST 
           END-EVALUATE.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           EVALUATE CODE-TYPE-LS
              WHEN "AC" MOVE ACLIST-HELP--FILE TO HELP-LINK-FILE
                        MOVE ACLIST-HELP--DIR TO HELP-LINK-DIR
              WHEN "BS" MOVE BSLIST-HELP--FILE TO HELP-LINK-FILE
                        MOVE BSLIST-HELP--DIR TO HELP-LINK-DIR
              WHEN "CH" MOVE CDCHLS-HELP--FILE TO HELP-LINK-FILE
                        MOVE CDCHLS-HELP--DIR TO HELP-LINK-DIR
              WHEN "CO" MOVE COLIST-HELP--FILE TO HELP-LINK-FILE
                        MOVE COLIST-HELP--DIR TO HELP-LINK-DIR
              WHEN "LS" MOVE LSLIST-HELP--FILE TO HELP-LINK-FILE
                        MOVE LSLIST-HELP--DIR TO HELP-LINK-DIR
              WHEN "PL" MOVE PLLIST-HELP--FILE TO HELP-LINK-FILE
                        MOVE PLLIST-HELP--DIR TO HELP-LINK-DIR
              WHEN "PO" MOVE POLIST-HELP--FILE TO HELP-LINK-FILE
                        MOVE POLIST-HELP--DIR TO HELP-LINK-DIR
              WHEN "PU" MOVE PULIST-HELP--FILE TO HELP-LINK-FILE
                        MOVE PULIST-HELP--DIR TO HELP-LINK-DIR
              WHEN "SR" MOVE SRLIST-HELP--FILE TO HELP-LINK-FILE
                        MOVE SRLIST-HELP--DIR TO HELP-LINK-DIR
              WHEN "TS" MOVE TSLIST-HELP--FILE TO HELP-LINK-FILE
                        MOVE TSLIST-HELP--DIR TO HELP-LINK-DIR
           END-EVALUATE.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/ACLIST_EVA.CPY".
              COPY "LIBLS/CDCHLS_EVA.CPY".
              COPY "LIBLS/BSLIST_EVA.CPY".
              COPY "LIBLS/COLIST_EVA.CPY".
              COPY "LIBLS/LSLIST_EVA.CPY".
              COPY "LIBLS/PLLIST_EVA.CPY".
              COPY "LIBLS/POLIST_EVA.CPY".
              COPY "LIBLS/PULIST_EVA.CPY".
              COPY "LIBLS/SRLIST_EVA.CPY".
              COPY "LIBLS/TSLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
      ******************************************************************
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.

           IF ( FIRST-TIME = "Y" )
              MOVE " NO RECORDS FOUND " TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CDLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
