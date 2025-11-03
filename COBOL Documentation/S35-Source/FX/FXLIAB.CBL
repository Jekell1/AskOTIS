       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FXLIAB.
       DATE-WRITTEN.    020294.
      ****************************************************
      *  DESCRIPTION:  FIX BW-LOANS, BW-PRLOANS, BW-LIASMLN, BW-LIAOTH
      *                    LN-TOTERN, LN-FINCHG AND ALL LN-(LCFLAGS)
      *                    LN-GIFTNET
      *                    BW-INTYTD, BW-INTLYR, BW-RE-INTYTD,
      *                    BW-RE-INTLYR
      *
      * REV:
      *  JTG 101289 ADDED LOGIC FOR BW-PRLOANS
      *             FIXED LOGIC FOR LN-TOTERN RE: COMPONENTS
      *  BAH 113090 ADDED LOGIC FOR BW-RE-INTYTD BW-RE-INTLYR
      *  BAH 070594 ADDED FIXLN-BUF, DONT DESTROY CONVERSION INFO
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  JTG 072597 ADDED SET OF LN-GIFTNET & USE OF TOTAL-FEEFINCHG
      *                                          AND TOTAL-INSFINCHG
      *  KEC 021699 ADDED CHECK TO LOOK FOR REVOLVING LOANS WITH
      *              A CURRENT BALANCE (LN-CURBAL) OF ZERO.
      *  JTG 022699 MOVED CHECK TO LOOK FOR REVOLVING LOANS AND ADD 1
      *             TO BW-LOANS INDEPENDENT OF LN-CURBAL
      *  JTG 990720 ADDED LOGIC FOR LN-ACTIVE-FG     REGENCY REVOLVING #45
      *             ALSO ALLOWED LOAN UPDATE OPTION OF "R" WHICH WILL
      *             UPDATE THE LOAN LIKE 'Y' BUT ALSO UPDATE LN-ACTIVE-FG
      *  GLF 990728 ADDED PRINT OPTIONS AND A REPORT TO PRINT FOR MISSING
      *             BORROWERS
      *  BLV 030103 FIXED BUGS WHICH HAD BEEN FIXED IN A10 & LOST OR NEVER
      *             GOT TO A15, IF "Y" FOR PRIOR LOANS, FIX LOOPING THROUGH
      *             BORROWER HISTORY & ADDING TO PRIOR LOAN COUNT FOR EVERY
      *             LOAN IN LNFILE AND DIDN'T SKIP VOIDS IN HISTORY
      *   CS 061110 ALLOW TO RUN IN BATCH, ALSO REMOVED THE ACCEPT OPTION OF "S"      *             PER DISCUS WITH BETH [REGIONAL ACCEPT PR#153]
      *
      * KEC 2016.0309 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-GROUP, CD-SECURED & CD-TRW TO BE
      *         CD-CO-GROUP, CD-CO-SECURED & CD-CO-TRW.
      * BAH 181002 GLOBAL BWFILE
      * BAH 181126 GLOBAL LHFILE
      * BAH 181203 GLOBAL LNFILE
      * BAH 190918 REMOVED ACCESS-CALL ON LNFILE
      * BAH 2024.1002 SEND-LEGEND WASNT DISPLAYING "IN PROGRESS" DUE TO NOT
      *               USING MESS-BUF S35Q-136
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

      ******************************************
      *      WORKING STORAGE SECTION
      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)FX/FXLIAB S35 10/02/24-11:44:06 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LH.CPY".
       COPY "LIBLP/LP01LH_SQL.CPY".
       COPY "LIBLP/LPWSLH.CPY".
      *COPY "LIBLP/LP01NO.CPY".
      *COPY "LIBLP/LP01NO_SQL.CPY".
      *COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  PR-IFN            PIC X(2) VALUE "PR".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.

       01  SUB               PIC 99.
      * YYYYMMDD
       01  TODAYS-DATE       PIC 9(8).

       01  MESS-LIST         PIC X(5)  VALUE "MESS.".
       01  MESS-BUF          PIC X(40) VALUE SPACES.

       01  LC-FLAG-TAB       VALUE " ".
           03  LC-FLAGS      OCCURS 99.
               05  FLAGS-SET PIC X.
               05  SMALLOAN  PIC X.
               05  SALEFIN   PIC X.
               05  SERVACCT  PIC X.
               05  LOANTYPE  PIC X.
               05  GIFTNET   PIC X.

       COPY "LIBGB/EOBUF_EXT.CPY".
           03 BRANCH             PIC 9(4).
           03 PRLN-BUF           PIC X.
           03 FIXLN-BUF          PIC X.

       01  VERSION-CONTROL       PIC X VALUE "A".

       01  DETAIL-LINE       PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO1       PIC Z(4).
           03  FILLER        PIC X.
           03  D-MESS        PIC X(18).
           03  FILLER        PIC X.
           03  D-SSNO        PIC 999/99/9999.
           03  FILLER        PIC X.
           03  D-LOAN        PIC X(6).
           03  FILLER        PIC X.
           03  D-ACCTNO      PIC 9(8).

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  D-BRNO2       PIC Z(4).
           03  FILLER        PIC X.
           03  D-MESS2       PIC X(22).
           03  D-CLASS       PIC 9(2).
           03  FILLER        PIC X.
           03  D-LOAN2       PIC X(6).
           03  FILLER        PIC X.
           03  D-ACCTNO2     PIC 9(8).

       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/FXLIAB_DEF.CPY".
       COPY "LIBFX/FXLIAB_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/FXLIAB_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LH1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *******************************************************
       MAIN-PROGRAM SECTION.
      *******************************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "FXLIAB" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
               GO TO END-ROUTINE.

           IF BRANCH = 0
              MOVE 0 TO BR-NO
           ELSE
              MOVE BRANCH TO BR-NO.

           PERFORM START-BR-FILE.

       NEXT-BR.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF BRANCH NOT = 0
              IF BR-NO NOT = BRANCH
                 GO TO END-ROUTINE.

      *NEXT-BR-ACCEPT.
      *    IF ACCEPT-BUF = "S"
      *       MOVE "A" TO ACCEPT-BUF
      *    ELSE
      *    IF ACCEPT-BUF = "A"
      *       MOVE "E  A010ACCEPT." TO VDU
      *       PERFORM SCREENIO
      *       IF VDUSTATUS = "1>"
      *          GO TO END-ROUTINE
      *       ELSE
      *          IF NOT (VDUBUF = "Y" OR "S")
      *             GO TO NEXT-BR-ACCEPT.


           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF BR-DATA-PATH = " " OR BR-MACHINE = " "
                 STRING "BRANCH "
                           BR-NO
                              " MISSING DATA PATH, WILL SKIP"
                                    DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS
                 GO TO NEXT-BR.

      D    STOP MESS.
           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF EXT-ROUTE-BUF = "00"
                  STRING "PROGRAM IN PROGRESS......BRNO: " BR-NO
                             DELIMITED BY SIZE INTO MESS-BUF
                  PERFORM SEND-LEGEND.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LH-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM GLOBAL-UPDATE.

           GO TO NEXT-BR.

       END-ROUTINE.

           GO TO END-PROGRAM.

      *******************************************************
       GLOBAL-UPDATE SECTION.
      *******************************************************

           PERFORM OPEN-LN1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.
 
           PERFORM START-LN1-FILE.

           PERFORM OPEN-BW1-FILE.
           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE 0             TO BW-SSNO.

           PERFORM START-BW1-FILE.

           PERFORM OPEN-LH1-FILE.
           PERFORM OPEN-CD1-FILE.

      ********************************************
      *    BUILD FLAG TABLE
      ********************************************

           PERFORM OPEN-LC1-FILE.
           MOVE 0 TO LC-CODE.
           PERFORM START-LC1-FILE.
       NEXT-LC.
           PERFORM READ-LC1-FILE-NEXT.
           IF IO-FG = 9
              PERFORM CLOSE-LC1-FILE
              GO TO NEXT-BW.

           IF LC-CODE NOT = 0
              MOVE "Y"           TO FLAGS-SET(LC-CODE)
              MOVE LC-SMALLOAN   TO SMALLOAN(LC-CODE)
              MOVE LC-SALEFIN    TO SALEFIN(LC-CODE)
              MOVE LC-SERVACCT   TO SERVACCT(LC-CODE)
              MOVE LC-LOANTYPE   TO LOANTYPE(LC-CODE)
              IF LC-GIFTNET = " "
                 MOVE "N"        TO GIFTNET(LC-CODE)
              ELSE
                 MOVE LC-GIFTNET TO GIFTNET(LC-CODE).

           GO TO NEXT-LC.

      ********************************************
      *    CLEAR BW FIELDS.
      ********************************************
       NEXT-BW.
           PERFORM READ-BW1-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR) 
              GO TO NEXT-LN.

           MOVE 0 TO BW-LOANS  BW-LIASMLN BW-RE-INTYTD BW-RE-INTLYR
                     BW-LIAOTH BW-INTYTD  BW-INTLYR.
           IF PRLN-BUF = "Y"
              MOVE 0 TO BW-PRLOANS.

           PERFORM REWRITE-BW1-FILE.

           GO TO NEXT-BW.

      ********************************************
      *    UPDATE LN / BW FIELDS
      ********************************************
       NEXT-LN.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO GLOBAL-UPDATE-EXIT.

           IF ( LN-POCD-NONLOAN )
      *       UNLOCK LN-FILE
              GO TO NEXT-LN.

           IF LN-CLASS = 0 OR FLAGS-SET(LN-CLASS) NOT = "Y"
      *       UNLOCK LN-FILE
              PERFORM CLASS-DETAIL-LINE-PROCESS
              GO TO NEXT-LN.

           IF FIXLN-BUF = "Y" 
              MOVE LN-REC TO HOLD-LN-REC
              MOVE SMALLOAN(LN-CLASS)  TO LN-SMALLOAN
              MOVE SALEFIN(LN-CLASS)   TO LN-SALEFIN
              MOVE SERVACCT(LN-CLASS)  TO LN-SERVACCT
              MOVE LOANTYPE(LN-CLASS)  TO LN-LOANTYPE
              MOVE GIFTNET(LN-CLASS)   TO LN-GIFTNET
              PERFORM COMPUTE-EARNINGS
              COMPUTE LN-TOTERN = LN-TOTINT + LN-TOTLCHG + LN-TOTDEF
                                + EARN-TOT-ACCERN
              PERFORM GET-SP
              PERFORM LOAN-CALCULATIONS
              COMPUTE LN-FINCHG = LN-INTCHG + LN-EXTCHG + LN-SERCHG
                                + (LN-MAINTFEE * LN-ORGTERM)
                                + TOTAL-INSFINCHG
                                + TOTAL-FEEFINCHG
              IF LN-REC NOT = HOLD-LN-REC
                 PERFORM REWRITE-LN1-FILE.
      *          PERFORM UPDATE-NOTICE-FILE.

           MOVE 0 TO SUB.
       UPD-LIAB.
           ADD 1 TO SUB.
           IF SUB > 4
              GO TO NEXT-LN.
           IF LN-SSNO(SUB) = 0
              GO TO UPD-LIAB.

           MOVE LN-SSNO(SUB) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              PERFORM BORR-DETAIL-LINE-PROCESS
              GO TO UPD-LIAB.

      * UPDATE INTEREST YTD:
      * READ COLLATERAL TO DETERMINE IF REAL ESTATE
           MOVE LN-COLLCD TO CD-CODE.
           MOVE "CO"      TO CD-TYPE.
           PERFORM READ-CD1-FILE.
           IF IO-FG NOT = 0
              MOVE " " TO CD-CO-SECURED.

           IF SUB = 1
              IF CD-CO-SECURED = "R"
                 ADD LN-YTDINTPD TO BW-RE-INTYTD
                 ADD LN-LYRINTPD TO BW-RE-INTLYR
              ELSE
                 ADD LN-YTDINTPD TO BW-INTYTD
                 ADD LN-LYRINTPD TO BW-INTLYR.

           IF LN-CURBAL NOT = 0
              ADD 1 TO BW-LOANS.

           IF LN-SERVACCT = "Y"
              ADD LN-CURBAL TO BW-LIAOTH
           ELSE
              IF LN-SMALLOAN = "Y"
                 ADD LN-CURBAL TO BW-LIASMLN
              ELSE
                 ADD LN-CURBAL TO BW-LIAOTH.

           IF PRLN-BUF = "N"
              GO TO REWRITE-BW1.

      * SEARCH HISTORY FILES FOR PRIOR LOANS

           IF ( LN-CURBAL = 0 )
              ADD 1 TO BW-PRLOANS.

      * CAN ONLY LOOP THRU HERE ONCE !! IT WAS GOING THRU HISTORY FOR EVERY
      * LOAN IN THE LN-FILE !!
           IF BW-PRLOANS > 1
              GO TO REWRITE-BW1.

           MOVE LH-PATH-OWNBR TO LH-BRNO
                                 QLH1-WBEG-BRNO
                                 QLH1-WEND-BRNO.
           MOVE LN-SSNO(SUB)  TO LH-SSNO
                                 QLH1-WBEG-SSNO
                                 QLH1-WEND-SSNO.
           MOVE 0             TO LH-ACCTNO
                                 QLH1-WBEG-ACCTNO.
           MOVE ALL "9"       TO QLH1-WEND-ACCTNO.

           PERFORM START-LH1-FILE.
       UPD-PRLOANS-NEXT.
           PERFORM READ-LH1-FILE-NEXT.
           IF IO-FG = 0
              IF LH-SSNO = LN-SSNO(SUB) AND (LH-BRNO = LH-PATH-OWNBR)
      * DONT COUNT VOIDS !!
                 IF LH-POCD NOT = "**"
                    ADD 1 TO BW-PRLOANS
                 END-IF
                 GO TO UPD-PRLOANS-NEXT.

       REWRITE-BW1.
           PERFORM REWRITE-BW1-FILE.
           GO TO UPD-LIAB.

       GLOBAL-UPDATE-EXIT.
           EXIT.

      **********************************************
       BORR-DETAIL-LINE-PROCESS SECTION.
      **********************************************
           MOVE BR-NO TO D-BRNO1.
           MOVE "MISSING BORROWER = " TO D-MESS.
           MOVE LN-SSNO(SUB) TO D-SSNO.
           INSPECT D-SSNO REPLACING ALL "/" BY "-".
           MOVE "LOAN #" TO D-LOAN.
           MOVE LN-ACCTNO TO D-ACCTNO.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      **********************************************
       CLASS-DETAIL-LINE-PROCESS SECTION.
      **********************************************
           MOVE BR-NO TO D-BRNO2.
           MOVE "MISSING LOAN CLASS = " TO D-MESS2.
           MOVE LN-CLASS TO D-CLASS.
           MOVE "LOAN #" TO D-LOAN2.
           MOVE LN-ACCTNO TO D-ACCTNO2.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      **********************************************
       WRITE-DETAIL-LINE SECTION.
      **********************************************
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      **********************************************
       INITIALIZATION SECTION.
      **********************************************
           MOVE 0 TO Q-POS.
           MOVE 0 TO C-POS.
           MOVE 0 TO P-POS.
           MOVE "V" TO Q-BUF.
           MOVE "1" TO C-BUF.
           MOVE "00" TO P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-SP1-FILE.

           ACCEPT TODAYS-DATE FROM CENTURY-DATE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ********************************
       EO-EXEC-AUDIT SECTION.
      ********************************

      ********************************
       ENTER-PARAMETERS SECTION.
      ********************************
       ENTER-PARM.

           IF UP-KEY GO TO FIXLN-FIELD.

       ENTER-BRANCH.
           MOVE "ENNN040BRNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO EXIT-PARM.
           IF VDUSTATUS = "16"
              MOVE 0 TO BRANCH
              MOVE "S  A040BRNO." TO VDU
              MOVE "*ALL" TO VDUBUF
              PERFORM SCREENIO
              GO TO PRLN-FIELD.
           IF VDUSTATUS = "1>"
              GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BRANCH.

           MOVE VDUNUM0 TO BRANCH.

           IF BRANCH = 0
              GO TO ENTER-BRANCH.

       PRLN-FIELD.
           MOVE "E NA010PRLN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-BRANCH.
           IF VDUSTATUS NOT = "00" GO TO PRLN-FIELD.
           MOVE VDUBUF TO PRLN-BUF.
           IF PRLN-BUF NOT = "Y"
              IF PRLN-BUF NOT = "N"
                 GO TO PRLN-FIELD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    Y -  UPDATE LOAN FIELDS
      *    N -  DON'T CHANGE LOAN RECORD
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       FIXLN-FIELD.
           MOVE "E NA010FIXLN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PRLN-FIELD.
           IF VDUSTATUS NOT = "00" GO TO FIXLN-FIELD.
           MOVE VDUBUF TO FIXLN-BUF.
           IF NOT (FIXLN-BUF = "Y" OR "N")
                 MOVE "ENTER: 'Y' OR 'N' ONLY!" TO MESS
                 PERFORM SEND-MESS
                 GO TO FIXLN-FIELD.

       EXIT-PARM.
           EXIT.

      ********************************
       DISPLAY-PARAMETERS SECTION.
      ********************************
           MOVE "SNNN040BRNO."   TO VDU.
           MOVE BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010PRLN."   TO VDU.
           MOVE PRLN-BUF         TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010FIXLN."  TO VDU.
           MOVE FIXLN-BUF        TO VDUBUF.
           PERFORM SCREENIO.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO FXLIAB-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FXLIAB-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FXLIAB-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE FXLIAB-FORM-FIELDS TO WR-BUF.
           MOVE FXLIAB-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE FXLIAB-HELP--FILE TO HELP-LINK-FILE.
           MOVE FXLIAB-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/FXLIAB_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************************************
       MISC-PARA SECTION.
      *******************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE MESS-BUF  TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GET-SP.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM READ-SP1-FILE
           IF IO-FG NOT = 0
              MOVE " " TO SP-REC.

      *******************************************************
       IO-ROUTINE SECTION.
      *******************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLHGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPLH1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *******************************************************
       END-PROGRAM SECTION.
      *******************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "FX/FXMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN  
              DESTROY FXLIAB-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
