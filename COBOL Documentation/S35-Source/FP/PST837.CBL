       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PST837.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-12-2020.
      ******************************************************************
      *
      * IDENTIFICATION:  FP/PST837.C
      *
      * DESCRIPTION   :  UNSECURED DISCLOSURE
      *
      * STATE         :  NEW MEXICO    (NM)
      *
      *-----------------------------------------------------------------
      * REV:
      * AMM 2020.0512 PR#1433 ***NEW*** CALLED FROM LONP9J
      * BAH 2020.0713 NET BALANCE WAS ADDING LN-PRINSRB INSTEAD OF SUBTRACTING
      *                                                         #1448
      * BAH 2024.0105 ADDED F3-ALL DOCUMENT PRINT TO FILE WITH MANIFEST #1638
      * BAH 2024.0520 MISSING CHECK IF NOT COLLCD "UL", TO EXIT S35Q-8
      * BAH 2024.0521 CHANGED D01-ACCTNO BACK TO Z(5)9 AND D06-NUMBER
      *               BACK TO Z(6)  S35Q-32
      * BAH 2024.0521 MDTE-DATE WAS NOT PRINTING S35Q-36
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "LIBFP/FPFSPST.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBFP/FPFDPST.CPY".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FP/PST837.CBL S35 05/22/24-14:41:45 >".

       COPY "LIBFP/FPWSPST.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LP01CX_SQL.CPY".
       COPY "LIBLP/LPWSCX.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTD.CPY".
       COPY "LIBLP/LP01LTD_SQL.CPY".
       COPY "LIBLP/LPWSLTD.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01PC.CPY".
       COPY "LIBLP/LP01PC_SQL.CPY".
       COPY "LIBLP/LPWSPC.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01WK.CPY".

       01  93-FEE          PIC S9(5)V99 COMP-3.
       01  94-FEE          PIC S9(5)V99 COMP-3.
       01  COMPANY-NAME    PIC X(40).
       01  WS-CALL         PIC 999/999/9999.
       01  JUSTIFY-TYPE    PIC X VALUE "L".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                      PIC X(300)     VALUE SPACES.

      *=================================================================
      *
      *     D E T A I L - W O R L D - A C C E P T A N C E  ( W A C )
      *
      *-----------------------------------------------------------------
      *    CPI     : 12 CPI   [PIC X(96)]
      *    CUSTOMER: WORLD ACCEPTANCE
      *    FMNO    : 36
      *    STATE   : NEW MEXICO   (NM)
      *=================================================================
       01  DETAIL-LINE-01                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(13).
           03  D01-MAKER                    PIC X(27).
           03  FILLER                       PIC X(05).
           03  D01-ACCTNO                   PIC Z(5)9.
           03  FILLER                       PIC X(12).
           03  D01-LOANDATE                 PIC 99/99/99.
           03  FILLER                       PIC X(12).
           03  D01-BR-NAME                  PIC X(40).
           03  FILLER                       REDEFINES D01-BR-NAME.
               05  D01-BR-PHONE             PIC 999/999/9999.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-02                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(19).
           03  D02-INTERES                  PIC ZZ9.99       BLANK ZERO.
           03  FILLER                       PIC X(02).
           03  D02-APR                      PIC ZZ9.99.
           03  FILLER                       PIC X(19).
           03  D02-FINCHG                   PIC ZZZ,ZZ9.99.
           03  FILLER                       PIC X(16).
           03  D02-FINAMT                   PIC ZZZ,ZZ9.99.
           03  FILLER                       PIC X(18).
           03  D02-TOTAL                    PIC ZZZ,ZZ9.99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-03                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(15).
           03  D03-ORGTERM                  PIC ZZZ.
           03  FILLER                       PIC X(20).
           03  D03-REGPYAMT                 PIC $$$$$$.99.
           03  FILLER                       PIC X(27).
           03  D03-1STPYDATE                PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-04                 REDEFINES DETAIL-LINE.
           03  FILLER                     PIC X(77).
           03  D04-MDTE-DATE              PIC 99/99/99.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-05                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(24).
           03  D05-AMOUNT                   PIC ZZZ,ZZ9.99.
           03  FILLER                       REDEFINES D05-AMOUNT.
               05  FILLER                   PIC X(01).
               05  D05-SUM                  PIC ZZZZZ9.99.
           03  FILLER                       PIC X(07).
           03  D05-CHECK                    PIC X(70).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-06                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(21).
           03  D06-BORROWER                 PIC X(20).
           03  FILLER                       PIC X(16).
           03  D06-NUMBER                   PIC Z(6).
           03  FILLER                       PIC X(24).
           03  D06-LOANDATE                 PIC 99/99/99.
           03  FILLER                       PIC X(13).
           03  D06-TIME                     PIC 99/99/99     BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-07                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(80).
           03  D07-CLOSED-ADR               PIC X(40).
           03  FILLER                       REDEFINES D07-CLOSED-ADR.
               05  D07-CONTIDAD             PIC ZZZ,ZZ9.99 BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-08                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(05).
           03  D08-CONTACT                  PIC X(90).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-09                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(21).
           03  D09-NOMBRE                   PIC X(40).
           03  FILLER                       PIC X(06).
           03  D09-DOMICILIO                PIC X(60).

      *-----------------------------------------------------------------
       01  DETAIL-LINE-10                   REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(25).
           03  D10-ABONOS                   PIC ZZZ,ZZ9.99.
           03  FILLER                       PIC X(29).
           03  D10-ULTIMO                   PIC ZZZ,ZZ9.99.

       COPY "LIBFP/PST_COPYW.CPY".
       COPY "LIBFP/PST_INFOW.CPY".
       COPY "LIBFP/PST_FORMW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBFP/PST_SCREENW.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME PROG-BUF IP-BUF PRU-PATH.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PST-FILE MANIFEST-FILE.

       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-PC1-FILE.
           PERFORM CLOSE-LTD1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-FM2-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PST-FILE MANIFEST-FILE.

       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM PST-FORM-MAIN-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    P R I N T - R O U T I N E
      *
      ******************************************************************
       PRINT-ROUTINE SECTION.

      * ONLY INCLUDE IF COLLATERAL IS UNSECURED
           IF LN-COLLCD NOT = "UL"
              GO TO PRINT-ROUTINE-EXIT.

           MOVE 02     TO PST-WS-FORM-PAGE-MAX.
           PERFORM PST-FR-FORM-INITIALIZATION.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - -{ LOAD ACCOUNT INFO. }
           PERFORM PST-INFO-MODULE.

      *- - - - - - - - - - - - - - - - - - - - -{ SETUP PAGE COPY LOOP }
           MOVE ZEROES TO PAGE-COPY-ELE.
           MOVE 01     TO PAGE-COPY-MAX.

           PERFORM GET-FEES.

       PRINT-ROUTINE-PAGE-COPY.

           ADD 01 TO PAGE-COPY-ELE.

           IF ( PAGE-COPY-ELE > PAGE-COPY-MAX )
              GO TO PRINT-ROUTINE-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ PRINT PAGES  }
           PERFORM PRINT-PAGE-01-ROUTINE.

           GO TO PRINT-ROUTINE-PAGE-COPY.

       PRINT-ROUTINE-END.

           PERFORM PRINT-PST-FILE.

       PRINT-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-PAGE-01-ROUTINE SECTION.
      ******************************************************************

           IF BW-ORIGIN = "SPAN" OR IP-FMNO = 37
              MOVE 02 TO PST-WS-FORM-PAGE-NO
           ELSE
              MOVE 01 TO PST-WS-FORM-PAGE-NO.
           PERFORM PST-FR-FORM-START.

           IF ( PST-WS-STAT-BAD )
              GO TO PRINT-PAGE-01-ROUTINE-EXIT.

      * CHANGE FONT SIZE 
           MOVE 08                     TO PST-WS-FONT-SIZE.                     
           MOVE PST-WS-FONT-COURIER    TO PST-WS-FONT-TYPE.                     
           PERFORM PST-FR-FONT.                                                 
           MOVE 06.00                  TO PST-WS-LINE-SPACING-SIZE.             
           PERFORM PST-FR-LINE-SPACING.                                         
                                                                                
      
      * 37 BLANK
           IF IP-FMNO NOT = 37
              MOVE BR-PHONE                    TO D01-BR-PHONE
              INSPECT D01-BR-PHONE REPLACING ALL "/" BY "-".
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 37
              MOVE LN-ACCTNO                   TO D01-ACCTNO
              MOVE TRANS-DATE                  TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D01-LOANDATE
              MOVE BR-STMNT1                   TO D01-BR-NAME
              IF JOINT-NAME-FIRST-LAST NOT = SPACES
                STRING MAKER-NAME-FIRST-LAST," & ",
                   JOINT-NAME-FIRST-LAST
                   DELIMITED BY "  " INTO D01-MAKER
              ELSE
                MOVE MAKER-NAME-FIRST-LAST     TO D01-MAKER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 37
              COMPUTE D02-APR ROUNDED = LN-APRATE * 1
              MOVE LN-FINCHG                   TO D02-FINCHG
              MOVE DISCLOSED-FINAMT            TO D02-FINAMT
              MOVE TOTAL-REPAY                 TO D02-TOTAL.
           MOVE 03 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 37
              MOVE LN-1STPYDATE                   TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                    TO D03-1STPYDATE 
              IF LN-1STPYAMT NOT = LN-REGPYAMT
                 MOVE 1                           TO D03-ORGTERM
                 MOVE LN-1STPYAMT                 TO D03-REGPYAMT
              ELSE
                 MOVE LN-REGPYAMT                 TO D03-REGPYAMT
                 IF LN-LASTPYAMT NOT = LN-REGPYAMT
                    COMPUTE D03-ORGTERM = LN-ORGTERM - 1
                 ELSE
                    MOVE LN-ORGTERM               TO D03-ORGTERM.
           MOVE 06 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 37
              IF LN-1STPYAMT NOT = LN-REGPYAMT
                 COMPUTE D03-ORGTERM = LN-ORGTERM - 1
                 MOVE LN-REGPYAMT                 TO D03-REGPYAMT
              ELSE
                 IF LN-LASTPYAMT NOT = LN-REGPYAMT
                    MOVE 1                        TO D03-ORGTERM
                    MOVE LN-LASTPYAMT             TO D03-REGPYAMT.
           IF NOT ( IP-FMNO  = 118 OR 37 )
              MOVE MDTE-DATE                TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY              TO D04-MDTE-DATE.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * ORIGINATION CHARGE 
           IF IP-FMNO NOT = 37
              MOVE LN-SERCHG                   TO D05-AMOUNT.
           MOVE 07 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * INTEREST CHARGE
           IF IP-FMNO NOT = 37
              COMPUTE D05-AMOUNT = LN-INTCHG + LN-EXTCHG.
           MOVE GB-COMPANY-NAME TO COMPANY-NAME.
           CALL "C$JUSTIFY" USING COMPANY-NAME, JUSTIFY-TYPE.
           MOVE SPACES TO D05-CHECK.
           STRING BR-STMNT1, ", DBA ",COMPANY-NAME
              DELIMITED BY "  " INTO D05-CHECK.
           CALL "C$JUSTIFY" USING D05-CHECK, "C".
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO D05-CHECK.
           STRING BR-ADR1, ", ", BR-CITY, ", ", BR-STATE, " ", BR-ZIP
             DELIMITED BY "  " INTO D05-CHECK.
           CALL "C$JUSTIFY" USING D05-CHECK, "C".
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PAID TO YOU
           IF IP-FMNO NOT = 37
              MOVE LN-PROCEEDS                 TO D05-AMOUNT.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * AMOUNT TO UMC
           IF IP-FMNO NOT = 37
              COMPUTE D05-AMOUNT = 93-FEE + 94-FEE.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PAID ON PRIOR
           IF IP-FMNO NOT = 37
              COMPUTE D05-AMOUNT = LN-PRBAL + LN-PRINTDUE
                                 - LN-PRFINCHGRB - LN-PRINSRB.
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PRINCIPAL AMT
           IF IP-FMNO NOT = 37
              MOVE DISCLOSED-FINAMT            TO D05-SUM.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * TO CONTACT 
           IF IP-FMNO NOT = 37
              MOVE BR-PHONE TO WS-CALL
              INSPECT WS-CALL REPLACING ALL "/" BY "-"
              STRING "TO CONTACT"," ", COMPANY-NAME,
                 " ABOUT THIS ACCOUNT, CALL ", WS-CALL, "."
                 DELIMITED BY "  " INTO D08-CONTACT.
           MOVE 11 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 37
              MOVE MAKER-NAME-FIRST-LAST       TO D06-BORROWER
              MOVE LN-ACCTNO                   TO D06-NUMBER
              MOVE LN-LOANDATE                 TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO D06-LOANDATE
              PERFORM GET-POSTTIME
              MOVE LP-POSTTIME                 TO D06-TIME
              INSPECT D06-TIME REPLACING ALL "/" BY ":".
           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 37
              MOVE MAKER-ADDRESS               TO D07-CLOSED-ADR
              IF JOINT-NAME-FIRST-LAST NOT = SPACES
                 STRING "& ", JOINT-NAME-FIRST-LAST
                    DELIMITED BY "  " INTO D06-BORROWER.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 37
              MOVE MAKER-CSZ                   TO D07-CLOSED-ADR.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 37 AND BW-ORIGIN = "SPAN"
              MOVE MAKER-NAME-FIRST-LAST       TO D09-NOMBRE
              MOVE MAKER-ADDRESS-CSZ           TO D09-DOMICILIO.
           MOVE 08 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 37 AND BW-ORIGIN = "SPAN"
              COMPUTE D02-INTERES ROUNDED = LN-APRATE * 1
              MOVE LN-FINCHG                   TO D02-FINCHG
              MOVE DISCLOSED-FINAMT            TO D07-CONTIDAD
              MOVE TOTAL-REPAY                 TO D02-TOTAL.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF IP-FMNO NOT = 37 AND BW-ORIGIN = "SPAN"
              MOVE LN-REGPYAMT TO D10-ABONOS D10-ULTIMO.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PST-FR-FORM-FINISH.

       PRINT-PAGE-01-ROUTINE-EXIT.
           EXIT.

       COPY "LIBFP/FPFRPST.CPY".
       COPY "LIBFP/PST_COPY.CPY".
       COPY "LIBFP/PST_INFO.CPY".
       COPY "LIBFP/PST_FORM.CPY".

      *******************************
       GET-POSTTIME SECTION.
      *******************************
           MOVE LN-ACCTNO TO LP-ACCTNO.
           MOVE 1         TO LP-SEQNO.
           PERFORM OPEN-LP1-FILE.
           PERFORM READ-LP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           IF IO-FG = 9
              GO TO EXIT-GET-POSTTIME.

       EXIT-GET-POSTTIME.
           EXIT.

      *******************************
       GET-FEES SECTION.
      *******************************
           MOVE 0 TO 93-FEE 94-FEE.
           MOVE 1 TO SUB.
       NEXT-LN-FEE.
           IF SUB > 10
              GO TO EXIT-FIND-FEE.
           IF LN-FEECODE(SUB) = SPACES
              ADD 1 TO SUB
              GO TO NEXT-LN-FEE.

           IF LN-FEECODE(SUB) = "93"
              ADD LN-FEEAMT(SUB) TO 93-FEE
           ELSE
           IF LN-FEECODE(SUB) = "94"
              ADD LN-FEEAMT(SUB) TO 94-FEE.

           ADD 1 TO SUB.
           GO TO NEXT-LN-FEE.
       EXIT-FIND-FEE.
           EXIT.
 
      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPPCGS_SQL.CPY".
       COPY "LIBLP/LPLTDGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBFP/FPIOPST.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCK1RN.CPY".
       COPY "LIBLP/LPCX1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPFM2RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTD1RN.CPY".
       COPY "LIBLP/LPPC1RN.CPY".
       COPY "LIBLP/LPPD1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPTRC1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.
           PERFORM PRU-RESET.

       EXIT-PROG.

       STOP-RUN.
           EXIT PROGRAM.
