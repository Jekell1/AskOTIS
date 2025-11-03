      * COPYMEMBER: LIBLP/LP01BM
      ******************************************************************
      *
      *          ( BORROWER MEMO INDEX # 1 )
      *        ( KEY: BORROWER SSNO/DATE/SEQ  )
      *
      *=================================================================
      * THE FOLLOWING IS THE POSSIBLE LAYOUTS FOR BM1-KEY.
      * THESE LAYOUTS WERE IN A15 BUT WE NO LONGER USE THEM IN A30
      * WITH EXCEPTION OF JUDICAL & GARNISHMENT RECORDS WHICH WERE
      * FOR MODERN FINANCE & SOME OTHERS.
      * THE A15-TO-A30 PROGRAM (UP/UL1530) STILL ALLOWED ALL BM RECORDS
      * TO BE CONVERTED TO A30 JUST IN CASE.
      *
      * DATE      SEQ   DESC
      * ========  ====  ===========================================
      * 0         1     MEXICO LONGER FIRST NAME
      *                 JUDICAL DISTRICT                   [LONP2N]
      * 0         2     MEXICO LONGER MIDDLE NAME
      *                 JUDICAL CASE NUMBER                [LONP2N]
      * 0         3     MEXICO LONGER LAST NAME
      *                 JUDICAL ADDRESS                    [LONP2N]
      * 0         6     GARNISHEE NAME AND ADDRESS         [LONP2N]
      * 0         7     2ND LINE OF GARNISHEE NAME & ADR   [LONP2N]
      * 0         8     3RD LINE OF GARNISHEE NAME & ADR   [LONP2N]
      * 1         1     GARNISHEE NAME AND ADDRESS         [LONP2N]
      * 1         2?    2ND LINE OF GARNISHEE NAME & ADR   [LONP2N]
      * 1         3?    3RD LINE OF GARNISHEE NAME & ADR   [LONP2N]
      * 1         4?    4TH LINE OF GARNISHEE NAME & ADR   [LONP2N]
      *
      * 99999999  1     MEXICO OFFICAL ID (VOTER REG)
      * 99999999  2     MEXICO NATIONAL ID (CURP)
      * 99999999  3     MEXICO RFC
      * 99999999  4     MEXICO PURPOSE OF LOAN, P2, F10
      * 99999999  5     MEXICO SECOND LAST NAME, P2, F11
      * 99999999  6     MEXICO UNION ACCTNO
      * 99999999  1-10  BORROWER CONTACTS
      *
      *=================================================================
      * REV:
      * GLF 110498 CHANGED DATE FOR 2000 COMPATIBILITY
      * MJD 990125 ----- UPDATED IN FLEXGEN -----
      * KEC 990316 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 990510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 990805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 990811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 000410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * MJD 000428 ADDED REDEFINITION OF MEMO FOR PHONE INFO (TFC PR#37)
      * BAH 160216 EXPANDED PD0003
      * BAH 180822 MOVED BM-PHONE-DESC, BM-PHONE-NO AND BM-PHONE-EXT TO BW
      *
      ******************************************************************
       78  BM-SIZE                     VALUE 145.
       01  BM-REC.
           03  BM-BRNO                 PIC 9(4).
           03  BM1-KEY.
               05  BM-SSNO             PIC 9(9).
      * CCYYMMDD SUBTRACTED FROM 9'S
               05  BM-DATE             PIC 9(8).
               05  BM-SEQ              PIC 9(3).
      *>>XDD DATE=YYYYMMDD
           03  BM-LTOUCH-DATE          PIC 9(8) COMP-3.
           03  BM-LTOUCH-TIME-HHMM     PIC 9(4) COMP-3.
           03  BM-LTOUCH-USERID        PIC X(10).
           03  BM-MEMO                 PIC X(60).
      *    03  BM-MEMO-PHONE REDEFINES BM-MEMO.
      *        05  BM-PHONE-DESC       PIC X(46).
      *        05  BM-PHONE-NO         PIC 9(10).
      *        05  BM-PHONE-EXT        PIC 9(04).
           03  BM-MEMO-TYPE            PIC XX.
           03  FILLER                  PIC X(41).
