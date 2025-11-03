       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BISCAN.
       DATE-WRITTEN.    012494.
      **************************************************************************
      *   DIRECTORY  : CL
      *
      *   DESCRIPTION: CALLED PROGRAM TO SCAN FOR LOANS ON ALL BORROWERS
      *                BIFILE MASTER INDEX RECORDS
      *
      *  !!!! HEY   !!   THIS PROGRAM HAS BEEN ALTERED SO IT CAN BE CALLED
      *  !!!!!!!!!!!!!   FROM WORLD LIVE CHECK PROGRAM (LNCRE2) TO CREATE LOANS.
      *  !!!!!!!!!!!!!   I HAD TO CHANGE
      *  !!!!!!!!!!!!!   ALL OCCURANCES OF PERFORM SEND-MESS TO
      *  !!!!!!!!!!!!!   PERFORM CONDITIONAL-SEND-MESS SO ERRORS
      *  !!!!!!!!!!!!!   MIGHT NOT GO TO THE SCREEN, SO PLEASE DON'T
      *  !!!!!!!!!!!!!   PERFORM SEND-MESS, PERFORM CONDITIONAL-SEND-MESS
      *  !!!!!!!!!!!!!      /MJD 160921
      *
      *      BISCAN-CALL-CODE =
      *                       2      SP-DUAL-STATE NOT = "Y" OR
      *                              SP-DUAL-BRANCH NOT = "Y"
      *                              VERIFY NO RENEWALS HALF DONE
      *                       THEY WOULD "PB" A LOAN, PUT ON A "NB",
      *                       REVERSE THE "PB", CUSTOMER HAS 2 LOANS !!
      *                       3  (WI/BWCREA) JUST A WARNING THAT THE SSNO
      *                          ALREADY EXISTS SOMEWHERE
      *                       4
      *
      *                       5      ANOTHER LOAN BOOKED TODAY !
      *
      * A FEW NOTES, DOCUMENTING SOME STUFF AS OF 3/22/11:
      *
      * BISCAN-CALL-CODE OF 3 OR 4 ARE ONLY SET BY WI/BWCREA.C
      *   & BWCREA IS ONLY CALLED BY LOAN CREATES (XONPC0, LONPB0, ETC), BUT
      *   XONPC0, LNQUOT & INVENT (GIFTNET ORDER ENTRY) ARE THE ONLY ONES
      *   THAT CALL BWCREA AND SET THE FIELDS THAT GET SENT TO BISCAN AND
      *   THESE FIELDS ARE ONLY SET IF CANNED LOAN WAS ENTERED:
      *
      *   SP-DUAL-IN-BRANCH --> BWCREA-DUAL-IN-BRANCH -->  BISCAN-BRANCH-FG
      *   SP-DUAL-IN-STATE  --> BWCREA-DUAL-IN-STATE  -->  BISCAN-STATE-FG
      *   SP-ORGST          --> BWCREA-STATE          -->  BISCAN-STATE
      *
      *   SO BASICALLY IF YOU'RE RUNNING XONPC0 AND YOU SELECT A CAN & YOU
      *   ENTER AN SSNO & HIT F5 FOR BWCREA, THEN BISCAN IS CALLED WITH THE
      *   FIELDS SET  & A FLAG OF 4; IF THIS PROGRAM FINDS ANOTHER LOAN FOR
      *   THE SSNO VIOLATION OF THE FLAGS, IT SETS THE FLAG TO 9 & BWCREA
      *   ASKS FOR ANOTHER SSNO.
      *
      *   IF THE DUAL FLAGS ARE BOTH "N", BWCREA SETS THE CALL-FLAG TO 3
      *   WHICH JUST GIVES A WARNING THAT THE SSNO EXISTS.  NOT SURE IF
      *   I UNDERSTAND THAT.....
      *
      *   IF YOU'RE RUNNING OTHER LOAN CREATES (LONPB0, VONPC0, ZONPC0)
      *   & HIT F5 AT THE SSNO, NOTHING IS PUT IN THE BWCREA DUAL FLAGS
      *   OR STATE, SO AT MOST YOU WILL JUST GET A WARNING AT THIS POINT.
      *
      *                                                     BARB
      *
      * REV:
      *  JTG 081694 ADDED LOGIC FOR BI-ENTDATE
      *  JTG 020698 ADDED LOGIC FOR BI-SALEFIN
      *  JTG 030998 DUAL LOAN CHECK: IF ACCT IS CONVERTED & IN PL
      *             STATUS, DON'T SET DUAL LOAN FLAG STATUS
      *  JTG 040898 CHANGED TEST TO BI-CONVERCD NOT = " ", RE: 030998 CHANGE
      *  JTG 061198 REMOVE DOUBLE NOT LOGIC:
      *             (NOT (BI-CONVERCD NOT = " "     |    AND (BI-CONVERCD = " "
      *                     AND BI-PLCD NOT = " ")  |         OR BI-PLCD = " "
      *             )                               |        )
      *  BAH 061698 CHANGED "LOANS ON THE WORLD NETWORK" TO SAY "SYSTEM"
      *  BAH 061798 SEARCHING FOR HALF RENEWALS WAS ONLY TESTING FOR
      *             "PB", ADDED "RN", "SC", "RB" AND "RO"
      *  MG  990927  FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BAH 000731 ADDED ANOTHER "IF" UNDER THE DUAL CODE CHECK, TO ALLOW
      *             RESCISSION RENEWAL ACCOUNTS THRU, OTHERWISE, THE PRIOR
      *             LOAN WAS STOPPING YOU AS STILL OPEN BECAUSE IT DOES NOT
      *             GET PHYSICALLY PAID OUT UNTIL NEW LOAN TAKEN OUT OF
      *             RESCISSION, PR# 1231
      *  JTG 040527 CHANGED TO ALLOW DUAL LOANS IN BRANCH  B U T  NOT
      *             DUAL LOANS IN STATE                          LENDMARK #2259
      *  BAH 110322 WORLD "IL" LOANS PRIOR TO 3/21/2011 CANT TEST TOWARD
      *             DUAL LOAN FLAGS ! WORLD PR# 749
      *  BAH 130124 A30 CONVERSION
      *  BAH 160422 ADDED NEW VALUE "M" TO SP-DUAL-IN-STATE AND 
      *             SP-DUAL-IN-BRANCH, FUNCTIONS EXACTLY LIKE "N" EXCEPT
      *             REAL ESTATE LOANS ARE NOT CONSIDERED AS OPEN LOANS,
      *             AND NEITHER ARE P&L'S, REGENCY PR#5030
      *  MJD 160921 CHANGED SEND-MESS TO CONDITIONAL-SEND-MESS, ADDED EXTERNAL
      *             WORKERS EXT-CALLED-FROM AND EXT-RETURN-ERR-BUF TO INDICATE
      *             THAT THIS OPROGRAM WAS CALLED FROM LNCRE2.  ADDED READ OF
      *             BRANCH AND SETUP OF LOCAL PD-PATH IF CALLED FROM LIVE CHECK
      *             UPDATE.                                           61000#1191
      *  BAH 170323 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 171204 SKIP BI-GIFTNET "X" OR "A", TAX ADVANCE LOANS, FOR ANY DUAL
      *             CHECKS; THE "X" IS SET IN LONPC2 IF SP-NON-LOAN-FLAG = "Y"
      *                                                         WORLD PR#1288
      *  BAH 180328 DO NOT CONSIDER CHARGE OFFS OLDER THAN 5 YEARS FROM
      *             CHARGE OFF DATE AS OPEN LOANS FOR DUAL LOAN PURPOSES, #1308 
      *  BAH 20220127 REMOVED GP-I-AM-WORLD
      *  BAH 20220830 HARD CODED START
      *  BAH 20240122 ADDED BILNSC-CALLED-FROM TO SPEED UP FINDING 1 SSNO
      **************************************************************************
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)CL/BISCAN J35 01/22/24-09:53:40 >".

       COPY "LIBLP/LP01BI.CPY".
       COPY "LIBLP/LP01BI_SQL.CPY".
       COPY "LIBLP/LPWSBI.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       01  ERRCD               PIC X         VALUE SPACES.
       01  EDIT-LN-ACCTNO      PIC 999999/99.
       01  SUB                 PIC 999  COMP VALUE 0.

       01  TEST-5YEAR-DATE     PIC 9(8) COMP-3.
       01  HOLD-SSNO           PIC 9(9) COMP.
       01  ST-MATCH            PIC X.
       01  BR-MATCH            PIC X.
       01  CALL-CODE-4-BILNSC  PIC X         VALUE "N".
      * EXTERNAL WORKERS USED TO INDICATE WHETHER OR NOT THIS PROGRAM WAS 
      * CALLED FROM LNCRE2 (LIVE CHECK UPDATE).  IF IT IS THEN WE DO NOT
      * WANT TO USE SEND-MESS OR ALARM IF THERE ARE ISSUES.
       01  EXT-CALLED-FROM      PIC X(6) EXTERNAL.
       01  EXT-RETURN-ERR-BUF   PIC X(55) EXTERNAL.


       COPY "LIBWI/BILNSCW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      **************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/BISCANW.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME BISCAN-WORKERS EXIT-PATHNAME.  

      **************************************************
       MAIN-PROGRAM SECTION.
      **************************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

      * NEED READ OF BR HERE.  IF CALLED FROM LIVE CHECK UPDATE (LNCRE2)
      * IT NEEDS TO CHECK RELATIVE TO THE BRANCH BEING PROCESSED.
           IF EXT-CALLED-FROM = "LNCRE2"
              PERFORM SETUP-LOCAL-PATHS.

      * FIRST CHECK THAT THERE ARE NO UNFINISHED RENEWALS
           IF BISCAN-CALL-CODE = 2
              PERFORM SEARCH-FOR-RENEWALS
              IF BISCAN-CALL-CODE = 9
                 MOVE PD-SOC-SEC TO EDIT-LN-ACCTNO
                 INSPECT EDIT-LN-ACCTNO REPLACING ALL "/" BY "-"
                 STRING "UNFINISHED RENEWAL FOR ACCT # ",EDIT-LN-ACCTNO
                  DELIMITED BY "  " INTO MESS
                 PERFORM CONDITIONAL-SEND-MESS
                 GO TO END-ROUTINE.

      * GET THIS DATE ONCE TO TEST FOR 5 YEAR OLD CHARGE OFFS
           IF BISCAN-CALL-CODE = 2
              MOVE BISCAN-TRANS-DATE TO NDTE-DATE
              MOVE -5                TO NDTE-HOLD
              PERFORM INCREMENT-YEARS
              MOVE NDTE-DATE TO TEST-5YEAR-DATE.

           MOVE BISCAN-SSNO TO HOLD-SSNO.

           PERFORM CLEAR-BI-FILE.

           PERFORM OPEN-BI1-FILE.

           MOVE HOLD-SSNO TO BISCAN-SSNO
                             BI-SSNO
                             QBI1-WBEG-SSNO
                             QBI1-WEND-SSNO.
           MOVE 0         TO BI-ACCTNO BI-OWNBR
                             QBI1-WBEG-ACCTNO
                             QBI1-WBEG-OWNBR.
           MOVE ALL "9"   TO QBI1-WEND-ACCTNO
                             QBI1-WEND-OWNBR.

           PERFORM START-BI1-FILE.
       NEXT-REC.
           PERFORM READ-BI1-FILE-NEXT.
           IF (IO-BAD) OR (BI-SSNO NOT = BISCAN-SSNO)
              PERFORM CLOSE-BI1-FILE
              GO TO END-ROUTINE.

      * WORLD PR#1288 SKIP TAX ADVANCE LOANS FOR ANY DUAL CHECKS
      *               THE "X" IS SET IN LONPC2 IF SP-NON-LOAN-FLAG = "Y"
           IF BI-GIFTNET = "X" OR "A"
              GO TO NEXT-REC.

           MOVE "N" TO BR-MATCH ST-MATCH.
           IF BISCAN-BRANCH = BI-OWNBR
              MOVE "Y" TO BR-MATCH.
           IF BISCAN-STATE = BI-ORGST
              MOVE "Y" TO ST-MATCH.

           IF BISCAN-CALL-CODE = 1
              IF (BI-POFFDATE = 0) AND (BISCAN-BRANCH NOT = BI-OWNBR)
                 PERFORM LOANS-IN-NETWORK
                 PERFORM CLOSE-BI1-FILE
                 GO TO END-ROUTINE
              ELSE
                 GO NEXT-REC.

      * FOUND THIS THE CLEANEST WAY TO SKIP WORLD "IL" LOANS PRIOR TO 3/21/11
      * BECAUSE THESE LOANS DID NOT FALL UNDER THIS NEW LAW, TOO HARD TO
      * CHECK PREVIOUS LOANS SPR ! IF THEY ARE UNDER THE NEW LAW THEN THEY
      * WILL FALL INTO THE NEXT 'IF' AND GET STOPPED !
           IF BISCAN-CALL-CODE = 2 AND BI-ORGST = "IL"
              IF ( (BI-POFFDATE = 0)
                         AND (BI-GIFTNET NOT = "Y")
                          AND (BI-SALEFIN NOT = "Y")
                           AND (BI-CONVERCD = " " OR BI-PLCD = " ") )
                 IF BI-ENTDATE < 20110321
                    GO NEXT-REC.

      * DO NOT CONSIDER CHARGE OFFS OLDER THAN 5 YEARS FROM CHARGE OFF DATE
      * AS OPEN LOANS FOR DUAL LOAN PURPOSES, #1308 
           IF (BISCAN-CALL-CODE = 2) AND (BI-PLCD NOT = SPACES)
              IF (BI-PLDATE NOT = 0)
                 IF BI-PLDATE < TEST-5YEAR-DATE
                    GO NEXT-REC.

      *------------------------------------------------------------------
      * DUAL LOAN CHECK (SP-DUAL-IN-BRANCH = M AND SP-DUAL-IN-STATE = M)
      *  REGENCY "M" SAME AS "N" EXCEPT MORTGAGE LOANS ARE NOT AN OPEN LOAN
      *    MORTGAGE LOANS ARE SMALLLOAN "N" AND SALEFIN "N"
      *      NEITHER ARE P&L
      *
      *    SKIP TESTING WHEN ACCOUNTS IS: (THESE ARE NOT CONSIDERED OPEN)
      *          PAID OFF
      *            OR   GIFTNET
      *             OR   SALES FINANCE
      *              OR   P&L
      *               OR   NOT MORTGAGE LOAN (SMALLOAN "N" AND SALEFIN "N")
      *------------------------------------------------------------------

      *------------------------------------------------------------------
      * DUAL LOAN CHECK (SP-DUAL-IN-BRANCH = N AND SP-DUAL-IN-STATE = N)
      *
      *    SKIP TESTING WHEN ACCOUNTS IS:
      *          PAID OFF
      *            OR   GIFTNET
      *             OR   SALES FINANCE
      *              OR   CONVERTED AND P&L
      *------------------------------------------------------------------
           IF BISCAN-CALL-CODE = 2
      * NEEDED THIS TEST SO A RESCISSION RENEWAL ACCOUNT CAN PASS THE DUAL
      * TEST AND WILL BE TESTED AGAIN WHEN UPDATED OUT OF RESCISSION
      * THE PRIOR LOAN IS NOT PHYSICALLY PAID OUT UNTIL NEW LOAN TAKEN OUT
      * OF RESCISSION, PR# 1231
              IF BI-ACCTNO = BISCAN-PRLNNO AND
                (BISCAN-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
                 GO NEXT-REC
              ELSE
              IF ( (BI-POFFDATE = 0)
                         AND (BI-GIFTNET NOT = "Y")
                          AND (BI-SALEFIN NOT = "Y")
                           AND (BI-CONVERCD = " " OR BI-PLCD = " ")
                  )
                    AND ( ( BISCAN-BRANCH-FG = "N" AND BR-MATCH = "Y" )
                                    OR
                          ( BISCAN-STATE-FG = "N" AND
                            ( ST-MATCH = "Y" AND BR-MATCH = "N" ) )
                        )
                 PERFORM LOANS-IN-NETWORK
                 MOVE 9 TO BISCAN-CALL-CODE
                 PERFORM CLOSE-BI1-FILE
                 GO TO END-ROUTINE
              ELSE
      * "M" FUNCTIONS EXACTLY LIKE "N" EXCEPT REAL ESTATE LOANS ARE
      *     NOT CONSIDERED AS OPEN LOANS, AND NEITHER ARE P&L'S
      *     REAL ESTATE IS SMALLOAN "N" AND SALESFIN "N" (REGENCY)
      * THIS 'IF' STATEMENT IS LOOKING FOR WHAT IS AN OPEN LOAN:
      *     GIFTNET ACCOUNTS ARE NOT OPEN LOANS, SO MUST BE "N"
      *     SALESFIN ACCOUNTS ARE NOT OPEN LOANS, SO MUST BE "N" 
      *     SMALLOAN ACCOUNTS ARE OPEN LOANS, SO MUST BE "Y"
      *           THIS ALSO ALLOWS THE REAL ESTATE LOANS TO NOT BE
      *           OPEN BECAUSE THIS SMALLOAN FLAG WILL BE "N"
              IF ( (BI-POFFDATE = 0)
                      AND (BI-GIFTNET NOT = "Y")
                       AND (BI-SALEFIN = "N")
                        AND (BI-SMALLOAN = "Y")
                         AND (BI-PLCD = " ")
                  )
                  AND
                    ( ( BISCAN-BRANCH-FG = "M" AND BR-MATCH = "Y" )
                                OR
                       ( BISCAN-STATE-FG = "M" AND
                         ( ST-MATCH = "Y" AND BR-MATCH = "N" ) )
                    )
                 PERFORM LOANS-IN-NETWORK
                 MOVE 9 TO BISCAN-CALL-CODE
                 PERFORM CLOSE-BI1-FILE
                 GO TO END-ROUTINE
              ELSE
                 GO NEXT-REC.

           IF BISCAN-CALL-CODE = 3
              PERFORM LOANS-IN-NETWORK
              MOVE 9 TO BISCAN-CALL-CODE
              PERFORM CLOSE-BI1-FILE
              GO TO END-ROUTINE.

           IF BISCAN-CALL-CODE = 4
              IF ( (BI-POFFDATE = 0)
                        AND (BI-GIFTNET NOT = "Y")
                         AND (BI-SALEFIN NOT = "Y")
                          AND (BI-CONVERCD = " "
                                       OR BI-PLCD = " "
                              )
                 )
                 IF ( ( BISCAN-BRANCH-FG = "N" AND
                        BR-MATCH = "Y" )
                     OR
                      ( BISCAN-STATE-FG = "N" AND
                        ( ST-MATCH = "Y" AND
                          BR-MATCH = "N" )
                      )
                    )
                    PERFORM LOANS-IN-NETWORK
                    MOVE 9 TO BISCAN-CALL-CODE
                    PERFORM CLOSE-BI1-FILE
                    GO TO END-ROUTINE
                 ELSE
                    IF CALL-CODE-4-BILNSC = "N"
                       MOVE "Y" TO CALL-CODE-4-BILNSC
                       PERFORM LOANS-IN-NETWORK
                       GO TO NEXT-REC
                    ELSE
                       GO TO NEXT-REC
              ELSE
                 GO TO NEXT-REC.

      * TEST FOR LOAN MADE TODAY:
           IF BISCAN-CALL-CODE = 5
              IF (BI-POFFDATE = 0)
                 PERFORM TEST-ENTDATE
                 IF IO-GOOD
                    GO NEXT-REC
                 ELSE
                    PERFORM CLOSE-BI1-FILE
                    GO TO END-ROUTINE
              ELSE
                 GO TO NEXT-REC.

           GO TO END-ROUTINE.

      **************************************
       SEARCH-FOR-RENEWALS SECTION.
      **************************************
           PERFORM OPEN-PD1-FILE.

           MOVE "R"           TO PD-ID
                                 QPD1-WBEG-ID
                                 QPD1-WEND-ID.
           MOVE BISCAN-BRANCH TO PD-BRANCH
                                 QPD1-WBEG-BRANCH
                                 QPD1-WEND-BRANCH.
           MOVE 0             TO PD-SOC-SEC
                                 QPD1-WBEG-SOC-SEC.
           MOVE ALL "9"       TO QPD1-WEND-SOC-SEC.

           PERFORM START-PD1-FILE.
       SEARCH-NEXT-RENEWAL.
           PERFORM READ-PD1-FILE-NEXT.
           IF IO-BAD
              GO TO END-SEARCH-FOR-RENEWAL.

           IF (PD-BRANCH NOT = BISCAN-BRANCH) OR (PD-ID NOT = "R")
              GO TO END-SEARCH-FOR-RENEWAL.

           MOVE 0 TO SUB.
       SEARCH-NEXT.
           IF SUB = 4
              GO TO SEARCH-NEXT-RENEWAL.

           ADD 1 TO SUB.
           IF PD-SSNO(SUB) = BISCAN-SSNO
      * IF YOU FOUND A RENEWAL RECORD, IT COULD BE A RENEWAL IN PROCESS
      * YOU MIGHT BE MAKING THE "PB" LOAN RIGHT NOW....
              IF BISCAN-PRLNNO = PD-SOC-SEC AND
                (BISCAN-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
                 GO TO SEARCH-NEXT-RENEWAL
              ELSE
                 MOVE 9 TO BISCAN-CALL-CODE
                 GO TO END-SEARCH-FOR-RENEWAL
           ELSE
               GO TO SEARCH-NEXT.

           GO TO SEARCH-NEXT-RENEWAL.

       END-SEARCH-FOR-RENEWAL.
           PERFORM CLOSE-PD1-FILE.

      **************************************************
       BILNSC-SCAN SECTION.
      **************************************************
           MOVE BISCAN-TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
      * ALP-DATE STAYS 99/99/99, AND SO IS BILNSC-SYSDATE
           MOVE ALP-DATE      TO BILNSC-SYSDATE.
           MOVE BISCAN-SSNO   TO BILNSC-SSNO.
           MOVE BISCAN-BRANCH TO BILNSC-BRANCH.
           MOVE "BISCAN"      TO BILNSC-CALLED-FROM.
           MOVE "WI/BILNSC" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BILNSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       EXIT-BILNSC-SCAN.
           EXIT.

      **************************************************
       TEST-ENTDATE SECTION.
      **************************************************
           MOVE 0 TO IO-FG.

           IF BISCAN-TRANS-DATE = BI-ENTDATE
      * IF CALLED FROM LNCRE2 THEN THIS IS BEING CALLED FROM A BATCH PROCESS
      * THEREFORE WE CANNOT CALL ALARM OR OPEN A SCAN WINDOW. 
              IF EXT-CALLED-FROM NOT = "LNCRE2"
                 PERFORM ALARM
                 MOVE "- T O D A Y -" TO ERRMSG-HEAD
                 MOVE
           "       A LOAN WAS ALREADY MADE TO THIS BORROWER!!!"
                                                        TO ERRMSG-MSG1
                 MOVE SPACES TO ERRMSG-MSG2 ERRMSG-MSG3
                 PERFORM CALL-ERRMSG
                 PERFORM BILNSC-SCAN
              ELSE
                 IF EXT-RETURN-ERR-BUF = SPACES
                    MOVE
              "-TODAY- A LOAN WAS ALREADY MADE TO THIS BORROWER!!!"
                       TO EXT-RETURN-ERR-BUF
                 END-IF
              END-IF
              MOVE 9 TO IO-FG.

      **************************************************
       CONDITIONAL-SEND-MESS SECTION.
      **************************************************
           IF EXT-CALLED-FROM NOT = "LNCRE2"
              PERFORM SEND-MESS
           ELSE
              IF EXT-RETURN-ERR-BUF = SPACES
                 MOVE MESS TO EXT-RETURN-ERR-BUF
               END-IF
           END-IF.
                       
      **************************************************
       SETUP-LOCAL-PATHS SECTION.
      **************************************************

      * READ BRANCH & SET UP LOCAL PATHS IF CALLING FROM LNCRE2,
      * BATCH LOAN CREATE

           MOVE BISCAN-BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD   
              GO TO IO-ERROR.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.
           MOVE "B"        TO PD-PATH-OVERRIDE.

      **************************************************
       MISC SECTION.
      **************************************************
       FORM-RESET.
       SETUP-LINK-INFO.
       VDU-CONVERT-FIELD.
       LOANS-IN-NETWORK.
           MOVE "THIS BORROWER HAS LOANS ON THE SYSTEM?"
                                                        TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.  
      * IF CALLED FROM LNCRE2 THEN THIS IS BEING CALLED FROM A BATCH PROCESS
      * THEREFORE WE CANNOT CALL ALARM OR OPEN A SCAN WINDOW.      
           IF EXT-CALLED-FROM NOT = "LNCRE2"
              PERFORM ALARM
              PERFORM BILNSC-SCAN.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPBICL.CPY".

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-BI1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPBIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPBI1RN.CPY".
       COPY "LIBLP/LPPD1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
