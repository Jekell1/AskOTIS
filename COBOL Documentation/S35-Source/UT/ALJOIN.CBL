       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ALJOIN.
       DATE-WRITTEN. 02-10-84
      ******************************************************************
      *    DIR :  LP
      *    DESC:  LOAN AGED TRIAL BALANCE -
      *****************************************************
      *****************************************************
      * USING JOIN WITH LN & BW COLUMNS
      *****************************************************
      *****************************************************
      **********   IF AN ACCOUNT IS BOTH A REPO AND A PL, WILL PRINT AS
      **********   P&L TOTALS, AND NOT AS REPO TOTALS
      *
      *=================================================================
      * REV:
      * 042995 SLC ADDED PLTYPE LOGIC.
      * 071295 MJD CHANGED TOTALS,AND DET LINE SO THAT:
      *             CURRENT BALANCE = LN-CURBAL - LN-OT2BAL,
      *             OTHER BALANCE   = LN-OTHBAL + LN-OT2BAL.
      * 073195 MJD ADDED INCLUDE/SKIP/ONLY OPTIONS FOR POTS
      *             AND MISS PAYS.
      * MJD 092995 REMOVED INTEREST FROM SUMMARY LINE AND INCREASED
      *             OTHER FIELDS BY 3 SIG DIGITS.
      * JFF 100695 ADDED CLASS SCAN AND DEALER SCAN
      * MJD 010896 CORRECTED TEST OF LN-PLTYPE. IF "A" FOR ALL WAS ENTERED
      *             THEN THE PROGRAM WAS INCORRECTLY TESTING LN-PLTYPE
      *             FOR AN "A" INSTEAD OF FOR ALL TYPES.
      * BAH 012296 EXPANDED T-NO, TT-NO, DD-NO AND NM-NO TO PIC S9(7)
      * BAH 012296 EXPANDED T-OTH, TT-OTH, DD-OTH AND NM-OTH TO PIC S9(9)V99
      * BAH 012296 EXPANDED T-UNE, TT-UNE, DD-UNE AND NM-UNE TO PIC S9(9)V99
      * BAH 012296 EXPANDED T-ERN, TT-ERN, DD-ERN AND NM-ERN TO PIC S9(9)V99
      * MJD 031396 ADDED OPTION TO PRINT 0 UNEARNED (I,S,O).
      * MJD 051096 ADDED LOCAL SPOOLING LOGIC.
      * JFF 053196 FIXED CLASS TOTALS TO PRINT ALL CLASSES BEFORE
      *            BRANCH SUMMARY.
      * JTG 070196 MADE 0000-9999 AGEING RANGE SKIP THE TEST RE: ACCOUNTS
      *            BEING 10000+ DAYS RECENCY
      * MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      * BAH 080296 ADDED BANKRULE OPTION (A50)
      * JTG 081496 CHANGED TO USE LN-SPRCLASS FOR SPR READ
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER READING BWFILE.
      * JTG 121696 CHANGED TO DISPLAY "W","B","S" WEEKLY/BIWEEKLY
      * BAH 121896 IF ALL LNAMTS AND BALANCES, DONT TEST RANGE, BECAUSE
      *            LNAMTS > 999999.00 WERE NOT GETTING INCLUDED. ADV,PR#421
      * BLV 060497 CHANGED TO WORK WITH NEW BATCH DRIVER - BREXEC; ALSO -
      *            ADDED USERID BRANCH FOR RANGE OF 0 - 0
      * MJD 082297 CHANGED LN-REC/CONPROFILE TO LN-REC/CONPROFILE(1)
      * GLF 090297 REMOVED ALL USE OF GLOBAL
      * BLV 091597 CHANGED PROMPTS FROM 270+ TO 210+; FIX FOR NEW AGEING
      * MJD 091897 ADDED SUB TOTALS FOR DELINQUENT AND SLOW
      * BAH 110797 ADDED DATE-TYPE FLAG FOR THE DATE RANGE ENTERED TO
      *            BE EITHER ENTRY DATE OR LOAN DATE, PR# 545
      * BAH 111497 BROKE OUT LN-OT2BAL INTO SEPARATE TOTALS,  PL# 149
      * BLV 031198 ADD NEXT MO AGEING OPTION FOR WORLD; IF DEL-FLAG
      *            ENTERED AS 'Y', BUMP AGE-DATE TO NEXT MONTH
      * BAH 032098 DEFAULT RECORDS-FOUND TO "N", SO TOTALS DONT PRINT IF
      *            NO RECORDS ARE FOUND, IT WAS A " "
      * JTG 033198 SUPRESSED PAID AHEAD PRINTOUT IF SP-BANKRULE = 'D', WORLD
      * GLF 040298 ADD PRINT REPO TOTALS AND PL TOTALS IF THEY ARE NOT "S"
      * JTG 040598 CHANGED VERSION CONTROL TO 'H', RE: BLV 031198 ADD NEXT
      *            MO AGEING OPTION FOR WORLD; IF DEL-FLAG
      * BLV 040598 ADDED PASSWD, BRSECURE FOR WORLD
      * GLF 041498 REVERSE CHANGE 'BAH 032098' BUT MOVE 'N' TO
      *            RECORDS-FOUND AFTER THE INITIALIZATION SECTION SO
      *            TO AVOID THE 'NO RECORDS FOUND' MESSAGE WHILE IN
      *            BATCH MAINTENANCE
      * JTG 041598 FIXED TESTS ON SKIP, INCLUDE, ONLY FOR POTS &
      *            MISPAYS - THEY DID NOT WORK
      * GLF 041798 WILL PRINT REPOS ONLY, PL ONLY, CONT/REC ONLY, ALL, ETC..
      * MJD 042198 REMOVED DISPLAY OF ACCOUNT NUMBER DURING RUN.
      *            DEFAULT DEL-FLAG TO "N"
      * BAH 042398 IF RUNNING FOR 1 BRANCH, MORE THAN 1 CLASS, SUMMARY,
      *            THE LAST CLASS TOTALS WERE NOT PRINTING BECAUSE OF THE
      *            MOVE "N" TO MORE-THAN-1-FLAG, MOVED WHERE IT WAS !!
      * MJD 042498 ADDED SCREEN OPTION TO PRINT DUE DATE SUMMARY
      *            ADDED SCREEN OPTION TO SUPPRESS RANGE LINES, CHANGED
      *            VERSION-CONTROL TO "I".
      * MJD 042898 CHANGED TO ONLY RESET MORE-THAN-1-CLASS FLAG AFTER
      *            BRANCH BREAK. IF IT IS NOT DONE HERE, THEN TOTALS
      *            FOR THE LAST CLASS PROCESSED WILL NOT PRINT.
      * JTG 042998 ADDED LOGIC TO PRINT A COMPANY CLASS SUMMARY, WHEN
      *            REPORT SPANS MORE THAN 1 BRANCH AND MORE THAN 1 CLASS
      * JTG 052898 ADDED PRINT OF DELINQUENCY WHEN REPORT IS FOR:
      *                     REPO 'ONLY' OR P&L 'ONLY'
      * BLV 070798 NOT PRINTING CLASS BREAKOUTS ON GRAND TOTALS IF ONLY 1
      *            CLASS IN LAST BRANCH; ADDED MORE-THAN-1-CLASS-ON-REPORT
      * BLV 070798 IF RUNNING IN BATCH, USE EO-BUF-MESS FOR TITLE
      * BLV 071798 IF RUNNING IN BATCH, CHECK FOR DEL-FLAG NOT = Y
      *            INSTEAD OF DEL-FLAG = N (EO-EXEC)
      * BLV 073198 FIXED SKIPPING DUEDATE SUMMARY QUESTION; HAD TO UP ARROW
      * JTG 121698 DISPLAYED 'R' VIA UNITPER-MASK LOGIC FOR REVOLVING ACCOUNT
      * GLF 990226 ADDED I,S,O OF REVOLVING ACCOUNTS
      * JTG 990317 ADDED TEST IF NOT GP-I-AM-MERC TO INCLUDE REPO ACCOUNTS
      *            IN CONTRACTUAL AND RECENCY TOTALS
      * MJD 990324 ADDED LOGIC FOR NON-MONTHLY TOTALS
      *            IF COMPANY = TFC AND LOAN IS NON-MONTHLY
      * BAH 990415 WAS NOT REMOVING WORK FILE
      * BLV 990512 CHECK FOR SKIP, INCLUDE, ONLY REVOLVING WAS CHECKING
      *            LC-REVOLVING INSTEAD OF LN-REVOLVING - IF YOU SAID
      *            SKIP YOU GOT NO ACCOUNTS!!
      * GLB 990712 ADDITIONAL TYPE OPTION FOR DATE RANGE, B=BORROWER
      *            REFERENCE DATE
      * GLB 990712 PRINT "MISSING BORROWER" WHEN BW-REC INVALID
      * GLF 990830 REMOVED PRINT OF JULIAN (H-JULIAN) FROM HEADER PER JTG
      * MJD 990902 ADDED ANOTHER OPTION TO TOTALS ONLY. ID "G" IS ENTERRED,
      *            THEN ONLY GRAND TOTALS ARE PRINTED. BRANCH/CLASS TOTALS
      *            ARE SKIPPED. "C" - ONLY COMPANY CLASS TOTALS ARE PRINTED.
      *            "A" - ONLY COMPANY TOTALS ARE PRINTED ( WORLD PR#208 -
      *            THIS IS SLIGHTLY DIFFERENT THAN REQUEST DESCRIBES, BUT
      *            THIS WAS THE EASIEST WAY JR AND I COULD COME UP WITH AS
      *            ADDING GROUP TOTALS WOULD BE A MAJOR UNDERTAKING.)
      * MJD 990924 ADDED TEST FOR MULLEN. IF GP-I-AM-MULL THEN
      *            PRINT CLASS TOTALS ALL THE TIME. (MULL PR#239)
      * BAH 991012 DEFAULT REPO TO "I" ON THE SCREEN FOR MULLEN PR# 971
      *
      * KEC 2000.0501 {PR#00000} MERCURY FINANCE; CINDY
      *                AS OF A PROBLEM IN THE BATCH WHEN DOING THE
      *                'A10-TO-A15' CONVERSION FOR THEM BECAUSE OF THE
      *                REVOLVING CHECK (LN-REVOLVING IS SPACES):
      *                   WAS: ( LN-REVOLVING     = "N" )
      *                        ( LN-REVOLVING NOT = "N" )
      *                   NEW: ( LN-REVOLVING     = "Y" )
      *                        ( LN-REVOLVING NOT = "Y" )
      *  BLV 000503 IN ENTRY OF DEL-FLAG PARAMETER, IF NOT GP-I-AM-WORLD
      *       MOVE "N" TO DEL-FLAG; UPGRADES MAY HAVE Y IN BATCH PARAMETERS:
      *       REVOL FLAG WAS ADDED IN THE MIDDLE OF EOBUF
      * KEC 000530 {PR#J009} PARADATA FINANCIAL [JOHN GENTILE]
      *             CHANGED '03  D-PAYDATE   PIC 99/99/99.'
      *             TO      '03  D-PAYDATE   PIC X(08).'
      * BAH 020114 UNDISC TOTALS WERE TRUNCATING, PERCENTAGES WRONG, REGACC
      * BLV 030502 FIX APR TO PRINT 3 DIGITS INSTEAD OF 2, CASHCO
      * BAH 040903 REPLACE BR-NAME IN HEADING WITH BR-REGION-NUMBER
      *             FOR GP-I-AM-REGACC, REGACC PR# 77
      * BLF 051025 IF GP-INCLUDE-O2-IN-BAL = "Y", DON'T SUBTRACT O2 FROM
      *            CURBAL, PRINT CURBAL INCLUDING OTHER 2 ON DETAIL LINE &
      *            FOR BALANCE DUE COLUMN IN TOTALS, PR# 2623
      * BAH 101108 PRINT THE CLASS RANGE FOR BRANCH TOTALS, WHEN REPORTS GET
      *            SEPARATED FROM THE RANGES, YOU DONT KNOW WHICH TOTALS THESE
      *            WERE FOR. HCREDIT PR# 3681
      * BLM 110224 OPTION IF I-AM-WORLD TO ENTER "F" FOR DUE DATE SUMMARY &
      *            GENERATE ASCII FILE WITH DUE DATE TOTALS BY BRANCH. FILE
      *            NAME IS /USR/TMP/DUE_MMDDYY_GGGG (GGGG=GROUP OR ENDING
      *            BRANCH.  PASSWORD FOR OPTION LP/LNAGED/0.  WORLD PR# 745
      *  CS 110823 DONT ADD TO PAID AHEAD IF SP-BANKRULE = 'F', ALREADY DID THIS
      *            IF SP-BANKRULE = "D"  WORLD PR#752
      * BAH 120112 EXPAND UNEARNED TO PRINT 999999.99, WORLD
      * MJD 130118 ADDED EXT-EOBUF-WINDOW-HANDLE (LIBGB/EOBUF.EXT) TO DISPLAY
      *            AND DESTROY AT END OF PROGRAM.
      * BAH 130118 GP-I-AM-MULL WIPING OUT REPO FLAG SET IN BATCH, PL# 788
      *  CS 130924 EXPANDED WK-NUM-BAL,WK-BAL,T-NM-BAL,T-BAL,TT-NM-BAL,SUB-TT
      *            -BA,DD-BAL,NM-BAL & THE UNE TOTALS TABLES WORLD PR#932
      *  CS 130924 REMOVE THE SIGN FROM PERCENT WORLD PR#932
      * MJD 140430 CHANGED TO PERFORM CALL-WRFORM TO DISPLAY INITIAL-VALUES
      * MJD 140509 ADDED DISPLAY SPACES TO EXIT PROGRAM.
      * BLM 141021 CHANGED LOGIC FOR NEW FILPATH LOAD ROUTINES
      * BAH 160125 SPLIT OUT LTFILE, PD0003
      *  CS 160830 CHANGED WK-CLASS, HOLD-CLASS FROM PIC 9(2) TO 9(3)
      * BLM 170208 CHANGED AGE-DATE, BEG-DATE, END-DATE FROM MMDDYY TO
      *            YYYYMMDD, PR# PD0006
      *  CS 170317 CHANGED DETAIL-LINE LAST PAYMENT TO MM/DD/YY
      * BAH 170606 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0606 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *          ADDED 'SP' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 181210 GLOBAL LNFILE, L2G
      * BAH 20190625 REMOVED LN-REVOLVING, PT0153
      * BAH 20190924 REMOVED ACCESS-CALL LNFILE
      * BAH 20200623 ADDED A4GL-WHERE-CONSTRAINT
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
           SELECT DUE-FILE ASSIGN TO DUE-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY DUE-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-CLASS           PIC 9(3).
      *-----------------------------------------------------------------
      *
      *    COMPANY CLASS TOTAL RECORD
      *
      *    WK-TOTAL-TBL   MAPS WORKING STORAGE TOTAL-TBL(1) CLASS TOTALS
      *-----------------------------------------------------------------
      *
      *    WK-NM-TBL            OCCURS 31 TIMES.
      *    (01) =  CURRENT NONMON
      *    (02) =   1  WEEKS
      *     ....
      *    (31) =  30+ WEEKS
      *
      *    WK-AGETBL :
      *         TYP(1)       (2)             (3)         (4)        (5)
      *        CONTRACTUAL/RECENCY       PAID-AHEAD      REPO      P & L
      *    -------------------------------------------------------------
      * AGE(1)  =   30 DAYS           |  PAID-AHEAD  |  ONHAND  |  PL1
      *    (2)  =   60 DAYS           |  N/A         |  SOLD    |  PL2
      *    (3)  =   90 DAYS           |  N/A         |  N/A     |  PL3
      *    (4)  =  120 DAYS           |  N/A         |  N/A     |  PL4
      *    (5)  =  150 DAYS           |  N/A         |  N/A     |  PL5
      *    (6)  =  180 DAYS           |  N/A         |  N/A     |  N/A
      *    (7)  =  210 + DAYS         |  N/A         |  N/A     |  N/A
      *    (8)  =  <M> MISSPAYS       |  N/A         |  N/A     |  N/A
      *    (9)  =  <P> POTENTIAL 30'S |  N/A         |  N/A     |  N/A
      *    (10) =  CURRENT ACCOUNTS   |  N/A         |  N/A     |  N/A
      *-----------------------------------------------------------------
      *    WK-TYPETBL:
      *       TYP(1) = CONTRACTUAL
      *          (2) = RECENCY
      *          (3) = PAID AHEAD
      *                CONTRACTUALLY CURRENT AND
      *                  DELINQ-RECENCY
      *          (4) = REPO
      *          (5) = P & L
      *
      ******************************************************************
           03  WK-TOTAL-TBL.
               05  WK-NM-TBL            OCCURS 31 TIMES.
                   07  WK-NM-NO          PIC S9(7).
                   07  WK-NM-BAL         PIC S9(10)V99.
                   07  WK-NM-INT         PIC S9(7)V99.
                   07  WK-NM-OTH         PIC S9(9)V99.
                   07  WK-NM-OTH2        PIC S9(9)V99.
                   07  WK-NM-UNEARN      PIC S9(10)V99.
                   07  WK-NM-UNDISC      PIC S9(9)V99.
               05  WK-AGETBL            OCCURS 10 TIMES.
                   07  WK-TYPETBL       OCCURS 5  TIMES.
                       09  WK-NO         PIC S9(7).
                       09  WK-BAL        PIC S9(10)V99.
                       09  WK-INT        PIC S9(7)V99.
                       09  WK-OTH        PIC S9(9)V99.
                       09  WK-OTH2       PIC S9(9)V99.
                       09  WK-UNEARN     PIC S9(10)V99.
                       09  WK-UNDISC     PIC S9(9)V99.

       FD  DUE-FILE
           LABEL RECORDS ARE STANDARD.
       01  DUE-REC.
           03  DUE-BRNO                  PIC 9(4).
           03  DUE-DAY                   PIC 99.
           03  DUE-NO-LOANS              PIC 9(6).
           03  DUE-CR                    PIC X.
           03  DUE-LF                    PIC X.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/ALJOIN.CBL S35 11/22/23-15:31:43 >".

      ******************************************************************
      *
      *     F I L E   S P E C S  *
      *
      ******************************************************************
       COPY "LIBGB/GB01GR.CPY".
      *COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
      *COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
      *COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
      *COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
      *COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
      *COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
      *COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  QBR-WHERE-BEG-KEY.
           03  QBR-WBEG-NO                            PIC 9(4).
 
       01  QBR-WHERE-END-KEY.
           03  QBR-WEND-NO                            PIC 9(4).
 
       01  QBR-REC.
           03  QBR-NO                                 PIC 9(4).
           03  QBR-LTOUCH-DATE                        PIC X(10).
           03  QBR-NAME                               PIC X(40).
           03  QBR-ADR1                               PIC X(24).
           03  QBR-ADR2                               PIC X(24).
           03  QBR-CITY                               PIC X(15).
           03  QBR-STATE                              PIC XX.
           03  QBR-ZIP                                PIC X(10).
           03  QBR-PHONE                              PIC 9(10).
           03  QBR-RETAIN                             PIC 9(11).
           03  QBR-NEXT-CHECK                         PIC 9(6).
           03  QBR-PRINTER-1                          PIC 99.
           03  QBR-PRINTER-2                          PIC 99.
           03  QBR-PRINTER-3                          PIC 99.
           03  QBR-PRINTER-4                          PIC 99.
           03  QBR-FEE-GL-BRNO                        PIC 9(4).
           03  QBR-REMIT1                             PIC X(30).
           03  QBR-REMIT2                             PIC X(30).
           03  QBR-REMIT3                             PIC X(30).
           03  QBR-STMNT1                             PIC X(60).
           03  QBR-STMNT2                             PIC X(60).
           03  QBR-INS-PRODUCER-NUM                   PIC 9(8).
           03  QBR-DLRCHK                             PIC X.
           03  QBR-FRONT-END-FG                       PIC X.
           03  QBR-EMLNRG-PHONE                       PIC X.
           03  QBR-POST-GL                            PIC X.
           03  QBR-ALLOTMNTS                          PIC X.
           03  QBR-EXTRACT-AUTO-UPD                   PIC X.
           03  QBR-PCNI-LEDGER                        PIC X.
           03  QBR-PRINTERFG-1                        PIC X.
           03  QBR-PRINTERFG-2                        PIC X.
           03  QBR-PRINTERFG-3                        PIC X.
           03  QBR-PRINTERFG-4                        PIC X.
           03  QBR-NEGDLRSV                           PIC X.
           03  QBR-CPCORP-ORDER                       PIC X.
           03  QBR-PRIO-MEMO-FLAG                     PIC X.
           03  QBR-PREBILL-PL-JD-OPT                  PIC X.
           03  QBR-LOAN-NUM-FLAG                      PIC X.
           03  QBR-DOWNPY-FLAG                        PIC X.
           03  QBR-AUTO-INS-EARN                      PIC X.
           03  QBR-OTH-INS-LEVELS                     PIC X.
           03  QBR-ALLOW-INTERBR                      PIC X.
           03  QBR-UNER-DLANRP                        PIC X.
           03  QBR-SECOND-PROMISE                     PIC X.
           03  QBR-AVAIL-CREDIT                       PIC X.
           03  QBR-RECP1                              PIC X(30).
           03  QBR-RECP2                              PIC X(30).
           03  QBR-INC-WAIVERS                        PIC X.
           03  QBR-INC-INS                            PIC X.
           03  QBR-BP-NEXT-BATCH                      PIC 9(2).
           03  QBR-BP-NOPAYOFF                        PIC X.
           03  QBR-BP-ODDPAY                          PIC X.
           03  QBR-BP-LC                              PIC X.
           03  QBR-BP-NOPRIN                          PIC X.
           03  QBR-BP-FLASH                           PIC X.
           03  QBR-BP-ACTIONCD                        PIC X.
           03  QBR-BP-MAXCON                          PIC 9(3).
           03  QBR-BP-MAXREC                          PIC 9(3).
           03  QBR-BP-PL                              PIC X.
           03  QBR-BP-JD                              PIC X.
           03  QBR-BP-BRANCH                          PIC 9(5).
           03  QBR-AGE-ERN                            PIC X.
           03  QBR-OTHER-BRANCH-GL                    PIC X.
           03  QBR-SALES-SMALL                        PIC X.
           03  QBR-NEW-COLL-INQUIRY                   PIC X.
           03  QBR-CHANGE-MEMO                        PIC X.
           03  QBR-INS-ADDON                          PIC X.
           03  QBR-TRAILER-REC                        PIC X.
           03  QBR-ALDEL-FLAG                         PIC X.
           03  QBR-IB-LCHG-MESS                       PIC X.
           03  QBR-COL-QUEUE-PAST-DUE                 PIC X.
           03  QBR-LI-DLCOMM-CMSI-PP-MAX              PIC 9(4)V99.
           03  QBR-ADDON-FEECD-O2-1                   PIC XX.
           03  QBR-ADDON-FEECD-O2-2                   PIC XX.
           03  QBR-AUTO-CHECK                         PIC X.
           03  QBR-RECEIPT-PROMDATE                   PIC X.
           03  QBR-RECEIPT-INTPD                      PIC X.
           03  QBR-COLL-ID-SCAN                       PIC X.
           03  QBR-STOP-ON-COLL                       PIC X.
           03  QBR-DUEDATE-CHANGE-CD                  PIC X.
           03  QBR-CHECK-EOM-FG                       PIC X.
           03  QBR-VOLUME-DLANRP                      PIC X.
           03  QBR-PDTH-NXDUE                         PIC X.
           03  QBR-LC-CODE                            PIC X.
           03  QBR-VALIDATE-PRUS                      PIC X.
           03  QBR-AUTO-OPEN-BAL                      PIC X.
           03  QBR-OPEN-BAL-1                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-2                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-3                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-4                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-5                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-6                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-7                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-8                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-9                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-10                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-11                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-12                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-13                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-14                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-15                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-16                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-17                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-18                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-19                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-20                        PIC S9(7)V99.
           03  QBR-TENDER-CD-1                        PIC X(5).
           03  QBR-TENDER-CD-2                        PIC X(5).
           03  QBR-TENDER-CD-3                        PIC X(5).
           03  QBR-DLPARV-ADJ-FG                      PIC X.
           03  QBR-OVERRIDE-BR-STATE                  PIC X.
           03  QBR-DEFAULT-SC-CLASS                   PIC 9(3).
           03  QBR-RECDEL-NOT-REC                     PIC X.
           03  QBR-POST-AHEAD-FG                      PIC X.
           03  QBR-CAN-LOANS-FG                       PIC X.
           03  QBR-NON-MONTHLY-CAN-LOANS              PIC X.
           03  QBR-BP-ALLOW-PMT-ACCT-OTHBAL           PIC X.
           03  QBR-LBOX-ALLOW-PMT-ACCT-OTHBAL         PIC X.
           03  QBR-REGION-NUMBER                      PIC 9(2).
           03  QBR-OUR-FEE                            PIC X.
           03  QBR-REB-METHOD-1                       PIC 99.
           03  QBR-REB-METHOD-2                       PIC 99.
           03  QBR-REB-METHOD-3                       PIC 99.
           03  QBR-REB-METHOD-4                       PIC 99.
           03  QBR-REB-METHOD-5                       PIC 99.
           03  QBR-REB-METHOD-6                       PIC 99.
           03  QBR-REB-METHOD-7                       PIC 99.
           03  QBR-REB-METHOD-8                       PIC 99.
           03  QBR-REB-METHOD-9                       PIC 99.
           03  QBR-REB-METHOD-10                      PIC 99.
           03  QBR-REB-METHOD-11                      PIC 99.
           03  QBR-REBDAYS-MONTH-1                    PIC 99.
           03  QBR-REBDAYS-MONTH-2                    PIC 99.
           03  QBR-REBDAYS-MONTH-3                    PIC 99.
           03  QBR-REBDAYS-MONTH-4                    PIC 99.
           03  QBR-REBDAYS-MONTH-5                    PIC 99.
           03  QBR-REBDAYS-MONTH-6                    PIC 99.
           03  QBR-REBDAYS-MONTH-7                    PIC 99.
           03  QBR-REBDAYS-MONTH-8                    PIC 99.
           03  QBR-REBDAYS-MONTH-9                    PIC 99.
           03  QBR-REBDAYS-MONTH-10                   PIC 99.
           03  QBR-REBDAYS-MONTH-11                   PIC 99.
           03  QBR-ACCRUAL-MONTHS-1                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-2                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-3                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-4                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-5                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-6                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-7                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-8                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-9                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-10                  PIC 999.
           03  QBR-ACCRUAL-MONTHS-11                  PIC 999.
           03  QBR-GLOVERSHORT                        PIC 9(11).
           03  QBR-GLBANK                             PIC 9(11).
           03  QBR-GLONHAND                           PIC 9(11).
           03  QBR-BANKBAL                            PIC S9(9)V99.
           03  QBR-RETAIN-AMT                         PIC S9(9)V99.
           03  QBR-LCARDFG                            PIC X.
           03  QBR-DATA-PATH                          PIC X(6).
           03  QBR-MACHINE                            PIC XX.
           03  QBR-MANAGER                            PIC X(25).
           03  QBR-EOD-PROCEDURE                      PIC XXX.
           03  QBR-BP-PAYOFF-NONCASH                  PIC X.
           03  QBR-BP-PAYOFF-OV                       PIC X.
           03  QBR-LI-DLCOMM-CMSI-O3-MAX              PIC 9(4)V99.
           03  QBR-LR-GROUP-COLLID                    PIC X.
           03  QBR-UNITPER-FG                         PIC X.
           03  QBR-UNITPER-Y-D-FG                     PIC X.
           03  QBR-NO-PROMPT-FOR-OTHBR                PIC X.
           03  QBR-WORLD-BRANCH-TYPE                  PIC X.
           03  QBR-COL-QUEUE-BEHAVIOR-FG              PIC X.
           03  QBR-CONVERT-AUTO-ACCTNO                PIC X.
           03  QBR-TS-REPO-INS-FG                     PIC X.
           03  QBR-BOOK-SF-BR-ONLY                    PIC X.
           03  QBR-FORCE-VALID-CITY-COUNTY            PIC X.
           03  QBR-SET-COLLCD-DL-MFGCD                PIC X.
           03  QBR-DEFAULT-SF-UNITPER-CD              PIC X.
           03  QBR-DEFAULT-SF-PURCD                   PIC X(3).
           03  QBR-DEFAULT-LN-UNITPER-CD              PIC X.
           03  QBR-DEFAULT-LN-PURCD                   PIC X(3).
           03  QBR-TAP-OTHER-DL                       PIC X.
           03  QBR-AH-AND-CL                          PIC X.
           03  QBR-NO-CASH-PROCEEDS                   PIC X.
           03  QBR-RI-BEFORE-REPO                     PIC X.
           03  QBR-RI-BEFORE-P4P5                     PIC X.
           03  QBR-NOT-OURS-REFUND                    PIC X.
           03  QBR-SFDLO3-WINDOW-FG                   PIC X.
           03  QBR-CALC-INS-ADJ                       PIC X.
           03  QBR-AC-BEFORE-REPO                     PIC X.
           03  QBR-AC-BEFORE-P4P5                     PIC X.
           03  QBR-BANK-ROUTING-NO                    PIC 9(9).
           03  QBR-BANK-CLEARING-GL                   PIC 9(11).
           03  QBR-GLBANK-DEPOSIT                     PIC 9(11).
           03  QBR-BANK-ACCTNO                        PIC X(17).
           03  QBR-DEFAULT-SC-SUBCLASS                PIC 9(3).
           03  QBR-NO-OUT-DLR-RSV                     PIC X.
           03  QBR-COLLECTOR-QUEUE                    PIC X.
           03  QBR-COLL-QUEUE-CON-REC                 PIC X.
           03  QBR-JE-ALLOCATION-SKIP                 PIC X.
           03  QBR-EXPAND-APP-INPUT                   PIC X.
           03  QBR-A50-CPCORP                         PIC X.
           03  QBR-INBR-BILNSC-FG                     PIC X.
           03  QBR-INBR-BRIEF-FG                      PIC X.
           03  QBR-LEDGER-JUST-INTCHG                 PIC X.
           03  QBR-BYINQ-START-PAGE                   PIC 99.
           03  QBR-DSINQ-START-PAGE                   PIC 99.
           03  QBR-APP-REQUIRED-FOR-INPUT             PIC X.
           03  QBR-CREDIT-SCORING-FG                  PIC X.
           03  QBR-COL-QUEUE-LOW-TO-HI                PIC X.
           03  QBR-CAN-GROUP-ID                       PIC X(4).
           03  QBR-AUTO-LNSTAT-WINDOW                 PIC X.
           03  QBR-TIME-ZONE-FACTOR                   PIC S9.
           03  QBR-LONPF-OPT-DISPLAY                  PIC X.
           03  QBR-GIFTNET                            PIC X.
           03  QBR-GIFTNET-LEDGER-LINE                PIC X.
           03  QBR-NO-FT-EMP                          PIC 9(3).
           03  QBR-NO-PT-EMP                          PIC 99V99.
           03  QBR-BALLOON-LOANS-FG                   PIC X.
           03  QBR-GL-TYPE-ALLOWED                    PIC X.
           03  QBR-GL-ENTRY-DEFAULT                   PIC X.
           03  QBR-MIDSTREAM-1STPAY-FG                PIC X.
           03  QBR-NODEF-1ST-X-MONTHS                 PIC 9.
           03  QBR-ADD-INS-FEECODE                    PIC XX.
           03  QBR-ROAD-G-SGL-FEECODE                 PIC XX.
           03  QBR-ROAD-G-FAM-FEECODE                 PIC XX.
           03  QBR-NODEF-IF-ZERO-AMT                  PIC X.
           03  QBR-ASSESS-LATE-CHGS                   PIC X.
           03  QBR-NEXT-SOLICIT-NO                    PIC 9(6).
           03  QBR-ADDON-FEECD-O3-1                   PIC XX.
           03  QBR-ADDON-FEECD-O3-2                   PIC XX.
           03  QBR-IMMEDIATE-RECEIPT                  PIC X.
           03  QBR-IMMEDIATE-DOC-PRINT                PIC X.
           03  QBR-AUTO-PL-REF-AC                     PIC X.
           03  QBR-FAX-NUMBER                         PIC 9(10).
           03  QBR-ALPHA-DRAWERS                      PIC X.
           03  QBR-LATECHG-ON-ALLOTMENTS              PIC X.
           03  QBR-ALLOW-SYS-AS-EFFDATE               PIC X(01).
           03  QBR-TAX-PREBOOK-CUST                   PIC 9(4).
           03  QBR-TAX-PREBOOK-NONCUST                PIC 9(4).
           03  QBR-PASSWORD-DRAWER                    PIC X.
           03  QBR-ONESTEP-RN-FG                      PIC X.
           03  QBR-LI-LISCAN-FG                       PIC X.
           03  QBR-HIDE-END-BAL                       PIC X(01).
           03  QBR-LBOX-NOPAYOFF                      PIC X.
           03  QBR-LBOX-ODDPAY                        PIC X.
           03  QBR-LBOX-LC                            PIC X.
           03  QBR-LBOX-NOPRIN                        PIC X.
           03  QBR-LBOX-FLASH                         PIC X.
           03  QBR-LBOX-ACTIONCD                      PIC X.
           03  QBR-LBOX-MAXCON                        PIC 9(3).
           03  QBR-LBOX-MAXREC                        PIC 9(3).
           03  QBR-LBOX-PL                            PIC X.
           03  QBR-LBOX-JD                            PIC X.
           03  QBR-LBOX                               PIC X(01).
           03  QBR-LATECHG-ON-LBOX                    PIC X(01).
           03  QBR-ALLOW-OV-TRCD                      PIC X.
           03  QBR-GLOBAL-QUEUE                       PIC X(01).
           03  QBR-LBOX-PAYOFF-NONCASH                PIC X.
           03  QBR-LBOX-PAYOFF-OV                     PIC X.
           03  QBR-COLLECT-MANAGER                    PIC X(25).
           03  QBR-COLLECT-PHONE                      PIC 9(10).
           03  QBR-COLLECT-EXT                        PIC 9(4).
           03  QBR-ALT-INTO-THIS-BRANCH               PIC X.
           03  QBR-POFF-TITLE-WINDOW                  PIC X.
           03  QBR-COUPON-STATEMENT                   PIC X.
           03  QBR-SUPERVISOR                         PIC X(25).
           03  QBR-AREA-MANAGER                       PIC X(25).
           03  QBR-MIN-MONTHS-DEF                     PIC 99.
           03  QBR-CC-FLAG                            PIC X.
           03  QBR-CC-ADR1                            PIC X(24).
           03  QBR-CC-CITY                            PIC X(15).
           03  QBR-CC-STATE                           PIC XX.
           03  QBR-CC-ZIP                             PIC X(10).
           03  QBR-MIN-MONTHS-D2                      PIC 99.
           03  QBR-SKIP-MIDSTREAM-TRW                 PIC X.
           03  QBR-GL-ESCROW-BALANCE                  PIC 9(11).
           03  QBR-GL-ESCROW-PAYABLE                  PIC 9(11).
           03  QBR-ALLOW-FORCE-OPEN-FG                PIC X.
           03  QBR-ACH-BANK-NO                        PIC 9.
           03  QBR-WIMENU-CHECK-WRITING               PIC X.
           03  QBR-ADP-PAY-GROUP                      PIC X(3).
           03  QBR-ALLOW-SCANNED-CKS                  PIC X.
           03  QBR-CHECK-DEPOSIT-GL                   PIC 9(11).
           03  QBR-OPTION-PST-LASER                   PIC X.
           03  QBR-USE-DEBIT-CARDS                    PIC X.
           03  QBR-GL-DEBIT-CARD-PROCEEDS             PIC 9(11).
           03  QBR-WKDAY-BANKID                       PIC 9(4).
           03  QBR-PURGE-NOW-FLAG                     PIC X.
           03  QBR-EMAIL                              PIC X(50).
           03  QBR-MANAGER-EMAIL                      PIC X(50).
           03  QBR-VERITEC-LICENSE                    PIC 9(7).
           03  QBR-TEST-PP-AUTO-INSCOVR               PIC X.
           03  QBR-USE-NEW-WFARGO-ACCT                PIC X.

       01  QGR1-WHERE-BEG-KEY.
           05  QGR1-WBEG-ID               PIC X(4).

       01  QGR1-WHERE-END-KEY.
           05  QGR1-WEND-ID               PIC X(4).

       01  QGR-REC.
           03  QGR-ID                    PIC X(4).
           03  QGR-NAME                  PIC X(20).
           03  QGR-LEVEL                 PIC 9.
           03  QGR-MEMBER-ID-1           PIC X(4).
           03  QGR-MEMBER-ID-2           PIC X(4).
           03  QGR-MEMBER-ID-3           PIC X(4).
           03  QGR-MEMBER-ID-4           PIC X(4).
           03  QGR-MEMBER-ID-5           PIC X(4).
           03  QGR-MEMBER-ID-6           PIC X(4).
           03  QGR-MEMBER-ID-7           PIC X(4).
           03  QGR-MEMBER-ID-8           PIC X(4).
           03  QGR-MEMBER-ID-9           PIC X(4).
           03  QGR-MEMBER-ID-10          PIC X(4).
           03  QGR-MEMBER-ID-11           PIC X(4).
           03  QGR-MEMBER-ID-12           PIC X(4).
           03  QGR-MEMBER-ID-13           PIC X(4).
           03  QGR-MEMBER-ID-14           PIC X(4).
           03  QGR-MEMBER-ID-15           PIC X(4).
           03  QGR-MEMBER-ID-16           PIC X(4).
           03  QGR-MEMBER-ID-17           PIC X(4).
           03  QGR-MEMBER-ID-18           PIC X(4).
           03  QGR-MEMBER-ID-19           PIC X(4).
           03  QGR-MEMBER-ID-20           PIC X(4).
           03  QGR-MEMBER-ID-21           PIC X(4).
           03  QGR-MEMBER-ID-22           PIC X(4).
           03  QGR-MEMBER-ID-23           PIC X(4).
           03  QGR-MEMBER-ID-24           PIC X(4).
           03  QGR-MEMBER-ID-25           PIC X(4).
           03  QGR-MEMBER-ID-26           PIC X(4).
           03  QGR-MEMBER-ID-27           PIC X(4).
           03  QGR-MEMBER-ID-28           PIC X(4).
           03  QGR-MEMBER-ID-29           PIC X(4).
           03  QGR-MEMBER-ID-30          PIC X(4).
           03  QGR-MEMBER-ID-31           PIC X(4).
           03  QGR-MEMBER-ID-32           PIC X(4).
           03  QGR-MEMBER-ID-33           PIC X(4).
           03  QGR-MEMBER-ID-34           PIC X(4).
           03  QGR-MEMBER-ID-35           PIC X(4).
           03  QGR-MEMBER-ID-36           PIC X(4).
           03  QGR-MEMBER-ID-37           PIC X(4).
           03  QGR-MEMBER-ID-38           PIC X(4).
           03  QGR-MEMBER-ID-39           PIC X(4).
           03  QGR-MEMBER-ID-40          PIC X(4).
           03  QGR-MEMBER-ID-41           PIC X(4).
           03  QGR-MEMBER-ID-42           PIC X(4).
           03  QGR-MEMBER-ID-43           PIC X(4).
           03  QGR-MEMBER-ID-44           PIC X(4).
           03  QGR-MEMBER-ID-45           PIC X(4).
           03  QGR-MEMBER-ID-46           PIC X(4).
           03  QGR-MEMBER-ID-47           PIC X(4).
           03  QGR-MEMBER-ID-48           PIC X(4).
           03  QGR-MEMBER-ID-49           PIC X(4).
           03  QGR-MEMBER-ID-50          PIC X(4).
           03  QGR-MEMBER-ID-51           PIC X(4).
           03  QGR-MEMBER-ID-52           PIC X(4).
           03  QGR-MEMBER-ID-53           PIC X(4).
           03  QGR-MEMBER-ID-54           PIC X(4).
           03  QGR-MEMBER-ID-55           PIC X(4).
           03  QGR-MEMBER-ID-56           PIC X(4).
           03  QGR-MEMBER-ID-57           PIC X(4).
           03  QGR-MEMBER-ID-58           PIC X(4).
           03  QGR-MEMBER-ID-59           PIC X(4).
           03  QGR-MEMBER-ID-60          PIC X(4).
           03  QGR-MEMBER-ID-61           PIC X(4).
           03  QGR-MEMBER-ID-62           PIC X(4).
           03  QGR-MEMBER-ID-63           PIC X(4).
           03  QGR-MEMBER-ID-64           PIC X(4).
           03  QGR-MEMBER-ID-65           PIC X(4).
           03  QGR-MEMBER-ID-66           PIC X(4).
           03  QGR-MEMBER-ID-67           PIC X(4).
           03  QGR-MEMBER-ID-68           PIC X(4).
           03  QGR-MEMBER-ID-69           PIC X(4).
           03  QGR-MEMBER-ID-70          PIC X(4).
           03  QGR-MEMBER-ID-71           PIC X(4).
           03  QGR-MEMBER-ID-72           PIC X(4).
           03  QGR-MEMBER-ID-73           PIC X(4).
           03  QGR-MEMBER-ID-74           PIC X(4).
           03  QGR-MEMBER-ID-75           PIC X(4).
           03  QGR-MEMBER-ID-76           PIC X(4).
           03  QGR-MEMBER-ID-77           PIC X(4).
           03  QGR-MEMBER-ID-78           PIC X(4).
           03  QGR-MEMBER-ID-79           PIC X(4).
           03  QGR-MEMBER-ID-80          PIC X(4).
           03  QGR-MEMBER-ID-81           PIC X(4).
           03  QGR-MEMBER-ID-82           PIC X(4).
           03  QGR-MEMBER-ID-83           PIC X(4).
           03  QGR-MEMBER-ID-84           PIC X(4).
           03  QGR-MEMBER-ID-85           PIC X(4).
           03  QGR-MEMBER-ID-86           PIC X(4).
           03  QGR-MEMBER-ID-87           PIC X(4).
           03  QGR-MEMBER-ID-88           PIC X(4).
           03  QGR-MEMBER-ID-89           PIC X(4).
           03  QGR-MEMBER-ID-90          PIC X(4).
           03  QGR-MEMBER-ID-91           PIC X(4).
           03  QGR-MEMBER-ID-92           PIC X(4).
           03  QGR-MEMBER-ID-93           PIC X(4).
           03  QGR-MEMBER-ID-94           PIC X(4).
           03  QGR-MEMBER-ID-95           PIC X(4).
           03  QGR-MEMBER-ID-96           PIC X(4).
           03  QGR-MEMBER-ID-97           PIC X(4).
           03  QGR-MEMBER-ID-98           PIC X(4).
           03  QGR-MEMBER-ID-99           PIC X(4).
           03  QGR-MEMBER-ID-100          PIC X(4).
           03  QGR-CONTACT               PIC X(20).
           03  QGR-ADDR-1                PIC X(20).
           03  QGR-ADDR-2                PIC X(20).
           03  QGR-CITY                  PIC X(15).
           03  QGR-STATE                 PIC X(4).
           03  QGR-ZIP                   PIC X(10).
           03  QGR-PHONE                 PIC 9(10).
           03  QGR-HOME-BRANCH           PIC 9(4).
           03  QGR-LN-APPROVAL-LIMIT     PIC 9(7).
           03  QGR-LTOUCH-DATE           PIC X(10).

       01  QBW1-WHERE-BEG-KEY.
           03  QBW1-WBEG-OWNBR             PIC 9(4).
           03  QBW1-WBEG-SSNO             PIC 9(9).
       01  QBW1-WHERE-END-KEY.
           03  QBW1-WEND-OWNBR             PIC 9(4).
           03  QBW1-WEND-SSNO             PIC 9(9).
       01  QBW2-WHERE-BEG-KEY.
           03  QBW2-WBEG-OWNBR             PIC 9(4).
           03  QBW2-WBEG-LNAME            PIC X(35).
           03  QBW2-WBEG-FNAME            PIC X(35).
       01  QBW2-WHERE-END-KEY.
           03  QBW2-WEND-OWNBR             PIC 9(4).
           03  QBW2-WEND-LNAME            PIC X(35).
           03  QBW2-WEND-FNAME            PIC X(35).
       01  QBW3-WHERE-BEG-KEY.
           03  QBW3-WBEG-OWNBR             PIC 9(4).
           03  QBW3-WBEG-SPSSNO           PIC 9(9).
       01  QBW3-WHERE-END-KEY.
           03  QBW3-WEND-OWNBR             PIC 9(4).
           03  QBW3-WEND-SPSSNO           PIC 9(9).
       01  QBW4-WHERE-BEG-KEY.
           03  QBW4-WBEG-OWNBR             PIC 9(4).
           03  QBW4-WBEG-SPLNAME          PIC X(35).
           03  QBW4-WBEG-SPFNAME          PIC X(35).
       01  QBW4-WHERE-END-KEY.
           03  QBW4-WEND-OWNBR             PIC 9(4).
           03  QBW4-WEND-SPLNAME          PIC X(35).
           03  QBW4-WEND-SPFNAME          PIC X(35).
 
       01  QBW-REC.
           03  QBW-OWNBR                              PIC 9(4).
           03  QBW-SSNO                               PIC 9(9).
           03  QBW-LNAME                              PIC X(35).
           03  QBW-FNAME                              PIC X(35).
           03  QBW-MNAME                              PIC X(35).
           03  QBW-NICKNAME                           PIC X(35).
           03  QBW-GENERATION                         PIC X(7).
           03  QBW-ADR1                               PIC X(30).
           03  QBW-ADR2                               PIC X(30).
           03  QBW-CITY                               PIC X(30).
           03  QBW-STATE                              PIC XX.
           03  QBW-ZIP                                PIC X(10).
           03  QBW-LTOUCH-DATE                        PIC X(10).
           03  QBW-REFDATE                            PIC X(10).
           03  QBW-BDATE                              PIC X(10).
           03  QBW-PHONE                              PIC 9(10).
           03  QBW-CELL-PHONE                         PIC 9(10).
           03  QBW-NATIONALITY                        PIC X(8).
           03  QBW-GENDER                             PIC X.
           03  QBW-DEPEND                             PIC 99.
           03  QBW-RENTCD                             PIC X(4).
           03  QBW-TIMEADDR                           PIC X(4).
           03  QBW-CREDITCD                           PIC X(4).
           03  QBW-STRTGCD                            PIC X(6).
           03  QBW-DWELLCD                            PIC XX.
           03  QBW-COUNTY                             PIC X(4).
           03  QBW-ORIGIN                             PIC X(4).
           03  QBW-CENSUSCD                           PIC X(10).
           03  QBW-ORGSLSMN                           PIC 9(4).
           03  QBW-REPCODE                            PIC X(4).
           03  QBW-SPSSNO                             PIC 9(9).
           03  QBW-SPLNAME                            PIC X(35).
           03  QBW-SPFNAME                            PIC X(35).
           03  QBW-SP-BDATE                           PIC X(10).
           03  QBW-EMPLOYER                           PIC X(35).
           03  QBW-EMPLPOS                            PIC X(24).
           03  QBW-EMPPHONE                           PIC 9(10).
           03  QBW-EMPEXT                             PIC 9(4).
           03  QBW-EMPYEARS                           PIC 99.
           03  QBW-EMPPAY                             PIC 9(7)V99.
           03  QBW-EMPPAYOT                           PIC 9(7)V99.
           03  QBW-EMPPAYSR                           PIC X(8).
           03  QBW-EMPADDR1                           PIC X(30).
           03  QBW-EMPADDR2                           PIC X(30).
           03  QBW-EMPCITY                            PIC X(30).
           03  QBW-EMPSTATE                           PIC XX.
           03  QBW-EMPZIP                             PIC X(10).
           03  QBW-WORKHRS                            PIC X(9).
           03  QBW-LOANS                              PIC 99.
           03  QBW-INTYTD                             PIC S9(7)V99.
           03  QBW-INTLYR                             PIC S9(7)V99.
           03  QBW-LIASMLN                            PIC S9(7)V99.
           03  QBW-LIAOTH                             PIC S9(7)V99.
           03  QBW-LASTPODATE                         PIC X(10).
           03  QBW-PRLOANS                            PIC 9(3).
           03  QBW-RE-INTYTD                          PIC S9(7)V99.
           03  QBW-RE-INTLYR                          PIC S9(7)V99.
           03  QBW-CREDIT-LIMIT                       PIC 9(7)V99.
           03  QBW-CREDIT-SCORE                       PIC 9(4).
           03  QBW-CRED-SCORE-1                       PIC 9(4).
           03  QBW-CRED-SCORE-2                       PIC 9(4).
           03  QBW-CRED-SCORE-3                       PIC 9(4).
           03  QBW-CRED-SCORE-4                       PIC 9(4).
           03  QBW-BNKRPT-SCORE-1                     PIC 9(4).
           03  QBW-BNKRPT-SCORE-2                     PIC 9(4).
           03  QBW-BNKRPT-SCORE-3                     PIC 9(4).
           03  QBW-BNKRPT-SCORE-4                     PIC 9(4).
           03  QBW-LAST-FORM-NO                       PIC 9(4).
           03  QBW-LAST-FORM-DATE                     PIC X(10).
           03  QBW-PRIOR-RN-FOR10-DATE                PIC X(10).
           03  QBW-LAST-RN-FOR10-DATE                 PIC X(10).
           03  QBW-ADR-PHONE-CHG-DATE                 PIC X(10).
           03  QBW-BAD-ADR-FG                         PIC X(1).
           03  QBW-BAD-PHONE-FG                       PIC X(1).
           03  QBW-BAD-EMPPHONE-FG                    PIC X(1).
           03  QBW-BAD-MISC-PHONE1-FG                 PIC X(1).
           03  QBW-BAD-MISC-PHONE2-FG                 PIC X(1).
           03  QBW-BAD-CELL-FG                        PIC X(1).
           03  QBW-MISC-PHONE-1                       PIC 9(10).
           03  QBW-MISC-EXT-1                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-1                    PIC XX.
           03  QBW-MISC-PHONE-2                       PIC 9(10).
           03  QBW-MISC-EXT-2                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-2                    PIC XX.
           03  QBW-MISC-PHONE-3                       PIC 9(10).
           03  QBW-MISC-EXT-3                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-3                    PIC XX.
           03  QBW-MISC-PHONE-4                       PIC 9(10).
           03  QBW-MISC-EXT-4                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-4                    PIC XX.
           03  QBW-MISC-PHONE-5                       PIC 9(10).
           03  QBW-MISC-EXT-5                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-5                    PIC XX.
           03  QBW-MISC-PHONE-6                       PIC 9(10).
           03  QBW-MISC-EXT-6                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-6                    PIC XX.
           03  QBW-CONTACT-DESC-1                     PIC X(46).
           03  QBW-CONTACT-PHONE-1                    PIC 9(10).
           03  QBW-CONTACT-EXT-1                      PIC 9(4).
           03  QBW-CONTACT-RELATION-1                 PIC X(12).
           03  QBW-CONTACT-DESC-2                     PIC X(46).
           03  QBW-CONTACT-PHONE-2                    PIC 9(10).
           03  QBW-CONTACT-EXT-2                      PIC 9(4).
           03  QBW-CONTACT-RELATION-2                 PIC X(12).
           03  QBW-CONTACT-DESC-3                     PIC X(46).
           03  QBW-CONTACT-PHONE-3                    PIC 9(10).
           03  QBW-CONTACT-EXT-3                      PIC 9(4).
           03  QBW-CONTACT-RELATION-3                 PIC X(12).
           03  QBW-CONTACT-DESC-4                     PIC X(46).
           03  QBW-CONTACT-PHONE-4                    PIC 9(10).
           03  QBW-CONTACT-EXT-4                      PIC 9(4).
           03  QBW-CONTACT-RELATION-4                 PIC X(12).
           03  QBW-CONTACT-DESC-5                     PIC X(46).
           03  QBW-CONTACT-PHONE-5                    PIC 9(10).
           03  QBW-CONTACT-EXT-5                      PIC 9(4).
           03  QBW-CONTACT-RELATION-5                 PIC X(12).
           03  QBW-CONTACT-DESC-6                     PIC X(46).
           03  QBW-CONTACT-PHONE-6                    PIC 9(10).
           03  QBW-CONTACT-EXT-6                      PIC 9(4).
           03  QBW-CONTACT-RELATION-6                 PIC X(12).
           03  QBW-CONTACT-DESC-7                     PIC X(46).
           03  QBW-CONTACT-PHONE-7                    PIC 9(10).
           03  QBW-CONTACT-EXT-7                      PIC 9(4).
           03  QBW-CONTACT-RELATION-7                 PIC X(12).
           03  QBW-CONTACT-DESC-8                     PIC X(46).
           03  QBW-CONTACT-PHONE-8                    PIC 9(10).
           03  QBW-CONTACT-EXT-8                      PIC 9(4).
           03  QBW-CONTACT-RELATION-8                 PIC X(12).
           03  QBW-CONTACT-DESC-9                     PIC X(46).
           03  QBW-CONTACT-PHONE-9                    PIC 9(10).
           03  QBW-CONTACT-EXT-9                      PIC 9(4).
           03  QBW-CONTACT-RELATION-9                 PIC X(12).
           03  QBW-CONTACT-DESC-10                    PIC X(46).
           03  QBW-CONTACT-PHONE-10                   PIC 9(10).
           03  QBW-CONTACT-EXT-10                     PIC 9(4).
           03  QBW-CONTACT-RELATION-10                PIC X(12).
           03  QBW-SRCD                               PIC XX.
           03  QBW-CALL-ON-SUNDAY                     PIC X.
           03  QBW-SOLICIT                            PIC X.
           03  QBW-CONFIDENTIAL                       PIC X.
           03  QBW-PRIVACY-OPT-OUT                    PIC X.
           03  QBW-OPT-IN-EMAIL                       PIC X.
           03  QBW-OPT-IN-TEXT                        PIC X.
           03  QBW-CITY-TAX-CD                        PIC X(3).
           03  QBW-DEALTRACK-CHG-DATE                 PIC X(10).
           03  QBW-DECEASED                           PIC X.
           03  QBW-DECEASED-DATE                      PIC X(10).
           03  QBW-EMAIL                              PIC X(40).
           03  QBW-IDENT-NO                           PIC X(18).
           03  QBW-IDENT-TYPE                         PIC XX.
           03  QBW-IDENT-STATE                        PIC XX.
           03  QBW-MILITARY-FLAG                      PIC X.
           03  QBW-ORGBR                              PIC 9(4).

       01  QLN1-WHERE-BEG-KEY.
           03  QLN1-WBEG-OWNBR                       PIC 9(4).
           03  QLN1-WBEG-ACCTNO                      PIC 9(8).

       01  QLN1-WHERE-END-KEY.
           03  QLN1-WEND-OWNBR                       PIC 9(4).
           03  QLN1-WEND-ACCTNO                      PIC 9(8).

       01  QLN3-WHERE-BEG-KEY.
           03  QLN3-WBEG-OWNBR                       PIC 9(4).
           03  QLN3-WBEG-CLASS                       PIC 9(3).
           03  QLN3-WBEG-ACCTNO                      PIC 9(8).

       01  QLN3-WHERE-END-KEY.
           03  QLN3-WEND-OWNBR                       PIC 9(4).
           03  QLN3-WEND-CLASS                       PIC 9(3).
           03  QLN3-WEND-ACCTNO                      PIC 9(8).

       01  QLN4-WHERE-BEG-KEY.
           03  QLN4-WBEG-OWNBR                       PIC 9(4).
           03  QLN4-WBEG-SSNO1                       PIC 9(9).

       01  QLN4-WHERE-END-KEY.
           03  QLN4-WEND-OWNBR                       PIC 9(4).
           03  QLN4-WEND-SSNO1                       PIC 9(9).

       01  QLN5-WHERE-BEG-KEY.
           03  QLN5-WBEG-OWNBR                       PIC 9(4).
           03  QLN5-WBEG-SSNO2                       PIC 9(9).

       01  QLN5-WHERE-END-KEY.
           03  QLN5-WEND-OWNBR                       PIC 9(4).
           03  QLN5-WEND-SSNO2                       PIC 9(9).

      *01  QLN6-WHERE-BEG-KEY.
      *    03  QLN6-WBEG-OWNBR                       PIC 9(4).
      *    03  QLN6-WBEG-SSNO3                       PIC 9(9).

      *01  QLN6-WHERE-END-KEY.
      *    03  QLN6-WEND-OWNBR                       PIC 9(4).
      *    03  QLN6-WEND-SSNO3                       PIC 9(9).

      *01  QLN7-WHERE-BEG-KEY.
      *    03  QLN7-WBEG-OWNBR                       PIC 9(4).
      *    03  QLN7-WBEG-SSNO4                       PIC 9(9).

      *01  QLN7-WHERE-END-KEY.
      *    03  QLN7-WEND-OWNBR                       PIC 9(4).
      *    03  QLN7-WEND-SSNO4                       PIC 9(9).

       01  QLN-REC.
           03  QLN-OWNBR                             PIC 9(4).
           03  QLN-ACCTNO                            PIC 9(8).
           03  QLN-CLASS                             PIC 9(3).
           03  QLN-SUBCLASS                          PIC 99.
           03  QLN-LAWCODE                           PIC 99.
           03  QLN-SSNO1                             PIC 9(9).
           03  QLN-MAKERCD1                          PIC X.
           03  QLN-SSNO2                             PIC 9(9).
           03  QLN-MAKERCD2                          PIC X.
           03  QLN-SSNO3                             PIC 9(9).
           03  QLN-MAKERCD3                          PIC X.
           03  QLN-SSNO4                             PIC 9(9).
           03  QLN-MAKERCD4                          PIC X.
           03  QLN-ORGST                             PIC XX.
           03  QLN-ORGBR                             PIC 9(4).
           03  QLN-OWNST                             PIC XX.
           03  QLN-SPRCLASS                          PIC 9(3).
           03  QLN-CONVERCD                          PIC X.
           03  QLN-MIDSTREAM                         PIC X.
           03  QLN-PURCD                             PIC XXX.
           03  QLN-SRCD                              PIC XX.
           03  QLN-COLLCD                            PIC X(2).
           03  QLN-COLLID                            PIC X(20).
           03  QLN-POCD                              PIC XX.
           03  QLN-PLCD                              PIC X.
           03  QLN-PLTYPE                            PIC X.
           03  QLN-CONFIDENTCD                       PIC X.
           03  QLN-ALLOTCD                           PIC X.
           03  QLN-RECASTCD                          PIC X.
           03  QLN-SKIP-TRW                          PIC X.
           03  QLN-STATUSFG                          PIC X.
           03  QLN-SMALLOAN                          PIC X.
           03  QLN-SALEFIN                           PIC X.
           03  QLN-SERVACCT                          PIC X.
           03  QLN-LOANTYPE                          PIC X.
           03  QLN-ADDON-FG                          PIC X.
           03  QLN-PAY-SCHLD-FG                      PIC X.
           03  QLN-LC-CODE                           PIC 99.
           03  QLN-CAN-LOAN-NO                       PIC XX.
           03  QLN-STATFLAGS                         PIC X(20).
           03  QLN-ENTDATE                           PIC X(10).
           03  QLN-LOANDATE                          PIC X(10).
           03  QLN-PURDATE                           PIC X(10).
           03  QLN-INTDATE                           PIC X(10).
           03  QLN-CASHDATE                          PIC X(10).
           03  QLN-FILEDATE                          PIC X(10).
           03  QLN-RESCISSION-DATE                   PIC X(10).
           03  QLN-PART-RECOURSE-DATE                PIC X(10).
           03  QLN-ORIG-1STPYDATE                    PIC X(10).
           03  QLN-1STPYDATE                         PIC X(10).
           03  QLN-JDDATE                            PIC X(10).
           03  QLN-PLDATE                            PIC X(10).
           03  QLN-PL-TRDATE                         PIC X(10).
           03  QLN-ACCELDATE                         PIC X(10).
           03  QLN-REDUDATE                          PIC X(10).
           03  QLN-POFFDATE                          PIC X(10).
           03  QLN-PO-TRDATE                         PIC X(10).
           03  QLN-LEGALDATE                         PIC X(10).
           03  QLN-BNKRPTDATE                        PIC X(10).
           03  QLN-BNKRPT-DISMISS-DATE               PIC X(10).
           03  QLN-BNKRPT-DISCHARGE-DATE             PIC X(10).
           03  QLN-BNKRPT-REAFFIRM-DATE              PIC X(10).
           03  QLN-DEBTAIDATE                        PIC X(10).
           03  QLN-REPODATE                          PIC X(10).
           03  QLN-REDEEMDATE                        PIC X(10).
           03  QLN-SOLDDATE                          PIC X(10).
           03  QLN-INS-CONFIRM-DATE                  PIC X(10).
           03  QLN-PRIOR-RN-FOR10-DATE               PIC X(10).
           03  QLN-LAST-RN-FOR10-DATE                PIC X(10).
           03  QLN-TRW-DOFD-DATE                     PIC X(10).
           03  QLN-LAST-PYPA-DATE                    PIC X(10).
           03  QLN-BT-TRDATE                         PIC X(10).
           03  QLN-METRO2-LAST-PMT-DATE              PIC X(10).
           03  QLN-FOLLOWUP-DATE                     PIC X(10).
           03  QLN-LAST-ACCRUAL-DATE                 PIC X(10).
           03  QLN-APRATE                            PIC 999V999.
           03  QLN-EFFRATE                           PIC 999V999.
           03  QLN-SMPRATE                           PIC 999V999.
           03  QLN-YLDRATE                           PIC 999V999.
           03  QLN-OVRDRATE                          PIC 999V999.
           03  QLN-ENDORATE                          PIC 999V999.
           03  QLN-JDRATE                            PIC 999V999.
           03  QLN-ACCRATE                           PIC 999V999.
           03  QLN-REDURATE                          PIC 999V999.
           03  QLN-CONRATE                           PIC 999V999.
           03  QLN-SERVPER                           PIC 9(3)V99.
           03  QLN-SELLPR                            PIC 9(7)V99.
           03  QLN-DOWNPY                            PIC 9(7)V99.
           03  QLN-TRADEIN                           PIC S9(7)V99.
           03  QLN-LNAMT                             PIC 9(7)V99.
           03  QLN-FINCHG                            PIC 9(7)V99.
           03  QLN-INTCHG                            PIC 9(7)V99.
           03  QLN-SERCHG                            PIC 9(5)V99.
           03  QLN-EXTCHG                            PIC 9(5)V99.
           03  QLN-MAINTFEE                          PIC 9(5)V99.
           03  QLN-POINTS                            PIC 99V99.
           03  QLN-PROCEEDS                          PIC S9(7)V99.
           03  QLN-ORGTERM                           PIC 9(3).
           03  QLN-1STPYAMT                          PIC 9(7)V99.
           03  QLN-NOREGPY                           PIC 9(3).
           03  QLN-REGPYAMT                          PIC 9(7)V99 .
           03  QLN-LASTPYAMT                         PIC S9(7)V99.
           03  QLN-PROCASH                           PIC S9(7)V99.
           03  QLN-TILLNO                            PIC 9(2).
           03  QLN-STATE-FLAG                        PIC X.
           03  QLN-STATE-WORKER                      PIC S9(7)V99.
           03  QLN-STATE-WORKER2                     PIC S9(5)V99.
           03  QLN-FEECODE-1                         PIC XX.
           03  QLN-FEEAMT-1                          PIC 9(5)V99.
           03  QLN-FEEOURS-1                         PIC X.
           03  QLN-FEEFINCHG-1                       PIC X.
           03  QLN-FEECODE-2                         PIC XX.
           03  QLN-FEEAMT-2                          PIC 9(5)V99.
           03  QLN-FEEOURS-2                         PIC X.
           03  QLN-FEEFINCHG-2                       PIC X.
           03  QLN-FEECODE-3                         PIC XX.
           03  QLN-FEEAMT-3                          PIC 9(5)V99.
           03  QLN-FEEOURS-3                         PIC X.
           03  QLN-FEEFINCHG-3                       PIC X.
           03  QLN-FEECODE-4                         PIC XX.
           03  QLN-FEEAMT-4                          PIC 9(5)V99.
           03  QLN-FEEOURS-4                         PIC X.
           03  QLN-FEEFINCHG-4                       PIC X.
           03  QLN-FEECODE-5                         PIC XX.
           03  QLN-FEEAMT-5                          PIC 9(5)V99.
           03  QLN-FEEOURS-5                         PIC X.
           03  QLN-FEEFINCHG-5                       PIC X.
           03  QLN-FEECODE-6                         PIC XX.
           03  QLN-FEEAMT-6                          PIC 9(5)V99.
           03  QLN-FEEOURS-6                         PIC X.
           03  QLN-FEEFINCHG-6                       PIC X.
           03  QLN-FEECODE-7                         PIC XX.
           03  QLN-FEEAMT-7                          PIC 9(5)V99.
           03  QLN-FEEOURS-7                         PIC X.
           03  QLN-FEEFINCHG-7                       PIC X.
           03  QLN-FEECODE-8                         PIC XX.
           03  QLN-FEEAMT-8                          PIC 9(5)V99.
           03  QLN-FEEOURS-8                         PIC X.
           03  QLN-FEEFINCHG-8                       PIC X.
           03  QLN-FEECODE-9                         PIC XX.
           03  QLN-FEEAMT-9                          PIC 9(5)V99.
           03  QLN-FEEOURS-9                         PIC X.
           03  QLN-FEEFINCHG-9                       PIC X.
           03  QLN-FEECODE-10                        PIC XX.
           03  QLN-FEEAMT-10                         PIC 9(5)V99.
           03  QLN-FEEOURS-10                        PIC X.
           03  QLN-FEEFINCHG-10                      PIC X.
           03  QLN-FEECODE-11                        PIC XX.
           03  QLN-FEEAMT-11                         PIC 9(5)V99.
           03  QLN-FEEOURS-11                        PIC X.
           03  QLN-FEEFINCHG-11                      PIC X.
           03  QLN-FEECODE-12                        PIC XX.
           03  QLN-FEEAMT-12                         PIC 9(5)V99.
           03  QLN-FEEOURS-12                        PIC X.
           03  QLN-FEEFINCHG-12                      PIC X.
           03  QLN-FEECODE-13                        PIC XX.
           03  QLN-FEEAMT-13                         PIC 9(5)V99.
           03  QLN-FEEOURS-13                        PIC X.
           03  QLN-FEEFINCHG-13                      PIC X.
           03  QLN-FEECODE-14                        PIC XX.
           03  QLN-FEEAMT-14                         PIC 9(5)V99.
           03  QLN-FEEOURS-14                        PIC X.
           03  QLN-FEEFINCHG-14                      PIC X.
           03  QLN-FEECODE-15                        PIC XX.
           03  QLN-FEEAMT-15                         PIC 9(5)V99.
           03  QLN-FEEOURS-15                        PIC X.
           03  QLN-FEEFINCHG-15                      PIC X.
           03  QLN-FEECODE-16                        PIC XX.
           03  QLN-FEEAMT-16                         PIC 9(5)V99.
           03  QLN-FEEOURS-16                        PIC X.
           03  QLN-FEEFINCHG-16                      PIC X.
           03  QLN-FEECODE-17                        PIC XX.
           03  QLN-FEEAMT-17                         PIC 9(5)V99.
           03  QLN-FEEOURS-17                        PIC X.
           03  QLN-FEEFINCHG-17                      PIC X.
           03  QLN-FEECODE-18                        PIC XX.
           03  QLN-FEEAMT-18                         PIC 9(5)V99.
           03  QLN-FEEOURS-18                        PIC X.
           03  QLN-FEEFINCHG-18                      PIC X.
           03  QLN-FEECODE-19                        PIC XX.
           03  QLN-FEEAMT-19                         PIC 9(5)V99.
           03  QLN-FEEOURS-19                        PIC X.
           03  QLN-FEEFINCHG-19                      PIC X.
           03  QLN-FEECODE-20                        PIC XX.
           03  QLN-FEEAMT-20                         PIC 9(5)V99.
           03  QLN-FEEOURS-20                        PIC X.
           03  QLN-FEEFINCHG-20                      PIC X.
           03  QLN-INSURED                           PIC X.
           03  QLN-INSTYPES                          PIC X.
           03  QLN-INSELIGIBLE                       PIC X.
           03  QLN-INSCOMP-1                         PIC 99.
           03  QLN-INSOURS-1                         PIC X.
           03  QLN-INSEFF-DATE-1                     PIC X(10).
           03  QLN-INSEXP-DATE-1                     PIC X(10).
           03  QLN-INSPREM-1                         PIC 9(5)V99.
           03  QLN-INSCOVR-1                         PIC 9(7)V99.
           03  QLN-INSCOMM-1                         PIC 9(5)V99.
           03  QLN-INSANTICERN-1                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-1                     PIC S9(7)V99.
           03  QLN-INSUFERN-1                        PIC S9(7)V99.
           03  QLN-INSACCERN-1                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-1                   PIC 99.
           03  QLN-INSFINCHG-1                       PIC X.
           03  QLN-INSCOMP-2                         PIC 99.
           03  QLN-INSOURS-2                         PIC X.
           03  QLN-INSEFF-DATE-2                     PIC X(10).
           03  QLN-INSEXP-DATE-2                     PIC X(10).
           03  QLN-INSPREM-2                         PIC 9(5)V99.
           03  QLN-INSCOVR-2                         PIC 9(7)V99.
           03  QLN-INSCOMM-2                         PIC 9(5)V99.
           03  QLN-INSANTICERN-2                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-2                     PIC S9(7)V99.
           03  QLN-INSUFERN-2                        PIC S9(7)V99.
           03  QLN-INSACCERN-2                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-2                   PIC 99.
           03  QLN-INSFINCHG-2                       PIC X.
           03  QLN-INSCOMP-3                         PIC 99.
           03  QLN-INSOURS-3                         PIC X.
           03  QLN-INSEFF-DATE-3                     PIC X(10).
           03  QLN-INSEXP-DATE-3                     PIC X(10).
           03  QLN-INSPREM-3                         PIC 9(5)V99.
           03  QLN-INSCOVR-3                         PIC 9(7)V99.
           03  QLN-INSCOMM-3                         PIC 9(5)V99.
           03  QLN-INSANTICERN-3                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-3                     PIC S9(7)V99.
           03  QLN-INSUFERN-3                        PIC S9(7)V99.
           03  QLN-INSACCERN-3                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-3                   PIC 99.
           03  QLN-INSFINCHG-3                       PIC X.
           03  QLN-INSCOMP-4                         PIC 99.
           03  QLN-INSOURS-4                         PIC X.
           03  QLN-INSEFF-DATE-4                     PIC X(10).
           03  QLN-INSEXP-DATE-4                     PIC X(10).
           03  QLN-INSPREM-4                         PIC 9(5)V99.
           03  QLN-INSCOVR-4                         PIC 9(7)V99.
           03  QLN-INSCOMM-4                         PIC 9(5)V99.
           03  QLN-INSANTICERN-4                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-4                     PIC S9(7)V99.
           03  QLN-INSUFERN-4                        PIC S9(7)V99.
           03  QLN-INSACCERN-4                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-4                   PIC 99.
           03  QLN-INSFINCHG-4                       PIC X.
           03  QLN-INSCOMP-5                         PIC 99.
           03  QLN-INSOURS-5                         PIC X.
           03  QLN-INSEFF-DATE-5                     PIC X(10).
           03  QLN-INSEXP-DATE-5                     PIC X(10).
           03  QLN-INSPREM-5                         PIC 9(5)V99.
           03  QLN-INSCOVR-5                         PIC 9(7)V99.
           03  QLN-INSCOMM-5                         PIC 9(5)V99.
           03  QLN-INSANTICERN-5                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-5                     PIC S9(7)V99.
           03  QLN-INSUFERN-5                        PIC S9(7)V99.
           03  QLN-INSACCERN-5                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-5                   PIC 99.
           03  QLN-INSFINCHG-5                       PIC X.
           03  QLN-INSCOMP-6                         PIC 99.
           03  QLN-INSOURS-6                         PIC X.
           03  QLN-INSEFF-DATE-6                     PIC X(10).
           03  QLN-INSEXP-DATE-6                     PIC X(10).
           03  QLN-INSPREM-6                         PIC 9(5)V99.
           03  QLN-INSCOVR-6                         PIC 9(7)V99.
           03  QLN-INSCOMM-6                         PIC 9(5)V99.
           03  QLN-INSANTICERN-6                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-6                     PIC S9(7)V99.
           03  QLN-INSUFERN-6                        PIC S9(7)V99.
           03  QLN-INSACCERN-6                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-6                   PIC 99.
           03  QLN-INSFINCHG-6                       PIC X.
           03  QLN-INSCOMP-7                         PIC 99.
           03  QLN-INSOURS-7                         PIC X.
           03  QLN-INSEFF-DATE-7                     PIC X(10).
           03  QLN-INSEXP-DATE-7                     PIC X(10).
           03  QLN-INSPREM-7                         PIC 9(5)V99.
           03  QLN-INSCOVR-7                         PIC 9(7)V99.
           03  QLN-INSCOMM-7                         PIC 9(5)V99.
           03  QLN-INSANTICERN-7                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-7                     PIC S9(7)V99.
           03  QLN-INSUFERN-7                        PIC S9(7)V99.
           03  QLN-INSACCERN-7                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-7                   PIC 99.
           03  QLN-INSFINCHG-7                       PIC X.
           03  QLN-INSCOMP-8                         PIC 99.
           03  QLN-INSOURS-8                         PIC X.
           03  QLN-INSEFF-DATE-8                     PIC X(10).
           03  QLN-INSEXP-DATE-8                     PIC X(10).
           03  QLN-INSPREM-8                         PIC 9(5)V99.
           03  QLN-INSCOVR-8                         PIC 9(7)V99.
           03  QLN-INSCOMM-8                         PIC 9(5)V99.
           03  QLN-INSANTICERN-8                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-8                     PIC S9(7)V99.
           03  QLN-INSUFERN-8                        PIC S9(7)V99.
           03  QLN-INSACCERN-8                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-8                   PIC 99.
           03  QLN-INSFINCHG-8                       PIC X.
           03  QLN-INSCOMP-9                         PIC 99.
           03  QLN-INSOURS-9                         PIC X.
           03  QLN-INSEFF-DATE-9                     PIC X(10).
           03  QLN-INSEXP-DATE-9                     PIC X(10).
           03  QLN-INSPREM-9                         PIC 9(5)V99.
           03  QLN-INSCOVR-9                         PIC 9(7)V99.
           03  QLN-INSCOMM-9                         PIC 9(5)V99.
           03  QLN-INSANTICERN-9                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-9                     PIC S9(7)V99.
           03  QLN-INSUFERN-9                        PIC S9(7)V99.
           03  QLN-INSACCERN-9                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-9                   PIC 99.
           03  QLN-INSFINCHG-9                       PIC X.
           03  QLN-INSCOMP-10                        PIC 99.
           03  QLN-INSOURS-10                        PIC X.
           03  QLN-INSEFF-DATE-10                    PIC X(10).
           03  QLN-INSEXP-DATE-10                    PIC X(10).
           03  QLN-INSPREM-10                        PIC 9(5)V99.
           03  QLN-INSCOVR-10                        PIC 9(7)V99.
           03  QLN-INSCOMM-10                        PIC 9(5)V99.
           03  QLN-INSANTICERN-10                    PIC S9(7)V99.
           03  QLN-INSANTICADJ-10                    PIC S9(7)V99.
           03  QLN-INSUFERN-10                       PIC S9(7)V99.
           03  QLN-INSACCERN-10                      PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-10                  PIC 99.
           03  QLN-INSFINCHG-10                      PIC X.
           03  QLN-PY-YYYYMM-RECDEL                  PIC 9(6).
           03  QLN-MTD-PAYS-RECDEL                   PIC S9(7)V99.
           03  QLN-GARNISH-EXTR                      PIC X.
           03  QLN-SHIP-SAME-FG                      PIC X.
           03  QLN-AD-D-INS-COVR                     PIC 9(7).
           03  QLN-TOT-PARTIAL-CHGOFF                PIC S9(7)V99.
           03  QLN-LPO                               PIC X(4).
           03  QLN-REP-CD                            PIC X(8).
           03  QLN-EMP-ID                            PIC 9(8).
           03  QLN-PROCESS-CD                        PIC X.
           03  QLN-WCBC-CREDITLIM                    PIC S9(5).
           03  QLN-FOLLOWUP-CD                       PIC X(4).
           03  QLN-MIP-FG                            PIC X.
           03  QLN-MIP-MTD-CLAHPAID                  PIC S9(5)V99.
           03  QLN-MIP-CLPDTH-DATE                   PIC X(10).
           03  QLN-MIP-TOT-CLPAID                    PIC S9(7)V99.
           03  QLN-MIP-AHPDTH-DATE                   PIC X(10).
           03  QLN-MIP-TOT-AHPAID                    PIC S9(7)V99.
           03  QLN-IL-GROSS-INCOME                   PIC S9(7)V99.
           03  QLN-IL-GROSS-INCOME2                  PIC S9(7)V99.
           03  QLN-RESCISSION                        PIC X(1).
           03  QLN-INTERIM-TRW                       PIC X(1).
           03  QLN-DLNO                              PIC 9(4)V99.
           03  QLN-DLPOOL                            PIC 9(5).
           03  QLN-DLRECOURSE                        PIC X.
           03  QLN-DLPARTRCAMT                       PIC 9(7)V99.
           03  QLN-ORIGPURAMT                        PIC 9(7)V99.
           03  QLN-PURBAL                            PIC S9(7)V99.
           03  QLN-DLRVPRIN-1                        PIC 9(7)V99.
           03  QLN-DLRVPRIN-2                        PIC 9(7)V99.
           03  QLN-DLRVPRIN-3                        PIC 9(7)V99.
           03  QLN-DLRVCHGS-1                        PIC 9(7)V99.
           03  QLN-DLRVCHGS-2                        PIC 9(7)V99.
           03  QLN-DLRVCHGS-3                        PIC 9(7)V99.
           03  QLN-DLPARVHELD                        PIC 9(7)V99.
           03  QLN-DLPARVPAID                        PIC 9(7)V99.
           03  QLN-DLDISC                            PIC 9(7)V99.
           03  QLN-LCPDTH-DATE                       PIC X(10).
           03  QLN-INTPDTH-DATE                      PIC X(10).
           03  QLN-CURBAL                            PIC S9(7)V99.
           03  QLN-INTBAL                            PIC S9(7)V99.
           03  QLN-LCBAL                             PIC S9(7)V99.
           03  QLN-OTHBAL                            PIC S9(7)V99.
           03  QLN-OT2BAL                            PIC S9(7)V99.
           03  QLN-LCPAID                            PIC S9(7)V99.
           03  QLN-LCPARTIALS                        PIC S9(7)V99.
           03  QLN-STMT-COUNTER                      PIC 9(5).
           03  QLN-PRIOR-STMT-COUNTER                PIC 9(5).
           03  QLN-FROZEN-REASON                     PIC X(2).
           03  QLN-COUPON-STATEMENT                  PIC X.
           03  QLN-MONTHEND-BAL                      PIC 9(7)V99.
           03  QLN-ANTICTERM                         PIC 9(3).
           03  QLN-ANTICERN-1                        PIC S9(7)V99.
           03  QLN-ANTICADJ-1                        PIC S9(7)V99.
           03  QLN-UFERN-1                           PIC S9(7)V99.
           03  QLN-ACCERN-1                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-1                      PIC 99.
           03  QLN-ANTICERN-2                        PIC S9(7)V99.
           03  QLN-ANTICADJ-2                        PIC S9(7)V99.
           03  QLN-UFERN-2                           PIC S9(7)V99.
           03  QLN-ACCERN-2                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-2                      PIC 99.
           03  QLN-ANTICERN-3                        PIC S9(7)V99.
           03  QLN-ANTICADJ-3                        PIC S9(7)V99.
           03  QLN-UFERN-3                           PIC S9(7)V99.
           03  QLN-ACCERN-3                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-3                      PIC 99.
           03  QLN-ANTICERN-4                        PIC S9(7)V99.
           03  QLN-ANTICADJ-4                        PIC S9(7)V99.
           03  QLN-UFERN-4                           PIC S9(7)V99.
           03  QLN-ACCERN-4                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-4                      PIC 99.
           03  QLN-ANTICERN-5                        PIC S9(7)V99.
           03  QLN-ANTICADJ-5                        PIC S9(7)V99.
           03  QLN-UFERN-5                           PIC S9(7)V99.
           03  QLN-ACCERN-5                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-5                      PIC 99.
           03  QLN-ANTICERN-6                        PIC S9(7)V99.
           03  QLN-ANTICADJ-6                        PIC S9(7)V99.
           03  QLN-UFERN-6                           PIC S9(7)V99.
           03  QLN-ACCERN-6                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-6                      PIC 99.
           03  QLN-ANTICERN-7                        PIC S9(7)V99.
           03  QLN-ANTICADJ-7                        PIC S9(7)V99.
           03  QLN-UFERN-7                           PIC S9(7)V99.
           03  QLN-ACCERN-7                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-7                      PIC 99.
           03  QLN-ANTICERN-8                        PIC S9(7)V99.
           03  QLN-ANTICADJ-8                        PIC S9(7)V99.
           03  QLN-UFERN-8                           PIC S9(7)V99.
           03  QLN-ACCERN-8                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-8                      PIC 99.
           03  QLN-ANTICERN-9                        PIC S9(7)V99.
           03  QLN-ANTICADJ-9                        PIC S9(7)V99.
           03  QLN-UFERN-9                           PIC S9(7)V99.
           03  QLN-ACCERN-9                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-9                      PIC 99.
           03  QLN-ANTICERN-10                       PIC S9(7)V99.
           03  QLN-ANTICADJ-10                       PIC S9(7)V99.
           03  QLN-UFERN-10                          PIC S9(7)V99.
           03  QLN-ACCERN-10                         PIC S9(7)V99.
           03  QLN-EARN-ORDER-10                     PIC 99.
           03  QLN-LMO-PRIOR-INTEREST                PIC S9(7)V99.
           03  QLN-PRIOR-UNEARNED                    PIC S9(7)V99.
           03  QLN-ERNDELQ-STOP                      PIC X.
           03  QLN-NONACRERN                         PIC S9(7)V99.
           03  QLN-PRIOR-NONACRERN                   PIC S9(7)V99.
           03  QLN-ERN-IB-PDTH-DATE                  PIC X(10).
           03  QLN-ERN-IB-PRINBAL                    PIC S9(7)V99.
           03  QLN-ERN-IB-INTOWE                     PIC S9(7)V99.
           03  QLN-TOTERN                            PIC S9(7)V99.
           03  QLN-TOTOTH                            PIC S9(7)V99.
           03  QLN-TOTINT                            PIC S9(7)V99.
           03  QLN-TOTLCHG                           PIC S9(7)V99.
           03  QLN-TOTDEF                            PIC S9(7)V99.
           03  QLN-TOTSERACCT                        PIC S9(7)V99.
           03  QLN-LCWAIVERS                         PIC S9(5)V99.
           03  QLN-INTWAIVERS                        PIC S9(5)V99.
           03  QLN-TOTPAYMNTD                        PIC S9(7)V99.
           03  QLN-TOTEXCPAYMNTD                     PIC S9(7)V99.
           03  QLN-TOTNODEF                          PIC S99.
           03  QLN-YTDNODEF                          PIC S99.
           03  QLN-TOTNODEFACT                       PIC S99.
           03  QLN-YTDNODEFACT                       PIC S99.
           03  QLN-TOTNOALDEL                        PIC S99.
           03  QLN-YTDNOALDEL                        PIC S99.
           03  QLN-YTDINTPD                          PIC S9(7)V99.
           03  QLN-LYRINTPD                          PIC S9(7)V99.
           03  QLN-TOTINTPD                          PIC S9(7)V99.
           03  QLN-RECASTINT                         PIC 9(7)V99.
           03  QLN-RECASTPRIN                        PIC 9(7)V99.
           03  QLN-CL-REBATE                         PIC X.
           03  QLN-AH-REBATE                         PIC X.
           03  QLN-PP-REBATE                         PIC X.
           03  QLN-INT-REBATE                        PIC X.
           03  QLN-SC-REBATE                         PIC X.
           03  QLN-MF-REBATE                         PIC X.
           03  QLN-DF-REBATE                         PIC X.
           03  QLN-EX-REBATE                         PIC X.
           03  QLN-O1-REBATE                         PIC X.
           03  QLN-O2-REBATE                         PIC X.
           03  QLN-O3-REBATE                         PIC X.
           03  QLN-O4-REBATE                         PIC X.
           03  QLN-O5-REBATE                         PIC X.
           03  QLN-O6-REBATE                         PIC X.
           03  QLN-O7-REBATE                         PIC X.
           03  QLN-UN-O7-REB                         PIC X.
           03  QLN-UN-O8-REB                         PIC X.
           03  QLN-UN-O9-REB                         PIC X.
           03  QLN-UN-O10-REB                        PIC X.
           03  QLN-PLREASON                          PIC XX.
           03  QLN-COLLECTOR                         PIC X(3).
           03  QLN-ACTIONCD                          PIC X(4).
           03  QLN-ACTIONCD2                         PIC X(4).
           03  QLN-ATCODE                            PIC X(3).
           03  QLN-RECODE                            PIC X(3).
           03  QLN-ESCROW-FG                         PIC X.
           03  QLN-PROMDAYS-PASTDUE                  PIC S9(3).
           03  QLN-PROMID                            PIC X(3).
           03  QLN-PROMDATE                          PIC X(10).
           03  QLN-PROMAMT                           PIC 9(7)V99.
           03  QLN-PROMDATE2                         PIC X(10).
           03  QLN-DATE-PAID-LAST                    PIC X(10).
           03  QLN-AMT-PAID-LAST                     PIC 9(7)V99.
           03  QLN-LASTCFORMNO                       PIC 9(5).
           03  QLN-LASTCFORMDATE                     PIC X(10).
           03  QLN-DELCNT                            PIC 99.
           03  QLN-DELCNT2                           PIC 99.
           03  QLN-CREDITLIM                         PIC 9(7).
           03  QLN-CRSCORE                           PIC S9(3).
           03  QLN-LASTSFORMNO                       PIC 9(5).
           03  QLN-LASTSFORMDATE                     PIC X(10).
           03  QLN-SOLCNT                            PIC 99.
           03  QLN-MESSAGE                           PIC X(30).
           03  QLN-OLDACCTNO                         PIC 9(8).
           03  QLN-PRLNNO                            PIC 9(8).
           03  QLN-PRBAL                             PIC S9(7)V99.
           03  QLN-PRINTDUE                          PIC S9(7)V99.
           03  QLN-PRFINCHGRB                        PIC S9(7)V99.
           03  QLN-PRINSRB                           PIC S9(7)V99.
           03  QLN-PRPOCD                            PIC XX.
           03  QLN-PRPODATE                          PIC X(10).
           03  QLN-PRDELINQAMT                       PIC S9(7)V99.
           03  QLN-PRLNAMT                           PIC 9(7)V99.
           03  QLN-PRPDUE-DAYS                       PIC S9(5).
           03  QLN-PRDLDISC-UNEARN                   PIC S9(7)V99.
           03  QLN-PRCONPROFILE                      PIC 9(12).
           03  QLN-SUM-ORIGPURAMT                    PIC 9(7)V99.
           03  QLN-SUM-LNAMT                         PIC 9(7)V99.
           03  QLN-SUM-FINCHG                        PIC 9(7)V99.
           03  QLN-SUM-INTCHG                        PIC 9(7)V99.
           03  QLN-SUM-ORGTERM                       PIC 9(3).
           03  QLN-SUM-REGPYAMT                      PIC 9(7)V99.
           03  QLN-SUM-EXTCHG                        PIC 9(5)V99.
           03  QLN-SUM-SERCHG                        PIC 9(5)V99.
           03  QLN-SUM-MAINTFEE                      PIC 9(7)V99.
           03  QLN-UNITPER-CD                        PIC X.
           03  QLN-UNITPER-FREQ                      PIC 999.
           03  QLN-DAYS-IN-UNITPER                   PIC 9(3).
           03  QLN-UNITPER-PER-YEAR                  PIC 9(3)V9(12).
           03  QLN-REPOCD                            PIC X.
           03  QLN-BR-TFR-FG                         PIC X.
           03  QLN-O3INS-DLPROC                      PIC 9(7)V99.
           03  QLN-INSPREM-DLADJ                     PIC S9(5)V99.
           03  QLN-PLAMT                             PIC 9(7)V99.
           03  QLN-MTD-PRIN                          PIC S9(7)V99.
           03  QLN-MTD-LC                            PIC S9(5)V99.
           03  QLN-MTD-INT                           PIC S9(5)V99.
           03  QLN-MTD-OT                            PIC S9(5)V99.
           03  QLN-MTD-EX                            PIC S9(7)V99.
           03  QLN-BNKRPT-CHAPTER                    PIC 9(2).
           03  QLN-BMO-CON-STATUS                    PIC X.
           03  QLN-BMO-REC-STATUS                    PIC X.
           03  QLN-APP-NUMBER                        PIC 9(09).
           03  QLN-GIFTNET                           PIC X.
           03  QLN-GIFTNET-OUR-PO                    PIC 9(7).
           03  QLN-PROC-DISBURSED                    PIC X.
           03  QLN-NOODDPY                           PIC X.
           03  QLN-CONPROFILE-1                      PIC 9(12).
           03  QLN-CONPROFILE-2                      PIC 9(12).
           03  QLN-CONPROFILE-3                      PIC 9(12).
           03  QLN-CONPROFILE-4                      PIC 9(12).
           03  QLN-CONPROFILE-5                      PIC 9(12).
           03  QLN-RECPROFILE-1                      PIC 9(12).
           03  QLN-RECPROFILE-2                      PIC 9(12).
           03  QLN-RECPROFILE-3                      PIC 9(12).
           03  QLN-RECPROFILE-4                      PIC 9(12).
           03  QLN-RECPROFILE-5                      PIC 9(12).
           03  QLN-TRW-STATUS                        PIC XX.
           03  QLN-ORIGIN                            PIC X(4).
           03  QLN-PO-TRCD                           PIC XX.
           03  QLN-NEXIS-CRSCORE                     PIC 9(3).
           03  QLN-TRW-B-CONSUMER-IND                PIC X(2).
           03  QLN-PARVHELD-PAID-FG                  PIC X.
           03  QLN-ACCT-FROZEN                       PIC X.
           03  QLN-TRW-SPECIAL-CODE                  PIC X(2).
           03  QLN-CREATION-STATUS                   PIC X(1).
           03  QLN-TOTNOLCHG                         PIC S9(3).
           03  QLN-LBOX                              PIC X(1).
           03  QLN-EXTRA-PRIN-LFP                    PIC S9(7)V99.
           03  QLN-REMAIN-INT-SINCE-LFP              PIC S9(7)V99.
           03  QLN-REMAIN-PRIN-SINCE-LFP             PIC S9(7)V99.
           03  QLN-INTERIM-COUPON                    PIC X.
           03  QLN-DLCOMM-O3-CMSI                    PIC 9(5)V99.
           03  QLN-DLCOMM-PP-CMSI                    PIC 9(5)V99.
           03  QLN-TRW-J-CONSUMER-IND                PIC X(2).
           03  QLN-ACH                               PIC X.
           03  QLN-1099-C-FG                         PIC X.
           03  QLN-TRW-COMPLIANCE-CODE               PIC XX.
           03  QLN-UNWIND-REASON-CODE                PIC X(2).
           03  QLN-PBI-PBD-FG                        PIC X.
           03  QLN-OLD-ACCT-FORMAT-FG                PIC X.
           03  QLN-APPROVED-BY-USER                  PIC X(10).
           03  QLN-APPROVED-BY-CODE                  PIC X(3).
           03  QLN-APPROVED-BY-DATE                  PIC X(10).
           03  QLN-APPROVED-BY-TIME                  PIC 9(6).
           03  QLN-LTOUCH-DATE                       PIC X(10).
           03  QLN-LTOUCH-TIME                       PIC 9(6).
           03  QLN-BW-LNAME-1                        PIC X(35).
           03  QLN-BW-FNAME-1                        PIC X(35).
           03  QLN-BW-SPSSNO-1                       PIC 9(9).
           03  QLN-BW-SPLNAME-1                      PIC X(35).
           03  QLN-BW-SPFNAME-1                      PIC X(35).
           03  QLN-BW-REFDATE-1                      PIC X(10).
           03  QLN-BW-SOLICIT-1                      PIC X(1).
           03  QLN-BW-LNAME-2                        PIC X(35).
           03  QLN-BW-FNAME-2                        PIC X(35).
           03  QLN-BW-SPSSNO-2                       PIC 9(9).
           03  QLN-BW-SPLNAME-2                      PIC X(35).
           03  QLN-BW-SPFNAME-2                      PIC X(35).
           03  QLN-BW-REFDATE-2                      PIC X(10).
           03  QLN-BW-SOLICIT-2                      PIC X(1).
           03  QLN-BW-LNAME-3                        PIC X(35).
           03  QLN-BW-FNAME-3                        PIC X(35).
           03  QLN-BW-SPSSNO-3                       PIC 9(9).
           03  QLN-BW-SPLNAME-3                      PIC X(35).
           03  QLN-BW-SPFNAME-3                      PIC X(35).
           03  QLN-BW-REFDATE-3                      PIC X(10).
           03  QLN-BW-SOLICIT-3                      PIC X(1).
           03  QLN-BW-LNAME-4                        PIC X(35).
           03  QLN-BW-FNAME-4                        PIC X(35).
           03  QLN-BW-SPSSNO-4                       PIC 9(9).
           03  QLN-BW-SPLNAME-4                      PIC X(35).
           03  QLN-BW-SPFNAME-4                      PIC X(35).
           03  QLN-BW-REFDATE-4                      PIC X(10).
           03  QLN-BW-SOLICIT-4                      PIC X(1).

       01  QLC1-WHERE-BEG-KEY.
           03  QLC1-WBEG-CODE          PIC 9(03).

       01  QLC1-WHERE-END-KEY.
           03  QLC1-WEND-CODE          PIC 9(03).

       01  QLC-REC.
           03  QLC-CODE                PIC 9(3).
           03  QLC-LTOUCH-DATE         PIC X(10).
           03  QLC-DESC                PIC X(30).
           03  QLC-SMALLOAN            PIC X.
           03  QLC-SALEFIN             PIC X.
           03  QLC-SERVACCT            PIC X.
           03  QLC-LOANTYPE            PIC X.
           03  QLC-PROF1               PIC X.
           03  QLC-PROF2               PIC X.
           03  QLC-SKIP-TRW            PIC X.
           03  QLC-DISPLAY-ERN         PIC X.
           03  QLC-LCARDFG             PIC X.
           03  QLC-CALC-HI-CREDIT      PIC X.
           03  QLC-STOP-ON-DLNO        PIC X.
           03  QLC-SALES-FORCED-ENTRY  PIC X.
           03  QLC-AFP-CLASS           PIC X.
           03  QLC-CON-PDUE-DAYS       PIC X.
           03  QLC-REC-PDUE-DAYS       PIC X.
           03  QLC-GIFTNET             PIC X.
           03  QLC-APP-REQ-FOR-LOAN    PIC X.
           03  QLC-POOLID              PIC 9(5).
           03  QLC-USE-ALT-GL-FEE      PIC X.
           03  QLC-PROGRP-CLASS        PIC 9(3).
           03  QLC-ALLOW-CASH-PROCEEDS PIC X.

       01  QLTP1-WHERE-BEG-KEY.
           03  QLTP1-WBEG-BRANCH             PIC 9(04).
           03  QLTP1-WBEG-ACCTNO           PIC 9(08).
           03  QLTP1-WBEG-SEQNO            PIC 9(03).
 
       01  QLTP1-WHERE-END-KEY.
           03  QLTP1-WEND-BRANCH             PIC 9(04).
           03  QLTP1-WEND-ACCTNO           PIC 9(08).
           03  QLTP1-WEND-SEQNO            PIC 9(03).
 
       01  QLTP-REC.
           03  QLTP-BRNO              PIC 9(4).
           03  QLTP-ACCTNO            PIC 9(8).
           03  QLTP-SEQNO             PIC 999.
           03  QLTP-LTOUCH-DATE       PIC X(10).
           03  QLTP-SCHLD-TERM-1     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-1   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-2     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-2   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-3     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-3   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-4     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-4   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-5     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-5   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-6     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-6   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-7     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-7   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-8     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-8   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-9     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-9   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-10    PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-10  PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-11     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-11   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-12     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-12   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-13     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-13   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-14     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-14   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-15     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-15   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-16     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-16   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-17     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-17   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-18     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-18   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-19     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-19   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-20     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-20   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-21     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-21   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-22     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-22   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-23     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-23   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-24     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-24   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-25     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-25   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-26     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-26   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-27     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-27   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-28     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-28   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-29     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-29   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-30     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-30   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-31     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-31   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-32     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-32   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-33     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-33   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-34     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-34   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-35     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-35   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-36     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-36   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-37     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-37   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-38     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-38   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-39     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-39   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-40     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-40   PIC 9(6)V99.
           03  QLTP-SCHLD-TERM-41     PIC 9(4).
           03  QLTP-SCHLD-PAYMNT-41   PIC 9(6)V99.

       01  QSP1-WHERE-BEG-KEY.
           03  QSP1-WBEG-ORGST             PIC XX.
           03  QSP1-WBEG-SPRCLASS          PIC 999.
           03  QSP1-WBEG-SUBCLASS          PIC 99.
           03  QSP1-WBEG-LAWCODE           PIC 99.
 
       01  QSP1-WHERE-END-KEY.
           03  QSP1-WEND-ORGST             PIC XX.
           03  QSP1-WEND-SPRCLASS          PIC 999.
           03  QSP1-WEND-SUBCLASS          PIC 99.
           03  QSP1-WEND-LAWCODE           PIC 99.

       01  QSPA-REC.
           03  QSPA-ORGST                             PIC XX.
           03  QSPA-SPRCLASS                          PIC 999.
           03  QSPA-SUBCLASS                          PIC 99.
           03  QSPA-LAWCODE                           PIC 99.
           03  QSPA-LTOUCH-DATE                       PIC X(10).
           03  QSPA-DESC                              PIC X(15).
           03  QSPA-NOTES                             PIC X(60).
           03  QSPA-EFFDATE                           PIC X(10).
           03  QSPA-EXPDATE                           PIC X(10).
           03  QSPA-EXPD-MONTHS                       PIC 99.
           03  QSPA-EXPD-DAYS                         PIC 99.
           03  QSPA-EXPD-FRMLA                        PIC X.
           03  QSPA-EXPD-EXCEPT                       PIC X.
           03  QSPA-EXPR-FRMLA                        PIC X.
           03  QSPA-EXPR-REDU-FRMLA                   PIC X.
           03  QSPA-EXPR-RATE-1                       PIC 999V999.
           03  QSPA-EXPR-BREAK-1                      PIC 9(7)V99.
           03  QSPA-EXPR-YRTYPE-1                     PIC 9(4).
           03  QSPA-EXPR-INTMETH-1                    PIC X.
           03  QSPA-EXPR-RATE-2                       PIC 999V999.
           03  QSPA-EXPR-BREAK-2                      PIC 9(7)V99.
           03  QSPA-EXPR-YRTYPE-2                     PIC 9(4).
           03  QSPA-EXPR-INTMETH-2                    PIC X.
           03  QSPA-EXPR-RATE-3                       PIC 999V999.
           03  QSPA-EXPR-BREAK-3                      PIC 9(7)V99.
           03  QSPA-EXPR-YRTYPE-3                     PIC 9(4).
           03  QSPA-EXPR-INTMETH-3                    PIC X.
           03  QSPA-EXPR-RATE-4                       PIC 999V999.
           03  QSPA-EXPR-BREAK-4                      PIC 9(7)V99.
           03  QSPA-EXPR-YRTYPE-4                     PIC 9(4).
           03  QSPA-EXPR-INTMETH-4                    PIC X.
           03  QSPA-EXPR-RATE-5                       PIC 999V999.
           03  QSPA-EXPR-BREAK-5                      PIC 9(7)V99.
           03  QSPA-EXPR-YRTYPE-5                     PIC 9(4).
           03  QSPA-EXPR-INTMETH-5                    PIC X.
           03  QSPA-EXPR-LCHG                         PIC X.
           03  QSPA-JUDGFRMLA                         PIC X.
           03  QSPA-JUDGRATE                          PIC 999V999.
           03  QSPA-JUDGYRTYPE                        PIC 9(4).
           03  QSPA-JUDGINTMETH                       PIC X.
           03  QSPA-ACC-FRMLA                         PIC X.
           03  QSPA-ACC-YRTYPE                        PIC 9(4).
           03  QSPA-ACC-INTMETH                       PIC X.
           03  QSPA-ACC-RATE-1                        PIC 999V999.
           03  QSPA-ACC-BREAK-1                       PIC 9(7)V99.
           03  QSPA-ACC-RATE-2                        PIC 999V999.
           03  QSPA-ACC-BREAK-2                       PIC 9(7)V99.
           03  QSPA-ACC-RATE-3                        PIC 999V999.
           03  QSPA-ACC-BREAK-3                       PIC 9(7)V99.
           03  QSPA-ACC-RATE-4                        PIC 999V999.
           03  QSPA-ACC-BREAK-4                       PIC 9(7)V99.
           03  QSPA-ACC-RATE-5                        PIC 999V999.
           03  QSPA-ACC-BREAK-5                       PIC 9(7)V99.
           03  QSPA-1098FRMLA                         PIC X.
           03  QSPA-DISFRMLA                          PIC X.
           03  QSPA-CONTRFRMLA                        PIC X.
           03  QSPA-BANKRULE                          PIC X.
           03  QSPA-CONTPDTH                          PIC 9(4).
           03  QSPA-FINCHG                            PIC 99.
           03  QSPA-BANKUNITPER-CD                    PIC X.
           03  QSPA-TRWBANKRULE                       PIC X.
           03  QSPA-TRWCONTPDTH                       PIC 9(4).
           03  QSPA-TRWBANKUNITPER-CD                 PIC X.
           03  QSPA-RECFRMLA                          PIC X.
           03  QSPA-RECPAYCD                          PIC X.
           03  QSPA-RECPDTH                           PIC 9(3).
           03  QSPA-DISFRMLA-SPAY-FG                  PIC X.
           03  QSPA-ERNFRMLA-1                        PIC 99.
           03  QSPA-ERNAMT-1                          PIC 9(7)V99.
           03  QSPA-ERNPERCT-1                        PIC 9(3)V99.
           03  QSPA-ERNPERCD-1                        PIC X.
           03  QSPA-ERNACCRL-FG-1                     PIC X.
           03  QSPA-ERNACCRL-START-CD-1               PIC X.
           03  QSPA-ERNACCRL-DFCD-1                   PIC X.
           03  QSPA-ERN-DAYS-1                        PIC 99.
           03  QSPA-ERN-TERM-1                        PIC 999.
           03  QSPA-ERN-TYPE-1                        PIC X.
           03  QSPA-ERNFRMLA-2                        PIC 99.
           03  QSPA-ERNAMT-2                          PIC 9(7)V99.
           03  QSPA-ERNPERCT-2                        PIC 9(3)V99.
           03  QSPA-ERNPERCD-2                        PIC X.
           03  QSPA-ERNACCRL-FG-2                     PIC X.
           03  QSPA-ERNACCRL-START-CD-2               PIC X.
           03  QSPA-ERNACCRL-DFCD-2                   PIC X.
           03  QSPA-ERN-DAYS-2                        PIC 99.
           03  QSPA-ERN-TERM-2                        PIC 999.
           03  QSPA-ERN-TYPE-2                        PIC X.
           03  QSPA-ERNFRMLA-3                        PIC 99.
           03  QSPA-ERNAMT-3                          PIC 9(7)V99.
           03  QSPA-ERNPERCT-3                        PIC 9(3)V99.
           03  QSPA-ERNPERCD-3                        PIC X.
           03  QSPA-ERNACCRL-FG-3                     PIC X.
           03  QSPA-ERNACCRL-START-CD-3               PIC X.
           03  QSPA-ERNACCRL-DFCD-3                   PIC X.
           03  QSPA-ERN-DAYS-3                        PIC 99.
           03  QSPA-ERN-TERM-3                        PIC 999.
           03  QSPA-ERN-TYPE-3                        PIC X.
           03  QSPA-ERNFRMLA-4                        PIC 99.
           03  QSPA-ERNAMT-4                          PIC 9(7)V99.
           03  QSPA-ERNPERCT-4                        PIC 9(3)V99.
           03  QSPA-ERNPERCD-4                        PIC X.
           03  QSPA-ERNACCRL-FG-4                     PIC X.
           03  QSPA-ERNACCRL-START-CD-4               PIC X.
           03  QSPA-ERNACCRL-DFCD-4                   PIC X.
           03  QSPA-ERN-DAYS-4                        PIC 99.
           03  QSPA-ERN-TERM-4                        PIC 999.
           03  QSPA-ERN-TYPE-4                        PIC X.
           03  QSPA-ERNFRMLA-5                        PIC 99.
           03  QSPA-ERNAMT-5                          PIC 9(7)V99.
           03  QSPA-ERNPERCT-5                        PIC 9(3)V99.
           03  QSPA-ERNPERCD-5                        PIC X.
           03  QSPA-ERNACCRL-FG-5                     PIC X.
           03  QSPA-ERNACCRL-START-CD-5               PIC X.
           03  QSPA-ERNACCRL-DFCD-5                   PIC X.
           03  QSPA-ERN-DAYS-5                        PIC 99.
           03  QSPA-ERN-TERM-5                        PIC 999.
           03  QSPA-ERN-TYPE-5                        PIC X.
           03  QSPA-ERNFRMLA-6                        PIC 99.
           03  QSPA-ERNAMT-6                          PIC 9(7)V99.
           03  QSPA-ERNPERCT-6                        PIC 9(3)V99.
           03  QSPA-ERNPERCD-6                        PIC X.
           03  QSPA-ERNACCRL-FG-6                     PIC X.
           03  QSPA-ERNACCRL-START-CD-6               PIC X.
           03  QSPA-ERNACCRL-DFCD-6                   PIC X.
           03  QSPA-ERN-DAYS-6                        PIC 99.
           03  QSPA-ERN-TERM-6                        PIC 999.
           03  QSPA-ERN-TYPE-6                        PIC X.
           03  QSPA-ERNFRMLA-7                        PIC 99.
           03  QSPA-ERNAMT-7                          PIC 9(7)V99.
           03  QSPA-ERNPERCT-7                        PIC 9(3)V99.
           03  QSPA-ERNPERCD-7                        PIC X.
           03  QSPA-ERNACCRL-FG-7                     PIC X.
           03  QSPA-ERNACCRL-START-CD-7               PIC X.
           03  QSPA-ERNACCRL-DFCD-7                   PIC X.
           03  QSPA-ERN-DAYS-7                        PIC 99.
           03  QSPA-ERN-TERM-7                        PIC 999.
           03  QSPA-ERN-TYPE-7                        PIC X.
           03  QSPA-ERNFRMLA-8                        PIC 99.
           03  QSPA-ERNAMT-8                          PIC 9(7)V99.
           03  QSPA-ERNPERCT-8                        PIC 9(3)V99.
           03  QSPA-ERNPERCD-8                        PIC X.
           03  QSPA-ERNACCRL-FG-8                     PIC X.
           03  QSPA-ERNACCRL-START-CD-8               PIC X.
           03  QSPA-ERNACCRL-DFCD-8                   PIC X.
           03  QSPA-ERN-DAYS-8                        PIC 99.
           03  QSPA-ERN-TERM-8                        PIC 999.
           03  QSPA-ERN-TYPE-8                        PIC X.
           03  QSPA-ERNFRMLA-9                        PIC 99.
           03  QSPA-ERNAMT-9                          PIC 9(7)V99.
           03  QSPA-ERNPERCT-9                        PIC 9(3)V99.
           03  QSPA-ERNPERCD-9                        PIC X.
           03  QSPA-ERNACCRL-FG-9                     PIC X.
           03  QSPA-ERNACCRL-START-CD-9               PIC X.
           03  QSPA-ERNACCRL-DFCD-9                   PIC X.
           03  QSPA-ERN-DAYS-9                        PIC 99.
           03  QSPA-ERN-TERM-9                        PIC 999.
           03  QSPA-ERN-TYPE-9                        PIC X.
           03  QSPA-ERNFRMLA-10                       PIC 99.
           03  QSPA-ERNAMT-10                         PIC 9(7)V99.
           03  QSPA-ERNPERCT-10                       PIC 9(3)V99.
           03  QSPA-ERNPERCD-10                       PIC X.
           03  QSPA-ERNACCRL-FG-10                    PIC X.
           03  QSPA-ERNACCRL-START-CD-10              PIC X.
           03  QSPA-ERNACCRL-DFCD-10                  PIC X.
           03  QSPA-ERN-DAYS-10                       PIC 99.
           03  QSPA-ERN-TERM-10                       PIC 999.
           03  QSPA-ERN-TYPE-10                       PIC X.
           03  QSPA-ERNACCEL-CD                       PIC X.
           03  QSPA-ERNJD-CD                          PIC X.
           03  QSPA-MIDSTRERNCD                       PIC X.
           03  QSPA-EARN-ORDER-1                      PIC 99.
           03  QSPA-EARN-ORDER-2                      PIC 99.
           03  QSPA-EARN-ORDER-3                      PIC 99.
           03  QSPA-EARN-ORDER-4                      PIC 99.
           03  QSPA-EARN-ORDER-5                      PIC 99.
           03  QSPA-EARN-ORDER-6                      PIC 99.
           03  QSPA-EARN-ORDER-7                      PIC 99.
           03  QSPA-EARN-ORDER-8                      PIC 99.
           03  QSPA-EARN-ORDER-9                      PIC 99.
           03  QSPA-EARN-ORDER-10                     PIC 99.
           03  QSPA-ERNDELQFRMLA                      PIC X.
           03  QSPA-ERNDELQ-START                     PIC 999.
           03  QSPA-ERNDELQ-STOP                      PIC 999.
           03  QSPA-1098-INC-LC                       PIC X.
           03  QSPA-MAX-IB-ACR-DAYS                   PIC 999.
           03  QSPA-LCGRACE                           PIC 9(2).
           03  QSPA-LCGRACECD                         PIC X.
           03  QSPA-LCTYPE                            PIC X(2).
           03  QSPA-LCRATE                            PIC 9(4)V9(4).
           03  QSPA-LCFRMLA                           PIC X(2).
           03  QSPA-LCMINCHG                          PIC 99V99.
           03  QSPA-LCMAXAMT                          PIC 9(4)V99.
           03  QSPA-LCDELFAC                          PIC 9(3).
           03  QSPA-LCYRTYPE                          PIC 9(4).
           03  QSPA-LC-CODE                           PIC 99.
           03  QSPA-LCUNITPER-CD                      PIC X.
           03  QSPA-DEFFRMLA                          PIC 99.
           03  QSPA-DEFEXCEPT                         PIC X.
           03  QSPA-DEFWORKER                         PIC 99V9999.
           03  QSPA-DEFPERCD                          PIC 99.
           03  QSPA-DEFBASE                           PIC 99.
           03  QSPA-DEFPOLCD                          PIC X.
           03  QSPA-DEFPOLMX-1                        PIC 99.
           03  QSPA-DEFPOLMX-2                        PIC 99.
           03  QSPA-DEFPOLMX-3                        PIC 99.
           03  QSPA-DEFMIN                            PIC 99V99.
           03  QSPA-DEFMAX                            PIC 9999V99.
           03  QSPA-DEFUNITPER-CD                     PIC X.
           03  QSPA-PRFORM-1                          PIC 9(4).
           03  QSPA-PRFLAG-1                          PIC X.
           03  QSPA-PRFORM-2                          PIC 9(4).
           03  QSPA-PRFLAG-2                          PIC X.
           03  QSPA-PRFORM-3                          PIC 9(4).
           03  QSPA-PRFLAG-3                          PIC X.
           03  QSPA-PRFORM-4                          PIC 9(4).
           03  QSPA-PRFLAG-4                          PIC X.
           03  QSPA-PRFORM-5                          PIC 9(4).
           03  QSPA-PRFLAG-5                          PIC X.
           03  QSPA-PRFORM-6                          PIC 9(4).
           03  QSPA-PRFLAG-6                          PIC X.
           03  QSPA-PRFORM-7                          PIC 9(4).
           03  QSPA-PRFLAG-7                          PIC X.
           03  QSPA-PRFORM-8                          PIC 9(4).
           03  QSPA-PRFLAG-8                          PIC X.
           03  QSPA-PRFORM-9                          PIC 9(4).
           03  QSPA-PRFLAG-9                          PIC X.
           03  QSPA-PRFORM-10                         PIC 9(4).
           03  QSPA-PRFLAG-10                         PIC X.
           03  QSPA-PRFORM-11                         PIC 9(4).
           03  QSPA-PRFLAG-11                         PIC X.
           03  QSPA-PRFORM-12                         PIC 9(4).
           03  QSPA-PRFLAG-12                         PIC X.
           03  QSPA-PRFORM-13                         PIC 9(4).
           03  QSPA-PRFLAG-13                         PIC X.
           03  QSPA-PRFORM-14                         PIC 9(4).
           03  QSPA-PRFLAG-14                         PIC X.
           03  QSPA-PRFORM-15                         PIC 9(4).
           03  QSPA-PRFLAG-15                         PIC X.
           03  QSPA-PRFORM-16                         PIC 9(4).
           03  QSPA-PRFLAG-16                         PIC X.
           03  QSPA-PRFORM-17                         PIC 9(4).
           03  QSPA-PRFLAG-17                         PIC X.
           03  QSPA-PRFORM-18                         PIC 9(4).
           03  QSPA-PRFLAG-18                         PIC X.
           03  QSPA-PRFORM-19                         PIC 9(4).
           03  QSPA-PRFLAG-19                         PIC X.
           03  QSPA-PRFORM-20                         PIC 9(4).
           03  QSPA-PRFLAG-20                         PIC X.
           03  QSPA-IMED-REC                          PIC X.
           03  QSPA-SCFRMLA                           PIC 99.
           03  QSPA-SCRATE                            PIC 99V99.
           03  QSPA-SCMIN                             PIC 9(4)V99.
           03  QSPA-SCMAX                             PIC 9(6)V99.
           03  QSPA-MFFRMLA                           PIC 99.
           03  QSPA-MFRATE                            PIC 99V99.
           03  QSPA-MFMIN                             PIC 9(4)V99.
           03  QSPA-MFMAX                             PIC 9(5)V99.
           03  QSPA-INTMETHOD                         PIC X.
           03  QSPA-YEARTYPE                          PIC 9(4).
           03  QSPA-PYEXFRMLA                         PIC 99.
           03  QSPA-PYEXSPCODE                        PIC 99.
           03  QSPA-PYEXYEARTP                        PIC 9(4).
           03  QSPA-PYEXFMDATE                        PIC X.
           03  QSPA-PYEXBASECD                        PIC X.
           03  QSPA-PYEXBASE                          PIC 9(4).
           03  QSPA-PYEXDAYS                          PIC 9(4).
           03  QSPA-CAL-SPFRMLA                       PIC 99.
           03  QSPA-CAL-RATETYPE-1                    PIC X.
           03  QSPA-CAL-BREAKTYPE-1                   PIC X.
           03  QSPA-CAL-CHGABLE-1                     PIC X.
           03  QSPA-CAL-NORATES-1                     PIC S9.
           03  QSPA-CAL-TBFRMLA-1                     PIC 99.
           03  QSPA-CAL-RATE-1-1                      PIC 999V999.
           03  QSPA-CAL-BREAK-1-1                     PIC 9(6)V99.
           03  QSPA-CAL-RATE-1-2                      PIC 999V999.
           03  QSPA-CAL-BREAK-1-2                     PIC 9(6)V99.
           03  QSPA-CAL-RATE-1-3                      PIC 999V999.
           03  QSPA-CAL-BREAK-1-3                     PIC 9(6)V99.
           03  QSPA-CAL-RATE-1-4                      PIC 999V999.
           03  QSPA-CAL-BREAK-1-4                     PIC 9(6)V99.
           03  QSPA-CAL-RATE-1-5                      PIC 999V999.
           03  QSPA-CAL-BREAK-1-5                     PIC 9(6)V99.
           03  QSPA-CAL-RATETYPE-2                    PIC X.
           03  QSPA-CAL-BREAKTYPE-2                   PIC X.
           03  QSPA-CAL-CHGABLE-2                     PIC X.
           03  QSPA-CAL-NORATES-2                     PIC S9.
           03  QSPA-CAL-TBFRMLA-2                     PIC 99.
           03  QSPA-CAL-RATE-2-1                      PIC 999V999.
           03  QSPA-CAL-BREAK-2-1                     PIC 9(6)V99.
           03  QSPA-CAL-RATE-2-2                      PIC 999V999.
           03  QSPA-CAL-BREAK-2-2                     PIC 9(6)V99.
           03  QSPA-CAL-RATE-2-3                      PIC 999V999.
           03  QSPA-CAL-BREAK-2-3                     PIC 9(6)V99.
           03  QSPA-CAL-RATE-2-4                      PIC 999V999.
           03  QSPA-CAL-BREAK-2-4                     PIC 9(6)V99.
           03  QSPA-CAL-RATE-2-5                      PIC 999V999.
           03  QSPA-CAL-BREAK-2-5                     PIC 9(6)V99.
           03  QSPA-MINSF-FRMLA                       PIC 99.
           03  QSPA-MINSF-CHG                         PIC 9(6)V99.
           03  QSPA-MAXAPR-FRMLA                      PIC 99.
           03  QSPA-EDT-NOEDITS-1                     PIC S9.
           03  QSPA-EDT-TYPE-1-1                      PIC X.
           03  QSPA-EDT-HL-1-1                        PIC X.
           03  QSPA-EDT-COLL-1-1                      PIC XX.
           03  QSPA-EDT-CD-1-1                        PIC X.
           03  QSPA-EDT-RATE-1-1                      PIC 99V99.
           03  QSPA-EDT-RATETYPE-1-1                  PIC X.
           03  QSPA-EDT-AMT-1-1                       PIC 9(7)V99.
           03  QSPA-EDT-AMTYPE-1-1                    PIC X.
           03  QSPA-EDT-TERM-1-1                      PIC 9(3)V9.
           03  QSPA-EDT-TYPE-1-2                      PIC X.
           03  QSPA-EDT-HL-1-2                        PIC X.
           03  QSPA-EDT-COLL-1-2                      PIC XX.
           03  QSPA-EDT-CD-1-2                        PIC X.
           03  QSPA-EDT-RATE-1-2                      PIC 99V99.
           03  QSPA-EDT-RATETYPE-1-2                  PIC X.
           03  QSPA-EDT-AMT-1-2                       PIC 9(7)V99.
           03  QSPA-EDT-AMTYPE-1-2                    PIC X.
           03  QSPA-EDT-TERM-1-2                      PIC 9(3)V9.
           03  QSPA-EDT-TYPE-1-3                      PIC X.
           03  QSPA-EDT-HL-1-3                        PIC X.
           03  QSPA-EDT-COLL-1-3                      PIC XX.
           03  QSPA-EDT-CD-1-3                        PIC X.
           03  QSPA-EDT-RATE-1-3                      PIC 99V99.
           03  QSPA-EDT-RATETYPE-1-3                  PIC X.
           03  QSPA-EDT-AMT-1-3                       PIC 9(7)V99.
           03  QSPA-EDT-AMTYPE-1-3                    PIC X.
           03  QSPA-EDT-TERM-1-3                      PIC 9(3)V9.
           03  QSPA-EDT-TYPE-1-4                      PIC X.
           03  QSPA-EDT-HL-1-4                        PIC X.
           03  QSPA-EDT-COLL-1-4                      PIC XX.
           03  QSPA-EDT-CD-1-4                        PIC X.
           03  QSPA-EDT-RATE-1-4                      PIC 99V99.
           03  QSPA-EDT-RATETYPE-1-4                  PIC X.
           03  QSPA-EDT-AMT-1-4                       PIC 9(7)V99.
           03  QSPA-EDT-AMTYPE-1-4                    PIC X.
           03  QSPA-EDT-TERM-1-4                      PIC 9(3)V9.
           03  QSPA-EDT-TYPE-1-5                      PIC X.
           03  QSPA-EDT-HL-1-5                        PIC X.
           03  QSPA-EDT-COLL-1-5                      PIC XX.
           03  QSPA-EDT-CD-1-5                        PIC X.
           03  QSPA-EDT-RATE-1-5                      PIC 99V99.
           03  QSPA-EDT-RATETYPE-1-5                  PIC X.
           03  QSPA-EDT-AMT-1-5                       PIC 9(7)V99.
           03  QSPA-EDT-AMTYPE-1-5                    PIC X.
           03  QSPA-EDT-TERM-1-5                      PIC 9(3)V9.
           03  QSPA-EDT-TYPE-1-6                      PIC X.
           03  QSPA-EDT-HL-1-6                        PIC X.
           03  QSPA-EDT-COLL-1-6                      PIC XX.
           03  QSPA-EDT-CD-1-6                        PIC X.
           03  QSPA-EDT-RATE-1-6                      PIC 99V99.
           03  QSPA-EDT-RATETYPE-1-6                  PIC X.
           03  QSPA-EDT-AMT-1-6                       PIC 9(7)V99.
           03  QSPA-EDT-AMTYPE-1-6                    PIC X.
           03  QSPA-EDT-TERM-1-6                      PIC 9(3)V9.
           03  QSPA-EDT-NOEDITS-2                     PIC S9.
           03  QSPA-EDT-TYPE-2-1                      PIC X.
           03  QSPA-EDT-HL-2-1                        PIC X.
           03  QSPA-EDT-COLL-2-1                      PIC XX.
           03  QSPA-EDT-CD-2-1                        PIC X.
           03  QSPA-EDT-RATE-2-1                      PIC 99V99.
           03  QSPA-EDT-RATETYPE-2-1                  PIC X.
           03  QSPA-EDT-AMT-2-1                       PIC 9(7)V99.
           03  QSPA-EDT-AMTYPE-2-1                    PIC X.
           03  QSPA-EDT-TERM-2-1                      PIC 9(3)V9.
           03  QSPA-EDT-TYPE-2-2                      PIC X.
           03  QSPA-EDT-HL-2-2                        PIC X.
           03  QSPA-EDT-COLL-2-2                      PIC XX.
           03  QSPA-EDT-CD-2-2                        PIC X.
           03  QSPA-EDT-RATE-2-2                      PIC 99V99.
           03  QSPA-EDT-RATETYPE-2-2                  PIC X.
           03  QSPA-EDT-AMT-2-2                       PIC 9(7)V99.
           03  QSPA-EDT-AMTYPE-2-2                    PIC X.
           03  QSPA-EDT-TERM-2-2                      PIC 9(3)V9.
           03  QSPA-EDT-TYPE-2-3                      PIC X.
           03  QSPA-EDT-HL-2-3                        PIC X.
           03  QSPA-EDT-COLL-2-3                      PIC XX.
           03  QSPA-EDT-CD-2-3                        PIC X.
           03  QSPA-EDT-RATE-2-3                      PIC 99V99.
           03  QSPA-EDT-RATETYPE-2-3                  PIC X.
           03  QSPA-EDT-AMT-2-3                       PIC 9(7)V99.
           03  QSPA-EDT-AMTYPE-2-3                    PIC X.
           03  QSPA-EDT-TERM-2-3                      PIC 9(3)V9.
           03  QSPA-EDT-TYPE-2-4                      PIC X.
           03  QSPA-EDT-HL-2-4                        PIC X.
           03  QSPA-EDT-COLL-2-4                      PIC XX.
           03  QSPA-EDT-CD-2-4                        PIC X.
           03  QSPA-EDT-RATE-2-4                      PIC 99V99.
           03  QSPA-EDT-RATETYPE-2-4                  PIC X.
           03  QSPA-EDT-AMT-2-4                       PIC 9(7)V99.
           03  QSPA-EDT-AMTYPE-2-4                    PIC X.
           03  QSPA-EDT-TERM-2-4                      PIC 9(3)V9.
           03  QSPA-EDT-TYPE-2-5                      PIC X.
           03  QSPA-EDT-HL-2-5                        PIC X.
           03  QSPA-EDT-COLL-2-5                      PIC XX.
           03  QSPA-EDT-CD-2-5                        PIC X.
           03  QSPA-EDT-RATE-2-5                      PIC 99V99.
           03  QSPA-EDT-RATETYPE-2-5                  PIC X.
           03  QSPA-EDT-AMT-2-5                       PIC 9(7)V99.
           03  QSPA-EDT-AMTYPE-2-5                    PIC X.
           03  QSPA-EDT-TERM-2-5                      PIC 9(3)V9.
           03  QSPA-EDT-TYPE-2-6                      PIC X.
           03  QSPA-EDT-HL-2-6                        PIC X.
           03  QSPA-EDT-COLL-2-6                      PIC XX.
           03  QSPA-EDT-CD-2-6                        PIC X.
           03  QSPA-EDT-RATE-2-6                      PIC 99V99.
           03  QSPA-EDT-RATETYPE-2-6                  PIC X.
           03  QSPA-EDT-AMT-2-6                       PIC 9(7)V99.
           03  QSPA-EDT-AMTYPE-2-6                    PIC X.
           03  QSPA-EDT-TERM-2-6                      PIC 9(3)V9.
       01  QSPB-REC.
           03  QSPB-ORGST                             PIC XX.
           03  QSPB-SPRCLASS                          PIC 999.
           03  QSPB-SUBCLASS                          PIC 99.
           03  QSPB-LAWCODE                           PIC 99.
           03  QSPB-INSCOM-1                          PIC 99.
           03  QSPB-INSRATECD-1                       PIC 999.
           03  QSPB-INSJFAC-1                         PIC 9(4)V99.
           03  QSPB-INSDISCNT-1                       PIC 99V99.
           03  QSPB-INSCHGABL-1                       PIC X(2).
           03  QSPB-INSTERM-1-1                       PIC 9(3).
           03  QSPB-INSRATE-1-1                       PIC 99V9999.
           03  QSPB-INSTERM-1-2                       PIC 9(3).
           03  QSPB-INSRATE-1-2                       PIC 99V9999.
           03  QSPB-INSTERM-1-3                       PIC 9(3).
           03  QSPB-INSRATE-1-3                       PIC 99V9999.
           03  QSPB-INSTERM-1-4                       PIC 9(3).
           03  QSPB-INSRATE-1-4                       PIC 99V9999.
           03  QSPB-INSTERM-1-5                       PIC 9(3).
           03  QSPB-INSRATE-1-5                       PIC 99V9999.
           03  QSPB-INSTERM-1-6                       PIC 9(3).
           03  QSPB-INSRATE-1-6                       PIC 99V9999.
           03  QSPB-INSTERM-1-7                       PIC 9(3).
           03  QSPB-INSRATE-1-7                       PIC 99V9999.
           03  QSPB-INSTERM-1-8                       PIC 9(3).
           03  QSPB-INSRATE-1-8                       PIC 99V9999.
           03  QSPB-INSTERM-1-9                       PIC 9(3).
           03  QSPB-INSRATE-1-9                       PIC 99V9999.
           03  QSPB-INSTERM-1-10                      PIC 9(3).
           03  QSPB-INSRATE-1-10                      PIC 99V9999.
           03  QSPB-INSTERM-1-11                      PIC 9(3).
           03  QSPB-INSRATE-1-11                      PIC 99V9999.
           03  QSPB-INSTERM-1-12                      PIC 9(3).
           03  QSPB-INSRATE-1-12                      PIC 99V9999.
           03  QSPB-INSTERM-1-13                      PIC 9(3).
           03  QSPB-INSRATE-1-13                      PIC 99V9999.
           03  QSPB-INSTERM-1-14                      PIC 9(3).
           03  QSPB-INSRATE-1-14                      PIC 99V9999.
           03  QSPB-INSTERM-1-15                      PIC 9(3).
           03  QSPB-INSRATE-1-15                      PIC 99V9999.
           03  QSPB-INSTERM-1-16                      PIC 9(3).
           03  QSPB-INSRATE-1-16                      PIC 99V9999.
           03  QSPB-INSTERM-1-17                      PIC 9(3).
           03  QSPB-INSRATE-1-17                      PIC 99V9999.
           03  QSPB-INSTERM-1-18                      PIC 9(3).
           03  QSPB-INSRATE-1-18                      PIC 99V9999.
           03  QSPB-INSTERM-1-19                      PIC 9(3).
           03  QSPB-INSRATE-1-19                      PIC 99V9999.
           03  QSPB-INSTERM-1-20                      PIC 9(3).
           03  QSPB-INSRATE-1-20                      PIC 99V9999.
           03  QSPB-INSMINPREM-1                      PIC 9(4)V99.
           03  QSPB-INSMINCHGABL-1                    PIC 9(6).
           03  QSPB-INSMAXCHGABL-1                    PIC 9(7).
           03  QSPB-INSMAXBENIF-1                     PIC 9(7).
           03  QSPB-INSMAXTERM-1                      PIC 9(3).
           03  QSPB-INSCHGBEN-1                       PIC X.
           03  QSPB-INSEXCEPT-1                       PIC 99.
           03  QSPB-INSNOVER-1                        PIC 99.
           03  QSPB-INSMINAGE-1-1                     PIC 99.
           03  QSPB-INSMAXAGE-1-1                     PIC 99.
           03  QSPB-INSMAXCOV-1-1                     PIC 9(7).
           03  QSPB-INSMAXTRM-1-1                     PIC 9(3).
           03  QSPB-INSMINAGE-1-2                     PIC 99.
           03  QSPB-INSMAXAGE-1-2                     PIC 99.
           03  QSPB-INSMAXCOV-1-2                     PIC 9(7).
           03  QSPB-INSMAXTRM-1-2                     PIC 9(3).
           03  QSPB-INSMINAGE-1-3                     PIC 99.
           03  QSPB-INSMAXAGE-1-3                     PIC 99.
           03  QSPB-INSMAXCOV-1-3                     PIC 9(7).
           03  QSPB-INSMAXTRM-1-3                     PIC 9(3).
           03  QSPB-INSMINAGE-1-4                     PIC 99.
           03  QSPB-INSMAXAGE-1-4                     PIC 99.
           03  QSPB-INSMAXCOV-1-4                     PIC 9(7).
           03  QSPB-INSMAXTRM-1-4                     PIC 9(3).
           03  QSPB-INSMINAGE-1-5                     PIC 99.
           03  QSPB-INSMAXAGE-1-5                     PIC 99.
           03  QSPB-INSMAXCOV-1-5                     PIC 9(7).
           03  QSPB-INSMAXTRM-1-5                     PIC 9(3).
           03  QSPB-INSFEECD-1                        PIC XX.
           03  QSPB-INSFEE-FRMLA-1                    PIC X.
           03  QSPB-INSCHGABL-FG-1                    PIC X.
           03  QSPB-INSREJECT-FG-1                    PIC X.
           03  QSPB-INSFINCHG-1                       PIC X.
           03  QSPB-INSCOM-2                          PIC 99.
           03  QSPB-INSRATECD-2                       PIC 999.
           03  QSPB-INSJFAC-2                         PIC 9(4)V99.
           03  QSPB-INSDISCNT-2                       PIC 99V99.
           03  QSPB-INSCHGABL-2                       PIC X(2).
           03  QSPB-INSTERM-2-1                       PIC 9(3).
           03  QSPB-INSRATE-2-1                       PIC 99V9999.
           03  QSPB-INSTERM-2-2                       PIC 9(3).
           03  QSPB-INSRATE-2-2                       PIC 99V9999.
           03  QSPB-INSTERM-2-3                       PIC 9(3).
           03  QSPB-INSRATE-2-3                       PIC 99V9999.
           03  QSPB-INSTERM-2-4                       PIC 9(3).
           03  QSPB-INSRATE-2-4                       PIC 99V9999.
           03  QSPB-INSTERM-2-5                       PIC 9(3).
           03  QSPB-INSRATE-2-5                       PIC 99V9999.
           03  QSPB-INSTERM-2-6                       PIC 9(3).
           03  QSPB-INSRATE-2-6                       PIC 99V9999.
           03  QSPB-INSTERM-2-7                       PIC 9(3).
           03  QSPB-INSRATE-2-7                       PIC 99V9999.
           03  QSPB-INSTERM-2-8                       PIC 9(3).
           03  QSPB-INSRATE-2-8                       PIC 99V9999.
           03  QSPB-INSTERM-2-9                       PIC 9(3).
           03  QSPB-INSRATE-2-9                       PIC 99V9999.
           03  QSPB-INSTERM-2-10                      PIC 9(3).
           03  QSPB-INSRATE-2-10                      PIC 99V9999.
           03  QSPB-INSTERM-2-11                      PIC 9(3).
           03  QSPB-INSRATE-2-11                      PIC 99V9999.
           03  QSPB-INSTERM-2-12                      PIC 9(3).
           03  QSPB-INSRATE-2-12                      PIC 99V9999.
           03  QSPB-INSTERM-2-13                      PIC 9(3).
           03  QSPB-INSRATE-2-13                      PIC 99V9999.
           03  QSPB-INSTERM-2-14                      PIC 9(3).
           03  QSPB-INSRATE-2-14                      PIC 99V9999.
           03  QSPB-INSTERM-2-15                      PIC 9(3).
           03  QSPB-INSRATE-2-15                      PIC 99V9999.
           03  QSPB-INSTERM-2-16                      PIC 9(3).
           03  QSPB-INSRATE-2-16                      PIC 99V9999.
           03  QSPB-INSTERM-2-17                      PIC 9(3).
           03  QSPB-INSRATE-2-17                      PIC 99V9999.
           03  QSPB-INSTERM-2-18                      PIC 9(3).
           03  QSPB-INSRATE-2-18                      PIC 99V9999.
           03  QSPB-INSTERM-2-19                      PIC 9(3).
           03  QSPB-INSRATE-2-19                      PIC 99V9999.
           03  QSPB-INSTERM-2-20                      PIC 9(3).
           03  QSPB-INSRATE-2-20                      PIC 99V9999.
           03  QSPB-INSMINPREM-2                      PIC 9(4)V99.
           03  QSPB-INSMINCHGABL-2                    PIC 9(6).
           03  QSPB-INSMAXCHGABL-2                    PIC 9(7).
           03  QSPB-INSMAXBENIF-2                     PIC 9(7).
           03  QSPB-INSMAXTERM-2                      PIC 9(3).
           03  QSPB-INSCHGBEN-2                       PIC X.
           03  QSPB-INSEXCEPT-2                       PIC 99.
           03  QSPB-INSNOVER-2                        PIC 99.
           03  QSPB-INSMINAGE-2-1                     PIC 99.
           03  QSPB-INSMAXAGE-2-1                     PIC 99.
           03  QSPB-INSMAXCOV-2-1                     PIC 9(7).
           03  QSPB-INSMAXTRM-2-1                     PIC 9(3).
           03  QSPB-INSMINAGE-2-2                     PIC 99.
           03  QSPB-INSMAXAGE-2-2                     PIC 99.
           03  QSPB-INSMAXCOV-2-2                     PIC 9(7).
           03  QSPB-INSMAXTRM-2-2                     PIC 9(3).
           03  QSPB-INSMINAGE-2-3                     PIC 99.
           03  QSPB-INSMAXAGE-2-3                     PIC 99.
           03  QSPB-INSMAXCOV-2-3                     PIC 9(7).
           03  QSPB-INSMAXTRM-2-3                     PIC 9(3).
           03  QSPB-INSMINAGE-2-4                     PIC 99.
           03  QSPB-INSMAXAGE-2-4                     PIC 99.
           03  QSPB-INSMAXCOV-2-4                     PIC 9(7).
           03  QSPB-INSMAXTRM-2-4                     PIC 9(3).
           03  QSPB-INSMINAGE-2-5                     PIC 99.
           03  QSPB-INSMAXAGE-2-5                     PIC 99.
           03  QSPB-INSMAXCOV-2-5                     PIC 9(7).
           03  QSPB-INSMAXTRM-2-5                     PIC 9(3).
           03  QSPB-INSFEECD-2                        PIC XX.
           03  QSPB-INSFEE-FRMLA-2                    PIC X.
           03  QSPB-INSCHGABL-FG-2                    PIC X.
           03  QSPB-INSREJECT-FG-2                    PIC X.
           03  QSPB-INSFINCHG-2                       PIC X.
           03  QSPB-INSCOM-3                          PIC 99.
           03  QSPB-INSRATECD-3                       PIC 999.
           03  QSPB-INSJFAC-3                         PIC 9(4)V99.
           03  QSPB-INSDISCNT-3                       PIC 99V99.
           03  QSPB-INSCHGABL-3                       PIC X(2).
           03  QSPB-INSTERM-3-1                       PIC 9(3).
           03  QSPB-INSRATE-3-1                       PIC 99V9999.
           03  QSPB-INSTERM-3-2                       PIC 9(3).
           03  QSPB-INSRATE-3-2                       PIC 99V9999.
           03  QSPB-INSTERM-3-3                       PIC 9(3).
           03  QSPB-INSRATE-3-3                       PIC 99V9999.
           03  QSPB-INSTERM-3-4                       PIC 9(3).
           03  QSPB-INSRATE-3-4                       PIC 99V9999.
           03  QSPB-INSTERM-3-5                       PIC 9(3).
           03  QSPB-INSRATE-3-5                       PIC 99V9999.
           03  QSPB-INSTERM-3-6                       PIC 9(3).
           03  QSPB-INSRATE-3-6                       PIC 99V9999.
           03  QSPB-INSTERM-3-7                       PIC 9(3).
           03  QSPB-INSRATE-3-7                       PIC 99V9999.
           03  QSPB-INSTERM-3-8                       PIC 9(3).
           03  QSPB-INSRATE-3-8                       PIC 99V9999.
           03  QSPB-INSTERM-3-9                       PIC 9(3).
           03  QSPB-INSRATE-3-9                       PIC 99V9999.
           03  QSPB-INSTERM-3-10                      PIC 9(3).
           03  QSPB-INSRATE-3-10                      PIC 99V9999.
           03  QSPB-INSTERM-3-11                      PIC 9(3).
           03  QSPB-INSRATE-3-11                      PIC 99V9999.
           03  QSPB-INSTERM-3-12                      PIC 9(3).
           03  QSPB-INSRATE-3-12                      PIC 99V9999.
           03  QSPB-INSTERM-3-13                      PIC 9(3).
           03  QSPB-INSRATE-3-13                      PIC 99V9999.
           03  QSPB-INSTERM-3-14                      PIC 9(3).
           03  QSPB-INSRATE-3-14                      PIC 99V9999.
           03  QSPB-INSTERM-3-15                      PIC 9(3).
           03  QSPB-INSRATE-3-15                      PIC 99V9999.
           03  QSPB-INSTERM-3-16                      PIC 9(3).
           03  QSPB-INSRATE-3-16                      PIC 99V9999.
           03  QSPB-INSTERM-3-17                      PIC 9(3).
           03  QSPB-INSRATE-3-17                      PIC 99V9999.
           03  QSPB-INSTERM-3-18                      PIC 9(3).
           03  QSPB-INSRATE-3-18                      PIC 99V9999.
           03  QSPB-INSTERM-3-19                      PIC 9(3).
           03  QSPB-INSRATE-3-19                      PIC 99V9999.
           03  QSPB-INSTERM-3-20                      PIC 9(3).
           03  QSPB-INSRATE-3-20                      PIC 99V9999.
           03  QSPB-INSMINPREM-3                      PIC 9(4)V99.
           03  QSPB-INSMINCHGABL-3                    PIC 9(6).
           03  QSPB-INSMAXCHGABL-3                    PIC 9(7).
           03  QSPB-INSMAXBENIF-3                     PIC 9(7).
           03  QSPB-INSMAXTERM-3                      PIC 9(3).
           03  QSPB-INSCHGBEN-3                       PIC X.
           03  QSPB-INSEXCEPT-3                       PIC 99.
           03  QSPB-INSNOVER-3                        PIC 99.
           03  QSPB-INSMINAGE-3-1                     PIC 99.
           03  QSPB-INSMAXAGE-3-1                     PIC 99.
           03  QSPB-INSMAXCOV-3-1                     PIC 9(7).
           03  QSPB-INSMAXTRM-3-1                     PIC 9(3).
           03  QSPB-INSMINAGE-3-2                     PIC 99.
           03  QSPB-INSMAXAGE-3-2                     PIC 99.
           03  QSPB-INSMAXCOV-3-2                     PIC 9(7).
           03  QSPB-INSMAXTRM-3-2                     PIC 9(3).
           03  QSPB-INSMINAGE-3-3                     PIC 99.
           03  QSPB-INSMAXAGE-3-3                     PIC 99.
           03  QSPB-INSMAXCOV-3-3                     PIC 9(7).
           03  QSPB-INSMAXTRM-3-3                     PIC 9(3).
           03  QSPB-INSMINAGE-3-4                     PIC 99.
           03  QSPB-INSMAXAGE-3-4                     PIC 99.
           03  QSPB-INSMAXCOV-3-4                     PIC 9(7).
           03  QSPB-INSMAXTRM-3-4                     PIC 9(3).
           03  QSPB-INSMINAGE-3-5                     PIC 99.
           03  QSPB-INSMAXAGE-3-5                     PIC 99.
           03  QSPB-INSMAXCOV-3-5                     PIC 9(7).
           03  QSPB-INSMAXTRM-3-5                     PIC 9(3).
           03  QSPB-INSFEECD-3                        PIC XX.
           03  QSPB-INSFEE-FRMLA-3                    PIC X.
           03  QSPB-INSCHGABL-FG-3                    PIC X.
           03  QSPB-INSREJECT-FG-3                    PIC X.
           03  QSPB-INSFINCHG-3                       PIC X.
           03  QSPB-INSCOM-4                          PIC 99.
           03  QSPB-INSRATECD-4                       PIC 999.
           03  QSPB-INSJFAC-4                         PIC 9(4)V99.
           03  QSPB-INSDISCNT-4                       PIC 99V99.
           03  QSPB-INSCHGABL-4                       PIC X(2).
           03  QSPB-INSTERM-4-1                       PIC 9(3).
           03  QSPB-INSRATE-4-1                       PIC 99V9999.
           03  QSPB-INSTERM-4-2                       PIC 9(3).
           03  QSPB-INSRATE-4-2                       PIC 99V9999.
           03  QSPB-INSTERM-4-3                       PIC 9(3).
           03  QSPB-INSRATE-4-3                       PIC 99V9999.
           03  QSPB-INSTERM-4-4                       PIC 9(3).
           03  QSPB-INSRATE-4-4                       PIC 99V9999.
           03  QSPB-INSTERM-4-5                       PIC 9(3).
           03  QSPB-INSRATE-4-5                       PIC 99V9999.
           03  QSPB-INSTERM-4-6                       PIC 9(3).
           03  QSPB-INSRATE-4-6                       PIC 99V9999.
           03  QSPB-INSTERM-4-7                       PIC 9(3).
           03  QSPB-INSRATE-4-7                       PIC 99V9999.
           03  QSPB-INSTERM-4-8                       PIC 9(3).
           03  QSPB-INSRATE-4-8                       PIC 99V9999.
           03  QSPB-INSTERM-4-9                       PIC 9(3).
           03  QSPB-INSRATE-4-9                       PIC 99V9999.
           03  QSPB-INSTERM-4-10                      PIC 9(3).
           03  QSPB-INSRATE-4-10                      PIC 99V9999.
           03  QSPB-INSTERM-4-11                      PIC 9(3).
           03  QSPB-INSRATE-4-11                      PIC 99V9999.
           03  QSPB-INSTERM-4-12                      PIC 9(3).
           03  QSPB-INSRATE-4-12                      PIC 99V9999.
           03  QSPB-INSTERM-4-13                      PIC 9(3).
           03  QSPB-INSRATE-4-13                      PIC 99V9999.
           03  QSPB-INSTERM-4-14                      PIC 9(3).
           03  QSPB-INSRATE-4-14                      PIC 99V9999.
           03  QSPB-INSTERM-4-15                      PIC 9(3).
           03  QSPB-INSRATE-4-15                      PIC 99V9999.
           03  QSPB-INSTERM-4-16                      PIC 9(3).
           03  QSPB-INSRATE-4-16                      PIC 99V9999.
           03  QSPB-INSTERM-4-17                      PIC 9(3).
           03  QSPB-INSRATE-4-17                      PIC 99V9999.
           03  QSPB-INSTERM-4-18                      PIC 9(3).
           03  QSPB-INSRATE-4-18                      PIC 99V9999.
           03  QSPB-INSTERM-4-19                      PIC 9(3).
           03  QSPB-INSRATE-4-19                      PIC 99V9999.
           03  QSPB-INSTERM-4-20                      PIC 9(3).
           03  QSPB-INSRATE-4-20                      PIC 99V9999.
           03  QSPB-INSMINPREM-4                      PIC 9(4)V99.
           03  QSPB-INSMINCHGABL-4                    PIC 9(6).
           03  QSPB-INSMAXCHGABL-4                    PIC 9(7).
           03  QSPB-INSMAXBENIF-4                     PIC 9(7).
           03  QSPB-INSMAXTERM-4                      PIC 9(3).
           03  QSPB-INSCHGBEN-4                       PIC X.
           03  QSPB-INSEXCEPT-4                       PIC 99.
           03  QSPB-INSNOVER-4                        PIC 99.
           03  QSPB-INSMINAGE-4-1                     PIC 99.
           03  QSPB-INSMAXAGE-4-1                     PIC 99.
           03  QSPB-INSMAXCOV-4-1                     PIC 9(7).
           03  QSPB-INSMAXTRM-4-1                     PIC 9(3).
           03  QSPB-INSMINAGE-4-2                     PIC 99.
           03  QSPB-INSMAXAGE-4-2                     PIC 99.
           03  QSPB-INSMAXCOV-4-2                     PIC 9(7).
           03  QSPB-INSMAXTRM-4-2                     PIC 9(3).
           03  QSPB-INSMINAGE-4-3                     PIC 99.
           03  QSPB-INSMAXAGE-4-3                     PIC 99.
           03  QSPB-INSMAXCOV-4-3                     PIC 9(7).
           03  QSPB-INSMAXTRM-4-3                     PIC 9(3).
           03  QSPB-INSMINAGE-4-4                     PIC 99.
           03  QSPB-INSMAXAGE-4-4                     PIC 99.
           03  QSPB-INSMAXCOV-4-4                     PIC 9(7).
           03  QSPB-INSMAXTRM-4-4                     PIC 9(3).
           03  QSPB-INSMINAGE-4-5                     PIC 99.
           03  QSPB-INSMAXAGE-4-5                     PIC 99.
           03  QSPB-INSMAXCOV-4-5                     PIC 9(7).
           03  QSPB-INSMAXTRM-4-5                     PIC 9(3).
           03  QSPB-INSFEECD-4                        PIC XX.
           03  QSPB-INSFEE-FRMLA-4                    PIC X.
           03  QSPB-INSCHGABL-FG-4                    PIC X.
           03  QSPB-INSREJECT-FG-4                    PIC X.
           03  QSPB-INSFINCHG-4                       PIC X.
           03  QSPB-INSCOM-5                          PIC 99.
           03  QSPB-INSRATECD-5                       PIC 999.
           03  QSPB-INSJFAC-5                         PIC 9(4)V99.
           03  QSPB-INSDISCNT-5                       PIC 99V99.
           03  QSPB-INSCHGABL-5                       PIC X(2).
           03  QSPB-INSTERM-5-1                       PIC 9(3).
           03  QSPB-INSRATE-5-1                       PIC 99V9999.
           03  QSPB-INSTERM-5-2                       PIC 9(3).
           03  QSPB-INSRATE-5-2                       PIC 99V9999.
           03  QSPB-INSTERM-5-3                       PIC 9(3).
           03  QSPB-INSRATE-5-3                       PIC 99V9999.
           03  QSPB-INSTERM-5-4                       PIC 9(3).
           03  QSPB-INSRATE-5-4                       PIC 99V9999.
           03  QSPB-INSTERM-5-5                       PIC 9(3).
           03  QSPB-INSRATE-5-5                       PIC 99V9999.
           03  QSPB-INSTERM-5-6                       PIC 9(3).
           03  QSPB-INSRATE-5-6                       PIC 99V9999.
           03  QSPB-INSTERM-5-7                       PIC 9(3).
           03  QSPB-INSRATE-5-7                       PIC 99V9999.
           03  QSPB-INSTERM-5-8                       PIC 9(3).
           03  QSPB-INSRATE-5-8                       PIC 99V9999.
           03  QSPB-INSTERM-5-9                       PIC 9(3).
           03  QSPB-INSRATE-5-9                       PIC 99V9999.
           03  QSPB-INSTERM-5-10                      PIC 9(3).
           03  QSPB-INSRATE-5-10                      PIC 99V9999.
           03  QSPB-INSTERM-5-11                      PIC 9(3).
           03  QSPB-INSRATE-5-11                      PIC 99V9999.
           03  QSPB-INSTERM-5-12                      PIC 9(3).
           03  QSPB-INSRATE-5-12                      PIC 99V9999.
           03  QSPB-INSTERM-5-13                      PIC 9(3).
           03  QSPB-INSRATE-5-13                      PIC 99V9999.
           03  QSPB-INSTERM-5-14                      PIC 9(3).
           03  QSPB-INSRATE-5-14                      PIC 99V9999.
           03  QSPB-INSTERM-5-15                      PIC 9(3).
           03  QSPB-INSRATE-5-15                      PIC 99V9999.
           03  QSPB-INSTERM-5-16                      PIC 9(3).
           03  QSPB-INSRATE-5-16                      PIC 99V9999.
           03  QSPB-INSTERM-5-17                      PIC 9(3).
           03  QSPB-INSRATE-5-17                      PIC 99V9999.
           03  QSPB-INSTERM-5-18                      PIC 9(3).
           03  QSPB-INSRATE-5-18                      PIC 99V9999.
           03  QSPB-INSTERM-5-19                      PIC 9(3).
           03  QSPB-INSRATE-5-19                      PIC 99V9999.
           03  QSPB-INSTERM-5-20                      PIC 9(3).
           03  QSPB-INSRATE-5-20                      PIC 99V9999.
           03  QSPB-INSMINPREM-5                      PIC 9(4)V99.
           03  QSPB-INSMINCHGABL-5                    PIC 9(6).
           03  QSPB-INSMAXCHGABL-5                    PIC 9(7).
           03  QSPB-INSMAXBENIF-5                     PIC 9(7).
           03  QSPB-INSMAXTERM-5                      PIC 9(3).
           03  QSPB-INSCHGBEN-5                       PIC X.
           03  QSPB-INSEXCEPT-5                       PIC 99.
           03  QSPB-INSNOVER-5                        PIC 99.
           03  QSPB-INSMINAGE-5-1                     PIC 99.
           03  QSPB-INSMAXAGE-5-1                     PIC 99.
           03  QSPB-INSMAXCOV-5-1                     PIC 9(7).
           03  QSPB-INSMAXTRM-5-1                     PIC 9(3).
           03  QSPB-INSMINAGE-5-2                     PIC 99.
           03  QSPB-INSMAXAGE-5-2                     PIC 99.
           03  QSPB-INSMAXCOV-5-2                     PIC 9(7).
           03  QSPB-INSMAXTRM-5-2                     PIC 9(3).
           03  QSPB-INSMINAGE-5-3                     PIC 99.
           03  QSPB-INSMAXAGE-5-3                     PIC 99.
           03  QSPB-INSMAXCOV-5-3                     PIC 9(7).
           03  QSPB-INSMAXTRM-5-3                     PIC 9(3).
           03  QSPB-INSMINAGE-5-4                     PIC 99.
           03  QSPB-INSMAXAGE-5-4                     PIC 99.
           03  QSPB-INSMAXCOV-5-4                     PIC 9(7).
           03  QSPB-INSMAXTRM-5-4                     PIC 9(3).
           03  QSPB-INSMINAGE-5-5                     PIC 99.
           03  QSPB-INSMAXAGE-5-5                     PIC 99.
           03  QSPB-INSMAXCOV-5-5                     PIC 9(7).
           03  QSPB-INSMAXTRM-5-5                     PIC 9(3).
           03  QSPB-INSFEECD-5                        PIC XX.
           03  QSPB-INSFEE-FRMLA-5                    PIC X.
           03  QSPB-INSCHGABL-FG-5                    PIC X.
           03  QSPB-INSREJECT-FG-5                    PIC X.
           03  QSPB-INSFINCHG-5                       PIC X.
           03  QSPB-INSCOM-6                          PIC 99.
           03  QSPB-INSRATECD-6                       PIC 999.
           03  QSPB-INSJFAC-6                         PIC 9(4)V99.
           03  QSPB-INSDISCNT-6                       PIC 99V99.
           03  QSPB-INSCHGABL-6                       PIC X(2).
           03  QSPB-INSTERM-6-1                       PIC 9(3).
           03  QSPB-INSRATE-6-1                       PIC 99V9999.
           03  QSPB-INSTERM-6-2                       PIC 9(3).
           03  QSPB-INSRATE-6-2                       PIC 99V9999.
           03  QSPB-INSTERM-6-3                       PIC 9(3).
           03  QSPB-INSRATE-6-3                       PIC 99V9999.
           03  QSPB-INSTERM-6-4                       PIC 9(3).
           03  QSPB-INSRATE-6-4                       PIC 99V9999.
           03  QSPB-INSTERM-6-5                       PIC 9(3).
           03  QSPB-INSRATE-6-5                       PIC 99V9999.
           03  QSPB-INSTERM-6-6                       PIC 9(3).
           03  QSPB-INSRATE-6-6                       PIC 99V9999.
           03  QSPB-INSTERM-6-7                       PIC 9(3).
           03  QSPB-INSRATE-6-7                       PIC 99V9999.
           03  QSPB-INSTERM-6-8                       PIC 9(3).
           03  QSPB-INSRATE-6-8                       PIC 99V9999.
           03  QSPB-INSTERM-6-9                       PIC 9(3).
           03  QSPB-INSRATE-6-9                       PIC 99V9999.
           03  QSPB-INSTERM-6-10                      PIC 9(3).
           03  QSPB-INSRATE-6-10                      PIC 99V9999.
           03  QSPB-INSTERM-6-11                      PIC 9(3).
           03  QSPB-INSRATE-6-11                      PIC 99V9999.
           03  QSPB-INSTERM-6-12                      PIC 9(3).
           03  QSPB-INSRATE-6-12                      PIC 99V9999.
           03  QSPB-INSTERM-6-13                      PIC 9(3).
           03  QSPB-INSRATE-6-13                      PIC 99V9999.
           03  QSPB-INSTERM-6-14                      PIC 9(3).
           03  QSPB-INSRATE-6-14                      PIC 99V9999.
           03  QSPB-INSTERM-6-15                      PIC 9(3).
           03  QSPB-INSRATE-6-15                      PIC 99V9999.
           03  QSPB-INSTERM-6-16                      PIC 9(3).
           03  QSPB-INSRATE-6-16                      PIC 99V9999.
           03  QSPB-INSTERM-6-17                      PIC 9(3).
           03  QSPB-INSRATE-6-17                      PIC 99V9999.
           03  QSPB-INSTERM-6-18                      PIC 9(3).
           03  QSPB-INSRATE-6-18                      PIC 99V9999.
           03  QSPB-INSTERM-6-19                      PIC 9(3).
           03  QSPB-INSRATE-6-19                      PIC 99V9999.
           03  QSPB-INSTERM-6-20                      PIC 9(3).
           03  QSPB-INSRATE-6-20                      PIC 99V9999.
           03  QSPB-INSMINPREM-6                      PIC 9(4)V99.
           03  QSPB-INSMINCHGABL-6                    PIC 9(6).
           03  QSPB-INSMAXCHGABL-6                    PIC 9(7).
           03  QSPB-INSMAXBENIF-6                     PIC 9(7).
           03  QSPB-INSMAXTERM-6                      PIC 9(3).
           03  QSPB-INSCHGBEN-6                       PIC X.
           03  QSPB-INSEXCEPT-6                       PIC 99.
           03  QSPB-INSNOVER-6                        PIC 99.
           03  QSPB-INSMINAGE-6-1                     PIC 99.
           03  QSPB-INSMAXAGE-6-1                     PIC 99.
           03  QSPB-INSMAXCOV-6-1                     PIC 9(7).
           03  QSPB-INSMAXTRM-6-1                     PIC 9(3).
           03  QSPB-INSMINAGE-6-2                     PIC 99.
           03  QSPB-INSMAXAGE-6-2                     PIC 99.
           03  QSPB-INSMAXCOV-6-2                     PIC 9(7).
           03  QSPB-INSMAXTRM-6-2                     PIC 9(3).
           03  QSPB-INSMINAGE-6-3                     PIC 99.
           03  QSPB-INSMAXAGE-6-3                     PIC 99.
           03  QSPB-INSMAXCOV-6-3                     PIC 9(7).
           03  QSPB-INSMAXTRM-6-3                     PIC 9(3).
           03  QSPB-INSMINAGE-6-4                     PIC 99.
           03  QSPB-INSMAXAGE-6-4                     PIC 99.
           03  QSPB-INSMAXCOV-6-4                     PIC 9(7).
           03  QSPB-INSMAXTRM-6-4                     PIC 9(3).
           03  QSPB-INSMINAGE-6-5                     PIC 99.
           03  QSPB-INSMAXAGE-6-5                     PIC 99.
           03  QSPB-INSMAXCOV-6-5                     PIC 9(7).
           03  QSPB-INSMAXTRM-6-5                     PIC 9(3).
           03  QSPB-INSFEECD-6                        PIC XX.
           03  QSPB-INSFEE-FRMLA-6                    PIC X.
           03  QSPB-INSCHGABL-FG-6                    PIC X.
           03  QSPB-INSREJECT-FG-6                    PIC X.
           03  QSPB-INSFINCHG-6                       PIC X.
           03  QSPB-INSCOM-7                          PIC 99.
           03  QSPB-INSRATECD-7                       PIC 999.
           03  QSPB-INSJFAC-7                         PIC 9(4)V99.
           03  QSPB-INSDISCNT-7                       PIC 99V99.
           03  QSPB-INSCHGABL-7                       PIC X(2).
           03  QSPB-INSTERM-7-1                       PIC 9(3).
           03  QSPB-INSRATE-7-1                       PIC 99V9999.
           03  QSPB-INSTERM-7-2                       PIC 9(3).
           03  QSPB-INSRATE-7-2                       PIC 99V9999.
           03  QSPB-INSTERM-7-3                       PIC 9(3).
           03  QSPB-INSRATE-7-3                       PIC 99V9999.
           03  QSPB-INSTERM-7-4                       PIC 9(3).
           03  QSPB-INSRATE-7-4                       PIC 99V9999.
           03  QSPB-INSTERM-7-5                       PIC 9(3).
           03  QSPB-INSRATE-7-5                       PIC 99V9999.
           03  QSPB-INSTERM-7-6                       PIC 9(3).
           03  QSPB-INSRATE-7-6                       PIC 99V9999.
           03  QSPB-INSTERM-7-7                       PIC 9(3).
           03  QSPB-INSRATE-7-7                       PIC 99V9999.
           03  QSPB-INSTERM-7-8                       PIC 9(3).
           03  QSPB-INSRATE-7-8                       PIC 99V9999.
           03  QSPB-INSTERM-7-9                       PIC 9(3).
           03  QSPB-INSRATE-7-9                       PIC 99V9999.
           03  QSPB-INSTERM-7-10                      PIC 9(3).
           03  QSPB-INSRATE-7-10                      PIC 99V9999.
           03  QSPB-INSTERM-7-11                      PIC 9(3).
           03  QSPB-INSRATE-7-11                      PIC 99V9999.
           03  QSPB-INSTERM-7-12                      PIC 9(3).
           03  QSPB-INSRATE-7-12                      PIC 99V9999.
           03  QSPB-INSTERM-7-13                      PIC 9(3).
           03  QSPB-INSRATE-7-13                      PIC 99V9999.
           03  QSPB-INSTERM-7-14                      PIC 9(3).
           03  QSPB-INSRATE-7-14                      PIC 99V9999.
           03  QSPB-INSTERM-7-15                      PIC 9(3).
           03  QSPB-INSRATE-7-15                      PIC 99V9999.
           03  QSPB-INSTERM-7-16                      PIC 9(3).
           03  QSPB-INSRATE-7-16                      PIC 99V9999.
           03  QSPB-INSTERM-7-17                      PIC 9(3).
           03  QSPB-INSRATE-7-17                      PIC 99V9999.
           03  QSPB-INSTERM-7-18                      PIC 9(3).
           03  QSPB-INSRATE-7-18                      PIC 99V9999.
           03  QSPB-INSTERM-7-19                      PIC 9(3).
           03  QSPB-INSRATE-7-19                      PIC 99V9999.
           03  QSPB-INSTERM-7-20                      PIC 9(3).
           03  QSPB-INSRATE-7-20                      PIC 99V9999.
           03  QSPB-INSMINPREM-7                      PIC 9(4)V99.
           03  QSPB-INSMINCHGABL-7                    PIC 9(6).
           03  QSPB-INSMAXCHGABL-7                    PIC 9(7).
           03  QSPB-INSMAXBENIF-7                     PIC 9(7).
           03  QSPB-INSMAXTERM-7                      PIC 9(3).
           03  QSPB-INSCHGBEN-7                       PIC X.
           03  QSPB-INSEXCEPT-7                       PIC 99.
           03  QSPB-INSNOVER-7                        PIC 99.
           03  QSPB-INSMINAGE-7-1                     PIC 99.
           03  QSPB-INSMAXAGE-7-1                     PIC 99.
           03  QSPB-INSMAXCOV-7-1                     PIC 9(7).
           03  QSPB-INSMAXTRM-7-1                     PIC 9(3).
           03  QSPB-INSMINAGE-7-2                     PIC 99.
           03  QSPB-INSMAXAGE-7-2                     PIC 99.
           03  QSPB-INSMAXCOV-7-2                     PIC 9(7).
           03  QSPB-INSMAXTRM-7-2                     PIC 9(3).
           03  QSPB-INSMINAGE-7-3                     PIC 99.
           03  QSPB-INSMAXAGE-7-3                     PIC 99.
           03  QSPB-INSMAXCOV-7-3                     PIC 9(7).
           03  QSPB-INSMAXTRM-7-3                     PIC 9(3).
           03  QSPB-INSMINAGE-7-4                     PIC 99.
           03  QSPB-INSMAXAGE-7-4                     PIC 99.
           03  QSPB-INSMAXCOV-7-4                     PIC 9(7).
           03  QSPB-INSMAXTRM-7-4                     PIC 9(3).
           03  QSPB-INSMINAGE-7-5                     PIC 99.
           03  QSPB-INSMAXAGE-7-5                     PIC 99.
           03  QSPB-INSMAXCOV-7-5                     PIC 9(7).
           03  QSPB-INSMAXTRM-7-5                     PIC 9(3).
           03  QSPB-INSFEECD-7                        PIC XX.
           03  QSPB-INSFEE-FRMLA-7                    PIC X.
           03  QSPB-INSCHGABL-FG-7                    PIC X.
           03  QSPB-INSREJECT-FG-7                    PIC X.
           03  QSPB-INSFINCHG-7                       PIC X.
           03  QSPB-INSCOM-8                          PIC 99.
           03  QSPB-INSRATECD-8                       PIC 999.
           03  QSPB-INSJFAC-8                         PIC 9(4)V99.
           03  QSPB-INSDISCNT-8                       PIC 99V99.
           03  QSPB-INSCHGABL-8                       PIC X(2).
           03  QSPB-INSTERM-8-1                       PIC 9(3).
           03  QSPB-INSRATE-8-1                       PIC 99V9999.
           03  QSPB-INSTERM-8-2                       PIC 9(3).
           03  QSPB-INSRATE-8-2                       PIC 99V9999.
           03  QSPB-INSTERM-8-3                       PIC 9(3).
           03  QSPB-INSRATE-8-3                       PIC 99V9999.
           03  QSPB-INSTERM-8-4                       PIC 9(3).
           03  QSPB-INSRATE-8-4                       PIC 99V9999.
           03  QSPB-INSTERM-8-5                       PIC 9(3).
           03  QSPB-INSRATE-8-5                       PIC 99V9999.
           03  QSPB-INSTERM-8-6                       PIC 9(3).
           03  QSPB-INSRATE-8-6                       PIC 99V9999.
           03  QSPB-INSTERM-8-7                       PIC 9(3).
           03  QSPB-INSRATE-8-7                       PIC 99V9999.
           03  QSPB-INSTERM-8-8                       PIC 9(3).
           03  QSPB-INSRATE-8-8                       PIC 99V9999.
           03  QSPB-INSTERM-8-9                       PIC 9(3).
           03  QSPB-INSRATE-8-9                       PIC 99V9999.
           03  QSPB-INSTERM-8-10                      PIC 9(3).
           03  QSPB-INSRATE-8-10                      PIC 99V9999.
           03  QSPB-INSTERM-8-11                      PIC 9(3).
           03  QSPB-INSRATE-8-11                      PIC 99V9999.
           03  QSPB-INSTERM-8-12                      PIC 9(3).
           03  QSPB-INSRATE-8-12                      PIC 99V9999.
           03  QSPB-INSTERM-8-13                      PIC 9(3).
           03  QSPB-INSRATE-8-13                      PIC 99V9999.
           03  QSPB-INSTERM-8-14                      PIC 9(3).
           03  QSPB-INSRATE-8-14                      PIC 99V9999.
           03  QSPB-INSTERM-8-15                      PIC 9(3).
           03  QSPB-INSRATE-8-15                      PIC 99V9999.
           03  QSPB-INSTERM-8-16                      PIC 9(3).
           03  QSPB-INSRATE-8-16                      PIC 99V9999.
           03  QSPB-INSTERM-8-17                      PIC 9(3).
           03  QSPB-INSRATE-8-17                      PIC 99V9999.
           03  QSPB-INSTERM-8-18                      PIC 9(3).
           03  QSPB-INSRATE-8-18                      PIC 99V9999.
           03  QSPB-INSTERM-8-19                      PIC 9(3).
           03  QSPB-INSRATE-8-19                      PIC 99V9999.
           03  QSPB-INSTERM-8-20                      PIC 9(3).
           03  QSPB-INSRATE-8-20                      PIC 99V9999.
           03  QSPB-INSMINPREM-8                      PIC 9(4)V99.
           03  QSPB-INSMINCHGABL-8                    PIC 9(6).
           03  QSPB-INSMAXCHGABL-8                    PIC 9(7).
           03  QSPB-INSMAXBENIF-8                     PIC 9(7).
           03  QSPB-INSMAXTERM-8                      PIC 9(3).
           03  QSPB-INSCHGBEN-8                       PIC X.
           03  QSPB-INSEXCEPT-8                       PIC 99.
           03  QSPB-INSNOVER-8                        PIC 99.
           03  QSPB-INSMINAGE-8-1                     PIC 99.
           03  QSPB-INSMAXAGE-8-1                     PIC 99.
           03  QSPB-INSMAXCOV-8-1                     PIC 9(7).
           03  QSPB-INSMAXTRM-8-1                     PIC 9(3).
           03  QSPB-INSMINAGE-8-2                     PIC 99.
           03  QSPB-INSMAXAGE-8-2                     PIC 99.
           03  QSPB-INSMAXCOV-8-2                     PIC 9(7).
           03  QSPB-INSMAXTRM-8-2                     PIC 9(3).
           03  QSPB-INSMINAGE-8-3                     PIC 99.
           03  QSPB-INSMAXAGE-8-3                     PIC 99.
           03  QSPB-INSMAXCOV-8-3                     PIC 9(7).
           03  QSPB-INSMAXTRM-8-3                     PIC 9(3).
           03  QSPB-INSMINAGE-8-4                     PIC 99.
           03  QSPB-INSMAXAGE-8-4                     PIC 99.
           03  QSPB-INSMAXCOV-8-4                     PIC 9(7).
           03  QSPB-INSMAXTRM-8-4                     PIC 9(3).
           03  QSPB-INSMINAGE-8-5                     PIC 99.
           03  QSPB-INSMAXAGE-8-5                     PIC 99.
           03  QSPB-INSMAXCOV-8-5                     PIC 9(7).
           03  QSPB-INSMAXTRM-8-5                     PIC 9(3).
           03  QSPB-INSFEECD-8                        PIC XX.
           03  QSPB-INSFEE-FRMLA-8                    PIC X.
           03  QSPB-INSCHGABL-FG-8                    PIC X.
           03  QSPB-INSREJECT-FG-8                    PIC X.
           03  QSPB-INSFINCHG-8                       PIC X.
           03  QSPB-INSCOM-9                          PIC 99.
           03  QSPB-INSRATECD-9                       PIC 999.
           03  QSPB-INSJFAC-9                         PIC 9(4)V99.
           03  QSPB-INSDISCNT-9                       PIC 99V99.
           03  QSPB-INSCHGABL-9                       PIC X(2).
           03  QSPB-INSTERM-9-1                       PIC 9(3).
           03  QSPB-INSRATE-9-1                       PIC 99V9999.
           03  QSPB-INSTERM-9-2                       PIC 9(3).
           03  QSPB-INSRATE-9-2                       PIC 99V9999.
           03  QSPB-INSTERM-9-3                       PIC 9(3).
           03  QSPB-INSRATE-9-3                       PIC 99V9999.
           03  QSPB-INSTERM-9-4                       PIC 9(3).
           03  QSPB-INSRATE-9-4                       PIC 99V9999.
           03  QSPB-INSTERM-9-5                       PIC 9(3).
           03  QSPB-INSRATE-9-5                       PIC 99V9999.
           03  QSPB-INSTERM-9-6                       PIC 9(3).
           03  QSPB-INSRATE-9-6                       PIC 99V9999.
           03  QSPB-INSTERM-9-7                       PIC 9(3).
           03  QSPB-INSRATE-9-7                       PIC 99V9999.
           03  QSPB-INSTERM-9-8                       PIC 9(3).
           03  QSPB-INSRATE-9-8                       PIC 99V9999.
           03  QSPB-INSTERM-9-9                       PIC 9(3).
           03  QSPB-INSRATE-9-9                       PIC 99V9999.
           03  QSPB-INSTERM-9-10                      PIC 9(3).
           03  QSPB-INSRATE-9-10                      PIC 99V9999.
           03  QSPB-INSTERM-9-11                      PIC 9(3).
           03  QSPB-INSRATE-9-11                      PIC 99V9999.
           03  QSPB-INSTERM-9-12                      PIC 9(3).
           03  QSPB-INSRATE-9-12                      PIC 99V9999.
           03  QSPB-INSTERM-9-13                      PIC 9(3).
           03  QSPB-INSRATE-9-13                      PIC 99V9999.
           03  QSPB-INSTERM-9-14                      PIC 9(3).
           03  QSPB-INSRATE-9-14                      PIC 99V9999.
           03  QSPB-INSTERM-9-15                      PIC 9(3).
           03  QSPB-INSRATE-9-15                      PIC 99V9999.
           03  QSPB-INSTERM-9-16                      PIC 9(3).
           03  QSPB-INSRATE-9-16                      PIC 99V9999.
           03  QSPB-INSTERM-9-17                      PIC 9(3).
           03  QSPB-INSRATE-9-17                      PIC 99V9999.
           03  QSPB-INSTERM-9-18                      PIC 9(3).
           03  QSPB-INSRATE-9-18                      PIC 99V9999.
           03  QSPB-INSTERM-9-19                      PIC 9(3).
           03  QSPB-INSRATE-9-19                      PIC 99V9999.
           03  QSPB-INSTERM-9-20                      PIC 9(3).
           03  QSPB-INSRATE-9-20                      PIC 99V9999.
           03  QSPB-INSMINPREM-9                      PIC 9(4)V99.
           03  QSPB-INSMINCHGABL-9                    PIC 9(6).
           03  QSPB-INSMAXCHGABL-9                    PIC 9(7).
           03  QSPB-INSMAXBENIF-9                     PIC 9(7).
           03  QSPB-INSMAXTERM-9                      PIC 9(3).
           03  QSPB-INSCHGBEN-9                       PIC X.
           03  QSPB-INSEXCEPT-9                       PIC 99.
           03  QSPB-INSNOVER-9                        PIC 99.
           03  QSPB-INSMINAGE-9-1                     PIC 99.
           03  QSPB-INSMAXAGE-9-1                     PIC 99.
           03  QSPB-INSMAXCOV-9-1                     PIC 9(7).
           03  QSPB-INSMAXTRM-9-1                     PIC 9(3).
           03  QSPB-INSMINAGE-9-2                     PIC 99.
           03  QSPB-INSMAXAGE-9-2                     PIC 99.
           03  QSPB-INSMAXCOV-9-2                     PIC 9(7).
           03  QSPB-INSMAXTRM-9-2                     PIC 9(3).
           03  QSPB-INSMINAGE-9-3                     PIC 99.
           03  QSPB-INSMAXAGE-9-3                     PIC 99.
           03  QSPB-INSMAXCOV-9-3                     PIC 9(7).
           03  QSPB-INSMAXTRM-9-3                     PIC 9(3).
           03  QSPB-INSMINAGE-9-4                     PIC 99.
           03  QSPB-INSMAXAGE-9-4                     PIC 99.
           03  QSPB-INSMAXCOV-9-4                     PIC 9(7).
           03  QSPB-INSMAXTRM-9-4                     PIC 9(3).
           03  QSPB-INSMINAGE-9-5                     PIC 99.
           03  QSPB-INSMAXAGE-9-5                     PIC 99.
           03  QSPB-INSMAXCOV-9-5                     PIC 9(7).
           03  QSPB-INSMAXTRM-9-5                     PIC 9(3).
           03  QSPB-INSFEECD-9                        PIC XX.
           03  QSPB-INSFEE-FRMLA-9                    PIC X.
           03  QSPB-INSCHGABL-FG-9                    PIC X.
           03  QSPB-INSREJECT-FG-9                    PIC X.
           03  QSPB-INSFINCHG-9                       PIC X.
           03  QSPB-INSCOM-10                         PIC 99.
           03  QSPB-INSRATECD-10                      PIC 999.
           03  QSPB-INSJFAC-10                        PIC 9(4)V99.
           03  QSPB-INSDISCNT-10                      PIC 99V99.
           03  QSPB-INSCHGABL-10                      PIC X(2).
           03  QSPB-INSTERM-10-1                      PIC 9(3).
           03  QSPB-INSRATE-10-1                      PIC 99V9999.
           03  QSPB-INSTERM-10-2                      PIC 9(3).
           03  QSPB-INSRATE-10-2                      PIC 99V9999.
           03  QSPB-INSTERM-10-3                      PIC 9(3).
           03  QSPB-INSRATE-10-3                      PIC 99V9999.
           03  QSPB-INSTERM-10-4                      PIC 9(3).
           03  QSPB-INSRATE-10-4                      PIC 99V9999.
           03  QSPB-INSTERM-10-5                      PIC 9(3).
           03  QSPB-INSRATE-10-5                      PIC 99V9999.
           03  QSPB-INSTERM-10-6                      PIC 9(3).
           03  QSPB-INSRATE-10-6                      PIC 99V9999.
           03  QSPB-INSTERM-10-7                      PIC 9(3).
           03  QSPB-INSRATE-10-7                      PIC 99V9999.
           03  QSPB-INSTERM-10-8                      PIC 9(3).
           03  QSPB-INSRATE-10-8                      PIC 99V9999.
           03  QSPB-INSTERM-10-9                      PIC 9(3).
           03  QSPB-INSRATE-10-9                      PIC 99V9999.
           03  QSPB-INSTERM-10-10                     PIC 9(3).
           03  QSPB-INSRATE-10-10                     PIC 99V9999.
           03  QSPB-INSTERM-10-11                     PIC 9(3).
           03  QSPB-INSRATE-10-11                     PIC 99V9999.
           03  QSPB-INSTERM-10-12                     PIC 9(3).
           03  QSPB-INSRATE-10-12                     PIC 99V9999.
           03  QSPB-INSTERM-10-13                     PIC 9(3).
           03  QSPB-INSRATE-10-13                     PIC 99V9999.
           03  QSPB-INSTERM-10-14                     PIC 9(3).
           03  QSPB-INSRATE-10-14                     PIC 99V9999.
           03  QSPB-INSTERM-10-15                     PIC 9(3).
           03  QSPB-INSRATE-10-15                     PIC 99V9999.
           03  QSPB-INSTERM-10-16                     PIC 9(3).
           03  QSPB-INSRATE-10-16                     PIC 99V9999.
           03  QSPB-INSTERM-10-17                     PIC 9(3).
           03  QSPB-INSRATE-10-17                     PIC 99V9999.
           03  QSPB-INSTERM-10-18                     PIC 9(3).
           03  QSPB-INSRATE-10-18                     PIC 99V9999.
           03  QSPB-INSTERM-10-19                     PIC 9(3).
           03  QSPB-INSRATE-10-19                     PIC 99V9999.
           03  QSPB-INSTERM-10-20                     PIC 9(3).
           03  QSPB-INSRATE-10-20                     PIC 99V9999.
           03  QSPB-INSMINPREM-10                     PIC 9(4)V99.
           03  QSPB-INSMINCHGABL-10                   PIC 9(6).
           03  QSPB-INSMAXCHGABL-10                   PIC 9(7).
           03  QSPB-INSMAXBENIF-10                    PIC 9(7).
           03  QSPB-INSMAXTERM-10                     PIC 9(3).
           03  QSPB-INSCHGBEN-10                      PIC X.
           03  QSPB-INSEXCEPT-10                      PIC 99.
           03  QSPB-INSNOVER-10                       PIC 99.
           03  QSPB-INSMINAGE-10-1                    PIC 99.
           03  QSPB-INSMAXAGE-10-1                    PIC 99.
           03  QSPB-INSMAXCOV-10-1                    PIC 9(7).
           03  QSPB-INSMAXTRM-10-1                    PIC 9(3).
           03  QSPB-INSMINAGE-10-2                    PIC 99.
           03  QSPB-INSMAXAGE-10-2                    PIC 99.
           03  QSPB-INSMAXCOV-10-2                    PIC 9(7).
           03  QSPB-INSMAXTRM-10-2                    PIC 9(3).
           03  QSPB-INSMINAGE-10-3                    PIC 99.
           03  QSPB-INSMAXAGE-10-3                    PIC 99.
           03  QSPB-INSMAXCOV-10-3                    PIC 9(7).
           03  QSPB-INSMAXTRM-10-3                    PIC 9(3).
           03  QSPB-INSMINAGE-10-4                    PIC 99.
           03  QSPB-INSMAXAGE-10-4                    PIC 99.
           03  QSPB-INSMAXCOV-10-4                    PIC 9(7).
           03  QSPB-INSMAXTRM-10-4                    PIC 9(3).
           03  QSPB-INSMINAGE-10-5                    PIC 99.
           03  QSPB-INSMAXAGE-10-5                    PIC 99.
           03  QSPB-INSMAXCOV-10-5                    PIC 9(7).
           03  QSPB-INSMAXTRM-10-5                    PIC 9(3).
           03  QSPB-INSFEECD-10                       PIC XX.
           03  QSPB-INSFEE-FRMLA-10                   PIC X.
           03  QSPB-INSCHGABL-FG-10                   PIC X.
           03  QSPB-INSREJECT-FG-10                   PIC X.
           03  QSPB-INSFINCHG-10                      PIC X.
           03  QSPB-INSCOM-11                         PIC 99.
           03  QSPB-INSRATECD-11                      PIC 999.
           03  QSPB-INSJFAC-11                        PIC 9(4)V99.
           03  QSPB-INSDISCNT-11                      PIC 99V99.
           03  QSPB-INSCHGABL-11                      PIC X(2).
           03  QSPB-INSTERM-11-1                      PIC 9(3).
           03  QSPB-INSRATE-11-1                      PIC 99V9999.
           03  QSPB-INSTERM-11-2                      PIC 9(3).
           03  QSPB-INSRATE-11-2                      PIC 99V9999.
           03  QSPB-INSTERM-11-3                      PIC 9(3).
           03  QSPB-INSRATE-11-3                      PIC 99V9999.
           03  QSPB-INSTERM-11-4                      PIC 9(3).
           03  QSPB-INSRATE-11-4                      PIC 99V9999.
           03  QSPB-INSTERM-11-5                      PIC 9(3).
           03  QSPB-INSRATE-11-5                      PIC 99V9999.
           03  QSPB-INSTERM-11-6                      PIC 9(3).
           03  QSPB-INSRATE-11-6                      PIC 99V9999.
           03  QSPB-INSTERM-11-7                      PIC 9(3).
           03  QSPB-INSRATE-11-7                      PIC 99V9999.
           03  QSPB-INSTERM-11-8                      PIC 9(3).
           03  QSPB-INSRATE-11-8                      PIC 99V9999.
           03  QSPB-INSTERM-11-9                      PIC 9(3).
           03  QSPB-INSRATE-11-9                      PIC 99V9999.
           03  QSPB-INSTERM-11-10                     PIC 9(3).
           03  QSPB-INSRATE-11-10                     PIC 99V9999.
           03  QSPB-INSTERM-11-11                     PIC 9(3).
           03  QSPB-INSRATE-11-11                     PIC 99V9999.
           03  QSPB-INSTERM-11-12                     PIC 9(3).
           03  QSPB-INSRATE-11-12                     PIC 99V9999.
           03  QSPB-INSTERM-11-13                     PIC 9(3).
           03  QSPB-INSRATE-11-13                     PIC 99V9999.
           03  QSPB-INSTERM-11-14                     PIC 9(3).
           03  QSPB-INSRATE-11-14                     PIC 99V9999.
           03  QSPB-INSTERM-11-15                     PIC 9(3).
           03  QSPB-INSRATE-11-15                     PIC 99V9999.
           03  QSPB-INSTERM-11-16                     PIC 9(3).
           03  QSPB-INSRATE-11-16                     PIC 99V9999.
           03  QSPB-INSTERM-11-17                     PIC 9(3).
           03  QSPB-INSRATE-11-17                     PIC 99V9999.
           03  QSPB-INSTERM-11-18                     PIC 9(3).
           03  QSPB-INSRATE-11-18                     PIC 99V9999.
           03  QSPB-INSTERM-11-19                     PIC 9(3).
           03  QSPB-INSRATE-11-19                     PIC 99V9999.
           03  QSPB-INSTERM-11-20                     PIC 9(3).
           03  QSPB-INSRATE-11-20                     PIC 99V9999.
           03  QSPB-INSMINPREM-11                     PIC 9(4)V99.
           03  QSPB-INSMINCHGABL-11                   PIC 9(6).
           03  QSPB-INSMAXCHGABL-11                   PIC 9(7).
           03  QSPB-INSMAXBENIF-11                    PIC 9(7).
           03  QSPB-INSMAXTERM-11                     PIC 9(3).
           03  QSPB-INSCHGBEN-11                      PIC X.
           03  QSPB-INSEXCEPT-11                      PIC 99.
           03  QSPB-INSNOVER-11                       PIC 99.
           03  QSPB-INSMINAGE-11-1                    PIC 99.
           03  QSPB-INSMAXAGE-11-1                    PIC 99.
           03  QSPB-INSMAXCOV-11-1                    PIC 9(7).
           03  QSPB-INSMAXTRM-11-1                    PIC 9(3).
           03  QSPB-INSMINAGE-11-2                    PIC 99.
           03  QSPB-INSMAXAGE-11-2                    PIC 99.
           03  QSPB-INSMAXCOV-11-2                    PIC 9(7).
           03  QSPB-INSMAXTRM-11-2                    PIC 9(3).
           03  QSPB-INSMINAGE-11-3                    PIC 99.
           03  QSPB-INSMAXAGE-11-3                    PIC 99.
           03  QSPB-INSMAXCOV-11-3                    PIC 9(7).
           03  QSPB-INSMAXTRM-11-3                    PIC 9(3).
           03  QSPB-INSMINAGE-11-4                    PIC 99.
           03  QSPB-INSMAXAGE-11-4                    PIC 99.
           03  QSPB-INSMAXCOV-11-4                    PIC 9(7).
           03  QSPB-INSMAXTRM-11-4                    PIC 9(3).
           03  QSPB-INSMINAGE-11-5                    PIC 99.
           03  QSPB-INSMAXAGE-11-5                    PIC 99.
           03  QSPB-INSMAXCOV-11-5                    PIC 9(7).
           03  QSPB-INSMAXTRM-11-5                    PIC 9(3).
           03  QSPB-INSFEECD-11                       PIC XX.
           03  QSPB-INSFEE-FRMLA-11                   PIC X.
           03  QSPB-INSCHGABL-FG-11                   PIC X.
           03  QSPB-INSREJECT-FG-11                   PIC X.
           03  QSPB-INSFINCHG-11                      PIC X.
           03  QSPB-INSCOM-12                         PIC 99.
           03  QSPB-INSRATECD-12                      PIC 999.
           03  QSPB-INSJFAC-12                        PIC 9(4)V99.
           03  QSPB-INSDISCNT-12                      PIC 99V99.
           03  QSPB-INSCHGABL-12                      PIC X(2).
           03  QSPB-INSTERM-12-1                      PIC 9(3).
           03  QSPB-INSRATE-12-1                      PIC 99V9999.
           03  QSPB-INSTERM-12-2                      PIC 9(3).
           03  QSPB-INSRATE-12-2                      PIC 99V9999.
           03  QSPB-INSTERM-12-3                      PIC 9(3).
           03  QSPB-INSRATE-12-3                      PIC 99V9999.
           03  QSPB-INSTERM-12-4                      PIC 9(3).
           03  QSPB-INSRATE-12-4                      PIC 99V9999.
           03  QSPB-INSTERM-12-5                      PIC 9(3).
           03  QSPB-INSRATE-12-5                      PIC 99V9999.
           03  QSPB-INSTERM-12-6                      PIC 9(3).
           03  QSPB-INSRATE-12-6                      PIC 99V9999.
           03  QSPB-INSTERM-12-7                      PIC 9(3).
           03  QSPB-INSRATE-12-7                      PIC 99V9999.
           03  QSPB-INSTERM-12-8                      PIC 9(3).
           03  QSPB-INSRATE-12-8                      PIC 99V9999.
           03  QSPB-INSTERM-12-9                      PIC 9(3).
           03  QSPB-INSRATE-12-9                      PIC 99V9999.
           03  QSPB-INSTERM-12-10                     PIC 9(3).
           03  QSPB-INSRATE-12-10                     PIC 99V9999.
           03  QSPB-INSTERM-12-11                     PIC 9(3).
           03  QSPB-INSRATE-12-11                     PIC 99V9999.
           03  QSPB-INSTERM-12-12                     PIC 9(3).
           03  QSPB-INSRATE-12-12                     PIC 99V9999.
           03  QSPB-INSTERM-12-13                     PIC 9(3).
           03  QSPB-INSRATE-12-13                     PIC 99V9999.
           03  QSPB-INSTERM-12-14                     PIC 9(3).
           03  QSPB-INSRATE-12-14                     PIC 99V9999.
           03  QSPB-INSTERM-12-15                     PIC 9(3).
           03  QSPB-INSRATE-12-15                     PIC 99V9999.
           03  QSPB-INSTERM-12-16                     PIC 9(3).
           03  QSPB-INSRATE-12-16                     PIC 99V9999.
           03  QSPB-INSTERM-12-17                     PIC 9(3).
           03  QSPB-INSRATE-12-17                     PIC 99V9999.
           03  QSPB-INSTERM-12-18                     PIC 9(3).
           03  QSPB-INSRATE-12-18                     PIC 99V9999.
           03  QSPB-INSTERM-12-19                     PIC 9(3).
           03  QSPB-INSRATE-12-19                     PIC 99V9999.
           03  QSPB-INSTERM-12-20                     PIC 9(3).
           03  QSPB-INSRATE-12-20                     PIC 99V9999.
           03  QSPB-INSMINPREM-12                     PIC 9(4)V99.
           03  QSPB-INSMINCHGABL-12                   PIC 9(6).
           03  QSPB-INSMAXCHGABL-12                   PIC 9(7).
           03  QSPB-INSMAXBENIF-12                    PIC 9(7).
           03  QSPB-INSMAXTERM-12                     PIC 9(3).
           03  QSPB-INSCHGBEN-12                      PIC X.
           03  QSPB-INSEXCEPT-12                      PIC 99.
           03  QSPB-INSNOVER-12                       PIC 99.
           03  QSPB-INSMINAGE-12-1                    PIC 99.
           03  QSPB-INSMAXAGE-12-1                    PIC 99.
           03  QSPB-INSMAXCOV-12-1                    PIC 9(7).
           03  QSPB-INSMAXTRM-12-1                    PIC 9(3).
           03  QSPB-INSMINAGE-12-2                    PIC 99.
           03  QSPB-INSMAXAGE-12-2                    PIC 99.
           03  QSPB-INSMAXCOV-12-2                    PIC 9(7).
           03  QSPB-INSMAXTRM-12-2                    PIC 9(3).
           03  QSPB-INSMINAGE-12-3                    PIC 99.
           03  QSPB-INSMAXAGE-12-3                    PIC 99.
           03  QSPB-INSMAXCOV-12-3                    PIC 9(7).
           03  QSPB-INSMAXTRM-12-3                    PIC 9(3).
           03  QSPB-INSMINAGE-12-4                    PIC 99.
           03  QSPB-INSMAXAGE-12-4                    PIC 99.
           03  QSPB-INSMAXCOV-12-4                    PIC 9(7).
           03  QSPB-INSMAXTRM-12-4                    PIC 9(3).
           03  QSPB-INSMINAGE-12-5                    PIC 99.
           03  QSPB-INSMAXAGE-12-5                    PIC 99.
           03  QSPB-INSMAXCOV-12-5                    PIC 9(7).
           03  QSPB-INSMAXTRM-12-5                    PIC 9(3).
           03  QSPB-INSFEECD-12                       PIC XX.
           03  QSPB-INSFEE-FRMLA-12                   PIC X.
           03  QSPB-INSCHGABL-FG-12                   PIC X.
           03  QSPB-INSREJECT-FG-12                   PIC X.
           03  QSPB-INSFINCHG-12                      PIC X.
 
       01  QSPC-REC.
           03  QSPC-ORGST                             PIC XX.
           03  QSPC-SPRCLASS                          PIC 999.
           03  QSPC-SUBCLASS                          PIC 99.
           03  QSPC-LAWCODE                           PIC 99.
           03  QSPC-INSEARN-ORDER-1                   PIC 9(2).
           03  QSPC-INSEARN-ORDER-2                   PIC 9(2).
           03  QSPC-INSEARN-ORDER-3                   PIC 9(2).
           03  QSPC-INSEARN-ORDER-4                   PIC 9(2).
           03  QSPC-INSEARN-ORDER-5                   PIC 9(2).
           03  QSPC-INSEARN-ORDER-6                   PIC 9(2).
           03  QSPC-INSEARN-ORDER-7                   PIC 9(2).
           03  QSPC-INSEARN-ORDER-8                   PIC 9(2).
           03  QSPC-INSEARN-ORDER-9                   PIC 9(2).
           03  QSPC-INSEARN-ORDER-10                  PIC 9(2).
           03  QSPC-INSEARN-ORDER-11                  PIC 9(2).
           03  QSPC-INSEARN-ORDER-12                  PIC 9(2).
           03  QSPC-INSAH-JOINT-FG                    PIC X.
           03  QSPC-INS-O1-EDIT                       PIC X.
           03  QSPC-INS-O2-EDIT                       PIC X.
           03  QSPC-RBFRMLA-1                         PIC X(2).
           03  QSPC-RBDTE-1-1                         PIC X.
           03  QSPC-RBDAYS-1-1                        PIC 9(2).
           03  QSPC-RBYRTYPE-1-1                      PIC 9(4).
           03  QSPC-RBDTE-1-2                         PIC X.
           03  QSPC-RBDAYS-1-2                        PIC 9(2).
           03  QSPC-RBYRTYPE-1-2                      PIC 9(4).
           03  QSPC-RBMINRET-1                        PIC 9999V99.
           03  QSPC-RBNOMON-1                         PIC 9(2).
           03  QSPC-RBDEDCHG-1                        PIC 9999V99.
           03  QSPC-RBRECAST-1                        PIC 9(2).
           03  QSPC-RBEARLY-1                         PIC 9(2).
           03  QSPC-RBDEFER-1                         PIC 9(2).
           03  QSPC-RBPCT-1                           PIC 9(3)V9.
           03  QSPC-RBREDUC-1                         PIC 9(4)V99.
           03  QSPC-RBMIN-1                           PIC 9(4)V99.
           03  QSPC-RBSPOPT1-1                        PIC 9(2).
           03  QSPC-RBSPOPT2-1                        PIC 9(2).
           03  QSPC-RBMANAGER-1                       PIC 9(2).
           03  QSPC-RBUNITPER-CD-1                    PIC X.
           03  QSPC-RBFRMLA2-1                        PIC X(2).
           03  QSPC-RBFRMLA-2                         PIC X(2).
           03  QSPC-RBDTE-2-1                         PIC X.
           03  QSPC-RBDAYS-2-1                        PIC 9(2).
           03  QSPC-RBYRTYPE-2-1                      PIC 9(4).
           03  QSPC-RBDTE-2-2                         PIC X.
           03  QSPC-RBDAYS-2-2                        PIC 9(2).
           03  QSPC-RBYRTYPE-2-2                      PIC 9(4).
           03  QSPC-RBMINRET-2                        PIC 9999V99.
           03  QSPC-RBNOMON-2                         PIC 9(2).
           03  QSPC-RBDEDCHG-2                        PIC 9999V99.
           03  QSPC-RBRECAST-2                        PIC 9(2).
           03  QSPC-RBEARLY-2                         PIC 9(2).
           03  QSPC-RBDEFER-2                         PIC 9(2).
           03  QSPC-RBPCT-2                           PIC 9(3)V9.
           03  QSPC-RBREDUC-2                         PIC 9(4)V99.
           03  QSPC-RBMIN-2                           PIC 9(4)V99.
           03  QSPC-RBSPOPT1-2                        PIC 9(2).
           03  QSPC-RBSPOPT2-2                        PIC 9(2).
           03  QSPC-RBMANAGER-2                       PIC 9(2).
           03  QSPC-RBUNITPER-CD-2                    PIC X.
           03  QSPC-RBFRMLA2-2                        PIC X(2).
           03  QSPC-RBFRMLA-3                         PIC X(2).
           03  QSPC-RBDTE-3-1                         PIC X.
           03  QSPC-RBDAYS-3-1                        PIC 9(2).
           03  QSPC-RBYRTYPE-3-1                      PIC 9(4).
           03  QSPC-RBDTE-3-2                         PIC X.
           03  QSPC-RBDAYS-3-2                        PIC 9(2).
           03  QSPC-RBYRTYPE-3-2                      PIC 9(4).
           03  QSPC-RBMINRET-3                        PIC 9999V99.
           03  QSPC-RBNOMON-3                         PIC 9(2).
           03  QSPC-RBDEDCHG-3                        PIC 9999V99.
           03  QSPC-RBRECAST-3                        PIC 9(2).
           03  QSPC-RBEARLY-3                         PIC 9(2).
           03  QSPC-RBDEFER-3                         PIC 9(2).
           03  QSPC-RBPCT-3                           PIC 9(3)V9.
           03  QSPC-RBREDUC-3                         PIC 9(4)V99.
           03  QSPC-RBMIN-3                           PIC 9(4)V99.
           03  QSPC-RBSPOPT1-3                        PIC 9(2).
           03  QSPC-RBSPOPT2-3                        PIC 9(2).
           03  QSPC-RBMANAGER-3                       PIC 9(2).
           03  QSPC-RBUNITPER-CD-3                    PIC X.
           03  QSPC-RBFRMLA2-3                        PIC X(2).
           03  QSPC-RBFRMLA-4                         PIC X(2).
           03  QSPC-RBDTE-4-1                         PIC X.
           03  QSPC-RBDAYS-4-1                        PIC 9(2).
           03  QSPC-RBYRTYPE-4-1                      PIC 9(4).
           03  QSPC-RBDTE-4-2                         PIC X.
           03  QSPC-RBDAYS-4-2                        PIC 9(2).
           03  QSPC-RBYRTYPE-4-2                      PIC 9(4).
           03  QSPC-RBMINRET-4                        PIC 9999V99.
           03  QSPC-RBNOMON-4                         PIC 9(2).
           03  QSPC-RBDEDCHG-4                        PIC 9999V99.
           03  QSPC-RBRECAST-4                        PIC 9(2).
           03  QSPC-RBEARLY-4                         PIC 9(2).
           03  QSPC-RBDEFER-4                         PIC 9(2).
           03  QSPC-RBPCT-4                           PIC 9(3)V9.
           03  QSPC-RBREDUC-4                         PIC 9(4)V99.
           03  QSPC-RBMIN-4                           PIC 9(4)V99.
           03  QSPC-RBSPOPT1-4                        PIC 9(2).
           03  QSPC-RBSPOPT2-4                        PIC 9(2).
           03  QSPC-RBMANAGER-4                       PIC 9(2).
           03  QSPC-RBUNITPER-CD-4                    PIC X.
           03  QSPC-RBFRMLA2-4                        PIC X(2).
           03  QSPC-RBFRMLA-5                         PIC X(2).
           03  QSPC-RBDTE-5-1                         PIC X.
           03  QSPC-RBDAYS-5-1                        PIC 9(2).
           03  QSPC-RBYRTYPE-5-1                      PIC 9(4).
           03  QSPC-RBDTE-5-2                         PIC X.
           03  QSPC-RBDAYS-5-2                        PIC 9(2).
           03  QSPC-RBYRTYPE-5-2                      PIC 9(4).
           03  QSPC-RBMINRET-5                        PIC 9999V99.
           03  QSPC-RBNOMON-5                         PIC 9(2).
           03  QSPC-RBDEDCHG-5                        PIC 9999V99.
           03  QSPC-RBRECAST-5                        PIC 9(2).
           03  QSPC-RBEARLY-5                         PIC 9(2).
           03  QSPC-RBDEFER-5                         PIC 9(2).
           03  QSPC-RBPCT-5                           PIC 9(3)V9.
           03  QSPC-RBREDUC-5                         PIC 9(4)V99.
           03  QSPC-RBMIN-5                           PIC 9(4)V99.
           03  QSPC-RBSPOPT1-5                        PIC 9(2).
           03  QSPC-RBSPOPT2-5                        PIC 9(2).
           03  QSPC-RBMANAGER-5                       PIC 9(2).
           03  QSPC-RBUNITPER-CD-5                    PIC X.
           03  QSPC-RBFRMLA2-5                        PIC X(2).
           03  QSPC-RBFRMLA-6                         PIC X(2).
           03  QSPC-RBDTE-6-1                         PIC X.
           03  QSPC-RBDAYS-6-1                        PIC 9(2).
           03  QSPC-RBYRTYPE-6-1                      PIC 9(4).
           03  QSPC-RBDTE-6-2                         PIC X.
           03  QSPC-RBDAYS-6-2                        PIC 9(2).
           03  QSPC-RBYRTYPE-6-2                      PIC 9(4).
           03  QSPC-RBMINRET-6                        PIC 9999V99.
           03  QSPC-RBNOMON-6                         PIC 9(2).
           03  QSPC-RBDEDCHG-6                        PIC 9999V99.
           03  QSPC-RBRECAST-6                        PIC 9(2).
           03  QSPC-RBEARLY-6                         PIC 9(2).
           03  QSPC-RBDEFER-6                         PIC 9(2).
           03  QSPC-RBPCT-6                           PIC 9(3)V9.
           03  QSPC-RBREDUC-6                         PIC 9(4)V99.
           03  QSPC-RBMIN-6                           PIC 9(4)V99.
           03  QSPC-RBSPOPT1-6                        PIC 9(2).
           03  QSPC-RBSPOPT2-6                        PIC 9(2).
           03  QSPC-RBMANAGER-6                       PIC 9(2).
           03  QSPC-RBUNITPER-CD-6                    PIC X.
           03  QSPC-RBFRMLA2-6                        PIC X(2).
           03  QSPC-RBFRMLA-7                         PIC X(2).
           03  QSPC-RBDTE-7-1                         PIC X.
           03  QSPC-RBDAYS-7-1                        PIC 9(2).
           03  QSPC-RBYRTYPE-7-1                      PIC 9(4).
           03  QSPC-RBDTE-7-2                         PIC X.
           03  QSPC-RBDAYS-7-2                        PIC 9(2).
           03  QSPC-RBYRTYPE-7-2                      PIC 9(4).
           03  QSPC-RBMINRET-7                        PIC 9999V99.
           03  QSPC-RBNOMON-7                         PIC 9(2).
           03  QSPC-RBDEDCHG-7                        PIC 9999V99.
           03  QSPC-RBRECAST-7                        PIC 9(2).
           03  QSPC-RBEARLY-7                         PIC 9(2).
           03  QSPC-RBDEFER-7                         PIC 9(2).
           03  QSPC-RBPCT-7                           PIC 9(3)V9.
           03  QSPC-RBREDUC-7                         PIC 9(4)V99.
           03  QSPC-RBMIN-7                           PIC 9(4)V99.
           03  QSPC-RBSPOPT1-7                        PIC 9(2).
           03  QSPC-RBSPOPT2-7                        PIC 9(2).
           03  QSPC-RBMANAGER-7                       PIC 9(2).
           03  QSPC-RBUNITPER-CD-7                    PIC X.
           03  QSPC-RBFRMLA2-7                        PIC X(2).
           03  QSPC-RBFRMLA-8                         PIC X(2).
           03  QSPC-RBDTE-8-1                         PIC X.
           03  QSPC-RBDAYS-8-1                        PIC 9(2).
           03  QSPC-RBYRTYPE-8-1                      PIC 9(4).
           03  QSPC-RBDTE-8-2                         PIC X.
           03  QSPC-RBDAYS-8-2                        PIC 9(2).
           03  QSPC-RBYRTYPE-8-2                      PIC 9(4).
           03  QSPC-RBMINRET-8                        PIC 9999V99.
           03  QSPC-RBNOMON-8                         PIC 9(2).
           03  QSPC-RBDEDCHG-8                        PIC 9999V99.
           03  QSPC-RBRECAST-8                        PIC 9(2).
           03  QSPC-RBEARLY-8                         PIC 9(2).
           03  QSPC-RBDEFER-8                         PIC 9(2).
           03  QSPC-RBPCT-8                           PIC 9(3)V9.
           03  QSPC-RBREDUC-8                         PIC 9(4)V99.
           03  QSPC-RBMIN-8                           PIC 9(4)V99.
           03  QSPC-RBSPOPT1-8                        PIC 9(2).
           03  QSPC-RBSPOPT2-8                        PIC 9(2).
           03  QSPC-RBMANAGER-8                       PIC 9(2).
           03  QSPC-RBUNITPER-CD-8                    PIC X.
           03  QSPC-RBFRMLA2-8                        PIC X(2).
           03  QSPC-RBFRMLA-9                         PIC X(2).
           03  QSPC-RBDTE-9-1                         PIC X.
           03  QSPC-RBDAYS-9-1                        PIC 9(2).
           03  QSPC-RBYRTYPE-9-1                      PIC 9(4).
           03  QSPC-RBDTE-9-2                         PIC X.
           03  QSPC-RBDAYS-9-2                        PIC 9(2).
           03  QSPC-RBYRTYPE-9-2                      PIC 9(4).
           03  QSPC-RBMINRET-9                        PIC 9999V99.
           03  QSPC-RBNOMON-9                         PIC 9(2).
           03  QSPC-RBDEDCHG-9                        PIC 9999V99.
           03  QSPC-RBRECAST-9                        PIC 9(2).
           03  QSPC-RBEARLY-9                         PIC 9(2).
           03  QSPC-RBDEFER-9                         PIC 9(2).
           03  QSPC-RBPCT-9                           PIC 9(3)V9.
           03  QSPC-RBREDUC-9                         PIC 9(4)V99.
           03  QSPC-RBMIN-9                           PIC 9(4)V99.
           03  QSPC-RBSPOPT1-9                        PIC 9(2).
           03  QSPC-RBSPOPT2-9                        PIC 9(2).
           03  QSPC-RBMANAGER-9                       PIC 9(2).
           03  QSPC-RBUNITPER-CD-9                    PIC X.
           03  QSPC-RBFRMLA2-9                        PIC X(2).
           03  QSPC-RBFRMLA-10                        PIC X(2).
           03  QSPC-RBDTE-10-1                        PIC X.
           03  QSPC-RBDAYS-10-1                       PIC 9(2).
           03  QSPC-RBYRTYPE-10-1                     PIC 9(4).
           03  QSPC-RBDTE-10-2                        PIC X.
           03  QSPC-RBDAYS-10-2                       PIC 9(2).
           03  QSPC-RBYRTYPE-10-2                     PIC 9(4).
           03  QSPC-RBMINRET-10                       PIC 9999V99.
           03  QSPC-RBNOMON-10                        PIC 9(2).
           03  QSPC-RBDEDCHG-10                       PIC 9999V99.
           03  QSPC-RBRECAST-10                       PIC 9(2).
           03  QSPC-RBEARLY-10                        PIC 9(2).
           03  QSPC-RBDEFER-10                        PIC 9(2).
           03  QSPC-RBPCT-10                          PIC 9(3)V9.
           03  QSPC-RBREDUC-10                        PIC 9(4)V99.
           03  QSPC-RBMIN-10                          PIC 9(4)V99.
           03  QSPC-RBSPOPT1-10                       PIC 9(2).
           03  QSPC-RBSPOPT2-10                       PIC 9(2).
           03  QSPC-RBMANAGER-10                      PIC 9(2).
           03  QSPC-RBUNITPER-CD-10                   PIC X.
           03  QSPC-RBFRMLA2-10                       PIC X(2).
           03  QSPC-RBFRMLA-11                        PIC X(2).
           03  QSPC-RBDTE-11-1                        PIC X.
           03  QSPC-RBDAYS-11-1                       PIC 9(2).
           03  QSPC-RBYRTYPE-11-1                     PIC 9(4).
           03  QSPC-RBDTE-11-2                        PIC X.
           03  QSPC-RBDAYS-11-2                       PIC 9(2).
           03  QSPC-RBYRTYPE-11-2                     PIC 9(4).
           03  QSPC-RBMINRET-11                       PIC 9999V99.
           03  QSPC-RBNOMON-11                        PIC 9(2).
           03  QSPC-RBDEDCHG-11                       PIC 9999V99.
           03  QSPC-RBRECAST-11                       PIC 9(2).
           03  QSPC-RBEARLY-11                        PIC 9(2).
           03  QSPC-RBDEFER-11                        PIC 9(2).
           03  QSPC-RBPCT-11                          PIC 9(3)V9.
           03  QSPC-RBREDUC-11                        PIC 9(4)V99.
           03  QSPC-RBMIN-11                          PIC 9(4)V99.
           03  QSPC-RBSPOPT1-11                       PIC 9(2).
           03  QSPC-RBSPOPT2-11                       PIC 9(2).
           03  QSPC-RBMANAGER-11                      PIC 9(2).
           03  QSPC-RBUNITPER-CD-11                   PIC X.
           03  QSPC-RBFRMLA2-11                       PIC X(2).
           03  QSPC-RBFRMLA-12                        PIC X(2).
           03  QSPC-RBDTE-12-1                        PIC X.
           03  QSPC-RBDAYS-12-1                       PIC 9(2).
           03  QSPC-RBYRTYPE-12-1                     PIC 9(4).
           03  QSPC-RBDTE-12-2                        PIC X.
           03  QSPC-RBDAYS-12-2                       PIC 9(2).
           03  QSPC-RBYRTYPE-12-2                     PIC 9(4).
           03  QSPC-RBMINRET-12                       PIC 9999V99.
           03  QSPC-RBNOMON-12                        PIC 9(2).
           03  QSPC-RBDEDCHG-12                       PIC 9999V99.
           03  QSPC-RBRECAST-12                       PIC 9(2).
           03  QSPC-RBEARLY-12                        PIC 9(2).
           03  QSPC-RBDEFER-12                        PIC 9(2).
           03  QSPC-RBPCT-12                          PIC 9(3)V9.
           03  QSPC-RBREDUC-12                        PIC 9(4)V99.
           03  QSPC-RBMIN-12                          PIC 9(4)V99.
           03  QSPC-RBSPOPT1-12                       PIC 9(2).
           03  QSPC-RBSPOPT2-12                       PIC 9(2).
           03  QSPC-RBMANAGER-12                      PIC 9(2).
           03  QSPC-RBUNITPER-CD-12                   PIC X.
           03  QSPC-RBFRMLA2-12                       PIC X(2).
           03  QSPC-RBFRMLA-13                        PIC X(2).
           03  QSPC-RBDTE-13-1                        PIC X.
           03  QSPC-RBDAYS-13-1                       PIC 9(2).
           03  QSPC-RBYRTYPE-13-1                     PIC 9(4).
           03  QSPC-RBDTE-13-2                        PIC X.
           03  QSPC-RBDAYS-13-2                       PIC 9(2).
           03  QSPC-RBYRTYPE-13-2                     PIC 9(4).
           03  QSPC-RBMINRET-13                       PIC 9999V99.
           03  QSPC-RBNOMON-13                        PIC 9(2).
           03  QSPC-RBDEDCHG-13                       PIC 9999V99.
           03  QSPC-RBRECAST-13                       PIC 9(2).
           03  QSPC-RBEARLY-13                        PIC 9(2).
           03  QSPC-RBDEFER-13                        PIC 9(2).
           03  QSPC-RBPCT-13                          PIC 9(3)V9.
           03  QSPC-RBREDUC-13                        PIC 9(4)V99.
           03  QSPC-RBMIN-13                          PIC 9(4)V99.
           03  QSPC-RBSPOPT1-13                       PIC 9(2).
           03  QSPC-RBSPOPT2-13                       PIC 9(2).
           03  QSPC-RBMANAGER-13                      PIC 9(2).
           03  QSPC-RBUNITPER-CD-13                   PIC X.
           03  QSPC-RBFRMLA2-13                       PIC X(2).
           03  QSPC-RBFRMLA-14                        PIC X(2).
           03  QSPC-RBDTE-14-1                        PIC X.
           03  QSPC-RBDAYS-14-1                       PIC 9(2).
           03  QSPC-RBYRTYPE-14-1                     PIC 9(4).
           03  QSPC-RBDTE-14-2                        PIC X.
           03  QSPC-RBDAYS-14-2                       PIC 9(2).
           03  QSPC-RBYRTYPE-14-2                     PIC 9(4).
           03  QSPC-RBMINRET-14                       PIC 9999V99.
           03  QSPC-RBNOMON-14                        PIC 9(2).
           03  QSPC-RBDEDCHG-14                       PIC 9999V99.
           03  QSPC-RBRECAST-14                       PIC 9(2).
           03  QSPC-RBEARLY-14                        PIC 9(2).
           03  QSPC-RBDEFER-14                        PIC 9(2).
           03  QSPC-RBPCT-14                          PIC 9(3)V9.
           03  QSPC-RBREDUC-14                        PIC 9(4)V99.
           03  QSPC-RBMIN-14                          PIC 9(4)V99.
           03  QSPC-RBSPOPT1-14                       PIC 9(2).
           03  QSPC-RBSPOPT2-14                       PIC 9(2).
           03  QSPC-RBMANAGER-14                      PIC 9(2).
           03  QSPC-RBUNITPER-CD-14                   PIC X.
           03  QSPC-RBFRMLA2-14                       PIC X(2).
           03  QSPC-RBFRMLA-15                        PIC X(2).
           03  QSPC-RBDTE-15-1                        PIC X.
           03  QSPC-RBDAYS-15-1                       PIC 9(2).
           03  QSPC-RBYRTYPE-15-1                     PIC 9(4).
           03  QSPC-RBDTE-15-2                        PIC X.
           03  QSPC-RBDAYS-15-2                       PIC 9(2).
           03  QSPC-RBYRTYPE-15-2                     PIC 9(4).
           03  QSPC-RBMINRET-15                       PIC 9999V99.
           03  QSPC-RBNOMON-15                        PIC 9(2).
           03  QSPC-RBDEDCHG-15                       PIC 9999V99.
           03  QSPC-RBRECAST-15                       PIC 9(2).
           03  QSPC-RBEARLY-15                        PIC 9(2).
           03  QSPC-RBDEFER-15                        PIC 9(2).
           03  QSPC-RBPCT-15                          PIC 9(3)V9.
           03  QSPC-RBREDUC-15                        PIC 9(4)V99.
           03  QSPC-RBMIN-15                          PIC 9(4)V99.
           03  QSPC-RBSPOPT1-15                       PIC 9(2).
           03  QSPC-RBSPOPT2-15                       PIC 9(2).
           03  QSPC-RBMANAGER-15                      PIC 9(2).
           03  QSPC-RBUNITPER-CD-15                   PIC X.
           03  QSPC-RBFRMLA2-15                       PIC X(2).
           03  QSPC-RBFRMLA-16                        PIC X(2).
           03  QSPC-RBDTE-16-1                        PIC X.
           03  QSPC-RBDAYS-16-1                       PIC 9(2).
           03  QSPC-RBYRTYPE-16-1                     PIC 9(4).
           03  QSPC-RBDTE-16-2                        PIC X.
           03  QSPC-RBDAYS-16-2                       PIC 9(2).
           03  QSPC-RBYRTYPE-16-2                     PIC 9(4).
           03  QSPC-RBMINRET-16                       PIC 9999V99.
           03  QSPC-RBNOMON-16                        PIC 9(2).
           03  QSPC-RBDEDCHG-16                       PIC 9999V99.
           03  QSPC-RBRECAST-16                       PIC 9(2).
           03  QSPC-RBEARLY-16                        PIC 9(2).
           03  QSPC-RBDEFER-16                        PIC 9(2).
           03  QSPC-RBPCT-16                          PIC 9(3)V9.
           03  QSPC-RBREDUC-16                        PIC 9(4)V99.
           03  QSPC-RBMIN-16                          PIC 9(4)V99.
           03  QSPC-RBSPOPT1-16                       PIC 9(2).
           03  QSPC-RBSPOPT2-16                       PIC 9(2).
           03  QSPC-RBMANAGER-16                      PIC 9(2).
           03  QSPC-RBUNITPER-CD-16                   PIC X.
           03  QSPC-RBFRMLA2-16                       PIC X(2).
           03  QSPC-RBFRMLA-17                        PIC X(2).
           03  QSPC-RBDTE-17-1                        PIC X.
           03  QSPC-RBDAYS-17-1                       PIC 9(2).
           03  QSPC-RBYRTYPE-17-1                     PIC 9(4).
           03  QSPC-RBDTE-17-2                        PIC X.
           03  QSPC-RBDAYS-17-2                       PIC 9(2).
           03  QSPC-RBYRTYPE-17-2                     PIC 9(4).
           03  QSPC-RBMINRET-17                       PIC 9999V99.
           03  QSPC-RBNOMON-17                        PIC 9(2).
           03  QSPC-RBDEDCHG-17                       PIC 9999V99.
           03  QSPC-RBRECAST-17                       PIC 9(2).
           03  QSPC-RBEARLY-17                        PIC 9(2).
           03  QSPC-RBDEFER-17                        PIC 9(2).
           03  QSPC-RBPCT-17                          PIC 9(3)V9.
           03  QSPC-RBREDUC-17                        PIC 9(4)V99.
           03  QSPC-RBMIN-17                          PIC 9(4)V99.
           03  QSPC-RBSPOPT1-17                       PIC 9(2).
           03  QSPC-RBSPOPT2-17                       PIC 9(2).
           03  QSPC-RBMANAGER-17                      PIC 9(2).
           03  QSPC-RBUNITPER-CD-17                   PIC X.
           03  QSPC-RBFRMLA2-17                       PIC X(2).
           03  QSPC-RBFRMLA-18                        PIC X(2).
           03  QSPC-RBDTE-18-1                        PIC X.
           03  QSPC-RBDAYS-18-1                       PIC 9(2).
           03  QSPC-RBYRTYPE-18-1                     PIC 9(4).
           03  QSPC-RBDTE-18-2                        PIC X.
           03  QSPC-RBDAYS-18-2                       PIC 9(2).
           03  QSPC-RBYRTYPE-18-2                     PIC 9(4).
           03  QSPC-RBMINRET-18                       PIC 9999V99.
           03  QSPC-RBNOMON-18                        PIC 9(2).
           03  QSPC-RBDEDCHG-18                       PIC 9999V99.
           03  QSPC-RBRECAST-18                       PIC 9(2).
           03  QSPC-RBEARLY-18                        PIC 9(2).
           03  QSPC-RBDEFER-18                        PIC 9(2).
           03  QSPC-RBPCT-18                          PIC 9(3)V9.
           03  QSPC-RBREDUC-18                        PIC 9(4)V99.
           03  QSPC-RBMIN-18                          PIC 9(4)V99.
           03  QSPC-RBSPOPT1-18                       PIC 9(2).
           03  QSPC-RBSPOPT2-18                       PIC 9(2).
           03  QSPC-RBMANAGER-18                      PIC 9(2).
           03  QSPC-RBUNITPER-CD-18                   PIC X.
           03  QSPC-RBFRMLA2-18                       PIC X(2).
           03  QSPC-RBFRMLA-19                        PIC X(2).
           03  QSPC-RBDTE-19-1                        PIC X.
           03  QSPC-RBDAYS-19-1                       PIC 9(2).
           03  QSPC-RBYRTYPE-19-1                     PIC 9(4).
           03  QSPC-RBDTE-19-2                        PIC X.
           03  QSPC-RBDAYS-19-2                       PIC 9(2).
           03  QSPC-RBYRTYPE-19-2                     PIC 9(4).
           03  QSPC-RBMINRET-19                       PIC 9999V99.
           03  QSPC-RBNOMON-19                        PIC 9(2).
           03  QSPC-RBDEDCHG-19                       PIC 9999V99.
           03  QSPC-RBRECAST-19                       PIC 9(2).
           03  QSPC-RBEARLY-19                        PIC 9(2).
           03  QSPC-RBDEFER-19                        PIC 9(2).
           03  QSPC-RBPCT-19                          PIC 9(3)V9.
           03  QSPC-RBREDUC-19                        PIC 9(4)V99.
           03  QSPC-RBMIN-19                          PIC 9(4)V99.
           03  QSPC-RBSPOPT1-19                       PIC 9(2).
           03  QSPC-RBSPOPT2-19                       PIC 9(2).
           03  QSPC-RBMANAGER-19                      PIC 9(2).
           03  QSPC-RBUNITPER-CD-19                   PIC X.
           03  QSPC-RBFRMLA2-19                       PIC X(2).
           03  QSPC-RBFRMLA-20                        PIC X(2).
           03  QSPC-RBDTE-20-1                        PIC X.
           03  QSPC-RBDAYS-20-1                       PIC 9(2).
           03  QSPC-RBYRTYPE-20-1                     PIC 9(4).
           03  QSPC-RBDTE-20-2                        PIC X.
           03  QSPC-RBDAYS-20-2                       PIC 9(2).
           03  QSPC-RBYRTYPE-20-2                     PIC 9(4).
           03  QSPC-RBMINRET-20                       PIC 9999V99.
           03  QSPC-RBNOMON-20                        PIC 9(2).
           03  QSPC-RBDEDCHG-20                       PIC 9999V99.
           03  QSPC-RBRECAST-20                       PIC 9(2).
           03  QSPC-RBEARLY-20                        PIC 9(2).
           03  QSPC-RBDEFER-20                        PIC 9(2).
           03  QSPC-RBPCT-20                          PIC 9(3)V9.
           03  QSPC-RBREDUC-20                        PIC 9(4)V99.
           03  QSPC-RBMIN-20                          PIC 9(4)V99.
           03  QSPC-RBSPOPT1-20                       PIC 9(2).
           03  QSPC-RBSPOPT2-20                       PIC 9(2).
           03  QSPC-RBMANAGER-20                      PIC 9(2).
           03  QSPC-RBUNITPER-CD-20                   PIC X.
           03  QSPC-RBFRMLA2-20                       PIC X(2).
           03  QSPC-RBFRMLA-21                        PIC X(2).
           03  QSPC-RBDTE-21-1                        PIC X.
           03  QSPC-RBDAYS-21-1                       PIC 9(2).
           03  QSPC-RBYRTYPE-21-1                     PIC 9(4).
           03  QSPC-RBDTE-21-2                        PIC X.
           03  QSPC-RBDAYS-21-2                       PIC 9(2).
           03  QSPC-RBYRTYPE-21-2                     PIC 9(4).
           03  QSPC-RBMINRET-21                       PIC 9999V99.
           03  QSPC-RBNOMON-21                        PIC 9(2).
           03  QSPC-RBDEDCHG-21                       PIC 9999V99.
           03  QSPC-RBRECAST-21                       PIC 9(2).
           03  QSPC-RBEARLY-21                        PIC 9(2).
           03  QSPC-RBDEFER-21                        PIC 9(2).
           03  QSPC-RBPCT-21                          PIC 9(3)V9.
           03  QSPC-RBREDUC-21                        PIC 9(4)V99.
           03  QSPC-RBMIN-21                          PIC 9(4)V99.
           03  QSPC-RBSPOPT1-21                       PIC 9(2).
           03  QSPC-RBSPOPT2-21                       PIC 9(2).
           03  QSPC-RBMANAGER-21                      PIC 9(2).
           03  QSPC-RBUNITPER-CD-21                   PIC X.
           03  QSPC-RBFRMLA2-21                       PIC X(2).
           03  QSPC-RBFRMLA-22                        PIC X(2).
           03  QSPC-RBDTE-22-1                        PIC X.
           03  QSPC-RBDAYS-22-1                       PIC 9(2).
           03  QSPC-RBYRTYPE-22-1                     PIC 9(4).
           03  QSPC-RBDTE-22-2                        PIC X.
           03  QSPC-RBDAYS-22-2                       PIC 9(2).
           03  QSPC-RBYRTYPE-22-2                     PIC 9(4).
           03  QSPC-RBMINRET-22                       PIC 9999V99.
           03  QSPC-RBNOMON-22                        PIC 9(2).
           03  QSPC-RBDEDCHG-22                       PIC 9999V99.
           03  QSPC-RBRECAST-22                       PIC 9(2).
           03  QSPC-RBEARLY-22                        PIC 9(2).
           03  QSPC-RBDEFER-22                        PIC 9(2).
           03  QSPC-RBPCT-22                          PIC 9(3)V9.
           03  QSPC-RBREDUC-22                        PIC 9(4)V99.
           03  QSPC-RBMIN-22                          PIC 9(4)V99.
           03  QSPC-RBSPOPT1-22                       PIC 9(2).
           03  QSPC-RBSPOPT2-22                       PIC 9(2).
           03  QSPC-RBMANAGER-22                      PIC 9(2).
           03  QSPC-RBUNITPER-CD-22                   PIC X.
           03  QSPC-RBFRMLA2-22                       PIC X(2).
           03  QSPC-RBCOMCD                           PIC X.
           03  QSPC-ADDON-DISFRMLA                    PIC X.
           03  QSPC-ADDON-WHOLE-FG                    PIC X.
           03  QSPC-ADDON-ORG-MATURITY                PIC X.
           03  QSPC-CONSOLIDATE-RT-FG                 PIC X.
           03  QSPC-1STPY-ADDON-DAYS                  PIC 9(3).
           03  QSPC-1STPY-ADDON-YRTYPE                PIC 9(3).
           03  QSPC-ADDON-RATE-FG                     PIC X.
           03  QSPC-ADDON-RATE                        PIC 999V999.
           03  QSPC-FEEFRMLA-1                        PIC X.
           03  QSPC-FEECD-1                           PIC XX.
           03  QSPC-FEEID-1                           PIC X(7).
           03  QSPC-FEEFRMLA-2                        PIC X.
           03  QSPC-FEECD-2                           PIC XX.
           03  QSPC-FEEID-2                           PIC X(7).
           03  QSPC-ROUND-PAY-FG                      PIC X.
           03  QSPC-IRREGULAR-PAY-FG                  PIC X.
           03  QSPC-DLRVCHGS-RB-CD-1                  PIC X.
           03  QSPC-DLRVCHGS-RB-CD-2                  PIC X.
           03  QSPC-DLRVCHGS-RB-CD-3                  PIC X.
           03  QSPC-PARVHELD-RB-CD                    PIC X.
           03  QSPC-PARVPAID-RB-CD                    PIC X.
           03  QSPC-CONTR-CREDIT                      PIC 99V99.
           03  QSPC-CONTR-CREDIT-CD                   PIC X.
           03  QSPC-CONTR-CREDIT-MIN-DEL-PER          PIC 999V9.
           03  QSPC-OPT-CAL-CHGABLE                   PIC X.
           03  QSPC-PBDEL-DAYS                        PIC S999.
           03  QSPC-PBINCR-PERCENT                    PIC 999.
           03  QSPC-CPOT30-CD                         PIC X.
           03  QSPC-CPOTCHGOFF-CD                     PIC X.
           03  QSPC-RDPOT30-CD                        PIC X.
           03  QSPC-RDPOTCHGOFF-CD                    PIC X.
           03  QSPC-MAX-ALLOW-DELINQ                  PIC 99.
           03  QSPC-RESCIND-BUS-DAYS                  PIC X.
           03  QSPC-RESCIND-SATURDAY                  PIC X.
           03  QSPC-MIN-CREDIT-SCORE                  PIC 9(4).
           03  QSPC-DUAL-IN-BRANCH                    PIC X.
           03  QSPC-DUAL-IN-STATE                     PIC X.
           03  QSPC-PACESETTER-FG                     PIC X.
           03  QSPC-WAGE-ASSIGN-DAYS                  PIC 99.
           03  QSPC-CAL-PAYMENT-ROUND-FG              PIC X.
           03  QSPC-CAN-LOANS-FG                      PIC X.
           03  QSPC-ALLOW-DELINQ-POLCD                PIC X.
           03  QSPC-YTD-MAX-ALLOW-DELINQ              PIC 99.
           03  QSPC-LNCOST-FRMLA                      PIC X.
           03  QSPC-LNCOST-AMT                        PIC 9(7)V99.
           03  QSPC-DLDISC-RVPRIN1-PCNT               PIC 9(3)V99.
           03  QSPC-RENEWABLE-PAYS                    PIC S9(4).
           03  QSPC-RENEWABLE-DAYS                    PIC S9(4).
           03  QSPC-ALLOW-LN-BACK-DATE                PIC X.
           03  QSPC-DLDISC-RVPRIN1-AF-PCNT            PIC 9(3)V99.
           03  QSPC-CAL-MAX-AT-ESTIMATE               PIC X.
           03  QSPC-INSAH-RETRO                       PIC X.
           03  QSPC-INSAH-DAYS-WAIT                   PIC XX.
           03  QSPC-ADDON-EXCEPT-FG                   PIC X.
           03  QSPC-REPO-TRCD-1                       PIC X(02).
           03  QSPC-REPO-FMNO-1-1                     PIC 9(03).
           03  QSPC-REPO-FMNO-1-2                     PIC 9(03).
           03  QSPC-REPO-FMNO-1-3                     PIC 9(03).
           03  QSPC-REPO-FMNO-1-4                     PIC 9(03).
           03  QSPC-REPO-TRCD-2                       PIC X(02).
           03  QSPC-REPO-FMNO-2-1                     PIC 9(03).
           03  QSPC-REPO-FMNO-2-2                     PIC 9(03).
           03  QSPC-REPO-FMNO-2-3                     PIC 9(03).
           03  QSPC-REPO-FMNO-2-4                     PIC 9(03).
           03  QSPC-REPO-TRCD-3                       PIC X(02).
           03  QSPC-REPO-FMNO-3-1                     PIC 9(03).
           03  QSPC-REPO-FMNO-3-2                     PIC 9(03).
           03  QSPC-REPO-FMNO-3-3                     PIC 9(03).
           03  QSPC-REPO-FMNO-3-4                     PIC 9(03).
           03  QSPC-CANCEL-ADDON-FG                   PIC X.
           03  QSPC-DEFAULT-O2-INS-COMP               PIC 9(2).
           03  QSPC-LC-ON-JUDGEMENTS                  PIC X.
           03  QSPC-DELQ-IB-AGE                       PIC 999.
           03  QSPC-DELQ-IB-MAX-DAYS                  PIC 999.
           03  QSPC-INSURANCE-WRITTEN                 PIC X.
           03  QSPC-INTERIM-COUPON                    PIC X.
           03  QSPC-AUTO-ASSESS-LCHG                  PIC X.
           03  QSPC-MAX-CASH-PROCEEDS                 PIC 9(6)V99.
           03  QSPC-ALLOW-ZERO-PROC                   PIC X.
           03  QSPC-BANKRULE-RATE                     PIC 9(4).
           03  QSPC-AUTO-SEND-TRCD-1                  PIC X(02).
           03  QSPC-AUTO-SEND-FMNO-1-1                PIC 9(03).
           03  QSPC-AUTO-SEND-FMNO-1-2                PIC 9(03).
           03  QSPC-AUTO-SEND-FMNO-1-3                PIC 9(03).
           03  QSPC-AUTO-SEND-FMNO-1-4                PIC 9(03).
           03  QSPC-AUTO-SEND-TRCD-2                  PIC X(02).
           03  QSPC-AUTO-SEND-FMNO-2-1                PIC 9(03).
           03  QSPC-AUTO-SEND-FMNO-2-2                PIC 9(03).
           03  QSPC-AUTO-SEND-FMNO-2-3                PIC 9(03).
           03  QSPC-AUTO-SEND-FMNO-2-4                PIC 9(03).
           03  QSPC-DLDISC-CONV-BF-PCNT               PIC 999V999.
           03  QSPC-UNITPER-CD                        PIC X.
           03  QSPC-RENEWAL-EDIT                      PIC X.
           03  QSPC-ROUND-MFEE                        PIC X.
           03  QSPC-ADV-TO-1ST-NO-CORR                PIC X.
           03  QSPC-LC-INT-ON-REPO-NON-PL             PIC X.
           03  QSPC-CONTRACTUAL-SET-NONACCRUAL        PIC 9(3).
           03  QSPC-CONTRACTUAL-RESET-ACCRUAL         PIC 9(3).
           03  QSPC-MONTHS-PAY-NAPP-COUNTER           PIC 99.
           03  QSPC-SHORT-PAY-THRESHOLD               PIC 9(3).
           03  QSPC-CASH-AVAIL-PERCENT                PIC 99.
           03  QSPC-O1-JOINT-FG                       PIC X.
           03  QSPC-NET-BAL-REQ                       PIC S9(3).
           03  QSPC-AMT-PROCEEDS-REQ                  PIC S9(7).
           03  QSPC-ALT-PREPAY-MONTHS                 PIC 9(2).
           03  QSPC-PERCENT-REGPAY-AVAIL              PIC 9(3).
           03  QSPC-ALT-DEFFRMLA                      PIC 99.
           03  QSPC-ALT-DEFMIN                        PIC 99V99.
           03  QSPC-ALT-DEFMAX                        PIC 9999V99.
           03  QSPC-NON-LOAN-FLAG                     PIC X.
           03  QSPC-SKIP-TRW                          PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      *****ASCII DUE DATE FILE, /USR/TMP/DUE_GGGG_MMDDYY
      *    GGGG = GROUP ID OR ENDING BR#, DATE = SYSTEM DATE
       01  DUE-PATH.
           03  FILLER          PIC X(9).
           03  DUE-PATH-DUE    PIC X(4) VALUE "DUE_".
           03  DUE-PATH-MMDDYY PIC 9(6).
           03  DUE-PATH-UNDER  PIC X    VALUE "_".
           03  DUE-PATH-GROUP  PIC X(4).
           03  DUE-PATH-BRNO   REDEFINES DUE-PATH-GROUP
                               PIC 9(4).
      *----------------------------------------------------------------

       01  DUE-KEY       PIC 9(6)     VALUE 0.
       01  WK-IFN        PIC XX       VALUE "WK".

      ******************************************************************
      *
      *    L I N E    W O R K E R S
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99    VALUE 0.
           03  LN-COUNT         PIC 999   VALUE 99.
           03  LN-WK            PIC 999   VALUE 0.

       01  PAGE-NUMBER          PIC 9(4)  VALUE 0.

      ******************************************************************
      *
      *     M I S C E L L E N O U S     W O R K E R S
      *
      ******************************************************************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  NEW-BRANCH           PIC X        VALUE "Y".

      * MORE-THAN-1-CLASS FLAG:
      * THIS FLAG IS AN ATTEMPT TO SUPPRESS CLASS TOTALS IF A BRANCH
      * HAS ONLY ONE CLASS.  (CLASS TOTALS = BRANCH TOTALS FOR ONE CLASS)
      * STARTING VALUE = "N"
      * IF CLASS BREAK IS FOUND THAN SET TO "Y"
      * ONLY RESET BACK TO "N" AFTER CLASS / BRANCH TOTALS ARE PRINTED
      * FOR EACH BRANCH.
       01  MORE-THAN-1-CLASS           PIC X        VALUE "N".
       01  MORE-THAN-1-CLASS-ON-REPORT PIC X        VALUE "N".

       01  RECORDS-FOUND        PIC X        VALUE SPACES.
           88  NO-RECORDS-FOUND              VALUE "N".

       01  SUMMARY-FG           PIC X        VALUE "N".
       01  INVALID-BW-FG        PIC X.

       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).

       01  BAL-WK               PIC 9(7)     COMP-3.
       01  TRANS-DATE           PIC 9(8)     COMP-3 VALUE 0.
       01  UNEARNED             PIC S9(7)V99 COMP-3 VALUE 0.
       01  EARNED               PIC S9(7)V99 COMP-3 VALUE 0.
       01  UNDISCOUNT           PIC S9(7)V99 COMP-3 VALUE 0.

       01  HOLD-LN-OWNBR        PIC 9(4)     VALUE 0.
       01  HOLD-BR-NAME         PIC X(40)    VALUE SPACES.
       01  HOLD-LN-CLASS        PIC 9(3)     VALUE 0.

       01  HOLD-SP1-KEY            VALUE SPACES.
           03  HOLD-SP-ORGST       PIC XX.
           03  HOLD-SP-SPRCLASS    PIC 999.
           03  HOLD-SP-SUBCLASS    PIC 99.
           03  HOLD-SP-LAWCODE     PIC 99.

       01  HOLD-LPS-NUM.
           03  H-LPS-CLASS      PIC XXX.
           03  H-LPS-NUM        PIC X(13).

       01 HOLD-BALANCES.
          03  HOLD-CURBAL       PIC S9(9)V99.

      ******************************************************************
      *
      *               T O T A L S
      *
      ******************************************************************
      *  NOTE:
      *    WK-TOTAL-TBL   MAPS WORKING STORAGE TOTAL-TBL(1) CLASS TOTALS
      *
      *    TOTAL-TBL:
      *       LEV(1) =  BY LOAN CLASS
      *          (2) =  BY BRANCH
      *          (3) =  BY COMPANY
      *
      *-----------------------------------------------------------------
      **(NON-MONTHLY ACCOUNTS)**
      *    (01) =  CURRENT NONMON
      *    (02) =   1  WEEKS
      *     ....
      *    (31) =  30+ WEEKS
      *-----------------------------------------------------------------
      *    T-AGETBL :
      *         TYP(1)       (2)             (3)         (4)        (5)
      *        CONTRACTUAL/RECENCY       PAID-AHEAD      REPO      P & L
      *    -------------------------------------------------------------
      **(MONTHLY ACCOUNTS)**
      * AGE(1)  =   30 DAYS           |  PAID-AHEAD  |  ONHAND  |  PL1
      *    (2)  =   60 DAYS           |  N/A         |  SOLD    |  PL2
      *    (3)  =   90 DAYS           |  N/A         |  N/A     |  PL3
      *    (4)  =  120 DAYS           |  N/A         |  N/A     |  PL4
      *    (5)  =  150 DAYS           |  N/A         |  N/A     |  PL5
      *    (6)  =  180 DAYS           |  N/A         |  N/A     |  N/A
      *    (7)  =  210 + DAYS         |  N/A         |  N/A     |  N/A
      *    (8)  =  <M> MISSPAYS       |  N/A         |  N/A     |  N/A
      *    (9)  =  <P> POTENTIAL 30'S |  N/A         |  N/A     |  N/A
      *    (10) =  CURRENT ACCOUNTS   |  N/A         |  N/A     |  N/A
      *-----------------------------------------------------------------
      *    T-TYPETBL:
      *       TYP(1) = CONTRACTUAL
      *          (2) = RECENCY
      *          (3) = PAID AHEAD
      *                CONTRACTUALLY CURRENT AND
      *                  DELINQ-RECENCY
      *          (4) = REPO
      *          (5) = P & L
      *
      ******************************************************************
       01  TOTAL-TABLE            VALUE ZEROS.
           03  TOTAL-TBL          OCCURS 3  TIMES.
               05  T-NM-TBL       OCCURS 31 TIMES.
                   07  T-NM-NO       PIC S9(7).
                   07  T-NM-BAL      PIC S9(10)V99.
                   07  T-NM-INT      PIC S9(7)V99.
                   07  T-NM-OTH      PIC S9(9)V99.
                   07  T-NM-OTH2     PIC S9(9)V99.
                   07  T-NM-UNEARN   PIC S9(10)V99.
                   07  T-NM-UNDISC   PIC S9(9)V99.
               05  T-AGETBL       OCCURS 10 TIMES.
                   07  T-TYPETBL  OCCURS 5  TIMES.
                       09  T-NO      PIC S9(7).
                       09  T-BAL     PIC S9(10)V99.
                       09  T-INT     PIC S9(7)V99.
                       09  T-OTH     PIC S9(9)V99.
                       09  T-OTH2    PIC S9(9)V99.
                       09  T-UNEARN  PIC S9(10)V99.
                       09  T-UNDISC  PIC S9(9)V99.
       01  TT-TOTALS.
           03  TT-NM-TBL.
               05  TT-NM-NO       PIC S9(7).
               05  TT-NM-BAL      PIC S9(10)V99.
               05  TT-NM-INT      PIC S9(7)V99.
               05  TT-NM-OTH      PIC S9(9)V99.
               05  TT-NM-OTH2     PIC S9(9)V99.
               05  TT-NM-UNEARN   PIC S9(10)V99.
               05  TT-NM-UNDISC   PIC S9(9)V99.
           03  TT-TYPETBL         OCCURS 5 TIMES.
               05  TT-NO          PIC S9(7)    COMP-3.
               05  TT-BAL         PIC S9(10)V99 COMP-3.
               05  TT-INT         PIC S9(7)V99 COMP-3.
               05  TT-OTH         PIC S9(9)V99 COMP-3.
               05  TT-OTH2        PIC S9(9)V99 COMP-3.
               05  TT-UNEARN      PIC S9(10)V99 COMP-3.
               05  TT-UNDISC      PIC S9(9)V99 COMP-3.

      * ----------------------------------------------------------------
      * SUB TOTALS    1 - DELINQUENCY
      *               2 - SLOW
      * ----------------------------------------------------------------
       01  SUB-TT-TOTALS.
           03  SUB-TT-TYPETBL     OCCURS 2 TIMES.
               05  SUB-TT-NO      PIC S9(7)    COMP-3.
               05  SUB-TT-BAL     PIC S9(10)V99 COMP-3.
               05  SUB-TT-INT     PIC S9(7)V99 COMP-3.
               05  SUB-TT-OTH     PIC S9(9)V99 COMP-3.
               05  SUB-TT-OTH2    PIC S9(9)V99 COMP-3.
               05  SUB-TT-UNEARN  PIC S9(10)V99 COMP-3.
               05  SUB-TT-UNDISC  PIC S9(9)V99 COMP-3.

       01  LEV                    PIC 9(6) COMP-3.
       01  AGE                    PIC 9(6) COMP-3.
           88  AGE-CONTRACTUAL             VALUE 1 THRU 10.
           88  AGE-RECENCY                 VALUE 1 THRU 10.
           88  AGE-PAID-AHEAD              VALUE 1.
           88  AGE-REPO                    VALUE 1 THRU 2.
           88  AGE-PL                      VALUE 1 THRU 5.

       01  TYP                    PIC 9(6) COMP-3.
           88  TYP-CONTRACTUAL             VALUE 1.
           88  TYP-RECENCY                 VALUE 2.
           88  TYP-PAID-AHEAD              VALUE 3.
           88  TYP-REPO                    VALUE 4.
           88  TYP-PL                      VALUE 5.

       01  SUB                    PIC 9(6) COMP-3.
       01  SUB1                   PIC 9(6) COMP-3.
       01  SUB2                   PIC 9(6) COMP-3.

       01  DD-TOTAL-TABLE.
           03  DD-TOT-TBL         OCCURS 32 TIMES.
               05  DD-NO          PIC S9(7)    COMP-3.
               05  DD-BAL         PIC S9(10)V99 COMP-3.
               05  DD-INT         PIC S9(7)V99 COMP-3.
               05  DD-OTH         PIC S9(9)V99 COMP-3.
               05  DD-OTH2        PIC S9(9)V99 COMP-3.
               05  DD-PAY         PIC S9(9)V99 COMP-3.

       01  NM-TOTAL-TABLE.
           03  NM-TOT-TBL         OCCURS 27 TIMES.
               05  NM-NO          PIC S9(7)    COMP-3.
               05  NM-BAL         PIC S9(10)V99 COMP-3.
               05  NM-INT         PIC S9(7)V99 COMP-3.
               05  NM-OTH         PIC S9(9)V99 COMP-3.
               05  NM-OTH2        PIC S9(9)V99 COMP-3.
               05  NM-PAY         PIC S9(9)V99 COMP-3.

      *----------------------------------------------------------------
      *    BINARY VALUE OF A LESS ONE.
      *----------------------------------------------------------------
       01  NM-A                   PIC 9(4) COMP VALUE 64.
       01  NM-B                   PIC 9(4) COMP VALUE 0.

       01  NM-BX REDEFINES NM-B.
           03  FILLER             PIC X.
           03  NM-TEST            PIC X.

       01  NM-SUB                 PIC 9(4) COMP-3.
       01  NM-PRINT               PIC X.

       01  RPSUB                 PIC 9(6) COMP.

       01  PLSUB                 PIC 9(6) COMP.

      *----------------------------------------------------------------
      *    AGEING DESCRIPTION TABLE
      *----------------------------------------------------------------
       01  AGE-DESC-TABLE.
           03  FILLER             PIC X(12) VALUE "    30 DAYS:".
           03  FILLER             PIC X(12) VALUE "    60 DAYS:".
           03  FILLER             PIC X(12) VALUE "    90 DAYS:".
           03  FILLER             PIC X(12) VALUE "   120 DAYS:".
           03  FILLER             PIC X(12) VALUE "   150 DAYS:".
           03  FILLER             PIC X(12) VALUE "   180 DAYS:".
           03  FILLER             PIC X(12) VALUE "   210+DAYS:".
           03  FILLER             PIC X(12) VALUE "   MIS-PAYS:".
           03  FILLER             PIC X(12) VALUE "   POT 30'S:".
           03  FILLER             PIC X(12) VALUE "   CURRENT :".

       01  AGE-DESC-TBL           REDEFINES AGE-DESC-TABLE.
           03  AGE-DESC           OCCURS 33 TIMES
                                  PIC X(12).


      *----------------------------------------------------------------
      *    NON-MONTHLY AGEING DESCRIPTIONS
      *----------------------------------------------------------------
       01  NM-AGE-SUB             PIC 9(2).
       01  NM-AGE-DESC.
           03  FILLER             PIC X(01) VALUE SPACES.
           03  NM-AGE-ALP         PIC X(03) VALUE SPACES.
           03  FILLER          REDEFINES NM-AGE-ALP.
               05  NM-AGE-NUM     PIC 9(02).
               05  NM-AGE-SIGN    PIC X(01).
           03  NM-AGE-WKS         PIC X(08) VALUE " WEEKS :".

      *----------------------------------------------------------------
      *    REPO DESCRIPTION TABLE
      *----------------------------------------------------------------
       01  REPO-DESC-TABLE.
           03  FILLER             PIC X(12) VALUE "     ONHAND:".
           03  FILLER             PIC X(12) VALUE "       SOLD:".

       01  REPO-DESC-TBL          REDEFINES REPO-DESC-TABLE.
           03  REPO-DESC          OCCURS 2 TIMES
                                  PIC X(12).

      *----------------------------------------------------------------
      *    P & L DESCRIPTION TABLE
      *----------------------------------------------------------------
       01  PL-DESC-TABLE.
           03  FILLER             PIC X(12) VALUE "         P1:".
           03  FILLER             PIC X(12) VALUE "         P2:".
           03  FILLER             PIC X(12) VALUE "         P3:".
           03  FILLER             PIC X(12) VALUE "         P4:".
           03  FILLER             PIC X(12) VALUE "         P5:".

       01  PL-DESC-TBL           REDEFINES PL-DESC-TABLE.
           03  PL-DESC           OCCURS 5 TIMES
                                 PIC X(12).

       01  CLASS-FG               PIC X(3)    VALUE "   ".
           88  CLASS-BEG                      VALUE "BEG".
           88  CLASS-END                      VALUE "END".
       01  DEALER-FG              PIC X(5)    VALUE "     ".
           88  DEALER-BEG                     VALUE "DLBEG".
           88  DEALER-END                     VALUE "DLEND".

      **DUE DATE TOTALS, 1= BRANCH, 2= GRAND, FOR ASCII FILE FOR WORLD
       01  DUE-DATE-TOTALS.
           03  DUE-DATE-BR-GRAND   OCCURS 2  TIMES.
               05  DUE-DATE-DAY    OCCURS 32 TIMES.
                   07  DUE-DATE-NO PIC 9(6).
      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  AGE-DATE        PIC 9(8).
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  BEG-DEALER      PIC 9(4)V99.
           03  END-DEALER      PIC 9(4)V99.
           03  BEG-AGE         PIC 9(4).
           03  END-AGE         PIC 9(4).
           03  CPOT            PIC X.
           03  CMISS           PIC X.
           03  BEG-BAL         PIC 9(7).
           03  END-BAL         PIC 9(7).
           03  BEG-RAGE        PIC 9(4).
           03  END-RAGE        PIC 9(4).
           03  RPOT            PIC X.
           03  OWNED           PIC X.
           03  PL              PIC X.
           03  PLACTIVE        PIC X.
           03  JD              PIC X.
           03  REPO            PIC X.
           03  LEGAL           PIC X.
           03  DEBT            PIC X.
           03  BANK            PIC X.
           03  RC              PIC X.
           03  ZBAL            PIC X.
           03  ALPHA-SUMMARY   PIC X.
           03  SUM-ONLY        PIC X.
           88  SUM-ONLY-VALID  VALUE "Y" "N" "G" "A" "C".
           03  BEG-LNAMT       PIC 9(6).
           03  END-LNAMT       PIC 9(6).
           03  PLTYPE          PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  UEARN           PIC X.
      * THIS WAS REVOLVING FLAG, IF WE REMOVE IT, THEY HAVE REBUILD BATCHES
           03  FILLER          PIC X.
           03  LOCAL-PRINT-FG  PIC X.
           03  BRULE           PIC X.
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  DATE-TYPE       PIC X.
           03  DEL-FLAG        PIC X.
           03  DUEDATE-SUMMARY PIC X.
           03  PRINT-RANGES    PIC X.

       01  VERSION-CONTROL     PIC X VALUE "I".

       01  SV-BEG-BR           PIC 9(4)   VALUE 9999.
       01  SV-END-BR           PIC 9(4)   VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-LOCAL-PRINT-FG   PIC X     VALUE SPACES.

      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  H-TITLE            PIC X(30)    VALUE
           "AGED TRIAL BALANCE     AS OF: ".
           03  H-AGEDATE        PIC X(10).
           03  FILLER           PIC X(3).

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-BRNO      PIC 9(4).
           03  H-BRNOX     REDEFINES H-BRNO PIC X(4).
           03  FILLER      PIC X        VALUE " ".
           03  H-BRDESC    PIC X(20).
           03  FILLER      PIC X(77)    VALUE " ".
           03  FILLER      PIC X(20)    VALUE
           "99.99.99 - LOAN DATE".

       01  HEAD4.
           03  FILLER      PIC X(8)     VALUE "CLASS  :".
           03  FILLER      PIC XX       VALUE "  ".
           03  H-LCNO      PIC Z99.
           03  H-LCNOX     REDEFINES H-LCNO PIC XXX.
           03  FILLER      PIC X        VALUE " ".
           03  H-LCDESC    PIC X(20).
           03  FILLER      PIC XX       VALUE "  ".
           03  H-LCBEG     PIC XXX.
           03  H-LCDASH    PIC XXX       VALUE "   ".
           03  H-LCEND     PIC XXX.

       01  HEAD5.
           03  FILLER      PIC X(44)    VALUE " ".
           03  FILLER      PIC X(50)    VALUE
               "LAST  REC-    RECENCY   UNEARNED  CONTRACTUAL     ".
           03  FILLER      PIC X(37)    VALUE
               "PAID        ------  BALANCES  -------".

       01  HEAD6.
           03  FILLER      PIC X(35)    VALUE
               "ACCOUNT NO.  DUE BORROWER          ".
           03  FILLER      PIC X(46)    VALUE
               "DEALER PAYMENT ENCY    PROFILE    CHARGES    P".
           03  FILLER      PIC X(41)    VALUE
               "ROFILE   AGE THRU   APR OT-DUE INT/LC-DUE".
           03  HEAD6-PRIN  PIC X(10)    VALUE
               " PRINCIPAL".

      ******************************************************************
      *
      *    D E T A I L   L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM  PIC X(13).
           03  FILLER      PIC X.
           03  D-DUEDAY    PIC 99.
           03  D-BW-BAD    PIC X.
           03  D-NAME      PIC X(16).
           03  FILLER      PIC X.
           03  D-DLNO      PIC ZZZ9.99.
           03  FILLER      PIC X.
           03  D-PAYDATE   PIC X(08).
           03  FILLER      REDEFINES D-PAYDATE.
               05  FILLER  PIC XX.
               05  D-DASH1 PIC X.
               05  FILLER  PIC XX.
               05  D-DASH2 PIC X.
               05  FILLER  PIC XX.
           03  D-RECENCYX.
               05  D-RECENCY   PIC ZZZZ.
           03  FILLER      PIC X.
           03  D-PYRECPRO  PIC 9(12).
           03  D-XPYRECPRO REDEFINES D-PYRECPRO PIC X(12).
           03  D-UNEARNED  PIC ZZZZZ9.99-.
           03  D-PROFILE   PIC 9(12).
           03  D-XPROFILE  REDEFINES D-PROFILE PIC X(12).
           03  D-CONTUALX.
               05  D-CONTUAL   PIC ZZZZ.
           03  FILLER      PIC X.
           03  D-PDTHRU    PIC 9999.
           03  FILLER      PIC X.
           03  D-APR       PIC ZZ9.99.
           03  D-OTHBAL    PIC ZZZZ9.99-.
           03  D-INTBAL    PIC ZZZZ9.99-.
           03  D-CURBAL    PIC ZZZZZ9.99.

       01  SUMMARY-LINE1 REDEFINES DETAIL-LINE.
           03  S-SUMNAME        PIC X(20).
       01  SUMMARY-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(46).
           03  S-AGETYPE        PIC X(80).
       01  SUMMARY-LINE3 REDEFINES DETAIL-LINE.
           03  S-NAME           PIC X(12).
           03  FILLER           REDEFINES S-NAME.
               05  FILLER       PIC X(9).
               05  S-LASTINITIAL.
                   07  S-NAMEDUE    PIC 99.
               05  S-NAMECOLON  PIC X.
           03  S-NO             PIC ZZZZZZZ9-.
           03  S-NO-P           PIC ZZ9.99-.
           03  S-OTH            PIC Z(6)9.99-.
           03  S-OTH2           PIC Z(7)9.99-.
           03  S-INT            PIC Z(8)9.99-.
           03  S-INT-P          PIC ZZ9.99.
           03  FILLER           PIC X.
           03  S-BAL            PIC Z(10)9.99-.
           03  S-BAL-P          PIC ZZ9.99.
           03  FILLER           PIC X.
           03  S-UNEARN         PIC Z(9)9.99-.
           03  S-UNEARN-P       PIC ZZ9.99.
           03  FILLER           PIC X.
           03  S-UNDISC         PIC Z(7)9.99-.
           03  S-UNDISC-P       PIC ZZ9.99.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(20).
           03  RANGE-SELECTION  PIC X(112).
           03  RANGE-DEALER REDEFINES RANGE-SELECTION PIC Z(4).99.

       01  SUMMARY1.
           03  FILLER           PIC X(15)  VALUE SPACES.
           03  FILLER           PIC X(53)  VALUE
           "   NO----%      OTH DUE    OTH2 DUE   INT/LC DUE----%".
           03  FILLER           PIC X(42)  VALUE
           "      BALANCE DUE----%       UNEARNED----%".
           03  FILLER           PIC X(18) VALUE
           "     UNE DISC----%".

       01  SUMMARY2.
           03  FILLER           PIC X(15)  VALUE SPACES.
           03  FILLER           PIC X(53)  VALUE
           "   NO----%      OTH DUE    OTH2 DUE   INT/LC DUE----%".
           03  FILLER           PIC X(42)  VALUE
           "      BALANCE DUE----%        PMT DUE----%".


      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
*NEW*  COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".

*NEW*  COPY "LIBGB/GETENVW.CPY".
*NEW*  COPY "LIBGB/CONAME_EXT.CPY".
*NEW*  COPY "LIBGB/SCREENW.CPY".
*NEW*  COPY "LIBLP/LNAGED_DEF.CPY".
*NEW*  COPY "LIBLP/LNAGED_WKS.CPY".
*NEW*  COPY "LIBGB/HELPLINK.CPY".
*NEW*  COPY "LIBGB/PRU_LNK.CPY".


      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".

*NEW*  SCREEN SECTION.
*NEW*  COPY "LIBLP/LNAGED_SCN.CPY".

       PROCEDURE DIVISION
*NEW*      USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 WK-FILE DUE-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE DUE-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *     M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
      D    STOP MESS.
           PERFORM SQL-CONNECT.
           MOVE FORM-PATHNAME TO FORM-PATH.
*NEW*      MOVE "LNAGED" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
*NEW*      IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           IF LOCAL-PRINT-FG = "Y"
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO NEXT-GROUP-LOC.

           MOVE "Y" TO NEW-BRANCH.

*LIO*      MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
*LIO*      MOVE BR-MACHINE                 TO FILPATH-MACHINE.
*LIO*      MOVE BR-DATA-PATH               TO FILPATH-DATA.

*LIO*      MOVE "B"                        TO BW-PATH-OVERRIDE
*LIO*                                         LN-PATH-OVERRIDE
*LIO*                                         LTP-PATH-OVERRIDE.

      * 4/23/98 BAH, THIS WAS COMMENTED OUT, BUT THIS IS WHERE IT NEEDS
      * TO BE, THE OTHER ONE, IN END-OF-GROUP, WAS CAUSING THE LAST CLASS
      * TOTALS TO NOT PRINT IF YOU ONLY HAD 1 BRANCH, 2 OR MORE CLASSES !!
      * 042898 MJD, THIS DOES NOT BELONG HERE. IF FLAG IS SET HERE, YOU
      * WILL NEVER SEE TOTALS FOR LAST CLASS IN EACH BRANCH. IT SHOULD NOT
      * BE RESET TO 'N' UNTIL AFTER CLASS BRANCH TOTALS HAVE BEEN PRINTED FOR
      * PREVIOS BRANCH.
      *        MOVE "N"       TO MORE-THAN-1-CLASS.

      *    PERFORM OPEN-BW1-FILE.
      *    PERFORM OPEN-LN3-FILE.

           PERFORM LOAD-BW1-FILE.
           PERFORM LOAD-LN3-FILE.

           MOVE BR-NO     TO LN-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS.
           MOVE 0         TO LN-ACCTNO.

           PERFORM START-LN3-FILE.

      *----------------------------------------------------------------
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
*L2G       IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           PERFORM TRAP-INTERRUPT.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              GO TO NEXT-LN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  POCD - **  IS A DELETED CONTRACT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF LN-DLNO < BEG-DEALER OR LN-DLNO > END-DEALER
              GO TO NEXT-LN.

      *  GLB  07/12/99 MOVED UP HERE SO BW-DATE WOULD BE AVAILABLE
           PERFORM PROCESS-BORROWER.

      * VERIFY BETWEEN BEGINNING AND ENDING DATES

           MOVE LN-LOANDATE TO SYS-DATE.
           IF DATE-TYPE = "E"
              MOVE LN-ENTDATE TO SYS-DATE.
           IF DATE-TYPE = "B"
              MOVE BW-REFDATE TO SYS-DATE
              IF BEG-DATE = 0
      * DONT TEST DATE RANGE AT ALL IF BW-REFDATE IS 0 AND BEG-DATE IS
      * 0, YOU ALREADY MATCH AND ENDDATE WILL BOMB OUT WITH A 0
                 IF (BW-REFDATE = 0)
                    GO TO TEST-LNAMT
                 ELSE
                    GO TO TEST-ENDDATE.

           IF SYS-DATE < BEG-DATE
               GO TO NEXT-LN.

       TEST-ENDDATE.
           MOVE LN-LOANDATE TO SYS-DATE.
           IF DATE-TYPE = "E"
              MOVE LN-ENTDATE TO SYS-DATE.
           IF DATE-TYPE = "B"
              MOVE BW-REFDATE TO SYS-DATE.

            IF SYS-DATE > END-DATE
               GO TO NEXT-LN.


       TEST-LNAMT.
      * NEED FOR LOANS THAT ARE 999999.??
           IF BEG-LNAMT = 0 AND END-LNAMT = 999999
              NEXT SENTENCE
           ELSE
              IF LN-SUM-LNAMT < BEG-LNAMT OR LN-SUM-LNAMT > END-LNAMT
                 GO TO NEXT-LN.

           IF PL = "O"
              IF LN-PLDATE = 0
                 GO TO NEXT-LN.

           IF PL = "S"
              IF LN-PLDATE NOT = 0
                 GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "A"
                 IF LN-PLCD NOT = "P"
                    GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "I"
                 IF LN-PLCD NOT = "I"
                    IF LN-PLCD NOT = "W"
                       GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLTYPE NOT = "A"
                 IF LN-PLTYPE NOT = PLTYPE
                    GO TO NEXT-LN.

           IF LN-SALEFIN NOT = "Y"
              GO TO CONT-LN.

           IF RC = "F"
              IF LN-DLRECOURSE NOT = "F"
                 GO TO NEXT-LN.

           IF RC = "P"
              IF LN-DLRECOURSE NOT = "P"
                 GO TO NEXT-LN.

           IF RC = "N"
              IF LN-DLRECOURSE NOT = "N"
                 GO TO NEXT-LN.


      *----------------------------------------------------------------
       CONT-LN.
           IF OWNED = "O"
              IF LN-SERVACCT = "Y"
                 GO TO NEXT-LN.

           IF OWNED = "S"
              IF LN-SERVACCT = "N"
                 GO TO NEXT-LN.

           IF JD = "O"
              IF LN-JDDATE = 0
                 GO TO NEXT-LN.

           IF JD = "S"
              IF LN-JDDATE NOT = 0
                 GO TO NEXT-LN.

           IF REPO = "O"
              IF NOT (LN-REPOCD = "X" OR "S")
                 GO TO NEXT-LN.

           IF REPO = "S"
              IF LN-REPOCD = "X" OR "S"
                 GO TO NEXT-LN.

           IF BANK = "O"
              IF LN-BNKRPTDATE = 0
                 GO TO NEXT-LN.

           IF BANK = "S"
              IF LN-BNKRPTDATE NOT = 0
                 GO TO NEXT-LN.

           IF LEGAL = "O"
              IF LN-LEGALDATE = 0
                 GO TO NEXT-LN.

           IF LEGAL = "S"
              IF LN-LEGALDATE NOT = 0
                 GO TO NEXT-LN.

           IF DEBT = "O"
              IF LN-DEBTAIDATE = 0
                 GO TO NEXT-LN.

           IF DEBT = "S"
              IF LN-DEBTAIDATE NOT = 0
                 GO TO NEXT-LN.

           IF ZBAL = "O"
              IF ( LN-CURBAL NOT = 0 ) OR
                 ( LN-INTBAL NOT = 0 ) OR
                 ( LN-LCBAL  NOT = 0 ) OR
                 ( LN-OTHBAL NOT = 0 )
                     GO TO NEXT-LN.

           IF ZBAL = "S"
              IF LN-CURBAL = 0
                 IF LN-INTBAL = 0
                    IF LN-LCBAL = 0
                       IF LN-OTHBAL = 0
                          GO TO NEXT-LN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NEED FOR LOANS THAT ARE 999999.??
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BEG-BAL = 0 AND END-BAL = 9999999
              NEXT SENTENCE
           ELSE
              COMPUTE BAL-WK ROUNDED =
                      LN-CURBAL + LN-INTBAL + LN-LCBAL + LN-OTHBAL
              IF ( BAL-WK < BEG-BAL ) OR ( BAL-WK > END-BAL )
                 GO TO NEXT-LN.

           MOVE LN-ORGST     TO HOLD-SP-ORGST.
           MOVE LN-SPRCLASS  TO HOLD-SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO HOLD-SP-SUBCLASS.
           MOVE LN-LAWCODE   TO HOLD-SP-LAWCODE.

           IF HOLD-SP1-KEY NOT = SP1-KEY
              MOVE HOLD-SP1-KEY TO SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 STRING "MISSING SPR: ",LN-OWNBR,LN-CLASS,LN-ACCTNO
                               DELIMITED BY "  " INTO MESS
                 PERFORM SEND-MESS
                 GO TO IO-ERROR.

           IF BRULE = "T"
              MOVE SP-TRWBANKRULE TO SP-BANKRULE
              MOVE SP-TRWCONTPDTH TO SP-CONTPDTH
              MOVE SP-TRWBANKUNITPER-CD TO SP-BANKUNITPER-CD.

           PERFORM COMPUTE-RECENCY.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-PAID-AHEAD.
           PERFORM COMPUTE-REPO.
           PERFORM COMPUTE-PL.

           IF NOT (BEG-AGE = 0 AND END-AGE = 9999)
              IF CONTRACTUAL < BEG-AGE OR CONTRACTUAL > END-AGE
                 GO TO NEXT-LN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST CONTRACTUAL: POTENTIALS AND MISS PAYS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF CPOT = "S"
              IF CPOT30 = "Y"
                 GO TO NEXT-LN
              END-IF
           ELSE
              IF CPOT = "O"
                 IF CPOT30 NOT = "Y"
                    GO TO NEXT-LN.

           IF CMISS = "S"
              IF CPOT30 = "M"
                 GO TO NEXT-LN
              END-IF
           ELSE
              IF CMISS = "O"
                 IF CPOT30 NOT = "M"
                    GO TO NEXT-LN.


           IF NOT (BEG-RAGE = 0 AND END-RAGE = 9999)
              IF RECDEL < BEG-RAGE OR RECDEL > END-RAGE
                 GO TO NEXT-LN.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST RECENCY: POTENTIALS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF RPOT = "S"
              IF RDPOT30 NOT = " "
                  GO TO NEXT-LN
              END-IF
           ELSE
              IF RPOT = "O"
                 IF RDPOT30 = " "
                    GO TO NEXT-LN.

           PERFORM COMPUTE-EARNINGS.
      *    MOVE LN-TOTERN       TO EARNED.
           MOVE EARN-TOT-UNEARN TO UNEARNED.

      * TEST FOR UNEARNED PRINT OPTION
           IF UEARN = "O"
              IF UNEARNED NOT = 0
                 GO TO NEXT-LN.

           IF UEARN = "S"
              IF UNEARNED = 0
                 GO TO NEXT-LN.

      * UNEARNED TOTALS WILL BE LESS UNEARNED DISCOUNT
           SUBTRACT EARN-UNEARN(5) FROM EARN-TOT-UNEARN
               GIVING UNEARNED.
           MOVE EARN-UNEARN(5) TO UNDISCOUNT.

           IF FIRST-TIME = "Y"
              MOVE BR-NAME  TO HOLD-BR-NAME
              MOVE LN-OWNBR TO HOLD-LN-OWNBR
              MOVE LN-CLASS TO HOLD-LN-CLASS
              MOVE 0        TO LC-CODE
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF LN-OWNBR NOT = HOLD-LN-OWNBR
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              IF GP-I-AM-MULL
                 MOVE "Y"   TO MORE-THAN-1-CLASS
              ELSE
                 MOVE "N"   TO MORE-THAN-1-CLASS
              END-IF
              MOVE LN-OWNBR TO HOLD-LN-OWNBR
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF LN-CLASS NOT = HOLD-LN-CLASS
              MOVE "Y" TO MORE-THAN-1-CLASS
              MOVE "Y" TO MORE-THAN-1-CLASS-ON-REPORT
              PERFORM CLASS-TOTALS
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-LN.

      *----------------------------------------------------------------
       END-OF-GROUP.
      *    PERFORM CLOSE-LN3-FILE.
      *    PERFORM CLOSE-BW1-FILE.

           IF LOCAL-PRINT-FG = "Y"
              IF FIRST-TIME NOT = "Y"
                 PERFORM CLASS-TOTALS
                 PERFORM BRANCH-TOTALS
                 IF GP-I-AM-MULL
                    MOVE "Y" TO MORE-THAN-1-CLASS
                 ELSE
                    MOVE "N" TO MORE-THAN-1-CLASS
                 END-IF
                 MOVE "Y" TO FIRST-TIME
                 MOVE 0 TO PAGE-NUMBER
              ELSE
                 MOVE 0 TO PAGE-NUMBER.

           IF DUEDATE-SUMMARY = "F"
              MOVE 1 TO LEV
              PERFORM UPDATE-DUE-DATE-FILE
              MOVE ZEROS TO DUE-DATE-BR-GRAND(1).

      * BAH 4/23/98 COMMENTED OUT, NEEDS TO STAY "Y" UNTIL YOU READ
      * THE NEXT BRANCH. IF YOU HIT END OF FILE, YOUR LAST CLASS TOTALS
      * DIDN'T PRINT !!
      * MJD 042898 DO NOT RESET HERE. MUST STAY "Y" UNTIL LAST
      * CLASS IS TOTALD OUT FOR THE BRANCH.
      *    MOVE "N" TO MORE-THAN-1-CLASS.

           GO TO NEXT-GROUP-LOC.
      *----------------------------------------------------------------
       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF LOCAL-PRINT-FG = "N"
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              PERFORM COMPANY-TOTALS
              IF PRINT-RANGES = "Y"
              PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *   I N I T I A L I Z A T I O N   *
      *
      ******************************************************************
       INITIALIZATION SECTION.
*NEW*      MOVE LNAGED-QUEUE-POS TO Q-POS.
*NEW*      MOVE LNAGED-COPIES-POS TO C-POS.
*NEW*      MOVE LNAGED-PRU-POS   TO P-POS.
*NEW*      MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES      TO RECORDS-FOUND.

           MOVE AGE-DATE    TO AGEING-DATE TRANS-DATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE    TO H-AGEDATE.

           PERFORM ZERO-TT-TOTALS.
           PERFORM ZERO-DD-TOTALS.
           PERFORM ZERO-NM-TOTALS.

      *----------------------------------------------------------------
       SET-HEAD.
*NEW*      MOVE EXT-CONAME-CONAME  TO H-NAME.
*NEW*      MOVE EXT-CONAME-SYSDATE TO H-DATE.
*NEW*      MOVE EXT-CONAME-USER-ID TO H-USERID.
*NEW*      IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

*NEW* * BECAUSE WE USED TO DO XFER AND WRFORM AND NOT CHECK STAT IF MESS
*NEW* * DIDNT EXIST, DO NOT WANT IT TO DISPLAY FOR BATCH
*NEW*      IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS...................." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE LOCAL-PRINT-FG TO SV-LOCAL-PRINT-FG
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG LOCAL-PRINT-FG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
*NEW*            MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           IF LOCAL-PRINT-FG NOT = "Y"
              PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           IF DUEDATE-SUMMARY = "F"
              MOVE MKTEMP-DEFAULT TO DUE-PATH
              MOVE "DUE_"         TO DUE-PATH-DUE
              IF GROUP-FLAG = "Y"
                 MOVE ENT-GROUP   TO DUE-PATH-GROUP
              ELSE
                 MOVE END-BRANCH  TO DUE-PATH-BRNO
              END-IF
              MOVE "_"            TO DUE-PATH-UNDER
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-MMDDYY
              MOVE DATE-MMDDYY    TO DUE-PATH-MMDDYY
              PERFORM OPEN-DUE-FILE-OUTPUT
              PERFORM CLOSE-DUE-FILE
              PERFORM OPEN-DUE-FILE.

           IF GP-I-AM-MULL
              MOVE "Y" TO MORE-THAN-1-CLASS
                          MORE-THAN-1-CLASS-ON-REPORT
           ELSE
              MOVE "N" TO MORE-THAN-1-CLASS
                          MORE-THAN-1-CLASS-ON-REPORT.

           PERFORM ENABLE-INTERRUPT.

      ******************************************************************
      *
      *    SETUP DATES FOR BATCH PROCESS
      *
      ******************************************************************
       EO-EXEC-AUDIT SECTION.
*NEW*      MOVE EXT-EOBUF-DATE2 TO AGE-DATE.
           MOVE JULIAN-BEG TO BEG-DATE.
           MOVE JULIAN-END TO END-DATE.

           IF DEL-FLAG NOT = "Y"
              GO TO EXIT-EO-EXEC-AUDIT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SETUP NEXT MONTH DATE (WORLD ONLY, FOR CORRECT EOM AGEING):
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EXT-EOBUF-DATE2 TO NDTE-DATE.
           MOVE 1               TO NDTE-DD.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO AGE-DATE.

       EXIT-EO-EXEC-AUDIT.
           EXIT.
      *----------------------------------------------------------------
      ******************************************************************
      *
      *       ENTER REPORT PARAMETERS
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.
           MOVE "L" TO DATE-TYPE.
      * DO NOT WIPE OUT PAGE 2 PARAMETER SETTING IF IN BATCH MAINT!
           IF EXT-ROUTE-BUF = "00"
              IF GP-I-AM-MULL
                 MOVE "S  A010RP." TO VDU
                 MOVE "I" TO VDUBUF
                 PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENTER-PARM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * FOR RE-ENTRY ON F4 FROM ACCEPT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF VDUSTATUS = "1U" GO TO SUMYN.

      *----------------------------------------------------------------
       AGEDT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * BATCH WILL USE DATE RAN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060AGEDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGEDT.

           MOVE VDU-DATE-YYYYMMDD TO AGE-DATE.

           IF AGE-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000AGEDATE." TO VDU
              MOVE DATE-YYYYMMDD TO VDU-DATE-YYYYMMDD AGE-DATE
              PERFORM SCREENIO.

           GO TO BEGDATE-01.

       DATE-TYPE.
           MOVE "E  A010DATETYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AGEDT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DATE-TYPE.
           MOVE VDUBUF TO DATE-TYPE.
           IF DATE-TYPE NOT = "E" AND NOT = "L" AND NOT = "B"
              MOVE "'E'=ENTRYDATE 'L'=LOANDATE 'B'=BORROWER REFDATE"
              TO MESS
              PERFORM SEND-MESS
              GO TO DATE-TYPE.


      *---------------------------------------------------------------
       BEGDATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DATE-TYPE.
           IF VDUSTATUS = "16" GO TO ALL-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDATE-01.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
      *-----------------------------------------------------------
       ENDDATE-02.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGDATE-01.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO ENDDATE-02.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

      * MAKE SURE THE DATE RANGE IS VALID.

           IF NOT (DATE-TYPE = "B" AND BEG-DATE = 0)
              IF END-DATE < BEG-DATE
                 MOVE "INVALID DATE RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO BEGDATE-01.

           GO TO BR-01.

      *----------------------------------------------------------
       ALL-DATE.

           MOVE "S  8080DATE1." TO VDU.
           MOVE JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DATE2."   TO VDU.
           MOVE JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENDDATE-02.

           IF VDUSTATUS = "1<"
              MOVE GRSCAN-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              MOVE SCAN-BUF TO GRSCAN-BUF
              IF SCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.

           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              PERFORM INVALID-RANGE
              GO TO BR-01.

           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0             TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N"           TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.

           MOVE VDUNUM0 TO BEG-CLASS.

      *----------------------------------------------------------------
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.

           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO CL-01.

           GO TO DL-01.

      *----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0             TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99            TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       DL-01.
           MOVE "ENNN042DL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1<"
              MOVE "DLBEG" TO DEALER-FG
              PERFORM DEALER-SCAN
              IF IO-FG NOT = 0
              GO TO DL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DL-01.

           MOVE VDUNUM2 TO BEG-DEALER.

      *----------------------------------------------------------------
       DL-02.
           MOVE "ENNN042DL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DL-01.
           IF VDUSTATUS = "1<"
              MOVE "DLEND" TO DEALER-FG
              PERFORM DEALER-SCAN
              IF IO-FG NOT = 0
              GO TO DL-02.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DL-02.

           MOVE VDUNUM2 TO END-DEALER.

           IF BEG-DEALER > END-DEALER
              PERFORM INVALID-RANGE
              GO TO DL-01.

           GO TO AGE-01.

      *----------------------------------------------------------------
       ALL-DL.
           MOVE "SNNN042DL1." TO VDU.
           MOVE 0             TO BEG-DEALER VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SNNN042DL2." TO VDU.
           MOVE 9999.99       TO END-DEALER VDUNUM2.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       AGE-01.
           MOVE "ENNN040AGE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLAGE.
           IF VDUSTATUS = "1U" GO TO DL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGE-01.

           MOVE VDUNUM0 TO BEG-AGE.

      *----------------------------------------------------------------
       AGE-02.
           MOVE "ENNN040AGE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AGE-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGE-02.

           MOVE VDUNUM0 TO END-AGE.

           IF BEG-AGE > END-AGE
              PERFORM INVALID-RANGE
              GO TO AGE-01.

           IF NOT ( BEG-AGE < 30)
               MOVE "SNNA010C-POT." TO VDU
               MOVE " " TO VDUBUF
               PERFORM SCREENIO
               MOVE "SNNA010C-MISS." TO VDU
               MOVE " " TO VDUBUF
               PERFORM SCREENIO
               GO TO RAGE01.
           GO TO CPOTYN.

      *----------------------------------------------------------------
       ALLAGE.
           MOVE "SNNN040AGE1." TO VDU.
           MOVE 0              TO BEG-AGE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040AGE2." TO VDU.
           MOVE 9999           TO END-AGE VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       CPOTYN.
           MOVE "E  A010C-POT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AGE-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CPOTYN.

           MOVE VDUBUF TO CPOT.
           IF CPOT = "S"
                 MOVE "SNNA010C-MISS." TO VDU
                 MOVE "S" TO VDUBUF
                 PERFORM SCREENIO
                 GO TO RAGE01.
           IF CPOT NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO CPOTYN.

      *----------------------------------------------------------------
       CMISSYN.
           MOVE "E  A010C-MISS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CPOTYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CMISSYN.

           MOVE VDUBUF TO CMISS.

           IF CMISS NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO CMISSYN.

      *----------------------------------------------------------------
       RAGE01.
           MOVE "ENNN040RAGE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALRAGE.
           IF VDUSTATUS = "1U"
                IF NOT (BEG-AGE < 30)
                   GO TO AGE-02
                ELSE
                IF CPOT = "S"
                    GO TO CPOTYN
                ELSE
                    GO TO CMISSYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RAGE01.

           MOVE VDUNUM0 TO BEG-RAGE.

      *----------------------------------------------------------------
       RAGE02.
           MOVE "ENNN040RAGE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RAGE01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RAGE02.

           MOVE VDUNUM0 TO END-RAGE.

           IF BEG-RAGE > END-RAGE
              PERFORM INVALID-RANGE
              GO TO RAGE01.

           IF NOT (BEG-RAGE < 30)
               MOVE "SNNA010R-POT." TO VDU
               MOVE " " TO VDUBUF
               PERFORM SCREENIO
               GO TO BAL01.
           GO TO RPOTYN.

      *----------------------------------------------------------------
       ALRAGE.
           MOVE "SNNN040RAGE1." TO VDU.
           MOVE 0               TO BEG-RAGE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040RAGE2." TO VDU.
           MOVE 9999            TO END-RAGE VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       RPOTYN.
           MOVE "E  A010R-POT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RAGE02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RPOTYN.

           MOVE VDUBUF TO RPOT.

           IF RPOT NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO RPOTYN.

      *----------------------------------------------------------------
       BAL01.
           MOVE "ENNN070BB1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLBAL.
           IF VDUSTATUS = "1U"
                IF NOT (BEG-RAGE < 30)
                   GO TO RAGE02
                ELSE
                   GO TO RPOTYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BAL01.

           MOVE VDUNUM0 TO BEG-BAL.

      *----------------------------------------------------------------
       BAL02.
           MOVE "ENNN070BB2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BAL01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BAL02.

           MOVE VDUNUM0 TO END-BAL.

           IF BEG-BAL > END-BAL
              PERFORM INVALID-RANGE
              GO TO BAL01.

           GO TO AMT01.

      *----------------------------------------------------------------
       ALLBAL.
           MOVE "SNNN070BB1." TO VDU.
           MOVE 0             TO BEG-BAL VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN070BB2." TO VDU.
           MOVE 9999999       TO END-BAL VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       AMT01.
           MOVE "ENNN060LA1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLAMT.
           IF VDUSTATUS = "1U" GO TO BAL02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT01.

           MOVE VDUNUM0 TO BEG-LNAMT.

      *----------------------------------------------------------------
       AMT02.
           MOVE "ENNN060LA2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT02.

           MOVE VDUNUM0 TO END-LNAMT.

           IF BEG-LNAMT > END-LNAMT
              PERFORM INVALID-RANGE
              GO TO AMT01.

           GO TO TYPEYN.

      *----------------------------------------------------------------
       ALLAMT.
           MOVE "SNNN060LA1." TO VDU.
           MOVE 0             TO BEG-LNAMT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060LA2." TO VDU.
           MOVE 999999        TO END-LNAMT VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       TYPEYN.
           MOVE "E  A010TYPE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TYPEYN.

           MOVE VDUBUF TO OWNED.

           IF OWNED NOT = "O" AND NOT = "S" AND NOT = "B"
              GO TO TYPEYN.

      *----------------------------------------------------------------
       PLYN.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TYPEYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLYN.

           MOVE VDUBUF TO PL.

           IF PL = "I" OR "O"
              GO TO PLTYPEYN.

           IF PL NOT = "S" GO TO PLYN.

           MOVE "S  A010ACTIVE." TO VDU.
           MOVE "B"              TO PLACTIVE VDUBUF.
           PERFORM SCREENIO.
           GO TO BANKRULE.

      *----------------------------------------------------------------
       PLTYPEYN.
           MOVE "E  A010PLTYPE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PLYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLTYPEYN.

           MOVE VDUBUF TO PLTYPE.

           IF NOT (PLTYPE = "A" OR "P" OR "2" OR "3" OR "4" OR "5")
              GO TO PLTYPEYN.

      *----------------------------------------------------------------
       ACTVYN.
           MOVE "E  A010ACTIVE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PLTYPEYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ACTVYN.

           MOVE VDUBUF TO PLACTIVE.

           IF PLACTIVE NOT = "I" AND NOT = "A" AND NOT = "B"
              GO TO ACTVYN.

       BANKRULE.
           MOVE "E  A010BRULE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ACTVYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BANKRULE.
           MOVE VDUBUF TO BRULE.
           IF BRULE = "I" GO TO RECRSE.
           IF BRULE NOT = "T" GO TO BANKRULE.

      *----------------------------------------------------------------
       RECRSE.
           MOVE "E  A010RC." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BANKRULE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RECRSE.

           MOVE VDUBUF TO RC.

           IF RC NOT = "F"
              AND NOT = "P"
               AND NOT = "N"
                AND NOT = "A"
                    GO TO RECRSE.

      *----------------------------------------------------------------
       JDYN.
           MOVE "E  A010JD." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RECRSE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO JDYN.

           MOVE VDUBUF TO JD.

           IF JD NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO JDYN.

      *----------------------------------------------------------------
       REPOYN.
           MOVE "E  A010RP." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO JDYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REPOYN.

           MOVE VDUBUF TO REPO.

           IF REPO NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO REPOYN.

      *----------------------------------------------------------------
       BANKYN.
           MOVE "E  A010BK." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REPOYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BANKYN.

           MOVE VDUBUF TO BANK.

           IF BANK NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO BANKYN.

      *----------------------------------------------------------------
       LEGLYN.
           MOVE "E  A010LG." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BANKYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LEGLYN.

           MOVE VDUBUF TO LEGAL.

           IF LEGAL NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO LEGLYN.

      *----------------------------------------------------------------
       DEBTYN.
           MOVE "E  A010DA." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO LEGLYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DEBTYN.

           MOVE VDUBUF TO DEBT.

           IF DEBT NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO DEBTYN.

      *----------------------------------------------------------------
       ZBALYN.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DEBTYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ZBALYN.

           MOVE VDUBUF TO ZBAL.

           IF ZBAL = "I"     GO TO UEARNYN.
           IF ZBAL = "S"     GO TO UEARNYN.
           IF ZBAL NOT = "O" GO TO ZBALYN.

      *----------------------------------------------------------------
       UEARNYN.
           MOVE "E  A010UEARN." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ZBALYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO UEARNYN.

           MOVE VDUBUF TO UEARN.

           IF UEARN = "I"     GO TO ENTER-DEL-FLAG.
           IF UEARN = "S"     GO TO ENTER-DEL-FLAG.
           IF UEARN NOT = "O" GO TO UEARNYN.

      *----------------------------------------------------------------
       ENTER-DEL-FLAG.
           MOVE "S  A200DELPROMPT:P." TO VDU.
           MOVE "BEG MO DELINQENCY?: "    TO VDUBUF.
           PERFORM SCREENIO.
      * DEFAUT FLAG TO 'N'
           MOVE "S  A010DELFLAG." TO VDU.
           IF DEL-FLAG = "Y" OR "N"
              MOVE DEL-FLAG TO VDUBUF
           ELSE
              MOVE "N" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "E  A010DELFLAG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO UEARNYN.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-DEL-FLAG.

           MOVE VDUBUF TO DEL-FLAG.

           IF DEL-FLAG NOT = "Y" AND NOT = "N"
              GO TO ENTER-DEL-FLAG.

           IF DEL-FLAG = "N"
              GO TO DUEYN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SETUP NEXT MONTH DATE:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE AGE-DATE TO NDTE-DATE.
           MOVE 1        TO NDTE-DD.
           MOVE 1        TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO AGE-DATE.
      *-----------------------------------------------------------------
       DUEYN.
           MOVE "E  A010DUESUM." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-DEL-FLAG.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DUEYN.

           MOVE VDUBUF TO DUEDATE-SUMMARY.

           IF GP-I-AM-WORLD
              IF NOT (DUEDATE-SUMMARY = "N" OR "Y" OR "F")
                 GO TO DUEYN
              END-IF
           ELSE
              IF NOT (DUEDATE-SUMMARY = "N" OR "Y")
                 GO TO DUEYN.

           IF DUEDATE-SUMMARY = "F"
               MOVE 0  TO PASSWDW-SEQ
               MOVE "CREATE AN ASCII DUE DATE FILE"
                  TO PASSWDW-PROMPT2
               PERFORM VERIFY-PASSWORD
               IF PASSWDW-STAT NOT = "OK"
                  GO TO DUEYN.

      *----------------------------------------------------------------
       ALPYN.
           MOVE "E  A010ALPSUM." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DUEYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ALPYN.

           MOVE VDUBUF TO ALPHA-SUMMARY.

           IF ALPHA-SUMMARY = "Y"     GO TO SUMYN.
           IF ALPHA-SUMMARY NOT = "N" GO TO ALPYN.

      *----------------------------------------------------------------
       SUMYN.
           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ALPYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SUMYN.

           MOVE VDUBUF TO SUM-ONLY.

           IF NOT SUM-ONLY-VALID
              MOVE "INVALID ENTRY, USE F8-HELP FOR MORE INFORMATION"
                                                              TO MESS
              PERFORM SEND-MESS
              GO TO SUMYN.

      *----------------------------------------------------------------
       RANGES-YN.
           MOVE "E  A010RANGES." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SUMYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RANGES-YN.

           MOVE VDUBUF TO PRINT-RANGES.

           IF NOT (PRINT-RANGES = "Y" OR "N")
              GO TO RANGES-YN.

      *----------------------------------------------------------------
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO EXIT-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO EXIT-PARM.

      *    IF GROUP-FLAG = "Y"
      *       MOVE "N" TO LOCAL-PRINT-FG
      *       GO TO EXIT-PARM.
      *-----------------------------------------------------------------
       ASK-LOCAL.
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO MESS.
           MOVE MESS-LIST TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE "S  A200SPOOL-PMPT:P." TO VDU.
           MOVE "LOCAL SPOOL   (Y/N):"    TO VDUBUF.
           PERFORM SCREENIO.

      *     MOVE "S  A050YES-NO." TO VDU.
      *     MOVE "(Y/N)"          TO VDUBUF.
      *     PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO RANGES-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

      *----------------------------------------------------------------
       EXIT-PARM.
           EXIT.
      ******************************************************************
      *
      *       DISPLAY REPORT PARAMETERS
      *
      ******************************************************************
       DISPLAY-PARAMETERS SECTION.

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH    TO VDUNUM0
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "SNNN040BR1." TO VDU.
      *    MOVE BEG-BRANCH    TO VDUNUM0.
      *    PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "SNNN040BR2." TO VDU.
      *    MOVE END-BRANCH    TO VDUNUM0.
      *    PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N020CL1." TO VDU.
           MOVE BEG-CLASS     TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N020CL2." TO VDU.
           MOVE END-CLASS     TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N042DL1." TO VDU.
           MOVE BEG-DEALER    TO VDUNUM2.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N042DL2." TO VDU.
           MOVE END-DEALER    TO VDUNUM2.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040AGE1." TO VDU.
           MOVE BEG-AGE        TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040AGE2." TO VDU.
           MOVE END-AGE        TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN A010C-POT." TO VDU.
           MOVE CPOT            TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN A010C-MISS." TO VDU.
           MOVE CMISS            TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040RAGE1." TO VDU.
           MOVE BEG-RAGE        TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040RAGE2." TO VDU.
           MOVE END-RAGE        TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN A010R-POT." TO VDU.
           MOVE RPOT            TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N070BB1." TO VDU.
           MOVE BEG-BAL       TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N070BB2." TO VDU.
           MOVE END-BAL       TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N060LA1." TO VDU.
           MOVE BEG-LNAMT     TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N060LA2." TO VDU.
           MOVE END-LNAMT     TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010TYPE." TO VDU.
           MOVE OWNED          TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PL." TO VDU.
           MOVE PL           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PLTYPE." TO VDU.
           MOVE PLTYPE           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ACTIVE." TO VDU.
           MOVE PLACTIVE         TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010BRULE." TO VDU.
           MOVE BRULE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010RC." TO VDU.
           MOVE RC           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010JD." TO VDU.
           MOVE JD           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010RP." TO VDU.
           MOVE REPO         TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010BK." TO VDU.
           MOVE BANK         TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010LG." TO VDU.
           MOVE LEGAL        TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010DA." TO VDU.
           MOVE DEBT         TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010UEARN." TO VDU.
           MOVE UEARN           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A200DELPROMPT:P." TO VDU
           MOVE "BEG MO DELINQENCY?: "    TO VDUBUF
           PERFORM SCREENIO
           MOVE "S  A010DELFLAG." TO VDU
           MOVE DEL-FLAG TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ALPSUM." TO VDU.
           MOVE ALPHA-SUMMARY    TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010DUESUM." TO VDU.
           MOVE DUEDATE-SUMMARY  TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY          TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010RANGES." TO VDU.
           MOVE PRINT-RANGES     TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A200SPOOL-PMPT:P." TO VDU.
           MOVE "LOCAL SPOOL   (Y/N):"    TO VDUBUF.
           PERFORM SCREENIO.

      *     MOVE "S  A050YES-NO." TO VDU.
      *     MOVE "(Y/N)"          TO VDUBUF.
      *     PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
      *
      *   DETAIL LINE PROCESS    *
      *
      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.

           IF FIRST-TIME = "Y"
*NEW*         MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           IF INVALID-BW-FG = "Y"
              MOVE "?" TO D-BW-BAD
              MOVE "MISSING BORROWER" TO D-NAME
           ELSE
              MOVE NAME-WORKER TO D-NAME.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO HOLD-LPS-NUM.
           MOVE H-LPS-NUM        TO D-LOAN-NUM.
           MOVE LN-1STPYDA       TO D-DUEDAY.
           MOVE LN-DLNO          TO D-DLNO.

           IF LN-DATE-PAID-LAST NOT = 0
              MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO ALP-DATE-EDIT
           ELSE
              MOVE LN-LOANDATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY  TO ALP-DATE-EDIT.

           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT TO D-PAYDATE.
           IF LN-DATE-PAID-LAST = 0
              MOVE "." TO D-DASH1
              IF D-DASH2 = "/"
                 MOVE "." TO D-DASH2.

           IF RDPOT30 = "Y"
              MOVE "<P>" TO D-RECENCYX
           ELSE
           IF RDPOT30 = "M"
              MOVE "<M>" TO D-RECENCYX
           ELSE
           IF RDPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-RECENCYX
           ELSE
           IF RDPOTCHGOFF = "M"
              MOVE "<PL>" TO D-RECENCYX
           ELSE
              MOVE RECDEL TO D-RECENCY.

           MOVE LN-RECPROFILE(1) TO D-PYRECPRO.
           INSPECT D-XPYRECPRO REPLACING ALL "9" BY "-".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * DETERMINE ACCOUNT STATUS (IB-PC):
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE 0  TO HOLD-CURBAL.
           MOVE TRANS-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.

      *    IF BR-AGE-ERN = "T" OR SPACE
      *       MOVE LN-TOTERN       TO D-UNEARNED
      *    ELSE
              MOVE EARN-TOT-UNEARN TO D-UNEARNED.

           MOVE LN-CONPROFILE(1) TO D-PROFILE.
           INSPECT D-XPROFILE REPLACING ALL "9" BY "-".

           IF CPOT30 = "Y"
              MOVE "<P>" TO D-CONTUALX
           ELSE
           IF CPOT30 = "M"
              MOVE "<M>" TO D-CONTUALX
           ELSE
           IF CPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-CONTUALX
           ELSE
           IF CPOTCHGOFF = "M"
              MOVE "<PL>" TO D-CONTUALX
           ELSE
              MOVE CONTRACTUAL TO D-CONTUAL.

           MOVE PDTH-DATE     TO D-PDTHRU.
           COMPUTE D-APR ROUNDED = LN-APRATE * 1.

           ADD LN-CURBAL TO HOLD-CURBAL.
           IF GP-INCLUDE-O2-IN-BAL = "Y"
              MOVE " PRIN + O2" TO HEAD6-PRIN
           ELSE
              MOVE " PRINCIPAL" TO HEAD6-PRIN
              SUBTRACT LN-OT2BAL FROM HOLD-CURBAL.

           IF LN-OTHBAL NOT = 0 OR LN-OT2BAL NOT = 0
              COMPUTE D-OTHBAL = LN-OTHBAL + LN-OT2BAL.

           IF LN-INTBAL NOT = 0 OR LN-LCBAL NOT = 0
              ADD LN-INTBAL LN-LCBAL GIVING D-INTBAL.

           IF HOLD-CURBAL NOT = 0
              MOVE HOLD-CURBAL TO D-CURBAL.

      *----------------------------------------------------------------
       DETAIL-LN-ACCTOT.
           PERFORM UPDATE-BY-DUEDAY.

           IF ALPHA-SUMMARY = "Y"
              PERFORM UPDATE-BY-ALPHA.

           MOVE 1 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 2 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 3 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.

           IF NOT (SUM-ONLY = "Y" OR "G" OR "A" OR "C")
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.

           GO TO END-DETAIL-LN-PROCESS.

      *----------------------------------------------------------------
       DETAIL-LN-ACCTBL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ADDING TO TOTALS: CONTRACTUAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF (
                 ( GP-I-AM-MERC ) AND
                 ( ( REPO = "O" ) OR
                   (   PL = "O" ) OR
                   ( ( NOT LN-IS-REPO-AR ) AND
                     ( LN-PLCD = " "     )
                   )
                 )
              ) OR
              (
                 ( NOT GP-I-AM-MERC  ) AND
                 ( (   PL = "O"      ) OR
                   (   LN-PLCD = " " )
                 )
              )
              ADD 1                   TO T-NO    (LEV CSUB 1)
              ADD LN-OTHBAL           TO T-OTH   (LEV CSUB 1)
              ADD LN-OT2BAL           TO T-OTH2  (LEV CSUB 1)
              ADD LN-INTBAL LN-LCBAL  TO T-INT   (LEV CSUB 1)
              ADD HOLD-CURBAL         TO T-BAL   (LEV CSUB 1)
              ADD UNEARNED            TO T-UNEARN(LEV CSUB 1)
              ADD UNDISCOUNT          TO T-UNDISC(LEV CSUB 1)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ADDING TO TOTALS: RECENCY
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              ADD 1                   TO T-NO    (LEV RDSUB 2)
              ADD LN-OTHBAL           TO T-OTH   (LEV RDSUB 2)
              ADD LN-OT2BAL           TO T-OTH2  (LEV RDSUB 2)
              ADD LN-INTBAL LN-LCBAL  TO T-INT   (LEV RDSUB 2)
              ADD HOLD-CURBAL         TO T-BAL   (LEV RDSUB 2)
              ADD UNEARNED            TO T-UNEARN(LEV RDSUB 2)
              ADD UNDISCOUNT          TO T-UNDISC(LEV RDSUB 2)

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ADDING TO TOTALS: PAID AHEAD
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF ( PSUB > ZEROES         ) AND
                 ( SP-BANKRULE NOT = "D" AND SP-BANKRULE NOT = "F")
                 ADD 1                  TO T-NO    (LEV PSUB 3)
                 ADD LN-OTHBAL          TO T-OTH   (LEV PSUB 3)
                 ADD LN-OT2BAL          TO T-OTH2  (LEV PSUB 3)
                 ADD LN-INTBAL LN-LCBAL TO T-INT   (LEV PSUB 3)
                 ADD HOLD-CURBAL        TO T-BAL   (LEV PSUB 3)
                 ADD UNEARNED           TO T-UNEARN(LEV PSUB 3)
                 ADD UNDISCOUNT         TO T-UNDISC(LEV PSUB 3).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ADDING TO TOTALS: REPO
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( LN-IS-REPO-AR ) AND
              ( LN-PLCD = SPACES  )
              ADD 1                   TO T-NO    (LEV RPSUB 4)
              ADD LN-OTHBAL           TO T-OTH   (LEV RPSUB 4)
              ADD LN-OT2BAL           TO T-OTH2  (LEV RPSUB 4)
              ADD LN-INTBAL LN-LCBAL  TO T-INT   (LEV RPSUB 4)
              ADD HOLD-CURBAL         TO T-BAL   (LEV RPSUB 4)
              ADD UNEARNED            TO T-UNEARN(LEV RPSUB 4)
              ADD UNDISCOUNT          TO T-UNDISC(LEV RPSUB 4).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ADDING TO TOTALS: P & L
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF ( LN-PLCD NOT = SPACES)
                 ADD 1                   TO T-NO    (LEV PLSUB 5)
                 ADD LN-OTHBAL           TO T-OTH   (LEV PLSUB 5)
                 ADD LN-OT2BAL           TO T-OTH2  (LEV PLSUB 5)
                 ADD LN-INTBAL LN-LCBAL  TO T-INT   (LEV PLSUB 5)
                 ADD HOLD-CURBAL         TO T-BAL   (LEV PLSUB 5)
                 ADD UNEARNED            TO T-UNEARN(LEV PLSUB 5)
                 ADD UNDISCOUNT          TO T-UNDISC(LEV PLSUB 5).

      *----------------------------------------------------------------
       UPDATE-BY-DUEDAY.
           MOVE LN-1STPYDATE      TO NUM-DATE.
           MOVE NUM-DA            TO D-DUEDAY.

           IF NUM-DA < 1 OR NUM-DA > 31
              MOVE 32             TO NUM-DA.

           ADD 1                  TO DD-NO(NUM-DA).
           ADD LN-OTHBAL          TO DD-OTH(NUM-DA).
           ADD LN-OT2BAL          TO DD-OTH2(NUM-DA).
           ADD LN-INTBAL LN-LCBAL TO DD-INT(NUM-DA).
           ADD HOLD-CURBAL        TO DD-BAL(NUM-DA).
           ADD LN-REGPYAMT        TO DD-PAY(NUM-DA).

           IF DUEDATE-SUMMARY = "F"
              ADD 1               TO DUE-DATE-NO(1, NUM-DA)
                                     DUE-DATE-NO(2, NUM-DA).

      *----------------------------------------------------------------
       UPDATE-BY-ALPHA.
           MOVE BW-LNAME           TO NM-TEST.

           IF NM-TEST < "A" OR NM-TEST > "Z"
              MOVE 27              TO NM-SUB
           ELSE
              SUBTRACT NM-A FROM NM-B GIVING NM-SUB.

           ADD 1                   TO NM-NO(NM-SUB).
           ADD LN-OTHBAL           TO NM-OTH(NM-SUB).
           ADD LN-OT2BAL           TO NM-OTH2(NM-SUB).
           ADD LN-INTBAL LN-LCBAL  TO NM-INT(NM-SUB).
           ADD HOLD-CURBAL         TO NM-BAL(NM-SUB).
           ADD LN-REGPYAMT         TO NM-PAY(NM-SUB).

       END-DETAIL-LN-PROCESS.
           EXIT.

      ******************************************************************
      *
      *    PROCESS BORROWER      *
      *
      ******************************************************************
       PROCESS-BORROWER SECTION.

           MOVE SPACES TO INVALID-BW-FG.

           MOVE LN-SSNO(1)       TO BW-SSNO.
           MOVE QLN-BW-LNAME-1   TO BW-LNAME.
           MOVE QLN-BW-FNAME-1   TO BW-FNAME.
           MOVE QLN-BW-SPSSNO-1  TO BW-SPSSNO.
           MOVE QLN-BW-SPLNAME-1 TO BW-SPLNAME.
           MOVE QLN-BW-SPFNAME-1 TO BW-SPFNAME.
           MOVE QLN-BW-SOLICIT-1 TO BW-SOLICIT.

      *    PERFORM READ-BW1-FILE.
           IF BW-LNAME = SPACES AND BW-FNAME = SPACES
              MOVE BEG-DATE TO BW-REFDATE
              MOVE "Y" TO INVALID-BW-FG
              GO TO END-PROCESS-BORROWER.

           MOVE QLN-BW-REFDATE-1 TO SQL-DATE-YYYY-MM-DD.
           PERFORM SQL-GET-DATE.
           MOVE SQL-DATE-YYYYMMDD TO BW-REFDATE.


           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
                   BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".

       END-PROCESS-BORROWER.
           EXIT.

      ******************************************************************
      *
      *        HEAD PAGE ROUTINE    *
      *
      ******************************************************************
       HEAD-PAGE SECTION.

           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.
           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2       TO PR-REC.
           MOVE 1           TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3        TO PR-REC.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD4       TO PR-REC.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF SUMMARY-FG = "N"
              MOVE HEAD5  TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE HEAD6  TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 7      TO LN-COUNT
              MOVE 2      TO LN-FEED
            ELSE
              MOVE 5      TO LN-COUNT
              MOVE 2      TO LN-FEED.

      *----------------------------------------------------------------
       END-HEAD-PAGE.
           EXIT.
      ******************************************************************
      *
      *    WRITE DETAIL LINE     *
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

      *----------------------------------------------------------------
       WRITE-DET-LINE.
           ADD LN-FEED       TO LN-COUNT.

*NEW*      IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE  TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1            TO LN-FEED.
           MOVE SPACES       TO DETAIL-LINE.

      ******************************************************************
      *
      *       RANGE LINES           *
      *
      ******************************************************************
       PRINT-RANGE-LINES SECTION.

           MOVE "Y"                    TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES   TO LN-COUNT.
           MOVE 4                      TO LN-FEED.
      *-------------------------------------------------------------
           IF DATE-TYPE = "L"
              MOVE "BEG LOAN DATE:" TO RANGE-LINE
              MOVE  BEG-DATE         TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "END LOAN DATE:" TO RANGE-LINE
              MOVE  END-DATE         TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
           IF DATE-TYPE = "E"
              MOVE "BEG ENTRY DATE:" TO RANGE-LINE
              MOVE  BEG-DATE         TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "END ENTRY DATE:" TO RANGE-LINE
              MOVE  END-DATE         TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
           IF DATE-TYPE = "B"
              MOVE "BEG BW REFDATE:" TO RANGE-LINE
              MOVE  BEG-DATE         TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BW REFDATE:" TO RANGE-LINE
              MOVE  END-DATE         TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.

      *--------------------------------------------------------------
           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DEALER   :" TO RANGE-LINE.
           MOVE BEG-DEALER             TO RANGE-DEALER.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-DEALER             TO RANGE-DEALER.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "CONTRACTUAL AGE    :" TO RANGE-LINE.
           MOVE BEG-AGE                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-AGE                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "POTENTIALS         :" TO RANGE-LINE.
           MOVE CPOT                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MISS PAYS          :" TO RANGE-LINE.
           MOVE CMISS                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "RECENCY AGE        :" TO RANGE-LINE.
           MOVE BEG-RAGE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-RAGE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "POTENTIALS          :" TO RANGE-LINE.
           MOVE RPOT                    TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING BALANCE  :" TO RANGE-LINE.
           MOVE BEG-BAL                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-BAL                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING LOAN AMT :" TO RANGE-LINE.
           MOVE BEG-LNAMT              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-LNAMT              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "OWNED,SERVICE,BOTH :" TO RANGE-LINE.
           MOVE OWNED                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "P & L              :" TO RANGE-LINE.
           MOVE PL                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "P & L TYPE         :" TO RANGE-LINE.
           MOVE PLTYPE                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ACTIVE             :" TO RANGE-LINE.
           MOVE PLACTIVE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BANK RULE  (I/T)   :" TO RANGE-LINE.
           MOVE BRULE                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "RECOURSE           :" TO RANGE-LINE.
           MOVE RC                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "JUDGEMENTS         :" TO RANGE-LINE.
           MOVE JD                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "REPO               :" TO RANGE-LINE.
           MOVE REPO                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BANKRUPT           :" TO RANGE-LINE.
           MOVE BANK                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "LEGAL              :" TO RANGE-LINE.
           MOVE LEGAL                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "DEBT-AID           :" TO RANGE-LINE.
           MOVE DEBT                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ZERO BALANCES      :" TO RANGE-LINE.
           MOVE ZBAL                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ZERO UNEARNED      :" TO RANGE-LINE.
           MOVE UEARN                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "DUEDATE SUMMARY    :" TO RANGE-LINE.
           MOVE DUEDATE-SUMMARY        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ALPHA SUMMARY      :" TO RANGE-LINE.
           MOVE ALPHA-SUMMARY          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SUMMARY ONLY       :" TO RANGE-LINE.
           MOVE SUM-ONLY               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PRINT RANGES       :" TO RANGE-LINE.
           MOVE PRINT-RANGES           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      ******************************************************************
      *
      *    SET BRANCH REPORT HEADINGS
      *
      ******************************************************************
       SET-BRANCH-HEADINGS SECTION.
           MOVE BR-NAME   TO H-BRDESC.
           IF GP-I-AM-REGACC
              MOVE SPACES TO H-BRDESC
              STRING "REGION: ",BR-REGION-NUMBER DELIMITED BY "  "
                    INTO H-BRDESC.
           MOVE HOLD-LN-OWNBR TO BR-NO H-BRNO.


      ******************************************************************
      *
      *          CLASS TOTALS
      *
      ******************************************************************
       CLASS-TOTALS SECTION.
           IF NOT (SUM-ONLY = "G" OR "A" OR "C")
              IF MORE-THAN-1-CLASS = "Y"
                 MOVE "Y"             TO SUMMARY-FG
                 MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
                 MOVE 1               TO LN-FEED
                 MOVE "CLASS TOTALS:" TO S-SUMNAME
                 PERFORM WRITE-DETAIL-LINE
                 MOVE 1               TO LEV
                 PERFORM PRINT-AGEING-SUMMARY.

           PERFORM UPDATE-WK-FILE.
           MOVE ZEROS            TO TOTAL-TBL(1).
           MOVE "N"              TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1                TO LN-FEED.

      ******************************************************************
      *
      *          UPDATE WORK FILE FOR COMPANY CLASS SUMMARY
      *
      ******************************************************************
       UPDATE-WK-FILE SECTION.
           MOVE HOLD-LN-CLASS TO WK-CLASS.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              MOVE HOLD-LN-CLASS TO WK-CLASS
              MOVE ZEROS TO WK-TOTAL-TBL
              PERFORM WRITE-WK-FILE.

           PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 5
                      AFTER SUB1 FROM 1 BY 1 UNTIL SUB1 > 10
               ADD T-NO    (1 SUB1 SUB2) TO WK-NO    (SUB1 SUB2)
               ADD T-BAL   (1 SUB1 SUB2) TO WK-BAL   (SUB1 SUB2)
               ADD T-INT   (1 SUB1 SUB2) TO WK-INT   (SUB1 SUB2)
               ADD T-OTH   (1 SUB1 SUB2) TO WK-OTH   (SUB1 SUB2)
               ADD T-OTH2  (1 SUB1 SUB2) TO WK-OTH2  (SUB1 SUB2)
               ADD T-UNEARN(1 SUB1 SUB2) TO WK-UNEARN(SUB1 SUB2)
               ADD T-UNDISC(1 SUB1 SUB2) TO WK-UNDISC(SUB1 SUB2)
           END-PERFORM.

           PERFORM REWRITE-WK-FILE.

      ******************************************************************
      *
      *          BRANCH TOTALS
      *
      ******************************************************************
       BRANCH-TOTALS SECTION.

           IF NOT (SUM-ONLY = "G" OR "A" OR "C")
              MOVE "Y"                      TO SUMMARY-FG
              MOVE SPACES                   TO H-LCNOX
              MOVE "**** ALL CLASSES ****"  TO H-LCDESC
              IF NOT (BEG-CLASS = 0 AND END-CLASS = 99)
                 MOVE BEG-CLASS TO H-LCBEG
                 MOVE " - " TO H-LCDASH
                 MOVE END-CLASS TO H-LCEND
              END-IF
              MOVE EXT-CONAME-RPT-LINES     TO LN-COUNT
              MOVE 1                        TO LN-FEED
              MOVE "BRANCH TOTALS:"         TO S-SUMNAME
              PERFORM WRITE-DETAIL-LINE
              MOVE 2                        TO LEV
              PERFORM PRINT-AGEING-SUMMARY.

           MOVE ZEROS                    TO TOTAL-TBL(2).
           MOVE "N"                      TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES     TO LN-COUNT.
           MOVE 1                        TO LN-FEED.

      ******************************************************************
      *  UPDATE ASCII DUE DATE FILE (WORLD PR# 745)
      ******************************************************************
       UPDATE-DUE-DATE-FILE SECTION.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 32
              IF DUE-DATE-NO(LEV, SUB) > 0
                 IF LEV = 1
                    MOVE BR-NO              TO DUE-BRNO
                 ELSE
                    MOVE 9999               TO DUE-BRNO
                 END-IF
                 MOVE SUB                   TO DUE-DAY
                 MOVE DUE-DATE-NO(LEV, SUB) TO DUE-NO-LOANS
                 MOVE X"0D"                 TO DUE-CR
                 MOVE X"0A"                 TO DUE-LF
                 ADD 1 TO DUE-KEY
                 PERFORM WRITE-DUE-FILE
                 IF IO-BAD
                    GO TO IO-ERROR
                 END-IF
              END-IF
           END-PERFORM.

      ******************************************************************
      *
      *          COMPANY TOTALS
      *
      ******************************************************************
       COMPANY-TOTALS SECTION.

           IF BEG-BRANCH = END-BRANCH
              GO TO SKIP-COMPANY-TOTALS.

           IF MORE-THAN-1-CLASS-ON-REPORT = "N"
              GO TO SKIP-COMPANY-CLASS-SUMMARY.

      *    -------------------------------------------------------------
      *    TEST TO SKIP CLASS / BRANCH SUMMARY
      *    -------------------------------------------------------------
           IF SUM-ONLY = "A"
              GO TO SKIP-COMPANY-CLASS-SUMMARY.

           MOVE 0 TO WK-CLASS.
           PERFORM START-WK-FILE.
       COMPANY-TOTALS-CLASS-NEXT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO SKIP-COMPANY-CLASS-SUMMARY.

           MOVE WK-CLASS TO LC-CODE
                            H-LCNO.
           PERFORM READ-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID" TO LC-DESC.
           MOVE LC-DESC TO H-LCDESC.
           MOVE SPACES TO H-LCBEG H-LCDASH H-LCEND.

           MOVE "Y"             TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1               TO LN-FEED.
           MOVE SPACES                    TO H-BRNOX.
           MOVE "**** ALL BRANCHES ****"  TO H-BRDESC.
           MOVE "COMPANY CLASS TOTALS:" TO S-SUMNAME.
           PERFORM WRITE-DETAIL-LINE.

           MOVE WK-TOTAL-TBL TO TOTAL-TBL(1).
           MOVE 1               TO LEV.
           PERFORM PRINT-AGEING-SUMMARY.
           GO TO COMPANY-TOTALS-CLASS-NEXT.


       SKIP-COMPANY-CLASS-SUMMARY.
      *    -------------------------------------------------------------
      *    TEST TO SKIP BRANCH/CLASS GRAND TOTALS
      *    -------------------------------------------------------------
           IF SUM-ONLY = "C"
              GO TO SKIP-COMPANY-TOTALS.

           MOVE "Y"                       TO SUMMARY-FG.
           MOVE SPACES                    TO H-LCNOX.
           MOVE "**** ALL CLASSES ****"   TO H-LCDESC.
           IF NOT (BEG-CLASS = 0 AND END-CLASS = 99)
              MOVE BEG-CLASS TO H-LCBEG
              MOVE " - " TO H-LCDASH
              MOVE END-CLASS TO H-LCEND.
           MOVE SPACES                    TO H-BRNOX.
           MOVE "**** ALL BRANCHES ****"  TO H-BRDESC.
           MOVE EXT-CONAME-RPT-LINES      TO LN-COUNT.
           MOVE 1                         TO LN-FEED.
           MOVE "COMPANY TOTALS:"         TO S-SUMNAME.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 3                         TO LEV.
           PERFORM PRINT-AGEING-SUMMARY.

       SKIP-COMPANY-TOTALS.
           IF DUEDATE-SUMMARY = "Y" OR "F"
              PERFORM RESET-TOT-PAGE
              MOVE "Y"                       TO SUMMARY-FG
              MOVE SPACES                    TO H-LCNOX
              MOVE "**** ALL CLASSES ****"   TO H-LCDESC
              IF NOT (BEG-CLASS = 0 AND END-CLASS = 99)
                 MOVE BEG-CLASS TO H-LCBEG
                 MOVE " - " TO H-LCDASH
                 MOVE END-CLASS TO H-LCEND
              END-IF
              MOVE SPACES                    TO H-BRNOX
              MOVE "**** ALL BRANCHES ****"  TO H-BRDESC
              MOVE EXT-CONAME-RPT-LINES      TO LN-COUNT
              MOVE 1                         TO LN-FEED
              MOVE "********* BY PAYMENT DUE DATE *********"
                                             TO S-AGETYPE
              MOVE "N"                       TO NM-PRINT
              MOVE 32                        TO NM-SUB
              PERFORM PRINT-BYDUEDATE-SUMMARY.

           IF ALPHA-SUMMARY = "Y"
              PERFORM RESET-TOT-PAGE
              PERFORM MOVE-NM-TO-DD
              MOVE "Y"                       TO SUMMARY-FG
              MOVE SPACES                    TO H-LCNOX
              MOVE "**** ALL CLASSES ****"   TO H-LCDESC
              IF NOT (BEG-CLASS = 0 AND END-CLASS = 99)
                 MOVE BEG-CLASS TO H-LCBEG
                 MOVE " - " TO H-LCDASH
                 MOVE END-CLASS TO H-LCEND
              END-IF
              MOVE SPACES                    TO H-BRNOX
              MOVE "**** ALL BRANCHES ****"  TO H-BRDESC
              MOVE EXT-CONAME-RPT-LINES      TO LN-COUNT
              MOVE 1                         TO LN-FEED
              MOVE "****** A L P H A   S U M M A R Y ******"
                                          TO S-AGETYPE
              MOVE "Y"                    TO NM-PRINT
              MOVE 27                     TO NM-SUB
              PERFORM PRINT-BYDUEDATE-SUMMARY.

           IF DUEDATE-SUMMARY = "F"
              IF BEG-BRANCH NOT = END-BRANCH
                 MOVE 2 TO LEV
                 PERFORM UPDATE-DUE-DATE-FILE.

      ******************************************************************
      *
      *          PRINT AGEING SUMMARY
      *
      ******************************************************************
       PRINT-AGEING-SUMMARY SECTION.

           PERFORM ZERO-TT-TOTALS.

           PERFORM ACCUM-AGEING-TOTALS-NM
                   VARYING AGE FROM 1 BY 1 UNTIL AGE > 31.
           PERFORM ACCUM-AGEING-TOTALS
                   VARYING AGE FROM 1 BY 1 UNTIL AGE > 10
                     AFTER TYP FROM 1 BY 1 UNTIL TYP > 5.


           PERFORM CONTRACT-PRINT.

           IF ( REPO NOT = "S" ) AND
              ( PL   NOT = "O" )
              PERFORM REPO-PRINT.

           IF (  PL = "O" ) OR
              (( PL = "I" ) AND ( REPO NOT = "O" ))
              PERFORM PL-PRINT.

      *    IF PL = "O"
      *          PERFORM PL-PRINT
      *    ELSE
      *    IF REPO = "O"
      *          PERFORM REPO-PRINT
      *    ELSE
      *    IF PL = "S"
      *       IF REPO = "S"
      *        PERFORM CONTRACT-PRINT
      *    ELSE
      *    IF PL = "I"
      *       IF REPO = "I"
      *        PERFORM CONTRACT-PRINT
      *        PERFORM REPO-PRINT
      *        PERFORM PL-PRINT
      *    ELSE
      *    IF PL = "S"
      *       IF REPO = "I"
      *        PERFORM CONTRACT-PRINT
      *        PERFORM REPO-PRINT
      *    ELSE
      *    IF PL = "I"
      *       IF REPO = "S"
      *        PERFORM CONTRACT-PRINT
      *        PERFORM PL-PRINT.

      *=================================================================
       EXIT-PRINT-AGEING-SUMMARY.
           EXIT.
      ******************************************************************
      *
      *    FOR CONTRACTUAL DELINQUENCY PRINTING
      *
      ******************************************************************
       CONTRACT-PRINT SECTION.
           MOVE 1         TO TYP.
           MOVE "********* CONTRACTUAL DELINQUENCY *********"
                          TO S-AGETYPE.
           MOVE 1         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUMMARY1  TO DETAIL-LINE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2         TO LN-FEED.
           PERFORM CLEAR-SUM-SUB-TOTALS.
           PERFORM VARYING AGE FROM 1 BY 1 UNTIL AGE > 10
              PERFORM PRINT-DETAIL-SUM
              IF TYP = 1 OR 2
                 PERFORM ACCUM-SUM-SUB-TOTALS
              END-IF
              IF TYP = 1 OR 3
                 IF AGE = 7 OR 9
                    PERFORM PRINT-SUM-SUB-TOTALS
                 END-IF
              END-IF
           END-PERFORM.
           MOVE 2         TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FOR NON MONTHS DELINQUENCY PRINTING
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GP-I-AM-TFC
              MOVE 1         TO TYP
              MOVE "********* NON-MONTHLY DELINQUENCY *********"
                             TO S-AGETYPE
              MOVE 2         TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE SUMMARY1  TO DETAIL-LINE
              MOVE 2         TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE 2         TO LN-FEED
              PERFORM PRINT-NM-DETAIL-SUM
                      VARYING AGE FROM 1 BY 1 UNTIL AGE > 31
              MOVE 2         TO LN-FEED
              PERFORM PRINT-NM-SUM-TOTALS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FOR RECENCY DELINQUENCY PRINTING
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "  ********* RECENCY DELINQUENCY *********"
                          TO S-AGETYPE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUMMARY1  TO DETAIL-LINE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2         TO LN-FEED.

           PERFORM CLEAR-SUM-SUB-TOTALS.
           PERFORM VARYING AGE FROM 1 BY 1 UNTIL AGE > 10
                     AFTER TYP FROM 2 BY 1 UNTIL TYP > 3
              PERFORM PRINT-DETAIL-SUM
              IF TYP = 1 OR 2
                 PERFORM ACCUM-SUM-SUB-TOTALS
              END-IF
              IF TYP = 1 OR 3
                 IF AGE = 7 OR 9
                    PERFORM PRINT-SUM-SUB-TOTALS
                 END-IF
              END-IF
           END-PERFORM.

           MOVE 2         TO TYP.
           MOVE 2         TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.
           MOVE 3         TO TYP.
           MOVE 1         TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.

       END-CONTRACT-PRINT.
           EXIT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FOR REPO PRINTING
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       REPO-PRINT SECTION.
           MOVE "  ************* REPOSSESSION ************"
                          TO S-AGETYPE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUMMARY1  TO DETAIL-LINE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2         TO LN-FEED.

           PERFORM CLEAR-SUM-SUB-TOTALS.
           MOVE 4         TO TYP.
           PERFORM VARYING AGE FROM 1 BY 1 UNTIL AGE > 2
                   PERFORM PRINT-DETAIL-SUM
           END-PERFORM.

           MOVE 4         TO TYP.
           MOVE 2         TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.
           MOVE 1         TO LN-FEED.

       END-REPO-PRINT.
           EXIT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FOR P & L DELINQUENCY PRINTING
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       PL-PRINT SECTION.
           MOVE "  **************** P & L ****************"
                          TO S-AGETYPE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUMMARY1  TO DETAIL-LINE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2         TO LN-FEED.

           PERFORM CLEAR-SUM-SUB-TOTALS.
           MOVE 5         TO TYP.
           PERFORM VARYING AGE FROM 1 BY 1 UNTIL AGE > 5
                   PERFORM PRINT-DETAIL-SUM
           END-PERFORM.

           MOVE 5         TO TYP.
           MOVE 2         TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.
           MOVE 1         TO LN-FEED.

       END-PL-PRINT.
           EXIT.
      ******************************************************************
      *
      *    PRINT-DETAIL-SUM SECTION.
      *
      ******************************************************************
       PRINT-DETAIL-SUM SECTION.

           IF T-NO(LEV AGE TYP) = 0
              IF T-BAL(LEV AGE TYP) = 0
                 IF T-INT(LEV AGE TYP) = 0
                    IF T-OTH(LEV AGE TYP) = 0
                      IF T-OTH2(LEV AGE TYP) = 0
                       IF T-UNEARN(LEV AGE TYP) = 0
                           IF T-UNDISC(LEV AGE TYP) = 0
                              GO TO END-PRINT-DETAIL-SUM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TYP = 3
              MOVE "  PAID-AHEAD" TO S-NAME
           ELSE
           IF TYP = 4
              MOVE REPO-DESC(AGE) TO S-NAME
           ELSE
           IF TYP = 5
              MOVE PL-DESC(AGE) TO S-NAME
           ELSE
              MOVE AGE-DESC(AGE)  TO S-NAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF T-NO(LEV AGE TYP) NOT = 0
              MOVE T-NO(LEV AGE TYP) TO S-NO
              IF TYP NOT = 3
                 IF TT-NO(TYP) NOT = 0
                    COMPUTE S-NO-P ROUNDED =
                            ( T-NO(LEV AGE TYP) / TT-NO(TYP) ) * 100
                 ELSE
                    MOVE 0 TO S-NO-P.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF T-BAL(LEV AGE TYP) NOT = 0
              MOVE T-BAL(LEV AGE TYP) TO S-BAL
              IF TYP NOT = 3
                 IF TT-BAL(TYP) NOT = 0
                    COMPUTE S-BAL-P ROUNDED =
                            ( T-BAL(LEV AGE TYP) / TT-BAL(TYP) ) * 100
                 ELSE
                    MOVE 0 TO S-BAL-P.

      * INT/LC DUE  %
           IF T-INT(LEV AGE TYP) NOT = 0
              MOVE T-INT(LEV AGE TYP) TO S-INT
              IF TYP NOT = 3
                 IF TT-INT(TYP) NOT = 0
                    COMPUTE S-INT-P ROUNDED =
                            ( T-INT(LEV AGE TYP) / TT-INT(TYP) ) * 100
                 ELSE
                    MOVE 0 TO S-INT-P.

      * OTHER DUE
           IF T-OTH(LEV AGE TYP) NOT = 0
              MOVE T-OTH(LEV AGE TYP) TO S-OTH.

      * OTHER2 DUE
           IF T-OTH2(LEV AGE TYP) NOT = 0
              MOVE T-OTH2(LEV AGE TYP) TO S-OTH2.

      * UNEARNED   %
           IF T-UNEARN(LEV AGE TYP) NOT = 0
              MOVE T-UNEARN(LEV AGE TYP) TO S-UNEARN
              IF TYP NOT = 3
                 IF TT-UNEARN(TYP) NOT = 0
                    COMPUTE S-UNEARN-P ROUNDED =
                     ( T-UNEARN(LEV AGE TYP) / TT-UNEARN(TYP) ) * 100
                 ELSE
                    MOVE 0 TO S-UNEARN-P.

      * UNEARNED DISC  %
           IF T-UNDISC(LEV AGE TYP) NOT = 0
              MOVE T-UNDISC(LEV AGE TYP) TO S-UNDISC
              IF TYP NOT = 3
                 IF TT-UNDISC(TYP) NOT = 0
                    COMPUTE S-UNDISC-P ROUNDED =
                       (T-UNDISC(LEV AGE TYP) / TT-UNDISC(TYP)) * 100
                 ELSE
                    MOVE 0 TO S-UNDISC-P.

           PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------
       END-PRINT-DETAIL-SUM.
           EXIT.

      ******************************************************************
      *
      *    PRINT-NM-DETAIL-SUM SECTION.
      *
      ******************************************************************
       PRINT-NM-DETAIL-SUM SECTION.

           IF T-NM-NO(LEV AGE) = 0
              IF T-NM-BAL(LEV AGE) = 0
                 IF T-NM-INT(LEV AGE) = 0
                    IF T-NM-OTH(LEV AGE) = 0
                      IF T-NM-OTH2(LEV AGE) = 0
                       IF T-NM-UNEARN(LEV AGE) = 0
                           IF T-NM-UNDISC(LEV AGE) = 0
                              GO TO END-PRINT-NM-DETAIL-SUM.

           SUBTRACT 1 FROM AGE GIVING NM-AGE-SUB.
           IF NM-AGE-SUB = 0
              MOVE "CUR" TO NM-AGE-ALP
           ELSE
              MOVE NM-AGE-SUB TO NM-AGE-NUM
              IF NM-AGE-SUB = 30
                 MOVE "+" TO NM-AGE-SIGN
              ELSE
                 MOVE " " TO NM-AGE-SIGN.
           MOVE NM-AGE-DESC TO S-NAME.

           IF T-NM-NO(LEV AGE) NOT = 0
              MOVE T-NM-NO(LEV AGE) TO S-NO
              IF TT-NM-NO NOT = 0
                 COMPUTE S-NO-P ROUNDED =
                         (T-NM-NO(LEV AGE) / TT-NM-NO) * 100
              ELSE
                 MOVE 0 TO S-NO-P.

           IF T-NM-BAL(LEV AGE) NOT = 0
              MOVE T-NM-BAL(LEV AGE) TO S-BAL
              IF TT-NM-BAL NOT = 0
                 COMPUTE S-BAL-P ROUNDED =
                         (T-NM-BAL(LEV AGE) / TT-NM-BAL) * 100
              ELSE
                 MOVE 0 TO S-BAL-P.

      * INT/LC DUE  %
           IF T-NM-INT(LEV AGE) NOT = 0
              MOVE T-NM-INT(LEV AGE) TO S-INT
              IF TT-NM-INT NOT = 0
                 COMPUTE S-INT-P ROUNDED =
                         (T-NM-INT(LEV AGE) / TT-NM-INT) * 100
              ELSE
                 MOVE 0 TO S-INT-P.

      * OTHER DUE
           IF T-NM-OTH(LEV AGE) NOT = 0
              MOVE T-NM-OTH(LEV AGE) TO S-OTH.

      * OTHER2 DUE
           IF T-NM-OTH2(LEV AGE) NOT = 0
              MOVE T-NM-OTH2(LEV AGE) TO S-OTH2.

      * UNEARNED   %
           IF T-NM-UNEARN(LEV AGE) NOT = 0
              MOVE T-NM-UNEARN(LEV AGE) TO S-UNEARN
              IF TT-NM-UNEARN NOT = 0
                 COMPUTE S-UNEARN-P ROUNDED =
                  (T-NM-UNEARN(LEV AGE) / TT-NM-UNEARN) * 100
              ELSE
                 MOVE 0 TO S-UNEARN-P.

      * UNEARNED DISC  %
           IF T-NM-UNDISC(LEV AGE) NOT = 0
              MOVE T-NM-UNDISC(LEV AGE) TO S-UNDISC
              IF TT-NM-UNDISC NOT = 0
                 COMPUTE S-UNDISC-P ROUNDED =
                    (T-NM-UNDISC(LEV AGE) / TT-NM-UNDISC) * 100
              ELSE
                 MOVE 0 TO S-UNDISC-P.

           PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------
       END-PRINT-NM-DETAIL-SUM.
           EXIT.
      ******************************************************************
      * ROUTINES FOR SUB TOTALS:
      *  PRINT SUB TOTALS FOR DELINQUENCY (30,60,90,120,150,180,210+)
      *                       SLOW  (ABOVE DELINQUENCY, <M>, <P>)
      ******************************************************************
       PRINT-SUM-SUB-TOTALS SECTION.
           IF NOT ( AGE = 7 OR 9 )
              GO TO EXIT-PRINT-SUM-SUB-TOTALS.

      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DO NOT PRINT DELINQ TOTALS IF THERE ARE NO DELINQ ACCTS
      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF AGE = 7
              IF SUB-TT-NO(1)  = 0 AND SUB-TT-BAL(1) = 0 AND
                 SUB-TT-INT(1) = 0 AND SUB-TT-OTH(1) = 0 AND
                 SUB-TT-UNEARN(1) = 0 AND SUB-TT-INT(1) = 0 AND
                 SUB-TT-UNDISC(1) = 0 AND SUB-TT-OTH2(1) = 0
                    GO TO EXIT-PRINT-SUM-SUB-TOTALS
              ELSE
                 MOVE 1 TO SUB1
                 MOVE "   TOTAL DEL" TO S-NAME.

      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DO NOT PRINT SLOW TOTALS IF THERE ARE NO <M> OR <P> ACCTS
      *       (IE. SLOW TOTALS = DELINQ TOTALS)
      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF AGE = 9
              IF SUB-TT-TYPETBL(2) = SUB-TT-TYPETBL(1)
                    GO TO EXIT-PRINT-SUM-SUB-TOTALS
              ELSE
                 MOVE 2 TO SUB1
                 MOVE "  TOTAL SLOW" TO S-NAME.

      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WHEN WE GET HERE TYP = 1 (CONTRACTUAL) OR 3 (PAID-AHEAD)
      *    BUT WE REALLY WANT TO TOTAL TYP = 1 (CONTRACTUAL)
      *    OR TYP = 2 (RECENCY)
      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TYP = 3
              MOVE 2 TO SUB2
           ELSE
              MOVE 1 TO SUB2.

      *    - - - - - - - - - - - - - - - - - - - - - - -
           IF SUB-TT-NO(SUB1) NOT = 0
              MOVE SUB-TT-NO(SUB1)  TO S-NO
              COMPUTE S-NO-P ROUNDED =
                      ( SUB-TT-NO(SUB1) / TT-NO(SUB2) ) * 100.
      *    - - - - - - - - - - - - - - - - - - - - - - -
           IF SUB-TT-BAL(SUB1) NOT = 0
              MOVE SUB-TT-BAL(SUB1) TO S-BAL
              COMPUTE S-BAL-P ROUNDED =
                      ( SUB-TT-BAL(SUB1) / TT-BAL(SUB2) ) * 100.
      *    - - - - - - - - - - - - - - - - - - - - - - -
           IF SUB-TT-INT(SUB1) NOT = 0
              MOVE SUB-TT-INT(SUB1) TO S-INT
              COMPUTE S-INT-P ROUNDED =
                      ( SUB-TT-INT(SUB1) / TT-INT(SUB2) ) * 100.
      *    - - - - - - - - - - - - - - - - - - - - - - -
           IF SUB-TT-OTH(SUB1) NOT = 0
              MOVE SUB-TT-OTH(SUB1) TO S-OTH.
           IF SUB-TT-OTH2(SUB1) NOT = 0
              MOVE SUB-TT-OTH2(SUB1) TO S-OTH2.
      *    - - - - - - - - - - - - - - - - - - - - - - -
           IF SUB-TT-UNEARN(SUB1) NOT = 0
              MOVE SUB-TT-UNEARN(SUB1) TO S-UNEARN
              COMPUTE S-UNEARN-P ROUNDED =
                ( SUB-TT-UNEARN(SUB1) / TT-UNEARN(SUB2) ) * 100.
      *    - - - - - - - - - - - - - - - - - - - - - - -
           IF SUB-TT-UNDISC(SUB1) NOT = 0
              MOVE SUB-TT-UNDISC(SUB1) TO S-UNDISC
              COMPUTE S-UNDISC-P ROUNDED =
                (SUB-TT-UNDISC(SUB1) / TT-UNDISC(SUB2)) * 100.

           PERFORM WRITE-DETAIL-LINE.

      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SLOW TOTALS NEED TO INCLUDE DELINQUENCY
      *      ONLY CLEAR AFTER SLOW TOTALS ARE PRINTED
      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF AGE = 9
              PERFORM CLEAR-SUM-SUB-TOTALS.

       EXIT-PRINT-SUM-SUB-TOTALS.
           EXIT.
      *----------------------------------------------------------------
       CLEAR-SUM-SUB-TOTALS SECTION.
      *    - - - - - - - - -
      *    CLEAR DEL TOTALS
      *    - - - - - - - - -
           MOVE 0 TO SUB-TT-NO(1)  SUB-TT-BAL(1) SUB-TT-INT(1)
                     SUB-TT-OTH(1) SUB-TT-UNEARN(1)  SUB-TT-OTH2(1)
                     SUB-TT-UNDISC(1).
      *    - - - - - - - - -
      *    CLEAR SLOW TOTALS
      *    - - - - - - - - -
           MOVE 0 TO SUB-TT-NO(2)  SUB-TT-BAL(2) SUB-TT-INT(2)
                     SUB-TT-OTH(2) SUB-TT-UNEARN(2) SUB-TT-OTH2(2)
                     SUB-TT-UNDISC(2).
       EXIT-CLEAR-SUM-SUB-TOTALS.
           EXIT.
      *----------------------------------------------------------------
       ACCUM-SUM-SUB-TOTALS SECTION.
      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    INCREMENT SLOW TOTALS (30/60/90/120/150/180/210+/<M>/<P>)
      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ADD  T-NO(LEV AGE TYP) TO SUB-TT-NO(2).
           ADD T-BAL(LEV AGE TYP) TO SUB-TT-BAL(2).
           ADD T-INT(LEV AGE TYP) TO SUB-TT-INT(2).
           ADD T-OTH(LEV AGE TYP) TO SUB-TT-OTH(2).
           ADD T-OTH2(LEV AGE TYP) TO SUB-TT-OTH2(2).
           ADD T-UNEARN(LEV AGE TYP) TO SUB-TT-UNEARN(2).
           ADD T-UNDISC(LEV AGE TYP) TO SUB-TT-UNDISC(2).
      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    INCREMENT DEL TOTALS (30/60/90/120/150/180/210+)
      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF AGE < 8
              ADD  T-NO(LEV AGE TYP) TO SUB-TT-NO(1)
              ADD T-BAL(LEV AGE TYP) TO SUB-TT-BAL(1)
              ADD T-INT(LEV AGE TYP) TO SUB-TT-INT(1)
              ADD T-OTH(LEV AGE TYP) TO SUB-TT-OTH(1)
              ADD T-OTH2(LEV AGE TYP) TO SUB-TT-OTH2(1)
              ADD T-UNEARN(LEV AGE TYP) TO SUB-TT-UNEARN(1)
              ADD T-UNDISC(LEV AGE TYP) TO SUB-TT-UNDISC(1).
       EXIT-ACCUM-SUM-SUB-TOTALS.
           EXIT.
      ******************************************************************
      *
      *     PRINT-BYDUEDATE-SUMMARY SECTION.
      *
      ******************************************************************
       PRINT-BYDUEDATE-SUMMARY SECTION.

           PERFORM ZERO-TT-TOTALS.
           PERFORM ACCUM-BYDUEDATE-TOTALS
                   VARYING LEV FROM 1 BY 1 UNTIL LEV > NM-SUB.
           MOVE 1         TO LEV.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUMMARY2  TO DETAIL-LINE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2         TO LN-FEED.
           PERFORM PRINT-BYDUEDATE-SUM VARYING LEV
                   FROM 1 BY 1 UNTIL LEV > NM-SUB.
           MOVE 2         TO LN-FEED.
           PERFORM PRINT-DUE-TOTALS.

      ******************************************************************
      *
      *    PRINT-BYDUEDATE-SUM SECTION.
      *
      ******************************************************************
       PRINT-BYDUEDATE-SUM SECTION.

           IF DD-NO(LEV) = 0
              IF DD-BAL(LEV) = 0
                 IF DD-INT(LEV) = 0
                    IF DD-OTH(LEV) = 0
                      IF DD-OTH2(LEV) = 0
                        IF DD-PAY(LEV) = 0
                          GO TO END-PRINT-BYDUEDATE-SUM.

           IF NM-PRINT = "N"
              MOVE LEV    TO NUM-DA
              MOVE NUM-DA TO S-NAMEDUE
           ELSE
              ADD LEV NM-A GIVING NM-B
              MOVE NM-TEST TO S-LASTINITIAL.

           MOVE ":" TO S-NAMECOLON.

           IF DD-NO(LEV) NOT = 0
              MOVE DD-NO(LEV) TO S-NO
              IF TT-NO(1) NOT = 0
                 COMPUTE S-NO-P ROUNDED =
                                ( DD-NO(LEV) / TT-NO(1) ) * 100
              ELSE
                 MOVE 0 TO S-NO-P.

            IF DD-BAL(LEV) NOT = 0
              MOVE DD-BAL(LEV) TO S-BAL
              IF TT-BAL(1) NOT = 0
                 COMPUTE S-BAL-P ROUNDED =
                                 ( DD-BAL(LEV) / TT-BAL(1) ) * 100
              ELSE
                 MOVE 0 TO S-BAL-P.

           IF DD-INT(LEV) NOT = 0
              MOVE DD-INT(LEV) TO S-INT
              IF TT-INT(1) NOT = 0
                 COMPUTE S-INT-P ROUNDED =
                                 ( DD-INT(LEV) / TT-INT(1) ) * 100
              ELSE
                 MOVE 0 TO S-INT-P.

           IF DD-OTH(LEV) NOT = 0
              MOVE DD-OTH(LEV) TO S-OTH.

           IF DD-OTH2(LEV) NOT = 0
              MOVE DD-OTH2(LEV) TO S-OTH2.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ACTUALLY HOLDS REGPYAMT SUMMATION
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF DD-PAY(LEV) NOT = 0
              MOVE DD-PAY(LEV) TO S-UNEARN
              IF TT-UNEARN(1) NOT = 0
                 COMPUTE S-UNEARN-P ROUNDED =
                           ( DD-PAY(LEV) / TT-UNEARN(1) ) * 100
              ELSE
                 MOVE 0 TO S-UNEARN-P.

           PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------
       END-PRINT-BYDUEDATE-SUM.
           EXIT.
      ******************************************************************
      *
      *    ZERO-TT-TOTALS SECTION.
      *
      ******************************************************************
       ZERO-TT-TOTALS SECTION.
           MOVE 1 TO SUB.

           MOVE 0 TO TT-NM-NO  TT-NM-BAL  TT-NM-INT
                     TT-NM-OTH TT-NM-OTH2 TT-NM-UNEARN
                     TT-NM-UNDISC.
       ZERO-NEXT-TT-TOTALS.
           MOVE 0 TO TT-NO(SUB) TT-BAL(SUB) TT-INT(SUB) TT-OTH(SUB)
                     TT-OTH2(SUB) TT-UNEARN(SUB) TT-UNDISC(SUB).
           IF SUB < 5
              ADD 1 TO SUB
              GO TO ZERO-NEXT-TT-TOTALS.

       END-ZERO-TT-TOTALS.
           EXIT.

      ******************************************************************
      *
      *    ZERO-DD-TOTALS SECTION.
      *
      ******************************************************************
       ZERO-DD-TOTALS SECTION.
           MOVE 1 TO SUB.

      *----------------------------------------------------------------
       ZERO-NEXT-DD-TOTALS.
           MOVE 0 TO DD-NO(SUB) DD-BAL(SUB) DD-INT(SUB) DD-OTH(SUB)
                        DD-OTH2(SUB) DD-PAY(SUB).
           IF SUB < 32
              ADD 1 TO SUB
              GO TO ZERO-NEXT-DD-TOTALS.

      *----------------------------------------------------------------
       END-ZERO-DD-TOTALS.
           EXIT.
      ******************************************************************
      *
      *    ZERO-NM-TOTALS SECTION.
      *
      ******************************************************************
       ZERO-NM-TOTALS SECTION.
           MOVE 1 TO SUB.

      *----------------------------------------------------------------
       ZERO-NEXT-NM-TOTALS.
           MOVE 0 TO NM-NO(SUB) NM-BAL(SUB) NM-INT(SUB) NM-OTH(SUB)
                             NM-OTH2(SUB) NM-PAY(SUB).

           IF SUB < 27
              ADD 1 TO SUB
              GO TO ZERO-NEXT-NM-TOTALS.

      *----------------------------------------------------------------
       END-ZERO-NM-TOTALS.
           EXIT.

      ******************************************************************
      *
      *    MOVE-NM-TO-DD SECTION.
      *
      ******************************************************************
       MOVE-NM-TO-DD SECTION.
           MOVE 1 TO SUB.

      *----------------------------------------------------------------
       MOVE-NEXT-NM-TOTALS.
           MOVE NM-NO(SUB)  TO DD-NO(SUB).
           MOVE NM-BAL(SUB) TO DD-BAL(SUB).
           MOVE NM-INT(SUB) TO DD-INT(SUB).
           MOVE NM-OTH(SUB) TO DD-OTH(SUB).
           MOVE NM-OTH2(SUB) TO DD-OTH2(SUB).
           MOVE NM-PAY(SUB) TO DD-PAY(SUB).

           IF SUB < 27
              ADD 1 TO SUB
              GO TO MOVE-NEXT-NM-TOTALS.

      *----------------------------------------------------------------
       END-MOVE-NM-TOTALS.
           EXIT.
      *****************************************************************
      *
      *    C L A S S   S C A N   R O U T I N E
      *
      *****************************************************************
       CLASS-SCAN SECTION.
           MOVE 0 TO LCSCAN-KEY.
           MOVE " " TO LCSCAN-VERIFY.
           MOVE LCSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF TO LCSCAN-BUF.
           IF SCAN-STATUS NOT = "00"
            OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
              MOVE "SNNN020CL1." TO VDU
           ELSE
              MOVE "SNNN020CL2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.
      ***************************************************************
      *
      *    D E A L E R    S C A N   R O U T I N E
      *
      ***************************************************************
       DEALER-SCAN SECTION.
           MOVE 0 TO DLSCAN-NUM.
           MOVE DLSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF TO DLSCAN-BUF.
           IF SCAN-STATUS NOT = "00"
            OR DLSCAN-NUM = " "
               MOVE 9 TO IO-FG
               GO TO EXIT-DEALER-SCAN.
           IF ( DEALER-FG = "DLBEG" )
              MOVE "SN N042DL1." TO VDU
           ELSE
              MOVE "SN N042DL2." TO VDU.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           MOVE 0 TO IO-FG.

       EXIT-DEALER-SCAN.
           EXIT.

       VERIFY-PASSWORD SECTION.
           MOVE " " TO PASSWDW-STAT.
           MOVE "LNAGED" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER PASSWORD TO " TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.


*NEW* *********************************************
*NEW*  FORM-RESET SECTION.
*NEW* *********************************************
*NEW*      MOVE EXT-CONAME-CONAME TO LNAGED-CONAME.
*NEW*      MOVE EXT-CONAME-SYSDATE TO LNAGED-SYSDATE.
*NEW*      DISPLAY LNAGED-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
*NEW*      MOVE LNAGED-FORM-FIELDS TO WR-BUF.
*NEW*      MOVE LNAGED-FORM-LIST TO WR-LIST.
*NEW*      PERFORM CALL-WRFORM.


*NEW* *********************************************
*NEW*  SETUP-LINK-INFO SECTION.
*NEW* *********************************************
*NEW*      MOVE LNAGED-HELP--FILE TO HELP-LINK-FILE.
*NEW*      MOVE LNAGED-HELP--DIR  TO HELP-LINK-DIR.

*NEW* *********************************************
*NEW*  VDU-CONVERT-FIELD SECTION.
*NEW* *********************************************
*NEW*      MOVE "00" TO STAT.
*NEW*      MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
*NEW*      EVALUATE TRUE
*NEW*         COPY "LIBLP/LNAGED_EVA.CPY".
*NEW*         WHEN OTHER       MOVE   "2A" TO STAT
*NEW*      END-EVALUATE.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    M E M B E R S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
*NEW*  COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
      *----------------------------------------------------------------
       COMPUTE-REPO.

            IF ( LN-REPO-ONHAND )
               MOVE 1 TO RPSUB
            ELSE
            IF ( LN-REPO-SOLD   )
               MOVE 2 TO RPSUB
            ELSE
               MOVE ZEROES TO RPSUB.

      *----------------------------------------------------------------
       COMPUTE-PL.

            MOVE ZEROES TO PLSUB.

            IF ( LN-PLCD NOT = SPACES )
               IF ( LN-PLTYPE = "2" )
                  MOVE 2 TO PLSUB
               ELSE
               IF ( LN-PLTYPE = "3" )
                  MOVE 3 TO PLSUB
               ELSE
               IF ( LN-PLTYPE = "4" )
                  MOVE 4 TO PLSUB
               ELSE
               IF ( LN-PLTYPE = "5" )
                  MOVE 5 TO PLSUB
               ELSE
                  MOVE 1 TO PLSUB.

      *----------------------------------------------------------------
       ACCUM-BYDUEDATE-TOTALS.
           ADD DD-NO(LEV)  TO TT-NO(1).
           ADD DD-BAL(LEV) TO TT-BAL(1).
           ADD DD-INT(LEV) TO TT-INT(1).
           ADD DD-OTH(LEV) TO TT-OTH(1).
           ADD DD-OTH2(LEV) TO TT-OTH2(1).
           ADD DD-PAY(LEV) TO TT-UNEARN(1).

      *----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       PRINT-DUE-TOTALS.
           MOVE "      TOTAL:" TO S-NAME.
           MOVE TT-NO(1)       TO S-NO.
           MOVE TT-BAL(1)      TO S-BAL.
           MOVE TT-INT(1)      TO S-INT.
           MOVE TT-OTH(1)      TO S-OTH.
           MOVE TT-OTH2(1)     TO S-OTH2.
           MOVE TT-UNEARN(1)   TO S-UNEARN.
           PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------
       PRINT-SUM-TOTALS.
           IF TYP = 3
              MOVE "  PAID-AHEAD" TO S-NAME
           ELSE
           IF TYP = 4
              MOVE " TOTAL REPO:" TO S-NAME
           ELSE
           IF TYP = 5
              MOVE "  TOTAL P&L:" TO S-NAME
           ELSE
              MOVE "      TOTAL:" TO S-NAME.

           MOVE TT-NO(TYP)  TO S-NO.
           MOVE TT-BAL(TYP) TO S-BAL.
           MOVE TT-INT(TYP) TO S-INT.
           MOVE TT-OTH(TYP) TO S-OTH.
           MOVE TT-OTH2(TYP) TO S-OTH2.
           MOVE TT-UNEARN(TYP) TO S-UNEARN.
           MOVE TT-UNDISC(TYP) TO S-UNDISC.
           PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------
       PRINT-NM-SUM-TOTALS.
           MOVE "      TOTAL:" TO S-NAME.
           MOVE TT-NM-NO  TO S-NO.
           MOVE TT-NM-BAL TO S-BAL.
           MOVE TT-NM-INT TO S-INT.
           MOVE TT-NM-OTH TO S-OTH.
           MOVE TT-NM-OTH2 TO S-OTH2.
           MOVE TT-NM-UNEARN TO S-UNEARN.
           MOVE TT-NM-UNDISC TO S-UNDISC.
           PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------
       ACCUM-AGEING-TOTALS.
           ADD T-NO(LEV AGE TYP)  TO TT-NO(TYP).
           ADD T-BAL(LEV AGE TYP) TO TT-BAL(TYP).
           ADD T-INT(LEV AGE TYP) TO TT-INT(TYP).
           ADD T-OTH(LEV AGE TYP) TO TT-OTH(TYP).
           ADD T-OTH2(LEV AGE TYP) TO TT-OTH2(TYP).
           ADD T-UNEARN(LEV AGE TYP) TO TT-UNEARN(TYP).
           ADD T-UNDISC(LEV AGE TYP) TO TT-UNDISC(TYP).
      *----------------------------------------------------------------
       ACCUM-AGEING-TOTALS-NM.
           ADD T-NM-NO(LEV AGE)     TO TT-NM-NO.
           ADD T-NM-BAL(LEV AGE)    TO TT-NM-BAL.
           ADD T-NM-INT(LEV AGE)    TO TT-NM-INT.
           ADD T-NM-OTH(LEV AGE)    TO TT-NM-OTH.
           ADD T-NM-OTH2(LEV AGE)   TO TT-NM-OTH2.
           ADD T-NM-UNEARN(LEV AGE) TO TT-NM-UNEARN.
           ADD T-NM-UNDISC(LEV AGE) TO TT-NM-UNDISC.
      *----------------------------------------------------------------
       SET-CLASS-HEADINGS.
           MOVE HOLD-LN-CLASS TO H-LCNO.

           IF HOLD-LN-CLASS NOT = LC-CODE
              MOVE HOLD-LN-CLASS TO LC-CODE
              PERFORM READ-LC1-FILE
              IF IO-FG NOT = 0
                 MOVE "INVALID" TO LC-DESC.

           MOVE LC-DESC TO H-LCDESC.
           MOVE SPACES TO H-LCBEG H-LCDASH H-LCEND.
           MOVE "N"     TO SUMMARY-FG.

      *----------------------------------------------------------------
       RESET-TOT-PAGE.
           MOVE ZEROS         TO TOTAL-TBL(3).
           MOVE "Y"           TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES  TO LN-COUNT.
           MOVE 1             TO LN-FEED.

      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
      ******************************************************************

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *COPY "LIBGB/GBBRRN.CPY".
       LOAD-BR-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BR-ALL-BASE
              MOVE FILPATH-MACHINE         TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH.

      *-----------------------------------------------------------------
       OPEN-BR-FILE.
           PERFORM LOAD-BR-FILE.
           PERFORM OPEN-IT.
           MOVE BR-PATH-SQL TO E-FILE.

           EXEC SQL
                CONNECT TO :SQL-CONNECT-SERVER AS BR_CONNECT
                      USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BR-FILE.
           PERFORM START-IT.
           MOVE BR-PATH-SQL TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           IF ( BR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BR-FILE.

           IF ( QBR-WEND-NO = ZEROES    ) OR
              ( QBR-WEND-NO NOT NUMERIC )
              MOVE BR-NO                TO QBR-WBEG-NO
              MOVE ALL "9"              TO QBR-WEND-NO.

           EXEC SQL SET CONNECTION BR_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BR_CURSOR CURSOR FOR
            SELECT BRFILE.BR_NO,
                   ISNULL(BRFILE.BR_MACHINE,''),
                   ISNULL(BRFILE.BR_DATA_PATH,''),
                   ISNULL(BRFILE.BR_NAME,''),
                   BRFILE.BR_REGION_NUMBER,
                   ISNULL(BRFILE.BR_LOAN_NUM_FLAG,'')
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO >= :QBR-WBEG-NO
              AND BRFILE.BR_NO <= :QBR-WEND-NO
           END-EXEC.

           EXEC SQL
               OPEN BR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBR-WBEG-NO
                          QBR-WEND-NO.

           MOVE STAT---GOOD TO BR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BR-FILE.
           PERFORM READ-IT.
           MOVE BR-PATH-SQL TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           MOVE BR-NO TO QBR-NO.

           EXEC SQL SET CONNECTION BR_CONNECT END-EXEC.

           EXEC SQL
            SELECT BRFILE.BR_NO,
                   ISNULL(BRFILE.BR_MACHINE,''),
                   ISNULL(BRFILE.BR_DATA_PATH,''),
                   ISNULL(BRFILE.BR_NAME,''),
                   BRFILE.BR_REGION_NUMBER,
                   ISNULL(BRFILE.BR_LOAN_NUM_FLAG,'')
               INTO
                   :QBR-NO, :QBR-MACHINE, :QBR-DATA-PATH, :QBR-NAME,
                       :QBR-REGION-NUMBER, :QBR-LOAN-NUM-FLAG
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO = :QBR-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       READ-BR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BR-PATH-SQL TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           INITIALIZE QBR-REC.

           EXEC SQL SET CONNECTION BR_CONNECT END-EXEC.

           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BR_CURSOR INTO 
                   :QBR-NO, :QBR-MACHINE, :QBR-DATA-PATH, :QBR-NAME,
                       :QBR-REGION-NUMBER, :QBR-LOAN-NUM-FLAG
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLCODE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BR-FILE.
           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BR-CURSOR-STAT.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *COPY "LIBGB/GBGR1RN.CPY".
       LOAD-GR1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO GR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GR-ALL-MACHINE
              MOVE GR-ALL-PATH             TO GR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
      *- - - -{ DO NOT KNOW WHERE THIS WOULD BE USED, }- - - - - - - - -
      *- - - -{ BUT STAYING CONSISTENT  }- - - - - - - - - - - - - - - -
           IF ( GR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO GR-ALL-BASE
              MOVE FILPATH-MACHINE         TO GR-ALL-MACHINE
              MOVE GR-ALL-PATH             TO GR-PATH.

      *-----------------------------------------------------------------
       OPEN-GR1-FILE.
           PERFORM LOAD-GR1-FILE.
           PERFORM OPEN-IT.
           MOVE GR-PATH-SQL TO E-FILE.

           EXEC SQL
                CONNECT TO :SQL-CONNECT-SERVER AS GR_CONNECT
                      USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GR1-FILE.
           PERFORM START-IT.
           MOVE GR-PATH-SQL TO E-FILE.
           MOVE GR1-KEY TO E-KEYX.

           IF ( GR1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GR1-FILE.

           IF ( QGR1-WEND-ID = SPACES    )
              MOVE GR-ID                TO QGR1-WBEG-ID
              MOVE ALL "Z"              TO QGR1-WEND-ID
              CALL "C$TOLOWER" USING QGR1-WEND-ID, VALUE 4.

           EXEC SQL SET CONNECTION GR_CONNECT END-EXEC.

           EXEC SQL
            DECLARE GR1_CURSOR CURSOR FOR
            SELECT ISNULL(GRFILE.GR_ID,''),
                   GRFILE.GR_LEVEL,
                   ISNULL(GRFILE.GR_MEMBER_ID_1,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_2,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_3,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_4,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_5,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_6,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_7,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_8,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_9,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_10,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_11,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_12,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_13,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_14,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_15,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_16,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_17,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_18,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_19,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_20,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_21,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_22,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_23,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_24,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_25,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_26,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_27,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_28,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_29,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_30,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_31,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_32,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_33,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_34,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_35,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_36,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_37,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_38,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_39,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_40,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_41,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_42,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_43,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_44,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_45,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_46,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_47,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_48,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_49,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_50,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_51,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_52,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_53,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_54,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_55,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_56,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_57,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_58,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_59,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_60,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_61,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_62,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_63,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_64,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_65,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_66,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_67,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_68,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_69,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_70,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_71,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_72,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_73,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_74,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_75,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_76,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_77,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_78,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_79,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_80,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_81,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_82,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_83,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_84,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_85,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_86,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_87,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_88,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_89,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_90,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_91,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_92,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_93,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_94,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_95,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_96,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_97,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_98,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_99,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_100,'')
            FROM DBO.GRFILE
            WHERE GRFILE.GR_ID >= :QGR1-WBEG-ID
              AND GRFILE.GR_ID <= :QGR1-WEND-ID
            FOR READ ONLY
           END-EXEC.

           EXEC SQL
               OPEN GR1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGR1-WBEG-ID
                          QGR1-WEND-ID.

           MOVE STAT---GOOD TO GR1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GR1-FILE.
           PERFORM READ-IT.
           MOVE GR-PATH-SQL TO E-FILE.
           MOVE GR1-KEY TO E-KEYX.

           MOVE GR-ID TO QGR-ID.

           EXEC SQL SET CONNECTION GR_CONNECT END-EXEC.

           EXEC SQL
            SELECT ISNULL(GRFILE.GR_ID,''),
                   GRFILE.GR_LEVEL,
                   ISNULL(GRFILE.GR_MEMBER_ID_1,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_2,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_3,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_4,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_5,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_6,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_7,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_8,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_9,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_10,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_11,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_12,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_13,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_14,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_15,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_16,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_17,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_18,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_19,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_20,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_21,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_22,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_23,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_24,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_25,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_26,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_27,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_28,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_29,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_30,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_31,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_32,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_33,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_34,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_35,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_36,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_37,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_38,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_39,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_40,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_41,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_42,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_43,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_44,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_45,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_46,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_47,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_48,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_49,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_50,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_51,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_52,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_53,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_54,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_55,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_56,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_57,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_58,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_59,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_60,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_61,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_62,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_63,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_64,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_65,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_66,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_67,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_68,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_69,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_70,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_71,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_72,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_73,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_74,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_75,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_76,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_77,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_78,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_79,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_80,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_81,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_82,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_83,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_84,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_85,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_86,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_87,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_88,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_89,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_90,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_91,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_92,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_93,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_94,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_95,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_96,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_97,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_98,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_99,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_100,'')
            INTO 
                   :QGR-ID, :QGR-LEVEL, :QGR-MEMBER-ID-1,
                   :QGR-MEMBER-ID-2,
                       :QGR-MEMBER-ID-3, :QGR-MEMBER-ID-4,
                       :QGR-MEMBER-ID-5, :QGR-MEMBER-ID-6,
                       :QGR-MEMBER-ID-7, :QGR-MEMBER-ID-8,
                       :QGR-MEMBER-ID-9, :QGR-MEMBER-ID-10,
                       :QGR-MEMBER-ID-11, :QGR-MEMBER-ID-12,
                       :QGR-MEMBER-ID-13, :QGR-MEMBER-ID-14,
                       :QGR-MEMBER-ID-15, :QGR-MEMBER-ID-16,
                       :QGR-MEMBER-ID-17, :QGR-MEMBER-ID-18,
                       :QGR-MEMBER-ID-19, :QGR-MEMBER-ID-20,
                       :QGR-MEMBER-ID-21, :QGR-MEMBER-ID-22,
                       :QGR-MEMBER-ID-23, :QGR-MEMBER-ID-24,
                       :QGR-MEMBER-ID-25, :QGR-MEMBER-ID-26,
                       :QGR-MEMBER-ID-27, :QGR-MEMBER-ID-28,
                       :QGR-MEMBER-ID-29, :QGR-MEMBER-ID-30,
                       :QGR-MEMBER-ID-31, :QGR-MEMBER-ID-32,
                       :QGR-MEMBER-ID-33, :QGR-MEMBER-ID-34,
                       :QGR-MEMBER-ID-35, :QGR-MEMBER-ID-36,
                       :QGR-MEMBER-ID-37, :QGR-MEMBER-ID-38,
                       :QGR-MEMBER-ID-39, :QGR-MEMBER-ID-40,
                       :QGR-MEMBER-ID-41, :QGR-MEMBER-ID-42,
                       :QGR-MEMBER-ID-43, :QGR-MEMBER-ID-44,
                       :QGR-MEMBER-ID-45, :QGR-MEMBER-ID-46,
                       :QGR-MEMBER-ID-47, :QGR-MEMBER-ID-48,
                       :QGR-MEMBER-ID-49, :QGR-MEMBER-ID-50,
                       :QGR-MEMBER-ID-51, :QGR-MEMBER-ID-52,
                       :QGR-MEMBER-ID-53, :QGR-MEMBER-ID-54,
                       :QGR-MEMBER-ID-55, :QGR-MEMBER-ID-56,
                       :QGR-MEMBER-ID-57, :QGR-MEMBER-ID-58,
                       :QGR-MEMBER-ID-59, :QGR-MEMBER-ID-60,
                       :QGR-MEMBER-ID-61, :QGR-MEMBER-ID-62,
                       :QGR-MEMBER-ID-63, :QGR-MEMBER-ID-64,
                       :QGR-MEMBER-ID-65, :QGR-MEMBER-ID-66,
                       :QGR-MEMBER-ID-67, :QGR-MEMBER-ID-68,
                       :QGR-MEMBER-ID-69, :QGR-MEMBER-ID-70,
                       :QGR-MEMBER-ID-71, :QGR-MEMBER-ID-72,
                       :QGR-MEMBER-ID-73, :QGR-MEMBER-ID-74,
                       :QGR-MEMBER-ID-75, :QGR-MEMBER-ID-76,
                       :QGR-MEMBER-ID-77, :QGR-MEMBER-ID-78,
                       :QGR-MEMBER-ID-79, :QGR-MEMBER-ID-80,
                       :QGR-MEMBER-ID-81, :QGR-MEMBER-ID-82,
                       :QGR-MEMBER-ID-83, :QGR-MEMBER-ID-84,
                       :QGR-MEMBER-ID-85, :QGR-MEMBER-ID-86,
                       :QGR-MEMBER-ID-87, :QGR-MEMBER-ID-88,
                       :QGR-MEMBER-ID-89, :QGR-MEMBER-ID-90,
                       :QGR-MEMBER-ID-91, :QGR-MEMBER-ID-92,
                       :QGR-MEMBER-ID-93, :QGR-MEMBER-ID-94,
                       :QGR-MEMBER-ID-95, :QGR-MEMBER-ID-96,
                       :QGR-MEMBER-ID-97, :QGR-MEMBER-ID-98,
                       :QGR-MEMBER-ID-99, :QGR-MEMBER-ID-100
            FROM DBO.GRFILE
            WHERE GRFILE.GR_ID = :QGR-ID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-GR1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GR-FIELDS.

      *-----------------------------------------------------------------
       READ-GR1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GR-PATH-SQL TO E-FILE.
           MOVE GR1-KEY TO E-KEYX.
           INITIALIZE QGR-REC.

           EXEC SQL SET CONNECTION GR_CONNECT END-EXEC.

           IF ( GR1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GR1_CURSOR INTO 
                   :QGR-ID, :QGR-LEVEL, :QGR-MEMBER-ID-1,
                   :QGR-MEMBER-ID-2,
                       :QGR-MEMBER-ID-3, :QGR-MEMBER-ID-4,
                       :QGR-MEMBER-ID-5, :QGR-MEMBER-ID-6,
                       :QGR-MEMBER-ID-7, :QGR-MEMBER-ID-8,
                       :QGR-MEMBER-ID-9, :QGR-MEMBER-ID-10,
                       :QGR-MEMBER-ID-11, :QGR-MEMBER-ID-12,
                       :QGR-MEMBER-ID-13, :QGR-MEMBER-ID-14,
                       :QGR-MEMBER-ID-15, :QGR-MEMBER-ID-16,
                       :QGR-MEMBER-ID-17, :QGR-MEMBER-ID-18,
                       :QGR-MEMBER-ID-19, :QGR-MEMBER-ID-20,
                       :QGR-MEMBER-ID-21, :QGR-MEMBER-ID-22,
                       :QGR-MEMBER-ID-23, :QGR-MEMBER-ID-24,
                       :QGR-MEMBER-ID-25, :QGR-MEMBER-ID-26,
                       :QGR-MEMBER-ID-27, :QGR-MEMBER-ID-28,
                       :QGR-MEMBER-ID-29, :QGR-MEMBER-ID-30,
                       :QGR-MEMBER-ID-31, :QGR-MEMBER-ID-32,
                       :QGR-MEMBER-ID-33, :QGR-MEMBER-ID-34,
                       :QGR-MEMBER-ID-35, :QGR-MEMBER-ID-36,
                       :QGR-MEMBER-ID-37, :QGR-MEMBER-ID-38,
                       :QGR-MEMBER-ID-39, :QGR-MEMBER-ID-40,
                       :QGR-MEMBER-ID-41, :QGR-MEMBER-ID-42,
                       :QGR-MEMBER-ID-43, :QGR-MEMBER-ID-44,
                       :QGR-MEMBER-ID-45, :QGR-MEMBER-ID-46,
                       :QGR-MEMBER-ID-47, :QGR-MEMBER-ID-48,
                       :QGR-MEMBER-ID-49, :QGR-MEMBER-ID-50,
                       :QGR-MEMBER-ID-51, :QGR-MEMBER-ID-52,
                       :QGR-MEMBER-ID-53, :QGR-MEMBER-ID-54,
                       :QGR-MEMBER-ID-55, :QGR-MEMBER-ID-56,
                       :QGR-MEMBER-ID-57, :QGR-MEMBER-ID-58,
                       :QGR-MEMBER-ID-59, :QGR-MEMBER-ID-60,
                       :QGR-MEMBER-ID-61, :QGR-MEMBER-ID-62,
                       :QGR-MEMBER-ID-63, :QGR-MEMBER-ID-64,
                       :QGR-MEMBER-ID-65, :QGR-MEMBER-ID-66,
                       :QGR-MEMBER-ID-67, :QGR-MEMBER-ID-68,
                       :QGR-MEMBER-ID-69, :QGR-MEMBER-ID-70,
                       :QGR-MEMBER-ID-71, :QGR-MEMBER-ID-72,
                       :QGR-MEMBER-ID-73, :QGR-MEMBER-ID-74,
                       :QGR-MEMBER-ID-75, :QGR-MEMBER-ID-76,
                       :QGR-MEMBER-ID-77, :QGR-MEMBER-ID-78,
                       :QGR-MEMBER-ID-79, :QGR-MEMBER-ID-80,
                       :QGR-MEMBER-ID-81, :QGR-MEMBER-ID-82,
                       :QGR-MEMBER-ID-83, :QGR-MEMBER-ID-84,
                       :QGR-MEMBER-ID-85, :QGR-MEMBER-ID-86,
                       :QGR-MEMBER-ID-87, :QGR-MEMBER-ID-88,
                       :QGR-MEMBER-ID-89, :QGR-MEMBER-ID-90,
                       :QGR-MEMBER-ID-91, :QGR-MEMBER-ID-92,
                       :QGR-MEMBER-ID-93, :QGR-MEMBER-ID-94,
                       :QGR-MEMBER-ID-95, :QGR-MEMBER-ID-96,
                       :QGR-MEMBER-ID-97, :QGR-MEMBER-ID-98,
                       :QGR-MEMBER-ID-99, :QGR-MEMBER-ID-100
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLCODE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-GR1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GR-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GR1-FILE.
           IF ( GR1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GR1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GR1-CURSOR-STAT.
 
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       LOAD-LN3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *-----------------------------------------------------------------
       OPEN-LN3-FILE.
           PERFORM LOAD-LN3-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

           EXEC SQL
                CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT
                      USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LN3-FILE.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
                  DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           IF ( QLN3-WEND-OWNBR = ZEROES    ) OR
              ( QLN3-WEND-OWNBR NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN3-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN3-WEND-OWNBR
              MOVE LN-CLASS             TO QLN3-WBEG-CLASS
              MOVE ALL "9"              TO QLN3-WEND-CLASS
              MOVE LN-ACCTNO            TO QLN3-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN3-WEND-ACCTNO.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
            SELECT  LNFILE.LN_OWNBR,
                    LNFILE.LN_CLASS,
                    LNFILE.LN_ACCTNO,
                    LNFILE.LN_SSNO1,
                    ISNULL(BWFILE1.BW_LNAME,'') AS LNAME_1,
                    ISNULL(BWFILE1.BW_FNAME,'') AS FNAME_1,
       	            BWFILE1.BW_SPSSNO AS SPSSNO_1,
       	            ISNULL(BWFILE1.BW_SPLNAME,'') AS SPLNAME_1,
       	            ISNULL(BWFILE1.BW_SPFNAME,'') AS SPFNAME_1,
                    ISNULL(CAST(BWFILE1.BW_REFDATE AS VARCHAR(10)),'0')
                               AS REFDATE_1,
       	            ISNULL(BWFILE1.BW_SOLICIT,'') AS SOLICIT_1,
                    LNFILE.LN_SSNO2,
       	            ISNULL(BWFILE2.BW_LNAME,'') AS LNAME_2,
       	            ISNULL(BWFILE2.BW_FNAME,'') AS FNAME_2,
       	            BWFILE2.BW_SPSSNO AS SPSSNO_2,
       	            ISNULL(BWFILE2.BW_SPLNAME,'') AS SPLNAME_2,
       	            ISNULL(BWFILE2.BW_SPFNAME,'') AS SPFNAME_2,
       	            ISNULL(CAST(BWFILE2.BW_REFDATE AS VARCHAR(10)),'0')
                               AS REFDATE_2,
       	            ISNULL(BWFILE2.BW_SOLICIT,'') AS SOLICIT_2,
                    LNFILE.LN_SSNO3,
       	            ISNULL(BWFILE3.BW_LNAME,'') AS LNAME_3,
       	            ISNULL(BWFILE3.BW_FNAME,'') AS FNAME_3,
       	            BWFILE3.BW_SPSSNO AS SPSSNO_3,
       	            ISNULL(BWFILE3.BW_SPLNAME,'') AS SPLNAME_3,
       	            ISNULL(BWFILE3.BW_SPFNAME,'') AS SPFNAME_3,
       	            ISNULL(CAST(BWFILE3.BW_REFDATE AS VARCHAR(10)),'0')
                               AS REFDATE_3,
       	            ISNULL(BWFILE3.BW_SOLICIT,'') AS SOLICIT_3,
                    LNFILE.LN_SSNO4,
       	            ISNULL(BWFILE4.BW_LNAME,'') AS LNAME_4,
       	            ISNULL(BWFILE4.BW_FNAME,'') AS FNAME_4,
       	            BWFILE4.BW_SPSSNO AS SPSSNO_4,
       	            ISNULL(BWFILE4.BW_SPLNAME,'') AS SPLNAME_4,
       	            ISNULL(BWFILE4.BW_SPFNAME,'') AS SPFNAME_4,
       	            ISNULL(CAST(BWFILE4.BW_REFDATE AS VARCHAR(10)),'0')
                               AS REFDATE_4,
       	            ISNULL(BWFILE4.BW_SOLICIT,'') AS SOLICIT_4,
                    ISNULL(LNFILE.LN_POCD,''),
                    LNFILE.LN_DLNO,
                    ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
                    ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
                    LNFILE.LN_SUM_LNAMT,
                    ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
                    ISNULL(LNFILE.LN_PLCD,''),
                    ISNULL(LNFILE.LN_PLTYPE,''),
                    ISNULL(LNFILE.LN_SALEFIN,''),
                    ISNULL(LNFILE.LN_DLRECOURSE,''),
                    ISNULL(LNFILE.LN_SERVACCT,''),
                    ISNULL(CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),'0'),
                    ISNULL(LNFILE.LN_REPOCD,''),
               ISNULL(CAST(LNFILE.LN_BNKRPTDATE AS VARCHAR(10)),'0'),
               ISNULL(CAST(LNFILE.LN_LEGALDATE AS VARCHAR(10)),'0'),
               ISNULL(CAST(LNFILE.LN_DEBTAIDATE  AS VARCHAR(10)),'0'),
                    LNFILE.LN_CURBAL,
                    LNFILE.LN_INTBAL,
                    LNFILE.LN_LCBAL,
                    LNFILE.LN_OTHBAL,
                    ISNULL(LNFILE.LN_ORGST,''),
                    LNFILE.LN_SPRCLASS,
                    LNFILE.LN_SUBCLASS,
                    LNFILE.LN_LAWCODE,
               ISNULL(CAST(LNFILE.LN_DATE_PAID_LAST
                          AS VARCHAR(10)),'0'),
                    ISNULL(LNFILE.LN_UNITPER_CD,''),
                    LNFILE.LN_UNITPER_FREQ,
                    LNFILE.LN_RECPROFILE_1,
                    LNFILE.LN_RECPROFILE_2,
                    LNFILE.LN_RECPROFILE_3,
                    LNFILE.LN_RECPROFILE_4,
                    LNFILE.LN_RECPROFILE_5,
                    LNFILE.LN_CONPROFILE_1,
                    LNFILE.LN_CONPROFILE_2,
                    LNFILE.LN_CONPROFILE_3,
                    LNFILE.LN_CONPROFILE_4,
                    LNFILE.LN_CONPROFILE_5,
                    LNFILE.LN_APRATE,
                    LNFILE.LN_OT2BAL,
                ISNULL(CAST(LNFILE.LN_1STPYDATE
                          AS VARCHAR(10)),'0'),
                    LNFILE.LN_REGPYAMT,
                    LNFILE.LN_ANTICERN_1,
                    LNFILE.LN_ANTICERN_2,
                    LNFILE.LN_ANTICERN_3,
                    LNFILE.LN_ANTICERN_4,
                    LNFILE.LN_ANTICERN_5,
                    LNFILE.LN_ANTICERN_6,
                    LNFILE.LN_ANTICERN_7,
                    LNFILE.LN_ANTICERN_8,
                    LNFILE.LN_ANTICERN_9,
                    LNFILE.LN_ANTICERN_10,
                    LNFILE.LN_ANTICADJ_1,
                    LNFILE.LN_ANTICADJ_2,
                    LNFILE.LN_ANTICADJ_3,
                    LNFILE.LN_ANTICADJ_4,
                    LNFILE.LN_ANTICADJ_5,
                    LNFILE.LN_ANTICADJ_6,
                    LNFILE.LN_ANTICADJ_7,
                    LNFILE.LN_ANTICADJ_8,
                    LNFILE.LN_ANTICADJ_9,
                    LNFILE.LN_ANTICADJ_10,
                    LNFILE.LN_ACCERN_1,
                    LNFILE.LN_ACCERN_2,
                    LNFILE.LN_ACCERN_3,
                    LNFILE.LN_ACCERN_4,
                    LNFILE.LN_ACCERN_5,
                    LNFILE.LN_ACCERN_6,
                    LNFILE.LN_ACCERN_7,
                    LNFILE.LN_ACCERN_8,
                    LNFILE.LN_ACCERN_9,
                    LNFILE.LN_ACCERN_10,
                    LNFILE.LN_UFERN_1,
                    LNFILE.LN_UFERN_2,
                    LNFILE.LN_UFERN_3,
                    LNFILE.LN_UFERN_4,
                    LNFILE.LN_UFERN_5,
                    LNFILE.LN_UFERN_6,
                    LNFILE.LN_UFERN_7,
                    LNFILE.LN_UFERN_8,
                    LNFILE.LN_UFERN_9,
                    LNFILE.LN_UFERN_10,
                    LNFILE.LN_MTD_PAYS_RECDEL,
                    LNFILE.LN_PY_YYYYMM_RECDEL,
                    LNFILE.LN_TOTPAYMNTD,
                    LNFILE.LN_1STPYAMT,
                    LNFILE.LN_LASTPYAMT,
                    ISNULL(LNFILE.LN_PAY_SCHLD_FG,''),
                    LNFILE.LN_TOTNODEF,
                    LNFILE.LN_TOTNOALDEL,
                    LNFILE.LN_NOREGPY,
                    ISNULL(CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),'0'),
                    ISNULL(LNFILE.LN_STATUSFG,''),
             ISNULL(CAST(LNFILE.LN_ACCELDATE AS VARCHAR(10)),'0'),
                    ISNULL(CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),'0'),
                    ISNULL(LNFILE.LN_LOANTYPE,''),
                    LNFILE.LN_JDRATE,
                    ISNULL(LNFILE.LN_STATFLAGS,''),
             ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)),'0'),
                    LNFILE.LN_ORGTERM,
                    LNFILE.LN_SUM_ORGTERM
          FROM DBO.LNFILE
            LEFT JOIN DBO.BWFILE BWFILE1 
              ON (LNFILE.LN_OWNBR = BWFILE1.BW_OWNBR	
       	      AND LNFILE.LN_SSNO1 = BWFILE1.BW_SSNO)
            LEFT JOIN DBO.BWFILE BWFILE2 
              ON (LNFILE.LN_OWNBR = BWFILE2.BW_OWNBR	
              AND LNFILE.LN_SSNO2 = BWFILE2.BW_SSNO)
            LEFT JOIN DBO.BWFILE BWFILE3 
              ON (LNFILE.LN_OWNBR = BWFILE3.BW_OWNBR	
              AND LNFILE.LN_SSNO3 = BWFILE3.BW_SSNO) 	
            LEFT JOIN DBO.BWFILE BWFILE4 
              ON (LNFILE.LN_OWNBR = BWFILE4.BW_OWNBR	
              AND LNFILE.LN_SSNO4 = BWFILE4.BW_SSNO) 
            WHERE LNFILE.LN_OWNBR >= :QLN3-WBEG-OWNBR
              AND LNFILE.LN_OWNBR <= :QLN3-WEND-OWNBR
              AND LNFILE.LN_CLASS >= :QLN3-WBEG-CLASS
              AND LNFILE.LN_CLASS <= :QLN3-WEND-CLASS
              AND LNFILE.LN_ACCTNO >= :QLN3-WBEG-ACCTNO
              AND LNFILE.LN_ACCTNO <= :QLN3-WEND-ACCTNO 
            ORDER BY LNFILE.LN_OWNBR,
                     LNFILE.LN_CLASS,
                     LNFILE.LN_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN3-WBEG-OWNBR
                          QLN3-WEND-OWNBR
                          QLN3-WBEG-CLASS
                          QLN3-WEND-CLASS
                          QLN3-WBEG-ACCTNO
                          QLN3-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
                  DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
                  FETCH LN3_CURSOR INTO 
              :QLN-OWNBR, :QLN-CLASS, :QLN-ACCTNO, :QLN-SSNO1,
              :QLN-BW-LNAME-1,:QLN-BW-FNAME-1,
              :QLN-BW-SPSSNO-1,
              :QLN-BW-SPLNAME-1, :QLN-BW-SPFNAME-1, :QLN-BW-REFDATE-1,
              :QLN-BW-SOLICIT-1,
              :QLN-SSNO2,
            :QLN-BW-LNAME-2, :QLN-BW-FNAME-2,
            :QLN-BW-SPSSNO-2,
            :QLN-BW-SPLNAME-2, :QLN-BW-SPFNAME-2,
            :QLN-BW-REFDATE-2,
            :QLN-BW-SOLICIT-2,
            :QLN-SSNO3,
            :QLN-BW-LNAME-3, :QLN-BW-FNAME-3,
            :QLN-BW-SPSSNO-3,
            :QLN-BW-SPLNAME-3, :QLN-BW-SPFNAME-3, :QLN-BW-REFDATE-3,
            :QLN-BW-SOLICIT-3,
            :QLN-SSNO4,
            :QLN-BW-LNAME-4, :QLN-BW-FNAME-4, :QLN-BW-SPSSNO-4,
            :QLN-BW-SPLNAME-4, :QLN-BW-SPFNAME-4, :QLN-BW-REFDATE-4,
            :QLN-BW-SOLICIT-4,
            :QLN-POCD, :QLN-DLNO, :QLN-LOANDATE, :QLN-ENTDATE,
                :QLN-SUM-LNAMT, :QLN-PLDATE, :QLN-PLCD, :QLN-PLTYPE,
                :QLN-SALEFIN, :QLN-DLRECOURSE, :QLN-SERVACCT,
                :QLN-JDDATE, :QLN-REPOCD, :QLN-BNKRPTDATE,
                :QLN-LEGALDATE, :QLN-DEBTAIDATE, :QLN-CURBAL,
                :QLN-INTBAL, :QLN-LCBAL, :QLN-OTHBAL, :QLN-ORGST,
                :QLN-SPRCLASS, :QLN-SUBCLASS, :QLN-LAWCODE,
                :QLN-DATE-PAID-LAST, :QLN-UNITPER-CD,
                :QLN-UNITPER-FREQ, :QLN-RECPROFILE-1,
                :QLN-RECPROFILE-2, :QLN-RECPROFILE-3,
                :QLN-RECPROFILE-4, :QLN-RECPROFILE-5,
                :QLN-CONPROFILE-1, :QLN-CONPROFILE-2,
                :QLN-CONPROFILE-3, :QLN-CONPROFILE-4,
                :QLN-CONPROFILE-5, :QLN-APRATE, :QLN-OT2BAL,
                :QLN-1STPYDATE, :QLN-REGPYAMT, :QLN-ANTICERN-1,
                :QLN-ANTICERN-2, :QLN-ANTICERN-3, :QLN-ANTICERN-4,
                :QLN-ANTICERN-5, :QLN-ANTICERN-6, :QLN-ANTICERN-7,
                :QLN-ANTICERN-8, :QLN-ANTICERN-9, :QLN-ANTICERN-10,
                :QLN-ANTICADJ-1, :QLN-ANTICADJ-2, :QLN-ANTICADJ-3,
                :QLN-ANTICADJ-4, :QLN-ANTICADJ-5, :QLN-ANTICADJ-6,
                :QLN-ANTICADJ-7, :QLN-ANTICADJ-8, :QLN-ANTICADJ-9,
                :QLN-ANTICADJ-10, :QLN-ACCERN-1, :QLN-ACCERN-2,
                :QLN-ACCERN-3, :QLN-ACCERN-4, :QLN-ACCERN-5,
                :QLN-ACCERN-6, :QLN-ACCERN-7, :QLN-ACCERN-8,
                :QLN-ACCERN-9, :QLN-ACCERN-10, :QLN-UFERN-1,
                :QLN-UFERN-2, :QLN-UFERN-3, :QLN-UFERN-4,
                :QLN-UFERN-5, :QLN-UFERN-6, :QLN-UFERN-7,
                :QLN-UFERN-8, :QLN-UFERN-9, :QLN-UFERN-10,
                :QLN-MTD-PAYS-RECDEL, :QLN-PY-YYYYMM-RECDEL,
                :QLN-TOTPAYMNTD, :QLN-1STPYAMT, :QLN-LASTPYAMT,
                :QLN-PAY-SCHLD-FG, :QLN-TOTNODEF, :QLN-TOTNOALDEL,
                :QLN-NOREGPY, :QLN-INTDATE, :QLN-STATUSFG,
                :QLN-ACCELDATE, :QLN-REDUDATE, :QLN-LOANTYPE,
                :QLN-JDRATE, :QLN-STATFLAGS, :QLN-ORIG-1STPYDATE,
                :QLN-ORGTERM, :QLN-SUM-ORGTERM
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLCODE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *COPY "LIBLP/LPBW1RN.CPY".
       LOAD-BW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *-----------------------------------------------------------------
       OPEN-BW1-FILE.
           PERFORM LOAD-BW1-FILE.
           PERFORM OPEN-IT.
           MOVE BW-PATH-SQL TO E-FILE.

           EXEC SQL
                CONNECT TO :SQL-CONNECT-SERVER AS BW1_CONNECT
                      USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BW1-FILE.
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY TO E-KEYX.

           IF ( BW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW1-FILE.

           IF ( QBW1-WEND-OWNBR = ZEROES    ) OR
              ( QBW1-WEND-OWNBR NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW1-WBEG-OWNBR
                                           QBW1-WEND-OWNBR
              MOVE BW-SSNO              TO QBW1-WBEG-SSNO
              MOVE ALL "9"              TO QBW1-WEND-SSNO.

           EXEC SQL SET CONNECTION BW1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BW1_CURSOR CURSOR FOR
            SELECT BWFILE.BW_OWNBR,
                   ISNULL(BWFILE.BW_LNAME,''),
                   ISNULL(BWFILE.BW_FNAME,''),
                   BWFILE.BW_SPSSNO,
                   ISNULL(BWFILE.BW_SPLNAME,''),
                   ISNULL(BWFILE.BW_SPFNAME,''),
                   BWFILE.BW_SSNO,
                   ISNULL(CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_SOLICIT,'')
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR >= :QBW1-WBEG-OWNBR
              AND BWFILE.BW_OWNBR <= :QBW1-WEND-OWNBR
              AND BWFILE.BW_SSNO >= :QBW1-WBEG-SSNO
              AND BWFILE.BW_SSNO <= :QBW1-WEND-SSNO
           END-EXEC.

           EXEC SQL
               OPEN BW1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBW1-WBEG-OWNBR 
                          QBW1-WEND-OWNBR
                          QBW1-WBEG-SSNO
                          QBW1-WEND-SSNO.

           MOVE STAT---GOOD TO BW1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BW1-FILE.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR. 
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           MOVE BW-OWNBR   TO QBW-OWNBR.
           MOVE BW-SSNO    TO QBW-SSNO.

           EXEC SQL SET CONNECTION BW1_CONNECT END-EXEC.
           EXEC SQL
            SELECT BWFILE.BW_OWNBR,
                   ISNULL(BWFILE.BW_LNAME,''),
                   ISNULL(BWFILE.BW_FNAME,''),
                   BWFILE.BW_SPSSNO,
                   ISNULL(BWFILE.BW_SPLNAME,''),
                   ISNULL(BWFILE.BW_SPFNAME,''),
                   BWFILE.BW_SSNO,
                   ISNULL(CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_SOLICIT,'')
            INTO 
               :QBW-OWNBR, :QBW-LNAME, :QBW-FNAME, :QBW-SPSSNO,
               :QBW-SPLNAME, :QBW-SPFNAME, :QBW-SSNO, :QBW-REFDATE,
               :QBW-SOLICIT
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR = :QBW-OWNBR
              AND BWFILE.BW_SSNO = :QBW-SSNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       READ-BW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY TO E-KEYX.
           INITIALIZE QBW-REC.

           EXEC SQL SET CONNECTION BW1_CONNECT END-EXEC.
           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH BW1_CURSOR INTO 
                :QBW-OWNBR, :QBW-LNAME, :QBW-FNAME, :QBW-SPSSNO,
                :QBW-SPLNAME, :QBW-SPFNAME, :QBW-SSNO, :QBW-REFDATE,
                :QBW-SOLICIT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLCODE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BW1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BW1-CURSOR-STAT.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *COPY "LIBLP/LPLC1RN.CPY".
       LOAD-LC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LC-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LC-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO LC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LC-ALL-MACHINE
              MOVE LC-ALL-PATH             TO LC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LC-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO LC-ALL-BASE
              MOVE FILPATH-MACHINE         TO LC-ALL-MACHINE
              MOVE LC-ALL-PATH             TO LC-PATH.

      *-----------------------------------------------------------------
       OPEN-LC1-FILE.
           PERFORM LOAD-LC1-FILE.
           PERFORM OPEN-IT.
           MOVE LC-PATH-SQL TO E-FILE.

           EXEC SQL
                CONNECT TO :SQL-CONNECT-SERVER AS LC1_CONNECT
                      USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LC1-FILE.
           PERFORM START-IT.
           MOVE LC-PATH-SQL TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           IF ( LC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LC1-FILE.

           IF ( QLC1-WEND-CODE = ZEROES    ) OR
              ( QLC1-WEND-CODE NOT NUMERIC )
              MOVE LC-CODE              TO QLC1-WBEG-CODE
              MOVE ALL "9"              TO QLC1-WEND-CODE.

           EXEC SQL SET CONNECTION LC1_CONNECT END-EXEC.
           EXEC SQL
            DECLARE LC1_CURSOR CURSOR FOR
            SELECT LCFILE.LC_CODE,
                   ISNULL(LCFILE.LC_DESC,'')
            FROM DBO.LCFILE
            WHERE LCFILE.LC_CODE >= :QLC1-WBEG-CODE
              AND LCFILE.LC_CODE <= :QLC1-WEND-CODE
           END-EXEC.

           EXEC SQL
               OPEN LC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLC1-WBEG-CODE 
                          QLC1-WEND-CODE.

           MOVE STAT---GOOD TO LC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-LC1-FILE-GREATER.
           PERFORM START-IT.
           MOVE LC-PATH-SQL TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           IF ( LC1-GR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LC1-FILE.

           IF ( QLC1-WEND-CODE = ZEROES    ) OR
              ( QLC1-WEND-CODE NOT NUMERIC )
              MOVE LC-CODE              TO QLC1-WBEG-CODE
              MOVE ALL "9"              TO QLC1-WEND-CODE.

           EXEC SQL SET CONNECTION LC1_CONNECT END-EXEC.
           EXEC SQL
            DECLARE LC1_GR_CURSOR CURSOR FOR
            SELECT LCFILE.LC_CODE,
                   ISNULL(LCFILE.LC_DESC,'')
            FROM DBO.LCFILE
            WHERE LCFILE.LC_CODE >  :QLC1-WBEG-CODE
              AND LCFILE.LC_CODE <= :QLC1-WEND-CODE
           END-EXEC.

           EXEC SQL
               OPEN LC1_GR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLC1-WBEG-CODE 
                          QLC1-WEND-CODE.

           MOVE STAT---GOOD TO LC1-GR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LC1-FILE.
           PERFORM READ-IT.
           MOVE LC-PATH-SQL TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           MOVE LC-CODE TO QLC-CODE.

           EXEC SQL SET CONNECTION LC1_CONNECT END-EXEC.
           EXEC SQL
            SELECT LCFILE.LC_CODE,
                   ISNULL(LCFILE.LC_DESC,'')
            INTO
                   :QLC-CODE, :QLC-DESC
            FROM DBO.LCFILE
            WHERE LCFILE.LC_CODE = :QLC-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-LC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LC-FIELDS.

      *-----------------------------------------------------------------
      *    STILL LOADING FILE WORKERS LIKE E-WORKERS
      *-----------------------------------------------------------------
       READ-LC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LC-PATH-SQL TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.
           INITIALIZE QLC-REC.

           EXEC SQL SET CONNECTION LC1_CONNECT END-EXEC.
           IF ( LC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LC1_CURSOR INTO 
                   :QLC-CODE, :QLC-DESC
              END-EXEC
           ELSE
           IF ( LC1-GR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LC1_GR_CURSOR INTO
                   :QLC-CODE, :QLC-DESC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLCODE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-LC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LC1-FILE.

           IF ( LC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LC1-CURSOR-STAT.

           IF ( LC1-GR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LC1_GR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LC1-GR-CURSOR-STAT.


      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *COPY "LIBLP/LPSP1RN.CPY".
       LOAD-SP1-FILE.
           PERFORM LOAD-SPA1-FILE.
           PERFORM LOAD-SPB1-FILE.
           PERFORM LOAD-SPC1-FILE.

      *-----------------------------------------------------------------
       OPEN-SP1-FILE.
           PERFORM OPEN-SPA1-FILE.
           IF ( IO-FG = 0 )
               PERFORM OPEN-SPB1-FILE
               IF ( IO-FG = 0 )
                  PERFORM OPEN-SPC1-FILE
                  IF ( IO-FG = 0 )
                     MOVE " " TO IO-MULTI-FG
                  ELSE
                     MOVE "C" TO IO-MULTI-FG
               ELSE
                  MOVE "B" TO IO-MULTI-FG
            ELSE
               MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       START-SP1-FILE.
           PERFORM SET-SP1-TO-ALL.
           PERFORM START-SPA1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-SP1-FILE.
           PERFORM SET-SP1-TO-ALL.
           PERFORM READ-SPA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM READ-SPB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM READ-SPC1-FILE
                 IF ( IO-FG = 0 )
                    PERFORM SET-ALL-TO-SP1
                 ELSE
                    MOVE "C" TO IO-MULTI-FG
              ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       READ-SP1-FILE-NEXT.
           PERFORM SET-SP1-TO-ALL.
           PERFORM READ-SPA1-FILE-NEXT.
           IF ( IO-FG = 0 )
              MOVE SPA1-KEY TO SPB1-KEY
              PERFORM READ-SPB1-FILE
              IF ( IO-FG = 0 )
                 MOVE SPA1-KEY TO SPC1-KEY
                 PERFORM READ-SPC1-FILE
                 IF ( IO-FG = 0 )
                    PERFORM SET-ALL-TO-SP1
                 ELSE
                    MOVE "C" TO IO-MULTI-FG
              ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       CLOSE-SP1-FILE.
           PERFORM CLOSE-SPA1-FILE.
           PERFORM CLOSE-SPB1-FILE.
           PERFORM CLOSE-SPC1-FILE.

      *================================================================E
       LOAD-SPA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SP-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SP-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SPA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SPA-ALL-MACHINE
              MOVE SPA-ALL-PATH             TO SPA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SP-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SPA-ALL-BASE
              MOVE FILPATH-MACHINE         TO SPA-ALL-MACHINE
              MOVE SPA-ALL-PATH             TO SPA-PATH.

      *-----------------------------------------------------------------
       OPEN-SPA1-FILE.
           PERFORM LOAD-SPA1-FILE.
           PERFORM OPEN-IT.
           MOVE SPA-PATH-SQL TO E-FILE.
           EXEC SQL
                CONNECT TO :SQL-CONNECT-SERVER AS SPA1_CONNECT
                      USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.
      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SPA1-FILE.
           PERFORM START-IT.
           MOVE SPA-PATH-SQL TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.

           IF ( SPA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SPA1-FILE.

           IF ( QSP1-WEND-ORGST = SPACES  )
              MOVE SPA-ORGST            TO QSP1-WBEG-ORGST 
              MOVE ALL "Z"              TO QSP1-WEND-ORGST 
              MOVE SPA-SPRCLASS         TO QSP1-WBEG-SPRCLASS
              MOVE ALL "9"              TO QSP1-WEND-SPRCLASS
              MOVE SPA-SUBCLASS         TO QSP1-WBEG-SUBCLASS
              MOVE ALL "9"              TO QSP1-WEND-SUBCLASS
              MOVE SPA-LAWCODE          TO QSP1-WBEG-LAWCODE
              MOVE ALL "9"              TO QSP1-WEND-LAWCODE.

           EXEC SQL SET CONNECTION SPA1_CONNECT END-EXEC.
           EXEC SQL
            DECLARE SPA1_CURSOR CURSOR FOR
            SELECT ISNULL(SPAFILE.SPA_ORGST,''),
                   SPAFILE.SPA_SPRCLASS,
                   SPAFILE.SPA_SUBCLASS,
                   SPAFILE.SPA_LAWCODE,
                   ISNULL(SPAFILE.SPA_TRWBANKRULE,''),
                   ISNULL(SPAFILE.SPA_BANKRULE,''),
                   SPAFILE.SPA_TRWCONTPDTH,
                   SPAFILE.SPA_CONTPDTH,
                   ISNULL(SPAFILE.SPA_TRWBANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_BANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_RECFRMLA,''),
                   SPAFILE.SPA_RECPDTH,
                   ISNULL(SPAFILE.SPA_DEFUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_LCUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_EXPR_FRMLA,''),
                   ISNULL(SPAFILE.SPA_JUDGFRMLA,''),
                   ISNULL(SPAFILE.SPA_ACC_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPR_REDU_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPD_FRMLA,''),
                   SPAFILE.SPA_EXPD_MONTHS,
                   SPAFILE.SPA_EXPD_DAYS
            FROM DBO.SPAFILE
            WHERE SPAFILE.SPA_ORGST >= :QSP1-WBEG-ORGST
              AND SPAFILE.SPA_ORGST <= :QSP1-WEND-ORGST
              AND SPAFILE.SPA_SPRCLASS >= :QSP1-WBEG-SPRCLASS
              AND SPAFILE.SPA_SPRCLASS <= :QSP1-WEND-SPRCLASS
              AND SPAFILE.SPA_SUBCLASS >= :QSP1-WBEG-SUBCLASS
              AND SPAFILE.SPA_SUBCLASS <= :QSP1-WEND-SUBCLASS
              AND SPAFILE.SPA_LAWCODE >= :QSP1-WBEG-LAWCODE 
              AND SPAFILE.SPA_LAWCODE <= :QSP1-WEND-LAWCODE 
           END-EXEC.

           EXEC SQL
               OPEN SPA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QSP1-WBEG-ORGST
                          QSP1-WEND-ORGST.

           MOVE ZEROES TO QSP1-WBEG-SPRCLASS
                          QSP1-WEND-SPRCLASS
                          QSP1-WBEG-SUBCLASS
                          QSP1-WEND-SUBCLASS
                          QSP1-WBEG-LAWCODE
                          QSP1-WEND-LAWCODE.

           MOVE STAT---GOOD TO SPA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SPA1-FILE.
           PERFORM READ-IT.
           MOVE SPA-PATH-SQL TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.

           MOVE SPA-ORGST            TO QSPA-ORGST.
           MOVE SPA-SPRCLASS         TO QSPA-SPRCLASS.
           MOVE SPA-SUBCLASS         TO QSPA-SUBCLASS.
           MOVE SPA-LAWCODE          TO QSPA-LAWCODE.

           EXEC SQL SET CONNECTION SPA1_CONNECT END-EXEC.
           EXEC SQL
            SELECT ISNULL(SPAFILE.SPA_ORGST,''),
                   SPAFILE.SPA_SPRCLASS,
                   SPAFILE.SPA_SUBCLASS,
                   SPAFILE.SPA_LAWCODE,
                   ISNULL(SPAFILE.SPA_TRWBANKRULE,''),
                   ISNULL(SPAFILE.SPA_BANKRULE,''),
                   SPAFILE.SPA_TRWCONTPDTH,
                   SPAFILE.SPA_CONTPDTH,
                   ISNULL(SPAFILE.SPA_TRWBANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_BANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_RECFRMLA,''),
                   SPAFILE.SPA_RECPDTH,
                   ISNULL(SPAFILE.SPA_DEFUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_LCUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_EXPR_FRMLA,''),
                   ISNULL(SPAFILE.SPA_JUDGFRMLA,''),
                   ISNULL(SPAFILE.SPA_ACC_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPR_REDU_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPD_FRMLA,''),
                   SPAFILE.SPA_EXPD_MONTHS,
                   SPAFILE.SPA_EXPD_DAYS
            INTO 
                   :QSPA-ORGST, :QSPA-SPRCLASS, :QSPA-SUBCLASS,
                     :QSPA-LAWCODE, :QSPA-TRWBANKRULE, :QSPA-BANKRULE,
                     :QSPA-TRWCONTPDTH, :QSPA-CONTPDTH,
                     :QSPA-TRWBANKUNITPER-CD, :QSPA-BANKUNITPER-CD,
                     :QSPA-RECFRMLA, :QSPA-RECPDTH, :QSPA-DEFUNITPER-CD,
                     :QSPA-LCUNITPER-CD, :QSPA-EXPR-FRMLA,
                     :QSPA-JUDGFRMLA, :QSPA-ACC-FRMLA,
                     :QSPA-EXPR-REDU-FRMLA, :QSPA-EXPD-FRMLA,
                     :QSPA-EXPD-MONTHS, :QSPA-EXPD-DAYS
            FROM DBO.SPAFILE
            WHERE SPAFILE.SPA_ORGST = :QSPA-ORGST
              AND SPAFILE.SPA_SPRCLASS = :QSPA-SPRCLASS
              AND SPAFILE.SPA_SUBCLASS = :QSPA-SUBCLASS
              AND SPAFILE.SPA_LAWCODE = :QSPA-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SPA-FIELDS.

      *-----------------------------------------------------------------
       READ-SPA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SPA-PATH-SQL TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.
           INITIALIZE QSPA-REC.

           EXEC SQL SET CONNECTION SPA1_CONNECT END-EXEC.
           IF ( SPA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SPA1_CURSOR INTO 
                   :QSPA-ORGST, :QSPA-SPRCLASS, :QSPA-SUBCLASS,
                     :QSPA-LAWCODE, :QSPA-TRWBANKRULE, :QSPA-BANKRULE,
                     :QSPA-TRWCONTPDTH, :QSPA-CONTPDTH,
                     :QSPA-TRWBANKUNITPER-CD, :QSPA-BANKUNITPER-CD,
                     :QSPA-RECFRMLA, :QSPA-RECPDTH, :QSPA-DEFUNITPER-CD,
                     :QSPA-LCUNITPER-CD, :QSPA-EXPR-FRMLA,
                     :QSPA-JUDGFRMLA, :QSPA-ACC-FRMLA,
                     :QSPA-EXPR-REDU-FRMLA, :QSPA-EXPD-FRMLA,
                     :QSPA-EXPD-MONTHS, :QSPA-EXPD-DAYS
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SPA-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SPA1-FILE.
           IF ( SPA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SPA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SPA1-CURSOR-STAT.

      *=================================================================
       LOAD-SPB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SP-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SP-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SPB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SPB-ALL-MACHINE
              MOVE SPB-ALL-PATH             TO SPB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SP-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SPB-ALL-BASE
              MOVE FILPATH-MACHINE         TO SPB-ALL-MACHINE
              MOVE SPB-ALL-PATH             TO SPB-PATH.

      *-----------------------------------------------------------------
       OPEN-SPB1-FILE.
           PERFORM LOAD-SPB1-FILE.
           PERFORM OPEN-IT.
           MOVE SPB-PATH-SQL TO E-FILE.
           EXEC SQL
                CONNECT TO :SQL-CONNECT-SERVER AS SPB1_CONNECT
                      USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.

      *-----------------------------------------------------------------
       READ-SPB1-FILE.
           PERFORM READ-IT.
           MOVE SPB-PATH-SQL TO E-FILE.
           MOVE SPB1-KEY TO E-KEYX.

           MOVE SPB-ORGST            TO QSPB-ORGST.
           MOVE SPB-SPRCLASS         TO QSPB-SPRCLASS.
           MOVE SPB-SUBCLASS         TO QSPB-SUBCLASS.
           MOVE SPB-LAWCODE          TO QSPB-LAWCODE.

           EXEC SQL SET CONNECTION SPB1_CONNECT END-EXEC.
           EXEC SQL
            SELECT ISNULL(SPBFILE.SPB_ORGST,''),
                   SPBFILE.SPB_SPRCLASS,
                   SPBFILE.SPB_SUBCLASS,
                   SPBFILE.SPB_LAWCODE
            INTO 
                   :QSPA-ORGST, :QSPA-SPRCLASS, :QSPA-SUBCLASS,
                     :QSPA-LAWCODE
            FROM DBO.SPBFILE
            WHERE SPBFILE.SPB_ORGST = :QSPB-ORGST
              AND SPBFILE.SPB_SPRCLASS = :QSPB-SPRCLASS
              AND SPBFILE.SPB_SUBCLASS = :QSPB-SUBCLASS
              AND SPBFILE.SPB_LAWCODE = :QSPB-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SPB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SPB1-FILE.

      *=================================================================
       LOAD-SPC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SP-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SP-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SPC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SPC-ALL-MACHINE
              MOVE SPC-ALL-PATH             TO SPC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SP-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SPC-ALL-BASE
              MOVE FILPATH-MACHINE         TO SPC-ALL-MACHINE
              MOVE SPC-ALL-PATH             TO SPC-PATH.

      *-----------------------------------------------------------------
       OPEN-SPC1-FILE.
           PERFORM LOAD-SPC1-FILE.
           PERFORM OPEN-IT.
           MOVE SPC-PATH-SQL TO E-FILE.
           EXEC SQL
                CONNECT TO :SQL-CONNECT-SERVER AS SPC1_CONNECT
                      USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.

      *-----------------------------------------------------------------
       READ-SPC1-FILE.
           PERFORM READ-IT.
           MOVE SPC-PATH-SQL TO E-FILE.
           MOVE SPC1-KEY TO E-KEYX.

           MOVE SPC-ORGST            TO QSPC-ORGST.
           MOVE SPC-SPRCLASS         TO QSPC-SPRCLASS.
           MOVE SPC-SUBCLASS         TO QSPC-SUBCLASS.
           MOVE SPC-LAWCODE          TO QSPC-LAWCODE.

           EXEC SQL SET CONNECTION SPC1_CONNECT END-EXEC.
           EXEC SQL
            SELECT ISNULL(SPCFILE.SPC_ORGST,''),
                   SPCFILE.SPC_SPRCLASS,
                   SPCFILE.SPC_SUBCLASS,
                   SPCFILE.SPC_LAWCODE,
                   ISNULL(SPCFILE.SPC_RDPOTCHGOFF_CD,''),
                   ISNULL(SPCFILE.SPC_CPOT30_CD,''),
                   ISNULL(SPCFILE.SPC_CPOTCHGOFF_CD,''),
                   SPCFILE.SPC_BANKRULE_RATE,
                   ISNULL(SPCFILE.SPC_CONTR_CREDIT_CD,''),
                   SPCFILE.SPC_CONTR_CREDIT,
                   SPCFILE.SPC_CONTR_CREDIT_MIN_DEL_PER
            INTO 
               :QSPC-ORGST, :QSPC-SPRCLASS, :QSPC-SUBCLASS,
               :QSPC-LAWCODE, :QSPC-RDPOTCHGOFF-CD,
               :QSPC-CPOT30-CD, :QSPC-CPOTCHGOFF-CD,
               :QSPC-BANKRULE-RATE, :QSPC-CONTR-CREDIT-CD,
               :QSPC-CONTR-CREDIT, :QSPC-CONTR-CREDIT-MIN-DEL-PER
            FROM DBO.SPCFILE
            WHERE SPCFILE.SPC_ORGST = :QSPC-ORGST
              AND SPCFILE.SPC_SPRCLASS = :QSPC-SPRCLASS
              AND SPCFILE.SPC_SUBCLASS = :QSPC-SUBCLASS
              AND SPCFILE.SPC_LAWCODE = :QSPC-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SPC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SPC1-FILE.

      *-----------------------------------------------------------------
       SET-ALL-TO-SP1.
           MOVE SPA1-KEY   TO SP1-KEY.
           MOVE SPA-AREA-A TO SP-AREA-A.
           MOVE SPB-AREA-B TO SP-AREA-B.
           MOVE SPC-AREA-C TO SP-AREA-C.

       SET-SP1-TO-ALL.
           MOVE SP1-KEY   TO SPA1-KEY.
           MOVE SP-AREA-A TO SPA-AREA-A.
           MOVE SP1-KEY   TO SPB1-KEY.
           MOVE SP-AREA-B TO SPB-AREA-B.
           MOVE SP1-KEY   TO SPC1-KEY.
           MOVE SP-AREA-C TO SPC-AREA-C.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *COPY "LIBLP/LPLTP1RN.CPY".
       GET-LT-P-REC.
           MOVE 0 TO IO-FG.
           IF NOT (LN-ACCTNO = LTP-ACCTNO)
              MOVE LN-OWNBR  TO LTP-BRNO
              MOVE LN-ACCTNO TO LTP-ACCTNO
              MOVE 1         TO LTP-SEQNO
              PERFORM OPEN-LTP1-FILE
              PERFORM READ-LTP1-FILE
              PERFORM CLOSE-LTP1-FILE.

       LOAD-LTP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTP-ALL-DATA
              MOVE LTP-ALL-PATH             TO LTP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTP-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTP-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTP-ALL-DATA
              MOVE LTP-ALL-PATH             TO LTP-PATH.

      *-----------------------------------------------------------------
       OPEN-LTP1-FILE.
           PERFORM LOAD-LTP1-FILE.
           PERFORM OPEN-IT.
           MOVE LTP-PATH-SQL TO E-FILE.

           EXEC SQL
                CONNECT TO :SQL-CONNECT-SERVER AS LTP1_CONNECT
                      USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.
      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTP1-FILE.
           PERFORM START-IT.
           MOVE LTP-PATH-OWNBR  TO LTP-BRNO.
           MOVE LTP-PATH-SQL    TO E-FILE.
           MOVE LTP1-KEY TO E-KEYX.

           IF ( LTP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTP1-FILE.

           IF ( QLTP1-WEND-BRANCH = ZEROES    ) OR
              ( QLTP1-WEND-BRANCH NOT NUMERIC )
              MOVE LTP-PATH-OWNBR        TO QLTP1-WBEG-BRANCH
              MOVE LTP-PATH-OWNBR        TO QLTP1-WEND-BRANCH
              MOVE LTP-ACCTNO            TO QLTP1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTP1-WEND-ACCTNO
              MOVE LTP-SEQNO             TO QLTP1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTP1-WEND-SEQNO.

           EXEC SQL SET CONNECTION LTP1_CONNECT END-EXEC.
           EXEC SQL
            DECLARE LTP1_CURSOR CURSOR FOR
            SELECT LTPFILE.LTP_BRNO,
                   LTPFILE.LTP_ACCTNO,
                   LTPFILE.LTP_SEQNO,
                   LTPFILE.LTP_SCHLD_PAYMNT_1,
                   LTPFILE.LTP_SCHLD_PAYMNT_2,
                   LTPFILE.LTP_SCHLD_PAYMNT_3,
                   LTPFILE.LTP_SCHLD_PAYMNT_4,
                   LTPFILE.LTP_SCHLD_PAYMNT_5,
                   LTPFILE.LTP_SCHLD_PAYMNT_6,
                   LTPFILE.LTP_SCHLD_PAYMNT_7,
                   LTPFILE.LTP_SCHLD_PAYMNT_8,
                   LTPFILE.LTP_SCHLD_PAYMNT_9,
                   LTPFILE.LTP_SCHLD_PAYMNT_10,
                   LTPFILE.LTP_SCHLD_PAYMNT_11,
                   LTPFILE.LTP_SCHLD_PAYMNT_12,
                   LTPFILE.LTP_SCHLD_PAYMNT_13,
                   LTPFILE.LTP_SCHLD_PAYMNT_14,
                   LTPFILE.LTP_SCHLD_PAYMNT_15,
                   LTPFILE.LTP_SCHLD_PAYMNT_16,
                   LTPFILE.LTP_SCHLD_PAYMNT_17,
                   LTPFILE.LTP_SCHLD_PAYMNT_18,
                   LTPFILE.LTP_SCHLD_PAYMNT_19,
                   LTPFILE.LTP_SCHLD_PAYMNT_20,
                   LTPFILE.LTP_SCHLD_PAYMNT_21,
                   LTPFILE.LTP_SCHLD_PAYMNT_22,
                   LTPFILE.LTP_SCHLD_PAYMNT_23,
                   LTPFILE.LTP_SCHLD_PAYMNT_24,
                   LTPFILE.LTP_SCHLD_PAYMNT_25,
                   LTPFILE.LTP_SCHLD_PAYMNT_26,
                   LTPFILE.LTP_SCHLD_PAYMNT_27,
                   LTPFILE.LTP_SCHLD_PAYMNT_28,
                   LTPFILE.LTP_SCHLD_PAYMNT_29,
                   LTPFILE.LTP_SCHLD_PAYMNT_30,
                   LTPFILE.LTP_SCHLD_PAYMNT_31,
                   LTPFILE.LTP_SCHLD_PAYMNT_32,
                   LTPFILE.LTP_SCHLD_PAYMNT_33,
                   LTPFILE.LTP_SCHLD_PAYMNT_34,
                   LTPFILE.LTP_SCHLD_PAYMNT_35,
                   LTPFILE.LTP_SCHLD_PAYMNT_36,
                   LTPFILE.LTP_SCHLD_PAYMNT_37,
                   LTPFILE.LTP_SCHLD_PAYMNT_38,
                   LTPFILE.LTP_SCHLD_PAYMNT_39,
                   LTPFILE.LTP_SCHLD_PAYMNT_40,
                   LTPFILE.LTP_SCHLD_PAYMNT_41,
                   LTPFILE.LTP_SCHLD_TERM_1,
                   LTPFILE.LTP_SCHLD_TERM_2,
                   LTPFILE.LTP_SCHLD_TERM_3,
                   LTPFILE.LTP_SCHLD_TERM_4,
                   LTPFILE.LTP_SCHLD_TERM_5,
                   LTPFILE.LTP_SCHLD_TERM_6,
                   LTPFILE.LTP_SCHLD_TERM_7,
                   LTPFILE.LTP_SCHLD_TERM_8,
                   LTPFILE.LTP_SCHLD_TERM_9,
                   LTPFILE.LTP_SCHLD_TERM_10,
                   LTPFILE.LTP_SCHLD_TERM_11,
                   LTPFILE.LTP_SCHLD_TERM_12,
                   LTPFILE.LTP_SCHLD_TERM_13,
                   LTPFILE.LTP_SCHLD_TERM_14,
                   LTPFILE.LTP_SCHLD_TERM_15,
                   LTPFILE.LTP_SCHLD_TERM_16,
                   LTPFILE.LTP_SCHLD_TERM_17,
                   LTPFILE.LTP_SCHLD_TERM_18,
                   LTPFILE.LTP_SCHLD_TERM_19,
                   LTPFILE.LTP_SCHLD_TERM_20,
                   LTPFILE.LTP_SCHLD_TERM_21,
                   LTPFILE.LTP_SCHLD_TERM_22,
                   LTPFILE.LTP_SCHLD_TERM_23,
                   LTPFILE.LTP_SCHLD_TERM_24,
                   LTPFILE.LTP_SCHLD_TERM_25,
                   LTPFILE.LTP_SCHLD_TERM_26,
                   LTPFILE.LTP_SCHLD_TERM_27,
                   LTPFILE.LTP_SCHLD_TERM_28,
                   LTPFILE.LTP_SCHLD_TERM_29,
                   LTPFILE.LTP_SCHLD_TERM_30,
                   LTPFILE.LTP_SCHLD_TERM_31,
                   LTPFILE.LTP_SCHLD_TERM_32,
                   LTPFILE.LTP_SCHLD_TERM_33,
                   LTPFILE.LTP_SCHLD_TERM_34,
                   LTPFILE.LTP_SCHLD_TERM_35,
                   LTPFILE.LTP_SCHLD_TERM_36,
                   LTPFILE.LTP_SCHLD_TERM_37,
                   LTPFILE.LTP_SCHLD_TERM_38,
                   LTPFILE.LTP_SCHLD_TERM_39,
                   LTPFILE.LTP_SCHLD_TERM_40,
                   LTPFILE.LTP_SCHLD_TERM_41
            FROM DBO.LTPFILE
            WHERE LTPFILE.LTP_BRNO >= :QLTP1-WBEG-BRANCH
              AND LTPFILE.LTP_BRNO <= :QLTP1-WEND-BRANCH
              AND LTPFILE.LTP_ACCTNO >= :QLTP1-WBEG-ACCTNO
              AND LTPFILE.LTP_ACCTNO <= :QLTP1-WEND-ACCTNO
              AND LTPFILE.LTP_SEQNO >= :QLTP1-WEND-SEQNO
              AND LTPFILE.LTP_SEQNO <= :QLTP1-WEND-SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTP1-WBEG-BRANCH 
                          QLTP1-WEND-BRANCH
                          QLTP1-WBEG-ACCTNO
                          QLTP1-WEND-ACCTNO
                          QLTP1-WBEG-SEQNO
                          QLTP1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTP1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTP1-FILE.
           PERFORM READ-IT.
           MOVE LTP-PATH-OWNBR  TO LTP-BRNO. 
           MOVE LTP-PATH-SQL    TO E-FILE.

           MOVE LTP-BRNO   TO QLTP-BRNO.
           MOVE LTP-ACCTNO TO QLTP-ACCTNO.
           MOVE LTP-SEQNO  TO QLTP-SEQNO.

           EXEC SQL SET CONNECTION LTP1_CONNECT END-EXEC.
           EXEC SQL
            SELECT LTPFILE.LTP_BRNO,
                   LTPFILE.LTP_ACCTNO,
                   LTPFILE.LTP_SEQNO,
                   LTPFILE.LTP_SCHLD_PAYMNT_1,
                   LTPFILE.LTP_SCHLD_PAYMNT_2,
                   LTPFILE.LTP_SCHLD_PAYMNT_3,
                   LTPFILE.LTP_SCHLD_PAYMNT_4,
                   LTPFILE.LTP_SCHLD_PAYMNT_5,
                   LTPFILE.LTP_SCHLD_PAYMNT_6,
                   LTPFILE.LTP_SCHLD_PAYMNT_7,
                   LTPFILE.LTP_SCHLD_PAYMNT_8,
                   LTPFILE.LTP_SCHLD_PAYMNT_9,
                   LTPFILE.LTP_SCHLD_PAYMNT_10,
                   LTPFILE.LTP_SCHLD_PAYMNT_11,
                   LTPFILE.LTP_SCHLD_PAYMNT_12,
                   LTPFILE.LTP_SCHLD_PAYMNT_13,
                   LTPFILE.LTP_SCHLD_PAYMNT_14,
                   LTPFILE.LTP_SCHLD_PAYMNT_15,
                   LTPFILE.LTP_SCHLD_PAYMNT_16,
                   LTPFILE.LTP_SCHLD_PAYMNT_17,
                   LTPFILE.LTP_SCHLD_PAYMNT_18,
                   LTPFILE.LTP_SCHLD_PAYMNT_19,
                   LTPFILE.LTP_SCHLD_PAYMNT_20,
                   LTPFILE.LTP_SCHLD_PAYMNT_21,
                   LTPFILE.LTP_SCHLD_PAYMNT_22,
                   LTPFILE.LTP_SCHLD_PAYMNT_23,
                   LTPFILE.LTP_SCHLD_PAYMNT_24,
                   LTPFILE.LTP_SCHLD_PAYMNT_25,
                   LTPFILE.LTP_SCHLD_PAYMNT_26,
                   LTPFILE.LTP_SCHLD_PAYMNT_27,
                   LTPFILE.LTP_SCHLD_PAYMNT_28,
                   LTPFILE.LTP_SCHLD_PAYMNT_29,
                   LTPFILE.LTP_SCHLD_PAYMNT_30,
                   LTPFILE.LTP_SCHLD_PAYMNT_31,
                   LTPFILE.LTP_SCHLD_PAYMNT_32,
                   LTPFILE.LTP_SCHLD_PAYMNT_33,
                   LTPFILE.LTP_SCHLD_PAYMNT_34,
                   LTPFILE.LTP_SCHLD_PAYMNT_35,
                   LTPFILE.LTP_SCHLD_PAYMNT_36,
                   LTPFILE.LTP_SCHLD_PAYMNT_37,
                   LTPFILE.LTP_SCHLD_PAYMNT_38,
                   LTPFILE.LTP_SCHLD_PAYMNT_39,
                   LTPFILE.LTP_SCHLD_PAYMNT_40,
                   LTPFILE.LTP_SCHLD_PAYMNT_41,
                   LTPFILE.LTP_SCHLD_TERM_1,
                   LTPFILE.LTP_SCHLD_TERM_2,
                   LTPFILE.LTP_SCHLD_TERM_3,
                   LTPFILE.LTP_SCHLD_TERM_4,
                   LTPFILE.LTP_SCHLD_TERM_5,
                   LTPFILE.LTP_SCHLD_TERM_6,
                   LTPFILE.LTP_SCHLD_TERM_7,
                   LTPFILE.LTP_SCHLD_TERM_8,
                   LTPFILE.LTP_SCHLD_TERM_9,
                   LTPFILE.LTP_SCHLD_TERM_10,
                   LTPFILE.LTP_SCHLD_TERM_11,
                   LTPFILE.LTP_SCHLD_TERM_12,
                   LTPFILE.LTP_SCHLD_TERM_13,
                   LTPFILE.LTP_SCHLD_TERM_14,
                   LTPFILE.LTP_SCHLD_TERM_15,
                   LTPFILE.LTP_SCHLD_TERM_16,
                   LTPFILE.LTP_SCHLD_TERM_17,
                   LTPFILE.LTP_SCHLD_TERM_18,
                   LTPFILE.LTP_SCHLD_TERM_19,
                   LTPFILE.LTP_SCHLD_TERM_20,
                   LTPFILE.LTP_SCHLD_TERM_21,
                   LTPFILE.LTP_SCHLD_TERM_22,
                   LTPFILE.LTP_SCHLD_TERM_23,
                   LTPFILE.LTP_SCHLD_TERM_24,
                   LTPFILE.LTP_SCHLD_TERM_25,
                   LTPFILE.LTP_SCHLD_TERM_26,
                   LTPFILE.LTP_SCHLD_TERM_27,
                   LTPFILE.LTP_SCHLD_TERM_28,
                   LTPFILE.LTP_SCHLD_TERM_29,
                   LTPFILE.LTP_SCHLD_TERM_30,
                   LTPFILE.LTP_SCHLD_TERM_31,
                   LTPFILE.LTP_SCHLD_TERM_32,
                   LTPFILE.LTP_SCHLD_TERM_33,
                   LTPFILE.LTP_SCHLD_TERM_34,
                   LTPFILE.LTP_SCHLD_TERM_35,
                   LTPFILE.LTP_SCHLD_TERM_36,
                   LTPFILE.LTP_SCHLD_TERM_37,
                   LTPFILE.LTP_SCHLD_TERM_38,
                   LTPFILE.LTP_SCHLD_TERM_39,
                   LTPFILE.LTP_SCHLD_TERM_40,
                   LTPFILE.LTP_SCHLD_TERM_41
            INTO 
                   :QLTP-BRNO, :QLTP-ACCTNO, :QLTP-SEQNO,
                       :QLTP-SCHLD-PAYMNT-1, :QLTP-SCHLD-PAYMNT-2,
                       :QLTP-SCHLD-PAYMNT-3, :QLTP-SCHLD-PAYMNT-4,
                       :QLTP-SCHLD-PAYMNT-5, :QLTP-SCHLD-PAYMNT-6,
                       :QLTP-SCHLD-PAYMNT-7, :QLTP-SCHLD-PAYMNT-8,
                       :QLTP-SCHLD-PAYMNT-9, :QLTP-SCHLD-PAYMNT-10,
                       :QLTP-SCHLD-PAYMNT-11, :QLTP-SCHLD-PAYMNT-12,
                       :QLTP-SCHLD-PAYMNT-13, :QLTP-SCHLD-PAYMNT-14,
                       :QLTP-SCHLD-PAYMNT-15, :QLTP-SCHLD-PAYMNT-16,
                       :QLTP-SCHLD-PAYMNT-17, :QLTP-SCHLD-PAYMNT-18,
                       :QLTP-SCHLD-PAYMNT-19, :QLTP-SCHLD-PAYMNT-20,
                       :QLTP-SCHLD-PAYMNT-21, :QLTP-SCHLD-PAYMNT-22,
                       :QLTP-SCHLD-PAYMNT-23, :QLTP-SCHLD-PAYMNT-24,
                       :QLTP-SCHLD-PAYMNT-25, :QLTP-SCHLD-PAYMNT-26,
                       :QLTP-SCHLD-PAYMNT-27, :QLTP-SCHLD-PAYMNT-28,
                       :QLTP-SCHLD-PAYMNT-29, :QLTP-SCHLD-PAYMNT-30,
                       :QLTP-SCHLD-PAYMNT-31, :QLTP-SCHLD-PAYMNT-32,
                       :QLTP-SCHLD-PAYMNT-33, :QLTP-SCHLD-PAYMNT-34,
                       :QLTP-SCHLD-PAYMNT-35, :QLTP-SCHLD-PAYMNT-36,
                       :QLTP-SCHLD-PAYMNT-37, :QLTP-SCHLD-PAYMNT-38,
                       :QLTP-SCHLD-PAYMNT-39, :QLTP-SCHLD-PAYMNT-40,
                       :QLTP-SCHLD-PAYMNT-41, :QLTP-SCHLD-TERM-1,
                       :QLTP-SCHLD-TERM-2, :QLTP-SCHLD-TERM-3,
                       :QLTP-SCHLD-TERM-4, :QLTP-SCHLD-TERM-5,
                       :QLTP-SCHLD-TERM-6, :QLTP-SCHLD-TERM-7,
                       :QLTP-SCHLD-TERM-8, :QLTP-SCHLD-TERM-9,
                       :QLTP-SCHLD-TERM-10, :QLTP-SCHLD-TERM-11,
                       :QLTP-SCHLD-TERM-12, :QLTP-SCHLD-TERM-13,
                       :QLTP-SCHLD-TERM-14, :QLTP-SCHLD-TERM-15,
                       :QLTP-SCHLD-TERM-16, :QLTP-SCHLD-TERM-17,
                       :QLTP-SCHLD-TERM-18, :QLTP-SCHLD-TERM-19,
                       :QLTP-SCHLD-TERM-20, :QLTP-SCHLD-TERM-21,
                       :QLTP-SCHLD-TERM-22, :QLTP-SCHLD-TERM-23,
                       :QLTP-SCHLD-TERM-24, :QLTP-SCHLD-TERM-25,
                       :QLTP-SCHLD-TERM-26, :QLTP-SCHLD-TERM-27,
                       :QLTP-SCHLD-TERM-28, :QLTP-SCHLD-TERM-29,
                       :QLTP-SCHLD-TERM-30, :QLTP-SCHLD-TERM-31,
                       :QLTP-SCHLD-TERM-32, :QLTP-SCHLD-TERM-33,
                       :QLTP-SCHLD-TERM-34, :QLTP-SCHLD-TERM-35,
                       :QLTP-SCHLD-TERM-36, :QLTP-SCHLD-TERM-37,
                       :QLTP-SCHLD-TERM-38, :QLTP-SCHLD-TERM-39,
                       :QLTP-SCHLD-TERM-40, :QLTP-SCHLD-TERM-41
            FROM DBO.LTPFILE
            WHERE LTPFILE.LTP_BRNO = :QLTP-BRNO
              AND LTPFILE.LTP_ACCTNO = :QLTP-ACCTNO
              AND LTPFILE.LTP_SEQNO = :QLTP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTP-FIELDS.

      *-----------------------------------------------------------------
       READ-LTP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTP-PATH-OWNBR  TO LTP-BRNO.
           MOVE LTP-PATH-SQL    TO E-FILE.
           MOVE LTP1-KEY TO E-KEYX.
           INITIALIZE QLTP-REC.

           EXEC SQL SET CONNECTION LTP1_CONNECT END-EXEC.
           IF ( LTP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTP1_CURSOR INTO 
                   :QLTP-BRNO, :QLTP-ACCTNO, :QLTP-SEQNO,
                       :QLTP-SCHLD-PAYMNT-1, :QLTP-SCHLD-PAYMNT-2,
                       :QLTP-SCHLD-PAYMNT-3, :QLTP-SCHLD-PAYMNT-4,
                       :QLTP-SCHLD-PAYMNT-5, :QLTP-SCHLD-PAYMNT-6,
                       :QLTP-SCHLD-PAYMNT-7, :QLTP-SCHLD-PAYMNT-8,
                       :QLTP-SCHLD-PAYMNT-9, :QLTP-SCHLD-PAYMNT-10,
                       :QLTP-SCHLD-PAYMNT-11, :QLTP-SCHLD-PAYMNT-12,
                       :QLTP-SCHLD-PAYMNT-13, :QLTP-SCHLD-PAYMNT-14,
                       :QLTP-SCHLD-PAYMNT-15, :QLTP-SCHLD-PAYMNT-16,
                       :QLTP-SCHLD-PAYMNT-17, :QLTP-SCHLD-PAYMNT-18,
                       :QLTP-SCHLD-PAYMNT-19, :QLTP-SCHLD-PAYMNT-20,
                       :QLTP-SCHLD-PAYMNT-21, :QLTP-SCHLD-PAYMNT-22,
                       :QLTP-SCHLD-PAYMNT-23, :QLTP-SCHLD-PAYMNT-24,
                       :QLTP-SCHLD-PAYMNT-25, :QLTP-SCHLD-PAYMNT-26,
                       :QLTP-SCHLD-PAYMNT-27, :QLTP-SCHLD-PAYMNT-28,
                       :QLTP-SCHLD-PAYMNT-29, :QLTP-SCHLD-PAYMNT-30,
                       :QLTP-SCHLD-PAYMNT-31, :QLTP-SCHLD-PAYMNT-32,
                       :QLTP-SCHLD-PAYMNT-33, :QLTP-SCHLD-PAYMNT-34,
                       :QLTP-SCHLD-PAYMNT-35, :QLTP-SCHLD-PAYMNT-36,
                       :QLTP-SCHLD-PAYMNT-37, :QLTP-SCHLD-PAYMNT-38,
                       :QLTP-SCHLD-PAYMNT-39, :QLTP-SCHLD-PAYMNT-40,
                       :QLTP-SCHLD-PAYMNT-41, :QLTP-SCHLD-TERM-1,
                       :QLTP-SCHLD-TERM-2, :QLTP-SCHLD-TERM-3,
                       :QLTP-SCHLD-TERM-4, :QLTP-SCHLD-TERM-5,
                       :QLTP-SCHLD-TERM-6, :QLTP-SCHLD-TERM-7,
                       :QLTP-SCHLD-TERM-8, :QLTP-SCHLD-TERM-9,
                       :QLTP-SCHLD-TERM-10, :QLTP-SCHLD-TERM-11,
                       :QLTP-SCHLD-TERM-12, :QLTP-SCHLD-TERM-13,
                       :QLTP-SCHLD-TERM-14, :QLTP-SCHLD-TERM-15,
                       :QLTP-SCHLD-TERM-16, :QLTP-SCHLD-TERM-17,
                       :QLTP-SCHLD-TERM-18, :QLTP-SCHLD-TERM-19,
                       :QLTP-SCHLD-TERM-20, :QLTP-SCHLD-TERM-21,
                       :QLTP-SCHLD-TERM-22, :QLTP-SCHLD-TERM-23,
                       :QLTP-SCHLD-TERM-24, :QLTP-SCHLD-TERM-25,
                       :QLTP-SCHLD-TERM-26, :QLTP-SCHLD-TERM-27,
                       :QLTP-SCHLD-TERM-28, :QLTP-SCHLD-TERM-29,
                       :QLTP-SCHLD-TERM-30, :QLTP-SCHLD-TERM-31,
                       :QLTP-SCHLD-TERM-32, :QLTP-SCHLD-TERM-33,
                       :QLTP-SCHLD-TERM-34, :QLTP-SCHLD-TERM-35,
                       :QLTP-SCHLD-TERM-36, :QLTP-SCHLD-TERM-37,
                       :QLTP-SCHLD-TERM-38, :QLTP-SCHLD-TERM-39,
                       :QLTP-SCHLD-TERM-40, :QLTP-SCHLD-TERM-41
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLCODE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTP-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LTP1-FILE.

           IF ( LTP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTP1-CURSOR-STAT.

      ******************************************************************
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       OPEN-DUE-FILE.
           PERFORM OPEN-IT.
           MOVE DUE-PATH TO E-FILE.
           OPEN I-O DUE-FILE.
           IF IO-FG = 8
             GO TO OPEN-DUE-FILE.
           IF IO-FG = 7
             CLOSE DUE-FILE
             GO TO OPEN-DUE-FILE.
           UNLOCK DUE-FILE.
       OPEN-DUE-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE DUE-PATH TO E-FILE.
           OPEN OUTPUT DUE-FILE.
           IF IO-FG = 8
             GO TO OPEN-DUE-FILE.
           IF IO-FG = 7
             CLOSE DUE-FILE
             GO TO OPEN-DUE-FILE.
           UNLOCK DUE-FILE.
       WRITE-DUE-FILE.
           PERFORM WRITE-IT.
           MOVE DUE-PATH TO E-FILE.
           MOVE DUE-KEY TO E-KEYX.
           WRITE DUE-REC.
           IF IO-FG = 8
             GO TO WRITE-DUE-FILE.
           UNLOCK DUE-FILE.
       CLOSE-DUE-FILE.
           PERFORM CLOSE-IT.
           CLOSE DUE-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

*NEW*      IF EXT-ROUTE-BUF = "00"
              IF DUEDATE-SUMMARY = "F"
                 MOVE "DUE DATE ASCII FILE: XXXXXXXXXXXXXXXXXXXXXXXX"
                    TO MESS
                 INSPECT MESS REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXX"
                    BY DUE-PATH
                 PERFORM SEND-MESS.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

      *----------------------------------------------------------------
       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
                 MOVE SV-LOCAL-PRINT-FG TO LOCAL-PRINT-FG
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

*NEW*      IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN |MJD001
              DESTROY LNAGED-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
