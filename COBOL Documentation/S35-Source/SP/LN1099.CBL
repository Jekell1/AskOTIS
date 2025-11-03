       IDENTIFICATION DIVISION.
       PROGRAM-ID.       LN1099.
       DATE-WRITTEN.   01/11/01.
      **********************************************************************
      *                      (SP)
      *       DESCRIPTION:  LOAN 1099-C EXTRACTION, WORLD PR# 250
      *       1099-C IS CANCELLATION OF DEBT
      *
      *       HOW WORLD RUNS THIS PER JIM, 12/13/07, FOR EX 12/07:
      *           GROUP R1 OR R2,
      *           ALL CLASSES
      *           INCLUDE JUDGEMENTS
      *           SKIP BANKRUPTS
      *           AU DATE RANGE, CUR YR, 01/01/07-12/31/07
      *           PL CHGOFF DATE RANGE, 3-5 YRS AGO, 01/01/02-12/31/04
      *           PL LAST PAY DATE RANGE, 3 YRS+, 01/01/94-12/31/04
      *           YEAR END DATE  12/31/07
      *           PRINT DETAIL Y
      *           PAGE ON BRANCH N
      *
      *
      *    NOTE:  I BROUGHT THIS OVER FROM A10 - WRITTEN CUSTOM
      *           FOR WORLD (ALTHOUGH THEY NEVER USED!!!!) BUT
      *           I SPENT QUITE A BIT OF TIME ON IT & I DIDN'T
      *           WANT TO LOSE IN CASE SOMEONE ELSE REALLY NEEDS
      *           TO DO THESE AUTOMATICALLY.  THIS DOES NOT
      *           COMPILE IN A15, WOULD NEED TO ADD THE NEW
      *           RECORD TYPE TO THE ICFILE & FIX THE MEMO
      *           MEMO ROUTINES FOR A15.  ALSO, BY DEFINITION
      *           I THINK THIS FORM WAS TO BE SENT TO LOANS WHICH
      *           HAD NO ACTIVITY OR COLLECTION? ACTIVITY AFTER
      *           A CERTAIN DATE....JIM & WORLD CAME UP WITH A
      *           BUNCH OF DATE CHECKS ON LAST PMT & ALL THE
      *           MISC DATES (LEGAL, JUDGEMENT, ETC) WHICH THEY
      *           THOUGHT WOULD GRAB THE APPROPRIATE ACCOUNTS -
      *           THIS MAY NOT BE CORRECT FOR SOMEONE ELSE.
      *           (I PUT SP01IC.A10 & IRPREP.A10 IN A15 SV DIRECTORIES)
      *                                                   BARB
      *
      * REV:
      * 010118 BLV ADD LOAN MEMO TO DOCUMENT 1099, WORLD PR# 250
      * 010122 BLV IF WORLD ACCESS ICFILE WITH GROUP SUFFIX SO
      *            THEY CAN SET UP IRS ID INFO BY STATE & RUN TO
      *            SEPARATE OUTPUT FILES, WORLD PR# 250
      * 010124 BLV FIX LT-ALL-PATH NOT BEING SET.  SKIP LOAN IF LN-1099-FG
      *            ALREADY SET TO "Y" OR "N"
      * 050728 BLF BROUGHT PROGRAM AND RELATED CHANGES OVER FROM A10 WHERE
      *            THIS WAS DONE FOR WORLD BUT NEVER USED. WORLD PR# 411
      * 051007 BLF CHANGED TO USE LN-LAST-PYPA-DATE, NOT LN-DATE-PAID-LAST
      * 051108 BLF FINAL? CHANGES FOR WORLD PR# 411, PER JIM:  WHEN
      *            CHECKING TO INCLUDE IF CURBAL LESS UNEARNED > $600,
      *            JUST USE LN-CURBAL - UNEARNED (DON'T ADD INTBAL, LCBAL,
      *            OTBAL TO CURBAL).  IF LN-LAST-PYPA-DATE = 0, USE
      *            LOANDATE IN COMPARISONS TO SEE WHETHER TO INCLUDE.
      * 051109 BLF PER JIM, UNEARNED IS NOT GETTING SUBTRACT FROM CURBAL;
      *            SO READ LIBLP/LPEARN INTO PROGRAM & STARRED OFF THE
      *            LINE THAT SKIPS P&L'S WHEN ADDING TO UNEARNED
      * 060202 BLF DON'T FREEZE ACCOUNT WHEN PRODUCING 1099-C, WORLD
      * 071127 BLF PUT IN TESTS TO SKIP INVALID SSNO'S, WORLD PR# 561
      * 071128 BLF ADD OPTION TO GENERATE TIN MATCHING FILE TO SEND TO
      *            IRS, WORLD PR# 561
      * 071213 BLF WHEN GENERATING TIN MATCHING FILE, EDIT OUT SPECIAL
      *            CHARACTERS FROM NAME, ALLOW ONLY ALPHA, NUMERIC,
      *            SPACE, DASH OR APERSAND
      * 071218 BLF EDIT OUT SPECIAL CHARACTERS FROM NAME IN ICFILE GOING
      *            TO IRS, WORLD PR# 561
      * 080114 BLF WHEN CREATING TIN FILE, INITIALIZE WORK-FNAME &
      *            WORK-LNAME - NOTICED WHEN WORKING OF WORLD PR# 564 AND
      *            FIXED.  COULD HAVE PUT A MESSED UP NAME IN TIN FILE ONLY
      *            IF A NAME WITH 1 BAD CHARACTER WAS IMMEDIATE NEXT IN FILE
      *            AFTER A NAME THAT WAS 16 LONG, OR NAME WITH 2 BAD CHARS
      *            FOLLOWED A NAME THAT WAS 15 LONG, ETC.    PROBABLY
      *            FAIRLY UNLIKELY SINCE NOT TOO MANY NAMES ARE THAT LONG
      *            AND NOT THAT MANY HAVE BAD CHARACTERS.
      * 080130 BLF FIX CURBAL-LESS-UNEARNED TO BE SIGNED, UNSIGNED WORKER
      *            WAS ALLOWING MESSED UP ACCOUNTS TO GET 1099'S
      * 081118 CS ADD TEST GP-INCL-OTH-IN-EARNED = 'R'. IF 'R' EXCLUDE
      *            OTHER(ANTICERN(6) AND MAINTFEE(ANTICERN(4) FROM
      *            ANTICIPATED     REGACC PR#210
      * 091015 CS  AFTER CREATING THE TIN FILE, IF GP-I-AM-WORLD & LN-CONVERCD
      *            NOT = SPACES GO TO NEXT LOAN PR#658 WORLD
      * 100208 BLM FIX DATE BEING WRITTEN TO LM.  BUG CAUSED CENTURY TO NOT BE
      *            CORRECT, SO DATE WOULD DISPLAY CORRECTLY, BUT COULD BE IN
      *            WRONG ORDER.  FOR EXAMPLE, WORLD RAN 1099'S ON 1/27/10,
      *            WHICH SHOULD BE 99999999 - 20100127 = 79899872.  BUT
      *            CENTURY MESSED UP, GOT REALLY OLD DATES LIKE:
      *            1/27/1510, 99999999 - 15100127 = 84899872 OR REALLY FUTURE
      *            1/27/8610, 99999999 - 86100127 = 13899872.  FUTURE ONES
      *            DON'T REALLY MATTER, REALLY OLD ONES DISPLAY AS LAST MEMO
      *            ON ACCOUNT INSTEAD OF FIRST.
      * 131220 BLM TIN MATCHING FILE WAS GETTING REJECTED BY IRS (TINEXT).
      *            ADD CARRIAGE RETURN TO END OF TIN MATCHING FILE & MAKE
      *            FILE VARIABLE LENGTH BY ADDING "OUTPUT" TO ASSIGN
      *            CLAUSE, WORLD PR# 952B
      * BLM 141216 CREATE WORK FILE KEYED BY BRNO, SSNO & WRITE EACH RECORD
      *            AS IT IS EXTRACTED.  IF IT'S ALREADY IN FILE, DON'T
      *            INCLUDE IN REPORT OR IN TIN MATCH FILE.  IRS REJECTES DUPS.
      *            DUPS WILL STILL BE WRITTEN TO THE 1099 FILE. WORLD PR# 1040
      * BAH 160201 SPLIT OUT LTFILE, PD0003
      * BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      *
      * KEC 2016.0222 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      * BAH 2016.1230 ADDED IC-B-PAYAMT(13) - IB-B-PAYAMT(16), PR#5214
      * BLM 170406 EXPANDED WORK-FNAME, WORK-LNAME; NAMES OF 15 OR MORE
      *            CAUSED INVALID TRAILING SPACE IN IRS TIN FILE, 
      *            WORLD PR# 1239
      *  20171003 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181115 GLOBAL FMFILE
      * BAH 181214 GLOBAL LNFILE
      * BAH 190312 MOVED FM-BRNO2 INTO LPWSFM-EDIT-FM-INITKEY TO REMOVE THE
      *            REDEFINE IN THE FM-REC
      * BAH 20190614 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      *  20190715 BAH MAKE LN-ACCTNO LOOK LIKE A15 IN THE TIN FILE, MOVE
      *               TO A 9(6) FIRST SO THERE ARE NO LEADING ZEROS, QA0050
      *  20190716 CHANGED BW-NAME TO PAYEE-NAME, A WORKING STORAGE
      *           FIELD SO IT CONTINUES TO MATCH A15. WE CHANGED NAME FIELDS
      *           FROM 16 TO 35, QA0051
      * BAH 20190930 REMOVED ACCESS-CALL LNFILE
      * BAH 20220111 ADDED IC-B-EVENT-CODE "F", CHANGED IC-B-PAYAMT FROM OCCURS
      *              16 TO 18, ZERO OUT NEW AMT FIELDS. #1537
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * BAH 20220815 HARD CODED START
      * BAH 20240916 F7 IN ENTER-PARAMETERS WAS GOING TO ENDIT, OUTSIDE OF THE
      *              SECTION, CAUSING FALL THRU LOGIC. COULDNT EXIT! S35Q-128
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBSP/SPFSIC.CPY".
           SELECT WK-FILE ASSIGN WK-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY WK-KEY
                  FILE STATUS FILE-STAT.
           SELECT TIN-FILE ASSIGN OUTPUT TIN-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  ACCESS MODE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.
       SELECT WK2-FILE ASSIGN MKTEMP-BUF
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBSP/SPFDIC.CPY".
      ******************************************************
      *   ASCII FILE FOR PRINT SHOP TO DO 1099 SUBSTITUTES
      *   LIKE DMCSEX ASCII FILE, BUT NOT IN ANY ORDER &
      *   WITH SSNO TACKED ON END OF FILE; AMT IS 1099
      *   AMT INSTEAD OF AVAILABLE CREDIT
      ******************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-LNAME     PIC X(16).
           03  WK-FNAME     PIC X(16).
           03  WK-ADR       PIC X(48).
           03  WK-CITY      PIC X(15).
           03  WK-STATE     PIC XX.
           03  WK-ZIP       PIC X(10).
           03  WK-OWNBR     PIC 9999.
           03  WK-REPCD     PIC X(4).
           03  WK-SRCD      PIC XX.
           03  WK-ACCTNO    PIC 9(6).
           03  WK-1099-AMT  PIC 9(7).99.
           03  WK-SSNO      PIC 9(9).
           03  WK-CR        PIC X.
           03  WK-LF        PIC X.

       FD  TIN-FILE
           RECORD IS VARYING IN SIZE FROM 1 TO 63 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  TIN-REC.
           03  TIN-BYTE                PIC X OCCURS 63 TIMES.

       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRNO  PIC 9(4).
               05  WK2-SSNO  PIC 9(9).

       COPY "LIBGB/GBFDPR.CPY".
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/LN1099 S35 09/16/24-11:41:18 >".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBSP/SPWSIC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".

       01  WK-PATH.
           03  WK-PATH-BASE          PIC X(09).
           03  FILLER                PIC X(01)   VALUE "/".
           03  WK-PATH-MACHINE       PIC X(02).
           03  FILLER                PIC X(04)   VALUE "/ED/".
           03  WK-PATH-EXTRACT-NAME.
               05  FILLER              PIC X(5)   VALUE "1099.".
               05  WK-PATH-DATE-MMDDYY PIC 9(6).
               05  FILLER              PIC X      VALUE ".".
               05  WK-PATH-BRNO        PIC 9999.
               05  WK-PATH-GROUP   REDEFINES WK-PATH-BRNO PIC X(4).

       01  TIN-PATH.
           03  TIN-PATH-BASE            PIC X(09).
           03  FILLER                   PIC X(01)   VALUE "/".
           03  TIN-PATH-MACHINE         PIC X(02).
           03  FILLER                   PIC X(04)   VALUE "/ED/".
           03  TIN-PATH-NAME.
               05  FILLER               PIC X(9)   VALUE "1099CTIN.".
               05  TIN-PATH-DATE-MMDDYY PIC 9(6).
               05  FILLER               PIC X      VALUE ".".
               05  TIN-PATH-BRNO        PIC 9999.
               05  TIN-PATH-GROUP   REDEFINES TIN-PATH-BRNO PIC X(4).

       01  IC-KEY               PIC 9(6)     VALUE 0.
       01  WK-KEY               PIC 9(6)     VALUE 0.
       01  WK-IFN               PIC XX       VALUE "WK".
       01  WK2-IFN              PIC XXX      VALUE "WK2".
       COPY "LIBLP/LP01LP.CPY".

      ******************************************************************
      *
      *    L I N E    W O R K E R S
      *
      ******************************************************************
       01  LN-FEED          PIC 99    VALUE 0.
       01  LN-COUNT         PIC 999   VALUE 99.

       01  PAGE-NUMBER          PIC 9(4)  VALUE 0.

       01  PAYEE-NAME.
           03  PAYEE-LNAME      PIC X(16).
           03  PAYEE-FNAME      PIC X(16).

       01  CARRAGE-RET          PIC X VALUE X"0D".
       01  SUB                  PIC 9(6) COMP-3       VALUE 0.
       01  SUB2                 PIC 9(7) COMP-3       VALUE 0.
       01  MATCH                PIC X        VALUE "N".
       01  HOLD-FM1-KEY         PIC X(28)     VALUE SPACES.
       01  ZIP-WORK             PIC X(10).
       01  X REDEFINES ZIP-WORK.
           03  FILLER           PIC X(6).
           03  PART-2           PIC X(4).
       01  X1 REDEFINES ZIP-WORK.
           03  FILLER           PIC X(5).
           03  PART-2-A         PIC X(4).
           03  ZIP-JUNK         PIC X.
       01  X3 REDEFINES ZIP-WORK.
           03  PART-3-ZIP       PIC X(5).
           03  PART-3-DASH      PIC X(1).
           03  PART-3-EXT       PIC X(4).

       01  WORK-FIELD           PIC X(40).
       01  X3 REDEFINES WORK-FIELD.
           03  WORK-CHAR        PIC X OCCURS 40 TIMES.

       01  TEST-CHAR            PIC X.
           88 VALID-CHAR        VALUE " " "-" "&".

       01  WORK-NAME            PIC X(16).
       01  FILLER REDEFINES WORK-NAME.
           03  NAME-CHAR        PIC X OCCURS 16 TIMES.

       01  WORK-FNAME           PIC X(18).
       01  FILLER REDEFINES WORK-FNAME.
           03  FNAME-CHAR       PIC X OCCURS 18 TIMES.

       01  WORK-LNAME           PIC X(18).
       01  FILLER REDEFINES WORK-LNAME.
           03  LNAME-CHAR       PIC X OCCURS 18 TIMES.

       01  NAME-IN              PIC X(32).
       01  FILLER REDEFINES NAME-IN.
           03  NAME-IN-CHAR     PIC X OCCURS 32 TIMES.

       01  NAME-OUT             PIC X(32).
       01  FILLER REDEFINES NAME-OUT.
           03  NAME-OUT-CHAR    PIC X OCCURS 32 TIMES.

       01  WORK-ACCTNO          PIC 9(6).
       01  BAD-NAME-FG          PIC X    VALUE "N".
       01  BAD-NAME-COUNTER     PIC 9(8) VALUE 0.
       01  DUP-COUNTER          PIC 9(8) VALUE 0.
       01  DUP-FG               PIC X    VALUE "N".

       01  SOCSEC-KEY      PIC 9(9).
       01  FILLER REDEFINES SOCSEC-KEY.
           03  SS-1        PIC 9(3).
           03  SS-2        PIC 99.
           03  SS-3        PIC 9(4).
       01  FILLER REDEFINES SOCSEC-KEY.
           03  SS-FIRST4   PIC 9(4).
           03  SS-LAST5    PIC 9(5).
       01  FILLER REDEFINES SOCSEC-KEY.
           03  SS-CHAR1    PIC 9.
           03  FILLER      PIC 9(8).

       01  PAYER-ACCT.
           03  PAYER-BRNO       PIC 9999.
           03  PAYER-DASH1      PIC X.
           03  PAYER-CLASS      PIC 99.
           03  PAYER-DASH2      PIC X.
           03  PAYER-NUMBER     PIC 9(6).
           03  PAYER-DASH3      PIC X.
           03  PAYER-SEQNO      PIC 9(2).

       01  WORK-FIELD-2         PIC X(40).
       01  X4 REDEFINES WORK-FIELD-2.
           03  WORK-CHAR-2      PIC X OCCURS 40 TIMES.

       01  NUM-EDIT             PIC ZZZZZ9.99.
       01  CURBAL-LESS-UNEARNED PIC S9(9)V99 VALUE 0.

       01  BR-TOTAL-AMT         PIC 9(9)V99 VALUE 0.
       01  BR-TOTAL-ACCOUNTS    PIC 9(9)V99 VALUE 0.
       01  GRAND-TOTAL-AMT      PIC 9(9)V99 VALUE 0.
       01  GRAND-TOTAL-ACCOUNTS PIC 9(9)V99 VALUE 0.

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH           PIC 9(4).
           03  END-BRANCH           PIC 9(4).
           03  BEG-CLASS            PIC 99.
           03  END-CLASS            PIC 99.
           03  JUDGEMENT-FG         PIC X.
           03  BANKRUPT-FG          PIC X.
           03  AU-BEG-LAST-PAY-DATE PIC 9(8).
           03  AU-END-LAST-PAY-DATE PIC 9(8).
           03  BEG-PL-DATE          PIC 9(8).
           03  END-PL-DATE          PIC 9(8).
           03  PL-BEG-LAST-PAY-DATE PIC 9(8).
           03  PL-END-LAST-PAY-DATE PIC 9(8).
           03  YEAREND-DATE         PIC 9(8).
           03  FORMNO               PIC 999.
           03  REPORT-ONLY-FG       PIC X.
           03  PRINT-DETAIL-FG      PIC X.
           03  BRANCH-PAGE-FG       PIC X.
           03  GROUP-FLAG           PIC X.
           03  ENT-GROUP            PIC X(4).
           03  CREATE-TIN-FG        PIC X.

       01  VERSION-CONTROL          PIC X VALUE "A".

       01  MESS-LIST                PIC X(5) VALUE "MESS.".

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
           03  FILLER           PIC X(35)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(33)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(31)    VALUE " ".
           03  FILLER           PIC X(29)    VALUE
               "LOAN 1099-C EXTRACTION REPORT".
           03  FILLER           PIC X(35)    VALUE " ".
           03  FILLER           PIC X(17)    VALUE
               "* = INVALID NAME ".

       01  HEAD3.
           03  FILLER      PIC X(54)    VALUE
               "BRNO  ACCOUNT NO.   BORROWER".
           03  FILLER      PIC X(40)    VALUE
               "TRW LOANDATE  PLDATE  LAST PMT    LEGAL ".
           03  FILLER      PIC X(27)    VALUE
               "JUDGMENT DEBTAID    BALANCE".
           03  FILLER      PIC X(11)    VALUE
               "  REPORTED ".

      ******************************************************************
      *
      *    D E T A I L   L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO      PIC 9999.
           03  FILLER      PIC XX.
           03  D-LOAN-NUM  PIC X(13).
           03  FILLER      PIC X.
           03  D-SSNO      PIC 999/99/9999.
           03  D-DUP       PIC X.
           03  FILLER      PIC X.
           03  D-NAME      PIC X(21).
           03  D-STAR      PIC X.
           03  D-TRW       PIC X(2).
           03  FILLER      PIC X.
           03  D-LOANDATE  PIC 99/99/99 BLANK WHEN ZERO.
           03  FILLER      PIC X.
           03  D-PLDATE    PIC 99/99/99.
           03  FILLER      PIC X.
           03  D-PAYDATE   PIC 99/99/99 BLANK WHEN ZERO.
           03  FILLER      PIC X.
           03  D-LEGALDATE PIC 99/99/99 BLANK WHEN ZERO.
           03  FILLER      PIC X.
           03  D-JDDATE    PIC 99/99/99 BLANK WHEN ZERO.
           03  FILLER      PIC X.
           03  D-DEBTDATE  PIC 99/99/99 BLANK WHEN ZERO.
           03  FILLER      PIC X.
           03  D-CURBAL    PIC ZZZZZZ.99.
           03  FILLER      PIC X.
           03  D-REPORTED  PIC ZZZZZZ.99-.
       01  TOTAL-LINE1 REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(32).
           03  T-PROMPT1   PIC X(15).
           03  T-BRNO      PIC ZZZZ.
           03  FILLER      PIC X(8).
           03  T-PROMPT2   PIC X(20).
           03  T-ACCOUNTS  PIC ZZZZZ9.
           03  FILLER      PIC X(8).
           03  T-PROMPT3   PIC X(15).
           03  T-AMT       PIC ZZZZZ,ZZZ.99.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(22).
           03  RANGE-SELECTION  PIC X(110).
           03  RANGE-DATE REDEFINES RANGE-SELECTION PIC 9999/99/99.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/LN1099_DEF.CPY".
       COPY "LIBSP/LN1099_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/LN1099_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               IC-FILE PR-FILE WK-FILE TIN-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           CLOSE
               IC-FILE PR-FILE WK-FILE TIN-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *******************************************
       MAIN-MODULE SECTION.
      *******************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LN1099" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE 0 TO IC-KEY.
           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.
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
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LN-PATH-OVERRIDE
                              BW-PATH-OVERRIDE
                              FM-PATH-OVERRIDE
                              LM-PATH-OVERRIDE
                              LTI-PATH-OVERRIDE
                              LTP-PATH-OVERRIDE
                              GB-PATH-OVERRIDE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROCESSING BRANCH &&&&..." TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
              MOVE MESS TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO END-OF-GROUP.

           PERFORM OPEN-LN3-FILE.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.
           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           IF LN-1099-C-FG = "Y"
              GO TO NEXT-LN.

      * VOIDED LOAN, RESCISSION
           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              GO TO NEXT-LN.

           IF LN-PLDATE = 0
              GO TO NEXT-LN.

           IF LN-POFFDATE NOT = 0
              GO TO NEXT-LN.

           IF JUDGEMENT-FG = "O"
              IF LN-JDDATE = 0
                 GO TO NEXT-LN.

           IF JUDGEMENT-FG = "S"
              IF LN-JDDATE NOT = 0
                 GO TO NEXT-LN.

           IF BANKRUPT-FG = "O"
              IF LN-BNKRPTDATE = 0
                 GO TO NEXT-LN.

           IF BANKRUPT-FG = "S"
              IF LN-BNKRPTDATE NOT = 0
                 GO TO NEXT-LN.

      *    COMPUTE CURBAL = LN-CURBAL + LN-INTBAL
      *                   + LN-OTHBAL + LN-LCBAL.
           PERFORM COMPUTE-EARNINGS.
           COMPUTE CURBAL-LESS-UNEARNED = LN-CURBAL - EARN-TOT-UNEARN.
           IF CURBAL-LESS-UNEARNED < 600.00
              GO TO NEXT-LN.

           IF LN-SSNO(1) = 0
              GO TO NEXT-LN.

           MOVE LN-SSNO(1) TO SOCSEC-KEY.
           IF SS-1 = 0 OR SS-2 = 0 OR SS-3 = 0
              GO TO NEXT-LN.

           IF SS-FIRST4 = 0 GO TO NEXT-LN.
           IF SS-1 = 666    GO TO NEXT-LN.
           IF SS-CHAR1 = 8  GO TO NEXT-LN.
           IF SS-CHAR1 = 9  GO TO NEXT-LN.

           IF (SS-1 > 728 AND SS-1 < 750)
              GO TO NEXT-LN.

           IF (SS-1 > 771 AND SS-1 < 800)
              GO TO NEXT-LN.

           MOVE YEAREND-DATE TO POFF-PAYDATE.
           MOVE "PO"         TO POFF-LPTRCD.
           MOVE "M"          TO POFF-MAKERCD.
           MOVE "N"          TO POFF-DEF-STOP.
           PERFORM PAYOFF-LOAN-ROUTINE.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           PERFORM LOAN-CALCULATIONS.

           IF LN-TRW-STATUS = "68" OR LN-TRW-SPECIAL-CODE = "AU"
              GO TO TEST-SETTLED-ACCTS.

           IF LN-PLDATE < BEG-PL-DATE OR LN-PLDATE > END-PL-DATE
              GO TO NEXT-LN.

           IF LN-LAST-PYPA-DATE = 0
              MOVE LN-LOANDATE TO SYS-DATE
           ELSE
              MOVE LN-LAST-PYPA-DATE TO SYS-DATE.
           IF SYS-DATE < PL-BEG-LAST-PAY-DATE
              GO TO NEXT-LN.

           IF LN-LAST-PYPA-DATE = 0
              MOVE LN-LOANDATE TO SYS-DATE
           ELSE
              MOVE LN-LAST-PYPA-DATE TO SYS-DATE.
           IF SYS-DATE > PL-END-LAST-PAY-DATE
              GO TO NEXT-LN.

           GO TO ACCOUNT-REQUIRES-1099.

       TEST-SETTLED-ACCTS.

           IF LN-LAST-PYPA-DATE = 0
              MOVE LN-LOANDATE TO SYS-DATE
           ELSE
              MOVE LN-LAST-PYPA-DATE TO SYS-DATE.
           IF SYS-DATE < AU-BEG-LAST-PAY-DATE
              GO TO NEXT-LN.

           IF LN-LAST-PYPA-DATE = 0
              MOVE LN-LOANDATE TO SYS-DATE
           ELSE
              MOVE LN-LAST-PYPA-DATE TO SYS-DATE.
           IF SYS-DATE > AU-END-LAST-PAY-DATE
              GO TO NEXT-LN.

       ACCOUNT-REQUIRES-1099.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO BW-ZIP
              MOVE "NOT ON FILE!!" TO BW-LNAME.

           PERFORM CREATE-TIN-REC.

           IF LN-CONVERCD NOT = " "
              GO TO NEXT-LN.

           PERFORM CREATE-DETAIL-LINE.

           IF REPORT-ONLY-FG = "Y"
              GO TO NEXT-LN.

           IF FORMNO NOT = 0
              PERFORM CREATE-FM-REC.

           PERFORM CREATE-IC-REC.

           MOVE "Y"          TO LN-1099-C-FG.
           MOVE YEAREND-DATE TO LN-FILEDATE.

           PERFORM REWRITE-LN3-FILE.

           PERFORM CREATE-WORK-FILE.

           PERFORM CREATE-MEMO.

           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-FM1-FILE.
           IF CREATE-TIN-FG = "Y"
              PERFORM CLOSE-WK2-FILE
              PERFORM OPEN-WK2-FILE-OUTPUT
              PERFORM CLOSE-WK2-FILE
              PERFORM OPEN-WK2-FILE.

           PERFORM PRINT-BRANCH-TOTALS.
           IF BRANCH-PAGE-FG = "Y"
              MOVE 99 TO LN-COUNT.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           PERFORM PRINT-GRAND-TOTALS.
           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      *******************************************
       CREATE-DETAIL-LINE SECTION.
      *******************************************
           MOVE LN-OWNBR TO D-BRNO.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE LN-SSNO(1) TO D-SSNO.
           INSPECT D-SSNO REPLACING ALL "/" BY "-".
           IF DUP-FG = "Y"
              MOVE "D" TO D-DUP.
           MOVE SPACES TO D-NAME.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                  INTO D-NAME.
           IF BAD-NAME-FG = "Y"
              MOVE "*" TO D-STAR.
           IF LN-TRW-STATUS = "68"
              MOVE "68" TO D-TRW.
           IF LN-TRW-SPECIAL-CODE = "AU"
              MOVE "AU" TO D-TRW.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LOANDATE.
           MOVE LN-PLDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-PLDATE.
           MOVE LN-LAST-PYPA-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-PAYDATE.
           MOVE LN-LEGALDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LEGALDATE.
           MOVE LN-JDDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-JDDATE.
           MOVE LN-DEBTAIDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DEBTDATE.
           MOVE LN-CURBAL TO D-CURBAL.
           MOVE CURBAL-LESS-UNEARNED TO D-REPORTED.
           ADD CURBAL-LESS-UNEARNED TO BR-TOTAL-AMT GRAND-TOTAL-AMT.
           ADD 1 TO BR-TOTAL-ACCOUNTS GRAND-TOTAL-ACCOUNTS.
           MOVE 1 TO LN-FEED.
           IF PRINT-DETAIL-FG = "Y"
              PERFORM WRITE-DETAIL-LINE
           ELSE
           IF (PRINT-DETAIL-FG = "B" AND BAD-NAME-FG = "Y")
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.

      ********************************************
       PRINT-BRANCH-TOTALS SECTION.
      ********************************************
           MOVE "TOTAL BRANCH #" TO T-PROMPT1.
           MOVE BR-NO TO T-BRNO.
           MOVE "NO. OF ACCOUNTS" TO T-PROMPT2.
           MOVE BR-TOTAL-ACCOUNTS TO T-ACCOUNTS.
           MOVE "AMT TO REPORT" TO T-PROMPT3.
           MOVE BR-TOTAL-AMT TO T-AMT.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           IF PRINT-DETAIL-FG = "Y"
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE 0 TO BR-TOTAL-AMT BR-TOTAL-ACCOUNTS.

      ********************************************
       PRINT-GRAND-TOTALS SECTION.
      ********************************************
           MOVE "GRAND TOTALS" TO T-PROMPT1.
           MOVE "NO. OF ACCOUNTS" TO T-PROMPT2.
           MOVE GRAND-TOTAL-ACCOUNTS TO T-ACCOUNTS.
           MOVE "AMT TO REPORT" TO T-PROMPT3.
           MOVE GRAND-TOTAL-AMT TO T-AMT.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "NO OF BAD NAMES" TO T-PROMPT2.
           MOVE BAD-NAME-COUNTER TO T-ACCOUNTS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "NO OF SKIPPED DUPS" TO T-PROMPT2.
           MOVE DUP-COUNTER TO T-ACCOUNTS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ****************************************
       INITIALIZATION SECTION.
      ****************************************
           MOVE LN1099-QUEUE-POS TO Q-POS.
           MOVE LN1099-COPIES-POS TO C-POS.
           MOVE LN1099-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF CREATE-TIN-FG = "Y"
              PERFORM SET-TIN-PATH
              IF EXT-EOBUF-ERRCD NOT = " "
                 GO TO ENDIT.

           IF REPORT-ONLY-FG = "Y"
              GO TO SKIP-OPEN-WORK-FILES.

           PERFORM SET-WORK-PATH.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO ENDIT.

           PERFORM OPEN-IC-FILE-OUTPUT.
           PERFORM CLOSE-IC-FILE.
           PERFORM OPEN-IC-FILE.

       SKIP-OPEN-WORK-FILES.

           IF EXT-ROUTE-BUF = "00"
              MOVE "1099-C REPORT/EXTRACTION IN PROGRESS." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-PR-FILE.

           IF CREATE-TIN-FG = "Y"
              MOVE MKTEMP-DEFAULT TO MKTEMP-BUF
              PERFORM MKTEMP-CALL
              IF STAT NOT = "00"
                 MOVE "CANNOT MAKE WORK FILE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENDIT
              END-IF
              PERFORM OPEN-WK2-FILE-OUTPUT
              PERFORM CLOSE-WK2-FILE
              PERFORM OPEN-WK2-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       SET-WORK-PATH SECTION.
      ******************************************************************

           MOVE EXT-FILPATH-BASE      TO WK-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE   TO WK-PATH-MACHINE.

           ACCEPT DATE-YYYYMMDD FROM CENTURY-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO WK-PATH-DATE-MMDDYY.

           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP TO WK-PATH-GROUP
           ELSE
              MOVE BEG-BRANCH TO WK-PATH-BRNO.

           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              PERFORM OPEN-WK-FILE-OUTPUT
              MOVE 0 TO WK-KEY
              GO TO SET-WORK-PATH-EXIT.

       SET-WK-PATH-ASK.
           IF REPORT-ONLY-FG = "Y"
              GO TO OPEN-WK-OUTPUT.
           PERFORM ALARM.

           MOVE "PRINTSHOP FILE ALREADY EXISTS, CONTINUE (Y/N):"
                                               TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "R RA010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO SET-WK-PATH-ASK.
           IF VDUBUF = "N"
              MOVE "E" TO EXT-EOBUF-ERRCD
              GO TO SET-WORK-PATH-EXIT.
           IF VDUBUF NOT = "Y"
              GO TO SET-WK-PATH-ASK.

       OPEN-WK-OUTPUT.
           PERFORM OPEN-WK-FILE-OUTPUT.

           MOVE 0 TO WK-KEY.
           MOVE SPACES TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SET-WORK-PATH-EXIT.
           EXIT.

      ******************************************************************
       SET-TIN-PATH SECTION.
      ******************************************************************

           MOVE EXT-FILPATH-BASE      TO TIN-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE   TO TIN-PATH-MACHINE.

           ACCEPT DATE-YYYYMMDD FROM CENTURY-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO TIN-PATH-DATE-MMDDYY.

           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP TO TIN-PATH-GROUP
           ELSE
              MOVE BEG-BRANCH TO TIN-PATH-BRNO.

           MOVE TIN-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              PERFORM OPEN-TIN-FILE-OUTPUT
              GO TO SET-TIN-PATH-EXIT.

       SET-TIN-PATH-ASK.
           PERFORM ALARM.
           MOVE "TIN MATCH FILE ALREADY EXISTS, CONTINUE (Y/N):"
                                              TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "R RA010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO SET-TIN-PATH-ASK.
           IF VDUBUF = "N"
              MOVE "E" TO EXT-EOBUF-ERRCD
              GO TO SET-TIN-PATH-EXIT.
           IF VDUBUF NOT = "Y"
              GO TO SET-TIN-PATH-ASK.

           PERFORM OPEN-TIN-FILE-OUTPUT.

           MOVE SPACES TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SET-TIN-PATH-EXIT.
           EXIT.

      *****************************************
       ENTER-PARAMETERS SECTION.
      *****************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO PAGE-ON-BRANCH-YN.

       BR-01.
           MOVE "E RA000BEGBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BEGBR." TO VDU
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
              MOVE "S  A000ENDBR." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CL-01.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BEGBR." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "ENNN040ENDBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO CL-01.
       ALL-BR.
           MOVE "SNNN040BEGBR." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040ENDBR." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       CL-01.
           MOVE "ENNN020BEGCL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-02.
           MOVE "ENNN020ENDCL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO JUDGEMENT-YN.
       ALL-CL.
           MOVE "SNNN020BEGCL." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020ENDCL." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       JUDGEMENT-YN.
           MOVE "E  A010JUDGEMENT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO JUDGEMENT-YN.
           MOVE VDUBUF TO JUDGEMENT-FG.
           IF JUDGEMENT-FG NOT = "I"
              IF JUDGEMENT-FG NOT = "S"
                 IF JUDGEMENT-FG NOT = "O"
                 MOVE "PLEASE ENTER I, S, OR O!" TO MESS
                 PERFORM SEND-MESS
                 GO TO JUDGEMENT-YN.

       BANKRUPT-YN.
           MOVE "E  A010BANKRUPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO JUDGEMENT-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO BANKRUPT-YN.
           MOVE VDUBUF TO BANKRUPT-FG.
           IF BANKRUPT-FG NOT = "I"
              IF BANKRUPT-FG NOT = "S"
                 IF BANKRUPT-FG NOT = "O"
                 MOVE "PLEASE ENTER I, S, OR O!" TO MESS
                 PERFORM SEND-MESS
                 GO TO BANKRUPT-YN.

       AU-LAST-PAY-01.
           MOVE "ERNM060AU_LASTPAY1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BANKRUPT-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO AU-LAST-PAY-01.
           MOVE VDU-DATE-YYYYMMDD TO AU-BEG-LAST-PAY-DATE.

       AU-LAST-PAY-02.
           MOVE "ERNM060AU_LASTPAY2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO AU-LAST-PAY-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO AU-LAST-PAY-02.
           MOVE VDU-DATE-YYYYMMDD TO AU-END-LAST-PAY-DATE.

       PLDATE-01.
           MOVE "ERNM060PLDATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO AU-LAST-PAY-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PLDATE-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-PL-DATE.
       PLDATE-02.
           MOVE "ERNM060PLDATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PLDATE-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PLDATE-02.
           MOVE VDU-DATE-YYYYMMDD TO END-PL-DATE.
       PL-LAST-PAY-01.
           MOVE "ERNM060PL_LASTPAY1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BANKRUPT-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO PL-LAST-PAY-01.
           MOVE VDU-DATE-YYYYMMDD TO PL-BEG-LAST-PAY-DATE.

       PL-LAST-PAY-02.
           MOVE "ERNM060PL_LASTPAY2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PL-LAST-PAY-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO PL-LAST-PAY-02.
           MOVE VDU-DATE-YYYYMMDD TO PL-END-LAST-PAY-DATE.

       YEAR-END-DATE.
           MOVE "ERNM060YRENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PLDATE-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO YEAR-END-DATE.
           MOVE VDU-DATE-YYYYMMDD TO YEAREND-DATE.
       FMNO.
           MOVE "ENNN030FORMNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO YEAR-END-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO FMNO.
           MOVE VDUNUM0 TO FORMNO.
           IF FORMNO = 0
              GO TO REPORT-ONLY-YN.

           PERFORM OPEN-FD1-FILE.
           MOVE FORMNO TO FD-FMNO.
           PERFORM READ-FD1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID FORM NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO FMNO.
           PERFORM CLOSE-FD1-FILE.

       REPORT-ONLY-YN.
           MOVE "E  A010REPORTONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO FMNO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO REPORT-ONLY-YN.
           MOVE VDUBUF TO REPORT-ONLY-FG.
           IF REPORT-ONLY-FG NOT = "Y"
              IF REPORT-ONLY-FG NOT = "N"
                 MOVE "PLEASE ENTER Y OR N!" TO MESS
                 PERFORM SEND-MESS
                 GO TO REPORT-ONLY-YN.
       PRINT-DETAIL-YN.
           MOVE "E  A010DETAILYN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO REPORT-ONLY-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO PRINT-DETAIL-YN.
           MOVE VDUBUF TO PRINT-DETAIL-FG.
           IF PRINT-DETAIL-FG NOT = "Y"
              IF PRINT-DETAIL-FG NOT = "N"
                 IF PRINT-DETAIL-FG NOT = "B"
                    MOVE "PLEASE ENTER Y OR N!" TO MESS
                    PERFORM SEND-MESS
                    GO TO PRINT-DETAIL-YN.

       PAGE-ON-BRANCH-YN.
           MOVE "E  A010PAGEFG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PRINT-DETAIL-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO PAGE-ON-BRANCH-YN.
           MOVE VDUBUF TO BRANCH-PAGE-FG.
           IF BRANCH-PAGE-FG NOT = "Y"
              IF BRANCH-PAGE-FG NOT = "N"
                 MOVE "PLEASE ENTER Y OR N!" TO MESS
                 PERFORM SEND-MESS
                 GO TO PAGE-ON-BRANCH-YN.

       CREATE-TIN-FILE-YN.
           MOVE "E  A010TINFG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PAGE-ON-BRANCH-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO CREATE-TIN-FILE-YN.
           MOVE VDUBUF TO CREATE-TIN-FG.
           IF CREATE-TIN-FG NOT = "Y"
              IF CREATE-TIN-FG NOT = "N"
                 MOVE "PLEASE ENTER Y OR N!" TO MESS
                 PERFORM SEND-MESS
                 GO TO CREATE-TIN-FILE-YN.

       EXIT-PARM.
           EXIT.

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BEGBR." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000ENDBR." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEGBR." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040ENDBR." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN020BEGCL." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020ENDCL." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A000JUDGEMENT." TO VDU.
           MOVE JUDGEMENT-FG TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000BANKRUPT." TO VDU.
           MOVE BANKRUPT-FG TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  8000AU_LASTPAY1." TO VDU.
           MOVE AU-BEG-LAST-PAY-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8000AU_LASTPAY2." TO VDU.
           MOVE AU-END-LAST-PAY-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8000PLDATE1." TO VDU.
           MOVE BEG-PL-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8000PLDATE2." TO VDU.
           MOVE END-PL-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8000PL_LASTPAY1." TO VDU.
           MOVE PL-BEG-LAST-PAY-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8000PL_LASTPAY2." TO VDU.
           MOVE PL-END-LAST-PAY-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8000YRENDDATE." TO VDU.
           MOVE YEAREND-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "SNNN020FORMNO." TO VDU.
           MOVE FORMNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A000REPORTONLY." TO VDU.
           MOVE REPORT-ONLY-FG TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000DETAILYN." TO VDU.
           MOVE PRINT-DETAIL-FG TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000TINFG." TO VDU.
           MOVE CREATE-TIN-FG TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************
       CREATE-FM-REC SECTION.
      ******************************************
           PERFORM OPEN-FM1-FILE.

           PERFORM CLEAR-FM-FILE.

           MOVE SPACES TO LPWSFM-EDIT-FM-INITKEY.


           MOVE LN-ACCTNO          TO LPWSFM-EDIT-FM-ACCTNO.
           MOVE FORMNO             TO LPWSFM-EDIT-FM-FORMNO.
           MOVE LN-OWNBR           TO LPWSFM-EDIT-FM-BRNO2.

           MOVE FM-PATH-OWNBR          TO FM-BRNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE FORMNO                 TO FM-FMNO.

           MOVE FM1-KEY       TO HOLD-FM1-KEY.
           PERFORM READ-FM1-FILE.

           MOVE BW-ZIP             TO FM-ZIP.
           MOVE SPACES             TO FM-PRINT.
           MOVE 1                  TO FM-STMT-COUNTER.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.

           IF IO-FG = 0
              PERFORM REWRITE-FM1-FILE
           ELSE
              MOVE HOLD-FM1-KEY TO FM1-KEY
              PERFORM WRITE-FM1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           PERFORM CLOSE-FM1-FILE.

      ****************************************
       CREATE-IC-REC SECTION.
      ****************************************

           ADD 1 TO IC-KEY.

           MOVE SPACES TO IC-REC.

           MOVE "B"          TO IC-TYPE.
      * "F" - BY AGREEMENT #1537
           MOVE "F"          TO IC-B-EVENT-CODE.
           MOVE YEAREND-DATE TO DATE-YYYYMMDD IC-B-DATE-CANCEL-YYYYMMDD.
           MOVE DATE-YYYYMMDD-YYYY TO IC-B-PAY-YEAR.
           MOVE "LOAN"       TO IC-B-DEBT-DESC.
           MOVE BW-LNAME     TO WORK-FIELD.
           PERFORM EDIT-NAME-CONTROL.
           MOVE WORK-FIELD-2 TO IC-B-NAME-CONTROL.
           MOVE "2"          TO IC-B-TIN-TYPE-CD.
           MOVE BW-SSNO      TO IC-B-TIN.
           MOVE LN-OWNBR     TO PAYER-BRNO.
           MOVE LN-CLASS     TO PAYER-CLASS.
           MOVE LN-ACCTNO    TO PAYER-NUMBER.
           MOVE 01           TO PAYER-SEQNO.
           MOVE "-" TO PAYER-DASH1 PAYER-DASH2 PAYER-DASH3.
           MOVE PAYER-ACCT   TO IC-B-ACCTNO.
           MOVE LN-OWNBR     TO IC-B-OFFICE-CODE.

           MOVE CURBAL-LESS-UNEARNED TO IC-B-PAYAMT(2).
           PERFORM ZERO-OTHER-AMTS.
           IF BW-STATE > "ZZ"
              MOVE "1"       TO IC-B-FOREIGN-CD.
           IF BW-STATE = "MX"
              MOVE "1"       TO IC-B-FOREIGN-CD.

      * CHANGED THIS FROM BW-NAME TO A WORKING STORAGE FIELD SO IT
      * CONTINUES TO MATCH A15. WE CHANGED NAME FIELDS FROM 16 TO 35
           MOVE BW-LNAME TO PAYEE-LNAME.
           MOVE BW-FNAME TO PAYEE-FNAME.

           MOVE PAYEE-NAME TO NAME-IN.
           PERFORM FIX-NAME.
           IF CREATE-TIN-FG = "N"
              IF BAD-NAME-FG = "Y"
                 ADD 1 TO BAD-NAME-COUNTER.
           MOVE NAME-OUT     TO IC-B-PAYEE-NAME.
           MOVE BW-ADR1 TO WORK-FIELD.
           MOVE BW-ADR2 TO WORK-FIELD-2.
           MOVE 40 TO SUB.
           MOVE 1 TO SUB2.
           PERFORM MERGE-IT.
           MOVE WORK-FIELD TO IC-B-PAYEE-ADDR.
           MOVE BW-CITY    TO IC-B-PAYEE-CITY.
           MOVE BW-STATE   TO IC-B-PAYEE-STATE.
           MOVE BW-ZIP     TO ZIP-WORK.
      *    -------------------------------------------------------------
      *    IF ZIP IS IN XXXXX-XXXX FORMAT THEN SHIFT EXT TO PUT
      *       INTO XXXXXXXXXX FORMAT
      *    -------------------------------------------------------------
           IF PART-3-DASH = "-"
              MOVE PART-2 TO PART-2-A
              MOVE SPACE TO ZIP-JUNK.
           MOVE ZIP-WORK TO IC-B-PAYEE-ZIP.

           MOVE CARRAGE-RET TO IC-CARR-RTN.
           PERFORM WRITE-IC-FILE.

      ******************************************************************
       EDIT-NAME-CONTROL SECTION.
      ******************************************************************

      * EDIT NAME CONTROL, NO SPACES, PERIODS, APOSTROPHES

           MOVE 1 TO SUB SUB2.
       EDIT-NAME-LOOP.
           IF SUB2 > 4
              GO TO EXIT-EDIT-NAME.
           IF SUB > 16
              GO TO EXIT-EDIT-NAME.
           IF WORK-CHAR(SUB) = " "  OR "." OR "'"
              ADD 1 TO SUB
              GO TO EDIT-NAME-LOOP.

           MOVE WORK-CHAR(SUB) TO WORK-CHAR-2(SUB2).
           ADD 1 TO SUB SUB2.

           GO TO EDIT-NAME-LOOP.
       EXIT-EDIT-NAME.
           EXIT.

      ****************************************
       FIX-NAME SECTION.
      ****************************************
           MOVE 1 TO SUB SUB2.
           MOVE "N" TO BAD-NAME-FG.
       FIX-NAME-LOOP.
           IF SUB > 32
              GO TO EXIT-FIX-NAME.

           MOVE NAME-IN-CHAR(SUB) TO TEST-CHAR.
           IF TEST-CHAR NOT ALPHABETIC
               IF TEST-CHAR NOT NUMERIC
                  IF NOT VALID-CHAR
                     MOVE "Y" TO BAD-NAME-FG
                     IF SUB < 32
                        IF NAME-IN-CHAR(SUB + 1) = " "
                           ADD 1 TO SUB
                           GO TO FIX-NAME-LOOP
                        ELSE
                           MOVE " " TO NAME-IN-CHAR(SUB)
                        END-IF
                     ELSE
                        MOVE " " TO NAME-IN-CHAR(SUB).

           MOVE NAME-IN-CHAR(SUB) TO NAME-OUT-CHAR(SUB2).
           ADD 1 TO SUB SUB2.
           GO TO FIX-NAME-LOOP.
       EXIT-FIX-NAME.
           EXIT.

      ******************************************************************
       MERGE-IT SECTION.
      ******************************************************************

      * ADR 1 IN WORK-FIELD, COUNTS LENGTH BY STARTING AT THE END,
      * THEN LOOPS TO MOVE AS MUCH OF ADR 2 THAT WILL FIT

       MERGE-LOOP.

           IF WORK-CHAR(SUB) = " " AND SUB > 1
              SUBTRACT 1 FROM SUB
              GO TO MERGE-LOOP.

           IF SUB = 1
              MOVE WORK-FIELD-2 TO WORK-FIELD
              GO TO EXIT-MERGE-IT.
           ADD 2 TO SUB.
           IF SUB > 39
              GO TO EXIT-MERGE-IT.

       MERGE-THEM.
           MOVE WORK-CHAR-2(SUB2) TO WORK-CHAR(SUB).
           IF SUB < 40
              ADD 1 TO SUB SUB2
              GO TO MERGE-THEM.

       EXIT-MERGE-IT.
           EXIT.

      ******************************************************************
       CREATE-WORK-FILE SECTION.
      ******************************************************************

      * CREATE WORK FILE FOR PRINT SHOP TO PRINT 1099 SUBSTITUTES(WORLD)

           MOVE SPACES      TO WK-REC.
           MOVE BW-LNAME    TO WK-LNAME
           MOVE BW-FNAME    TO WK-FNAME
           MOVE BW-ZIP      TO WK-ZIP.
           MOVE BW-ADR1     TO WK-ADR.
           IF BW-ADR2 NOT = SPACES
              STRING BW-ADR1," ",BW-ADR2 DELIMITED BY "   "
                 INTO WK-ADR.
           MOVE BW-CITY     TO WK-CITY.
           MOVE BW-STATE    TO WK-STATE.
           MOVE LN-OWNBR    TO WK-OWNBR.
           MOVE BW-REPCODE  TO WK-REPCD.
           MOVE BW-SRCD     TO WK-SRCD.
           MOVE LN-ACCTNO   TO WK-ACCTNO.

           MOVE CURBAL-LESS-UNEARNED TO WK-1099-AMT.
           MOVE BW-SSNO TO WK-SSNO.

           MOVE X"0D" TO WK-CR.
           MOVE X"0A" TO WK-LF.

           ADD 1 TO WK-KEY.
           PERFORM WRITE-WK-FILE.

       EXIT-PROCESS-WORK-FILE.
           EXIT.

      ****************************************
       CREATE-TIN-REC SECTION.
      ****************************************

      * IRS REJECTS IF DUPLICATES, ONLY WRITE SSNO ONCE FOR BRANCH

           IF CREATE-TIN-FG NOT = "Y"
              GO TO EDIT-FNAME.

           MOVE "N"        TO DUP-FG.
           MOVE LN-OWNBR   TO WK2-BRNO.
           MOVE LN-SSNO(1) TO WK2-SSNO.
           PERFORM WRITE-WK2-FILE.
           IF IO-BAD
              IF E-CODE = "22"
                 ADD 1 TO DUP-COUNTER
                 MOVE "Y" TO DUP-FG
                 GO TO EXIT-CREATE-TIN
              ELSE
                 GO TO IO-ERROR.
       EDIT-FNAME.
           MOVE 1 TO SUB SUB2.
           MOVE "N" TO BAD-NAME-FG.
           MOVE SPACES TO WORK-FNAME WORK-LNAME.
           MOVE BW-FNAME TO WORK-NAME.
       EDIT-FNAME-LOOP.
           IF SUB > 16
              GO TO EDIT-LNAME.
           MOVE NAME-CHAR(SUB) TO TEST-CHAR.
           IF TEST-CHAR NOT ALPHABETIC
               IF TEST-CHAR NOT NUMERIC
                  IF NOT VALID-CHAR
                     MOVE "Y" TO BAD-NAME-FG
                     ADD 1 TO SUB
                     GO TO EDIT-FNAME-LOOP.
           MOVE NAME-CHAR(SUB) TO FNAME-CHAR(SUB2).
           ADD 1 TO SUB SUB2.
           GO TO EDIT-FNAME-LOOP.
       EDIT-LNAME.
           MOVE 1 TO SUB SUB2.
           MOVE BW-LNAME TO WORK-NAME.
       EDIT-LNAME-LOOP.
           IF SUB > 16
              GO TO CREATE-TIN.
           MOVE NAME-CHAR(SUB) TO TEST-CHAR.
           IF TEST-CHAR NOT ALPHABETIC
               IF TEST-CHAR NOT NUMERIC
                  IF NOT VALID-CHAR
                     MOVE "Y" TO BAD-NAME-FG
                     ADD 1 TO SUB
                     GO TO EDIT-LNAME-LOOP.
           MOVE NAME-CHAR(SUB) TO LNAME-CHAR(SUB2).
           ADD 1 TO SUB SUB2.
           GO TO EDIT-LNAME-LOOP.
       CREATE-TIN.
           IF BAD-NAME-FG = "Y"
              ADD 1 TO BAD-NAME-COUNTER.

           MOVE SPACES TO TIN-REC.

      *  20190715 BAH MAKE LN-ACCTNO LOOK LIKE A15 IN THE TIN FILE, MOVE
      *               TO A 9(6) FIRST SO THERE ARE NO LEADING ZEROS, QA0050
           MOVE LN-ACCTNO TO WORK-ACCTNO.

           STRING "2", ";",
           LN-SSNO(1), ";",
           WORK-FNAME, " ", WORK-LNAME, ";",
           LN-OWNBR, WORK-ACCTNO,
           X"0D"
           DELIMITED BY "  " INTO TIN-REC.

           IF CREATE-TIN-FG = "Y"
              PERFORM WRITE-TIN-FILE.

       EXIT-CREATE-TIN.
           EXIT.

      *************************************************
       CREATE-MEMO SECTION.
      *************************************************
           MOVE LN-ACCTNO TO LM-ACCTNO.
           ACCEPT DATE-YYYYMMDD FROM CENTURY-DATE.
           SUBTRACT DATE-YYYYMMDD FROM 99999999 GIVING LM-DATE.
           MOVE 0 TO LM-SEQ.

           PERFORM OPEN-LM1-FILE.

       CREATE-MEMO-NEXT.
           IF LM-SEQ = 999
              GO TO CREATE-MEMO-EXIT.

           ADD 1 TO LM-SEQ.
           PERFORM READ-LM1-FILE.
           IF IO-FG = 0
      *       UNLOCK LM-FILE
              GO TO CREATE-MEMO-NEXT.

           MOVE LN-OWNBR TO LM-BRNO.
           MOVE "9"      TO LM-MEMO-TYPE.
           MOVE " " TO MESS.
           MOVE YEAREND-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY          TO ALP-DATE-EDIT.
           MOVE CURBAL-LESS-UNEARNED TO NUM-EDIT.
           STRING "1099-C SENT FOR YEAR ENDING " ALP-DATE-EDIT
                 ", FOR $" NUM-EDIT
                     DELIMITED BY SIZE INTO LM-MEMO.

           PERFORM WRITE-LM1-FILE.
           IF IO-FG = 9
              PERFORM IO-ERROR.

       CREATE-MEMO-EXIT.
           PERFORM CLOSE-LM1-FILE.

      *******************************************
       HEAD-PAGE SECTION.
      *******************************************

           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.
           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD2       TO PR-REC.
           MOVE 1           TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD3        TO PR-REC.
           MOVE 2           TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      *************************************
       WRITE-DETAIL-LINE SECTION.
      *************************************
           ADD LN-FEED       TO LN-COUNT.

           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE  TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1            TO LN-FEED.
           MOVE SPACES       TO DETAIL-LINE.

      *************************************
       PRINT-RANGE-LINES SECTION.
      *************************************

           MOVE 4                      TO LN-FEED.
      *-------------------------------------------------------------
           IF GROUP-FLAG = "Y"
              MOVE "SELECTED GROUP      :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH    :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING              :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
       
           MOVE "BEGINNING CLASS     :" TO RANGE-LINE.
           MOVE BEG-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING              :" TO RANGE-LINE.
           MOVE END-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "INCLUDE JUDGEMENTS  :" TO RANGE-LINE.
           MOVE  JUDGEMENT-FG          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "INCLUDE BANKRUPTS   :" TO RANGE-LINE.
           MOVE  BANKRUPT-FG           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "BEG AU LAST PAY DATE:" TO RANGE-LINE.
           MOVE  AU-BEG-LAST-PAY-DATE   TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END AU LAST PAY DATE:" TO RANGE-LINE.
           MOVE  AU-END-LAST-PAY-DATE   TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "BEGINNING PL DATE   :" TO RANGE-LINE.
           MOVE  BEG-PL-DATE          TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING PL DATE      :" TO RANGE-LINE.
           MOVE  END-PL-DATE          TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "BEG PL LAST PAY DATE:" TO RANGE-LINE.
           MOVE  PL-BEG-LAST-PAY-DATE   TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END PL LAST PAY DATE:" TO RANGE-LINE.
           MOVE  PL-END-LAST-PAY-DATE   TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "YEAR END DATE       :" TO RANGE-LINE.
           MOVE  YEAREND-DATE         TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "FORM NUMBER         :" TO RANGE-LINE.
           MOVE  FORMNO               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "REPORT ONLY         :" TO RANGE-LINE.
           MOVE  REPORT-ONLY-FG       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "PRINT DETAIL        :" TO RANGE-LINE.
           MOVE  PRINT-DETAIL-FG      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "PAGE ON BRANCH      :" TO RANGE-LINE.
           MOVE  BRANCH-PAGE-FG       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           IF REPORT-ONLY-FG = "N"
              MOVE "IRS FILE PATH       :" TO RANGE-LINE
              MOVE  IC-PATH              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "PRINTSHOP FILE PATH :" TO RANGE-LINE
              MOVE  WK-PATH              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
       
           MOVE "CREATE TIN MATCHING FILE  :" TO RANGE-LINE.
           MOVE  CREATE-TIN-FG                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           IF CREATE-TIN-FG = "Y"
              MOVE "TIN MATCH FILE PATH :" TO RANGE-LINE
              MOVE  TIN-PATH               TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.

      **************************************************************************
       COMPUTE-EARNINGS SECTION.
      **************************************************************************

      * COPIED LIBLP/LPEARN INTO PROGRAM BECAUSE WE NEED TO REMOVE THE CHECK
      * ON LN-PLCD WHICH SKIPS ADDING TO UNEARNED
           MOVE 0 TO EARN-TOT-ANTICERN
                     EARN-TOT-ANTICADJ
                     EARN-TOT-ACCERN
                     EARN-TOT-UFERN
                     EARN-TOT-UNEARN
                     EARN-UNEARN(1) EARN-UNEARN(2)
                     EARN-UNEARN(3) EARN-UNEARN(4)
                     EARN-UNEARN(5) EARN-UNEARN(6).

           MOVE 1 TO EARN.
           PERFORM COMPUTE-EARNINGS-ACCUM 6 TIMES.

       COMPUTE-EARNINGS-EXIT.
           EXIT.

      ************************************************************
       COMPUTE-EARNINGS-ACCUM SECTION.
      ************************************************************

      * REGACC PR#210 DONT INCLUDE MAINTFEE OR OTHER IN ANTIC.

           IF EARN < 6 OR
             (EARN = 6 AND NOT(GP-INCL-OTH-IN-EARNED-N OR
                                GP-INCL-OTH-IN-EARNED-R))
             IF NOT(EARN = 4 AND GP-INCL-OTH-IN-EARNED-R)
              ADD LN-ANTICERN(EARN) TO EARN-TOT-ANTICERN
              ADD LN-ANTICADJ(EARN) TO EARN-TOT-ANTICADJ
              ADD LN-ACCERN(EARN) TO EARN-TOT-ACCERN
              ADD LN-UFERN(EARN) TO EARN-TOT-UFERN.


           IF LN-CURBAL NOT = 0
            IF LN-POCD-OPEN OR LN-POCD-PB-RESC
      *      IF LN-PLCD = " "
               IF LN-SERVACCT NOT = "Y"
                IF LN-ANTICADJ(EARN) = 0
                   COMPUTE EARN-UNEARN(EARN) =
                                 LN-ANTICERN(EARN) - LN-ACCERN(EARN)
                   PERFORM COMPUTE-EARNINGS-ACCUM-RBTEST
                   IF EARN < 6 OR (EARN = 6 AND
                                   NOT (GP-INCL-OTH-IN-EARNED-N  OR
                                          GP-INCL-OTH-IN-EARNED-R))
                      IF NOT(EARN = 4 AND GP-INCL-OTH-IN-EARNED-R)
                          ADD EARN-UNEARN(EARN) TO EARN-TOT-UNEARN.

           ADD 1 TO EARN.

      ************************************************
       COMPUTE-EARNINGS-ACCUM-RBTEST SECTION.
      ************************************************
      * NOTE: WHAT ABOUT LEVEL 2 INS. ADDONS RE: REBATE
           IF EARN = 1 AND (NOT (LN-REBATE(4) = " " OR "C"))
              MOVE 0 TO EARN-UNEARN(EARN)
           ELSE
           IF EARN = 3 AND (NOT (LN-REBATE(5) = " " OR "C"))
              MOVE 0 TO EARN-UNEARN(EARN)
           ELSE
           IF EARN = 4 AND (NOT (LN-REBATE(6) = " " OR "C"))
              MOVE 0 TO EARN-UNEARN(EARN).

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPFMCL.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO LN1099-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LN1099-SYSDATE.
           DISPLAY LN1099-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LN1099-FORM-FIELDS TO WR-BUF.
           MOVE LN1099-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LN1099-HELP--FILE TO HELP-LINK-FILE.
           MOVE LN1099-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/LN1099_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-ROUTINE SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       ZERO-OTHER-AMTS.
           MOVE 0 TO IC-B-PAYAMT(1) IC-B-PAYAMT(3) IC-B-PAYAMT(4)
                     IC-B-PAYAMT(5) IC-B-PAYAMT(6) IC-B-PAYAMT(7)
                     IC-B-PAYAMT(8) IC-B-PAYAMT(9) IC-B-PAYAMT(10)
                     IC-B-PAYAMT(11) IC-B-PAYAMT(12) IC-B-PAYAMT(13)
                     IC-B-PAYAMT(14) IC-B-PAYAMT(15) IC-B-PAYAMT(16)
                     IC-B-PAYAMT(17) IC-B-PAYAMT(18).

      ********************************************
       IO-ROUTINE SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPLN3IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBSP/SPICI.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       OPEN-TIN-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE TIN-PATH TO E-FILE.
           OPEN OUTPUT TIN-FILE.
           IF IO-FG = 8
              GO TO OPEN-TIN-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE TIN-FILE
              GO TO OPEN-TIN-FILE-OUTPUT.
       WRITE-TIN-FILE.
           PERFORM WRITE-IT.
           MOVE TIN-PATH TO E-FILE.
           WRITE TIN-REC.
           IF IO-FG = 8
              GO TO WRITE-TIN-FILE.
       CLOSE-TIN-FILE.
           PERFORM CLOSE-IT.
           MOVE TIN-PATH TO E-FILE.
           CLOSE TIN-FILE.

       COPY "LIBGB/FERRORS.CPY".

      ***************************************
       END-PROGRAM SECTION.
      ***************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

       EXIT-PROG.
           IF CREATE-TIN-FG = "Y"
              MOVE MKTEMP-BUF TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/IRMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LN1099-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
