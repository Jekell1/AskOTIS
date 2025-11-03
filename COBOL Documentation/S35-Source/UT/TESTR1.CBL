      *  A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TESTR1.
      *****************************************************
      * CINDY
      *
      *    CREATES PIPE DELIMITED /USR/EXTRACT/             
      *  REV:
      * BAH 20200831 REMOVED RESET ON BR1 
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WK-GLOBRD-FILE ASSIGN OUTPUT WK-GLOBRD-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT WK-LPTBNO-FILE ASSIGN OUTPUT WK-LPTBNO-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT WK-MATURITY-FILE ASSIGN OUTPUT WK-MATURITY-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT WK-EXPIR-FILE ASSIGN OUTPUT WK-EXPIR-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT WK-INSURANCE-FILE ASSIGN OUTPUT WK-INSURANCE-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT WK-LPUPER-FILE ASSIGN OUTPUT WK-LPUPER-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT WK-LPAMTS-FILE ASSIGN OUTPUT WK-LPAMTS-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT WK-LPLCHG-FILE ASSIGN OUTPUT WK-LPLCHG-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT WK-SPPDTH-FILE ASSIGN OUTPUT WK-SPPDTH-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT WK-LPCEPP-FILE ASSIGN OUTPUT WK-LPCEPP-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT WK-LPIBPC-FILE ASSIGN OUTPUT WK-LPIBPC-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT WK-LNCOST-FILE ASSIGN OUTPUT WK-LNCOST-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT WK-LPEARN-FILE ASSIGN OUTPUT WK-LPEARN-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT WK-LPCDEL-FILE ASSIGN OUTPUT WK-LPCDEL-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT WK-LPLCAS-FILE ASSIGN OUTPUT WK-LPLCAS-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

%%         SELECT WK-LPLCAP-FILE ASSIGN OUTPUT WK-LPLCAP-PATH
%%                ORGANIZATION LINE SEQUENTIAL
%%                LOCK MODE AUTOMATIC WITH LOCK ON RECORD
%%                FILE STATUS FILE-STAT.

           SELECT WK-LPPDUE-FILE ASSIGN OUTPUT WK-LPPDUE-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

           SELECT WK-AGEING-FILE ASSIGN OUTPUT WK-AGEING-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.


       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      **************************************************
      *   LOAN ASCII FILE    RSZ =  (INCLUDES 0D0A)
      **************************************************
       FD  WK-GLOBRD-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-GLOBRD-REC.
           03  WK-GLOBRD-BYTE        PIC X OCCURS 2000 TIMES.

       FD  WK-LPTBNO-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-LPTBNO-REC.
           03  WK-LPTBNO-BYTE        PIC X OCCURS 2000 TIMES.

       FD  WK-MATURITY-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-MATURITY-REC.
           03  WK-MATURITY-BYTE        PIC X OCCURS 2000 TIMES.

       FD  WK-EXPIR-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-EXPIR-REC.
           03  WK-EXPIR-BYTE        PIC X OCCURS 2000 TIMES.

      *CALCULATE INSURANCE TERM FOR EACH INSURANCE
       FD  WK-INSURANCE-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-INSURANCE-REC.
           03  WK-INS-BYTE        PIC X OCCURS 2000 TIMES.

      *NO UNIT PERIODS PER YEAR, NO OF DAYS IN UNIT PERIOD
      *NO OF FULL AND FRACTIONAL UNIT PERIODS BETWEEN INTDATE AND 1STPYDATE
       FD  WK-LPUPER-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-LPUPER-REC.
           03  WK-LPUPER-BYTE        PIC X OCCURS 2000 TIMES.

       FD  WK-LPAMTS-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-LPAMTS-REC.
           03  WK-LPAMTS-BYTE        PIC X OCCURS 2000 TIMES.
 
       FD  WK-LPLCHG-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-LPLCHG-REC.
           03  WK-LPLCHG-BYTE        PIC X OCCURS 2000 TIMES.

       FD  WK-SPPDTH-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-SPPDTH-REC.
           03  WK-SPPDTH-BYTE        PIC X OCCURS 2000 TIMES.
 
       FD  WK-LPCEPP-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-LPCEPP-REC.
           03  WK-LPCEPP-BYTE        PIC X OCCURS 2000 TIMES.

       FD  WK-LPIBPC-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-LPIBPC-REC.
           03  WK-LPIBPC-BYTE        PIC X OCCURS 2000 TIMES.

       FD  WK-LNCOST-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-LNCOST-REC.
           03  WK-LNCOST-BYTE        PIC X OCCURS 2000 TIMES.

       FD WK-LPEARN-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-LPEARN-REC.
           03  WK-LPEANR-BYTE        PIC X OCCURS 2000 TIMES.

       FD WK-LPCDEL-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-LPCDEL-REC.
           03  WK-LPCDEL-BYTE        PIC X OCCURS 2000 TIMES.

       FD WK-LPLCAS-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-LPLCAS-REC.
           03  WK-LPLCAS-BYTE        PIC X OCCURS 2000 TIMES.


       FD WK-LPLCAP-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-LPLCAP-REC.
           03  WK-LPLCAP-BYTE        PIC X OCCURS 2000 TIMES.

       FD WK-LPPDUE-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-LPPDUE-REC.
           03  WK-LPPDUE-BYTE        PIC X OCCURS 2000 TIMES.


       FD WK-AGEING-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-AGEING-REC.
           03  WK-AGEING-BYTE        PIC X OCCURS 2000 TIMES.



       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/TESTR1 S35 03/31/23-09:24:37 >".

      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    W K - P A T H
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  WK-GLOBRD-PATH.
           03  WK-GLOBRD-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-GLOBRD-FILENAME               PIC X(8) 
                 VALUE "/GLOBRD.".                    
           03  WK-GLOBRD-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-GLOBRD-PATH-YR                PIC 99.
           03  WK-GLOBRD-PATH-MO                PIC 99.
           03  WK-GLOBRD-PATH-DA                PIC 99.

       01  WK-LPTBNO-PATH.
           03  WK-LPTBNO-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-LPTBNO-FILENAME               PIC X(8) 
                 VALUE "/LPTBNO.".                    
           03  WK-LPTBNO-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-LPTBNO-PATH-YR                PIC 99.
           03  WK-LPTBNO-PATH-MO                PIC 99.
           03  WK-LPTBNO-PATH-DA                PIC 99.

       01  WK-MATURITY-PATH.
           03  WK-MATURITY-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-MATURITY-FILENAME               PIC X(10) 
                 VALUE "/MATURITY.".                    

           03  WK-MATURITY-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-MATURITY-PATH-YR                PIC 99.
           03  WK-MATURITY-PATH-MO                PIC 99.
           03  WK-MATURITY-PATH-DA                PIC 99.


       01  WK-EXPIR-PATH.
           03  WK-EXPIR-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-EXPIR-FILENAME               PIC X(07) 
                 VALUE "/EXPIR.".                    

           03  WK-EXPIR-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-EXPIR-PATH-YR                PIC 99.
           03  WK-EXPIR-PATH-MO                PIC 99.
           03  WK-EXPIR-PATH-DA                PIC 99.

       01  WK-INSURANCE-PATH.
           03  WK-INS-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-EXPIR-FILENAME               PIC X(11) 
                 VALUE "/INSURANCE.".                    

           03  WK-INS-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-INS-PATH-YR                PIC 99.
           03  WK-INS-PATH-MO                PIC 99.
           03  WK-INS-PATH-DA                PIC 99.

       01  WK-LPUPER-PATH.
           03  WK-LPUPER-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-UPER-FILENAME               PIC X(8) 
                 VALUE "/LPUPER.".                       

           03  WK-LPUPER-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-LPUPER-PATH-YR                PIC 99.
           03  WK-LPUPER-PATH-MO                PIC 99.
           03  WK-LPUPER-PATH-DA                PIC 99.

       01  WK-LPAMTS-PATH.
           03  WK-LPAMTS-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-LPAMTS-FILENAME               PIC X(8) 
                 VALUE "/LPAMTS.".                       

           03  WK-LPAMTS-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-LPAMTS-PATH-YR                PIC 99.
           03  WK-LPAMTS-PATH-MO                PIC 99.
           03  WK-LPAMTS-PATH-DA                PIC 99.


       01  WK-LPLCHG-PATH.
           03  WK-LPLCHG-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-UPER-FILENAME               PIC X(8) 
                 VALUE "/LPLCHG.".                       

           03  WK-LPLCHG-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-LPLCHG-PATH-YR                PIC 99.
           03  WK-LPLCHG-PATH-MO                PIC 99.
           03  WK-LPLCHG-PATH-DA                PIC 99.

       01  WK-SPPDTH-PATH.
           03  WK-SPPDTH-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-UPER-FILENAME               PIC X(8) 
                 VALUE "/SPPDTH.".                       

           03  WK-SPPDTH-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-SPPDTH-PATH-YR                PIC 99.
           03  WK-SPPDTH-PATH-MO                PIC 99.
           03  WK-SPPDTH-PATH-DA                PIC 99.

       01  WK-LPCEPP-PATH.
           03  WK-LPCEPP-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-UPER-FILENAME               PIC X(8) 
                 VALUE "/LPCEPP.".                       

           03  WK-LPCEPP-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-LPCEPP-PATH-YR                PIC 99.
           03  WK-LPCEPP-PATH-MO                PIC 99.
           03  WK-LPCEPP-PATH-DA                PIC 99.


       01  WK-LPIBPC-PATH.
           03  WK-LPIBPC-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-LPIBPC-FILENAME               PIC X(8) 
                 VALUE "/LPIBPC.".                       

           03  WK-LPIBPC-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-LPIBPC-PATH-YR                PIC 99.
           03  WK-LPIBPC-PATH-MO                PIC 99.
           03  WK-LPIBPC-PATH-DA                PIC 99.
      
       01  WK-LNCOST-PATH.
           03  WK-LNCOST-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-LNCOST-FILENAME               PIC X(8) 
                 VALUE "/LNCOST.".                       

           03  WK-LNCOST-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-LNCOST-PATH-YR                PIC 99.
           03  WK-LNCOST-PATH-MO                PIC 99.
           03  WK-LNCOST-PATH-DA                PIC 99.
      
       01  WK-LPEARN-PATH.
           03  WK-LPEARN-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-LPEARN-FILENAME               PIC X(8) 
                 VALUE "/LPEARN.".                       

           03  WK-LPEARN-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-LPEARN-PATH-YR                PIC 99.
           03  WK-LPEARN-PATH-MO                PIC 99.
           03  WK-LPEARN-PATH-DA                PIC 99.

       01  WK-LPCDEL-PATH.
           03  WK-LPCDEL-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-LPCDEL-FILENAME               PIC X(8) 
                 VALUE "/LPCDEL.".                       

           03  WK-LPCDEL-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-LPCDEL-PATH-YR                PIC 99.
           03  WK-LPCDEL-PATH-MO                PIC 99.
           03  WK-LPCDEL-PATH-DA                PIC 99.

       01  WK-LPLCAS-PATH.
           03  WK-LPLCAS-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-LPLCAS-FILENAME               PIC X(8) 
                 VALUE "/LPLCAS.".                       

           03  WK-LPLCAS-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-LPLCAS-PATH-YR                PIC 99.
           03  WK-LPLCAS-PATH-MO                PIC 99.
           03  WK-LPLCAS-PATH-DA                PIC 99.



       01  WK-LPLCAP-PATH.
           03  WK-LPLCAP-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-LPLCAP-FILENAME               PIC X(8) 
                 VALUE "/LPLCAP.".                       

           03  WK-LPLCAP-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-LPLCAP-PATH-YR                PIC 99.
           03  WK-LPLCAP-PATH-MO                PIC 99.
           03  WK-LPLCAP-PATH-DA                PIC 99.


       01  WK-LPPDUE-PATH.
           03  WK-LPPDUE-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-LPPDUE-FILENAME               PIC X(8) 
                 VALUE "/LPPDUE.".                       

           03  WK-LPPDUE-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-LPPDUE-PATH-YR                PIC 99.
           03  WK-LPPDUE-PATH-MO                PIC 99.
           03  WK-LPPDUE-PATH-DA                PIC 99.


       01  WK-AGEING-PATH.
           03  WK-AGEING-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-AGEING-FILENAME               PIC X(8) 
                 VALUE "/AGEING.".                       

           03  WK-AGEING-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-AGEING-PATH-YR                PIC 99.
           03  WK-AGEING-PATH-MO                PIC 99.
           03  WK-AGEING-PATH-DA                PIC 99.




       01  WS-GLOBRD-REC.
           03  WS-GLOBRD-LN-BRACCT                PIC Z(14).
           03  WS-GLOBRD-LN-ACCTNO                PIC Z(10).
           03  WS-GLOBRD-LN-OWNBR                 PIC Z(6).
           03  WS-GLOBRD-LN-SP1-KEY               PIC X(9).
           03  WS-GLOBRD-HOLIDAYS.
               05  WS-GLOBRD-HOLIDAY-1                PIC 9(4).
               05  WS-GLOBRD-HOLIDAY-2                PIC 9(4).
               05  WS-GLOBRD-HOLIDAY-3                PIC 9(4).
               05  WS-GLOBRD-HOLIDAY-4                PIC 9(4).
               05  WS-GLOBRD-HOLIDAY-5                PIC 9(4).
               05  WS-GLOBRD-HOLIDAY-6                PIC 9(4).
               05  WS-GLOBRD-HOLIDAY-7                PIC 9(4).
               05  WS-GLOBRD-HOLIDAY-8                PIC 9(4).
               05  WS-GLOBRD-HOLIDAY-9                PIC 9(4).
               05  WS-GLOBRD-HOLIDAY-10               PIC 9(4).
               05  WS-GLOBRD-HOLIDAY-11               PIC 9(4).
               05  WS-GLOBRD-HOLIDAY-12               PIC 9(4).
               05  WS-GLOBRD-HOLIDAY-13               PIC 9(4).
               05  WS-GLOBRD-HOLIDAY-14               PIC 9(4).
               05  WS-GLOBRD-HOLIDAY-15               PIC 9(4).
               05  WS-GLOBRD-HOLIDAY-16               PIC 9(4).

           03  WS-GLOBRD-HOLIDAY-TAB REDEFINES 
                                WS-GLOBRD-HOLIDAYS.
               05  WS-GLOBRD-HOLIDAY        PIC 9999 OCCURS 16 TIMES.
       01  WK-GLOBRD-IFN        PIC X(6)     VALUE "GLOBRD".

       01  WS-LPTBNO-REC.
           03  WS-LPTBNO-LN-BRACCT                PIC Z(14).
           03  WS-LPTBNO-LN-ACCTNO                PIC Z(10).
           03  WS-LPTBNO-LN-OWNBR                 PIC Z(6).
           03  WS-LPTBNO-LN-SP1-KEY               PIC X(9).
           03  WS-LPTBNO-SP-CAL-NORATES-1         PIC ZZ9-.
           03  WS-LPTBNO-SP-CAL-NORATES-2         PIC ZZ9-.
           03  WS-LPTBNO-SP-EDT-NOEDITS-1         PIC ZZ9-.
           03  WS-LPTBNO-SP-EDT-NOEDITS-2         PIC ZZ9-.
           03  WS-LPTBNO-SP-INSNOVER-1            PIC ZZZ9.
           03  WS-LPTBNO-SP-INSNOVER-2            PIC ZZZ9.
           03  WS-LPTBNO-SP-INSNOVER-3            PIC ZZZ9.
           03  WS-LPTBNO-SP-INSNOVER-4            PIC ZZZ9.

       01  WK-LPTBNO-IFN        PIC X(6)     VALUE "LPTBNO".

       01  WS-MATURITY-REC.
           03  WS-MATURITY-LN-BRACCT                PIC Z(14).
           03  WS-MATURITY-LN-ACCTNO                PIC Z(10).
           03  WS-MATURITY-LN-OWNBR                 PIC Z(6).
      *INPUT TO LPMDTE
           03  WS-MATURITY-MDTE-FLAG                PIC X.
      *'Y' USES ORIG TERM, " " USES SUM-ORGTERM
           03  WS-MATURITY-MDTE-ORGTERM-FG          PIC X.
           03  WS-MATURITY-MDTE-DC-FLAG             PIC X.
      *    03  WS-MATURITY-LN-ORGTERM               PIC ZZ9.
           03  WS-MATURITY-LN-ORGTERM               PIC ZZZZ9.

      *    03  WS-MATURITY-LN-TOTNODEF              PIC Z9-.
           03  WS-MATURITY-LN-TOTNODEF              PIC ZZZ9-.

           03  WS-MATURITY-LN-1STPYDATE             PIC 99/99/9999.

      *    03  WS-MATURITY-LN-SUM-ORGTERM           PIC ZZ9.
           03  WS-MATURITY-LN-SUM-ORGTERM           PIC ZZZZ9.

           03  WS-MATURITY-LN-ORIG-1STPYDATE        PIC 99/99/9999.
      *LN-1STPYDATE + DF IF MDTE-ORGTERM-FG = 'Y'
      *INPUT TO INCREMENT-UNITPER (DATER)
           03  WS-MATURITY-DESC2                    PIC X(15).
           03  WS-MATURITY-NDTE-HOLD                PIC S9(5)   
                                                    LEADING SEPARATE.
           03  WS-MATURITY-LN-UNITPER-CD            PIC X.
           03  WS-MATURITY-LN-UNITPER-FREQ          PIC 999.
      *OUPUT
           03  WS-MATURITY-DESC3                    PIC X(15).
           03  WS-MATURITY-MDTE-DATE                PIC 99/99/9999.

       01  WK-MATURITY-IFN        PIC X(5)     VALUE "WKMAT".

       01  WS-EXPIR-REC.
           03  WS-EXPIR-LN-BRACCT                PIC Z(14).
           03  WS-EXPIR-LN-ACCTNO                PIC Z(10).
           03  WS-EXPIR-LN-OWNBR                 PIC Z(6).
      *INPUT TO LPMDTE
      *    03  WS-EXPIR-SP-FRMLA                 PIC X.
           03  WS-EXPIR-SP-EXPD-FRMLA            PIC X.
           03  WS-EXPIR-MDTE-FLAG                PIC X.
           03  WS-EXPIR-MDTE-DC-FLAG             PIC X.
           03  WS-EXPIR-MDTE-ORGTERM-FG          PIC X.
      *MATURITY DATE CALCULATION.
           03  WS-EXPIR-MDTE-DATE                PIC 99/99/9999.
           03  WS-EXPIR-SP-EXPD-MONTHS           PIC Z9.
           03  WS-EXPIR-SP-EXPD-DAYS             PIC Z9.
           03  WS-EXPIR-DESC1                    PIC X(10).
           03  WS-EXPIR-XDTE-DATE                PIC 99/99/9999.

       01  WK-EXPIR-IFN        PIC X(5)     VALUE "WKEXP".

       01  WS-INSURANCE-REC.
           03  WS-INS-LN-BRACCT                PIC Z(14).
           03  WS-INS-LN-ACCTNO                PIC Z(10).
           03  WS-INS-LN-OWNBR                 PIC Z(6).
      *INSURANCE LEVEL
           03  WS-INS-LN-INSCODES                       PIC XXX.
           03  WS-INS-ITRM-SUB                       PIC 99.
           03  WS-INS-ITRM-TODAY                     PIC 99/99/9999.
           03  WS-INS-LN-INSEFF-DATE                 PIC 99/99/9999.
           03  WS-INS-LN-INSEXP-DATE                 PIC 99/99/9999.
           03  WS-INS-LN-REBATE                      PIC X.
           03  WS-LN-UNITPER-CD                      PIC X.
           03  WS-INS-DESC1                          PIC X(5).
           03  WS-INS-ITRM-INSTERM                   PIC ZZZZZ9.
           03  WS-INS-DESC2                          PIC X(9).
           03  WS-INS-ITRM-INEFFECT-FG               PIC X.

       01  WK-INSURANCE-IFN        PIC X(5)     VALUE "WKINS".

       01  WS-LPUPER-REC.
           03  WS-LPUPER-LN-BRACCT                PIC Z(14).
           03  WS-LPUPER-LN-ACCTNO                PIC Z(10).
           03  WS-LPUPER-LN-OWNBR                 PIC Z(6).
      *IN
           03  WS-LPUPER-UNITPER-CD          PIC X.
           03  WS-LPUPER-UNITPER-FREQ        PIC ZZZZ9.
           03  WS-LPUPER-LN-INTDATE             PIC 99/99/9999.
           03  WS-LPUPER-LN-1STPYDATE           PIC 99/99/9999.
           03  WS-LPUPER-LN-TERM                PIC ZZZZ9.
      *    WORKERS
           03  WS-LPUPER-SINGLE-PAY-CD         PIC X.
           03  WS-LPUPER-SINGLE-PAY-FREQ     PIC ZZZZ9.                
      *OUTPUT
           03  WS-LPUPER-UNITPER-PER-YEAR     PIC  999.9(15).
           03  WS-LPUPER-DAYS-IN-UNITPER      PIC ZZZZ9.
           03  WS-LPUPER-FULL-UNITPER         PIC ZZZZ9.
           03  WS-LPUPER-FRACT-UNITPER        PIC .9(18).
           03  WS-LPUPER-ERRCD                PIC XX.
       
           
       01  WK-LPUPER-IFN        PIC X(4)     VALUE "UPER".


       01  WS-LPAMTS-REC.
           03  WS-LPAMTS-LN-BRACCT                PIC Z(14).
           03  WS-LPAMTS-LN-ACCTNO                PIC Z(10).
           03  WS-LPAMTS-LN-OWNBR                 PIC Z(6).
           03  WS-LPAMTS-LN-CREDIT-LIMIT          PIC ZZZZZZZ9.    
           03  WS-LPAMTS-LN-NOREGPY                PIC ZZZZ9.
           03  WS-LPAMTS-LN-1STPYAMT              PIC ZZZZZZZZ9.99.
           03  WS-LPAMTS-LN-REGPYAMT              PIC ZZZZZZZZ9.99.
           03  WS-LPAMTS-LN-LASTPYAMT             PIC ZZZZZZZZ9.99.
           03  WS-LPAMTS-LN-EXTCHG                PIC ZZZZZZ9.99.
           03  WS-LPAMTS-LN-MAINTFEE              PIC ZZZZZZ9.99.

           03  WS-LPAMTS-INSTBL OCCURS 8 TIMES.
               05  WS-LPAMTS-SUB                  PIC ZZZZ9.      
               05  WS-LPAMTS-LN-INSOURS           PIC X.
               05  WS-LPAMTS-LN-INSPREM           PIC ZZZZZZ9.99.
               05  WS-LPAMTS-LN-INSFINCHG         PIC X.
 
           03  WS-LPAMTS-FEETBL OCCURS 10 TIMES.
               05  WS-LPAMTS-LN-FEECODE           PIC XX.
               05  WS-LPAMTS-LN-FEEOURS           PIC X.
               05  WS-LPAMTS-LN-FEEFINCHG         PIC X.
               05  WS-LPAMTS-LN-FEEAMT            PIC ZZZZZZ9.99.


           03  WS-LPAMTS-TOTAL-REPAY              PIC ZZZZZZZZ9.99.
           03  WS-LPAMTS-TOTAL-NOTE               PIC ZZZZZZZZ9.99.
           03  WS-LPAMTS-ADJ-PRINCIPAL            PIC ZZZZZZZZ9.99.   
           03  WS-LPAMTS-TOTAL-INSPREM-NONMEMO PIC ZZZZZZZZ9.99.
           03  WS-LPAMTS-TOTAL-INSFINCHG       PIC ZZZZZZZZ9.99.
           03  WS-LPAMTS-TOTAL-FEEFINCHG       PIC ZZZZZZZZ9.99.   
           03  WS-LPAMTS-TOTAL-INS-TAX         PIC ZZZZZZZZ9.99.   

           03  WS-LPAMTS-PRINCIPLE              PIC ZZZZZZZZ9.99.   
           03  WS-LPAMTS-FINANCED-AMOUNT        PIC ZZZZZZZZ9.99.   
           03  WS-LPAMTS-DISCLOSED-FINAMT       PIC ZZZZZZZZ9.99.   
           03  WS-LPAMTS-INT-CHARGEABLE         PIC ZZZZZZZZ9.99.   

           03  WS-LPAMTS-NONMEMO-INS            PIC ZZZZZZZZ9.99.   
           03  WS-LPAMTS-NONMEMO-FEES           PIC ZZZZZZZZ9.99.   
           03  WS-LPAMTS-INSUR-OURS             PIC ZZZZZZZZ9.99.
           03  WS-LPAMTS-FEEOURS                PIC ZZZZZZZZ9.99.
           03  WS-LPAMTS-LIVE-CHECK-FEES        PIC ZZZZZZZZ9.99.


           03  WS-LPAMTS-CASH-ADVANCE     PIC ZZZZZZZZ9.99.   
           03  WS-LPAMTS-SPCASH-ADVANCE   PIC ZZZZZZZZ9.99.   

           03  WS-LPAMTS-HIGH-CREDIT      PIC ZZZZZZZ9.    


       01  WK-LPAMTS-IFN        PIC X(5)     VALUE "LPAMT".


       01  WS-LPLCHG-REC.
           03  WS-LPLCHG-LN-BRACCT       PIC Z(14).
           03  WS-LPLCHG-LN-ACCTNO       PIC Z(10).
           03  WS-LPLCHG-LN-OWNBR        PIC Z(6).
           03  WS-LPLCHG-LCHG-LATE       PIC ZZZZZZZZ9.99-.
           03  WS-LPLCHG-LCHG-CURPAY     PIC ZZZZZZZZ9.99-.
           03  WS-LPLCHG-LN-LCPDTH-DATE  PIC 99/99/9999.
           03  WS-LPLCHG-SP-LCRATE       PIC ZZZZZZ.9999.
      *THESE ARE FROM LPUPWK
      *USED FOR NON MONTHLY
           03  WS-LPLCHG-UPWK-UNITPER-CD   PIC X.
           03  WS-LPLCHG-UPWK-UNITPER-FREQ  PIC ZZZZ9.
           03  WS-LPLCHG-UPWK-LC-GRACE-FAC       PIC ZZZZ9.9(13).            
           03  WS-LPLCHG-UPWK-LC-MIN-FAC         PIC ZZZZ9.9(13).
           03  WS-LPLCHG-UPWK-LC-MAX-FAC         PIC ZZZZ9.9(13).
           03  WS-LPLCHG-UPWK-LC-PER-FAC         PIC ZZZZ9.9(13).
           03  WS-LPLCHG-UPWK-ERRCD              PIC X.                   
      *LPLCHG
           03  WS-LPLCHG-LCHG-WORK               PIC ZZZZZZZZ9.99-.
           03  WS-LPLCHG-LCHG-CHARGE             PIC ZZZZZZZZ9.99-.

       01  WK-LPLCHG-IFN        PIC X(4)     VALUE "LCHG".


       01  WS-SPPDTH-REC.
           03  WS-SPPDTH-LN-BRACCT       PIC Z(14).
           03  WS-SPPDTH-LN-ACCTNO       PIC Z(10).
           03  WS-SPPDTH-LN-OWNBR        PIC Z(6).
           03  WS-SPPDTH-PDTH-DATE-WORK  PIC 99/99/9999.
           03  WS-SPPDTH-PDTH-CREDIT     PIC ZZZ9.99.
           03  WS-SPPDTH-PDTH-DEF-FG     PIC X.
           03  WS-SPPDTH-PDTH-ALDEL-FG   PIC X.
           03  WS-SPPDTH-LN-TOTNOALDEL   PIC ZZZ9-.
           03  WS-SPPDTH-LN-TOTNODEF     PIC ZZZ9-.
      *WORKERS
           03  WS-SPPDTH-PDTH-PAYMNTD        PIC ZZZZZZZZ9.99-.
           03  WS-SPPDTH-PDTH-MONTHS         PIC ZZZZ9-.
           03  WS-SPPDTH-PDTH-YEARS          PIC ZZZ9.              
           03  WS-SPPDTH-PDTH-WORKER         PIC ZZZZZZZZ9.99-.    
      *USED TO LOOP THRU PAYMENT SCHEDULE
           03  WS-SPPDTH-PDTH-CNT            PIC ZZZZ9-.
      *USED TO LOOP THRU PAYMENT SCHEDULE
           03  WS-SPPDTH-PDTH-PDTH           PIC ZZZZ9-.
      *OUTPUT
           03  WS-SPPDTH-PDTH-DATE       PIC 99/99.               
           03  WS-SPPDTH-PDTH-DATE-FULL  PIC 99/99/9999.
           03  WS-SPPDTH-PDTH-LCPDTHRU   PIC Z.9(12). 
           03  WS-SPPDTH-PDTH-CUR-SCHD-DATE PIC 99/99/9999.
           03  WS-SPPDTH-PDTH-CUR-SCHD-PAY PIC ZZZZZZZZ9.99-.
           03  WS-SPPDTH-PDTH-NXT-SCHD-DATE PIC 99/99/9999.
           03  WS-SPPDTH-PDTH-NXT-SCHD-PAY  PIC ZZZZZZZZ9.99-.
           03  WS-SPPDTH-PDTH-PAYMENTS           PIC ZZZZ9.9(12).
           03  WS-SPPDTH-PDTH-AMT-TO-ADVANCE   PIC ZZZZZZZZ9.99-.
           03  WS-SPPDTH-PDTH-E-OPTION     PIC X.
           03  WS-SPPDTH-PDTH-E-1STPYAMT   PIC ZZZZZZZZ9.99-.
           03  WS-SPPDTH-PDTH-E-REGPYAMT   PIC ZZZZZZZZ9.99-.   
           03  WS-SPPDTH-PDTH-E-LASTPYAMT  PIC ZZZZZZZZ9.99-.    


       01  WK-SPPDTH-IFN        PIC X(6)     VALUE "SPPDTH".

       01  WS-LPCEPP-REC.
           03  WS-LPCEPP-LN-BRACCT       PIC Z(14).
           03  WS-LPCEPP-LN-ACCTNO       PIC Z(10).
           03  WS-LPCEPP-LN-OWNBR        PIC Z(6).
           03  WS-LPCEPP-CEPP1-DATE      PIC 99/99/9999.
           03  WS-LPCEPP-CEPP2-DATE      PIC 99/99/9999.
           03  WS-LPCEPP-LN-UNITPER-CD   PIC X.
           03  WS-LPCEPP-LN-UNITPER-FREQ  PIC ZZZZ9. 
           03  WS-LPCEPP-LN-TOTNODEF      PIC ZZZ9-.
           03  WS-LPCEPP-LN-TOTNOALDEL PIC ZZZ9-.           
           03  WS-LPCEPP-CEPP-INCDEF-FG     PIC X.
           03  WS-LPCEPP-CEPP-INCALDEL-FG   PIC X.
           03  WS-LPCEPP-CEPP-NOCAP-FG      PIC X.
           03  WS-LPCEPP-DATE-CAP                PIC XX.
           03  WS-LPCEPP-USE-DATE         PIC 99/99/9999.
      *IF NOT SPACES ELAPSED PERIODS NOT CAPPED AT ORIGINAL TERM
      *VALUE OTHER THAN SPACES DF/AD WILL BE CONSIDERED
           03  WS-LPCEPP-ELAPSED-DAYS      PIC ZZZZZZZZZZ9.
           03  WS-LPCEPP-CEPP-PERIODS       PIC ZZZZZZ9-.
           03  WS-LPCEPP-CEPP-UNITPER-REM   PIC ZZZZ9-.

       01  WK-LPCEPP-IFN        PIC X(6)     VALUE "LPCEPP".

       01  WS-LPIBPC-REC.
           03  WS-LPIBPC-LN-BRACCT                PIC Z(14).
           03  WS-LPIBPC-LN-ACCTNO                PIC Z(10).
           03  WS-LPIBPC-LN-OWNBR                 PIC Z(6).
           03  WS-LPIBPC-LN-DATE-PAID-LAST        PIC 99/99/9999.
           03  WS-LPIBPC-LN-JDDATE                PIC 99/99/9999.
           03  WS-LPIBPC-LN-ACCELDATE             PIC 99/99/9999.
           03  WS-LPIBPC-LN-REDUDATE              PIC 99/99/9999.
           03  WS-LPIBPC-LN-STATUS-FG             PIC X.
           03  WS-LPIBPC-LN-STATUSFLAGS           PIC X(20).
           03  WS-LPIBPC-LN-TOTNODEF              PIC ZZZ9.
           03  WS-LPIBPC-MDTE-ORGTERM-FG          PIC X.
           03  WS-LPIBPC-MDTE-FLAG                PIC X.
           03  WS-LPIBPC-SP-EXPD-FRMLA            PIC X.
           03  WS-LPIBPC-SP-EXPD-MONTHS           PIC ZZZ9.
           03  WS-LPIBPC-SP-EXPD-DAYS             PIC ZZZ9.
           03  WS-LPIBPC-MDTE-DATE                PIC 99/99/9999.
           03  WS-LPIBPC-EXPIR-DATE              PIC 99/99/9999.
           03  WS-LPIBPC-DATE                     PIC 99/99/9999.
           03  WS-LPIBPC-IBPC-FG                  PIC X.
           03  WS-LPIBPC-IBPC-STAT                PIC X.
           03  WS-LPIBPC-IBPC-STAT-2              PIC X.
           03  WS-LPIBPC-IBPC-EXPIRED-FLAG        PIC X.

       01  WK-LPIBPC-IFN        PIC X(4)     VALUE "IBPC".

       01  WS-LNCOST-REC.
           03  WS-LNCOST-LN-BRACCT                PIC Z(14).
           03  WS-LNCOST-LN-ACCTNO                PIC Z(10).
           03  WS-LNCOST-LN-OWNBR                 PIC Z(6).
           03  WS-LNCOST-SP1-KEY.
                  05  WS-LNCOST-ST                PIC X(2).
                  05  WS-LNCOST-CLASS             PIC 9(3).
                  05  WS-LNCOST-SUBCLASS          PIC 9(2).
                  05  WS-LNCOST-LAWCODE           PIC 9(2).
           03  WS-LNCOST-TYPE                     PIC X(15).
           03  WS-LNCOST-SP-FRMLA                 PIC X.
           03  WS-LNCOST-LNAMT                    PIC ZZZZZZZ9.99-.      
           03  WS-LNCOST-SP-AMT                   PIC ZZZZZZZ9.99-.
           03  WS-LNCOST-DLPARVHELD               PIC ZZZZZZZ9.99-.
           03  WS-LNCOST-DLPARVPAID               PIC ZZZZZZZ9.99-.
           03  WS-LNCOST-LN-LOANCOST              PIC ZZZZZZZ9.99-.

       01  WK-LNCOST-IFN        PIC X(6)     VALUE "LNCOST".

       01  WS-LPEARN-REC.
           03  WS-LPEARN-LN-BRACCT                PIC Z(14).
           03  WS-LPEARN-LN-ACCTNO                PIC Z(10).
           03  WS-LPEARN-LN-OWNBR                 PIC Z(6).
           03  WS-LPEARN-GP-INCL-OTH-IN-EARNED    PIC X.
           03  WS-LPEARN-LN-ANTICTBL.
               05  WS-LPEARN-LN-ORG-ANTICERN-1   PIC ZZZZZZZZ9.99-.
               05  WS-LPEARN-LN-ANTICERN-1       PIC ZZZZZZZZ9.99-.
               05  WS-LPEARN-LN-ANTICADJ-1       PIC ZZZZZZZZ9.99-.
               05  WS-LN-UFERN-1                 PIC ZZZZZZZZ9.99-.
               05  WS-LN-ACCERN-1                PIC ZZZZZZZZ9.99-.

               05  WS-LPEARN-LN-ORG-ANTICERN-2   PIC ZZZZZZZZ9.99-.
               05  WS-LPEARN-LN-ANTICERN-2       PIC ZZZZZZZZ9.99-.
               05  WS-LPEARN-LN-ANTICADJ-2       PIC ZZZZZZZZ9.99-.
               05  WS-LN-UFERN-2                 PIC ZZZZZZZZ9.99-.
               05  WS-LN-ACCERN-2                PIC ZZZZZZZZ9.99-.

               05  WS-LPEARN-LN-ORG-ANTICERN-3   PIC ZZZZZZZZ9.99-.
               05  WS-LPEARN-LN-ANTICERN-3       PIC ZZZZZZZZ9.99-.
               05  WS-LPEARN-LN-ANTICADJ-3       PIC ZZZZZZZZ9.99-.
               05  WS-LN-UFERN-3                 PIC ZZZZZZZZ9.99-.
               05  WS-LN-ACCERN-3                PIC ZZZZZZZZ9.99-.

               05  WS-LPEARN-LN-ORG-ANTICERN-4   PIC ZZZZZZZZ9.99-.
               05  WS-LPEARN-LN-ANTICERN-4       PIC ZZZZZZZZ9.99-.
               05  WS-LPEARN-LN-ANTICADJ-4       PIC ZZZZZZZZ9.99-.
               05  WS-LN-UFERN-4                 PIC ZZZZZZZZ9.99-.
               05  WS-LN-ACCERN-4                PIC ZZZZZZZZ9.99-.

               05  WS-LPEARN-LN-ORG-ANTICERN-5   PIC ZZZZZZZZ9.99-.
               05  WS-LPEARN-LN-ANTICERN-5       PIC ZZZZZZZZ9.99-.
               05  WS-LPEARN-LN-ANTICADJ-5       PIC ZZZZZZZZ9.99-.
               05  WS-LN-UFERN-5                 PIC ZZZZZZZZ9.99-.
               05  WS-LN-ACCERN-5                PIC ZZZZZZZZ9.99-.

               05  WS-LPEARN-LN-ORG-ANTICERN-6   PIC ZZZZZZZZ9.99-.
               05  WS-LPEARN-LN-ANTICERN-6       PIC ZZZZZZZZ9.99-.
               05  WS-LPEARN-LN-ANTICADJ-6       PIC ZZZZZZZZ9.99-.
               05  WS-LN-UFERN-6                 PIC ZZZZZZZZ9.99-.
               05  WS-LN-ACCERN-6                PIC ZZZZZZZZ9.99-.

           03  WS-LPEARN-LN-ANTICTBL-X REDEFINES  WS-LPEARN-LN-ANTICTBL
                                           OCCURS 6.
               05  WS-LPEARN-LN-ORG-ANTIC-IN  PIC ZZZZZZZZ9.99-.
               05  WS-LPEARN-LN-ANTICERN-IN   PIC ZZZZZZZZ9.99-.
               05  WS-LPEARN-LN-ANTICADJ-IN   PIC ZZZZZZZZ9.99-.
               05  WS-LPEARN-LN-UFERN-IN      PIC ZZZZZZZZ9.99-.
               05  WS-LPEARN-LN-ACCERN-IN     PIC ZZZZZZZZ9.99-.

           03  WS-LPEARN-DESC                     PIC X(9).
           03  WS-LPEARN-ANTICERN-DESC            PIC X(12).
           03  WS-LPEARN-EARN-TOT-ANTICERN        PIC ZZZZZZZZ9.99-.
           03  WS-LPEARN-ANTICADJ-DESC            PIC X(12).
           03  WS-LPEARN-EARN-TOT-ANTICADJ        PIC ZZZZZZZZ9.99-.
           03  WS-LPEARN-UFERN-DESC               PIC X(12).
           03  WS-LPEARN-EARN-TOT-UFERN           PIC ZZZZZZZ9.99-.
           03  WS-LPEARN-ACCERN-DESC              PIC X(12).
           03  WS-LPEARN-EARN-TOT-ACCERN          PIC ZZZZZZZ9.99-.

       01  WK-LPEARN-IFN        PIC X(6)     VALUE "LPEARN".


       01  WS-LPCDEL-REC.
           03  WS-LPCDEL-LN-BRACCT                PIC Z(14).
           03  WS-LPCDEL-LN-ACCTNO                PIC Z(10).
           03  WS-LPCDEL-LN-OWNBR                 PIC Z(6).
           03  WS-LPCDEL-SP-KEY                   PIC X(9).
           03  WS-LPCDEL-SP-CONTRFRMLA            PIC X.
           03  WS-LPCDEL-LN-TOTPAYMNTD            PIC ZZZZZZZZ9.99-.
           03  WS-LPCDEL-LN-TOTLCHG               PIC ZZZZZZZZ9.99-.
           03  WS-LPCDEL-LN-ADDON-FG              PIC X.
           03  WS-LPCDEL-LN-PAY-SCHLD-FG          PIC X.
           03  WS-LPCDEL-CDEL-LPCRNO-FG           PIC X.
      *LN-1STPYDATE
           03  WS-LPCDEL-CEPP1-DATE               PIC 99/99/9999.
      *TODAYS-DATE
           03  WS-LPCDEL-CEPP2-DATE               PIC 99/99/9999.

      *FIRST 5 OF PAYMENT SCHEDULE
%%         03  WS-LPCDEL-LTP-SCHLD-TERM-1          PIC ZZZZZ9.
           03  WS-LPCDEL-LTP-SCHLD-PAYMNT-1        PIC ZZZZZZZ9.99.

%%         03  WS-LPCDEL-LTP-SCHLD-TERM-2          PIC ZZZZZ9.
           03  WS-LPCDEL-LTP-SCHLD-PAYMNT-2        PIC ZZZZZZZ9.99.

%%         03  WS-LPCDEL-LTP-SCHLD-TERM-3          PIC ZZZZZ9.
           03  WS-LPCDEL-LTP-SCHLD-PAYMNT-3        PIC ZZZZZZZ9.99.

%%         03  WS-LPCDEL-LTP-SCHLD-TERM-4          PIC ZZZZZ9.
           03  WS-LPCDEL-LTP-SCHLD-PAYMNT-4        PIC ZZZZZZZ9.99.

%%         03  WS-LPCDEL-LTP-SCHLD-TERM-5          PIC ZZZZZ9.
           03  WS-LPCDEL-LTP-SCHLD-PAYMNT-5        PIC ZZZZZZZ9.99.
      *OUT
      *AMT TO ADVANCE PAID THRU
           03  WS-LPCDEL-CDEL-DELINQUENT          PIC ZZZZZZZZ9.99-.

      *PRINCIPAL DUE 
           03  WS-LPCDEL-CDEL-ARREARAGE           PIC ZZZZZZZZ9.99-.


       01  WK-LPCDEL-IFN        PIC X(6)     VALUE "LPCDEL".


       01  WS-LPLCAS-REC.
           03  WS-LPLCAS-LN-BRACCT                PIC Z(14).
           03  WS-LPLCAS-LN-ACCTNO                PIC Z(10).
           03  WS-LPLCAS-LN-OWNBR                 PIC Z(6).
      *TODAYS DATE
           03  WS-LPLCAS-LCAS-PAYDATE             PIC 99/99/9999.
           03  WS-LPLCAS-LCAS-1STPYDATE           PIC 99/99/9999.
      *LN-LCPDTH
           03  WS-LPLCAS-LCAS-LCPDTH-DATE         PIC 99/99/9999.
           03  WS-LPLCAS-NEXT-DUE-DATE            PIC 99/99/9999.
           03  WS-LPLCAS-LN-UNITPER-CD            PIC X.
      *OUTPUT
           03  WS-LPLCAS-PASS-HOLIDAY-TAB.
               05  WS-LPLCAS-PASS-HOLIDAY         PIC 9999 OCCURS
                                               6 TIMES.
           03  WS-LPLCAS-LCAS-LCGRACE             PIC ZZZZ9.       
           03  WS-LPLCAS-LCAS-GRACE-DATE          PIC 99/99/9999.
           03  WS-LPLCAS-LCAS-ELAPSED-DAYS        PIC ZZZZZZ9-.       
           03  WS-LPLCAS-LCAS-ASSESS              PIC X.

       01  WK-LPLCAS-IFN        PIC X(6)     VALUE "LPLCAS".


       01  WS-LPLCAP-REC.
           03  WS-LPLCAP-LN-BRACCT                PIC Z(14).
           03  WS-LPLCAP-LN-ACCTNO                PIC Z(10).
           03  WS-LPLCAP-LN-OWNBR                 PIC Z(6).
           03  WS-LPLCAP-LN-LCPDTH-DATE           PIC 99/99/9999. 
           03  WS-LPLCAP-LN-LCBAL                 PIC ZZZZZZZZ9.99-.
           03  WS-LPLCAP-LN-INTBAL                PIC ZZZZZZZZ9.99-.
           03  WS-LPLCAP-LN-OTHBAL                PIC ZZZZZZZZ9.99-.
           03  WS-LPLCAP-LN-OT2BAL                PIC ZZZZZZZZ9.99-.
           03  WS-LPLCAP-NOT-PERFORMED-MESS       PIC X(15).
           03  WS-LPLCAP-SP1-KEY                  PIC X(10).
           03  WS-LPLCAP-SP-LCFRMLA               PIC X.
           03  WS-LPLCAP-SP-LCTYPE                PIC X.
           03  WS-LPLCAP-SP-LCRATE                PIC ZZZZZ9.9999.
           03  WS-LPLCAP-LCAP-PAYDATE          PIC 99/99/9999.
           03  WS-LPLCAP-LCAP-1STPYDATE        PIC 99/99/9999.
           03  WS-LPLCAP-LCAP-LPTRCD           PIC X(2).
           03  WS-LPLCAP-LCAP-TRAMT            PIC ZZZZZZZZ9.99-.
           03  WS-LPLCAP-LCAP-LPAPLC               PIC ZZZZZZZZ9.99-.
           03  WS-LPLCAP-LCAP-LPAPINT              PIC ZZZZZZZZ9.99-.
      *OUTPUT
           03  WS-LPLCAP-LCAP-PARTIALS            PIC ZZZZZZZZ9.99-.
           03  WS-LPLCAP-LCAP-LCPAID              PIC ZZZZZZZZ9.99-.
           03  WS-LPLCAP-LCAP-NOLCHG              PIC ZZZZ9-.
           03  WS-LPLCAP-LCAP-APP                 PIC ZZZZZZZZ9.99-.
           03  WS-LPLCAP-LCAP-OWE                 PIC ZZZZZZZZ9.99-.
           03  WS-LPLCAP-LCAP-CALC-LCPDTH-DT      PIC 99/99/9999.
           03  WS-LPLCAP-CALC-LP-LCBAL            PIC ZZZZZZZZ9.99-.

       01  WK-LPLCAP-IFN        PIC X(6)     VALUE "LPLCAP".


       01  WS-LPPDUE-REC.
           03  WS-LPPDUE-LN-BRACCT                PIC Z(14).
           03  WS-LPPDUE-LN-ACCTNO                PIC Z(10).
           03  WS-LPPDUE-LN-OWNBR                 PIC Z(6).
           03  WS-LPPDUE-GP-PDTH-GT-SCHED-PAY     PIC X.
           03  WS-LPPDUE-PDUE-1STPYDATE           PIC 99/99/9999.
           03  WS-LPPDUE-PDUE-TODAY-DATE          PIC 99/99/9999.
           03  WS-LPPDUE-LN-UNITPER-CD            PIC X.

           03  WS-LPPDUE-MATURITY-DATE            PIC 99/99/9999.
           03  WS-LPPDUE-PDTH-DATE-FULL           PIC 99/99/9999.
           03  WS-LPPDUE-PDUE-DAYS                PIC ZZZZZZ9.
       01  WK-LPPDUE-IFN        PIC X(6)     VALUE "LPPDUE".

       01  WS-AGEING-REC.
           03  WS-AGEING-LN-BRACCT                PIC Z(14).
           03  WS-AGEING-LN-ACCTNO                PIC Z(10).
           03  WS-AGEING-LN-OWNBR                 PIC Z(6).
           03  WS-AGEING-AGEING-DATE              PIC 99/99/9999.
           03  WS-AGEING-SP1-KEY                   PIC X(9).
           03  WS-AGEING-SP-BANKRULE              PIC X.
           03  WS-AGEING-SP-CONTPDTH              PIC ZZZZZ9.
           03  WS-AGEING-SP-CONTR-CREDIT-CD       PIC X.
           03  WS-AGEING-SP-CONTR-CREDIT          PIC 99.99.
           03  WS-AGEING-LN-TOTPAYMNTD            PIC ZZZZZZZZ9.99-.
           03  WS-AGEING-LN-DATE-PAID-LAST        PIC 99/99/9999.
           03  WS-AGEING-LN-PY-MMYY-RECDEL        PIC 99/99.
           03  WS-AGEING-LN-MTD-PAYS-RECDEL       PIC ZZZZZZZZ9.99-.
           03  WS-AGEING-PDTH-PAYS                PIC ZZZZZZ9.
           03  WS-AGEING-PDTH-REMAIN              PIC .999999999999.
      *    03  WS-AGEING-WORK                     PIC ZZZZZZZZ9.99-.
           03  WS-PDTH-DATE-FULL                  PIC 99/99/9999.
      * RECENCY
           03  WS-AGEING-RDSUB                    PIC ZZZZZZZ9-.
           03  WS-AGEING-RSUB                     PIC ZZZZZZZ9-.
           03  WS-AGEING-RDPOT30                  PIC X.
           03  WS-AGEING-RDPOTCHGOFF              PIC X.
           03  WS-AGEING-RECENCY                  PIC ZZZZZZ9-.
      *RECENCY STATUS
           03  WS-AGEING-RECDEL                   PIC ZZZZZZ9-.
      *CONTRACTUAL
           03  WS-AGEING-CSUB                    PIC ZZZZZZZ9-.
      *CONTRACTUAL STATUS
           03  WS-AGEING-CONTRACTUAL              PIC ZZZZZZ9-.
           03  WS-AGEING-CPOT30                  PIC X.
           03  WS-AGEING-CPOTCHGOFF              PIC X.
           03  WS-AGEING-CSUB-ALDEL-FG           PIC X.
      *PAID AHEAD
           03  WS-AGEING-PSUB                    PIC ZZZZZZZ9-.
      *E-
           03  WS-AGEING-E-1STPYAMT              PIC ZZZZZZZZ9.99-.
           03  WS-AGEING-E-REGPYAMT              PIC ZZZZZZZZ9.99-.
           03  WS-AGEING-E-LASTPYAMT             PIC ZZZZZZZZ9.99-.
       01  WK-AGEING-IFN        PIC X(6)     VALUE "AGEING".

       01  HOLD-LP-TRAMT               PIC 9(7)V99.


       01  HOLD-SP1-KEY                PIC X(9)     VALUE SPACES.
       01  FILLER            REDEFINES HOLD-SP1-KEY.
           03  HOLD-SP-ORGST           PIC X(2).
           03  HOLD-SP-SPRCLASS        PIC 9(3).
           03  HOLD-SP-SUBCLASS        PIC 9(2).
           03  HOLD-SP-LAWCODE         PIC 9(2).

       01  HOLD-FIL-PATH.
           03  FILLER           PIC X(9).
           03  HOLD-FIL-MACHINE PIC X(2).
           03  FILLER           PIC X(7). 

       01  HOLD-PDTH-E-OPTION   PIC X.
       01  HOLD-NOCAP-FG        PIC X.
       01  HOLD-DATE-CAP        PIC X.
       01  HOLD-INCDEF-FG       PIC X.
       01  HOLD-INCALDEL-FG     PIC X.
       01  BW-SOLICIT           PIC X.
       01  HOLD-BR-LOAN-NUM-FLAG PIC X.

       01  SUB                  PIC 99.
       01  JUSTIFY-TYPE     PIC X VALUE "L".

       01  TODAYS-DATE      PIC 9(8).
 
       01  WS-BRACCT.
           03  WS-OWNBR     PIC Z(6).
           03  WS-NUMBER    PIC 9(6).
       01  WS-LN-ACCTNO  PIC 9(8).
       01  WS-LN-OWNBR   PIC 9(4).

       01  CR-1          PIC X VALUE X"0D".
      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99   COMP VALUE 0.
           03  LN-COUNT         PIC 999  COMP VALUE 99.
           03  LN-WK            PIC 999  COMP VALUE 0.
       01  PAGE-NUMBER          PIC 9(4) COMP VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
 
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".

       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  DISPLAY-BUF.
           03  FILLER           PIC X(12) VALUE "PROCESSING..".
           03  DISPLAY-BRNO     PIC Z(4).
           03  FILLER           PIC X(7).
           03  FILLER           PIC X(17) VALUE SPACES.

       01  TOTAL-RECORDS        PIC S9(13)  COMP-3 VALUE 0.
       01  TOTAL-BYPASSED       PIC S9(13)  COMP-3 VALUE 0.
       01  TOTAL-INCLUDED       PIC S9(13)  COMP-3 VALUE 0.
    
       01  CALCULATE-ORG-ANTIC-TBL-X.
           03  CALCULATED-ORG-ANTIC-1       PIC S9(7)V99.
           03  CALCULATED-ORG-ANTIC-2       PIC S9(7)V99.
           03  CALCULATED-ORG-ANTIC-3       PIC S9(7)V99.
           03  CALCULATED-ORG-ANTIC-4       PIC S9(7)V99.
           03  CALCULATED-ORG-ANTIC-5       PIC S9(7)V99.
           03  CALCULATED-ORG-ANTIC-6       PIC S9(7)V99.


       01  CALCULATED-ORG-ANTIC-TBL REDEFINES 
                              CALCULATE-ORG-ANTIC-TBL-X  OCCURS 6.
           03  CALCULATED-ORG-ANTIC   PIC S9(7)V99.         
    
       01  ORIGINAL-FIELDS.
           03  ORIGINAL-SP-EXPR-FRMLA       PIC X.
           03  ORIGINAL-SP-EXPD-FRMLA       PIC X.
           03  ORIGINAL-SP-EXPD-MONTHS      PIC 99.
           03  ORIGINAL-SP-EXPD-DAYS        PIC 99.
           03  ORIGINAL-LN-TOTNODEF         PIC S99.
           03  ORIGINAL-LN-TOTNOALDEL       PIC S99.
           03  ORIGINAL-SP-CONTR-CREDIT-CD  PIC X.
           03  ORIGINAL-SP-CONTR-CREDIT     PIC 99V99. 
           03  ORIGINAL-SP-LCMINCHG         PIC 99V99. 
           03  ORIGINAL-SP-LCMAXAMT         PIC 9999V99.
           03  ORIGINAL-GP-INCL-OTH-IN-EARNED PIC X.
           03  ORIGINAL-SP-LNCOST-AMT       PIC S9(7)V99.
           03  ORIGINAL-SP-LNCOST-FRMLA     PIC X.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR          PIC 9999.
           03  END-BR          PIC 9999.
           03  BEG-CLASS       PIC 999.
           03  END-CLASS       PIC 999.
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  BEG-ACCTNO      PIC 9(6).
           03  END-ACCTNO      PIC 9(6).
           03  INC-MATURITY    PIC X.
           03  INC-EXPIRATION  PIC X.
           03  INC-INSURANCE   PIC X.
           03  INC-LPUPER      PIC X.
           03  INC-LPAMTS      PIC X.
           03  INC-LPLCHG      PIC X.
           03  INC-SPPDTH      PIC X.
           03  INC-LPCEPP      PIC X.
           03  INC-LPIBPC      PIC X.
           03  INC-LPTBNO      PIC X.
           03  INC-CALL-TO-GLOBRD      PIC X.
           03  INC-LNCOST      PIC X.
           03  INC-LPEARN      PIC X.
           03  INC-LPCDEL      PIC X.
           03  INC-LPLCAS      PIC X.
           03  INC-LPLCAP      PIC X.
           03  INC-LPPDUE      PIC X.
           03  INC-AGEING      PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "B".

       01  SV-BEG-BR           PIC 9(4)   VALUE 9999.
       01  SV-END-BR           PIC 9(4)   VALUE 9999.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(13)    VALUE " ".

       01  HEAD3.
           03  FILLER           PIC X(9)    VALUE "BRANCH NO".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(13)   VALUE "TOTAL RECORDS".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(12)   VALUE "TOTAL LN AMT".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(11)   VALUE "TOTAL FILES".


       01  DETAIL-LINE          PIC X(80) VALUE SPACES.
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC X(2).
           03  D-NUMBER         PIC Z(6).
           03  FILLER           PIC X(2).
           03  D-NAME           PIC X(15).
           03  FILLER           PIC X(2).
           03  D-REPODATE       PIC 99/99/99.
           03  FILLER           PIC X(5).
           03  D-CURBAL         PIC Z(6).99.

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  D-TOTAL-BRNO     PIC Z(4).
           03  FILLER           PIC X(10).
           03  D-TOTAL-REC      PIC Z(7).
           03  FILLER           PIC X(20).
           03  D-TOTAL-AMT      PIC Z(9).99.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(5).
           03  RANGE1           PIC X(10).
           03  RANGE1N REDEFINES RANGE1 PIC 9(10).
           03  RANGE1D REDEFINES RANGE1 PIC ZZ99/99/99.
           03  FILLER           PIC X(2).
           03  RANGE2           PIC X(10).
           03  RANGE2N REDEFINES RANGE2 PIC 9(10).
           03  RANGE2D REDEFINES RANGE2 PIC ZZ99/99/99.

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPCEPPW.CPY". 
       COPY "LIBLP/LNCOSTW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUT/TESTR1_DEF.CPY".
       COPY "LIBUT/TESTR1_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/TESTR1_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-MATURITY-FILE WK-EXPIR-FILE
               WK-INSURANCE-FILE
               WK-LPUPER-FILE WK-LPAMTS-FILE WK-LPLCHG-FILE
               WK-SPPDTH-FILE WK-LPCEPP-FILE WK-LPIBPC-FILE
               WK-LPTBNO-FILE WK-GLOBRD-FILE WK-LNCOST-FILE
               WK-LPEARN-FILE WK-LPCDEL-FILE WK-LPLCAS-FILE
               WK-LPLCAP-FILE WK-LPPDUE-FILE WK-AGEING-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               PR-FILE WK-MATURITY-FILE WK-EXPIR-FILE
               WK-INSURANCE-FILE
               WK-LPUPER-FILE WK-LPAMTS-FILE WK-LPLCHG-FILE
               WK-SPPDTH-FILE WK-LPCEPP-FILE WK-LPIBPC-FILE
               WK-LPTBNO-FILE WK-GLOBRD-FILE WK-LNCOST-FILE
               WK-LPEARN-FILE WK-LPCDEL-FILE WK-LPLCAS-FILE
               WK-LPLCAP-FILE WK-LPPDUE-FILE WK-AGEING-FILE.

       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      *************************************
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TESTR1" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
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
           PERFORM NPR-TRAP-INTERRUPT.

           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BR
              GO TO END-ROUTINE.
       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.
           MOVE "B"        TO LN-PATH-OVERRIDE
                              GB-PATH-OVERRIDE
                             LTP-PATH-OVERRIDE.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO END-OF-GROUP.

           PERFORM LOAD-LN3-FILE.
           PERFORM LOAD-LTP1-FILE.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-LTP1-FILE.


           PERFORM GLOBAL-UPDATE.

           PERFORM CLOSE-LN3-FILE.

       END-OF-GROUP.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      **************************
       GLOBAL-UPDATE SECTION.
      **************************
           IF EXT-ROUTE-BUF = "00"
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE BR-NO TO DISPLAY-BRNO
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE MESS-LIST   TO WR-LIST
                 PERFORM CALL-WRFORM.

           MOVE BR-NO TO LN-OWNBR.
           MOVE 0     TO LN-CLASS LN-ACCTNO.
           PERFORM START-LN3-FILE.
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
*L2G       IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO EXIT-GLOBAL-UPDATE.
           IF LN-ACCTNO = 75150 
      D    STOP MESS
             MOVE SPACES TO MESS.

           ADD 1 TO TOTAL-RECORDS.

           IF LN-OWNBR > END-BR
              GO TO EXIT-GLOBAL-UPDATE.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.

           IF LN-LOANDATE < BEG-DATE OR LN-LOANDATE > END-DATE
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.

           IF LN-ACCTNO < BEG-ACCTNO OR LN-ACCTNO > END-ACCTNO
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.
           
           IF LN-POCD-NONLOAN
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.

      * EXCLUDE ZERO BALANCES
           IF LN-CURBAL = 0 AND LN-INTBAL = 0 AND LN-LCBAL = 0
               AND LN-OTHBAL = 0
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.

      *CHECK FOR NO PAYOFF DATE ON PL.
           IF LN-PLDATE NOT = ZEROS
              IF LN-POFFDATE NOT = 0
                 ADD 1 TO TOTAL-BYPASSED
                 GO TO NEXT-LN.

      * EXCLUDE PL < 01/01/2010
           IF LN-PLDATE NOT = ZEROS
              IF LN-PLDATE NOT > 20100101
                 ADD 1 TO  TOTAL-BYPASSED
                 GO TO NEXT-LN.

       GET-SPR.
           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.
           PERFORM READ-SP1-FILE
           IF IO-FG NOT = 0
               ADD 1 TO TOTAL-BYPASSED
               GO TO NEXT-LN.
           MOVE SP1-KEY TO HOLD-SP1-KEY.           

           IF INC-CALL-TO-GLOBRD = "Y"
              IF NOT (SP-LCGRACECD = "W" OR "S")
                 NEXT SENTENCE 
              ELSE
                 PERFORM CREATE-GLOBRD-WORK.

           PERFORM SETUP-ORIGINAL-HOLD-FIELDS.
           PERFORM SAVE-ORIGINAL-FIELDS.
          
           IF INC-LPTBNO = "Y"
              PERFORM CREATE-LPTBNO-WORK.
           IF INC-MATURITY = "Y" 
              PERFORM CREATE-MATURITY-WORK.  
           IF INC-EXPIRATION = "Y"
              PERFORM CREATE-EXPIR-WORK.
           IF INC-INSURANCE = "Y"
              IF LN-INSCODES NOT = "NNN"
                 PERFORM CREATE-INSURANCE-WORK.
           IF INC-LPUPER = "Y"
               PERFORM CREATE-LPUPER-WORK.
           IF INC-LPAMTS = "Y"
               PERFORM CREATE-LPAMTS-WORK.
           IF INC-LPLCHG = "Y"
               PERFORM CREATE-LPLCHG-WORK.
           IF INC-SPPDTH = "Y"
               MOVE SPACES TO HOLD-PDTH-E-OPTION 
               IF SP-BANKRULE = "E"
                  MOVE "E" TO HOLD-PDTH-E-OPTION 
               END-IF
               PERFORM CREATE-SPPDTH-WORK.
           IF INC-LPCEPP = "Y"
               PERFORM CREATE-LPCEPP-WORK.
           IF INC-LPIBPC   = "Y"
               PERFORM CREATE-LPIBPC-WORK.
           IF INC-LNCOST = "Y"
               PERFORM CREATE-LNCOST-WORK.
           IF INC-LPEARN = "Y"
               PERFORM CREATE-LPEARN-WORK.
           IF INC-LPCDEL= "Y"
               PERFORM CREATE-LPCDEL-WORK.
           IF INC-LPLCAS = "Y"
               PERFORM CREATE-LPLCAS-WORK.
           IF INC-LPLCAP = "Y"
              PERFORM CREATE-LPLCAP-WORK.
      *PAST DUE DAYS CALC
           IF INC-LPPDUE = "Y"
              PERFORM CREATE-LPPDUE-WORK.
           IF INC-AGEING = "Y"
              PERFORM CREATE-AGEING-WORK.
      

           GO TO NEXT-LN.

       EXIT-GLOBAL-UPDATE.
           EXIT.

      /**********************************
      *   I N I T I A L I Z A T I O N   *
      ***********************************
       INITIALIZATION SECTION.
           MOVE TESTR1-QUEUE-POS TO Q-POS.
           MOVE TESTR1-COPIES-POS TO C-POS.
           MOVE TESTR1-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.

           PERFORM OPEN-PR-FILE.
           PERFORM ENABLE-INTERRUPT.

           IF BEG-BR = 0
              IF END-BR = 0
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                             END-BR.

           MOVE 0 TO TOTAL-RECORDS.

           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD  TO TODAYS-DATE.

           MOVE "FIL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO  HOLD-FIL-PATH.
           IF STAT NOT = "00"
              GO TO IO-ERROR. 


           MOVE HOLD-FIL-MACHINE TO WK-MATURITY-MACHINE 
                                    WK-EXPIR-MACHINE
                                    WK-INS-MACHINE
                                    WK-LPUPER-MACHINE
                                    WK-LPAMTS-MACHINE
                                    WK-LPLCHG-MACHINE
                                    WK-SPPDTH-MACHINE
                                    WK-LPCEPP-MACHINE 
                                    WK-LPIBPC-MACHINE
                                    WK-LPTBNO-MACHINE
                                    WK-GLOBRD-MACHINE
                                    WK-LNCOST-MACHINE
                                    WK-LPEARN-MACHINE
                                    WK-LPCDEL-MACHINE
                                    WK-LPLCAS-MACHINE
                                    WK-LPLCAP-MACHINE
                                    WK-LPPDUE-MACHINE
                                    WK-AGEING-MACHINE.

           MOVE TODAYS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY-DD TO WK-GLOBRD-PATH-DA.
           MOVE DATE-MMDDYY-MM TO WK-GLOBRD-PATH-MO.
           MOVE DATE-MMDDYY-YY TO WK-GLOBRD-PATH-YR.

           CALL "C$TOLOWER" USING WK-GLOBRD-PATH1,VALUE 12.
           IF INC-CALL-TO-GLOBRD = "Y"
               PERFORM OPEN-WK-GLOBRD-FILE-OUTPUT.


           MOVE TODAYS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY-DD TO WK-LPTBNO-PATH-DA.
           MOVE DATE-MMDDYY-MM TO WK-LPTBNO-PATH-MO.
           MOVE DATE-MMDDYY-YY TO WK-LPTBNO-PATH-YR.

           CALL "C$TOLOWER" USING WK-LPTBNO-PATH1,VALUE 12.
           IF INC-LPTBNO = "Y"
               PERFORM OPEN-WK-LPTBNO-FILE-OUTPUT.


           MOVE TODAYS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY-DD TO WK-MATURITY-PATH-DA.
           MOVE DATE-MMDDYY-MM TO WK-MATURITY-PATH-MO.
           MOVE DATE-MMDDYY-YY TO WK-MATURITY-PATH-YR.

           CALL "C$TOLOWER" USING WK-MATURITY-PATH1,VALUE 12.
           IF INC-MATURITY = "Y"
               PERFORM OPEN-WK-MATURITY-FILE-OUTPUT.


           MOVE TODAYS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY-DD TO WK-EXPIR-PATH-DA.
           MOVE DATE-MMDDYY-MM TO WK-EXPIR-PATH-MO.
           MOVE DATE-MMDDYY-YY TO WK-EXPIR-PATH-YR.

           CALL "C$TOLOWER" USING WK-EXPIR-PATH1,VALUE 12.
           IF INC-EXPIRATION = "Y"
              PERFORM OPEN-WK-EXPIR-FILE-OUTPUT.


           MOVE TODAYS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY-DD TO WK-INS-PATH-DA.
           MOVE DATE-MMDDYY-MM TO WK-INS-PATH-MO.
           MOVE DATE-MMDDYY-YY TO WK-INS-PATH-YR.

           CALL "C$TOLOWER" USING WK-INS-PATH1,VALUE 12.
           IF INC-INSURANCE = "Y"
              PERFORM OPEN-WK-INSURANCE-FILE-OUTPUT.


           IF INC-LPUPER    = "Y"
              MOVE TODAYS-DATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY-DD TO WK-LPUPER-PATH-DA 
              MOVE DATE-MMDDYY-MM TO WK-LPUPER-PATH-MO 
              MOVE DATE-MMDDYY-YY TO WK-LPUPER-PATH-YR 
              CALL "C$TOLOWER" USING WK-LPUPER-PATH1,VALUE 12 
                  PERFORM OPEN-WK-LPUPER-FILE-OUTPUT.

           IF INC-LPAMTS    = "Y"
              MOVE TODAYS-DATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY-DD TO WK-LPAMTS-PATH-DA 
              MOVE DATE-MMDDYY-MM TO WK-LPAMTS-PATH-MO 
              MOVE DATE-MMDDYY-YY TO WK-LPAMTS-PATH-YR 
              CALL "C$TOLOWER" USING WK-LPAMTS-PATH1,VALUE 12 
              PERFORM OPEN-WK-LPAMTS-FILE-OUTPUT.


           IF INC-LPLCHG   = "Y"
              MOVE TODAYS-DATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY-DD TO WK-LPLCHG-PATH-DA 
              MOVE DATE-MMDDYY-MM TO WK-LPLCHG-PATH-MO 
              MOVE DATE-MMDDYY-YY TO WK-LPLCHG-PATH-YR  
              CALL "C$TOLOWER" USING WK-LPLCHG-PATH1,VALUE 12 
              PERFORM OPEN-WK-LPLCHG-FILE-OUTPUT.


           IF INC-SPPDTH   = "Y"
              MOVE TODAYS-DATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY-DD TO WK-SPPDTH-PATH-DA 
              MOVE DATE-MMDDYY-MM TO WK-SPPDTH-PATH-MO 
              MOVE DATE-MMDDYY-YY TO WK-SPPDTH-PATH-YR 
              CALL "C$TOLOWER" USING WK-SPPDTH-PATH1,VALUE 12 
              PERFORM OPEN-WK-SPPDTH-FILE-OUTPUT.


           IF INC-LPCEPP   = "Y"
              MOVE TODAYS-DATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY-DD TO WK-LPCEPP-PATH-DA 
              MOVE DATE-MMDDYY-MM TO WK-LPCEPP-PATH-MO 
              MOVE DATE-MMDDYY-YY TO WK-LPCEPP-PATH-YR 
              CALL "C$TOLOWER" USING WK-LPCEPP-PATH1,VALUE 12 
              PERFORM OPEN-WK-LPCEPP-FILE-OUTPUT.

           IF INC-LPIBPC   = "Y"
              MOVE TODAYS-DATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY-DD TO WK-LPIBPC-PATH-DA 
              MOVE DATE-MMDDYY-MM TO WK-LPIBPC-PATH-MO 
              MOVE DATE-MMDDYY-YY TO WK-LPIBPC-PATH-YR 
              CALL "C$TOLOWER" USING WK-LPIBPC-PATH1,VALUE 12 
              PERFORM OPEN-WK-LPIBPC-FILE-OUTPUT.

           IF INC-LNCOST   = "Y"
              MOVE TODAYS-DATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY-DD TO WK-LNCOST-PATH-DA 
              MOVE DATE-MMDDYY-MM TO WK-LNCOST-PATH-MO 
              MOVE DATE-MMDDYY-YY TO WK-LNCOST-PATH-YR 
              CALL "C$TOLOWER" USING WK-LNCOST-PATH1,VALUE 12 
              PERFORM OPEN-WK-LNCOST-FILE-OUTPUT.

           IF INC-LPEARN   = "Y"
              MOVE TODAYS-DATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY-DD TO WK-LPEARN-PATH-DA 
              MOVE DATE-MMDDYY-MM TO WK-LPEARN-PATH-MO 
              MOVE DATE-MMDDYY-YY TO WK-LPEARN-PATH-YR 
              CALL "C$TOLOWER" USING WK-LPEARN-PATH1,VALUE 12 
              PERFORM OPEN-WK-LPEARN-FILE-OUTPUT.

           IF INC-LPCDEL   = "Y"
              MOVE TODAYS-DATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY-DD TO WK-LPCDEL-PATH-DA 
              MOVE DATE-MMDDYY-MM TO WK-LPCDEL-PATH-MO 
              MOVE DATE-MMDDYY-YY TO WK-LPCDEL-PATH-YR 
              CALL "C$TOLOWER" USING WK-LPCDEL-PATH1,VALUE 12 
              PERFORM OPEN-WK-LPCDEL-FILE-OUTPUT.


           IF INC-LPLCAS   = "Y"
              MOVE TODAYS-DATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY-DD TO WK-LPLCAS-PATH-DA 
              MOVE DATE-MMDDYY-MM TO WK-LPLCAS-PATH-MO 
              MOVE DATE-MMDDYY-YY TO WK-LPLCAS-PATH-YR 
              CALL "C$TOLOWER" USING WK-LPLCAS-PATH1,VALUE 12 
              PERFORM OPEN-WK-LPLCAS-FILE-OUTPUT.

           IF INC-LPLCAP   = "Y"
              MOVE TODAYS-DATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY-DD TO WK-LPLCAP-PATH-DA 
              MOVE DATE-MMDDYY-MM TO WK-LPLCAP-PATH-MO 
              MOVE DATE-MMDDYY-YY TO WK-LPLCAP-PATH-YR 
              CALL "C$TOLOWER" USING WK-LPLCAP-PATH1,VALUE 12 
              PERFORM OPEN-WK-LPLCAP-FILE-OUTPUT.

           IF INC-LPPDUE   = "Y"
              MOVE TODAYS-DATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY-DD TO WK-LPPDUE-PATH-DA 
              MOVE DATE-MMDDYY-MM TO WK-LPPDUE-PATH-MO 
              MOVE DATE-MMDDYY-YY TO WK-LPPDUE-PATH-YR 
              CALL "C$TOLOWER" USING WK-LPPDUE-PATH1,VALUE 12 
              PERFORM OPEN-WK-LPPDUE-FILE-OUTPUT.

           IF INC-AGEING   = "Y"
              MOVE TODAYS-DATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY-DD TO WK-AGEING-PATH-DA 
              MOVE DATE-MMDDYY-MM TO WK-AGEING-PATH-MO 
              MOVE DATE-MMDDYY-YY TO WK-AGEING-PATH-YR 
              CALL "C$TOLOWER" USING WK-AGEING-PATH1,VALUE 12 
              PERFORM OPEN-WK-AGEING-FILE-OUTPUT.




      *-----------------------------------------------
           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.


      ****************************************
      *    SETUP DATES FOR BATCH PROCESS
      ****************************************
       EO-EXEC-AUDIT SECTION.
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ****************************************
      *       ENTER REPORT PARAMETERS
      ****************************************
       ENTER-PARAMETERS SECTION.
       ENTER-PARM.
      * FOR RE-ENTRY ON F4 FROM ACCEPT.

       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
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
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO CL-01.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO CL-01.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO CL-01.
           GO TO BEGDATE-01.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       BEGDATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "16" GO TO ALL-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDATE-01.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
      *-----------------------------------------------------------
       ENDDATE-02.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGDATE-01.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO ENDDATE-02.

           MOVE VDU-DATE-YYYYMMDD  TO END-DATE.

      * MAKE SURE THE DATE RANGE IS VALID.

           IF BEG-DATE NOT = 0
              MOVE BEG-DATE  TO NUM-DATE
              MOVE END-DATE TO SYS-DATE
              PERFORM CMPDAT
              IF SYS-IS-LT
                 MOVE "INVALID DATE RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO BEGDATE-01.

           GO TO ENTER-BEG-ACCTNO.            

      *----------------------------------------------------------
       ALL-DATE.

           MOVE "S  8080DATE1." TO VDU.
           MOVE JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DATE2."   TO VDU.
           MOVE JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------
       ENTER-BEG-ACCTNO.         
           MOVE "ENNN060ACCT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "16" GO TO ENTER-ALL-ACCTNO 
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO ENDDATE-02 
              ELSE
                 GO TO CL-02.
           MOVE VDUNUM0 TO BEG-ACCTNO.

      *----------------------------------------------------------
       ENTER-END-ACCTNO.
           MOVE "ENNN060ACCT2." TO VDU.
           PERFORM SCREENIO.
           
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
               GO TO ENTER-BEG-ACCTNO.
           MOVE VDUNUM0 TO END-ACCTNO.
           GO TO ENTER-MATURITY.
      *-----------------------------------------------------------
       ENTER-ALL-ACCTNO.
           MOVE "SNNN060ACCT1." TO VDU.
           MOVE 0 TO BEG-ACCTNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060ACCT2." TO VDU.
           MOVE 999999 TO END-ACCTNO VDUNUM0.
           PERFORM SCREENIO.          
      *----------------------------------------------------------
       ENTER-MATURITY.
           MOVE "E  A010MATURITY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-END-ACCTNO.

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-MATURITY.

           MOVE VDUBUF TO INC-MATURITY.

      *----------------------------------------------------------
       ENTER-EXPIRATION.
           MOVE "E  A010EXPIRATION." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-EXPIRATION.   

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-EXPIRATION.

           MOVE VDUBUF TO INC-EXPIRATION.

      *----------------------------------------------------------
       ENTER-INSURANCE.
           MOVE "E  A010INSURANCE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-EXPIRATION.   

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-INSURANCE.   

           MOVE VDUBUF TO INC-INSURANCE.   


      *----------------------------------------------------------
       ENTER-LPUPER.   
           MOVE "E  A010LPUPER." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-LPUPER.       

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-LPUPER.      

           MOVE VDUBUF TO INC-LPUPER.          

      *----------------------------------------------------------
       ENTER-LPAMTS.   
           MOVE "E  A010LPAMTS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-LPUPER.       

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-LPUPER.      

           MOVE VDUBUF TO INC-LPAMTS.          

      *----------------------------------------------------------
       ENTER-LPLCHG.   
           MOVE "E  A010LPLCHG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-LPAMTS.       

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-LPLCHG.      

           MOVE VDUBUF TO INC-LPLCHG.
      *----------------------------------------------------------
       ENTER-SPPDTH.   
           MOVE "E  A010SPPDTH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-LPLCHG.       

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-SPPDTH.      

           MOVE VDUBUF TO INC-SPPDTH.                  

      *----------------------------------------------------------
       ENTER-LPCEPP.   
           MOVE "E  A010LPCEPP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-SPPDTH.       

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-LPCEPP.      

           MOVE VDUBUF TO INC-LPCEPP.                  

      *----------------------------------------------------------
       ENTER-LPIBPC.   
           MOVE "E  A010LPIBPC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-LPCEPP.       

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-LPIBPC.      

           MOVE VDUBUF TO INC-LPIBPC.                  

      *----------------------------------------------------------
       ENTER-LPTBNO.   
           MOVE "E  A010LPTBNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-LPIBPC.       

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-LPTBNO.      

           MOVE VDUBUF TO INC-LPTBNO.                  

      *----------------------------------------------------------
       ENTER-INC-CALL-TO-GLOBRD.
           MOVE "E  A010GLOBRD." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-LPTBNO.   

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-INC-CALL-TO-GLOBRD.

           MOVE VDUBUF TO INC-CALL-TO-GLOBRD.          

      *----------------------------------------------------------
       ENTER-LNCOST.   
           MOVE "E  A010LNCOST." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-INC-CALL-TO-GLOBRD.

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-LNCOST.      

           MOVE VDUBUF TO INC-LNCOST.                  

      *----------------------------------------------------------
       ENTER-LPEARN.   
           MOVE "E  A010LPEARN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-LNCOST.       

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-LPEARN.      

           MOVE VDUBUF TO INC-LPEARN.                  

      *----------------------------------------------------------
       ENTER-LPCDEL.   
           MOVE "E  A010LPCDEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-LPEARN.       

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-LPCDEL.      

           MOVE VDUBUF TO INC-LPCDEL.                  
      *----------------------------------------------------------
       ENTER-LPLCAS.   
           MOVE "E  A010LPLCAS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-LPCDEL.       

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-LPLCAS.      

           MOVE VDUBUF TO INC-LPLCAS.                  

      *----------------------------------------------------------
       ENTER-LPLCAP.   
           MOVE "E  A010LPLCAP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-LPLCAS.       

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-LPLCAP.      

           MOVE VDUBUF TO INC-LPLCAP.                  

      *----------------------------------------------------------
       ENTER-LPPDUE.   
           MOVE "E  A010LPPDUE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-LPLCAS.       

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-LPPDUE.      

           MOVE VDUBUF TO INC-LPPDUE.                  

      *----------------------------------------------------------
       ENTER-AGEING.   
           MOVE "E  A010AGEING." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-LPPDUE.       

           IF NOT (VDUBUF =  "Y" OR "N")
              GO TO ENTER-AGEING.      

           MOVE VDUBUF TO INC-AGEING.                  


      *============================================
       EXIT-PARM.
           EXIT.

      ****************************************
      *       DISPLAY REPORT PARAMETERS
      ****************************************
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
              MOVE BEG-BR    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR    TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SN N020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
    
       CREATE-GLOBRD-WORK SECTION.
           PERFORM SET-COMMON.  
           MOVE SPACES TO WK-GLOBRD-REC WS-GLOBRD-REC.
           MOVE WS-BRACCT TO WS-GLOBRD-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-GLOBRD-LN-BRACCT,JUSTIFY-TYPE.       

           MOVE WS-LN-ACCTNO TO WS-GLOBRD-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-GLOBRD-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-GLOBRD-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-GLOBRD-LN-OWNBR, JUSTIFY-TYPE.
           
           MOVE SP1-KEY         TO WS-GLOBRD-LN-SP1-KEY.
           PERFORM UPDATE-GLOBRD-WORK.

        UPDATE-GLOBRD-WORK SECTION.
           IF SP-LCGRACECD = "W" OR "S"
              MOVE GB-HOLIDAY-TAB TO WS-GLOBRD-HOLIDAYS.
           STRING
              WS-GLOBRD-LN-BRACCT           ,"|"
              WS-GLOBRD-LN-ACCTNO           ,"|"
              WS-GLOBRD-LN-OWNBR            ,"|"
              WS-GLOBRD-LN-SP1-KEY          ,"|"
              WS-GLOBRD-HOLIDAY-1           ,"|"
              WS-GLOBRD-HOLIDAY-2           ,"|"
              WS-GLOBRD-HOLIDAY-3           ,"|"
              WS-GLOBRD-HOLIDAY-4           ,"|"
              WS-GLOBRD-HOLIDAY-5           ,"|"
              WS-GLOBRD-HOLIDAY-6           ,"|"
              WS-GLOBRD-HOLIDAY-7           ,"|"
              WS-GLOBRD-HOLIDAY-8           ,"|"
              WS-GLOBRD-HOLIDAY-9           ,"|"
              WS-GLOBRD-HOLIDAY-10          ,"|"
              WS-GLOBRD-HOLIDAY-11          ,"|"
              WS-GLOBRD-HOLIDAY-12          ,"|"
              WS-GLOBRD-HOLIDAY-13          ,"|"
              WS-GLOBRD-HOLIDAY-14          ,"|"
              WS-GLOBRD-HOLIDAY-15          ,"|"
              WS-GLOBRD-HOLIDAY-16,CR-1     
                 DELIMITED BY "  " INTO WK-GLOBRD-REC.
           PERFORM WRITE-WK-GLOBRD-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

       CREATE-LPTBNO-WORK SECTION.
           PERFORM SET-COMMON.
           MOVE SPACES TO WK-LPTBNO-REC WS-LPTBNO-REC.
           MOVE WS-BRACCT TO WS-LPTBNO-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPTBNO-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPTBNO-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPTBNO-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-LPTBNO-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPTBNO-LN-OWNBR, JUSTIFY-TYPE.

           MOVE SP1-KEY         TO WS-LPTBNO-LN-SP1-KEY.
           PERFORM UPDATE-LPTBNO-WORK.

        UPDATE-LPTBNO-WORK SECTION.
           PERFORM COMPUTE-SP-TABLE-NUMS.
           MOVE SP-CAL-NORATES(1)   TO WS-LPTBNO-SP-CAL-NORATES-1.
           CALL "C$JUSTIFY" USING WS-LPTBNO-SP-CAL-NORATES-1,
                                               JUSTIFY-TYPE.
           MOVE SP-CAL-NORATES(2)   TO WS-LPTBNO-SP-CAL-NORATES-2.
           CALL "C$JUSTIFY" USING WS-LPTBNO-SP-CAL-NORATES-2,
                                               JUSTIFY-TYPE.
           
           MOVE SP-EDT-NOEDITS(1)      TO WS-LPTBNO-SP-EDT-NOEDITS-1.
           CALL "C$JUSTIFY" USING WS-LPTBNO-SP-EDT-NOEDITS-1,
                                                        JUSTIFY-TYPE.

           MOVE SP-EDT-NOEDITS(2)      TO WS-LPTBNO-SP-EDT-NOEDITS-2.
           CALL "C$JUSTIFY" USING WS-LPTBNO-SP-EDT-NOEDITS-2,
                                                        JUSTIFY-TYPE.

           MOVE SP-INSNOVER(1)         TO  WS-LPTBNO-SP-INSNOVER-1.
           CALL "C$JUSTIFY" USING WS-LPTBNO-SP-INSNOVER-1,
                                                JUSTIFY-TYPE.

           MOVE SP-INSNOVER(2)         TO  WS-LPTBNO-SP-INSNOVER-2
           CALL "C$JUSTIFY" USING WS-LPTBNO-SP-INSNOVER-2,
                                                JUSTIFY-TYPE.

           MOVE SP-INSNOVER(3)         TO  WS-LPTBNO-SP-INSNOVER-3.
           CALL "C$JUSTIFY" USING WS-LPTBNO-SP-INSNOVER-3,
                                                JUSTIFY-TYPE.

           MOVE SP-INSNOVER(4)         TO  WS-LPTBNO-SP-INSNOVER-4.
           CALL "C$JUSTIFY" USING WS-LPTBNO-SP-INSNOVER-4,
                                                JUSTIFY-TYPE.

           STRING
              WS-LPTBNO-LN-BRACCT           ,"|"
              WS-LPTBNO-LN-ACCTNO           ,"|"
              WS-LPTBNO-LN-OWNBR            ,"|"
              WS-LPTBNO-LN-SP1-KEY          ,"|"
              WS-LPTBNO-SP-CAL-NORATES-1    ,"|"
              WS-LPTBNO-SP-CAL-NORATES-2    ,"|"
              WS-LPTBNO-SP-EDT-NOEDITS-1    ,"|"
              WS-LPTBNO-SP-EDT-NOEDITS-2    ,"|"
              WS-LPTBNO-SP-INSNOVER-1       ,"|"
              WS-LPTBNO-SP-INSNOVER-2       ,"|"
              WS-LPTBNO-SP-INSNOVER-3       ,"|"
              WS-LPTBNO-SP-INSNOVER-4,  CR-1
                 DELIMITED BY "  " INTO WK-LPTBNO-REC.

           PERFORM WRITE-WK-LPTBNO-FILE.
 
           IF IO-BAD
              GO TO IO-ERROR.

      ****************************
       CREATE-MATURITY-WORK SECTION.
      ****************************
           PERFORM SET-COMMON.  
      *" Y " UPDATED MATURITY, ORG TERM, DC AFFECT MATURITY
           MOVE SPACES TO WK-MATURITY-REC WS-MATURITY-REC.
      *SAVE THIS BECAUSE IT NEEDS RESTORED TO THE LOAN VALUES FOR NEXT
      *COPY MEMBER TEST.

           MOVE WS-BRACCT TO WS-MATURITY-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-BRACCT,JUSTIFY-TYPE.       

           MOVE WS-LN-ACCTNO TO WS-MATURITY-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-MATURITY-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-OWNBR, JUSTIFY-TYPE.
      

           MOVE " "                TO 
                                WS-MATURITY-MDTE-FLAG MDTE-FLAG.
           MOVE "Y"               TO WS-MATURITY-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE " "                TO WS-MATURITY-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 3                  TO LN-TOTNODEF.

      
%%         PERFORM UPDATE-MATURITY-WORK-REC.


      *"YY " ORG MATURITY, ORG TERM, DC AFFECT MATURITY
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-MATURITY-REC WS-MATURITY-REC.

           MOVE WS-BRACCT TO WS-MATURITY-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-MATURITY-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-ACCTNO,JUSTIFY-TYPE.
           MOVE WS-LN-OWNBR     TO WS-MATURITY-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-OWNBR, JUSTIFY-TYPE.
        
           MOVE "Y"                TO WS-MATURITY-MDTE-FLAG
                                                    MDTE-FLAG.
           MOVE "Y"              TO WS-MATURITY-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE " "                TO WS-MATURITY-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.

           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-MATURITY-WORK-REC.                               
 

      *"Y Y" ORG MATURITY, SUM TERM, DC DO NOT AFFECT MATURITY
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-MATURITY-REC WS-MATURITY-REC.
           MOVE WS-BRACCT TO WS-MATURITY-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-MATURITY-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-ACCTNO,JUSTIFY-TYPE.
           MOVE WS-LN-OWNBR     TO WS-MATURITY-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-OWNBR, JUSTIFY-TYPE.
        
           MOVE "Y"                TO WS-MATURITY-MDTE-FLAG
                                                    MDTE-FLAG.
           MOVE " "              TO WS-MATURITY-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE "Y"                TO WS-MATURITY-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-MATURITY-WORK-REC.                               
 

      *"   " UPD MATURITY, SUM TERM, DC AFFECT MATURITY
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-MATURITY-REC WS-MATURITY-REC.
           MOVE WS-BRACCT TO WS-MATURITY-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-MATURITY-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-ACCTNO,JUSTIFY-TYPE.
           MOVE WS-LN-OWNBR     TO WS-MATURITY-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-OWNBR, JUSTIFY-TYPE.
        
           MOVE " "                TO WS-MATURITY-MDTE-FLAG
                                                    MDTE-FLAG.
           MOVE " "              TO WS-MATURITY-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE " "                TO WS-MATURITY-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-MATURITY-WORK-REC.                               
 

      *"YYY" ORG MATURITY, ORG TERM, DC DO NOTAFFECT MATURITY
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-MATURITY-REC WS-MATURITY-REC.
           PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE WS-BRACCT TO WS-MATURITY-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-MATURITY-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-ACCTNO,JUSTIFY-TYPE.
           MOVE WS-LN-OWNBR     TO WS-MATURITY-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-OWNBR, JUSTIFY-TYPE.
        
           MOVE "Y"                TO WS-MATURITY-MDTE-FLAG
                                                    MDTE-FLAG.
           MOVE "Y"              TO WS-MATURITY-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE "Y"                TO WS-MATURITY-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-MATURITY-WORK-REC.                               

%%         PERFORM RESTORE-ORIGINAL-FIELDS.
%%         MOVE " " TO MDTE-FLAG.
%%         MOVE " " TO MDTE-ORGTERM-FG.
%%         MOVE " " TO MDTE-DC-FLAG.
      *------------------------------------------------ 
       EXIT-CREATE-MATURITY-WORK.        
           EXIT.

       UPDATE-MATURITY-WORK-REC SECTION.
      *ORIGINAL LOAN TERM
           MOVE LN-ORGTERM         TO WS-MATURITY-LN-ORGTERM.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-ORGTERM
                                             ,JUSTIFY-TYPE.

           MOVE LN-SUM-ORGTERM     TO WS-MATURITY-LN-SUM-ORGTERM.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-SUM-ORGTERM, 
                                                      JUSTIFY-TYPE.

           MOVE LN-TOTNODEF         TO WS-MATURITY-LN-TOTNODEF.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-TOTNODEF,
                                                    JUSTIFY-TYPE.

           MOVE LN-1STPYDATE       TO DATE-YYYYMMDD.     
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-MATURITY-LN-1STPYDATE.
         

           MOVE LN-ORIG-1STPYDATE  TO DATE-YYYYMMDD.        
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-MATURITY-LN-ORIG-1STPYDATE.

           PERFORM MATURITY-DATE-CALCULATION.
           MOVE "INC UNITPER"  TO WS-MATURITY-DESC2.
           MOVE NDTE-HOLD          TO WS-MATURITY-NDTE-HOLD.
           CALL "C$JUSTIFY" USING WS-MATURITY-NDTE-HOLD, 
                                             JUSTIFY-TYPE.
           
           MOVE LN-UNITPER-CD      TO WS-MATURITY-LN-UNITPER-CD.
           MOVE LN-UNITPER-FREQ    TO WS-MATURITY-LN-UNITPER-FREQ.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-UNITPER-FREQ,
                                                JUSTIFY-TYPE.

           MOVE "MATURITY"            TO WS-MATURITY-DESC3.
           MOVE MDTE-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY        TO WS-MATURITY-MDTE-DATE. 
           CALL "C$JUSTIFY" USING WS-MATURITY-MDTE-DATE,JUSTIFY-TYPE.
         

           STRING
              WS-MATURITY-LN-BRACCT           ,"|"
              WS-MATURITY-LN-ACCTNO           ,"|"
              WS-MATURITY-LN-OWNBR            ,"|"
              WS-MATURITY-MDTE-FLAG           ,"|"
              WS-MATURITY-MDTE-ORGTERM-FG     ,"|"
              WS-MATURITY-MDTE-DC-FLAG        ,"|"
              WS-MATURITY-LN-ORGTERM          ,"|"
              WS-MATURITY-LN-TOTNODEF         ,"|"
              WS-MATURITY-LN-1STPYDATE        ,"|"
              WS-MATURITY-LN-SUM-ORGTERM      ,"|"
              WS-MATURITY-LN-ORIG-1STPYDATE   ,"|"
              WS-MATURITY-DESC2               ,"|"
              WS-MATURITY-NDTE-HOLD           ,"|"
              WS-MATURITY-LN-UNITPER-CD       ,"|" 
              WS-MATURITY-LN-UNITPER-FREQ     ,"|"
      *OUPUT
              WS-MATURITY-DESC3               ,"|"
              WS-MATURITY-MDTE-DATE,CR-1
                 DELIMITED BY "  " INTO WK-MATURITY-REC.
           PERFORM WRITE-WK-MATURITY-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       UPDATE-MATURITY-WORK-EXIT.
           EXIT.


      ****************************
       CREATE-EXPIR-WORK SECTION.
      ****************************
      *SP-EXPD-FRMLA = " "
           PERFORM SET-COMMON.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
      *--------------------------------------------
      *1
%%         PERFORM RESTORE-ORIGINAL-FIELDS.

           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE " "                 TO WS-EXPIR-SP-EXPD-FRMLA 
%%                                        SP-EXPD-FRMLA.
                                          
           MOVE " "                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE "Y"               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE " "                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 0                  TO SP-EXPD-MONTHS
                                      SP-EXPD-DAYS.

           MOVE 3                  TO LN-TOTNODEF.

           PERFORM UPDATE-EXPIR-WORK-REC.                               
      
      *SP-XPD-FRMLA = " " AND EXPIRATION MONTHS/DAYS
      *2
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.

           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE " "                 TO WS-EXPIR-SP-EXPD-FRMLA 
%%                                        SP-EXPD-FRMLA.

           MOVE " "                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE "Y"               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE " "                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.

           MOVE 3                  TO LN-TOTNODEF.
           MOVE 3                  TO SP-EXPD-MONTHS.
           MOVE 10                 TO SP-EXPD-DAYS.
           PERFORM UPDATE-EXPIR-WORK-REC.                               
            
      *SP-XPD-FRMLA = " "
      *3
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS. 
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE " "                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE "Y"                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE "Y"               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE " "                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 0                  TO SP-EXPD-MONTHS.
           MOVE 0                 TO SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-EXPIR-WORK-REC.                               

      *SP-XPD-FRMLA = " " WITH EXPIRATION MONTHS/DAYS
      *4
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE " "                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE "Y"                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE "Y"               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE " "                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 3                 TO SP-EXPD-MONTHS.
           MOVE 10                TO SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-EXPIR-WORK-REC.                               

      *SP-XPD-FRMLA = " " 
      *5
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE " "                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE "Y"                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE " "               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE "Y"                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 0                 TO SP-EXPD-MONTHS.
           MOVE 0                 TO SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-EXPIR-WORK-REC.                               


      *SP-XPD-FRMLA = " " WITH EXPIRATION MONTHS/DAYS 
      *6
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.

           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE " "                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE "Y"                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE " "               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE "Y"                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 3                 TO SP-EXPD-MONTHS.
           MOVE 10                TO SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-EXPIR-WORK-REC.                               

      *SP-XPD-FRMLA = " "  
      *7
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE " "                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE " "                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE " "               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE " "                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 0                  TO SP-EXPD-MONTHS.
           MOVE 0                 TO   SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-EXPIR-WORK-REC.                               


      *SP-XPD-FRMLA = " "  WITH EXPIRATION MONTH/DAYS
      *8
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE " "                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE " "                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE " "               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE " "                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 3                  TO SP-EXPD-MONTHS.
           MOVE 10                 TO   SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.

           PERFORM UPDATE-EXPIR-WORK-REC.                               

      *SP-XPD-FRMLA = " "  
      *9
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE " "                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE "Y"                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE "Y"               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE "Y"                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 0                  TO SP-EXPD-MONTHS.
           MOVE 0                 TO   SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-EXPIR-WORK-REC.                               


      *SP-XPD-FRMLA = " "  WITH EXPIRATION MONTH/DAYS
      *10
           PERFORM SET-COMMON.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE " "                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE "Y"                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE "Y"               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE "Y"                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 3                  TO SP-EXPD-MONTHS.
           MOVE 10                 TO   SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.

           PERFORM UPDATE-EXPIR-WORK-REC.                               
      *--------------------------------------------------------------
      *SP-EXPD-FRMLA = "A"
      *11
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE "A"                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE " "                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE "Y"               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE " "                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 0                  TO SP-EXPD-MONTHS 
                                        SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-EXPIR-WORK-REC.                               


      *SP-EXPD-FRMLA = "A" WITH EXPIRED MONTHS/DAYS
      *12
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE "A"                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE " "                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE "Y"               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE " "                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 3                  TO SP-EXPD-MONTHS.
           MOVE 10                 TO   SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-EXPIR-WORK-REC.                               


      *SP-XPD-FRMLA = "A"
      *13
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE "A"                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE "Y"                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE "Y"               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE " "                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 0                  TO SP-EXPD-MONTHS.
           MOVE 0                 TO SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-EXPIR-WORK-REC.                               


      *SP-XPD-FRMLA = "A" WITH EXPIRATION MONTHS/DAYS
      *14
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE "A"                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE "Y"                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE "Y"               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE " "                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 3                 TO SP-EXPD-MONTHS.
           MOVE 10                TO SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-EXPIR-WORK-REC.                               


      *SP-XPD-FRMLA = "A" 
      *15
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE "A"                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE "Y"                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE " "               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE "Y"                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 0                 TO SP-EXPD-MONTHS.
           MOVE 0                 TO SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-EXPIR-WORK-REC.                               


      *SP-XPD-FRMLA = "A" WITH EXPIRATION MONTHS/DAYS 
      *16
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE "A"                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE "Y"                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE " "               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE "Y"                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 3                 TO SP-EXPD-MONTHS.
           MOVE 10                TO SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-EXPIR-WORK-REC.                               

      *SP-XPD-FRMLA = "A"  
      *17
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE "A"                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE " "                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE " "               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE " "                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 0                  TO SP-EXPD-MONTHS.
           MOVE 0                 TO   SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-EXPIR-WORK-REC.                               


      *SP-XPD-FRMLA = "A"  WITH EXPIRATION MONTH/DAYS
      *18
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE "A"                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE " "                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE " "               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE " "                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 3                  TO SP-EXPD-MONTHS.
           MOVE 10                 TO   SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.

           PERFORM UPDATE-EXPIR-WORK-REC.                               

      *SP-XPD-FRMLA = "A"  
      *19
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE "A"                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE "Y"                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE "Y"               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE "Y"                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 0                  TO SP-EXPD-MONTHS.
           MOVE 0                 TO   SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-EXPIR-WORK-REC.                               


      *SP-XPD-FRMLA = "A"  WITH EXPIRATION MONTH/DAYS
      *20
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR     TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE. 

           MOVE "A"                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.

           MOVE "Y"                TO 
                                WS-EXPIR-MDTE-FLAG MDTE-FLAG.

           MOVE "Y"               TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE "Y"                TO WS-EXPIR-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           MOVE 3                  TO SP-EXPD-MONTHS.
           MOVE 10                 TO   SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.

           PERFORM UPDATE-EXPIR-WORK-REC.                               
      *-----------------------------------------------------

      *SP-EXPD-FRMLA  = "B"
      *21
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.
           
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.
 
           MOVE WS-LN-ACCTNO  TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, 
                                               JUSTIFY-TYPE.
           MOVE WS-LN-OWNBR   TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE.

           MOVE "B"                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.
           MOVE "Y"                TO WS-EXPIR-MDTE-FLAG
                                                    MDTE-FLAG.
           MOVE " "              TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE "Y"                TO WS-EXPIR-MDTE-DC-FLAG 
           MOVE 0                  TO SP-EXPD-MONTHS
                                      SP-EXPD-DAYS.    
           MOVE 3                  TO LN-TOTNODEF.
           PERFORM UPDATE-EXPIR-WORK-REC.                               
 
      *SP-EXPD-FRMLA  = "B" WITH EXPIRED MONTHS/DAYS
      *22
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-EXPIR-REC WS-EXPIR-REC.

           
           MOVE WS-BRACCT TO WS-EXPIR-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-BRACCT,JUSTIFY-TYPE.
           
           MOVE WS-LN-ACCTNO  TO WS-EXPIR-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-ACCTNO, 
                                               JUSTIFY-TYPE.
           MOVE WS-LN-OWNBR   TO WS-EXPIR-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-EXPIR-LN-OWNBR, JUSTIFY-TYPE.
        
           MOVE "B"                 TO WS-EXPIR-SP-EXPD-FRMLA
%%                                        SP-EXPD-FRMLA.
           MOVE "Y"                TO WS-EXPIR-MDTE-FLAG
                                                    MDTE-FLAG.
           MOVE " "              TO WS-EXPIR-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE "Y"                TO WS-EXPIR-MDTE-DC-FLAG.
           MOVE 3                  TO SP-EXPD-MONTHS.
           MOVE 10                 TO SP-EXPD-DAYS.
           MOVE 3                  TO LN-TOTNODEF.

           PERFORM UPDATE-EXPIR-WORK-REC.                               
%%         PERFORM RESTORE-ORIGINAL-FIELDS.

%%         MOVE " " TO MDTE-FLAG.
%%         MOVE " " TO MDTE-ORGTERM-FG.
%%         MOVE " " TO MDTE-DC-FLAG.

       
       EXIT-CREATE-EXPIR-WORK.        
           EXIT.


       UPDATE-EXPIR-WORK-REC SECTION.
           PERFORM EXPIRATION-DATE-CALCULATION.
           MOVE MDTE-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY
           MOVE DATE-MMDDYYYY      TO WS-EXPIR-MDTE-DATE. 
           MOVE SP-EXPD-MONTHS     TO WS-EXPIR-SP-EXPD-MONTHS.
           CALL "C$JUSTIFY" USING WS-EXPIR-SP-EXPD-MONTHS,
                                               JUSTIFY-TYPE. 
           MOVE SP-EXPD-DAYS       TO WS-EXPIR-SP-EXPD-DAYS.         
           CALL "C$JUSTIFY" USING WS-EXPIR-SP-EXPD-DAYS,  
                                               JUSTIFY-TYPE. 
           MOVE "EXPIR"               TO WS-EXPIR-DESC1.
           MOVE XDTE-DATE          TO DATE-YYYYMMDD.              
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY
           MOVE DATE-MMDDYYYY      TO WS-EXPIR-XDTE-DATE.

           STRING
              WS-EXPIR-LN-BRACCT           ,"|"
              WS-EXPIR-LN-ACCTNO           ,"|"
              WS-EXPIR-LN-OWNBR            ,"|"
              WS-EXPIR-SP-EXPD-FRMLA       ,"|"
              WS-EXPIR-MDTE-FLAG           ,"|"
              WS-EXPIR-MDTE-ORGTERM-FG     ,"|"              
              WS-EXPIR-MDTE-DC-FLAG        ,"|"
              WS-EXPIR-MDTE-DATE           ,"|"
              WS-EXPIR-SP-EXPD-MONTHS      ,"|"
              WS-EXPIR-SP-EXPD-DAYS        ,"|"
              WS-EXPIR-DESC1               ,"|"
              WS-EXPIR-XDTE-DATE,CR-1
                 DELIMITED BY "  " INTO WK-EXPIR-REC.
           PERFORM WRITE-WK-EXPIR-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       UPDATE-EXPIR-WORK-EXIT.
           EXIT.

      ****************************
       CREATE-INSURANCE-WORK SECTION.
      ****************************

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF LN-INSPREM(SUB) NOT = 0
                 MOVE SUB TO ITRM-SUB   
                 PERFORM CREATE-INS-WORK 
              END-IF
           END-PERFORM.

       CREATE-INS-WORK SECTION.
           PERFORM SET-COMMON. 
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-INSURANCE-REC WS-INSURANCE-REC.  
           MOVE WS-BRACCT TO WS-INS-LN-BRACCT.  
           CALL "C$JUSTIFY" USING WS-INS-LN-BRACCT,JUSTIFY-TYPE.  

           MOVE WS-LN-ACCTNO TO WS-INS-LN-ACCTNO.  
           CALL "C$JUSTIFY" USING WS-INS-LN-ACCTNO, JUSTIFY-TYPE.  

           MOVE WS-LN-OWNBR     TO WS-INS-LN-OWNBR.  
           CALL "C$JUSTIFY" USING WS-INS-LN-OWNBR, JUSTIFY-TYPE.
           MOVE LN-INSCODES          TO WS-INS-LN-INSCODES.  

           MOVE LN-INSCODES          TO WS-INS-LN-INSCODES.
           MOVE  ITRM-SUB            TO WS-INS-ITRM-SUB.
           CALL "C$JUSTIFY" USING WS-INS-ITRM-SUB, JUSTIFY-TYPE.

%%    *    MOVE TODAYS-DATE          TO ITRM-TODAY.
           MOVE  TODAYS-DATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO  WS-INS-ITRM-TODAY.                 

           MOVE  LN-INSEFF-DATE(ITRM-SUB)      TO DATE-YYYYMMDD.             
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-INS-LN-INSEFF-DATE.

           MOVE  LN-INSEXP-DATE(ITRM-SUB)      TO DATE-YYYYMMDD.            
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-INS-LN-INSEXP-DATE.
         
           IF ITRM-SUB > 3
              MOVE LN-REBATE(ITRM-SUB + 5) TO WS-INS-LN-REBATE
           ELSE
              MOVE  LN-REBATE(ITRM-SUB)           TO WS-INS-LN-REBATE.
           MOVE  LN-UNITPER-CD        TO WS-LN-UNITPER-CD.
           PERFORM UPDATE-INSURANCE-WORK-REC.
           PERFORM RESTORE-ORIGINAL-FIELDS. 
       EXIT-CREATE-INS-WORK.        


       UPDATE-INSURANCE-WORK-REC SECTION.
           MOVE TODAYS-DATE TO ITRM-TODAY.
           PERFORM INS-TERM-CALCULATION.
           MOVE "TERM"                TO WS-INS-DESC1.
           MOVE ITRM-INSTERM          TO WS-INS-ITRM-INSTERM.
           CALL "C$JUSTIFY" USING WS-INS-ITRM-INSTERM, JUSTIFY-TYPE.
       
           MOVE "IN EFFECT"           TO WS-INS-DESC2.
           MOVE ITRM-INEFFECT-FG      TO WS-INS-ITRM-INEFFECT-FG.

           STRING
              WS-INS-LN-BRACCT           ,"|"
              WS-INS-LN-ACCTNO           ,"|"
              WS-INS-LN-OWNBR            ,"|"
              WS-INS-LN-INSCODES         ,"|"
              WS-INS-ITRM-SUB            ,"|"
              WS-INS-LN-INSEFF-DATE      ,"|"
              WS-INS-LN-INSEXP-DATE      ,"|"
              WS-INS-LN-REBATE           ,"|"
              WS-LN-UNITPER-CD           ,"|"
              WS-INS-DESC1               ,"|"
              WS-INS-ITRM-INSTERM        ,"|"
              WS-INS-DESC2               ,"|"
              WS-INS-ITRM-INEFFECT-FG, CR-1
                 DELIMITED BY "  " INTO WK-INSURANCE-REC. 
              PERFORM WRITE-WK-INSURANCE-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.


       CREATE-LPUPER-WORK SECTION.
           PERFORM SET-COMMON. 
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-LPUPER-REC WS-LPUPER-REC.  

           MOVE WS-BRACCT TO WS-LPUPER-LN-BRACCT.  
           CALL "C$JUSTIFY" USING WS-LPUPER-LN-BRACCT,JUSTIFY-TYPE.  

           MOVE WS-LN-ACCTNO TO WS-LPUPER-LN-ACCTNO.  
           CALL "C$JUSTIFY" USING WS-LPUPER-LN-ACCTNO, JUSTIFY-TYPE.  

           MOVE WS-LN-OWNBR       TO WS-LPUPER-LN-OWNBR.  
           CALL "C$JUSTIFY" USING WS-LPUPER-LN-OWNBR, JUSTIFY-TYPE.

           MOVE LN-UNITPER-CD     TO WS-LPUPER-UNITPER-CD
                                        UPER-UNITPER-CD.

           MOVE LN-UNITPER-FREQ   TO WS-LPUPER-UNITPER-FREQ
                                        UPER-UNITPER-FREQ.
           CALL "C$JUSTIFY" USING WS-LPUPER-UNITPER-FREQ, JUSTIFY-TYPE.

           MOVE LN-INTDATE        TO DATE-YYYYMMDD                       
                                        UPER-INTDATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY    TO WS-LPUPER-LN-INTDATE.

           MOVE LN-ORIG-1STPYDATE TO DATE-YYYYMMDD                
                                        UPER-1STPYDATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY    TO WS-LPUPER-LN-1STPYDATE.
           MOVE LN-ORGTERM        TO WS-LPUPER-LN-TERM 
                                        UPER-TERM.
           CALL "C$JUSTIFY" USING WS-LPUPER-LN-TERM,JUSTIFY-TYPE.
           PERFORM UPDATE-LPUPER-WORK-REC.
           PERFORM RESTORE-ORIGINAL-FIELDS.    


       UPDATE-LPUPER-WORK-REC SECTION.

           PERFORM UPER-CALCULATION.               
           MOVE UPER-UNITPER-PER-YEAR TO
                                          WS-LPUPER-UNITPER-PER-YEAR.
           CALL "C$JUSTIFY" USING WS-LPUPER-UNITPER-PER-YEAR 
                                                ,JUSTIFY-TYPE.

           MOVE UPER-DAYS-IN-UNITPER  TO WS-LPUPER-DAYS-IN-UNITPER.
           CALL 
            "C$JUSTIFY" USING WS-LPUPER-DAYS-IN-UNITPER, JUSTIFY-TYPE.   

           MOVE UPER-FULL-UNITPER TO WS-LPUPER-FULL-UNITPER.
           CALL "C$JUSTIFY" USING WS-LPUPER-FULL-UNITPER,JUSTIFY-TYPE.
           MOVE UPER-REUBEN   TO    WS-LPUPER-FRACT-UNITPER.  
           MOVE UPER-ERRCD      TO  WS-LPUPER-ERRCD.

           MOVE UPER-SINGLE-PAY-FREQ TO WS-LPUPER-SINGLE-PAY-FREQ.
           CALL "C$JUSTIFY" USING WS-LPUPER-SINGLE-PAY-FREQ,
                                                JUSTIFY-TYPE.
           MOVE UPER-SINGLE-PAY-CD TO WS-LPUPER-SINGLE-PAY-CD.
           

           STRING
              WS-LPUPER-LN-BRACCT           ,"|"
              WS-LPUPER-LN-ACCTNO           ,"|"
              WS-LPUPER-LN-OWNBR            ,"|"
               WS-LPUPER-UNITPER-CD         ,"|"
               WS-LPUPER-UNITPER-FREQ       ,"|"
               WS-LPUPER-LN-INTDATE         ,"|"
               WS-LPUPER-LN-1STPYDATE       ,"|"
               WS-LPUPER-LN-TERM            ,"|"
      *    WORKERS
               WS-LPUPER-SINGLE-PAY-CD      ,"|"
               WS-LPUPER-SINGLE-PAY-FREQ    ,"|"
      *OUTPUT
               WS-LPUPER-UNITPER-PER-YEAR  ,"|"
               WS-LPUPER-DAYS-IN-UNITPER   ,"|"
               WS-LPUPER-FULL-UNITPER      ,"|"
               WS-LPUPER-FRACT-UNITPER     ,"|"
               WS-LPUPER-ERRCD,CR-1
                    DELIMITED BY "  " INTO 
                                         WK-LPUPER-REC.
           PERFORM WRITE-WK-LPUPER-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       UPDATE-LPUPER-WORK-EXIT.
           EXIT.

       CREATE-LPAMTS-WORK SECTION.
           PERFORM SET-COMMON. 
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-LPAMTS-REC WS-LPAMTS-REC.  
           MOVE WS-BRACCT TO WS-LPAMTS-LN-BRACCT.  
           CALL "C$JUSTIFY" USING WS-LPAMTS-LN-BRACCT,JUSTIFY-TYPE.  

           MOVE WS-LN-ACCTNO TO WS-LPAMTS-LN-ACCTNO.  
           CALL "C$JUSTIFY" USING WS-LPAMTS-LN-ACCTNO, JUSTIFY-TYPE.  

           MOVE WS-LN-OWNBR       TO WS-LPAMTS-LN-OWNBR.  
           CALL "C$JUSTIFY" USING WS-LPAMTS-LN-OWNBR, JUSTIFY-TYPE.

          
           MOVE LN-CREDITLIM       TO WS-LPAMTS-LN-CREDIT-LIMIT.
           CALL "C$JUSTIFY" USING WS-LPAMTS-LN-CREDIT-LIMIT,           
                                                  JUSTIFY-TYPE.

           MOVE LN-NOREGPY         TO WS-LPAMTS-LN-NOREGPY.
           CALL "C$JUSTIFY" USING WS-LPAMTS-LN-NOREGPY,
                                             JUSTIFY-TYPE.

           MOVE LN-1STPYAMT TO WS-LPAMTS-LN-1STPYAMT.
           CALL "C$JUSTIFY" USING WS-LPAMTS-LN-1STPYAMT,
                                     JUSTIFY-TYPE.
           MOVE LN-REGPYAMT     TO WS-LPAMTS-LN-REGPYAMT.
           CALL "C$JUSTIFY" USING WS-LPAMTS-LN-REGPYAMT,
                                                 JUSTIFY-TYPE.

           MOVE LN-LASTPYAMT   TO WS-LPAMTS-LN-LASTPYAMT
           CALL "C$JUSTIFY" USING WS-LPAMTS-LN-LASTPYAMT,
                                             JUSTIFY-TYPE.  

           MOVE LN-EXTCHG TO WS-LPAMTS-LN-EXTCHG.
           CALL "C$JUSTIFY" USING WS-LPAMTS-LN-EXTCHG,JUSTIFY-TYPE.
 
           MOVE LN-MAINTFEE TO WS-LPAMTS-LN-MAINTFEE.
           CALL "C$JUSTIFY" USING WS-LPAMTS-LN-MAINTFEE,
                                             JUSTIFY-TYPE.

      *INSURANCE          
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
                  MOVE SUB          TO WS-LPAMTS-SUB(SUB)
                 CALL "C$JUSTIFY" USING WS-LPAMTS-SUB(SUB)              
                                             JUSTIFY-TYPE 

                 MOVE LN-INSOURS(SUB) TO                  
                               WS-LPAMTS-LN-INSOURS(SUB)

                 MOVE LN-INSPREM(SUB) TO
                              WS-LPAMTS-LN-INSPREM(SUB)
               CALL "C$JUSTIFY" USING WS-LPAMTS-LN-INSPREM(SUB)              
                                             JUSTIFY-TYPE 

               MOVE LN-INSFINCHG(SUB) TO
                            WS-LPAMTS-LN-INSFINCHG(SUB)

               CALL "C$JUSTIFY" USING WS-LPAMTS-LN-INSFINCHG(SUB)              
                                             JUSTIFY-TYPE 
           END-PERFORM.


           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
                      
                  MOVE LN-FEEOURS(SUB) TO
                            WS-LPAMTS-LN-FEEOURS(SUB)

               MOVE LN-FEEFINCHG(SUB) TO
                            WS-LPAMTS-LN-FEEFINCHG(SUB)

               MOVE LN-FEECODE(SUB) TO WS-LPAMTS-LN-FEECODE(SUB)

               MOVE LN-FEEAMT(SUB)  TO WS-LPAMTS-LN-FEEAMT(SUB)
               CALL "C$JUSTIFY" USING WS-LPAMTS-LN-FEEAMT(SUB),
                                                 JUSTIFY-TYPE
           END-PERFORM.
              
           PERFORM UPDATE-LPAMTS-WORK-REC.            
  %%       PERFORM RESTORE-ORIGINAL-FIELDS.

       UPDATE-LPAMTS-WORK-REC SECTION.
           PERFORM LOAN-CALCULATIONS.
           MOVE TOTAL-REPAY        TO WS-LPAMTS-TOTAL-REPAY.
           CALL "C$JUSTIFY" USING WS-LPAMTS-TOTAL-REPAY, JUSTIFY-TYPE.

           MOVE TOTAL-NOTE         TO WS-LPAMTS-TOTAL-NOTE.
           CALL "C$JUSTIFY" USING WS-LPAMTS-TOTAL-NOTE, JUSTIFY-TYPE.

           MOVE PRINCIPLE          TO WS-LPAMTS-PRINCIPLE. 
           CALL "C$JUSTIFY" USING WS-LPAMTS-PRINCIPLE, JUSTIFY-TYPE.

           MOVE ADJ-PRINCIPAL      TO WS-LPAMTS-ADJ-PRINCIPAL.
           CALL "C$JUSTIFY" USING WS-LPAMTS-ADJ-PRINCIPAL, JUSTIFY-TYPE.

           MOVE CASH-ADVANCE       TO WS-LPAMTS-CASH-ADVANCE.
           CALL "C$JUSTIFY" USING WS-LPAMTS-CASH-ADVANCE, JUSTIFY-TYPE.

           MOVE SPCASH-ADVANCE     TO WS-LPAMTS-SPCASH-ADVANCE.
           CALL "C$JUSTIFY" USING WS-LPAMTS-SPCASH-ADVANCE,
                                                     JUSTIFY-TYPE.

           MOVE FINANCED-AMOUNT    TO WS-LPAMTS-FINANCED-AMOUNT.
           CALL "C$JUSTIFY" USING WS-LPAMTS-FINANCED-AMOUNT,
                                           JUSTIFY-TYPE.

           MOVE DISCLOSED-FINAMT   TO WS-LPAMTS-DISCLOSED-FINAMT.
           CALL "C$JUSTIFY" USING WS-LPAMTS-DISCLOSED-FINAMT,
                                              JUSTIFY-TYPE

           MOVE HIGH-CREDIT        TO WS-LPAMTS-HIGH-CREDIT.
           CALL "C$JUSTIFY" USING WS-LPAMTS-HIGH-CREDIT, JUSTIFY-TYPE.
            
           MOVE INT-CHARGEABLE      TO WS-LPAMTS-INT-CHARGEABLE.
           CALL "C$JUSTIFY" USING WS-LPAMTS-INT-CHARGEABLE,            
                                                JUSTIFY-TYPE.

           MOVE NONMEMO-INS        TO WS-LPAMTS-NONMEMO-INS.
           CALL "C$JUSTIFY" USING WS-LPAMTS-NONMEMO-INS                
                                                JUSTIFY-TYPE.

           MOVE NONMEMO-FEES       TO WS-LPAMTS-NONMEMO-FEES.
           CALL "C$JUSTIFY" USING WS-LPAMTS-NONMEMO-FEES,JUSTIFY-TYPE.

           MOVE TOTAL-INSPREM-NONMEMO TO
                                WS-LPAMTS-TOTAL-INSPREM-NONMEMO.
           CALL "C$JUSTIFY" USING WS-LPAMTS-TOTAL-INSPREM-NONMEMO,
                                                     JUSTIFY-TYPE.

           MOVE TOTAL-INSFINCHG    TO WS-LPAMTS-TOTAL-INSFINCHG.
           CALL "C$JUSTIFY" USING WS-LPAMTS-TOTAL-INSFINCHG,
                                                JUSTIFY-TYPE.

           MOVE TOTAL-FEEFINCHG    TO WS-LPAMTS-TOTAL-FEEFINCHG.
           CALL "C$JUSTIFY" USING WS-LPAMTS-TOTAL-FEEFINCHG,
                                                JUSTIFY-TYPE.

           MOVE TOTAL-INS-TAX      TO WS-LPAMTS-TOTAL-INS-TAX.
           CALL "C$JUSTIFY" USING WS-LPAMTS-TOTAL-INS-TAX,
                                                JUSTIFY-TYPE.

           MOVE INSUR-OURS           TO WS-LPAMTS-INSUR-OURS.
           CALL "C$JUSTIFY" USING WS-LPAMTS-INSUR-OURS,JUSTIFY-TYPE.

           MOVE FEES-OURS          TO WS-LPAMTS-FEEOURS.
           CALL "C$JUSTIFY" USING WS-LPAMTS-FEEOURS,JUSTIFY-TYPE.

           MOVE CASH-ADVANCE        TO WS-LPAMTS-CASH-ADVANCE.
           CALL "C$JUSTIFY" USING WS-LPAMTS-CASH-ADVANCE,     
                                                 JUSTIFY-TYPE.
           MOVE SPCASH-ADVANCE      TO WS-LPAMTS-SPCASH-ADVANCE,
           CALL "C$JUSTIFY" USING WS-LPAMTS-SPCASH-ADVANCE,     
                                                 JUSTIFY-TYPE.

           MOVE LIVE-CHECK-FEES    TO WS-LPAMTS-LIVE-CHECK-FEES.
           CALL "C$JUSTIFY" USING WS-LPAMTS-LIVE-CHECK-FEES,
                                             JUSTIFY-TYPE.

           STRING
               WS-LPAMTS-LN-BRACCT         ,"|"              
               WS-LPAMTS-LN-ACCTNO         ,"|"       
               WS-LPAMTS-LN-OWNBR          ,"|" 
               WS-LPAMTS-LN-CREDIT-LIMIT   ,"|"   
               WS-LPAMTS-LN-NOREGPY        ,"|"    
               WS-LPAMTS-LN-1STPYAMT       ,"|"   
               WS-LPAMTS-LN-REGPYAMT       ,"|"   
               WS-LPAMTS-LN-LASTPYAMT      ,"|"   
               WS-LPAMTS-LN-EXTCHG         ,"|"   
               WS-LPAMTS-LN-MAINTFEE       ,"|"   

               WS-LPAMTS-SUB(1)            ,"|" 
               WS-LPAMTS-LN-INSOURS(1)     ,"|"
               WS-LPAMTS-LN-INSPREM(1)     ,"|"
               WS-LPAMTS-LN-INSFINCHG(1)   ,"|"

               WS-LPAMTS-SUB(2)            ,"|" 
               WS-LPAMTS-LN-INSOURS(2)     ,"|"
               WS-LPAMTS-LN-INSPREM(2)     ,"|"
               WS-LPAMTS-LN-INSFINCHG(2)   ,"|"

               WS-LPAMTS-SUB(3)            ,"|" 
               WS-LPAMTS-LN-INSOURS(3)     ,"|"
               WS-LPAMTS-LN-INSPREM(3)     ,"|"
               WS-LPAMTS-LN-INSFINCHG(3)   ,"|"

               WS-LPAMTS-SUB(4)            ,"|" 
               WS-LPAMTS-LN-INSOURS(4)     ,"|"
               WS-LPAMTS-LN-INSPREM(4)     ,"|"
               WS-LPAMTS-LN-INSFINCHG(4)   ,"|"

               WS-LPAMTS-SUB(5)            ,"|" 
               WS-LPAMTS-LN-INSOURS(5)     ,"|"
               WS-LPAMTS-LN-INSPREM(5)     ,"|"
               WS-LPAMTS-LN-INSFINCHG(5)   ,"|"

               WS-LPAMTS-SUB(6)            ,"|" 
               WS-LPAMTS-LN-INSOURS(6)     ,"|"
               WS-LPAMTS-LN-INSPREM(6)     ,"|"
               WS-LPAMTS-LN-INSFINCHG(6)   ,"|"

               WS-LPAMTS-SUB(7)            ,"|" 
               WS-LPAMTS-LN-INSOURS(7)     ,"|"
               WS-LPAMTS-LN-INSPREM(7)     ,"|"
               WS-LPAMTS-LN-INSFINCHG(7)   ,"|"

               WS-LPAMTS-SUB(8)            ,"|" 
               WS-LPAMTS-LN-INSOURS(8)     ,"|"
               WS-LPAMTS-LN-INSPREM(8)     ,"|"
               WS-LPAMTS-LN-INSFINCHG(8)   ,"|"

               WS-LPAMTS-LN-FEECODE(1)     ,"|"
               WS-LPAMTS-LN-FEEOURS(1)     ,"|"
               WS-LPAMTS-LN-FEEFINCHG(1)   ,"|"
               WS-LPAMTS-LN-FEEAMT(1)      ,"|"

               WS-LPAMTS-LN-FEECODE(2)     ,"|"
               WS-LPAMTS-LN-FEEOURS(2)     ,"|"
               WS-LPAMTS-LN-FEEFINCHG(2)   ,"|"
               WS-LPAMTS-LN-FEEAMT(2)      ,"|"

               WS-LPAMTS-LN-FEECODE(3)     ,"|"
               WS-LPAMTS-LN-FEEOURS(3)     ,"|"
               WS-LPAMTS-LN-FEEFINCHG(3)   ,"|"
               WS-LPAMTS-LN-FEEAMT(3)      ,"|"

               WS-LPAMTS-LN-FEECODE(4)     ,"|"
               WS-LPAMTS-LN-FEEOURS(4)     ,"|"
               WS-LPAMTS-LN-FEEFINCHG(4)   ,"|"
               WS-LPAMTS-LN-FEEAMT(4)      ,"|"

               WS-LPAMTS-LN-FEECODE(5)     ,"|"
               WS-LPAMTS-LN-FEEOURS(5)     ,"|"
               WS-LPAMTS-LN-FEEFINCHG(5)   ,"|"
               WS-LPAMTS-LN-FEEAMT(5)      ,"|"

               WS-LPAMTS-LN-FEECODE(6)     ,"|"
               WS-LPAMTS-LN-FEEOURS(6)     ,"|"
               WS-LPAMTS-LN-FEEFINCHG(6)   ,"|"
               WS-LPAMTS-LN-FEEAMT(6)      ,"|"

               WS-LPAMTS-LN-FEECODE(7)     ,"|"
               WS-LPAMTS-LN-FEEOURS(7)     ,"|"
               WS-LPAMTS-LN-FEEFINCHG(7)   ,"|"
               WS-LPAMTS-LN-FEEAMT(7)      ,"|"

               WS-LPAMTS-LN-FEECODE(8)     ,"|"
               WS-LPAMTS-LN-FEEOURS(8)     ,"|"
               WS-LPAMTS-LN-FEEFINCHG(8)   ,"|"
               WS-LPAMTS-LN-FEEAMT(8)      ,"|"

               WS-LPAMTS-LN-FEECODE(9)     ,"|"
               WS-LPAMTS-LN-FEEOURS(9)     ,"|"
               WS-LPAMTS-LN-FEEFINCHG(9)   ,"|"
               WS-LPAMTS-LN-FEEAMT(9)      ,"|"

               WS-LPAMTS-LN-FEECODE(10)     ,"|"
               WS-LPAMTS-LN-FEEOURS(10)     ,"|"
               WS-LPAMTS-LN-FEEFINCHG(10)   ,"|"
               WS-LPAMTS-LN-FEEAMT(10)      ,"|"

               "TOT REPAY"                 ,"|"
               WS-LPAMTS-TOTAL-REPAY       ,"|"   
               "TOT NOTE"                  ,"|"
               WS-LPAMTS-TOTAL-NOTE        ,"|"   
               "ADJ PRIN"                  ,"|"
               WS-LPAMTS-ADJ-PRINCIPAL     ,"|"   
               "TOT INSPREM NONMEMO"       ,"|"
               WS-LPAMTS-TOTAL-INSPREM-NONMEMO  ,"|"
               "TOT INSFINCHG"             ,"|"
               WS-LPAMTS-TOTAL-INSFINCHG   ,"|"
               "TOT FEEFINCHG"             ,"|"
               WS-LPAMTS-TOTAL-FEEFINCHG   ,"|"
               "TOT INSTAX"                ,"|"
               WS-LPAMTS-TOTAL-INS-TAX     ,"|"
               "PRINCIPLE"                 ,"|"
               WS-LPAMTS-PRINCIPLE         ,"|" 
               "FINAMT"                    ,"|"
               WS-LPAMTS-FINANCED-AMOUNT   ,"|" 
               "DISC FINAMT"               ,"|"
               WS-LPAMTS-DISCLOSED-FINAMT  ,"|" 
               "INT CHARG"                 ,"|"
               WS-LPAMTS-INT-CHARGEABLE    ,"|" 
               "NONMEMO INS"               ,"|"
               WS-LPAMTS-NONMEMO-INS       ,"|" 
               "NONMEMO FEE"               ,"|"
               WS-LPAMTS-NONMEMO-FEES      ,"|" 
               "INS OURS"                  ,"|"
               WS-LPAMTS-INSUR-OURS        ,"|" 
               "FEE OURS"                  ,"|"
               WS-LPAMTS-FEEOURS           ,"|"
               "LIVE CK FEES"              ,"|"
               WS-LPAMTS-LIVE-CHECK-FEES   ,"|"
               "CASH ADV"                  ,"|"
               WS-LPAMTS-CASH-ADVANCE      ,"|"
               "SPCASH ADV"                ,"|"
               WS-LPAMTS-SPCASH-ADVANCE    ,"|"
               "HI CREDIT"                 ,"|"
               WS-LPAMTS-HIGH-CREDIT, CR-1
                   DELIMITED BY "  " INTO WK-LPAMTS-REC.
               PERFORM WRITE-WK-LPAMTS-FILE.
               IF IO-FG NOT = 0
                   GO TO IO-ERROR.


       CREATE-LPLCHG-WORK SECTION.
      *THIS IS FORCING SUM-REGPYAMT TO LCHG-LATE, WE ARE TESTING
      *DECIMAL PRECISSION, NEED TO INCORPORATE WITH LPLCAP/LPLCAS
      *ONCE THOSE ITEMS ARE CONVERTED
      *DEPENDING ON LCTYPE, (C), DELINQUENT DOLLARS PASSED IN
           PERFORM SET-COMMON. 
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-LPLCHG-REC WS-LPLCHG-REC.  
           MOVE WS-BRACCT TO WS-LPLCHG-LN-BRACCT.  
           CALL "C$JUSTIFY" USING WS-LPLCHG-LN-BRACCT,JUSTIFY-TYPE.  

           MOVE WS-LN-ACCTNO TO WS-LPLCHG-LN-ACCTNO.  
           CALL "C$JUSTIFY" USING WS-LPLCHG-LN-ACCTNO, JUSTIFY-TYPE.  

           MOVE WS-LN-OWNBR       TO WS-LPLCHG-LN-OWNBR.  
           CALL "C$JUSTIFY" USING WS-LPLCHG-LN-OWNBR, JUSTIFY-TYPE.

           MOVE LN-SUM-REGPYAMT       TO WS-LPLCHG-LCHG-LATE
                                                      ,LCHG-LATE
           CALL "C$JUSTIFY" USING WS-LPLCHG-LCHG-LATE,
                                              JUSTIFY-TYPE.

           MOVE LN-SUM-REGPYAMT TO WS-LPLCHG-LCHG-CURPAY
                                                   ,LCHG-CURPAY
           CALL "C$JUSTIFY" USING WS-LPLCHG-LCHG-CURPAY,
                                              JUSTIFY-TYPE.

           MOVE LN-LCPDTH-DATE   TO DATE-YYYYMMDD,LCHG-LCPDTH-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY    TO WS-LPLCHG-LN-LCPDTH-DATE.

           MOVE SP-LCRATE      TO WS-LPLCHG-SP-LCRATE.
           CALL "C$JUSTIFY" USING WS-LPLCHG-SP-LCRATE,JUSTIFY-TYPE.

%%         MOVE 0       TO SP-LCMINCHG.
%%         MOVE 9999.99 TO SP-LCMAXAMT.

%%         PERFORM UPDATE-LPLCHG-WORK-REC.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.

       UPDATE-LPLCHG-WORK-REC SECTION.
      *IT APPEARS THAT ALL THE SPR'S HAVE A MIN OF 10.00 SO
      *I AM FORCING 0 TO SPLCMINCHG.

           PERFORM LATE-CHARGE-CALCULATION.

           MOVE UPWK-UNITPER-CD TO WS-LPLCHG-UPWK-UNITPER-CD.

           MOVE UPWK-UNITPER-FREQ TO WS-LPLCHG-UPWK-UNITPER-FREQ.
           CALL "C$JUSTIFY" USING WS-LPLCHG-UPWK-UNITPER-FREQ,
                                                      JUSTIFY-TYPE.

           MOVE UPWK-LC-GRACE-FAC    TO WS-LPLCHG-UPWK-LC-GRACE-FAC.
           CALL "C$JUSTIFY" USING WS-LPLCHG-UPWK-LC-GRACE-FAC,
                                                        JUSTIFY-TYPE.
           MOVE UPWK-LC-MIN-FAC   TO WS-LPLCHG-UPWK-LC-MIN-FAC.
           CALL "C$JUSTIFY" USING WS-LPLCHG-UPWK-LC-MIN-FAC,   
                                                        JUSTIFY-TYPE.
           MOVE UPWK-LC-MAX-FAC  TO WS-LPLCHG-UPWK-LC-MAX-FAC. 
           CALL "C$JUSTIFY" USING WS-LPLCHG-UPWK-LC-MAX-FAC,   
                                                        JUSTIFY-TYPE.

           MOVE UPWK-LC-PER-FAC  TO WS-LPLCHG-UPWK-LC-PER-FAC. 
           CALL "C$JUSTIFY" USING WS-LPLCHG-UPWK-LC-PER-FAC,   
                                                        JUSTIFY-TYPE.

           MOVE UPWK-ERRCD TO WS-LPLCHG-UPWK-ERRCD.

           MOVE LCHG-WORK      TO WS-LPLCHG-LCHG-WORK.
           CALL "C$JUSTIFY" USING WS-LPLCHG-LCHG-WORK,
                                              JUSTIFY-TYPE.

           MOVE LCHG-CHARGE    TO WS-LPLCHG-LCHG-CHARGE.
           CALL "C$JUSTIFY" USING WS-LPLCHG-LCHG-CHARGE,
                                        JUSTIFY-TYPE.

           STRING
               WS-LPLCHG-LN-BRACCT         ,"|"
               WS-LPLCHG-LN-ACCTNO         ,"|"
               WS-LPLCHG-LN-OWNBR          ,"|"
               WS-LPLCHG-LCHG-LATE         ,"|"
               WS-LPLCHG-LCHG-CURPAY       ,"|"
               WS-LPLCHG-LN-LCPDTH-DATE    ,"|"
               WS-LPLCHG-SP-LCRATE         ,"|"
               "LPUPWK WORKERS  "          ,"|"
               WS-LPLCHG-UPWK-UNITPER-CD   ,"|"
               WS-LPLCHG-UPWK-UNITPER-FREQ ,"|"
               WS-LPLCHG-UPWK-LC-GRACE-FAC ,"|"
               WS-LPLCHG-UPWK-LC-MIN-FAC   ,"|"
               WS-LPLCHG-UPWK-LC-MAX-FAC   ,"|"
               WS-LPLCHG-UPWK-LC-PER-FAC   ,"|"
               WS-LPLCHG-UPWK-ERRCD        ,"|"
               "LC WORK"                   ,"|"
               WS-LPLCHG-LCHG-WORK           ,"|"
               "LATE CHARGE AMT   "          ,"|"
               WS-LPLCHG-LCHG-CHARGE, CR-1
                  DELIMITED BY "  " INTO WK-LPLCHG-REC.
               PERFORM WRITE-WK-LPLCHG-FILE.

               IF IO-FG NOT = 0
                   GO TO IO-ERROR.


       CREATE-SPPDTH-WORK SECTION.
           PERFORM SET-COMMON. 
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-SPPDTH-REC WS-SPPDTH-REC.  
      *SAVE ORIGINAL VALUES
           
           MOVE WS-BRACCT TO WS-SPPDTH-LN-BRACCT.  
           CALL "C$JUSTIFY" USING WS-SPPDTH-LN-BRACCT,JUSTIFY-TYPE.  

           MOVE WS-LN-ACCTNO TO WS-SPPDTH-LN-ACCTNO.  
           CALL "C$JUSTIFY" USING WS-SPPDTH-LN-ACCTNO, JUSTIFY-TYPE.  

           MOVE WS-LN-OWNBR       TO WS-SPPDTH-LN-OWNBR.  
           CALL "C$JUSTIFY" USING WS-SPPDTH-LN-OWNBR, JUSTIFY-TYPE.

           MOVE LN-1STPYDATE         TO PDTH-DATE-WORK
                                        DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY. 
           MOVE DATE-MMDDYYYY       TO WS-SPPDTH-PDTH-DATE-WORK.
           

           MOVE 1                  TO LN-TOTNOALDEL
                                      WS-SPPDTH-LN-TOTNOALDEL.
           CALL "C$JUSTIFY" USING WS-SPPDTH-LN-TOTNOALDEL,
                                             JUSTIFY-TYPE.
           MOVE 2                  TO LN-TOTNODEF
                                      WS-SPPDTH-LN-TOTNODEF.
           CALL "C$JUSTIFY" USING WS-SPPDTH-LN-TOTNODEF,    
                                             JUSTIFY-TYPE.

           IF SP-CONTR-CREDIT-CD NOT = " "
                MOVE SP-CONTR-CREDIT      TO PDTH-CREDIT
                                             WS-SPPDTH-PDTH-CREDIT 
           ELSE
                MOVE 0                    TO WS-SPPDTH-PDTH-CREDIT.
             CALL "C$JUSTIFY" USING WS-SPPDTH-PDTH-CREDIT,
                                        JUSTIFY-TYPE.

           MOVE SPACES               TO PDTH-DEF-FG
                                        WS-SPPDTH-PDTH-DEF-FG.
           MOVE SPACES               TO PDTH-ALDEL-FG
                                        WS-SPPDTH-PDTH-ALDEL-FG.

           MOVE LN-TOTPAYMNTD        TO WS-SPPDTH-PDTH-PAYMNTD.
           CALL "C$JUSTIFY" USING WS-SPPDTH-PDTH-PAYMNTD,
                                                  JUSTIFY-TYPE.

           MOVE 0 TO  WS-SPPDTH-PDTH-E-1STPYAMT
                      WS-SPPDTH-PDTH-E-REGPYAMT
                      WS-SPPDTH-PDTH-E-LASTPYAMT.

           IF HOLD-PDTH-E-OPTION = "E"
               MOVE "E" TO PDTH-E-OPTION.

           PERFORM UPDATE-SPPDTH-WORK-REC.

      *FORCE "N" TO ALLOWABLE DELINQ FG, DEFERMENT FG

           PERFORM SET-COMMON. 
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-SPPDTH-REC WS-SPPDTH-REC.  
           MOVE WS-BRACCT TO WS-SPPDTH-LN-BRACCT.  
           CALL "C$JUSTIFY" USING WS-SPPDTH-LN-BRACCT,JUSTIFY-TYPE.  

           MOVE WS-LN-ACCTNO TO WS-SPPDTH-LN-ACCTNO.  
           CALL "C$JUSTIFY" USING WS-SPPDTH-LN-ACCTNO, JUSTIFY-TYPE.  

           MOVE WS-LN-OWNBR       TO WS-SPPDTH-LN-OWNBR.  
           CALL "C$JUSTIFY" USING WS-SPPDTH-LN-OWNBR, JUSTIFY-TYPE.

           MOVE LN-1STPYDATE         TO PDTH-DATE-WORK
                                        DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY. 
           MOVE DATE-MMDDYYYY       TO WS-SPPDTH-PDTH-DATE-WORK.
           

           MOVE 1                  TO LN-TOTNOALDEL
                                      WS-SPPDTH-LN-TOTNOALDEL.
           CALL "C$JUSTIFY" USING WS-SPPDTH-LN-TOTNOALDEL,
                                             JUSTIFY-TYPE.
           MOVE 2                  TO LN-TOTNODEF
                                      WS-SPPDTH-LN-TOTNODEF.
           CALL "C$JUSTIFY" USING WS-SPPDTH-LN-TOTNODEF,    
                                             JUSTIFY-TYPE.

           IF SP-CONTR-CREDIT-CD NOT = " "
                MOVE SP-CONTR-CREDIT      TO PDTH-CREDIT
                                             WS-SPPDTH-PDTH-CREDIT 
           ELSE
                MOVE 0                    TO WS-SPPDTH-PDTH-CREDIT.
             CALL "C$JUSTIFY" USING WS-SPPDTH-PDTH-CREDIT,
                                        JUSTIFY-TYPE.


           MOVE "N"                  TO PDTH-DEF-FG
                                        WS-SPPDTH-PDTH-DEF-FG.
           MOVE "N"                  TO PDTH-ALDEL-FG
                                        WS-SPPDTH-PDTH-ALDEL-FG.

           MOVE LN-TOTPAYMNTD        TO WS-SPPDTH-PDTH-PAYMNTD.
           CALL "C$JUSTIFY" USING WS-SPPDTH-PDTH-PAYMNTD,
                                                  JUSTIFY-TYPE.

           MOVE 0 TO  WS-SPPDTH-PDTH-E-1STPYAMT
                      WS-SPPDTH-PDTH-E-REGPYAMT
                      WS-SPPDTH-PDTH-E-LASTPYAMT.

           PERFORM UPDATE-SPPDTH-WORK-REC.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.

       UPDATE-SPPDTH-WORK-REC SECTION.
           PERFORM PAID-THRU-CALCULATION.
           IF LN-PAY-SCHLD-FG      = "Y"
              MOVE PDTH-WORKER TO WS-SPPDTH-PDTH-WORKER 
           ELSE 
              MOVE 0           TO WS-SPPDTH-PDTH-WORKER.
           CALL "C$JUSTIFY" USING WS-SPPDTH-PDTH-WORKER,
                                              JUSTIFY-TYPE.
            IF LN-PAY-SCHLD-FG = "Y"
                 MOVE PDTH TO WS-SPPDTH-PDTH-PDTH 
            ELSE
                 MOVE ZEROS TO WS-SPPDTH-PDTH-PDTH.

            CALL "C$JUSTIFY" USING WS-SPPDTH-PDTH-PDTH,
                                         JUSTIFY-TYPE.
           MOVE PDTH-DATE   TO WS-SPPDTH-PDTH-DATE.

           MOVE PDTH-DATE-FULL TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-SPPDTH-PDTH-DATE-FULL.
          
           MOVE PDTH-LCPDTHRU  TO WS-SPPDTH-PDTH-LCPDTHRU.
           CALL "C$JUSTIFY" USING WS-SPPDTH-PDTH-LCPDTHRU,
                                               JUSTIFY-TYPE.
           MOVE PDTH-CUR-SCHD-DATE TO DATE-YYYYMMDD.                        
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY      TO WS-SPPDTH-PDTH-CUR-SCHD-DATE.
 
           MOVE PDTH-CUR-SCHD-PAY TO WS-SPPDTH-PDTH-CUR-SCHD-PAY.
           CALL "C$JUSTIFY" USING WS-SPPDTH-PDTH-CUR-SCHD-PAY,
                                               JUSTIFY-TYPE.
           MOVE PDTH-NXT-SCHD-DATE TO DATE-YYYYMMDD.                  
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY   TO WS-SPPDTH-PDTH-NXT-SCHD-DATE.

           MOVE PDTH-NXT-SCHD-PAY TO WS-SPPDTH-PDTH-NXT-SCHD-PAY.
           CALL "C$JUSTIFY" USING WS-SPPDTH-PDTH-NXT-SCHD-PAY,
                                               JUSTIFY-TYPE.
           MOVE PDTH-PAYMENTS    TO WS-SPPDTH-PDTH-PAYMENTS.    
           CALL "C$JUSTIFY" USING WS-SPPDTH-PDTH-PAYMENTS
                                                  ,JUSTIFY-TYPE. 
           MOVE PDTH-AMT-TO-ADVANCE     TO
                                 WS-SPPDTH-PDTH-AMT-TO-ADVANCE. 
           CALL "C$JUSTIFY" USING WS-SPPDTH-PDTH-AMT-TO-ADVANCE
                                            JUSTIFY-TYPE.
   
            MOVE HOLD-PDTH-E-OPTION   TO WS-SPPDTH-PDTH-E-OPTION.

            IF HOLD-PDTH-E-OPTION = "E"
                MOVE PDTH-E-REGPYAMT     TO WS-SPPDTH-PDTH-E-REGPYAMT 
                CALL "C$JUSTIFY" USING WS-SPPDTH-PDTH-E-REGPYAMT,
                                                  JUSTIFY-TYPE 

                MOVE PDTH-E-1STPYAMT TO WS-SPPDTH-PDTH-E-1STPYAMT 
                CALL "C$JUSTIFY" USING WS-SPPDTH-PDTH-E-1STPYAMT 
                                             JUSTIFY-TYPE  

                MOVE PDTH-E-LASTPYAMT TO WS-SPPDTH-PDTH-E-LASTPYAMT 
                CALL "C$JUSTIFY" USING WS-SPPDTH-PDTH-E-LASTPYAMT 
                                             JUSTIFY-TYPE 
            ELSE
                MOVE ZEROS TO WS-SPPDTH-PDTH-E-REGPYAMT
                              WS-SPPDTH-PDTH-E-1STPYAMT
                              WS-SPPDTH-PDTH-E-LASTPYAMT.


           STRING
               WS-SPPDTH-LN-BRACCT             ,"|"
               WS-SPPDTH-LN-ACCTNO             ,"|"
               WS-SPPDTH-LN-OWNBR              ,"|"
               WS-SPPDTH-PDTH-DATE-WORK        ,"|"
               WS-SPPDTH-PDTH-CREDIT           ,"|"
               WS-SPPDTH-PDTH-DEF-FG           ,"|"
               WS-SPPDTH-PDTH-ALDEL-FG         ,"|"
               WS-SPPDTH-LN-TOTNOALDEL         ,"|"
               WS-SPPDTH-LN-TOTNODEF           ,"|"
               WS-SPPDTH-PDTH-PAYMNTD          ,"|"
               WS-SPPDTH-PDTH-MONTHS           ,"|"
               WS-SPPDTH-PDTH-YEARS            ,"|"
               WS-SPPDTH-PDTH-WORKER           ,"|"
               WS-SPPDTH-PDTH-CNT              ,"|"
               WS-SPPDTH-PDTH-PDTH             ,"|"
               WS-SPPDTH-PDTH-DATE             ,"|"
               WS-SPPDTH-PDTH-DATE-FULL        ,"|"
               WS-SPPDTH-PDTH-LCPDTHRU         ,"|"
               WS-SPPDTH-PDTH-CUR-SCHD-DATE    ,"|"
               WS-SPPDTH-PDTH-CUR-SCHD-PAY     ,"|"
               WS-SPPDTH-PDTH-NXT-SCHD-DATE    ,"|"
               WS-SPPDTH-PDTH-NXT-SCHD-PAY     ,"|"
               WS-SPPDTH-PDTH-PAYMENTS         ,"|"
               WS-SPPDTH-PDTH-AMT-TO-ADVANCE   ,"|"
               WS-SPPDTH-PDTH-E-OPTION         ,"|"
               WS-SPPDTH-PDTH-E-1STPYAMT       ,"|"
               WS-SPPDTH-PDTH-E-REGPYAMT       ,"|"
               WS-SPPDTH-PDTH-E-LASTPYAMT,CR-1       
                   DELIMITED BY "  " INTO WK-SPPDTH-REC.
               PERFORM WRITE-WK-SPPDTH-FILE.
               IF IO-BAD GO TO IO-ERROR.

       CREATE-LPCEPP-WORK SECTION.
      *DF,AD NO IMPACT, USES PAIDTHRU, CAPPED AT MATURITY   
      *(NOCAP-FG = SPACES)1
           PERFORM SET-COMMON. 
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-LPCEPP-REC WS-LPCEPP-REC.  

           MOVE WS-BRACCT TO WS-LPCEPP-LN-BRACCT.  
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-BRACCT,JUSTIFY-TYPE.  

           MOVE WS-LN-ACCTNO TO WS-LPCEPP-LN-ACCTNO.  
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-ACCTNO, JUSTIFY-TYPE.  

           MOVE WS-LN-OWNBR       TO WS-LPCEPP-LN-OWNBR.  
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-OWNBR, JUSTIFY-TYPE.

           MOVE LN-1STPYDATE      TO PDTH-DATE-WORK CEPP1-DATE
                                     DATE-YYYYMMDD.          
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPCEPP-CEPP1-DATE.


           MOVE 2        TO LN-TOTNODEF,WS-LPCEPP-LN-TOTNODEF.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-TOTNODEF,JUSTIFY-TYPE.

           MOVE 1        TO LN-TOTNOALDEL,WS-LPCEPP-LN-TOTNOALDEL.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-TOTNOALDEL,                
                                                 JUSTIFY-TYPE.
      *-----
           MOVE "N"               TO PDTH-DEF-FG,
                                     PDTH-ALDEL-FG.
      *----

           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL    TO CEPP2-DATE,DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY    TO  WS-LPCEPP-CEPP2-DATE.


           MOVE LN-UNITPER-CD      TO WS-LPCEPP-LN-UNITPER-CD. 
           MOVE LN-UNITPER-FREQ    TO WS-LPCEPP-LN-UNITPER-FREQ.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-UNITPER-FREQ
                                          JUSTIFY-TYPE.
      *-------
           MOVE SPACES             TO CEPP-INCDEF-FG HOLD-INCDEF-FG
                                       WS-LPCEPP-CEPP-INCDEF-FG.
           MOVE SPACES             TO  CEPP-INCALDEL-FG HOLD-INCALDEL-FG
                                       WS-LPCEPP-CEPP-INCALDEL-FG.
           MOVE SPACES             TO  CEPP-NOCAP-FG  HOLD-NOCAP-FG 
                                       WS-LPCEPP-CEPP-NOCAP-FG.

           MOVE "Y"               TO MDTE-FLAG.
           MOVE "Y"               TO MDTE-ORGTERM-FG.
           MOVE CEPP-NOCAP-FG     TO HOLD-NOCAP-FG.
           MOVE "P"               TO HOLD-DATE-CAP.
 
           MOVE CEPP-INCDEF-FG TO HOLD-INCDEF-FG.            
           MOVE CEPP-INCALDEL-FG TO  HOLD-INCALDEL-FG.             
      *-------
           PERFORM UPDATE-LPCEPP-WORK-REC.

      *DF,AD AFFECT,USES PAID THRU, CAPPED AT MATURITY
      *(NOCAP-FG = SPACES)2 
           PERFORM SET-COMMON. 
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-LPCEPP-REC WS-LPCEPP-REC.  
           MOVE SPACES TO HOLD-NOCAP-FG.
           MOVE SPACES TO HOLD-INCDEF-FG.            
           MOVE SPACES TO  HOLD-INCALDEL-FG.             
           MOVE WS-BRACCT TO WS-LPCEPP-LN-BRACCT.  
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-BRACCT,JUSTIFY-TYPE.  

           MOVE WS-LN-ACCTNO TO WS-LPCEPP-LN-ACCTNO.  
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-ACCTNO, JUSTIFY-TYPE.  

           MOVE WS-LN-OWNBR       TO WS-LPCEPP-LN-OWNBR.  
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-OWNBR, JUSTIFY-TYPE.

           MOVE LN-1STPYDATE      TO PDTH-DATE-WORK CEPP1-DATE
                                     DATE-YYYYMMDD.          
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPCEPP-CEPP1-DATE.
        
           MOVE 2   TO LN-TOTNODEF, WS-LPCEPP-LN-TOTNODEF.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-TOTNODEF,
                                                 JUSTIFY-TYPE.
           MOVE 1   TO LN-TOTNOALDEL, WS-LPCEPP-LN-TOTNOALDEL.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-TOTNOALDEL
                                                 JUSTIFY-TYPE.

      *------
           MOVE " "               TO PDTH-DEF-FG,
                                     PDTH-ALDEL-FG.
      *------
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL    TO CEPP2-DATE,DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY    TO  WS-LPCEPP-CEPP2-DATE.


           MOVE LN-UNITPER-CD      TO WS-LPCEPP-LN-UNITPER-CD. 
           MOVE LN-UNITPER-FREQ    TO WS-LPCEPP-LN-UNITPER-FREQ.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-UNITPER-FREQ
                                          JUSTIFY-TYPE.
      *----
           MOVE "X"                TO CEPP-INCDEF-FG 
                                       WS-LPCEPP-CEPP-INCDEF-FG.
           MOVE "X"                TO  CEPP-INCALDEL-FG
                                       WS-LPCEPP-CEPP-INCALDEL-FG.
           MOVE SPACES             TO  CEPP-NOCAP-FG
                                       WS-LPCEPP-CEPP-NOCAP-FG.

           MOVE SPACES            TO MDTE-FLAG.
           MOVE "Y"               TO MDTE-ORGTERM-FG.
           MOVE CEPP-NOCAP-FG     TO HOLD-NOCAP-FG.
           MOVE "P"               TO HOLD-DATE-CAP.

           MOVE CEPP-INCDEF-FG TO HOLD-INCDEF-FG.            
           MOVE CEPP-INCALDEL-FG TO  HOLD-INCALDEL-FG.             
      *----
           PERFORM UPDATE-LPCEPP-WORK-REC.

 
      *USE TODAYS DATE AS CEPP2-DATE,CAPPED AT MATURITY,    
      *DF,AD HAVE NO IMPACT3
           PERFORM SET-COMMON. 
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-LPCEPP-REC WS-LPCEPP-REC.
           MOVE WS-BRACCT TO WS-LPCEPP-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPCEPP-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LPCEPP-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-OWNBR, JUSTIFY-TYPE.

           MOVE LN-1STPYDATE      TO PDTH-DATE-WORK CEPP1-DATE
                                     DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPCEPP-CEPP1-DATE.
      
           MOVE 2        TO LN-TOTNODEF,WS-LPCEPP-LN-TOTNODEF.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-TOTNODEF,JUSTIFY-TYPE.

           MOVE 1        TO LN-TOTNOALDEL,WS-LPCEPP-LN-TOTNOALDEL.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-TOTNOALDEL,                
                                                 JUSTIFY-TYPE.

           MOVE TODAYS-DATE       TO CEPP2-DATE,DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPCEPP-CEPP2-DATE.
            
           MOVE LN-UNITPER-CD      TO WS-LPCEPP-LN-UNITPER-CD.
           MOVE LN-UNITPER-FREQ    TO WS-LPCEPP-LN-UNITPER-FREQ.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-UNITPER-FREQ
                                          JUSTIFY-TYPE.
      *----------
           MOVE SPACES             TO CEPP-INCDEF-FG
                                       WS-LPCEPP-CEPP-INCDEF-FG.
           MOVE SPACES             TO CEPP-INCALDEL-FG
                                      WS-LPCEPP-CEPP-INCALDEL-FG.

           MOVE SPACES             TO CEPP-NOCAP-FG
                                      WS-LPCEPP-CEPP-NOCAP-FG.
          
           MOVE "Y"               TO MDTE-ORGTERM-FG.
           MOVE "Y"               TO MDTE-FLAG. 
           MOVE CEPP-NOCAP-FG     TO HOLD-NOCAP-FG.
           MOVE "T"               TO HOLD-DATE-CAP.

           MOVE CEPP-INCDEF-FG TO HOLD-INCDEF-FG.            
           MOVE CEPP-INCALDEL-FG TO  HOLD-INCALDEL-FG.             
      *----------
           PERFORM UPDATE-LPCEPP-WORK-REC.

      *USE TODAYS DATE AS CEPP2-DATE,CAPPED AT MATURITY,    
      *DF,AD IMPACT MATURITY4               
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-LPCEPP-REC WS-LPCEPP-REC.
           MOVE WS-BRACCT TO WS-LPCEPP-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPCEPP-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LPCEPP-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-OWNBR, JUSTIFY-TYPE.

           MOVE LN-1STPYDATE      TO PDTH-DATE-WORK CEPP1-DATE
                                     DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPCEPP-CEPP1-DATE.
      
           MOVE 2        TO LN-TOTNODEF,WS-LPCEPP-LN-TOTNODEF.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-TOTNODEF,JUSTIFY-TYPE.

           MOVE 1        TO LN-TOTNOALDEL,WS-LPCEPP-LN-TOTNOALDEL.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-TOTNOALDEL,                
                                                 JUSTIFY-TYPE.

           MOVE TODAYS-DATE       TO CEPP2-DATE,DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPCEPP-CEPP2-DATE.
            
           MOVE LN-UNITPER-CD      TO WS-LPCEPP-LN-UNITPER-CD.
           MOVE LN-UNITPER-FREQ    TO WS-LPCEPP-LN-UNITPER-FREQ.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-UNITPER-FREQ
                                          JUSTIFY-TYPE.
      *-----------
           MOVE "X"                TO CEPP-INCDEF-FG
                                       WS-LPCEPP-CEPP-INCDEF-FG.
           MOVE "X"                TO CEPP-INCALDEL-FG
                                      WS-LPCEPP-CEPP-INCALDEL-FG.

           MOVE SPACES             TO CEPP-NOCAP-FG
                                      WS-LPCEPP-CEPP-NOCAP-FG.
          
           MOVE "Y"               TO MDTE-ORGTERM-FG.
           MOVE " "               TO MDTE-FLAG. 
           MOVE CEPP-NOCAP-FG     TO HOLD-NOCAP-FG.
           MOVE "T"               TO HOLD-DATE-CAP.

           MOVE CEPP-INCDEF-FG TO HOLD-INCDEF-FG.            
           MOVE CEPP-INCALDEL-FG TO  HOLD-INCALDEL-FG.             
      *-----------
           PERFORM UPDATE-LPCEPP-WORK-REC.


      *NO CAP AT MATURITY, DF,AD DO NOT AFFECT MATURITY5
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-LPCEPP-REC WS-SPPDTH-REC.
           MOVE SPACES TO HOLD-NOCAP-FG.
           MOVE SPACES TO HOLD-INCDEF-FG.            
           MOVE SPACES TO  HOLD-INCALDEL-FG.             

           MOVE WS-BRACCT TO WS-LPCEPP-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPCEPP-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LPCEPP-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-OWNBR, JUSTIFY-TYPE.

           MOVE LN-1STPYDATE      TO PDTH-DATE-WORK CEPP1-DATE
                                     DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPCEPP-CEPP1-DATE.
      
           MOVE 2        TO LN-TOTNODEF,WS-LPCEPP-LN-TOTNODEF.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-TOTNODEF,JUSTIFY-TYPE.

           MOVE 1        TO LN-TOTNOALDEL,WS-LPCEPP-LN-TOTNOALDEL.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-TOTNOALDEL,                
                                                 JUSTIFY-TYPE.
           MOVE TODAYS-DATE       TO CEPP2-DATE,DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPCEPP-CEPP2-DATE.
            
           MOVE LN-UNITPER-CD      TO WS-LPCEPP-LN-UNITPER-CD.
           MOVE LN-UNITPER-FREQ    TO WS-LPCEPP-LN-UNITPER-FREQ.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-UNITPER-FREQ
                                          JUSTIFY-TYPE.
      *-----

           MOVE " "                TO   CEPP-INCDEF-FG
                                        WS-LPCEPP-CEPP-INCDEF-FG.
           MOVE " "                TO   CEPP-INCALDEL-FG
                                         WS-LPCEPP-CEPP-INCALDEL-FG.
           MOVE "X"                TO   CEPP-NOCAP-FG
                                         WS-LPCEPP-CEPP-NOCAP-FG.


           MOVE "Y"               TO MDTE-ORGTERM-FG.
           MOVE "Y"               TO MDTE-FLAG. 
           MOVE CEPP-NOCAP-FG     TO HOLD-NOCAP-FG.
           MOVE "T"               TO HOLD-DATE-CAP.
     
           MOVE CEPP-INCDEF-FG TO HOLD-INCDEF-FG.            
           MOVE CEPP-INCALDEL-FG TO  HOLD-INCALDEL-FG.             
      *-----
           PERFORM UPDATE-LPCEPP-WORK-REC.



      *NO CAP AT MATURITY, DF,AD AFFECT MATURITY6

           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-LPCEPP-REC WS-SPPDTH-REC.
           MOVE SPACES TO HOLD-NOCAP-FG.
           MOVE SPACES TO HOLD-INCDEF-FG.            
           MOVE SPACES TO  HOLD-INCALDEL-FG.             
           MOVE WS-BRACCT TO WS-LPCEPP-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPCEPP-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LPCEPP-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-OWNBR, JUSTIFY-TYPE.

           MOVE LN-1STPYDATE      TO PDTH-DATE-WORK CEPP1-DATE
                                     DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPCEPP-CEPP1-DATE.
           MOVE 2        TO LN-TOTNODEF,WS-LPCEPP-LN-TOTNODEF.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-TOTNODEF,JUSTIFY-TYPE.

           MOVE 1        TO LN-TOTNOALDEL,WS-LPCEPP-LN-TOTNOALDEL.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-TOTNOALDEL,                
                                                 JUSTIFY-TYPE.
      
           MOVE TODAYS-DATE       TO CEPP2-DATE,DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPCEPP-CEPP2-DATE.
            
           MOVE LN-UNITPER-CD      TO WS-LPCEPP-LN-UNITPER-CD.
           MOVE LN-UNITPER-FREQ    TO WS-LPCEPP-LN-UNITPER-FREQ.
           CALL "C$JUSTIFY" USING WS-LPCEPP-LN-UNITPER-FREQ
                                          JUSTIFY-TYPE.
      *----
           MOVE "X"                TO   CEPP-INCDEF-FG
                                        WS-LPCEPP-CEPP-INCDEF-FG.
           MOVE "X"                TO   CEPP-INCALDEL-FG
                                         WS-LPCEPP-CEPP-INCALDEL-FG.
           MOVE "X"                TO   CEPP-NOCAP-FG
                                         WS-LPCEPP-CEPP-NOCAP-FG.

           MOVE "Y"               TO MDTE-ORGTERM-FG.
           MOVE "Y"               TO MDTE-FLAG. 
           MOVE CEPP-NOCAP-FG     TO HOLD-NOCAP-FG.
           MOVE "T"               TO HOLD-DATE-CAP.
           MOVE CEPP-INCDEF-FG TO HOLD-INCDEF-FG.            
           MOVE CEPP-INCALDEL-FG TO  HOLD-INCALDEL-FG.             
            
           PERFORM UPDATE-LPCEPP-WORK-REC.
      *RESTORE VALUES
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
      *------------------------------------

       UPDATE-LPCEPP-WORK-REC SECTION.
           MOVE 0 TO ELAPSED-DAYS 
                     ELAPSED-MONTHS
                     ELAPSED-REM.
           PERFORM CONTRACTUAL-ELAPSED-PAYPER.
        
           IF HOLD-NOCAP-FG = " "
               IF HOLD-DATE-CAP = "P"
                  IF PDTH-DATE-FULL > MDTE-DATE
                      MOVE "M"         TO WS-LPCEPP-DATE-CAP
                      MOVE MDTE-DATE TO DATE-YYYYMMDD       
                  ELSE
                      MOVE "P"         TO WS-LPCEPP-DATE-CAP
                      MOVE PDTH-DATE-FULL               
                                       TO  DATE-YYYYMMDD          
                  END-IF
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY 
               MOVE DATE-MMDDYYYY TO WS-LPCEPP-USE-DATE.

            IF HOLD-NOCAP-FG = " "
                IF HOLD-DATE-CAP = "T"
                   IF TODAYS-DATE > MDTE-DATE
                      MOVE "M"     TO WS-LPCEPP-DATE-CAP
                      MOVE MDTE-DATE TO DATE-YYYYMMDD          
                   ELSE
                      MOVE "T"     TO WS-LPCEPP-DATE-CAP
                      MOVE TODAYS-DATE TO DATE-YYYYMMDD 
                   END-IF
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY 
               MOVE DATE-MMDDYYYY TO WS-LPCEPP-USE-DATE.
                     
                   
            IF HOLD-NOCAP-FG NOT = SPACES
               IF HOLD-DATE-CAP = "T"
                  IF HOLD-INCDEF-FG = "X" OR HOLD-INCALDEL-FG = "X"        
                     MOVE "TI"    TO WS-LPCEPP-DATE-CAP
                  ELSE
                     MOVE "TS"    TO WS-LPCEPP-DATE-CAP 
                  END-IF
               MOVE CEPP2-DATE  TO DATE-YYYYMMDD 
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY 
               MOVE DATE-MMDDYYYY TO WS-LPCEPP-USE-DATE.


           MOVE ELAPSED-DAYS           TO WS-LPCEPP-ELAPSED-DAYS.          
           CALL "C$JUSTIFY" USING WS-LPCEPP-ELAPSED-DAYS, 
                                         JUSTIFY-TYPE.
           MOVE CEPP-UNITPER-REM       TO WS-LPCEPP-CEPP-UNITPER-REM.
           CALL "C$JUSTIFY" USING WS-LPCEPP-CEPP-UNITPER-REM
                                         JUSTIFY-TYPE.

           MOVE CEPP-PERIODS           TO WS-LPCEPP-CEPP-PERIODS.
           CALL "C$JUSTIFY" USING WS-LPCEPP-CEPP-PERIODS     
                                                 JUSTIFY-TYPE.
          
           STRING
               WS-LPCEPP-LN-BRACCT             ,"|"
               WS-LPCEPP-LN-ACCTNO             ,"|"
               WS-LPCEPP-LN-OWNBR              ,"|"
               WS-LPCEPP-CEPP1-DATE            ,"|"
               WS-LPCEPP-CEPP2-DATE            ,"|"
               WS-LPCEPP-LN-UNITPER-CD         ,"|"
               WS-LPCEPP-LN-UNITPER-FREQ       ,"|"
               WS-LPCEPP-LN-TOTNODEF           ,"|"
               WS-LPCEPP-LN-TOTNOALDEL         ,"|"
               WS-LPCEPP-CEPP-INCDEF-FG        ,"|"
               WS-LPCEPP-CEPP-INCALDEL-FG      ,"|"
               WS-LPCEPP-CEPP-NOCAP-FG         ,"|"
               WS-LPCEPP-DATE-CAP              ,"|"
               WS-LPCEPP-USE-DATE              ,"|"
               WS-LPCEPP-ELAPSED-DAYS          ,"|"
               WS-LPCEPP-CEPP-PERIODS          ,"|"
               WS-LPCEPP-CEPP-UNITPER-REM, CR-1            
                   DELIMITED BY "  " INTO WK-LPCEPP-REC.

               PERFORM WRITE-WK-LPCEPP-FILE.
               IF IO-BAD GO TO IO-ERROR.

       CREATE-LPIBPC-WORK SECTION.
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-LPIBPC-REC WS-LPIBPC-REC.
           MOVE WS-BRACCT TO WS-LPIBPC-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPIBPC-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPIBPC-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPIBPC-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LPIBPC-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPIBPC-LN-OWNBR, JUSTIFY-TYPE.

           MOVE TODAYS-DATE                  TO IBPC-DATE 
                                                DATE-YYYYMMDD. 
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY               TO WS-LPIBPC-DATE.

           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.                   
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPIBPC-LN-DATE-PAID-LAST.

           MOVE LN-JDDATE         TO DATE-YYYYMMDD.       
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPIBPC-LN-JDDATE.          

           MOVE LN-ACCELDATE      TO DATE-YYYYMMDD.       
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPIBPC-LN-ACCELDATE.       

           MOVE LN-REDUDATE       TO DATE-YYYYMMDD. 
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPIBPC-LN-REDUDATE.        

           MOVE LN-STATUSFG       TO WS-LPIBPC-LN-STATUS-FG.
           MOVE LN-STATFLAGS      TO WS-LPIBPC-LN-STATUSFLAGS.

           MOVE 0                 TO LN-TOTNODEF 
                                     WS-LPIBPC-LN-TOTNODEF.
           CALL "C$JUSTIFY" USING WS-LPIBPC-LN-TOTNODEF, JUSTIFY-TYPE.
      *USE ORIGINAL TERM, NOT EXTENDED ADDON TERM.
           MOVE "Y"               TO MDTE-ORGTERM-FG 
                                     WS-LPIBPC-MDTE-ORGTERM-FG.
      *ORIGINAL MATURITY
           MOVE "Y"               TO MDTE-FLAG 
                                     WS-LPIBPC-MDTE-FLAG.
      *SETTING OPTIONS FOR SP-EXPD FIELDS. 
           MOVE SPACES            TO SP-EXPD-FRMLA 
                                     WS-LPIBPC-SP-EXPD-FRMLA.
           MOVE ZEROS             TO  SP-EXPD-MONTHS 
                                      WS-LPIBPC-SP-EXPD-MONTHS.
           CALL "C$JUSTIFY" USING WS-LPIBPC-SP-EXPD-MONTHS.

           MOVE ZEROS             TO  SP-EXPD-DAYS
                                      WS-LPIBPC-SP-EXPD-DAYS.
           CALL "C$JUSTIFY" USING WS-LPIBPC-SP-EXPD-DAYS.     

           PERFORM UPDATE-LPIBPC-WORK-REC.

      *INCLUDE 3 DEFERMENTS     
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-LPIBPC-REC WS-LPIBPC-REC.
           MOVE WS-BRACCT TO WS-LPIBPC-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPIBPC-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPIBPC-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPIBPC-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LPIBPC-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPIBPC-LN-OWNBR, JUSTIFY-TYPE.

           MOVE TODAYS-DATE                  TO IBPC-DATE 
                                                DATE-YYYYMMDD. 
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY               TO WS-LPIBPC-DATE.

           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.                   
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPIBPC-LN-DATE-PAID-LAST.

           MOVE LN-JDDATE         TO DATE-YYYYMMDD.       
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPIBPC-LN-JDDATE.          

           MOVE LN-ACCELDATE      TO DATE-YYYYMMDD.       
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPIBPC-LN-ACCELDATE.       

           MOVE LN-REDUDATE       TO DATE-YYYYMMDD. 
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPIBPC-LN-REDUDATE.        

           MOVE LN-STATUSFG       TO WS-LPIBPC-LN-STATUS-FG.
           MOVE LN-STATFLAGS      TO WS-LPIBPC-LN-STATUSFLAGS.

           MOVE 3                 TO LN-TOTNODEF
                                     WS-LPIBPC-LN-TOTNODEF.
      *USE ORIGINAL TERM, NOT EXTENDED ADDON TERM.
           MOVE "Y"               TO MDTE-ORGTERM-FG 
                                     WS-LPIBPC-MDTE-ORGTERM-FG.
      *UPDATED  MATURITY
           MOVE " "               TO MDTE-FLAG 
                                     WS-LPIBPC-MDTE-FLAG.
      *DUE DATE CHANGES HAVE NO AFFECT ON MATURITY.
           MOVE SPACES            TO SP-EXPD-FRMLA 
                                     WS-LPIBPC-SP-EXPD-FRMLA.   
           MOVE 3                 TO  SP-EXPD-MONTHS 
                                      WS-LPIBPC-SP-EXPD-MONTHS.

           MOVE 15                TO  SP-EXPD-DAYS
                                      WS-LPIBPC-SP-EXPD-DAYS.

           PERFORM UPDATE-LPIBPC-WORK-REC.

      *UPDATED MATURITY, DUE DATE CHANGES AFFECT
           PERFORM SET-COMMON.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WK-LPIBPC-REC WS-LPIBPC-REC.
           MOVE WS-BRACCT TO WS-LPIBPC-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPIBPC-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPIBPC-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPIBPC-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LPIBPC-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPIBPC-LN-OWNBR, JUSTIFY-TYPE.

           MOVE TODAYS-DATE                  TO IBPC-DATE 
                                                DATE-YYYYMMDD. 
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY               TO WS-LPIBPC-DATE.

           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.                   
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPIBPC-LN-DATE-PAID-LAST.

           MOVE LN-JDDATE         TO DATE-YYYYMMDD.       
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPIBPC-LN-JDDATE.          

           MOVE LN-ACCELDATE      TO DATE-YYYYMMDD.       
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPIBPC-LN-ACCELDATE.       

           MOVE LN-REDUDATE       TO DATE-YYYYMMDD. 
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY     TO WS-LPIBPC-LN-REDUDATE.        

           MOVE LN-STATUSFG       TO WS-LPIBPC-LN-STATUS-FG.
           MOVE LN-STATFLAGS      TO WS-LPIBPC-LN-STATUSFLAGS.

           MOVE 3                 TO LN-TOTNODEF
                                     WS-LPIBPC-LN-TOTNODEF.
      *USE ORG   TERM                                    
           MOVE "Y"               TO MDTE-ORGTERM-FG 
                                     WS-LPIBPC-MDTE-ORGTERM-FG.
      *UPDATED  MATURITY
           MOVE "Y"               TO MDTE-FLAG 
                                     WS-LPIBPC-MDTE-FLAG.
      *DUE DATE CHANGES HAVE NO AFFECT ON MATURITY.
           MOVE "B"               TO SP-EXPD-FRMLA 
                                     WS-LPIBPC-SP-EXPD-FRMLA.   
           MOVE 3                 TO  SP-EXPD-MONTHS 
                                      WS-LPIBPC-SP-EXPD-MONTHS.

           MOVE 15                TO  SP-EXPD-DAYS
                                      WS-LPIBPC-SP-EXPD-DAYS.

           PERFORM UPDATE-LPIBPC-WORK-REC.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
          

       UPDATE-LPIBPC-WORK-REC SECTION.
           PERFORM IBPC-TEST.
           MOVE MDTE-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY       TO WS-LPIBPC-MDTE-DATE.      

           MOVE XDTE-DATE           TO DATE-YYYYMMDD.        
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY       TO WS-LPIBPC-EXPIR-DATE.
           MOVE IBPC-FG             TO WS-LPIBPC-IBPC-FG.
           MOVE IBPC-STAT           TO WS-LPIBPC-IBPC-STAT.
           MOVE IBPC-STAT-2         TO WS-LPIBPC-IBPC-STAT-2.
           MOVE IBPC-EXPIRED-FLAG TO
                                  WS-LPIBPC-IBPC-EXPIRED-FLAG.

           STRING
               WS-LPIBPC-LN-BRACCT             ,"|"
               WS-LPIBPC-LN-ACCTNO             ,"|"
               WS-LPIBPC-LN-OWNBR              ,"|"
               WS-LPIBPC-LN-DATE-PAID-LAST     ,"|" 
               WS-LPIBPC-LN-JDDATE             ,"|" 
               WS-LPIBPC-LN-ACCELDATE          ,"|" 
               WS-LPIBPC-LN-REDUDATE           ,"|" 
               WS-LPIBPC-LN-STATUS-FG          ,"|"  
               WS-LPIBPC-LN-STATUSFLAGS        ,"|" 
               WS-LPIBPC-LN-TOTNODEF           ,"|"
               WS-LPIBPC-MDTE-ORGTERM-FG       ,"|"        
               WS-LPIBPC-MDTE-FLAG             ,"|"        
               WS-LPIBPC-SP-EXPD-FRMLA         ,"|"        
               WS-LPIBPC-SP-EXPD-MONTHS        ,"|"        
               WS-LPIBPC-SP-EXPD-DAYS          ,"|"        
               WS-LPIBPC-MDTE-DATE             ,"|"
               WS-LPIBPC-EXPIR-DATE            ,"|" 
               WS-LPIBPC-DATE                  ,"|" 
               WS-LPIBPC-IBPC-FG               ,"|" 
               WS-LPIBPC-IBPC-STAT             ,"|" 
               WS-LPIBPC-IBPC-STAT-2           ,"|"
               WS-LPIBPC-IBPC-EXPIRED-FLAG, CR-1
                   DELIMITED  BY "  " INTO WK-LPIBPC-REC.

           PERFORM WRITE-WK-LPIBPC-FILE.
           IF IO-BAD GO TO IO-ERROR.

       CREATE-LNCOST-WORK SECTION.
           MOVE SPACES TO WS-LNCOST-REC WK-LNCOST-REC.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-LNCOST-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LNCOST-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LNCOST-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LNCOST-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LNCOST-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LNCOST-LN-OWNBR, JUSTIFY-TYPE.

      *THESE VALUES WONT CHANGE PER TEST CASE
           MOVE LN-LNAMT TO LNCOST-LNAMT WS-LNCOST-LNAMT.
           CALL "C$JUSTIFY" USING WS-LNCOST-LNAMT, JUSTIFY-TYPE.

           MOVE LN-DLPARVHELD TO LNCOST-DLPARVHELD 
                                         WS-LNCOST-DLPARVHELD.
           CALL "C$JUSTIFY" USING WS-LNCOST-DLPARVHELD, JUSTIFY-TYPE.

           MOVE LN-DLPARVPAID TO LNCOST-DLPARVPAID 
                                       WS-LNCOST-DLPARVPAID.
           CALL "C$JUSTIFY" USING WS-LNCOST-DLPARVPAID, JUSTIFY-TYPE.


      *DIDNT WANT TO ALTER ORIGINAL SPR VALUES SO FORCING THEM HERE.

      *TAKES LN-LNAMT * COST / 100

            IF HOLD-SP1-KEY = "GA0020101" OR "GA0400102" OR
                              "GA0032301" OR "GA0050103"
                   MOVE "LNAMT * COST" TO WS-LNCOST-TYPE
                   MOVE "A"     TO SP-LNCOST-FRMLA
                                    WS-LNCOST-SP-FRMLA 
                   MOVE 5.00       TO SP-LNCOST-AMT
                                   WS-LNCOST-SP-AMT  
            ELSE

      *FIXED AMOUNT PER LOAN
           IF HOLD-SP1-KEY = "GA0010102" OR "GA0010103" OR
                                            "GA0010104" OR
                             "GA0031001" OR "GA0031001"    
                              MOVE "FIXED AMT" TO WS-LNCOST-TYPE
                              MOVE "F"   TO SP-LNCOST-FRMLA
                                            WS-LNCOST-SP-FRMLA 
                              MOVE 25.00  TO SP-LNCOST-AMT 
                                           WS-LNCOST-SP-AMT

            ELSE
      *FLAT AMOUNT & UPDATES PARTICIPATION PAID(NOT TESTING)
            IF HOLD-SP1-KEY = "IL0060101" OR "IL0060201" 
                           MOVE "4"            TO SP-LNCOST-FRMLA
                                                WS-LNCOST-SP-FRMLA 
                           MOVE "FIXED AMT" TO WS-LNCOST-TYPE
                           MOVE 50.00  TO SP-LNCOST-AMT 
                                          WS-LNCOST-SP-AMT
            ELSE

      *FIXED AMOUNT PER LOAN, + TAKES PARTICIPATION PAID AND
      *PUTS IN LN-ANTICERN(4) (NOT TESTING THE MOVE TO LN-ANTICERN)

            IF HOLD-SP1-KEY = "GA0010401" OR "GA0010402" OR 
                                           "GA0013001"    
                              MOVE "5" TO SP-LNCOST-FRMLA
                                          WS-LNCOST-SP-FRMLA 
                              MOVE "FIXED AMT" TO WS-LNCOST-TYPE
                              MOVE 75.00  TO SP-LNCOST-AMT 
                                            WS-LNCOST-SP-AMT
            ELSE
      *BOOKED A CLASS 30 LOAN FOR THE LAST 2 LNCOST FORMULA'S, CLASS 30
      *IS A SALES CLASS.

      * 'P' IS DEALER PARTICIPATION HELD LOAN #9
           IF HOLD-SP1-KEY = "GA0300202" OR "GA0010202"
                   MOVE SP-LNCOST-FRMLA TO WS-LNCOST-SP-FRMLA
                   MOVE "DLPARVHELD"  TO WS-LNCOST-TYPE
                   MOVE SP-LNCOST-AMT TO WS-LNCOST-SP-AMT
            ELSE
      *BOOK CLASS 30 USING LNCOST 3, WHICH IS DEALER PARTIC PAID #10
           IF HOLD-SP1-KEY = "GA0300401"
                   MOVE SP-LNCOST-FRMLA TO  WS-LNCOST-SP-FRMLA 
                              MOVE "DLPARVPAID" TO WS-LNCOST-TYPE
                   MOVE SP-LNCOST-AMT TO WS-LNCOST-SP-AMT
            ELSE
                    MOVE "UNKNOWN" TO WS-LNCOST-TYPE 
                    MOVE ZEROS TO WS-LNCOST-SP-AMT
                                  WS-LNCOST-DLPARVHELD
                                  WS-LNCOST-DLPARVPAID.

           CALL "C$JUSTIFY" USING WS-LNCOST-SP-AMT,JUSTIFY-TYPE.


           PERFORM UPDATE-LNCOST-WORK-REC.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.

       UPDATE-LNCOST-WORK-REC SECTION.
           PERFORM COMPUTE-LOANCOST.
           MOVE LNCOST-LOANCOST    TO WS-LNCOST-LN-LOANCOST.
           CALL "C$JUSTIFY" USING WS-LNCOST-LN-LOANCOST,JUSTIFY-TYPE.
           
           STRING
               WS-LNCOST-LN-BRACCT             ,"|"
               WS-LNCOST-LN-ACCTNO             ,"|"
               WS-LNCOST-LN-OWNBR              ,"|"
               HOLD-SP1-KEY                    ,"|"
               WS-LNCOST-TYPE                  ,"|"
               WS-LNCOST-SP-FRMLA              ,"|"                
               WS-LNCOST-LNAMT                 ,"|"                      
               WS-LNCOST-SP-AMT                ,"|"
               WS-LNCOST-DLPARVHELD            ,"|"                
               WS-LNCOST-DLPARVPAID            ,"|"                
               WS-LNCOST-LN-LOANCOST, CR-1
                   DELIMITED  BY "  " INTO WK-LNCOST-REC.
               PERFORM WRITE-WK-LNCOST-FILE.

       CREATE-LPEARN-WORK SECTION.
      *WORLD USES 'R' WHICH ISNT IN LNCOST, ITS IN THE PROGRAM TO
      *ROLL THE UNEARNED DISCOUNT TO THE NEW LOAN ON A RENEWAL.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WS-LPEARN-REC WK-LPEARN-REC.
      *OTHER IS NOT INCLUDED IN ANTICIPATED

      *---------------------------------------------
      *LNCOST NOT INCLUDED IN ANTICIPATED EARNINGS
           MOVE "N"   TO GP-INCL-OTH-IN-EARNED 
                      WS-LPEARN-GP-INCL-OTH-IN-EARNED.          
      *---------------------------------------------
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE 0 TO CALCULATED-ORG-ANTIC(SUB)
           END-PERFORM.
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-LPEARN-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPEARN-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPEARN-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPEARN-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LPEARN-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPEARN-LN-OWNBR, JUSTIFY-TYPE.
         
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
                IF LN-POCD NOT = "BT"
                   ADD LN-ANTICADJ(SUB) LN-ANTICERN(SUB)
                                  GIVING CALCULATED-ORG-ANTIC(SUB)
                END-IF
                MOVE CALCULATED-ORG-ANTIC(SUB) TO 
                                    WS-LPEARN-LN-ORG-ANTIC-IN(SUB)
                MOVE LN-ANTICERN(SUB)  TO WS-LPEARN-LN-ANTICERN-IN(SUB)       
                MOVE LN-ANTICADJ(SUB)  TO WS-LPEARN-LN-ANTICADJ-IN(SUB)
                MOVE LN-UFERN(SUB)     TO WS-LPEARN-LN-UFERN-IN(SUB)
                MOVE LN-ACCERN(SUB)    TO WS-LPEARN-LN-ACCERN-IN(SUB)
               CALL "C$JUSTIFY" USING WS-LPEARN-LN-ORG-ANTIC-IN(SUB),
                                             JUSTIFY-TYPE
               CALL "C$JUSTIFY" USING WS-LPEARN-LN-ANTICERN-IN(SUB)
                                         JUSTIFY-TYPE
               CALL "C$JUSTIFY" USING WS-LPEARN-LN-ANTICADJ-IN(SUB)
                                         JUSTIFY-TYPE
               CALL "C$JUSTIFY" USING WS-LPEARN-LN-UFERN-IN(SUB)
                                         JUSTIFY-TYPE
               CALL "C$JUSTIFY" USING WS-LPEARN-LN-ACCERN-IN(SUB)
                                         JUSTIFY-TYPE
           END-PERFORM.
          

           PERFORM UPDATE-LPEARN-WORK-REC.
      *TEST 2
           MOVE SPACES TO WS-LPEARN-REC WK-LPEARN-REC.

      *---------------------------------------------
      *LNCOST(ANTICERN 6)  INCLUDED IN ANTICERN
      * FOR EXAMPLE LOAN: 75155 HAS COMPONENT 4 AND 6(USED IN TEST3)
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE "Y"   TO GP-INCL-OTH-IN-EARNED 
                      WS-LPEARN-GP-INCL-OTH-IN-EARNED.          
      *     IF HOLD-SP1-KEY = "GA0010202" 
      *          MOVE LN-LNAMT TO LNCOST-LNAMT.
      *---------------------------------------------
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE 0 TO CALCULATED-ORG-ANTIC(SUB)
           END-PERFORM.
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-LPEARN-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPEARN-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPEARN-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPEARN-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LPEARN-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPEARN-LN-OWNBR, JUSTIFY-TYPE.
         
           
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
                IF LN-POCD NOT = "BT"
                   ADD LN-ANTICADJ(SUB) LN-ANTICERN(SUB)
                                  GIVING CALCULATED-ORG-ANTIC(SUB)
                END-IF
                MOVE CALCULATED-ORG-ANTIC(SUB) TO 
                                    WS-LPEARN-LN-ORG-ANTIC-IN(SUB)
                MOVE LN-ANTICERN(SUB)  TO WS-LPEARN-LN-ANTICERN-IN(SUB)       
                MOVE LN-ANTICADJ(SUB)  TO WS-LPEARN-LN-ANTICADJ-IN(SUB)
                MOVE LN-UFERN(SUB)     TO WS-LPEARN-LN-UFERN-IN(SUB)
                MOVE LN-ACCERN(SUB)    TO WS-LPEARN-LN-ACCERN-IN(SUB)
               CALL "C$JUSTIFY" USING WS-LPEARN-LN-ORG-ANTIC-IN(SUB),
                                             JUSTIFY-TYPE
               CALL "C$JUSTIFY" USING WS-LPEARN-LN-ANTICERN-IN(SUB)
                                         JUSTIFY-TYPE
               CALL "C$JUSTIFY" USING WS-LPEARN-LN-ANTICADJ-IN(SUB)
                                         JUSTIFY-TYPE
               CALL "C$JUSTIFY" USING WS-LPEARN-LN-UFERN-IN(SUB)
                                         JUSTIFY-TYPE
               CALL "C$JUSTIFY" USING WS-LPEARN-LN-ACCERN-IN(SUB)
                                         JUSTIFY-TYPE
           END-PERFORM.


           PERFORM UPDATE-LPEARN-WORK-REC.

      *TEST 3, THIS WAS USED BY REGIONAL ACCEPTANCE, REQUIRES A SALES FINANCE
      * AND USING MAINTFEE TO AMORTIZE PARTICIPATION HELD.
      * LNCOST-FRMLA = 5, WHICH TAKES THE FLAT AMOUNT IN THE SPR TO 
      * LN-ANTICERN(6)  + * LN-DLPARVPAID TO LN-ANTICERN(4)
      * GA/30/5/1   LOAN 75155
%%         PERFORM RESTORE-ORIGINAL-FIELDS.
      *-----------------------------------------------------------
           MOVE SPACES TO WS-LPEARN-REC WK-LPEARN-REC.
      *-------------------------------------------------------
           MOVE "R"   TO GP-INCL-OTH-IN-EARNED 
                      WS-LPEARN-GP-INCL-OTH-IN-EARNED.          
           
      *---------------------------------------------------------
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE 0 TO CALCULATED-ORG-ANTIC(SUB)
           END-PERFORM.
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-LPEARN-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPEARN-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPEARN-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPEARN-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LPEARN-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPEARN-LN-OWNBR, JUSTIFY-TYPE.
         
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
                IF LN-POCD NOT = "BT"
                   ADD LN-ANTICADJ(SUB) LN-ANTICERN(SUB)
                                  GIVING CALCULATED-ORG-ANTIC(SUB)
                END-IF
                MOVE CALCULATED-ORG-ANTIC(SUB) TO 
                                    WS-LPEARN-LN-ORG-ANTIC-IN(SUB)
                MOVE LN-ANTICERN(SUB)  TO WS-LPEARN-LN-ANTICERN-IN(SUB)       
                MOVE LN-ANTICADJ(SUB)  TO WS-LPEARN-LN-ANTICADJ-IN(SUB)
                MOVE LN-UFERN(SUB)     TO WS-LPEARN-LN-UFERN-IN(SUB)
                MOVE LN-ACCERN(SUB)    TO WS-LPEARN-LN-ACCERN-IN(SUB)
               CALL "C$JUSTIFY" USING WS-LPEARN-LN-ORG-ANTIC-IN(SUB),
                                             JUSTIFY-TYPE
               CALL "C$JUSTIFY" USING WS-LPEARN-LN-ANTICERN-IN(SUB)
                                         JUSTIFY-TYPE
               CALL "C$JUSTIFY" USING WS-LPEARN-LN-ANTICADJ-IN(SUB)
                                         JUSTIFY-TYPE
               CALL "C$JUSTIFY" USING WS-LPEARN-LN-UFERN-IN(SUB)
                                         JUSTIFY-TYPE
               CALL "C$JUSTIFY" USING WS-LPEARN-LN-ACCERN-IN(SUB)
                                         JUSTIFY-TYPE
           END-PERFORM.


           PERFORM UPDATE-LPEARN-WORK-REC.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.

       UPDATE-LPEARN-WORK-REC SECTION.
           MOVE "OUTPUT"        TO WS-LPEARN-DESC.
           PERFORM COMPUTE-EARNINGS.
           MOVE "ANTICERN"       TO WS-LPEARN-ANTICERN-DESC.
           MOVE EARN-TOT-ANTICERN TO WS-LPEARN-EARN-TOT-ANTICERN 
           CALL "C$JUSTIFY" USING WS-LPEARN-EARN-TOT-ANTICERN,
                                              JUSTIFY-TYPE.

           MOVE "ANTICADJ"       TO WS-LPEARN-ANTICADJ-DESC.
           MOVE EARN-TOT-ANTICADJ TO WS-LPEARN-EARN-TOT-ANTICADJ 
           CALL "C$JUSTIFY" USING WS-LPEARN-EARN-TOT-ANTICADJ,
                                              JUSTIFY-TYPE.

           MOVE "UPFRONT"       TO WS-LPEARN-UFERN-DESC.    
           MOVE EARN-TOT-UFERN    TO WS-LPEARN-EARN-TOT-UFERN 
           CALL "C$JUSTIFY" USING WS-LPEARN-EARN-TOT-UFERN,     
                                              JUSTIFY-TYPE.

           MOVE "ACCERN"       TO WS-LPEARN-ACCERN-DESC.
           MOVE EARN-TOT-ACCERN   TO WS-LPEARN-EARN-TOT-ACCERN 
           CALL "C$JUSTIFY" USING WS-LPEARN-EARN-TOT-ACCERN,   
                                              JUSTIFY-TYPE.

           STRING
               WS-LPEARN-LN-BRACCT             ,"|"
               WS-LPEARN-LN-ACCTNO             ,"|"
               WS-LPEARN-LN-OWNBR              ,"|"
               WS-LPEARN-GP-INCL-OTH-IN-EARNED ,"|"      

               WS-LPEARN-LN-ORG-ANTICERN-1     ,"|"
               WS-LPEARN-LN-ANTICERN-1         ,"|"
               WS-LPEARN-LN-ANTICADJ-1         ,"|"
               WS-LN-UFERN-1                   ,"|"
               WS-LN-ACCERN-1                  ,"|"

               WS-LPEARN-LN-ORG-ANTICERN-2     ,"|"
               WS-LPEARN-LN-ANTICERN-2         ,"|"
               WS-LPEARN-LN-ANTICADJ-2         ,"|"
               WS-LN-UFERN-2                   ,"|"
               WS-LN-ACCERN-2                  ,"|"


               WS-LPEARN-LN-ORG-ANTICERN-3     ,"|"
               WS-LPEARN-LN-ANTICERN-3         ,"|"
               WS-LPEARN-LN-ANTICADJ-3         ,"|"
               WS-LN-UFERN-3                   ,"|"
               WS-LN-ACCERN-3                  ,"|"

               WS-LPEARN-LN-ORG-ANTICERN-4     ,"|"
               WS-LPEARN-LN-ANTICERN-4         ,"|"
               WS-LPEARN-LN-ANTICADJ-4         ,"|"
               WS-LN-UFERN-4                   ,"|"
               WS-LN-ACCERN-4                  ,"|"

               WS-LPEARN-LN-ORG-ANTICERN-5     ,"|"
               WS-LPEARN-LN-ANTICERN-5         ,"|"
               WS-LPEARN-LN-ANTICADJ-5         ,"|"
               WS-LN-UFERN-5                   ,"|"
               WS-LN-ACCERN-5                  ,"|"

               WS-LPEARN-LN-ORG-ANTICERN-6     ,"|"
               WS-LPEARN-LN-ANTICERN-6         ,"|"
               WS-LPEARN-LN-ANTICADJ-6         ,"|"
               WS-LN-UFERN-6                   ,"|"
               WS-LN-ACCERN-6                  ,"|"                
               WS-LPEARN-DESC                  ,"|"
               WS-LPEARN-ANTICERN-DESC         ,"|"
               WS-LPEARN-EARN-TOT-ANTICERN     ,"|"
               WS-LPEARN-ANTICADJ-DESC         ,"|"
               WS-LPEARN-EARN-TOT-ANTICADJ     ,"|"
               WS-LPEARN-UFERN-DESC            ,"|"
               WS-LPEARN-EARN-TOT-UFERN        ,"|"
               WS-LPEARN-ACCERN-DESC           ,"|"
               WS-LPEARN-EARN-TOT-ACCERN, CR-1
                  DELIMITED BY "  " INTO WK-LPEARN-REC.

               PERFORM WRITE-WK-LPEARN-FILE.
               IF IO-BAD
                   GO TO IO-ERROR.

       CREATE-LPCDEL-WORK SECTION.
      *WILL NEED TO TEST LPCDEL-LPCRNO-FG = "Y", IN CREATE-NOTICE
           PERFORM SET-COMMON.
           PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WS-LPCDEL-REC WK-LPCDEL-REC.
  
           MOVE 0 TO WS-LPCDEL-LTP-SCHLD-TERM-1.
           CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-TERM-1
                                               JUSTIFY-TYPE. 
           MOVE 0 TO WS-LPCDEL-LTP-SCHLD-PAYMNT-1.
           CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-PAYMNT-1
                                               JUSTIFY-TYPE. 

           MOVE 0 TO WS-LPCDEL-LTP-SCHLD-TERM-2. 
           CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-TERM-2
                                               JUSTIFY-TYPE. 
           MOVE 0 TO WS-LPCDEL-LTP-SCHLD-PAYMNT-2. 
           CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-PAYMNT-2 
                                               JUSTIFY-TYPE.
           MOVE 0 TO WS-LPCDEL-LTP-SCHLD-TERM-3.
           CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-TERM-3  
                                               JUSTIFY-TYPE. 
  
           MOVE 0 TO WS-LPCDEL-LTP-SCHLD-PAYMNT-3.
           CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-PAYMNT-3
                                               JUSTIFY-TYPE. 

           MOVE 0 TO WS-LPCDEL-LTP-SCHLD-TERM-4. 
           CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-TERM-4
                                               JUSTIFY-TYPE. 
           MOVE 0 TO WS-LPCDEL-LTP-SCHLD-PAYMNT-4. 
           CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-PAYMNT-4
                                               JUSTIFY-TYPE. 
           MOVE 0 TO WS-LPCDEL-LTP-SCHLD-TERM-5. 
           CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-TERM-5
                                               JUSTIFY-TYPE.
           MOVE 0 TO WS-LPCDEL-LTP-SCHLD-PAYMNT-5.
           CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-PAYMNT-5
                                               JUSTIFY-TYPE.

           MOVE WS-BRACCT TO WS-LPCDEL-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPCDEL-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPCDEL-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPCDEL-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LPCDEL-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPCDEL-LN-OWNBR, JUSTIFY-TYPE.

      *SP1-KEY
           MOVE SP1-KEY           TO WS-LPCDEL-SP-KEY.
      *SP-CONTRACTUAL-FRMLA
           MOVE SP-CONTRFRMLA       TO WS-LPCDEL-SP-CONTRFRMLA.

      *PAYMENTS TO DATE
           MOVE LN-TOTPAYMNTD       TO WS-LPCDEL-LN-TOTPAYMNTD.
           PERFORM CALL "C$JUSTIFY" USING WS-LPCDEL-LN-TOTPAYMNTD
                                          JUSTIFY-TYPE.
           MOVE LN-TOTLCHG          TO WS-LPCDEL-LN-TOTLCHG.
           CALL "C$JUSTIFY" USING WS-LPCDEL-LN-TOTLCHG,JUSTIFY-TYPE.

      *ADDON FG
           MOVE LN-ADDON-FG        TO WS-LPCDEL-LN-ADDON-FG.

      *PAY SCHEDULE FG
           MOVE LN-PAY-SCHLD-FG TO WS-LPCDEL-LN-PAY-SCHLD-FG.

      *CEPP1 DATE
           MOVE LN-1STPYDATE        TO CEPP1-DATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-LPCDEL-CEPP1-DATE.

      *CEPP2 DATE
           MOVE TODAYS-DATE       TO CEPP2-DATE, DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-LPCDEL-CEPP2-DATE.
      

           PERFORM UPDATE-LPCDEL-WORK-REC.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.

       UPDATE-LPCDEL-WORK-REC SECTION.
           PERFORM CONTRACTUAL-DELINQUENCY.

           IF LN-PAY-SCHLD-FG = "Y" 
               MOVE LTP-SCHLD-TERM(1) TO WS-LPCDEL-LTP-SCHLD-TERM-1 
               CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-TERM-1 
                                                    JUSTIFY-TYPE 
               MOVE LTP-SCHLD-PAYMNT(1) TO
                               WS-LPCDEL-LTP-SCHLD-PAYMNT-1 
               CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-PAYMNT-1 
                                                  JUSTIFY-TYPE 

%%             MOVE LTP-SCHLD-TERM(2) TO WS-LPCDEL-LTP-SCHLD-TERM-2 
               CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-TERM-2 
                                                       JUSTIFY-TYPE 
               MOVE LTP-SCHLD-PAYMNT(2) TO 
                                 WS-LPCDEL-LTP-SCHLD-PAYMNT-2  
               CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-PAYMNT-2 
                                                  JUSTIFY-TYPE 

%%             MOVE LTP-SCHLD-TERM(3) TO WS-LPCDEL-LTP-SCHLD-TERM-3 
               CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-TERM-3 
                                                       JUSTIFY-TYPE 
               MOVE LTP-SCHLD-PAYMNT(3) TO
                                     WS-LPCDEL-LTP-SCHLD-PAYMNT-3 
               CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-PAYMNT-3 
                                                  JUSTIFY-TYPE 
%%             MOVE LTP-SCHLD-TERM(4) TO WS-LPCDEL-LTP-SCHLD-TERM-4 
               CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-TERM-4 
                                                       JUSTIFY-TYPE 
               MOVE LTP-SCHLD-PAYMNT(4) TO
                                    WS-LPCDEL-LTP-SCHLD-PAYMNT-4 
               CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-PAYMNT-4 
                                                  JUSTIFY-TYPE 
%%             MOVE LTP-SCHLD-TERM(5) TO WS-LPCDEL-LTP-SCHLD-TERM-5 
               CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-TERM-5 
                                                       JUSTIFY-TYPE 
               MOVE LTP-SCHLD-PAYMNT(5) TO 
                                    WS-LPCDEL-LTP-SCHLD-PAYMNT-5 
               CALL "C$JUSTIFY" USING WS-LPCDEL-LTP-SCHLD-PAYMNT-5 
                                                  JUSTIFY-TYPE.
      *OUTPUT
           MOVE CDEL-DELINQUENT TO WS-LPCDEL-CDEL-DELINQUENT.
           CALL "C$JUSTIFY" USING WS-LPCDEL-CDEL-DELINQUENT,
                                        JUSTIFY-TYPE.

           MOVE CDEL-ARREARAGE TO  WS-LPCDEL-CDEL-ARREARAGE. 
           CALL "C$JUSTIFY" USING WS-LPCDEL-CDEL-ARREARAGE,
                                        JUSTIFY-TYPE.
 
           STRING
               WS-LPCDEL-LN-BRACCT             ,"|"
               WS-LPCDEL-LN-ACCTNO             ,"|"
               WS-LPCDEL-LN-OWNBR              ,"|"
               WS-LPCDEL-SP-KEY                ,"|"
               WS-LPCDEL-SP-CONTRFRMLA         ,"|"
               WS-LPCDEL-LN-TOTPAYMNTD         ,"|"
               WS-LPCDEL-LN-TOTLCHG            ,"|"
               WS-LPCDEL-LN-ADDON-FG           ,"|"
               WS-LPCDEL-LN-PAY-SCHLD-FG       ,"|"
               WS-LPCDEL-CDEL-LPCRNO-FG        ,"|"
               WS-LPCDEL-CEPP1-DATE            ,"|"
               WS-LPCDEL-CEPP2-DATE            ,"|"
%%             WS-LPCDEL-LTP-SCHLD-TERM-1      ,"|"
%%             WS-LPCDEL-LTP-SCHLD-PAYMNT-1     ,"|"
%%             WS-LPCDEL-LTP-SCHLD-TERM-2       ,"|"
%%             WS-LPCDEL-LTP-SCHLD-PAYMNT-2     ,"|"
%%             WS-LPCDEL-LTP-SCHLD-TERM-3       ,"|"
%%             WS-LPCDEL-LTP-SCHLD-PAYMNT-3     ,"|"
%%             WS-LPCDEL-LTP-SCHLD-TERM-4       ,"|"
%%             WS-LPCDEL-LTP-SCHLD-PAYMNT-4     ,"|"
%%             WS-LPCDEL-LTP-SCHLD-TERM-5       ,"|"
%%             WS-LPCDEL-LTP-SCHLD-PAYMNT-5     ,"|"

               WS-LPCDEL-CDEL-DELINQUENT       ,"|"
               WS-LPCDEL-CDEL-ARREARAGE,CR-1                         
                  DELIMITED BY "  " INTO WK-LPCDEL-REC.

           PERFORM WRITE-WK-LPCDEL-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

      *-------------------------------------------------
       CREATE-LPLCAS-WORK SECTION.
           PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WS-LPLCAS-REC WK-LPLCAS-REC.
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-LPLCAS-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPLCAS-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPLCAS-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPLCAS-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LPLCAS-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPLCAS-LN-OWNBR, JUSTIFY-TYPE.

           MOVE TODAYS-DATE TO LCAS-PAYDATE, DATE-YYYYMMDD.          
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-LPLCAS-LCAS-PAYDATE.

           MOVE LN-1STPYDATE  TO LCAS-1STPYDATE, DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-LPLCAS-LCAS-1STPYDATE.
          
           MOVE LN-LCPDTH-DATE   TO LCAS-LCPDTH-DATE, DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-LPLCAS-LCAS-LCPDTH-DATE.

%%    * SETUP NEXT, THIS IS DONE IN LPLCAS AS WELL.
           MOVE LN-LCPDTH-DATE TO NDTE-DATE.
           IF (LN-UNITPER-CD = "M" AND LN-UNITPER-FREQ = 1)
              IF (NDTE-DD < LN-1STPYDA)
                 MOVE LN-1STPYDA TO NDTE-DD.
           MOVE 1 TO NDTE-HOLD.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO DATE-YYYYMMDD.                  
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY   TO WS-LPLCAS-NEXT-DUE-DATE.

           MOVE LN-UNITPER-CD    TO WS-LPLCAS-LN-UNITPER-CD.

           PERFORM UPDATE-LPLCAS-WORK-REC.
           PERFORM RESTORE-ORIGINAL-FIELDS.

       UPDATE-LPLCAS-WORK-REC SECTION.

           PERFORM LATE-ASSESS-ROUTINE.

           MOVE GB-HOLIDAY-TAB TO WS-LPLCAS-PASS-HOLIDAY-TAB.

           MOVE LCAS-LCGRACE TO WS-LPLCAS-LCAS-LCGRACE.
           CALL "C$JUSTIFY" USING WS-LPLCAS-LCAS-LCGRACE,
                                           JUSTIFY-TYPE.

      *CALLS CL/ELREAD FOR LN-LC-CODE
           MOVE LCAS-GRACE-DATE TO DATE-YYYYMMDD.                  
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO  WS-LPLCAS-LCAS-GRACE-DATE.

           MOVE ELAPSED-DAYS   TO WS-LPLCAS-LCAS-ELAPSED-DAYS.
           CALL "C$JUSTIFY" USING WS-LPLCAS-LCAS-ELAPSED-DAYS,
                                                 JUSTIFY-TYPE.

           MOVE LCAS-ASSESS TO  WS-LPLCAS-LCAS-ASSESS.


           STRING
               WS-LPLCAS-LN-BRACCT             ,"|"
               WS-LPLCAS-LN-ACCTNO             ,"|"
               WS-LPLCAS-LN-OWNBR              ,"|"
               WS-LPLCAS-LCAS-PAYDATE          ,"|"
               WS-LPLCAS-LCAS-1STPYDATE        ,"|"
               WS-LPLCAS-LCAS-LCPDTH-DATE      ,"|"
               WS-LPLCAS-NEXT-DUE-DATE         ,"|"
               WS-LPLCAS-LN-UNITPER-CD         ,"|"
               WS-LPLCAS-PASS-HOLIDAY-TAB      ,"|"
               WS-LPLCAS-LCAS-LCGRACE          ,"|"
               WS-LPLCAS-LCAS-GRACE-DATE       ,"|"
               WS-LPLCAS-LCAS-ELAPSED-DAYS     ,"|"
               WS-LPLCAS-LCAS-ASSESS, CR-1                           
                  DELIMITED BY "  " INTO WK-LPLCAS-REC.
          
               PERFORM WRITE-WK-LPLCAS-FILE.
              IF IO-FG NOT = 0
                  GO TO IO-ERROR.
      *----------------------------------------------------
       CREATE-LPLCAP-WORK SECTION.
           PERFORM RESTORE-ORIGINAL-FIELDS.
%%         MOVE SPACES TO WS-LPLCAP-REC WK-LPLCAP-REC.
           PERFORM SET-COMMON.

      *NOT ALL ACCOUNTS WILL PERFORM THE LATE-CHARGE-APPLY ROUTINE
           MOVE ZEROS TO LCAP-PARTIALS,LCAP-LCPAID,LCAP-NOLCHG,
                         LCAP-APP,LCAP-OWE,LCAP-LCPDTH-DATE.
                         
      *INITIALIZE LP
         
           MOVE ZEROS TO HOLD-LP-TRAMT.

           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-LPLCAP-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPLCAP-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LPLCAP-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LN-OWNBR, JUSTIFY-TYPE.

           MOVE LN-LCPDTH-DATE  TO DATE-YYYYMMDD. 
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-LPLCAP-LN-LCPDTH-DATE.
 
           MOVE LN-LCBAL TO WS-LPLCAP-LN-LCBAL.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LN-LCBAL 
                                            JUSTIFY-TYPE.
 
           MOVE LN-INTBAL TO WS-LPLCAP-LN-INTBAL.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LN-INTBAL, JUSTIFY-TYPE.

           MOVE LN-OTHBAL TO WS-LPLCAP-LN-OTHBAL.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LN-OTHBAL,JUSTIFY-TYPE.

           MOVE LN-OT2BAL TO WS-LPLCAP-LN-OT2BAL.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LN-OT2BAL,JUSTIFY-TYPE.
           
           MOVE SP1-KEY          TO WS-LPLCAP-SP1-KEY.
           MOVE SP-LCFRMLA       TO WS-LPLCAP-SP-LCFRMLA.
           MOVE SP-LCTYPE        TO WS-LPLCAP-SP-LCTYPE.
           MOVE SP-LCRATE        TO WS-LPLCAP-SP-LCRATE.
           CALL "C$JUSTIFY" USING WS-LPLCAP-SP-LCRATE,JUSTIFY-TYPE.

           MOVE LN-OTHBAL          TO LP-OTHBAL.
           MOVE LN-OT2BAL          TO LP-OT2BAL.
           MOVE LN-LCBAL           TO LP-LCBAL.
           MOVE LN-INTBAL          TO LP-INTBAL.
           MOVE LN-CURBAL          TO LP-CURBAL.
           MOVE TODAYS-DATE        TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE IBPC-FG            TO LP-IBPC.           

           IF (IBPC-FG = "P" OR LN-LOANTYPE = "I")
                IF LN-JDDATE = 0 OR ( LN-JDDATE NOT = 0 AND
                                      SP-LC-ON-JUDGEMENTS NOT = "N")
                    MOVE 0 TO LCAP-TRAMT
                    MOVE TODAYS-DATE TO LCAP-PAYDATE
                    MOVE LN-1STPYDATE TO LCAP-1STPYDATE
                    MOVE "AL"    TO LCAP-LPTRCD
                    MOVE LP-APLC  TO LCAP-LPAPLC
                    MOVE LP-APINT  TO LCAP-LPAPINT 
                    PERFORM SETUP-PRINT-FIELDS
                    PERFORM LATE-CHARGE-APPLY 
                END-IF
            ELSE
                MOVE "LPLCAP SKIPPED" TO
                                  WS-LPLCAP-NOT-PERFORMED-MESS.
 
           PERFORM UPDATE-LPLCAP-WORK-REC.

      *-----------------------------------------------------------
      *TRY AN EMULATE A PY WITH A TRAN-AMOUNT
           PERFORM RESTORE-ORIGINAL-FIELDS.      
           MOVE SPACES TO WS-LPLCAP-REC WK-LPLCAP-REC.
           PERFORM SET-COMMON.

      *NOT ALL ACCOUNTS WILL PERFORM THE LATE-CHARGE-APPLY ROUTINE
           MOVE ZEROS TO LCAP-PARTIALS,LCAP-LCPAID,LCAP-NOLCHG,
                         LCAP-APP,LCAP-OWE,LCAP-LCPDTH-DATE.
                         
      *INITIALIZE LP
         
           MOVE ZEROS TO HOLD-LP-TRAMT.

           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-LPLCAP-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPLCAP-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LPLCAP-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LN-OWNBR, JUSTIFY-TYPE.
          
           MOVE LN-LCPDTH-DATE    TO DATE-YYYYMMDD.               
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-LPLCAP-LN-LCPDTH-DATE.
 
           MOVE LN-LCBAL          TO WS-LPLCAP-LN-LCBAL.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LN-LCBAL,JUSTIFY-TYPE.

           MOVE LN-INTBAL         TO WS-LPLCAP-LN-INTBAL.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LN-INTBAL,JUSTIFY-TYPE.

           MOVE LN-OTHBAL         TO WS-LPLCAP-LN-OTHBAL.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LN-OTHBAL,JUSTIFY-TYPE.

           MOVE LN-OT2BAL         TO WS-LPLCAP-LN-OT2BAL.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LN-OT2BAL,JUSTIFY-TYPE.

           MOVE SP1-KEY            TO WS-LPLCAP-SP1-KEY.
           MOVE SP-LCFRMLA         TO WS-LPLCAP-SP-LCFRMLA.
           MOVE SP-LCTYPE          TO WS-LPLCAP-SP-LCTYPE.
           MOVE SP-LCRATE          TO WS-LPLCAP-SP-LCRATE.
           CALL "C$JUSTIFY" USING WS-LPLCAP-SP-LCRATE, JUSTIFY-TYPE.

           MOVE LN-OTHBAL          TO LP-OTHBAL.
           MOVE LN-OT2BAL          TO LP-OT2BAL.
           MOVE LN-LCBAL           TO LP-LCBAL.
           MOVE LN-INTBAL          TO LP-INTBAL.
           MOVE LN-CURBAL          TO LP-CURBAL.
           MOVE TODAYS-DATE        TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE IBPC-FG            TO LP-IBPC.         
 
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-CUR-SCHD-PAY TO LP-TRAMT HOLD-LP-TRAMT.
%%         IF LP-OTHBAL NOT = 0
              IF LP-OTHBAL NOT > HOLD-LP-TRAMT
                  MOVE LP-OTHBAL TO LP-APOTH
                  SUBTRACT LP-OTHBAL FROM HOLD-LP-TRAMT
                  MOVE 0 TO LP-OTHBAL
              ELSE
                  MOVE HOLD-LP-TRAMT TO LP-APOTH
                  SUBTRACT HOLD-LP-TRAMT FROM LP-OTHBAL
                  MOVE 0 TO LP-APINT LP-APCUR LP-APLC
                       HOLD-LP-TRAMT.
%%          IF LP-LCBAL NOT = 0
               IF LP-LCBAL NOT > HOLD-LP-TRAMT
                  MOVE LP-LCBAL TO LP-APLC
                  SUBTRACT LP-LCBAL FROM HOLD-LP-TRAMT
                  MOVE 0 TO LP-LCBAL
               ELSE
                  MOVE HOLD-LP-TRAMT TO LP-APLC
                  SUBTRACT HOLD-LP-TRAMT FROM LP-LCBAL
                  MOVE 0 TO LP-APCUR LP-APINT HOLD-LP-TRAMT.
            IF LP-INTBAL NOT = 0
               IF LP-INTBAL NOT > HOLD-LP-TRAMT
                  MOVE LP-INTBAL TO LP-APINT
                  SUBTRACT LP-INTBAL FROM HOLD-LP-TRAMT
                  MOVE 0 TO LP-INTBAL
               ELSE
                  MOVE HOLD-LP-TRAMT TO LP-APINT
                  SUBTRACT HOLD-LP-TRAMT FROM LP-INTBAL
                   MOVE 0 TO LP-APCUR HOLD-LP-TRAMT.

           IF (LN-ACCELDATE = 0) AND
                (IBPC-FG = "P" OR LN-LOANTYPE = "I")
               IF SP-LCFRMLA NOT = " "
                  MOVE HOLD-LP-TRAMT     TO LCAP-TRAMT
                  MOVE "PY"              TO LCAP-LPTRCD
                  MOVE TODAYS-DATE       TO LCAP-PAYDATE
                  MOVE LN-1STPYDATE      TO LCAP-1STPYDATE
                  MOVE LP-APLC           TO LCAP-LPAPLC
                  MOVE LP-APINT          TO LCAP-LPAPINT                 
                  PERFORM SETUP-PRINT-FIELDS
                  PERFORM LATE-CHARGE-APPLY 
               END-IF
           ELSE
                  MOVE "LCAP SKIPPED" 
                             TO WS-LPLCAP-NOT-PERFORMED-MESS.
           PERFORM UPDATE-LPLCAP-WORK-REC.
           PERFORM RESTORE-ORIGINAL-FIELDS.
      *========================================================
       EXIT-LPLCAP-CREATE-WORK.     
        
       SETUP-PRINT-FIELDS SECTION.
           MOVE LCAP-PAYDATE TO DATE-YYYYMMDD.      
           PERFORM CONVERT-YYYYMMDD-TO-DDMMYYYY.
           MOVE DATE-MMDDYYYY TO WS-LPLCAP-LCAP-PAYDATE.

           MOVE HOLD-LP-TRAMT TO WS-LPLCAP-LCAP-TRAMT.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LCAP-TRAMT,
                                         JUSTIFY-TYPE.

           MOVE LCAP-LPTRCD TO WS-LPLCAP-LCAP-LPTRCD.

           MOVE LCAP-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-LPLCAP-LCAP-1STPYDATE.

           MOVE LCAP-LPAPLC  TO WS-LPLCAP-LCAP-LPAPLC.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LCAP-LPAPLC,
                                              JUSTIFY-TYPE.

           MOVE LCAP-LPAPINT   TO WS-LPLCAP-LCAP-LPAPINT. 
           CALL "C$JUSTIFY" USING WS-LPLCAP-LCAP-LPAPINT,
                                               JUSTIFY-TYPE.

       UPDATE-LPLCAP-WORK-REC SECTION.
           MOVE LCAP-PARTIALS   TO WS-LPLCAP-LCAP-PARTIALS.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LCAP-PARTIALS,
                                            JUSTIFY-TYPE.                         
           MOVE LCAP-LCPAID      TO WS-LPLCAP-LCAP-LCPAID.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LCAP-LCPAID,
                                                 JUSTIFY-TYPE.

           MOVE LCAP-NOLCHG     TO WS-LPLCAP-LCAP-NOLCHG.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LCAP-NOLCHG,
                                              JUSTIFY-TYPE.
           MOVE LCAP-APP        TO WS-LPLCAP-LCAP-APP.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LCAP-APP,JUSTIFY-TYPE.


           MOVE LCAP-OWE        TO WS-LPLCAP-LCAP-OWE.
           CALL "C$JUSTIFY" USING WS-LPLCAP-LCAP-OWE,JUSTIFY-TYPE.


           MOVE LCAP-LCPDTH-DATE TO DATE-YYYYMMDD.             
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-LPLCAP-LCAP-CALC-LCPDTH-DT.    


           ADD LCAP-OWE         TO LP-LCBAL GIVING 
                                     WS-LPLCAP-CALC-LP-LCBAL.
           CALL "C$JUSTIFY" USING WS-LPLCAP-CALC-LP-LCBAL,
                                                JUSTIFY-TYPE.
  
       SETUP-WS-LPLCAP-REC.
               STRING
               WS-LPLCAP-LN-BRACCT             ,"|"
               WS-LPLCAP-LN-ACCTNO             ,"|"
               WS-LPLCAP-LN-OWNBR              ,"|"
               WS-LPLCAP-LN-LCPDTH-DATE        ,"|"
               WS-LPLCAP-LN-LCBAL              ,"|"
               WS-LPLCAP-LN-INTBAL             ,"|"
               WS-LPLCAP-LN-OTHBAL             ,"|"                 
               WS-LPLCAP-LN-OT2BAL             ,"|"                 
               WS-LPLCAP-NOT-PERFORMED-MESS    ,"|"
               WS-LPLCAP-SP1-KEY               ,"|"
               WS-LPLCAP-SP-LCFRMLA            ,"|"
               WS-LPLCAP-SP-LCTYPE             ,"|"
               WS-LPLCAP-SP-LCRATE             ,"|"
               WS-LPLCAP-LCAP-PAYDATE          ,"|" 
               WS-LPLCAP-LCAP-1STPYDATE        ,"|"
               WS-LPLCAP-LCAP-LPTRCD           ,"|"
               WS-LPLCAP-LCAP-TRAMT            ,"|"
               WS-LPLCAP-LCAP-LPAPLC           ,"|"
               WS-LPLCAP-LCAP-LPAPINT          ,"|"
               WS-LPLCAP-LCAP-PARTIALS         ,"|"
               WS-LPLCAP-LCAP-LCPAID           ,"|"
               WS-LPLCAP-LCAP-NOLCHG           ,"|"
               WS-LPLCAP-LCAP-APP              ,"|"
               WS-LPLCAP-LCAP-OWE              ,"|"
               WS-LPLCAP-LCAP-CALC-LCPDTH-DT   ,"|"
               WS-LPLCAP-CALC-LP-LCBAL, CR-1 DELIMITED             
                            BY "  " INTO WK-LPLCAP-REC.
          

              PERFORM WRITE-WK-LPLCAP-FILE.
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

      *-----------------------------------------------------------------
       CREATE-LPPDUE-WORK SECTION.
           PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WS-LPPDUE-REC WK-LPPDUE-REC.
           PERFORM SET-COMMON.
           MOVE ZEROS TO MDTE-DATE.                  
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-LPPDUE-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPPDUE-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPPDUE-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPPDUE-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LPPDUE-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPPDUE-LN-OWNBR, JUSTIFY-TYPE.

      *CAPS AT MATURITY      
           MOVE "N" TO  GP-PDTH-GT-SCHED-PAY 
                                  WS-LPPDUE-GP-PDTH-GT-SCHED-PAY.

           MOVE LN-1STPYDATE    TO PDUE-1STPYDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-LPPDUE-PDUE-1STPYDATE.

 
           MOVE TODAYS-DATE TO PDUE-TODAY-DATE
                               DATE-YYYYMMDD. 
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-LPPDUE-PDUE-TODAY-DATE. 
           
           MOVE LN-UNITPER-CD TO WS-LPPDUE-LN-UNITPER-CD.


           PERFORM  UPDATE-LPPDUE-WORK-REC.

      *------------------------------------------------------
      *SET GP-PDTH-GT-SCHED-PAY TO "Y"  (CAP AT MATURITY)
           PERFORM RESTORE-ORIGINAL-FIELDS.
           PERFORM SET-COMMON.
           MOVE SPACES TO WS-LPPDUE-REC WK-LPPDUE-REC.
           MOVE ZEROS TO MDTE-DATE.                  
                         
           MOVE WS-BRACCT TO WS-LPPDUE-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPPDUE-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPPDUE-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPPDUE-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LPPDUE-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPPDUE-LN-OWNBR, JUSTIFY-TYPE.

           MOVE "Y" TO GP-PDTH-GT-SCHED-PAY   
                                  WS-LPPDUE-GP-PDTH-GT-SCHED-PAY.

           MOVE LN-1STPYDATE    TO PDUE-1STPYDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-LPPDUE-PDUE-1STPYDATE.

 
           MOVE TODAYS-DATE TO PDUE-TODAY-DATE
                               DATE-YYYYMMDD. 
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-LPPDUE-PDUE-TODAY-DATE. 
           
           MOVE LN-UNITPER-CD TO WS-LPPDUE-LN-UNITPER-CD.


           PERFORM  UPDATE-LPPDUE-WORK-REC.
%%         PERFORM RESTORE-ORIGINAL-FIELDS.

       UPDATE-LPPDUE-WORK-REC SECTION.
           PERFORM PAST-DUE-DAYS-CALCULATION.

           MOVE MDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY  TO WS-LPPDUE-MATURITY-DATE.

           MOVE PDTH-DATE-FULL TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-LPPDUE-PDTH-DATE-FULL.

           MOVE PDUE-DAYS TO WS-LPPDUE-PDUE-DAYS.
           CALL "C$JUSTIFY" USING WS-LPPDUE-PDUE-DAYS,JUSTIFY-TYPE.

       SETUP-WS-LPPDUE-REC.
               STRING
               WS-LPPDUE-LN-BRACCT             ,"|"
               WS-LPPDUE-LN-ACCTNO             ,"|"
               WS-LPPDUE-LN-OWNBR              ,"|"
               WS-LPPDUE-GP-PDTH-GT-SCHED-PAY ,"|"
               WS-LPPDUE-PDUE-1STPYDATE       ,"|"
               WS-LPPDUE-PDUE-TODAY-DATE      ,"|"
               WS-LPPDUE-LN-UNITPER-CD        ,"|"
               WS-LPPDUE-MATURITY-DATE        ,"|"
               WS-LPPDUE-PDTH-DATE-FULL       ,"|"
               WS-LPPDUE-PDUE-DAYS, CR-1 DELIMITED 
                    BY "  " INTO WK-LPPDUE-REC.

              PERFORM WRITE-WK-LPPDUE-FILE.
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

      *----------------------------------------------
       CREATE-AGEING-WORK SECTION.
           PERFORM RESTORE-ORIGINAL-FIELDS.
           MOVE SPACES TO WS-AGEING-REC WK-AGEING-REC.
                         
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-AGEING-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-AGEING-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-AGEING-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-AGEING-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-AGEING-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-AGEING-LN-OWNBR, JUSTIFY-TYPE.

           MOVE TODAYS-DATE TO AGEING-DATE, DATE-YYYYMMDD.        
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEING-AGEING-DATE.           

           MOVE SP1-KEY TO WS-AGEING-SP1-KEY.

           MOVE SP-BANKRULE   TO WS-AGEING-SP-BANKRULE.
           MOVE SP-CONTPDTH   TO WS-AGEING-SP-CONTPDTH.
           CALL "C$JUSTIFY" USING WS-AGEING-SP-CONTPDTH,
                                              JUSTIFY-TYPE.
           MOVE SP-CONTR-CREDIT-CD TO WS-AGEING-SP-CONTR-CREDIT-CD.
           MOVE SP-CONTR-CREDIT TO WS-AGEING-SP-CONTR-CREDIT.          

           MOVE LN-TOTPAYMNTD  TO WS-AGEING-LN-TOTPAYMNTD.
           CALL "C$JUSTIFY" USING WS-AGEING-LN-TOTPAYMNTD,
                                             JUSTIFY-TYPE.
           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-AGEING-LN-DATE-PAID-LAST.

           MOVE LN-PY-YYYYMM-RECDEL  TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY TO WS-AGEING-LN-PY-MMYY-RECDEL.

           MOVE LN-MTD-PAYS-RECDEL  TO WS-AGEING-LN-MTD-PAYS-RECDEL.
           CALL "C$JUSTIFY" USING WS-AGEING-LN-MTD-PAYS-RECDEL 
                                                 JUSTIFY-TYPE.

           PERFORM UPDATE-AGEING-WORK-REC.            
           PERFORM RESTORE-ORIGINAL-FIELDS.

       UPDATE-AGEING-WORK-REC SECTION.
           PERFORM COMPUTE-CONTRACTUAL.

           MOVE PDTH-PAYS TO WS-AGEING-PDTH-PAYS.
           CALL "C$JUSTIFY" USING WS-AGEING-PDTH-PAYS,
                                      JUSTIFY-TYPE.

           MOVE PDTH-REMAIN TO WS-AGEING-PDTH-REMAIN.
          
           MOVE PDTH-DATE-FULL TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-PDTH-DATE-FULL.

           MOVE CONTRACTUAL TO WS-AGEING-CONTRACTUAL.
           CALL "C$JUSTIFY" USING WS-AGEING-CONTRACTUAL,
                                            JUSTIFY-TYPE.

      *IF SP-BANKRULE = "D" RECENCY COMPUTED WITHIN COMPUTE-CONTRACTUAL
      *DONT PERFORM IF 'D'
      *    IF SP-BANKRULE NOT = "D" 
               PERFORM COMPUTE-RECENCY.

           MOVE RDSUB   TO WS-AGEING-RDSUB.
           CALL "C$JUSTIFY" USING WS-AGEING-RDSUB,JUSTIFY-TYPE.
 
           MOVE RSUB TO WS-AGEING-RSUB.
           CALL "C$JUSTIFY" USING WS-AGEING-RSUB,JUSTIFY-TYPE.
       

           MOVE RDPOT30  TO   WS-AGEING-RDPOT30.

           MOVE RDPOTCHGOFF TO   WS-AGEING-RDPOTCHGOFF.

           MOVE RECENCY TO WS-AGEING-RECENCY.
           CALL "C$JUSTIFY" USING WS-AGEING-RECENCY,JUSTIFY-TYPE.

           MOVE RECDEL TO WS-AGEING-RECDEL.
           CALL "C$JUSTIFY" USING WS-AGEING-RECDEL,
                                              JUSTIFY-TYPE.
          


       SETUP-WS-AGEING-REC.
               STRING
               WS-AGEING-LN-BRACCT             ,"|"
               WS-AGEING-LN-ACCTNO             ,"|"
               WS-AGEING-LN-OWNBR              ,"|"
               WS-AGEING-AGEING-DATE           ,"|"
               WS-AGEING-SP1-KEY               ,"|"
               WS-AGEING-SP-BANKRULE           ,"|"
               WS-AGEING-SP-CONTPDTH           ,"|"           
               WS-AGEING-SP-CONTR-CREDIT-CD    ,"|"
               WS-AGEING-SP-CONTR-CREDIT       ,"|"
               WS-AGEING-LN-TOTPAYMNTD         ,"|"
               WS-AGEING-LN-DATE-PAID-LAST     ,"|" 
               WS-AGEING-LN-PY-MMYY-RECDEL     ,"|"
               WS-AGEING-LN-MTD-PAYS-RECDEL    ,"|"
               WS-AGEING-PDTH-PAYS             ,"|"
               WS-AGEING-PDTH-REMAIN           ,"|"
               WS-AGEING-RDSUB                 ,"|"
               WS-AGEING-RSUB                  ,"|"
               WS-AGEING-RDPOT30               ,"|"
               WS-AGEING-RDPOTCHGOFF           ,"|"
               WS-AGEING-RECDEL                ,"|"
               WS-AGEING-CONTRACTUAL, CR-1
                   DELIMITED BY "  " INTO WK-AGEING-REC.         

           PERFORM WRITE-WK-AGEING-FILE.
           IF IO-FG NOT = 0
               GO TO IO-ERROR.


      *=============================================================
       SET-COMMON SECTION.
           MOVE 0 TO WS-OWNBR WS-NUMBER WS-LN-ACCTNO WS-LN-OWNBR.
           MOVE LN-OWNBR   TO WS-OWNBR.
           MOVE LN-ACCTNO  TO WS-NUMBER.
           
           MOVE LN-ACCTNO  TO WS-LN-ACCTNO.
           MOVE LN-OWNBR   TO WS-LN-OWNBR.

      *=============================================================




      ***************************
       PRINT-RANGE-LINES SECTION.
      ***************************

           MOVE 2 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-SELECTION
              MOVE ENT-GROUP TO RANGE1
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BR - END BR" TO RANGE-SELECTION
              MOVE BEG-BR TO RANGE1N
              MOVE END-BR TO RANGE2N
              PERFORM WRITE-DETAIL-LINE.


           MOVE "BEG CLASS - END CLASS" TO RANGE-SELECTION.
           MOVE BEG-CLASS TO RANGE1N.
           MOVE END-CLASS TO RANGE2N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG DATE - END DATE" TO RANGE-SELECTION.
           MOVE BEG-DATE TO RANGE1D.
           MOVE END-DATE TO RANGE2D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS READ" TO RANGE-SELECTION.
           MOVE TOTAL-RECORDS TO RANGE1N.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS OUT OF RANGE" TO RANGE-SELECTION.
           MOVE TOTAL-BYPASSED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS SUBMITTED" TO RANGE-SELECTION.
           MOVE TOTAL-INCLUDED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ****************************
      *    WRITE DETAIL LINE     *
      ****************************
       WRITE-DETAIL-LINE SECTION.
       WRITE-DET-LINE.
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
       EXIT-WRITE-DETAIL-LINE.
           EXIT.

      *******************************
      *    HEAD PAGE ROUTINE        *
      *******************************
       HEAD-PAGE SECTION.
       HEAD-ROUTINE.
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.



       COPY "LIBGB/GPENV.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPTBNO.CPY".
       COPY "LIBLP/LNCOST.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPPDUE.CPY".



      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO TESTR1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TESTR1-SYSDATE.
           DISPLAY TESTR1-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE TESTR1-FORM-FIELDS TO WR-BUF. |MJD001
           MOVE TESTR1-FORM-LIST TO WR-LIST. |MJD001
           PERFORM CALL-WRFORM. |MJD001

       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE TESTR1-HELP--FILE TO HELP-LINK-FILE.
           MOVE TESTR1-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/TESTR1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       SETUP-ORIGINAL-HOLD-FIELDS SECTION.
           MOVE SPACES TO ORIGINAL-FIELDS.
           MOVE ZEROS TO ORIGINAL-LN-TOTNODEF
                         ORIGINAL-LN-TOTNOALDEL
                         ORIGINAL-SP-EXPD-MONTHS
                         ORIGINAL-SP-EXPD-DAYS
                         ORIGINAL-SP-CONTR-CREDIT 
                         ORIGINAL-SP-LCMINCHG 
                         ORIGINAL-SP-LCMAXAMT 
                         ORIGINAL-SP-LNCOST-AMT
                         ORIGINAL-SP-LNCOST-FRMLA 
                         ORIGINAL-GP-INCL-OTH-IN-EARNED.


       SAVE-ORIGINAL-FIELDS SECTION.
           MOVE LN-TOTNODEF     TO ORIGINAL-LN-TOTNODEF.
           MOVE LN-TOTNOALDEL   TO ORIGINAL-LN-TOTNOALDEL.
           MOVE SP-EXPR-FRMLA   TO ORIGINAL-SP-EXPR-FRMLA.
           MOVE SP-EXPD-FRMLA   TO ORIGINAL-SP-EXPD-FRMLA.
           MOVE SP-EXPD-MONTHS  TO ORIGINAL-SP-EXPD-MONTHS.
           MOVE SP-EXPD-DAYS    TO ORIGINAL-SP-EXPD-DAYS.
           MOVE SP-CONTR-CREDIT-CD TO 
                                   ORIGINAL-SP-CONTR-CREDIT-CD.
           MOVE SP-CONTR-CREDIT  TO ORIGINAL-SP-CONTR-CREDIT.
           MOVE SP-LCMINCHG      TO ORIGINAL-SP-LCMINCHG.
           MOVE SP-LCMAXAMT      TO ORIGINAL-SP-LCMAXAMT.
           MOVE GP-INCL-OTH-IN-EARNED
                            TO ORIGINAL-GP-INCL-OTH-IN-EARNED.          
           MOVE SP-LNCOST-AMT TO  ORIGINAL-SP-LNCOST-AMT.
           MOVE SP-LNCOST-FRMLA TO ORIGINAL-SP-LNCOST-FRMLA.

       RESTORE-ORIGINAL-FIELDS SECTION.
           MOVE ORIGINAL-LN-TOTNODEF     TO LN-TOTNODEF.
           MOVE ORIGINAL-LN-TOTNOALDEL   TO LN-TOTNOALDEL.
           MOVE ORIGINAL-SP-EXPR-FRMLA   TO SP-EXPR-FRMLA.
           MOVE ORIGINAL-SP-EXPD-FRMLA   TO SP-EXPD-FRMLA.
           MOVE ORIGINAL-SP-EXPD-MONTHS  TO SP-EXPD-MONTHS.
           MOVE ORIGINAL-SP-EXPD-DAYS    TO SP-EXPD-DAYS.
           MOVE ORIGINAL-SP-CONTR-CREDIT-CD TO
                                     SP-CONTR-CREDIT-CD.
           MOVE ORIGINAL-SP-CONTR-CREDIT TO SP-CONTR-CREDIT.        
           MOVE ORIGINAL-SP-LCMINCHG    TO SP-LCMINCHG.
           MOVE ORIGINAL-SP-LCMAXAMT    TO SP-LCMAXAMT.
           MOVE ORIGINAL-GP-INCL-OTH-IN-EARNED TO
                              GP-INCL-OTH-IN-EARNED.
           MOVE ORIGINAL-SP-LNCOST-AMT TO SP-LNCOST-AMT.
           MOVE ORIGINAL-SP-LNCOST-FRMLA TO
                                        SP-LNCOST-FRMLA.
           MOVE SPACES TO MDTE-FLAG
                         MDTE-ORGTERM-FG
                         MDTE-DC-FLAG.
           MOVE SPACES TO UPER-SINGLE-PAY-CD.
           MOVE ZEROS  TO UPER-SINGLE-PAY-FREQ.
           MOVE ZEROS  TO PDTH-CREDIT.
           MOVE SPACES TO PDTH-DEF-FG
                          PDTH-ALDEL-FG.
           MOVE SPACES TO HOLD-NOCAP-FG.
           MOVE SPACES TO HOLD-INCDEF-FG, HOLD-INCALDEL-FG.
           MOVE SPACES TO HOLD-NOCAP-FG.
           MOVE SPACES TO CEPP-INCDEF-FG.
           MOVE SPACES TO CEPP-INCALDEL-FG.
           MOVE SPACES TO CEPP-NOCAP-FG.
           MOVE 0 TO ELAPSED-DAYS 
                     ELAPSED-MONTHS
                     ELAPSED-REM.


      ***********************
       MISC-ROUTINES SECTION.
      ***********************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETPR.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       COPY "LIBLP/LPLTPCL.CPY".

      **********************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
      **********************
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".

       OPEN-WK-GLOBRD-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-GLOBRD-IFN TO E-FILE.
           OPEN OUTPUT WK-GLOBRD-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-GLOBRD-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-GLOBRD-FILE
              GO TO OPEN-WK-GLOBRD-FILE-OUTPUT.
       WRITE-WK-GLOBRD-FILE.
           PERFORM WRITE-IT.
           MOVE WK-GLOBRD-IFN TO E-FILE.
           WRITE WK-GLOBRD-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-GLOBRD-FILE.
       CLOSE-WK-GLOBRD-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-GLOBRD-FILE.
       OPEN-WK-LPTBNO-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-LPTBNO-IFN TO E-FILE.
           OPEN OUTPUT WK-LPTBNO-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-LPTBNO-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-LPTBNO-FILE
              GO TO OPEN-WK-LPTBNO-FILE-OUTPUT.
       WRITE-WK-LPTBNO-FILE.
           PERFORM WRITE-IT.
           MOVE WK-LPTBNO-IFN TO E-FILE.
           WRITE WK-LPTBNO-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-LPTBNO-FILE.
       CLOSE-WK-LPTBNO-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-LPTBNO-FILE.

       OPEN-WK-MATURITY-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-MATURITY-IFN TO E-FILE.
           OPEN OUTPUT WK-MATURITY-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-MATURITY-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-MATURITY-FILE
              GO TO OPEN-WK-MATURITY-FILE-OUTPUT.
       WRITE-WK-MATURITY-FILE.
           PERFORM WRITE-IT.
           MOVE WK-MATURITY-IFN TO E-FILE.
           WRITE WK-MATURITY-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-MATURITY-FILE.
       CLOSE-WK-MATURITY-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-MATURITY-FILE.

       OPEN-WK-EXPIR-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-EXPIR-IFN TO E-FILE.
           OPEN OUTPUT WK-EXPIR-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-EXPIR-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-EXPIR-FILE
              GO TO OPEN-WK-EXPIR-FILE-OUTPUT.
       WRITE-WK-EXPIR-FILE.
           PERFORM WRITE-IT.
           MOVE WK-EXPIR-IFN TO E-FILE.
           WRITE WK-EXPIR-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-EXPIR-FILE.
       CLOSE-WK-EXPIR-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-EXPIR-FILE.

       OPEN-WK-INSURANCE-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-INSURANCE-IFN TO E-FILE.
           OPEN OUTPUT WK-INSURANCE-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-INSURANCE-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-INSURANCE-FILE
              GO TO OPEN-WK-INSURANCE-FILE-OUTPUT.
       WRITE-WK-INSURANCE-FILE.
           PERFORM WRITE-IT.
           MOVE WK-INSURANCE-IFN TO E-FILE.
           WRITE WK-INSURANCE-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-INSURANCE-FILE.
       CLOSE-WK-INSURANCE-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-INSURANCE-FILE.

       OPEN-WK-LPUPER-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-LPUPER-IFN TO E-FILE.
           OPEN OUTPUT WK-LPUPER-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-LPUPER-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-LPUPER-FILE
              GO TO OPEN-WK-LPUPER-FILE-OUTPUT.
       WRITE-WK-LPUPER-FILE.
           PERFORM WRITE-IT.
           MOVE WK-LPUPER-IFN TO E-FILE.
           WRITE WK-LPUPER-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-LPUPER-FILE.
       CLOSE-WK-LPUPER-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-LPUPER-FILE.

       OPEN-WK-LPAMTS-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-LPAMTS-IFN TO E-FILE.
           OPEN OUTPUT WK-LPAMTS-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-LPAMTS-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-LPAMTS-FILE
              GO TO OPEN-WK-LPAMTS-FILE-OUTPUT.
       WRITE-WK-LPAMTS-FILE.
           PERFORM WRITE-IT.
           MOVE WK-LPAMTS-IFN TO E-FILE.
           WRITE WK-LPAMTS-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-LPAMTS-FILE.
       CLOSE-WK-LPAMTS-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-LPAMTS-FILE.

       OPEN-WK-LPLCHG-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-LPLCHG-IFN TO E-FILE.
           OPEN OUTPUT WK-LPLCHG-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-LPLCHG-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-LPLCHG-FILE
              GO TO OPEN-WK-LPLCHG-FILE-OUTPUT.
       WRITE-WK-LPLCHG-FILE.
           PERFORM WRITE-IT.
           MOVE WK-LPLCHG-IFN TO E-FILE.
           WRITE WK-LPLCHG-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-LPLCHG-FILE.
       CLOSE-WK-LPLCHG-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-LPLCHG-FILE.

       OPEN-WK-SPPDTH-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-SPPDTH-IFN TO E-FILE.
           OPEN OUTPUT WK-SPPDTH-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-SPPDTH-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-SPPDTH-FILE
              GO TO OPEN-WK-SPPDTH-FILE-OUTPUT.
       WRITE-WK-SPPDTH-FILE.
           PERFORM WRITE-IT.
           MOVE WK-SPPDTH-IFN TO E-FILE.
           WRITE WK-SPPDTH-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-SPPDTH-FILE.
       CLOSE-WK-SPPDTH-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-SPPDTH-FILE.


       OPEN-WK-LPCEPP-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-LPCEPP-IFN TO E-FILE.
           OPEN OUTPUT WK-LPCEPP-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-LPCEPP-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-LPCEPP-FILE
              GO TO OPEN-WK-LPCEPP-FILE-OUTPUT.
       WRITE-WK-LPCEPP-FILE.
           PERFORM WRITE-IT.
           MOVE WK-LPCEPP-IFN TO E-FILE.
           WRITE WK-LPCEPP-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-LPCEPP-FILE.
       CLOSE-WK-LPCEPP-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-LPCEPP-FILE.


       OPEN-WK-LPIBPC-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-LPIBPC-IFN TO E-FILE.
           OPEN OUTPUT WK-LPIBPC-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-LPIBPC-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-LPIBPC-FILE
              GO TO OPEN-WK-LPIBPC-FILE-OUTPUT.
       WRITE-WK-LPIBPC-FILE.
           PERFORM WRITE-IT.
           MOVE WK-LPIBPC-IFN TO E-FILE.
           WRITE WK-LPIBPC-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-LPIBPC-FILE.
       CLOSE-WK-LPIBPC-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-LPIBPC-FILE.

       OPEN-WK-LNCOST-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-LNCOST-IFN TO E-FILE.
           OPEN OUTPUT WK-LNCOST-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-LNCOST-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-LNCOST-FILE
              GO TO OPEN-WK-LNCOST-FILE-OUTPUT.
       WRITE-WK-LNCOST-FILE.
           PERFORM WRITE-IT.
           MOVE WK-LNCOST-IFN TO E-FILE.
           WRITE WK-LNCOST-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-LNCOST-FILE.
       CLOSE-WK-LNCOST-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-LNCOST-FILE.

       OPEN-WK-LPEARN-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-LPEARN-IFN TO E-FILE.
           OPEN OUTPUT WK-LPEARN-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-LPEARN-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-LPEARN-FILE
              GO TO OPEN-WK-LPEARN-FILE-OUTPUT.
       WRITE-WK-LPEARN-FILE.
           PERFORM WRITE-IT.
           MOVE WK-LPEARN-IFN TO E-FILE.
           WRITE WK-LPEARN-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-LPEARN-FILE.
       CLOSE-WK-LPEARN-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-LPEARN-FILE.

       OPEN-WK-LPCDEL-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-LPCDEL-IFN TO E-FILE.
           OPEN OUTPUT WK-LPCDEL-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-LPCDEL-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-LPCDEL-FILE
              GO TO OPEN-WK-LPCDEL-FILE-OUTPUT.
       WRITE-WK-LPCDEL-FILE.
           PERFORM WRITE-IT.
           MOVE WK-LPCDEL-IFN TO E-FILE.
           WRITE WK-LPCDEL-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-LPCDEL-FILE.
       CLOSE-WK-LPCDEL-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-LPCDEL-FILE.

       OPEN-WK-LPLCAS-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-LPLCAS-IFN TO E-FILE.
           OPEN OUTPUT WK-LPLCAS-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-LPLCAS-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-LPLCAS-FILE
              GO TO OPEN-WK-LPLCAS-FILE-OUTPUT.
       WRITE-WK-LPLCAS-FILE.
           PERFORM WRITE-IT.
           MOVE WK-LPLCAS-IFN TO E-FILE.
           WRITE WK-LPLCAS-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-LPLCAS-FILE.
       CLOSE-WK-LPLCAS-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-LPLCAS-FILE.


       OPEN-WK-LPLCAP-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-LPLCAP-IFN TO E-FILE.
           OPEN OUTPUT WK-LPLCAP-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-LPLCAP-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-LPLCAP-FILE
              GO TO OPEN-WK-LPLCAP-FILE-OUTPUT.
       WRITE-WK-LPLCAP-FILE.
           PERFORM WRITE-IT.
           MOVE WK-LPLCAP-IFN TO E-FILE.
           WRITE WK-LPLCAP-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-LPLCAP-FILE.
       CLOSE-WK-LPLCAS-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-LPLCAP-FILE.


       OPEN-WK-LPPDUE-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-LPPDUE-IFN TO E-FILE.
           OPEN OUTPUT WK-LPPDUE-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-LPPDUE-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-LPPDUE-FILE
              GO TO OPEN-WK-LPPDUE-FILE-OUTPUT.
       WRITE-WK-LPPDUE-FILE.
           PERFORM WRITE-IT.
           MOVE WK-LPPDUE-IFN TO E-FILE.
           WRITE WK-LPPDUE-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-LPPDUE-FILE.
       CLOSE-WK-LPPDUE-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-LPPDUE-FILE.


       OPEN-WK-AGEING-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-AGEING-IFN TO E-FILE.
           OPEN OUTPUT WK-AGEING-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-AGEING-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-AGEING-FILE
              GO TO OPEN-WK-AGEING-FILE-OUTPUT.
       WRITE-WK-AGEING-FILE.
           PERFORM WRITE-IT.
           MOVE WK-AGEING-IFN TO E-FILE.
           WRITE WK-AGEING-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-AGEING-FILE.
       CLOSE-WK-AGEING-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-AGEING-FILE.




       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BR
              MOVE SV-END-BR TO END-BR.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY TESTR1-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
