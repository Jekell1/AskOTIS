      *  A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPSCHD.
      *****************************************************
      * CINDY
      *
      *    CREATES PIPE DELIMITED /USR/EXTRACT/             
      *  REV:   BREAK DOWN PAYMENT SCHEDULE                   
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

           SELECT WK-LPSCHD-FILE ASSIGN OUTPUT WK-LPSCHD-PATH
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
       FD  WK-LPSCHD-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-LPSCHD-REC.
           03  WK-LPSCHD-BYTE        PIC X OCCURS 2000 TIMES.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/SPSCHD S35 03/31/23-09:24:22 >".

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


       01  WK-LPSCHD-PATH.
           03  WK-LPSCHD-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-LPSCHD-FILENAME               PIC X(08) 
                 VALUE "/LPSCHD.".                    

           03  WK-LPSCHD-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-LPSCHD-PATH-YR                PIC 99.
           03  WK-LPSCHD-PATH-MO                PIC 99.
           03  WK-LPSCHD-PATH-DA                PIC 99.


       01  WS-LPSCHD-REC.
           03  WS-LPSCHD-LN-BRACCT                PIC Z(14).
           03  WS-LPSCHD-LN-ACCTNO                PIC Z(10).
           03  WS-LPSCHD-LN-OWNBR                 PIC Z(6).
           03  WS-LPSCHD-SP1-KEY.
                  05  WS-LPSCHD-ST                PIC X(2).
                  05  WS-LPSCHD-CLASS             PIC 9(3).
                  05  WS-LPSCHD-SUBCLASS          PIC 9(2).
                  05  WS-LPSCHD-LAWCODE           PIC 9(2).
           03  WS-LPSCHD-SCHD-TERM                PIC ZZZZ9.
           03  WS-LPSCHD-PAYMENT-TBL.
               05  WS-LPSCHD-SCHD-AMT   OCCURS 100    PIC ZZZZZZZ9.99.
           03  WS-LPSCHD-PMT-TBL REDEFINES WS-LPSCHD-PAYMENT-TBL.
               05  WS-LPSCHD-SCHD-AMT-1           PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-2           PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-3           PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-4           PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-5           PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-6           PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-7           PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-8           PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-9           PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-10          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-11          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-12          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-13          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-14          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-15          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-16          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-17          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-18          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-19          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-20          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-21          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-22          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-23          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-24          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-25          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-26          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-27          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-28          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-29          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-30          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-31          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-32          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-33          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-34          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-35          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-36          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-37          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-38          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-39          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-40          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-41          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-42          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-43          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-44          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-45          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-46          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-47          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-48          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-49          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-50          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-51          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-52          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-53          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-54          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-55          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-56          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-57          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-58          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-59          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-60          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-61          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-62          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-63          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-64          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-65          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-66          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-67          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-68          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-69          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-70          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-71          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-72          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-73          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-74          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-75          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-76          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-77          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-78          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-79          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-80          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-81          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-82          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-83          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-84          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-85          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-86          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-87          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-88          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-89          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-90          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-91          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT92           PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-93          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-94          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-95          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-96          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-97          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-98          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-99          PIC ZZZZZZZ9.99.
               05  WS-LPSCHD-SCHD-AMT-100         PIC ZZZZZZZ9.99.








       01  WK-LPSCHD-IFN        PIC X(6)     VALUE "LPSCHD".



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

       01  SUB                  PIC 999.
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
           03  ORIGINAL-SP-LPSCHD-AMT       PIC S9(7)V99.
           03  ORIGINAL-SP-LPSCHD-FRMLA     PIC X.

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
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".

      *COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBGB/GETENVW.CPY".
      *COPY "LIBCL/GLOBRDW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
      *COPY "LIBLP/LPUPERW.CPY".
      *COPY "LIBLP/LPUPWKW.CPY".
      *COPY "LIBLP/LPXDTEW.CPY".
      *COPY "LIBLP/LPITRMW.CPY".
      *COPY "LIBLP/LPAMTSW.CPY".
      *COPY "LIBLP/LPLCHGW.CPY".
      *COPY "LIBLP/LPCEPPW.CPY". 
      *COPY "LIBLP/LNCOSTW.CPY".
      *COPY "LIBLP/LPEARNW.CPY".
      *COPY "LIBLP/LPIBPCW.CPY".
      *COPY "LIBLP/LPCDELW.CPY".
      *COPY "LIBLP/LPLCASW.CPY".
      *COPY "LIBLP/LPLCAPW.CPY".
      *COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUT/TESTR2_DEF.CPY".
       COPY "LIBUT/TESTR2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/TESTR2_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-LPSCHD-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               PR-FILE WK-LPSCHD-FILE.

       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      *************************************
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TESTR2" TO VDU-FORM-NAME.

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
              GO TO END-GLOBAL-UPDATE.
           IF LN-ACCTNO = 77066  
      D    STOP MESS
             MOVE SPACES TO MESS.

           ADD 1 TO TOTAL-RECORDS.

           IF LN-OWNBR > END-BR
              GO TO EXIT-GLOBAL-UPDATE.

           IF LN-PAY-SCHLD-FG NOT = "Y"
                GO TO NEXT-LN.

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
           
          
           PERFORM CREATE-LPSCHD-WORK.  

           GO TO NEXT-LN.
       END-GLOBAL-UPDATE.

       EXIT-GLOBAL-UPDATE.
           EXIT.





      /**********************************
      *   I N I T I A L I Z A T I O N   *
      ***********************************
       INITIALIZATION SECTION.
           MOVE TESTR2-QUEUE-POS TO Q-POS.
           MOVE TESTR2-COPIES-POS TO C-POS.
           MOVE TESTR2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.

           PERFORM OPEN-PR-FILE.

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


           MOVE HOLD-FIL-MACHINE TO WK-LPSCHD-MACHINE.

           MOVE TODAYS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY-DD TO WK-LPSCHD-PATH-DA.
           MOVE DATE-MMDDYY-MM TO WK-LPSCHD-PATH-MO.
           MOVE DATE-MMDDYY-YY TO WK-LPSCHD-PATH-YR.

           CALL "C$TOLOWER" USING WK-LPSCHD-PATH1,VALUE 12.
           PERFORM OPEN-WK-LPSCHD-FILE-OUTPUT.
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
           MOVE "E RA000BR1." TO VDU.
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
           
           GO TO EXIT-PARM.
      *-----------------------------------------------------------
       ENTER-ALL-ACCTNO.
           MOVE "SNNN060ACCT1." TO VDU.
           MOVE 0 TO BEG-ACCTNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060ACCT2." TO VDU.
           MOVE 999999 TO END-ACCTNO VDUNUM0.
           PERFORM SCREENIO.          


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
    
      ****************************
       CREATE-LPSCHD-WORK SECTION.
      ****************************
           MOVE SPACES TO WS-LPSCHD-REC WK-LPSCHD-REC.
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-LPSCHD-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-LPSCHD-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-LPSCHD-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-LPSCHD-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-LPSCHD-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-LPSCHD-LN-OWNBR, JUSTIFY-TYPE.

           PERFORM UPDATE-LPSCHD-WORK-REC.

       UPDATE-LPSCHD-WORK-REC SECTION.
           PERFORM GET-LT-P-REC.
           PERFORM UNPACK-PAYMENT-SCHEDULE.
      *    MOVE SCHD-TERM    TO WS-LPSCHD-SCHD-TERM.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 100
              MOVE SCHD-AMT(SUB)  TO WS-LPSCHD-SCHD-AMT(SUB)
              CALL "C$JUSTIFY" USING WS-LPSCHD-SCHD-AMT(SUB),
                                            JUSTIFY-TYPE 
           END-PERFORM.
           
           STRING
                   WS-LPSCHD-LN-BRACCT            ,"|"
                   WS-LPSCHD-LN-ACCTNO            ,"|"
                   WS-LPSCHD-LN-OWNBR             ,"|"
                   HOLD-SP1-KEY                   ,"|"
                   WS-LPSCHD-SCHD-AMT-1           ,"|"    
                   WS-LPSCHD-SCHD-AMT-2           ,"|"   
                   WS-LPSCHD-SCHD-AMT-3           ,"|"  
                   WS-LPSCHD-SCHD-AMT-4           ,"|" 
                   WS-LPSCHD-SCHD-AMT-5           ,"|"
                   WS-LPSCHD-SCHD-AMT-6           ,"|"  
                   WS-LPSCHD-SCHD-AMT-7           ,"|" 
                   WS-LPSCHD-SCHD-AMT-8           ,"|"
                   WS-LPSCHD-SCHD-AMT-9           ,"|"
                   WS-LPSCHD-SCHD-AMT-10          ,"|"
                   WS-LPSCHD-SCHD-AMT-11          ,"|"
                   WS-LPSCHD-SCHD-AMT-12          ,"|"
                   WS-LPSCHD-SCHD-AMT-13          ,"|"
                   WS-LPSCHD-SCHD-AMT-14          ,"|"
                   WS-LPSCHD-SCHD-AMT-15          ,"|"
                   WS-LPSCHD-SCHD-AMT-16          ,"|"
                   WS-LPSCHD-SCHD-AMT-17          ,"|"
                   WS-LPSCHD-SCHD-AMT-18          ,"|"
                   WS-LPSCHD-SCHD-AMT-19          ,"|"
                   WS-LPSCHD-SCHD-AMT-20          ,"|"
                   WS-LPSCHD-SCHD-AMT-21          ,"|"
                   WS-LPSCHD-SCHD-AMT-22          ,"|"
                   WS-LPSCHD-SCHD-AMT-23          ,"|"
                   WS-LPSCHD-SCHD-AMT-24          ,"|"
                   WS-LPSCHD-SCHD-AMT-25          ,"|"             
                   WS-LPSCHD-SCHD-AMT-26          ,"|"             
                   WS-LPSCHD-SCHD-AMT-27          ,"|"             
                   WS-LPSCHD-SCHD-AMT-28          ,"|"
                   WS-LPSCHD-SCHD-AMT-29          ,"|"
                   WS-LPSCHD-SCHD-AMT-30          ,"|"
                   WS-LPSCHD-SCHD-AMT-31          ,"|"
                   WS-LPSCHD-SCHD-AMT-32          ,"|"
                   WS-LPSCHD-SCHD-AMT-33          ,"|"
                   WS-LPSCHD-SCHD-AMT-34          ,"|"
                   WS-LPSCHD-SCHD-AMT-35          ,"|"
                   WS-LPSCHD-SCHD-AMT-36          ,"|"
                   WS-LPSCHD-SCHD-AMT-37          ,"|"
                   WS-LPSCHD-SCHD-AMT-38          ,"|"
                   WS-LPSCHD-SCHD-AMT-39          ,"|"
                   WS-LPSCHD-SCHD-AMT-40          ,"|"
                   WS-LPSCHD-SCHD-AMT-41          ,"|"
                   WS-LPSCHD-SCHD-AMT-42          ,"|"
                   WS-LPSCHD-SCHD-AMT-43          ,"|"
                   WS-LPSCHD-SCHD-AMT-44          ,"|"
                   WS-LPSCHD-SCHD-AMT-45          ,"|"
                   WS-LPSCHD-SCHD-AMT-46          ,"|"
                   WS-LPSCHD-SCHD-AMT-47          ,"|"
                   WS-LPSCHD-SCHD-AMT-48          ,"|"
                   WS-LPSCHD-SCHD-AMT-49          ,"|"
                   WS-LPSCHD-SCHD-AMT-50          ,"|"
                   WS-LPSCHD-SCHD-AMT-51          ,"|"
                   WS-LPSCHD-SCHD-AMT-52          ,"|"
                   WS-LPSCHD-SCHD-AMT-53          ,"|"
                   WS-LPSCHD-SCHD-AMT-54          ,"|"
                   WS-LPSCHD-SCHD-AMT-55          ,"|"
                   WS-LPSCHD-SCHD-AMT-56          ,"|"
                   WS-LPSCHD-SCHD-AMT-57          ,"|"
                   WS-LPSCHD-SCHD-AMT-58          ,"|"
                   WS-LPSCHD-SCHD-AMT-59          ,"|"
                   WS-LPSCHD-SCHD-AMT-60          ,"|"
                   WS-LPSCHD-SCHD-AMT-61          ,"|"
                   WS-LPSCHD-SCHD-AMT-62          ,"|"
                   WS-LPSCHD-SCHD-AMT-63          ,"|"
                   WS-LPSCHD-SCHD-AMT-64          ,"|"
                   WS-LPSCHD-SCHD-AMT-65          ,"|"
                   WS-LPSCHD-SCHD-AMT-66          ,"|"
                   WS-LPSCHD-SCHD-AMT-67          ,"|"
                   WS-LPSCHD-SCHD-AMT-68          ,"|"
                   WS-LPSCHD-SCHD-AMT-69          ,"|"
                   WS-LPSCHD-SCHD-AMT-70          ,"|"
                   WS-LPSCHD-SCHD-AMT-71          ,"|"
                   WS-LPSCHD-SCHD-AMT-72          ,"|"
                   WS-LPSCHD-SCHD-AMT-73          ,"|"
                   WS-LPSCHD-SCHD-AMT-74          ,"|"
                   WS-LPSCHD-SCHD-AMT-75          ,"|"                    
                   WS-LPSCHD-SCHD-AMT-76          ,"|"
                   WS-LPSCHD-SCHD-AMT-77          ,"|"
                   WS-LPSCHD-SCHD-AMT-78          ,"|"
                   WS-LPSCHD-SCHD-AMT-79          ,"|"
                   WS-LPSCHD-SCHD-AMT-80          ,"|"
                   WS-LPSCHD-SCHD-AMT-81          ,"|"
                   WS-LPSCHD-SCHD-AMT-82          ,"|"
                   WS-LPSCHD-SCHD-AMT-83          ,"|"
                   WS-LPSCHD-SCHD-AMT-84          ,"|"
                   WS-LPSCHD-SCHD-AMT-85          ,"|"
                   WS-LPSCHD-SCHD-AMT-86          ,"|"
                   WS-LPSCHD-SCHD-AMT-87          ,"|"
                   WS-LPSCHD-SCHD-AMT-88          ,"|"
                   WS-LPSCHD-SCHD-AMT-89          ,"|"
                   WS-LPSCHD-SCHD-AMT-90          ,"|"
                   WS-LPSCHD-SCHD-AMT-91          ,"|"
                   WS-LPSCHD-SCHD-AMT92           ,"|"
                   WS-LPSCHD-SCHD-AMT-93          ,"|"
                   WS-LPSCHD-SCHD-AMT-94          ,"|"
                   WS-LPSCHD-SCHD-AMT-95          ,"|"
                   WS-LPSCHD-SCHD-AMT-96          ,"|"
                   WS-LPSCHD-SCHD-AMT-97          ,"|"
                   WS-LPSCHD-SCHD-AMT-98          ,"|"
                   WS-LPSCHD-SCHD-AMT-99          ,"|"
                   WS-LPSCHD-SCHD-AMT-100,CR-1 DELIMITED 
                      BY "  " INTO WK-LPSCHD-REC.


            PERFORM WRITE-WK-LPSCHD-FILE.
            IF IO-BAD GO TO IO-ERROR.


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
      *COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPMDTE.CPY".
      *COPY "LIBLP/LPUPWK.CPY".
      *COPY "LIBLP/LPUPER.CPY".
      *COPY "LIBLP/LPXDTE.CPY".
      *COPY "LIBLP/LPITRM.CPY".
      *COPY "LIBLP/LPAMTS.CPY".
      *COPY "LIBLP/LPLCHG.CPY".
      *COPY "LIBLP/LPCEPP.CPY".
      *COPY "LIBLP/LPIBPC.CPY".
      *COPY "LIBLP/LPTBNO.CPY".
      *COPY "LIBLP/LNCOST.CPY".
      *COPY "LIBLP/LPEARN.CPY".
      *COPY "LIBLP/LPCDEL.CPY".
      *COPY "LIBLP/LPLCAS.CPY".
      *COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPSCHD.CPY".
      *COPY "LIBLP/LPPDUE.CPY".



      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO TESTR2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TESTR2-SYSDATE.
           DISPLAY TESTR2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE TESTR2-FORM-FIELDS TO WR-BUF. |MJD001
           MOVE TESTR2-FORM-LIST TO WR-LIST. |MJD001
           PERFORM CALL-WRFORM. |MJD001

       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE TESTR2-HELP--FILE TO HELP-LINK-FILE.
           MOVE TESTR2-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/TESTR2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


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


       OPEN-WK-LPSCHD-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-LPSCHD-IFN TO E-FILE.
           OPEN OUTPUT WK-LPSCHD-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-LPSCHD-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-LPSCHD-FILE
              GO TO OPEN-WK-LPSCHD-FILE-OUTPUT.
       WRITE-WK-LPSCHD-FILE.
           PERFORM WRITE-IT.
           MOVE WK-LPSCHD-IFN TO E-FILE.
           WRITE WK-LPSCHD-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-LPSCHD-FILE.
       CLOSE-WK-LPSCHD-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-LPSCHD-FILE.


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
              DESTROY TESTR2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
