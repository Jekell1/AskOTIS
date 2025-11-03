       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IODSIN.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-16-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IODSIN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : DSFILE 
      * TABLE DESC    : DEALER STATISTICS FILE
      * OUTPUT FILE   : XPC/IODSIN.PRE
      * USED FOR      : LIBLP/LPWSDS.CPY  --- QDS-REC
      *                 LIBLP/LPDS1R.CPY  --- READ
      *                 LIBLP/LPDS1IN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0416 ***NEW***
      *
      ******************************************************************
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

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)IO/IODSIN.CBL S34 09/29/21-08:25:18 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".

      *=================================================================
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LPWSDS.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QDS1-WHERE-BEG-KEY.
           03  QDS1-WBEG-DATE-CCYYMM      PIC 9(06).
           03  QDS1-WBEG-BRANCH           PIC 9(04).
           03  QDS1-WBEG-NUM              PIC 9(06).
           03  QDS1-WBEG-CLASS            PIC 9(03).
 
       01  QDS1-WHERE-END-KEY.
           03  QDS1-WEND-DATE-CCYYMM      PIC 9(06).
           03  QDS1-WEND-BRANCH             PIC 9(04).
           03  QDS1-WEND-NUM              PIC 9(06).
           03  QDS1-WEND-CLASS            PIC 9(03).

       01  QDS-REC.
           03  QDS-DATE-CCYYMM      PIC 9(6).
           03  QDS-BRANCH           PIC 9(4).
           03  QDS-NUM              PIC 9(6).
           03  QDS-CLASS            PIC 9(3).
           03  QDS-DLRVPRIN-1        PIC S9(8)V99.
           03  QDS-DLRVPRIN-2        PIC S9(8)V99.
           03  QDS-DLRVPRIN-3        PIC S9(8)V99.
           03  QDS-DLRVCHGS-1        PIC S9(8)V99.
           03  QDS-DLRVCHGS-2        PIC S9(8)V99.
           03  QDS-DLRVCHGS-3        PIC S9(8)V99.
           03  QDS-DLPARVHELD        PIC S9(8)V99.
           03  QDS-UNPAIDCK          PIC S9(8)V99.
           03  QDS-DLDISC            PIC S9(8)V99.
           03  QDS-DLPARVPAID        PIC S9(8)V99.
           03  QDS-SUMWGTTM          PIC S9(8).
           03  QDS-SUMWGTAV          PIC S9(9)V99.
           03  QDS-SUMWGTTA          PIC S9(11)V99.
           03  QDS-RCOURSE-UNITS-1   PIC S9(5).
           03  QDS-RCOURSE-UNITS-2   PIC S9(5).
           03  QDS-RCOURSE-UNITS-3   PIC S9(5).
           03  QDS-RCOURSE-AMT-1     PIC S9(9)V99.
           03  QDS-RCOURSE-AMT-2     PIC S9(9)V99.
           03  QDS-RCOURSE-AMT-3     PIC S9(9)V99.
           03  QDS-NPURUNIT-1        PIC S9(5).
           03  QDS-NPURUNIT-2        PIC S9(5).
           03  QDS-NPURUNIT-3        PIC S9(5).
           03  QDS-NPURAMT-1         PIC S9(8)V99.
           03  QDS-NPURAMT-2         PIC S9(8)V99.
           03  QDS-NPURAMT-3         PIC S9(8)V99.
           03  QDS-OPURUNIT-1        PIC S9(5).
           03  QDS-OPURUNIT-2        PIC S9(5).
           03  QDS-OPURUNIT-3        PIC S9(5).
           03  QDS-OPURAMT-1         PIC S9(8)V99.
           03  QDS-OPURAMT-2         PIC S9(8)V99.
           03  QDS-OPURAMT-3         PIC S9(8)V99.
           03  QDS-RNEWUNIT-1        PIC S9(5).
           03  QDS-RNEWUNIT-2        PIC S9(5).
           03  QDS-RNEWUNIT-3        PIC S9(5).
           03  QDS-RNEWAMT-1         PIC S9(8)V99.
           03  QDS-RNEWAMT-2         PIC S9(8)V99.
           03  QDS-RNEWAMT-3         PIC S9(8)V99.
           03  QDS-PAIDUNIT-1        PIC S9(5).
           03  QDS-PAIDUNIT-2        PIC S9(5).
           03  QDS-PAIDUNIT-3        PIC S9(5).
           03  QDS-PAIDAMT-1         PIC S9(8)V99.
           03  QDS-PAIDAMT-2         PIC S9(8)V99.
           03  QDS-PAIDAMT-3         PIC S9(8)V99.
           03  QDS-TODLUNIT-1        PIC S9(5).
           03  QDS-TODLUNIT-2        PIC S9(5).
           03  QDS-TODLUNIT-3        PIC S9(5).
           03  QDS-TODLAMT-1         PIC S9(8)V99.
           03  QDS-TODLAMT-2         PIC S9(8)V99.
           03  QDS-TODLAMT-3         PIC S9(8)V99.
           03  QDS-OUTSUNIT-1        PIC S9(5).
           03  QDS-OUTSUNIT-2        PIC S9(5).
           03  QDS-OUTSUNIT-3        PIC S9(5).
           03  QDS-OUTSAMT-1         PIC S9(8)V99.
           03  QDS-OUTSAMT-2         PIC S9(8)V99.
           03  QDS-OUTSAMT-3         PIC S9(8)V99.
           03  QDS-PLOSUNIT-1        PIC S9(5).
           03  QDS-PLOSUNIT-2        PIC S9(5).
           03  QDS-PLOSUNIT-3        PIC S9(5).
           03  QDS-PLOSAMT-1         PIC S9(8)V99.
           03  QDS-PLOSAMT-2         PIC S9(8)V99.
           03  QDS-PLOSAMT-3         PIC S9(8)V99.
           03  QDS-RCPLUNIT-1        PIC S9(5).
           03  QDS-RCPLUNIT-2        PIC S9(5).
           03  QDS-RCPLUNIT-3        PIC S9(5).
           03  QDS-RCPLAMT-1         PIC S9(8)V99.
           03  QDS-RCPLAMT-2         PIC S9(8)V99.
           03  QDS-RCPLAMT-3         PIC S9(8)V99.
           03  QDS-JDOSUNIT-1        PIC S9(5).
           03  QDS-JDOSUNIT-2        PIC S9(5).
           03  QDS-JDOSUNIT-3        PIC S9(5).
           03  QDS-JDOSAMT-1         PIC S9(8)V99.
           03  QDS-JDOSAMT-2         PIC S9(8)V99.
           03  QDS-JDOSAMT-3         PIC S9(8)V99.
           03  QDS-DELUNEARN-DATE    PIC X(10).
           03  QDS-PROCEEDS-1        PIC S9(8)V99.
           03  QDS-PROCEEDS-2        PIC S9(8)V99.
           03  QDS-NO-REPO-1-1-1   PIC S9(4).
           03  QDS-AMT-REPO-1-1-1  PIC S9(7)V99.
           03  QDS-NO-REPO-1-1-2   PIC S9(4).
           03  QDS-AMT-REPO-1-1-2  PIC S9(7)V99.
           03  QDS-NO-REPO-1-1-3   PIC S9(4).
           03  QDS-AMT-REPO-1-1-3  PIC S9(7)V99.
           03  QDS-NO-REPO-1-1-4   PIC S9(4).
           03  QDS-AMT-REPO-1-1-4  PIC S9(7)V99.
           03  QDS-NO-REPO-1-1-5   PIC S9(4).
           03  QDS-AMT-REPO-1-1-5  PIC S9(7)V99.
           03  QDS-NO-REPO-1-2-1   PIC S9(4).
           03  QDS-AMT-REPO-1-2-1  PIC S9(7)V99.
           03  QDS-NO-REPO-1-2-2   PIC S9(4).
           03  QDS-AMT-REPO-1-2-2  PIC S9(7)V99.
           03  QDS-NO-REPO-1-2-3   PIC S9(4).
           03  QDS-AMT-REPO-1-2-3  PIC S9(7)V99.
           03  QDS-NO-REPO-1-2-4   PIC S9(4).
           03  QDS-AMT-REPO-1-2-4  PIC S9(7)V99.
           03  QDS-NO-REPO-1-2-5   PIC S9(4).
           03  QDS-AMT-REPO-1-2-5  PIC S9(7)V99.
           03  QDS-NO-REPO-2-1-1   PIC S9(4).
           03  QDS-AMT-REPO-2-1-1  PIC S9(7)V99.
           03  QDS-NO-REPO-2-1-2   PIC S9(4).
           03  QDS-AMT-REPO-2-1-2  PIC S9(7)V99.
           03  QDS-NO-REPO-2-1-3   PIC S9(4).
           03  QDS-AMT-REPO-2-1-3  PIC S9(7)V99.
           03  QDS-NO-REPO-2-1-4   PIC S9(4).
           03  QDS-AMT-REPO-2-1-4  PIC S9(7)V99.
           03  QDS-NO-REPO-2-1-5   PIC S9(4).
           03  QDS-AMT-REPO-2-1-5  PIC S9(7)V99.
           03  QDS-NO-REPO-2-2-1   PIC S9(4).
           03  QDS-AMT-REPO-2-2-1  PIC S9(7)V99.
           03  QDS-NO-REPO-2-2-2   PIC S9(4).
           03  QDS-AMT-REPO-2-2-2  PIC S9(7)V99.
           03  QDS-NO-REPO-2-2-3   PIC S9(4).
           03  QDS-AMT-REPO-2-2-3  PIC S9(7)V99.
           03  QDS-NO-REPO-2-2-4   PIC S9(4).
           03  QDS-AMT-REPO-2-2-4  PIC S9(7)V99.
           03  QDS-NO-REPO-2-2-5   PIC S9(4).
           03  QDS-AMT-REPO-2-2-5  PIC S9(7)V99.
           03  QDS-NO-REPO-3-1-1   PIC S9(4).
           03  QDS-AMT-REPO-3-1-1  PIC S9(7)V99.
           03  QDS-NO-REPO-3-1-2   PIC S9(4).
           03  QDS-AMT-REPO-3-1-2  PIC S9(7)V99.
           03  QDS-NO-REPO-3-1-3   PIC S9(4).
           03  QDS-AMT-REPO-3-1-3  PIC S9(7)V99.
           03  QDS-NO-REPO-3-1-4   PIC S9(4).
           03  QDS-AMT-REPO-3-1-4  PIC S9(7)V99.
           03  QDS-NO-REPO-3-1-5   PIC S9(4).
           03  QDS-AMT-REPO-3-1-5  PIC S9(7)V99.
           03  QDS-NO-REPO-3-2-1   PIC S9(4).
           03  QDS-AMT-REPO-3-2-1  PIC S9(7)V99.
           03  QDS-NO-REPO-3-2-2   PIC S9(4).
           03  QDS-AMT-REPO-3-2-2  PIC S9(7)V99.
           03  QDS-NO-REPO-3-2-3   PIC S9(4).
           03  QDS-AMT-REPO-3-2-3  PIC S9(7)V99.
           03  QDS-NO-REPO-3-2-4   PIC S9(4).
           03  QDS-AMT-REPO-3-2-4  PIC S9(7)V99.
           03  QDS-NO-REPO-3-2-5   PIC S9(4).
           03  QDS-AMT-REPO-3-2-5  PIC S9(7)V99.
           03  QDS-UNITS-1-1-1      PIC S9(7).
           03  QDS-OUTBAL-1-1-1     PIC S9(9)V99.
           03  QDS-UNITS-1-1-2      PIC S9(7).
           03  QDS-OUTBAL-1-1-2     PIC S9(9)V99.
           03  QDS-UNITS-1-1-3      PIC S9(7).
           03  QDS-OUTBAL-1-1-3     PIC S9(9)V99.
           03  QDS-UNITS-1-1-4      PIC S9(7).
           03  QDS-OUTBAL-1-1-4     PIC S9(9)V99.
           03  QDS-UNITS-1-1-5      PIC S9(7).
           03  QDS-OUTBAL-1-1-5     PIC S9(9)V99.
           03  QDS-UNITS-1-1-6      PIC S9(7).
           03  QDS-OUTBAL-1-1-6     PIC S9(9)V99.
           03  QDS-UNITS-1-1-7      PIC S9(7).
           03  QDS-OUTBAL-1-1-7     PIC S9(9)V99.
           03  QDS-UNITS-1-1-8      PIC S9(7).
           03  QDS-OUTBAL-1-1-8     PIC S9(9)V99.
           03  QDS-UNITS-1-1-9      PIC S9(7).
           03  QDS-OUTBAL-1-1-9     PIC S9(9)V99.
           03  QDS-UNITS-1-1-10     PIC S9(7).
           03  QDS-OUTBAL-1-1-10    PIC S9(9)V99.
           03  QDS-UNITS-1-2-1      PIC S9(7).
           03  QDS-OUTBAL-1-2-1     PIC S9(9)V99.
           03  QDS-UNITS-1-2-2      PIC S9(7).
           03  QDS-OUTBAL-1-2-2     PIC S9(9)V99.
           03  QDS-UNITS-1-2-3      PIC S9(7).
           03  QDS-OUTBAL-1-2-3     PIC S9(9)V99.
           03  QDS-UNITS-1-2-4      PIC S9(7).
           03  QDS-OUTBAL-1-2-4     PIC S9(9)V99.
           03  QDS-UNITS-1-2-5      PIC S9(7).
           03  QDS-OUTBAL-1-2-5     PIC S9(9)V99.
           03  QDS-UNITS-1-2-6      PIC S9(7).
           03  QDS-OUTBAL-1-2-6     PIC S9(9)V99.
           03  QDS-UNITS-1-2-7      PIC S9(7).
           03  QDS-OUTBAL-1-2-7     PIC S9(9)V99.
           03  QDS-UNITS-1-2-8      PIC S9(7).
           03  QDS-OUTBAL-1-2-8     PIC S9(9)V99.
           03  QDS-UNITS-1-2-9      PIC S9(7).
           03  QDS-OUTBAL-1-2-9     PIC S9(9)V99.
           03  QDS-UNITS-1-2-10     PIC S9(7).
           03  QDS-OUTBAL-1-2-10    PIC S9(9)V99.
           03  QDS-UNITS-2-1-1      PIC S9(7).
           03  QDS-OUTBAL-2-1-1     PIC S9(9)V99.
           03  QDS-UNITS-2-1-2      PIC S9(7).
           03  QDS-OUTBAL-2-1-2     PIC S9(9)V99.
           03  QDS-UNITS-2-1-3      PIC S9(7).
           03  QDS-OUTBAL-2-1-3     PIC S9(9)V99.
           03  QDS-UNITS-2-1-4      PIC S9(7).
           03  QDS-OUTBAL-2-1-4     PIC S9(9)V99.
           03  QDS-UNITS-2-1-5      PIC S9(7).
           03  QDS-OUTBAL-2-1-5     PIC S9(9)V99.
           03  QDS-UNITS-2-1-6      PIC S9(7).
           03  QDS-OUTBAL-2-1-6     PIC S9(9)V99.
           03  QDS-UNITS-2-1-7      PIC S9(7).
           03  QDS-OUTBAL-2-1-7     PIC S9(9)V99.
           03  QDS-UNITS-2-1-8      PIC S9(7).
           03  QDS-OUTBAL-2-1-8     PIC S9(9)V99.
           03  QDS-UNITS-2-1-9      PIC S9(7).
           03  QDS-OUTBAL-2-1-9     PIC S9(9)V99.
           03  QDS-UNITS-2-1-10     PIC S9(7).
           03  QDS-OUTBAL-2-1-10    PIC S9(9)V99.
           03  QDS-UNITS-2-2-1      PIC S9(7).
           03  QDS-OUTBAL-2-2-1     PIC S9(9)V99.
           03  QDS-UNITS-2-2-2      PIC S9(7).
           03  QDS-OUTBAL-2-2-2     PIC S9(9)V99.
           03  QDS-UNITS-2-2-3      PIC S9(7).
           03  QDS-OUTBAL-2-2-3     PIC S9(9)V99.
           03  QDS-UNITS-2-2-4      PIC S9(7).
           03  QDS-OUTBAL-2-2-4     PIC S9(9)V99.
           03  QDS-UNITS-2-2-5      PIC S9(7).
           03  QDS-OUTBAL-2-2-5     PIC S9(9)V99.
           03  QDS-UNITS-2-2-6      PIC S9(7).
           03  QDS-OUTBAL-2-2-6     PIC S9(9)V99.
           03  QDS-UNITS-2-2-7      PIC S9(7).
           03  QDS-OUTBAL-2-2-7     PIC S9(9)V99.
           03  QDS-UNITS-2-2-8      PIC S9(7).
           03  QDS-OUTBAL-2-2-8     PIC S9(9)V99.
           03  QDS-UNITS-2-2-9      PIC S9(7).
           03  QDS-OUTBAL-2-2-9     PIC S9(9)V99.
           03  QDS-UNITS-2-2-10     PIC S9(7).
           03  QDS-OUTBAL-2-2-10    PIC S9(9)V99.
           03  QDS-UNITS-3-1-1      PIC S9(7).
           03  QDS-OUTBAL-3-1-1     PIC S9(9)V99.
           03  QDS-UNITS-3-1-2      PIC S9(7).
           03  QDS-OUTBAL-3-1-2     PIC S9(9)V99.
           03  QDS-UNITS-3-1-3      PIC S9(7).
           03  QDS-OUTBAL-3-1-3     PIC S9(9)V99.
           03  QDS-UNITS-3-1-4      PIC S9(7).
           03  QDS-OUTBAL-3-1-4     PIC S9(9)V99.
           03  QDS-UNITS-3-1-5      PIC S9(7).
           03  QDS-OUTBAL-3-1-5     PIC S9(9)V99.
           03  QDS-UNITS-3-1-6      PIC S9(7).
           03  QDS-OUTBAL-3-1-6     PIC S9(9)V99.
           03  QDS-UNITS-3-1-7      PIC S9(7).
           03  QDS-OUTBAL-3-1-7     PIC S9(9)V99.
           03  QDS-UNITS-3-1-8      PIC S9(7).
           03  QDS-OUTBAL-3-1-8     PIC S9(9)V99.
           03  QDS-UNITS-3-1-9      PIC S9(7).
           03  QDS-OUTBAL-3-1-9     PIC S9(9)V99.
           03  QDS-UNITS-3-1-10     PIC S9(7).
           03  QDS-OUTBAL-3-1-10    PIC S9(9)V99.
           03  QDS-UNITS-3-2-1      PIC S9(7).
           03  QDS-OUTBAL-3-2-1     PIC S9(9)V99.
           03  QDS-UNITS-3-2-2      PIC S9(7).
           03  QDS-OUTBAL-3-2-2     PIC S9(9)V99.
           03  QDS-UNITS-3-2-3      PIC S9(7).
           03  QDS-OUTBAL-3-2-3     PIC S9(9)V99.
           03  QDS-UNITS-3-2-4      PIC S9(7).
           03  QDS-OUTBAL-3-2-4     PIC S9(9)V99.
           03  QDS-UNITS-3-2-5      PIC S9(7).
           03  QDS-OUTBAL-3-2-5     PIC S9(9)V99.
           03  QDS-UNITS-3-2-6      PIC S9(7).
           03  QDS-OUTBAL-3-2-6     PIC S9(9)V99.
           03  QDS-UNITS-3-2-7      PIC S9(7).
           03  QDS-OUTBAL-3-2-7     PIC S9(9)V99.
           03  QDS-UNITS-3-2-8      PIC S9(7).
           03  QDS-OUTBAL-3-2-8     PIC S9(9)V99.
           03  QDS-UNITS-3-2-9      PIC S9(7).
           03  QDS-OUTBAL-3-2-9     PIC S9(9)V99.
           03  QDS-UNITS-3-2-10     PIC S9(7).
           03  QDS-OUTBAL-3-2-10    PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-1-1      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-1-1     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-1-2      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-1-2     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-1-3      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-1-3     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-1-4      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-1-4     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-1-5      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-1-5     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-1-6      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-1-6     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-1-7      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-1-7     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-1-8      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-1-8     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-1-9      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-1-9     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-1-10     PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-1-10    PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-2-1      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-2-1     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-2-2      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-2-2     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-2-3      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-2-3     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-2-4      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-2-4     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-2-5      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-2-5     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-2-6      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-2-6     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-2-7      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-2-7     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-2-8      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-2-8     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-2-9      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-2-9     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-1-2-10     PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-1-2-10    PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-1-1      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-1-1     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-1-2      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-1-2     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-1-3      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-1-3     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-1-4      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-1-4     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-1-5      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-1-5     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-1-6      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-1-6     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-1-7      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-1-7     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-1-8      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-1-8     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-1-9      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-1-9     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-1-10     PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-1-10    PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-2-1      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-2-1     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-2-2      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-2-2     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-2-3      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-2-3     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-2-4      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-2-4     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-2-5      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-2-5     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-2-6      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-2-6     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-2-7      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-2-7     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-2-8      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-2-8     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-2-9      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-2-9     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-2-2-10     PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-2-2-10    PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-1-1      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-1-1     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-1-2      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-1-2     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-1-3      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-1-3     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-1-4      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-1-4     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-1-5      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-1-5     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-1-6      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-1-6     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-1-7      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-1-7     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-1-8      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-1-8     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-1-9      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-1-9     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-1-10     PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-1-10    PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-2-1      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-2-1     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-2-2      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-2-2     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-2-3      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-2-3     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-2-4      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-2-4     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-2-5      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-2-5     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-2-6      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-2-6     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-2-7      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-2-7     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-2-8      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-2-8     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-2-9      PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-2-9     PIC S9(9)V99.
           03  QDS-UNITS-BNKRPT-3-2-10     PIC S9(7).
           03  QDS-OUTBAL-BNKRPT-3-2-10    PIC S9(9)V99.
           03  QDS-UNEARNED-AMT-1-1-1   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-1-2   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-1-3   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-1-4   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-1-5   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-1-6   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-1-7   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-1-8   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-1-9   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-1-10  PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-2-1   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-2-2   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-2-3   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-2-4   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-2-5   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-2-6   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-2-7   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-2-8   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-2-9   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-1-2-10  PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-1-1   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-1-2   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-1-3   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-1-4   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-1-5   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-1-6   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-1-7   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-1-8   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-1-9   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-1-10  PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-2-1   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-2-2   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-2-3   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-2-4   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-2-5   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-2-6   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-2-7   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-2-8   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-2-9   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-2-2-10  PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-1-1   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-1-2   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-1-3   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-1-4   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-1-5   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-1-6   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-1-7   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-1-8   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-1-9   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-1-10  PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-2-1   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-2-2   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-2-3   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-2-4   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-2-5   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-2-6   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-2-7   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-2-8   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-2-9   PIC S9(8)V99.
           03  QDS-UNEARNED-AMT-3-2-10  PIC S9(8)V99.
           03  QDS-DLDISC-VOL-1          PIC S9(9)V99.
           03  QDS-DLDISC-VOL-2          PIC S9(9)V99.
           03  QDS-DLDISC-VOL-3          PIC S9(9)V99.
           03  QDS-AFPDUEDL-MEMO      PIC S9(8)V99.
           03  QDS-UNPAID-MANUAL      PIC S9(8)V99.
           03  QDS-LTOUCH-DATE        PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  RECORDS-FOUND               PIC X(01)     VALUE "X".
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

       01  RECORDS-READ                PIC 9(11)     VALUE ZEROES.

       01  SUBSCRIPT-WORKERS.
           03  SUB                     PIC 9(09)     VALUE ZEROES.
           03  S1                      PIC 9(09)     VALUE ZEROES.
           03  S2                      PIC 9(09)     VALUE ZEROES.
           03  S3                      PIC 9(09)     VALUE ZEROES.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(77)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP-ARROW, (".
           03  FILLER PIC X(30) VALUE "Y)ES OR (N)O TO ACCEPT        ".
           03  FILLER PIC X(17) VALUE "   ACCEPT (Y/N): ".

       01  LEGEND-PRINTER-OPTIONS.
           03  FILLER PIC X(30) VALUE "F7-EXIT, ENTER PRINTER OPTIONS".

       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING BRANCH NO. (F3-ALL) ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING BRANCH NO. ".

       01  LEGEND-PROCESS-KEY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER NU".
           03  FILLER PIC X(30) VALUE "MBER OF FILE KEY TO PROCESS   ".

       01  LEGEND-BEG-END-RANGE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER RA".
           03  FILLER PIC X(30) VALUE "ANGE VALUE                    ".

      ******************************************************************
      *
      *    P A C E - W O R K E R S
      *
      ******************************************************************
       78  PACE-COUNTER-MAX                          VALUE 1000.
       77  PACE-COUNTER                PIC 9(6)      VALUE ZEROES.
       77  PACE-DISPLAY-MAX            PIC ZZZ,ZZ9   VALUE ZEROES.
       78  PACE-MAX                                  VALUE 15.
       77  PACE-LEV                    PIC 9(2)      VALUE ZEROES.

       01  PACE-HEAD-LINE.
           03  FILLER PIC X(28) VALUE "TIME        KEY             ".
           03  PACE-HEAD-MAX    PIC ZZZ,ZZ9 VALUE ZEROES.
           03  FILLER PIC X(05) VALUE " RECS".

       01  PACE-TIME.
           03  PACE-TIME-HH            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-MM            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-SS            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ".".
           03  PACE-TIME-HD            PIC 99.

       01  PACE-DETAIL.
           03  PACE-DETAIL-TIME        PIC 99/99/99/99.
           03  FILLER                  PIC X(1).
           03  PACE-DETAIL-KEY         PIC X(28).

       01  PACE-BUF.
           03  PACE-ELE                PIC X(40)     OCCURS PACE-MAX.

       01  PACE-LIST.
           03  FILLER PIC X(24) VALUE "PACE-01 PACE-02 PACE-03 ".
           03  FILLER PIC X(24) VALUE "PACE-04 PACE-05 PACE-06 ".
           03  FILLER PIC X(24) VALUE "PACE-07 PACE-08 PACE-09 ".
           03  FILLER PIC X(24) VALUE "PACE-10 PACE-11 PACE-12 ".
           03  FILLER PIC X(24) VALUE "PACE-13 PACE-14 PACE-15.".

      ******************************************************************
      *
      *    P A R A M E T E R - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(04).

           03  PROCESS-KEY             PIC 9(01).
           03  BEG-DATE-CCYYMM         PIC 9(06).
           03  END-DATE-CCYYMM         PIC 9(06).
           03  BEG-NUM                 PIC 9(06).
           03  END-NUM                 PIC 9(06).
           03  BEG-CLASS               PIC 9(03).
           03  END-CLASS               PIC 9(03).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

       01  SAVE-SCREEN-RANGES.
           03  SV-BEG-BRANCH           PIC 9(04)     VALUE 9999.
           03  SV-END-BRANCH           PIC 9(04)     VALUE 9999.

           03  SV-GROUP-OPTION.
               05  SV-GROUP-FLAG       PIC X(01).
               05  SV-ENT-GROUP        PIC X(04).

      ******************************************************************
      *
      *    R E P O R T - W O R K E R S
      *
      ******************************************************************
       01  PRINT-LINE-WORKERS.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.
           03  PAGE-NUMBER             PIC 9(04)     VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.

      *=================================================================
       01  HEADER-LINE-01.
           03  H01-DATE           PIC X(08) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H01-HOUR           PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(01) VALUE ":".
           03  H01-MINUTE         PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(30) VALUE SPACES.
           03  H01-NAME           PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(37) VALUE SPACES.
           03  FILLER             PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE           PIC 9(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER             PIC X(10) VALUE "USER ID:".
           03  H02-USERID         PIC X(10) VALUE SPACES.
           03  H02-TITLE          PIC X(92) VALUE SPACES.
           03  FILLER             PIC X(20) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER PIC X(30) VALUE "BRANCH   YYYY/MM    DLNO   CLA".
           03  FILLER PIC X(30) VALUE "SS  SUMWGTTM".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-DS-BRANCH                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-DS-DATE-CCYYMM             PIC 9999/99.
           03  FILLER                         PIC X(03).
           03  D01-DS-NUM                     PIC 9(06).
           03  FILLER                         PIC X(03).
           03  D01-DS-CLASS                   PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-DS-SUMWGTTM                PIC 9(08).

      *-------------------------------------------------------{ RANGES }
       01  RANGE-LINE-01                      REDEFINES DETAIL-LINE.
           03  R01-PROMPT                     PIC X(32).
           03  R01-FIELD                      PIC X(100).
           03  R01-DATE                       REDEFINES R01-FIELD
                                              PIC 99/99/99.
           03  R01-NUMERIC                    REDEFINES R01-FIELD
                                              PIC Z(10)9.
           03  R01-TEXT                       REDEFINES R01-FIELD
                                              PIC X(100).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
      *COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
      *-----------------------------------------------------------------
       COPY "LIBIO/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBIO/IODSIN_DEF.CPY".
       COPY "LIBIO/IODSIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IODSIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE.

       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *=================================================================
       MAIN-PROGRAM-NEXT-GROUP.

           IF ( GROUP-FLAG = "N" )
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO MAIN-PROGRAM-END.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO MAIN-PROGRAM-PROCESS-BRANCH.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM READ-BR-FILE-NEXT.

           IF ( IO-BAD             ) OR
              ( BR-NO > END-BRANCH )
              GO TO MAIN-PROGRAM-END.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-PROCESS-BRANCH.

           PERFORM PROCESS-BRANCH-MODULE.

           GO TO MAIN-PROGRAM-NEXT-GROUP.

      *=================================================================
       MAIN-PROGRAM-END.

           PERFORM WRITE-SUMMARY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY SUMMARY TO SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM GET-TIME.
           MOVE SPACES TO SENDMESS-HEAD.

           STRING "PROGRAM COMPLETED AT ", TIME-EDIT
                  DELIMITED BY SIZE INTO SENDMESS-HEAD.

           MOVE SPACES TO SENDMESS-MSG1.
           MOVE SPACES TO SENDMESS-MSG2.
           STRING "RECORDS READ: ", RECORDS-READ
                  DELIMITED BY SIZE INTO SENDMESS-MSG2.
           DISPLAY MESSAGE BOX SENDMESS-HEAD, X"0A"
                               SENDMESS-MSG1, X"0A"
                               SENDMESS-MSG2.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
      D    STOP FORM-PATHNAME.

           MOVE "X"               TO RECORDS-FOUND.
           MOVE FORM-PATHNAME     TO FORM-PATH.
           MOVE "IODSIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IODSIN-QUEUE-POS  TO Q-POS.
           MOVE IODSIN-COPIES-POS TO C-POS.
           MOVE IODSIN-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              PERFORM GET-TIME
              MOVE SPACES TO LEGEND
              STRING "STARTING TIME: ", TIME-EDIT
                     DELIMITED BY SIZE INTO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

           IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
              MOVE BEG-BRANCH TO SV-BEG-BRANCH
              MOVE END-BRANCH TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC  TO BEG-BRANCH
                                           END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH         TO SV-BEG-BRANCH
              MOVE END-BRANCH         TO SV-END-BRANCH
              MOVE GROUP-FLAG         TO SV-GROUP-FLAG
              MOVE ENT-GROUP          TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP        TO ENT-GROUP
              MOVE ZEROES             TO BEG-BRANCH
              MOVE 9999               TO END-BRANCH
              MOVE "Y"                TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE.

           PERFORM GET-GPENV.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

           PERFORM ENABLE-INTERRUPT.
           PERFORM SEND-PACE-HEAD.

           MOVE ZEROES TO PACE-LEV.
           MOVE ZEROES TO RECORDS-READ.

           PERFORM GET-TIME.
           MOVE "N"       TO RECORDS-FOUND.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE "R" TO TIMER-TYPE.
           PERFORM TIMER-START.
           MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.

      *=================================================================
       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.

           IF ( UP-KEY )
              GO TO ENTER-END-CLASS.

      *=================================================================
       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              MOVE ZEROES TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES               TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"                  TO GROUP-FLAG
              MOVE ZEROES               TO BEG-BRANCH
              MOVE 9999                 TO END-BRANCH
              GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.

           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.

           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE LEGEND-END-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "N"                    TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BR-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" ) GO TO ENTER-BEG-BRANCH.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 = ZEROES ) OR
              ( VDUNUM0 > 1      ) 
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-BEG-DATE-CCYYMM.

           MOVE "ENNN060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE-CCYYMM.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE-CCYYMM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-DATE-CCYYMM.

      *-----------------------------------------------------------------
       ENTER-END-DATE-CCYYMM.

           MOVE "ENNN060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE-CCYYMM.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE-CCYYMM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-DATE-CCYYMM.

           IF ( BEG-DATE-CCYYMM > END-DATE-CCYYMM )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE-CCYYMM.

           GO TO ENTER-BEG-NUM.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE-CCYYMM.

           MOVE "SNNN060BEG-DATE." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-DATE-CCYYMM.
           PERFORM SCREENIO.

           MOVE "SNNN060END-DATE." TO VDU.
           MOVE ALL "9"             TO VDUNUM0  END-DATE-CCYYMM.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-NUM.

           MOVE "ENNN060BEG-NUM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE-CCYYMM.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-NUM.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-NUM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-NUM.

      *-----------------------------------------------------------------
       ENTER-END-NUM.

           MOVE "ENNN060END-NUM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-NUM.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-NUM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-NUM.

           IF ( BEG-NUM > END-NUM )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-NUM.

           GO TO ENTER-BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-ALL-NUM.

           MOVE "SNNN060BEG-NUM." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-NUM.
           PERFORM SCREENIO.

           MOVE "SNNN060END-NUM." TO VDU.
           MOVE ALL "9"            TO VDUNUM0  END-NUM.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-CLASS.

           MOVE "ENNN030BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-NUM.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-END-CLASS.

           MOVE "ENNN030END-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-CLASS.

           IF ( BEG-CLASS > END-CLASS )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-CLASS.

           MOVE "SNNN030BEG-CLASS." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-CLASS.
           PERFORM SCREENIO.

           MOVE "SNNN030END-CLASS." TO VDU.
           MOVE ALL "9"            TO VDUNUM0  END-CLASS.
           PERFORM SCREENIO.


      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    D I S P L A Y - P A R A M E T E R S
      *
      *****************************************************************
       DISPLAY-PARAMETERS SECTION.

           IF ( GROUP-FLAG = "Y" )
              MOVE "S  A000BEG-BRANCH."   TO VDU
              MOVE ENT-GROUP              TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH."   TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH."   TO VDU
              MOVE BEG-BRANCH             TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH."   TO VDU
              MOVE END-BRANCH             TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN060BEG-DATE."       TO VDU.
           MOVE BEG-DATE-CCYYMM                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-DATE."       TO VDU.
           MOVE END-DATE-CCYYMM                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060BEG-NUM."       TO VDU.
           MOVE BEG-NUM                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-NUM."       TO VDU.
           MOVE END-NUM                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030BEG-CLASS."       TO VDU.
           MOVE BEG-CLASS                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-CLASS."       TO VDU.
           MOVE END-CLASS                 TO VDUNUM0.
           PERFORM SCREENIO.


      *=================================================================
       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      *****************************************************************
       EO-EXEC-AUDIT SECTION.

      *    MOVE EXT-EOBUF-DATE1 TO ???.
      *    MOVE EXT-EOBUF-DATE2 TO ???.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S C A N - G R 1 - F I L E
      *
      ******************************************************************
       SCAN-GR1-FILE SECTION.

           MOVE GRSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF   TO GRSCAN-BUF.

           IF ( SCAN-STATUS NOT = "00"   ) OR
              ( GRSCAN-KEY      = SPACES )
              MOVE "XX" TO VDUSTATUS
              GO TO SCAN-GR1-FILE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - -{ VALID SELECTION  }
           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE GRSCAN-KEY TO VDUBUF.
           MOVE "00"       TO VDUSTATUS.

      *=================================================================
       SCAN-GR1-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - B R A N C H - M O D U L E
      *
      ******************************************************************
       PROCESS-BRANCH-MODULE SECTION.

           MOVE PACE-COUNTER-MAX TO PACE-COUNTER.

      *- - - - - - - - - - - - - - - - - -{ VERIFY 'LOCAL' FILE PATHS  }
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO DS-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }

           MOVE BR-NO     TO QDS1-WBEG-BRANCH.
           MOVE BR-NO     TO QDS1-WEND-BRANCH.

           MOVE BEG-DATE-CCYYMM  TO QDS1-WBEG-DATE-CCYYMM.
           MOVE END-DATE-CCYYMM  TO QDS1-WEND-DATE-CCYYMM.
           MOVE BEG-NUM  TO QDS1-WBEG-NUM.
           MOVE END-NUM  TO QDS1-WEND-NUM.
           MOVE BEG-CLASS  TO QDS1-WBEG-CLASS.
           MOVE END-CLASS  TO QDS1-WEND-CLASS.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-DS1-FILE.
           PERFORM START-DS1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-DS1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( DS-DATE-CCYYMM < BEG-DATE-CCYYMM )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( DS-DATE-CCYYMM > END-DATE-CCYYMM )
              GO TO PROCESS-BRANCH-MODULE-END
           ELSE
              IF DS-BRANCH < BEG-BRANCH OR > END-BRANCH
                 GO TO PROCESS-BRANCH-NEXT.

           IF DS-NUM < BEG-NUM OR > END-NUM
              GO TO PROCESS-BRANCH-NEXT.
           IF DS-CLASS < BEG-CLASS OR > END-CLASS
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE DS-BRANCH                   TO D01-DS-BRANCH.
           MOVE DS-DATE-CCYYMM              TO D01-DS-DATE-CCYYMM.
           MOVE DS-NUM                      TO D01-DS-NUM.
           MOVE DS-CLASS                    TO D01-DS-CLASS.
           MOVE DS-SUMWGTTM                 TO D01-DS-SUMWGTTM.
           PERFORM WRITE-DETAIL-LINE.


           MOVE "Y" TO RECORDS-FOUND.
           IF PROCESS-KEY = 9
              GO TO PROCESS-BRANCH-MODULE-END.

           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-DS1-FILE.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P A C E - R O U T I N E
      *
      *=================================================================
      * DESC: DISPLAY THE PACE OF THE EXECUTION OF THE PROGRAM.
      ******************************************************************
       PACE-ROUTINE SECTION.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO PACE-ROUTINE-EXIT.

           ADD 01 TO PACE-COUNTER.

           IF ( PACE-COUNTER <= PACE-COUNTER-MAX )
              GO TO PACE-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ADD 01 TO PACE-LEV.

           IF ( PACE-LEV <= PACE-MAX )
              GO TO PACE-ROUTINE-UPDATE.

           PERFORM VARYING SUB FROM 2 BY 1 UNTIL SUB > PACE-MAX
              MOVE PACE-ELE(SUB) TO PACE-ELE(SUB - 1)
           END-PERFORM.

           MOVE PACE-MAX TO PACE-LEV.
           MOVE SPACES   TO PACE-ELE(PACE-LEV).

      *-----------------------------------------------------------------
       PACE-ROUTINE-UPDATE.

           MOVE SPACES TO PACE-DETAIL.

           PERFORM GET-TIME.
           MOVE CUR-HH    TO PACE-TIME-HH.
           MOVE CUR-MM    TO PACE-TIME-MM.
           MOVE CUR-SS    TO PACE-TIME-SS.
           MOVE CUR-HD    TO PACE-TIME-HD.
           MOVE PACE-TIME TO PACE-DETAIL-TIME.

           MOVE DS1-KEY TO PACE-DETAIL-KEY.

           MOVE PACE-DETAIL TO PACE-ELE(PACE-LEV).
           MOVE PACE-BUF    TO WR-BUF.
           MOVE PACE-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ZEROES TO PACE-COUNTER.

      *=================================================================
       PACE-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - S U M M A R Y
      *
      ******************************************************************
       WRITE-SUMMARY SECTION.

           IF ( NO-RECORDS-FOUND )
              GO TO WRITE-SUMMARY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 99     TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GROUP-FLAG = "Y" )
              MOVE "GROUP ID NAME                 : " TO R01-PROMPT
              MOVE ENT-GROUP                          TO R01-TEXT
              MOVE 2                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH NUMBER       : " TO R01-PROMPT
              MOVE BEG-BRANCH                         TO R01-NUMERIC
              MOVE 2                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING    BRANCH NUMBER       : " TO R01-PROMPT
              MOVE END-BRANCH                         TO R01-NUMERIC
              MOVE 1                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DATE-CCYYMM         : "    TO R01-PROMPT.
           MOVE BEG-DATE-CCYYMM                       TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE-CCYYMM         : "    TO R01-PROMPT.
           MOVE END-DATE-CCYYMM                       TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING NUM               : "    TO R01-PROMPT.
           MOVE BEG-NUM                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    NUM               : "    TO R01-PROMPT.
           MOVE END-NUM                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS               : "    TO R01-PROMPT.
           MOVE BEG-CLASS                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CLASS               : "    TO R01-PROMPT.
           MOVE END-CLASS                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : "    TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *=================================================================
       WRITE-SUMMARY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-PAGE-HEADER.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *=================================================================
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - P A G E - H E A D E R
      *
      ******************************************************************
       WRITE-PAGE-HEADER SECTION.

           MOVE 01 TO LN-COUNT.
           ADD  01 TO PAGE-NUMBER.
           PERFORM GET-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{01}
           MOVE EXT-CONAME-SYSDATE               TO H01-DATE.
           MOVE T-HH                             TO H01-HOUR.
           MOVE T-MM                             TO H01-MINUTE.
           MOVE EXT-CONAME-CONAME                TO H01-NAME.
           CALL "C$JUSTIFY"                   USING H01-NAME "C".
           MOVE PAGE-NUMBER                      TO H01-PAGE.

           MOVE HEADER-LINE-01                   TO PR-REC.
           MOVE 99                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{02}
           MOVE EXT-CONAME-USER-ID               TO H02-USERID.
           MOVE "SQL TABLE IO UTILITY - DSFILE"  TO H02-TITLE.
           CALL "C$JUSTIFY"                   USING H02-TITLE "C".

           MOVE HEADER-LINE-02                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{03}
           MOVE HEADER-LINE-03                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01 TO LN-FEED.

      *=================================================================
       WRITE-PAGE-HEADER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    F O R M - R E S E T
      *
      ******************************************************************
       FORM-RESET SECTION.

           MOVE EXT-CONAME-CONAME TO IODSIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IODSIN-SYSDATE.
           DISPLAY IODSIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IODSIN-FORM-FIELDS TO WR-BUF.
           MOVE IODSIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *=================================================================
       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       SETUP-LINK-INFO SECTION.

           MOVE IODSIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IODSIN-HELP--DIR  TO HELP-LINK-DIR.

      *=================================================================
       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.

           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBIO/IODSIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *=================================================================
       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      *COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *COPY "LIBGB/RMFILE.CPY".
      *=================================================================
       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-PACE-HEAD.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE PACE-COUNTER-MAX    TO PACE-HEAD-MAX
              MOVE "S  A400PACE-HEAD." TO VDU
              MOVE PACE-HEAD-LINE      TO VDUBUF
              PERFORM SCREENIO.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBIO/CONNECT_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      *-----------------------------------------------------------------
      *    LOAD DS-REC FROM QDS-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-DS-FIELDS.
           MOVE QDS-DATE-CCYYMM TO DS-DATE-CCYYMM.
           MOVE QDS-BRANCH TO DS-BRANCH.
           MOVE QDS-NUM TO DS-NUM.
           MOVE QDS-CLASS TO DS-CLASS.
           MOVE QDS-DLRVPRIN-1 TO DS-DLRVPRIN(1).
           MOVE QDS-DLRVPRIN-2 TO DS-DLRVPRIN(2).
           MOVE QDS-DLRVPRIN-3 TO DS-DLRVPRIN(3).
           MOVE QDS-DLRVCHGS-1 TO DS-DLRVCHGS(1).
           MOVE QDS-DLRVCHGS-2 TO DS-DLRVCHGS(2).
           MOVE QDS-DLRVCHGS-3 TO DS-DLRVCHGS(3).
           MOVE QDS-DLPARVHELD TO DS-DLPARVHELD.
           MOVE QDS-UNPAIDCK TO DS-UNPAIDCK.
           MOVE QDS-DLDISC TO DS-DLDISC.
           MOVE QDS-DLPARVPAID TO DS-DLPARVPAID.
           MOVE QDS-SUMWGTTM TO DS-SUMWGTTM.
           MOVE QDS-SUMWGTAV TO DS-SUMWGTAV.
           MOVE QDS-SUMWGTTA TO DS-SUMWGTTA.
           MOVE QDS-RCOURSE-UNITS-1 TO DS-RCOURSE-UNITS(1).
           MOVE QDS-RCOURSE-UNITS-2 TO DS-RCOURSE-UNITS(2).
           MOVE QDS-RCOURSE-UNITS-3 TO DS-RCOURSE-UNITS(3).
           MOVE QDS-RCOURSE-AMT-1 TO DS-RCOURSE-AMT(1).
           MOVE QDS-RCOURSE-AMT-2 TO DS-RCOURSE-AMT(2).
           MOVE QDS-RCOURSE-AMT-3 TO DS-RCOURSE-AMT(3).
           MOVE QDS-NPURUNIT-1 TO DS-NPURUNIT(1).
           MOVE QDS-NPURUNIT-2 TO DS-NPURUNIT(2).
           MOVE QDS-NPURUNIT-3 TO DS-NPURUNIT(3).
           MOVE QDS-NPURAMT-1 TO DS-NPURAMT(1).
           MOVE QDS-NPURAMT-2 TO DS-NPURAMT(2).
           MOVE QDS-NPURAMT-3 TO DS-NPURAMT(3).
           MOVE QDS-OPURUNIT-1 TO DS-OPURUNIT(1).
           MOVE QDS-OPURUNIT-2 TO DS-OPURUNIT(2).
           MOVE QDS-OPURUNIT-3 TO DS-OPURUNIT(3).
           MOVE QDS-OPURAMT-1 TO DS-OPURAMT(1).
           MOVE QDS-OPURAMT-2 TO DS-OPURAMT(2).
           MOVE QDS-OPURAMT-3 TO DS-OPURAMT(3).
           MOVE QDS-RNEWUNIT-1 TO DS-RNEWUNIT(1).
           MOVE QDS-RNEWUNIT-2 TO DS-RNEWUNIT(2).
           MOVE QDS-RNEWUNIT-3 TO DS-RNEWUNIT(3).
           MOVE QDS-RNEWAMT-1 TO DS-RNEWAMT(1).
           MOVE QDS-RNEWAMT-2 TO DS-RNEWAMT(2).
           MOVE QDS-RNEWAMT-3 TO DS-RNEWAMT(3).
           MOVE QDS-PAIDUNIT-1 TO DS-PAIDUNIT(1).
           MOVE QDS-PAIDUNIT-2 TO DS-PAIDUNIT(2).
           MOVE QDS-PAIDUNIT-3 TO DS-PAIDUNIT(3).
           MOVE QDS-PAIDAMT-1 TO DS-PAIDAMT(1).
           MOVE QDS-PAIDAMT-2 TO DS-PAIDAMT(2).
           MOVE QDS-PAIDAMT-3 TO DS-PAIDAMT(3).
           MOVE QDS-TODLUNIT-1 TO DS-TODLUNIT(1).
           MOVE QDS-TODLUNIT-2 TO DS-TODLUNIT(2).
           MOVE QDS-TODLUNIT-3 TO DS-TODLUNIT(3).
           MOVE QDS-TODLAMT-1 TO DS-TODLAMT(1).
           MOVE QDS-TODLAMT-2 TO DS-TODLAMT(2).
           MOVE QDS-TODLAMT-3 TO DS-TODLAMT(3).
           MOVE QDS-OUTSUNIT-1 TO DS-OUTSUNIT(1).
           MOVE QDS-OUTSUNIT-2 TO DS-OUTSUNIT(2).
           MOVE QDS-OUTSUNIT-3 TO DS-OUTSUNIT(3).
           MOVE QDS-OUTSAMT-1 TO DS-OUTSAMT(1).
           MOVE QDS-OUTSAMT-2 TO DS-OUTSAMT(2).
           MOVE QDS-OUTSAMT-3 TO DS-OUTSAMT(3).
           MOVE QDS-PLOSUNIT-1 TO DS-PLOSUNIT(1).
           MOVE QDS-PLOSUNIT-2 TO DS-PLOSUNIT(2).
           MOVE QDS-PLOSUNIT-3 TO DS-PLOSUNIT(3).
           MOVE QDS-PLOSAMT-1 TO DS-PLOSAMT(1).
           MOVE QDS-PLOSAMT-2 TO DS-PLOSAMT(2).
           MOVE QDS-PLOSAMT-3 TO DS-PLOSAMT(3).
           MOVE QDS-RCPLUNIT-1 TO DS-RCPLUNIT(1).
           MOVE QDS-RCPLUNIT-2 TO DS-RCPLUNIT(2).
           MOVE QDS-RCPLUNIT-3 TO DS-RCPLUNIT(3).
           MOVE QDS-RCPLAMT-1 TO DS-RCPLAMT(1).
           MOVE QDS-RCPLAMT-2 TO DS-RCPLAMT(2).
           MOVE QDS-RCPLAMT-3 TO DS-RCPLAMT(3).
           MOVE QDS-JDOSUNIT-1 TO DS-JDOSUNIT(1).
           MOVE QDS-JDOSUNIT-2 TO DS-JDOSUNIT(2).
           MOVE QDS-JDOSUNIT-3 TO DS-JDOSUNIT(3).
           MOVE QDS-JDOSAMT-1 TO DS-JDOSAMT(1).
           MOVE QDS-JDOSAMT-2 TO DS-JDOSAMT(2).
           MOVE QDS-JDOSAMT-3 TO DS-JDOSAMT(3).

           IF ( QDS-DELUNEARN-DATE = "1900-01-01" )
              MOVE ZEROES TO DS-DELUNEARN-DATE
           ELSE
              MOVE QDS-DELUNEARN-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO DS-DELUNEARN-DATE.

           MOVE QDS-PROCEEDS-1 TO DS-PROCEEDS(1).
           MOVE QDS-PROCEEDS-2 TO DS-PROCEEDS(2).
           MOVE QDS-NO-REPO-1-1-1 TO DS-NO-REPO(1 1 1).
           MOVE QDS-AMT-REPO-1-1-1 TO DS-AMT-REPO(1 1 1).
           MOVE QDS-NO-REPO-1-1-2 TO DS-NO-REPO(1 1 2).
           MOVE QDS-AMT-REPO-1-1-2 TO DS-AMT-REPO(1 1 2).
           MOVE QDS-NO-REPO-1-1-3 TO DS-NO-REPO(1 1 3).
           MOVE QDS-AMT-REPO-1-1-3 TO DS-AMT-REPO(1 1 3).
           MOVE QDS-NO-REPO-1-1-4 TO DS-NO-REPO(1 1 4).
           MOVE QDS-AMT-REPO-1-1-4 TO DS-AMT-REPO(1 1 4).
           MOVE QDS-NO-REPO-1-1-5 TO DS-NO-REPO(1 1 5).
           MOVE QDS-AMT-REPO-1-1-5 TO DS-AMT-REPO(1 1 5).
           MOVE QDS-NO-REPO-1-2-1 TO DS-NO-REPO(1 2 1).
           MOVE QDS-AMT-REPO-1-2-1 TO DS-AMT-REPO(1 2 1).
           MOVE QDS-NO-REPO-1-2-2 TO DS-NO-REPO(1 2 2).
           MOVE QDS-AMT-REPO-1-2-2 TO DS-AMT-REPO(1 2 2).
           MOVE QDS-NO-REPO-1-2-3 TO DS-NO-REPO(1 2 3).
           MOVE QDS-AMT-REPO-1-2-3 TO DS-AMT-REPO(1 2 3).
           MOVE QDS-NO-REPO-1-2-4 TO DS-NO-REPO(1 2 4).
           MOVE QDS-AMT-REPO-1-2-4 TO DS-AMT-REPO(1 2 4).
           MOVE QDS-NO-REPO-1-2-5 TO DS-NO-REPO(1 2 5).
           MOVE QDS-AMT-REPO-1-2-5 TO DS-AMT-REPO(1 2 5).
           MOVE QDS-NO-REPO-2-1-1 TO DS-NO-REPO(2 1 1).
           MOVE QDS-AMT-REPO-2-1-1 TO DS-AMT-REPO(2 1 1).
           MOVE QDS-NO-REPO-2-1-2 TO DS-NO-REPO(2 1 2).
           MOVE QDS-AMT-REPO-2-1-2 TO DS-AMT-REPO(2 1 2).
           MOVE QDS-NO-REPO-2-1-3 TO DS-NO-REPO(2 1 3).
           MOVE QDS-AMT-REPO-2-1-3 TO DS-AMT-REPO(2 1 3).
           MOVE QDS-NO-REPO-2-1-4 TO DS-NO-REPO(2 1 4).
           MOVE QDS-AMT-REPO-2-1-4 TO DS-AMT-REPO(2 1 4).
           MOVE QDS-NO-REPO-2-1-5 TO DS-NO-REPO(2 1 5).
           MOVE QDS-AMT-REPO-2-1-5 TO DS-AMT-REPO(2 1 5).
           MOVE QDS-NO-REPO-2-2-1 TO DS-NO-REPO(2 2 1).
           MOVE QDS-AMT-REPO-2-2-1 TO DS-AMT-REPO(2 2 1).
           MOVE QDS-NO-REPO-2-2-2 TO DS-NO-REPO(2 2 2).
           MOVE QDS-AMT-REPO-2-2-2 TO DS-AMT-REPO(2 2 2).
           MOVE QDS-NO-REPO-2-2-3 TO DS-NO-REPO(2 2 3).
           MOVE QDS-AMT-REPO-2-2-3 TO DS-AMT-REPO(2 2 3).
           MOVE QDS-NO-REPO-2-2-4 TO DS-NO-REPO(2 2 4).
           MOVE QDS-AMT-REPO-2-2-4 TO DS-AMT-REPO(2 2 4).
           MOVE QDS-NO-REPO-2-2-5 TO DS-NO-REPO(2 2 5).
           MOVE QDS-AMT-REPO-2-2-5 TO DS-AMT-REPO(2 2 5).
           MOVE QDS-NO-REPO-3-1-1 TO DS-NO-REPO(3 1 1).
           MOVE QDS-AMT-REPO-3-1-1 TO DS-AMT-REPO(3 1 1).
           MOVE QDS-NO-REPO-3-1-2 TO DS-NO-REPO(3 1 2).
           MOVE QDS-AMT-REPO-3-1-2 TO DS-AMT-REPO(3 1 2).
           MOVE QDS-NO-REPO-3-1-3 TO DS-NO-REPO(3 1 3).
           MOVE QDS-AMT-REPO-3-1-3 TO DS-AMT-REPO(3 1 3).
           MOVE QDS-NO-REPO-3-1-4 TO DS-NO-REPO(3 1 4).
           MOVE QDS-AMT-REPO-3-1-4 TO DS-AMT-REPO(3 1 4).
           MOVE QDS-NO-REPO-3-1-5 TO DS-NO-REPO(3 1 5).
           MOVE QDS-AMT-REPO-3-1-5 TO DS-AMT-REPO(3 1 5).
           MOVE QDS-NO-REPO-3-2-1 TO DS-NO-REPO(3 2 1).
           MOVE QDS-AMT-REPO-3-2-1 TO DS-AMT-REPO(3 2 1).
           MOVE QDS-NO-REPO-3-2-2 TO DS-NO-REPO(3 2 2).
           MOVE QDS-AMT-REPO-3-2-2 TO DS-AMT-REPO(3 2 2).
           MOVE QDS-NO-REPO-3-2-3 TO DS-NO-REPO(3 2 3).
           MOVE QDS-AMT-REPO-3-2-3 TO DS-AMT-REPO(3 2 3).
           MOVE QDS-NO-REPO-3-2-4 TO DS-NO-REPO(3 2 4).
           MOVE QDS-AMT-REPO-3-2-4 TO DS-AMT-REPO(3 2 4).
           MOVE QDS-NO-REPO-3-2-5 TO DS-NO-REPO(3 2 5).
           MOVE QDS-AMT-REPO-3-2-5 TO DS-AMT-REPO(3 2 5).
           MOVE QDS-UNITS-1-1-1 TO DS-UNITS(1 1 1).
           MOVE QDS-OUTBAL-1-1-1 TO DS-OUTBAL(1 1 1).
           MOVE QDS-UNITS-1-1-2 TO DS-UNITS(1 1 2).
           MOVE QDS-OUTBAL-1-1-2 TO DS-OUTBAL(1 1 2).
           MOVE QDS-UNITS-1-1-3 TO DS-UNITS(1 1 3).
           MOVE QDS-OUTBAL-1-1-3 TO DS-OUTBAL(1 1 3).
           MOVE QDS-UNITS-1-1-4 TO DS-UNITS(1 1 4).
           MOVE QDS-OUTBAL-1-1-4 TO DS-OUTBAL(1 1 4).
           MOVE QDS-UNITS-1-1-5 TO DS-UNITS(1 1 5).
           MOVE QDS-OUTBAL-1-1-5 TO DS-OUTBAL(1 1 5).
           MOVE QDS-UNITS-1-1-6 TO DS-UNITS(1 1 6).
           MOVE QDS-OUTBAL-1-1-6 TO DS-OUTBAL(1 1 6).
           MOVE QDS-UNITS-1-1-7 TO DS-UNITS(1 1 7).
           MOVE QDS-OUTBAL-1-1-7 TO DS-OUTBAL(1 1 7).
           MOVE QDS-UNITS-1-1-8 TO DS-UNITS(1 1 8).
           MOVE QDS-OUTBAL-1-1-8 TO DS-OUTBAL(1 1 8).
           MOVE QDS-UNITS-1-1-9 TO DS-UNITS(1 1 9).
           MOVE QDS-OUTBAL-1-1-9 TO DS-OUTBAL(1 1 9).
           MOVE QDS-UNITS-1-1-10 TO DS-UNITS(1 1 10).
           MOVE QDS-OUTBAL-1-1-10 TO DS-OUTBAL(1 1 10).
           MOVE QDS-UNITS-1-2-1 TO DS-UNITS(1 2 1).
           MOVE QDS-OUTBAL-1-2-1 TO DS-OUTBAL(1 2 1).
           MOVE QDS-UNITS-1-2-2 TO DS-UNITS(1 2 2).
           MOVE QDS-OUTBAL-1-2-2 TO DS-OUTBAL(1 2 2).
           MOVE QDS-UNITS-1-2-3 TO DS-UNITS(1 2 3).
           MOVE QDS-OUTBAL-1-2-3 TO DS-OUTBAL(1 2 3).
           MOVE QDS-UNITS-1-2-4 TO DS-UNITS(1 2 4).
           MOVE QDS-OUTBAL-1-2-4 TO DS-OUTBAL(1 2 4).
           MOVE QDS-UNITS-1-2-5 TO DS-UNITS(1 2 5).
           MOVE QDS-OUTBAL-1-2-5 TO DS-OUTBAL(1 2 5).
           MOVE QDS-UNITS-1-2-6 TO DS-UNITS(1 2 6).
           MOVE QDS-OUTBAL-1-2-6 TO DS-OUTBAL(1 2 6).
           MOVE QDS-UNITS-1-2-7 TO DS-UNITS(1 2 7).
           MOVE QDS-OUTBAL-1-2-7 TO DS-OUTBAL(1 2 7).
           MOVE QDS-UNITS-1-2-8 TO DS-UNITS(1 2 8).
           MOVE QDS-OUTBAL-1-2-8 TO DS-OUTBAL(1 2 8).
           MOVE QDS-UNITS-1-2-9 TO DS-UNITS(1 2 9).
           MOVE QDS-OUTBAL-1-2-9 TO DS-OUTBAL(1 2 9).
           MOVE QDS-UNITS-1-2-10 TO DS-UNITS(1 2 10).
           MOVE QDS-OUTBAL-1-2-10 TO DS-OUTBAL(1 2 10).
           MOVE QDS-UNITS-2-1-1 TO DS-UNITS(2 1 1).
           MOVE QDS-OUTBAL-2-1-1 TO DS-OUTBAL(2 1 1).
           MOVE QDS-UNITS-2-1-2 TO DS-UNITS(2 1 2).
           MOVE QDS-OUTBAL-2-1-2 TO DS-OUTBAL(2 1 2).
           MOVE QDS-UNITS-2-1-3 TO DS-UNITS(2 1 3).
           MOVE QDS-OUTBAL-2-1-3 TO DS-OUTBAL(2 1 3).
           MOVE QDS-UNITS-2-1-4 TO DS-UNITS(2 1 4).
           MOVE QDS-OUTBAL-2-1-4 TO DS-OUTBAL(2 1 4).
           MOVE QDS-UNITS-2-1-5 TO DS-UNITS(2 1 5).
           MOVE QDS-OUTBAL-2-1-5 TO DS-OUTBAL(2 1 5).
           MOVE QDS-UNITS-2-1-6 TO DS-UNITS(2 1 6).
           MOVE QDS-OUTBAL-2-1-6 TO DS-OUTBAL(2 1 6).
           MOVE QDS-UNITS-2-1-7 TO DS-UNITS(2 1 7).
           MOVE QDS-OUTBAL-2-1-7 TO DS-OUTBAL(2 1 7).
           MOVE QDS-UNITS-2-1-8 TO DS-UNITS(2 1 8).
           MOVE QDS-OUTBAL-2-1-8 TO DS-OUTBAL(2 1 8).
           MOVE QDS-UNITS-2-1-9 TO DS-UNITS(2 1 9).
           MOVE QDS-OUTBAL-2-1-9 TO DS-OUTBAL(2 1 9).
           MOVE QDS-UNITS-2-1-10 TO DS-UNITS(2 1 10).
           MOVE QDS-OUTBAL-2-1-10 TO DS-OUTBAL(2 1 10).
           MOVE QDS-UNITS-2-2-1 TO DS-UNITS(2 2 1).
           MOVE QDS-OUTBAL-2-2-1 TO DS-OUTBAL(2 2 1).
           MOVE QDS-UNITS-2-2-2 TO DS-UNITS(2 2 2).
           MOVE QDS-OUTBAL-2-2-2 TO DS-OUTBAL(2 2 2).
           MOVE QDS-UNITS-2-2-3 TO DS-UNITS(2 2 3).
           MOVE QDS-OUTBAL-2-2-3 TO DS-OUTBAL(2 2 3).
           MOVE QDS-UNITS-2-2-4 TO DS-UNITS(2 2 4).
           MOVE QDS-OUTBAL-2-2-4 TO DS-OUTBAL(2 2 4).
           MOVE QDS-UNITS-2-2-5 TO DS-UNITS(2 2 5).
           MOVE QDS-OUTBAL-2-2-5 TO DS-OUTBAL(2 2 5).
           MOVE QDS-UNITS-2-2-6 TO DS-UNITS(2 2 6).
           MOVE QDS-OUTBAL-2-2-6 TO DS-OUTBAL(2 2 6).
           MOVE QDS-UNITS-2-2-7 TO DS-UNITS(2 2 7).
           MOVE QDS-OUTBAL-2-2-7 TO DS-OUTBAL(2 2 7).
           MOVE QDS-UNITS-2-2-8 TO DS-UNITS(2 2 8).
           MOVE QDS-OUTBAL-2-2-8 TO DS-OUTBAL(2 2 8).
           MOVE QDS-UNITS-2-2-9 TO DS-UNITS(2 2 9).
           MOVE QDS-OUTBAL-2-2-9 TO DS-OUTBAL(2 2 9).
           MOVE QDS-UNITS-2-2-10 TO DS-UNITS(2 2 10).
           MOVE QDS-OUTBAL-2-2-10 TO DS-OUTBAL(2 2 10).
           MOVE QDS-UNITS-3-1-1 TO DS-UNITS(3 1 1).
           MOVE QDS-OUTBAL-3-1-1 TO DS-OUTBAL(3 1 1).
           MOVE QDS-UNITS-3-1-2 TO DS-UNITS(3 1 2).
           MOVE QDS-OUTBAL-3-1-2 TO DS-OUTBAL(3 1 2).
           MOVE QDS-UNITS-3-1-3 TO DS-UNITS(3 1 3).
           MOVE QDS-OUTBAL-3-1-3 TO DS-OUTBAL(3 1 3).
           MOVE QDS-UNITS-3-1-4 TO DS-UNITS(3 1 4).
           MOVE QDS-OUTBAL-3-1-4 TO DS-OUTBAL(3 1 4).
           MOVE QDS-UNITS-3-1-5 TO DS-UNITS(3 1 5).
           MOVE QDS-OUTBAL-3-1-5 TO DS-OUTBAL(3 1 5).
           MOVE QDS-UNITS-3-1-6 TO DS-UNITS(3 1 6).
           MOVE QDS-OUTBAL-3-1-6 TO DS-OUTBAL(3 1 6).
           MOVE QDS-UNITS-3-1-7 TO DS-UNITS(3 1 7).
           MOVE QDS-OUTBAL-3-1-7 TO DS-OUTBAL(3 1 7).
           MOVE QDS-UNITS-3-1-8 TO DS-UNITS(3 1 8).
           MOVE QDS-OUTBAL-3-1-8 TO DS-OUTBAL(3 1 8).
           MOVE QDS-UNITS-3-1-9 TO DS-UNITS(3 1 9).
           MOVE QDS-OUTBAL-3-1-9 TO DS-OUTBAL(3 1 9).
           MOVE QDS-UNITS-3-1-10 TO DS-UNITS(3 1 10).
           MOVE QDS-OUTBAL-3-1-10 TO DS-OUTBAL(3 1 10).
           MOVE QDS-UNITS-3-2-1 TO DS-UNITS(3 2 1).
           MOVE QDS-OUTBAL-3-2-1 TO DS-OUTBAL(3 2 1).
           MOVE QDS-UNITS-3-2-2 TO DS-UNITS(3 2 2).
           MOVE QDS-OUTBAL-3-2-2 TO DS-OUTBAL(3 2 2).
           MOVE QDS-UNITS-3-2-3 TO DS-UNITS(3 2 3).
           MOVE QDS-OUTBAL-3-2-3 TO DS-OUTBAL(3 2 3).
           MOVE QDS-UNITS-3-2-4 TO DS-UNITS(3 2 4).
           MOVE QDS-OUTBAL-3-2-4 TO DS-OUTBAL(3 2 4).
           MOVE QDS-UNITS-3-2-5 TO DS-UNITS(3 2 5).
           MOVE QDS-OUTBAL-3-2-5 TO DS-OUTBAL(3 2 5).
           MOVE QDS-UNITS-3-2-6 TO DS-UNITS(3 2 6).
           MOVE QDS-OUTBAL-3-2-6 TO DS-OUTBAL(3 2 6).
           MOVE QDS-UNITS-3-2-7 TO DS-UNITS(3 2 7).
           MOVE QDS-OUTBAL-3-2-7 TO DS-OUTBAL(3 2 7).
           MOVE QDS-UNITS-3-2-8 TO DS-UNITS(3 2 8).
           MOVE QDS-OUTBAL-3-2-8 TO DS-OUTBAL(3 2 8).
           MOVE QDS-UNITS-3-2-9 TO DS-UNITS(3 2 9).
           MOVE QDS-OUTBAL-3-2-9 TO DS-OUTBAL(3 2 9).
           MOVE QDS-UNITS-3-2-10 TO DS-UNITS(3 2 10).
           MOVE QDS-OUTBAL-3-2-10 TO DS-OUTBAL(3 2 10).
           MOVE QDS-UNITS-BNKRPT-1-1-1 TO DS-UNITS-BNKRPT(1 1 1).
           MOVE QDS-OUTBAL-BNKRPT-1-1-1 TO DS-OUTBAL-BNKRPT(1 1 1).
           MOVE QDS-UNITS-BNKRPT-1-1-2 TO DS-UNITS-BNKRPT(1 1 2).
           MOVE QDS-OUTBAL-BNKRPT-1-1-2 TO DS-OUTBAL-BNKRPT(1 1 2).
           MOVE QDS-UNITS-BNKRPT-1-1-3 TO DS-UNITS-BNKRPT(1 1 3).
           MOVE QDS-OUTBAL-BNKRPT-1-1-3 TO DS-OUTBAL-BNKRPT(1 1 3).
           MOVE QDS-UNITS-BNKRPT-1-1-4 TO DS-UNITS-BNKRPT(1 1 4).
           MOVE QDS-OUTBAL-BNKRPT-1-1-4 TO DS-OUTBAL-BNKRPT(1 1 4).
           MOVE QDS-UNITS-BNKRPT-1-1-5 TO DS-UNITS-BNKRPT(1 1 5).
           MOVE QDS-OUTBAL-BNKRPT-1-1-5 TO DS-OUTBAL-BNKRPT(1 1 5).
           MOVE QDS-UNITS-BNKRPT-1-1-6 TO DS-UNITS-BNKRPT(1 1 6).
           MOVE QDS-OUTBAL-BNKRPT-1-1-6 TO DS-OUTBAL-BNKRPT(1 1 6).
           MOVE QDS-UNITS-BNKRPT-1-1-7 TO DS-UNITS-BNKRPT(1 1 7).
           MOVE QDS-OUTBAL-BNKRPT-1-1-7 TO DS-OUTBAL-BNKRPT(1 1 7).
           MOVE QDS-UNITS-BNKRPT-1-1-8 TO DS-UNITS-BNKRPT(1 1 8).
           MOVE QDS-OUTBAL-BNKRPT-1-1-8 TO DS-OUTBAL-BNKRPT(1 1 8).
           MOVE QDS-UNITS-BNKRPT-1-1-9 TO DS-UNITS-BNKRPT(1 1 9).
           MOVE QDS-OUTBAL-BNKRPT-1-1-9 TO DS-OUTBAL-BNKRPT(1 1 9).
           MOVE QDS-UNITS-BNKRPT-1-1-10 TO DS-UNITS-BNKRPT(1 1 10).
           MOVE QDS-OUTBAL-BNKRPT-1-1-10 TO DS-OUTBAL-BNKRPT(1 1 10).
           MOVE QDS-UNITS-BNKRPT-1-2-1 TO DS-UNITS-BNKRPT(1 2 1).
           MOVE QDS-OUTBAL-BNKRPT-1-2-1 TO DS-OUTBAL-BNKRPT(1 2 1).
           MOVE QDS-UNITS-BNKRPT-1-2-2 TO DS-UNITS-BNKRPT(1 2 2).
           MOVE QDS-OUTBAL-BNKRPT-1-2-2 TO DS-OUTBAL-BNKRPT(1 2 2).
           MOVE QDS-UNITS-BNKRPT-1-2-3 TO DS-UNITS-BNKRPT(1 2 3).
           MOVE QDS-OUTBAL-BNKRPT-1-2-3 TO DS-OUTBAL-BNKRPT(1 2 3).
           MOVE QDS-UNITS-BNKRPT-1-2-4 TO DS-UNITS-BNKRPT(1 2 4).
           MOVE QDS-OUTBAL-BNKRPT-1-2-4 TO DS-OUTBAL-BNKRPT(1 2 4).
           MOVE QDS-UNITS-BNKRPT-1-2-5 TO DS-UNITS-BNKRPT(1 2 5).
           MOVE QDS-OUTBAL-BNKRPT-1-2-5 TO DS-OUTBAL-BNKRPT(1 2 5).
           MOVE QDS-UNITS-BNKRPT-1-2-6 TO DS-UNITS-BNKRPT(1 2 6).
           MOVE QDS-OUTBAL-BNKRPT-1-2-6 TO DS-OUTBAL-BNKRPT(1 2 6).
           MOVE QDS-UNITS-BNKRPT-1-2-7 TO DS-UNITS-BNKRPT(1 2 7).
           MOVE QDS-OUTBAL-BNKRPT-1-2-7 TO DS-OUTBAL-BNKRPT(1 2 7).
           MOVE QDS-UNITS-BNKRPT-1-2-8 TO DS-UNITS-BNKRPT(1 2 8).
           MOVE QDS-OUTBAL-BNKRPT-1-2-8 TO DS-OUTBAL-BNKRPT(1 2 8).
           MOVE QDS-UNITS-BNKRPT-1-2-9 TO DS-UNITS-BNKRPT(1 2 9).
           MOVE QDS-OUTBAL-BNKRPT-1-2-9 TO DS-OUTBAL-BNKRPT(1 2 9).
           MOVE QDS-UNITS-BNKRPT-1-2-10 TO DS-UNITS-BNKRPT(1 2 10).
           MOVE QDS-OUTBAL-BNKRPT-1-2-10 TO DS-OUTBAL-BNKRPT(1 2 10).
           MOVE QDS-UNITS-BNKRPT-2-1-1 TO DS-UNITS-BNKRPT(2 1 1).
           MOVE QDS-OUTBAL-BNKRPT-2-1-1 TO DS-OUTBAL-BNKRPT(2 1 1).
           MOVE QDS-UNITS-BNKRPT-2-1-2 TO DS-UNITS-BNKRPT(2 1 2).
           MOVE QDS-OUTBAL-BNKRPT-2-1-2 TO DS-OUTBAL-BNKRPT(2 1 2).
           MOVE QDS-UNITS-BNKRPT-2-1-3 TO DS-UNITS-BNKRPT(2 1 3).
           MOVE QDS-OUTBAL-BNKRPT-2-1-3 TO DS-OUTBAL-BNKRPT(2 1 3).
           MOVE QDS-UNITS-BNKRPT-2-1-4 TO DS-UNITS-BNKRPT(2 1 4).
           MOVE QDS-OUTBAL-BNKRPT-2-1-4 TO DS-OUTBAL-BNKRPT(2 1 4).
           MOVE QDS-UNITS-BNKRPT-2-1-5 TO DS-UNITS-BNKRPT(2 1 5).
           MOVE QDS-OUTBAL-BNKRPT-2-1-5 TO DS-OUTBAL-BNKRPT(2 1 5).
           MOVE QDS-UNITS-BNKRPT-2-1-6 TO DS-UNITS-BNKRPT(2 1 6).
           MOVE QDS-OUTBAL-BNKRPT-2-1-6 TO DS-OUTBAL-BNKRPT(2 1 6).
           MOVE QDS-UNITS-BNKRPT-2-1-7 TO DS-UNITS-BNKRPT(2 1 7).
           MOVE QDS-OUTBAL-BNKRPT-2-1-7 TO DS-OUTBAL-BNKRPT(2 1 7).
           MOVE QDS-UNITS-BNKRPT-2-1-8 TO DS-UNITS-BNKRPT(2 1 8).
           MOVE QDS-OUTBAL-BNKRPT-2-1-8 TO DS-OUTBAL-BNKRPT(2 1 8).
           MOVE QDS-UNITS-BNKRPT-2-1-9 TO DS-UNITS-BNKRPT(2 1 9).
           MOVE QDS-OUTBAL-BNKRPT-2-1-9 TO DS-OUTBAL-BNKRPT(2 1 9).
           MOVE QDS-UNITS-BNKRPT-2-1-10 TO DS-UNITS-BNKRPT(2 1 10).
           MOVE QDS-OUTBAL-BNKRPT-2-1-10 TO DS-OUTBAL-BNKRPT(2 1 10).
           MOVE QDS-UNITS-BNKRPT-2-2-1 TO DS-UNITS-BNKRPT(2 2 1).
           MOVE QDS-OUTBAL-BNKRPT-2-2-1 TO DS-OUTBAL-BNKRPT(2 2 1).
           MOVE QDS-UNITS-BNKRPT-2-2-2 TO DS-UNITS-BNKRPT(2 2 2).
           MOVE QDS-OUTBAL-BNKRPT-2-2-2 TO DS-OUTBAL-BNKRPT(2 2 2).
           MOVE QDS-UNITS-BNKRPT-2-2-3 TO DS-UNITS-BNKRPT(2 2 3).
           MOVE QDS-OUTBAL-BNKRPT-2-2-3 TO DS-OUTBAL-BNKRPT(2 2 3).
           MOVE QDS-UNITS-BNKRPT-2-2-4 TO DS-UNITS-BNKRPT(2 2 4).
           MOVE QDS-OUTBAL-BNKRPT-2-2-4 TO DS-OUTBAL-BNKRPT(2 2 4).
           MOVE QDS-UNITS-BNKRPT-2-2-5 TO DS-UNITS-BNKRPT(2 2 5).
           MOVE QDS-OUTBAL-BNKRPT-2-2-5 TO DS-OUTBAL-BNKRPT(2 2 5).
           MOVE QDS-UNITS-BNKRPT-2-2-6 TO DS-UNITS-BNKRPT(2 2 6).
           MOVE QDS-OUTBAL-BNKRPT-2-2-6 TO DS-OUTBAL-BNKRPT(2 2 6).
           MOVE QDS-UNITS-BNKRPT-2-2-7 TO DS-UNITS-BNKRPT(2 2 7).
           MOVE QDS-OUTBAL-BNKRPT-2-2-7 TO DS-OUTBAL-BNKRPT(2 2 7).
           MOVE QDS-UNITS-BNKRPT-2-2-8 TO DS-UNITS-BNKRPT(2 2 8).
           MOVE QDS-OUTBAL-BNKRPT-2-2-8 TO DS-OUTBAL-BNKRPT(2 2 8).
           MOVE QDS-UNITS-BNKRPT-2-2-9 TO DS-UNITS-BNKRPT(2 2 9).
           MOVE QDS-OUTBAL-BNKRPT-2-2-9 TO DS-OUTBAL-BNKRPT(2 2 9).
           MOVE QDS-UNITS-BNKRPT-2-2-10 TO DS-UNITS-BNKRPT(2 2 10).
           MOVE QDS-OUTBAL-BNKRPT-2-2-10 TO DS-OUTBAL-BNKRPT(2 2 10).
           MOVE QDS-UNITS-BNKRPT-3-1-1 TO DS-UNITS-BNKRPT(3 1 1).
           MOVE QDS-OUTBAL-BNKRPT-3-1-1 TO DS-OUTBAL-BNKRPT(3 1 1).
           MOVE QDS-UNITS-BNKRPT-3-1-2 TO DS-UNITS-BNKRPT(3 1 2).
           MOVE QDS-OUTBAL-BNKRPT-3-1-2 TO DS-OUTBAL-BNKRPT(3 1 2).
           MOVE QDS-UNITS-BNKRPT-3-1-3 TO DS-UNITS-BNKRPT(3 1 3).
           MOVE QDS-OUTBAL-BNKRPT-3-1-3 TO DS-OUTBAL-BNKRPT(3 1 3).
           MOVE QDS-UNITS-BNKRPT-3-1-4 TO DS-UNITS-BNKRPT(3 1 4).
           MOVE QDS-OUTBAL-BNKRPT-3-1-4 TO DS-OUTBAL-BNKRPT(3 1 4).
           MOVE QDS-UNITS-BNKRPT-3-1-5 TO DS-UNITS-BNKRPT(3 1 5).
           MOVE QDS-OUTBAL-BNKRPT-3-1-5 TO DS-OUTBAL-BNKRPT(3 1 5).
           MOVE QDS-UNITS-BNKRPT-3-1-6 TO DS-UNITS-BNKRPT(3 1 6).
           MOVE QDS-OUTBAL-BNKRPT-3-1-6 TO DS-OUTBAL-BNKRPT(3 1 6).
           MOVE QDS-UNITS-BNKRPT-3-1-7 TO DS-UNITS-BNKRPT(3 1 7).
           MOVE QDS-OUTBAL-BNKRPT-3-1-7 TO DS-OUTBAL-BNKRPT(3 1 7).
           MOVE QDS-UNITS-BNKRPT-3-1-8 TO DS-UNITS-BNKRPT(3 1 8).
           MOVE QDS-OUTBAL-BNKRPT-3-1-8 TO DS-OUTBAL-BNKRPT(3 1 8).
           MOVE QDS-UNITS-BNKRPT-3-1-9 TO DS-UNITS-BNKRPT(3 1 9).
           MOVE QDS-OUTBAL-BNKRPT-3-1-9 TO DS-OUTBAL-BNKRPT(3 1 9).
           MOVE QDS-UNITS-BNKRPT-3-1-10 TO DS-UNITS-BNKRPT(3 1 10).
           MOVE QDS-OUTBAL-BNKRPT-3-1-10 TO DS-OUTBAL-BNKRPT(3 1 10).
           MOVE QDS-UNITS-BNKRPT-3-2-1 TO DS-UNITS-BNKRPT(3 2 1).
           MOVE QDS-OUTBAL-BNKRPT-3-2-1 TO DS-OUTBAL-BNKRPT(3 2 1).
           MOVE QDS-UNITS-BNKRPT-3-2-2 TO DS-UNITS-BNKRPT(3 2 2).
           MOVE QDS-OUTBAL-BNKRPT-3-2-2 TO DS-OUTBAL-BNKRPT(3 2 2).
           MOVE QDS-UNITS-BNKRPT-3-2-3 TO DS-UNITS-BNKRPT(3 2 3).
           MOVE QDS-OUTBAL-BNKRPT-3-2-3 TO DS-OUTBAL-BNKRPT(3 2 3).
           MOVE QDS-UNITS-BNKRPT-3-2-4 TO DS-UNITS-BNKRPT(3 2 4).
           MOVE QDS-OUTBAL-BNKRPT-3-2-4 TO DS-OUTBAL-BNKRPT(3 2 4).
           MOVE QDS-UNITS-BNKRPT-3-2-5 TO DS-UNITS-BNKRPT(3 2 5).
           MOVE QDS-OUTBAL-BNKRPT-3-2-5 TO DS-OUTBAL-BNKRPT(3 2 5).
           MOVE QDS-UNITS-BNKRPT-3-2-6 TO DS-UNITS-BNKRPT(3 2 6).
           MOVE QDS-OUTBAL-BNKRPT-3-2-6 TO DS-OUTBAL-BNKRPT(3 2 6).
           MOVE QDS-UNITS-BNKRPT-3-2-7 TO DS-UNITS-BNKRPT(3 2 7).
           MOVE QDS-OUTBAL-BNKRPT-3-2-7 TO DS-OUTBAL-BNKRPT(3 2 7).
           MOVE QDS-UNITS-BNKRPT-3-2-8 TO DS-UNITS-BNKRPT(3 2 8).
           MOVE QDS-OUTBAL-BNKRPT-3-2-8 TO DS-OUTBAL-BNKRPT(3 2 8).
           MOVE QDS-UNITS-BNKRPT-3-2-9 TO DS-UNITS-BNKRPT(3 2 9).
           MOVE QDS-OUTBAL-BNKRPT-3-2-9 TO DS-OUTBAL-BNKRPT(3 2 9).
           MOVE QDS-UNITS-BNKRPT-3-2-10 TO DS-UNITS-BNKRPT(3 2 10).
           MOVE QDS-OUTBAL-BNKRPT-3-2-10 TO DS-OUTBAL-BNKRPT(3 2 10).
           MOVE QDS-UNEARNED-AMT-1-1-1 TO DS-UNEARNED-AMT(1 1 1).
           MOVE QDS-UNEARNED-AMT-1-1-2 TO DS-UNEARNED-AMT(1 1 2).
           MOVE QDS-UNEARNED-AMT-1-1-3 TO DS-UNEARNED-AMT(1 1 3).
           MOVE QDS-UNEARNED-AMT-1-1-4 TO DS-UNEARNED-AMT(1 1 4).
           MOVE QDS-UNEARNED-AMT-1-1-5 TO DS-UNEARNED-AMT(1 1 5).
           MOVE QDS-UNEARNED-AMT-1-1-6 TO DS-UNEARNED-AMT(1 1 6).
           MOVE QDS-UNEARNED-AMT-1-1-7 TO DS-UNEARNED-AMT(1 1 7).
           MOVE QDS-UNEARNED-AMT-1-1-8 TO DS-UNEARNED-AMT(1 1 8).
           MOVE QDS-UNEARNED-AMT-1-1-9 TO DS-UNEARNED-AMT(1 1 9).
           MOVE QDS-UNEARNED-AMT-1-1-10 TO DS-UNEARNED-AMT(1 1 10).
           MOVE QDS-UNEARNED-AMT-1-2-1 TO DS-UNEARNED-AMT(1 2 1).
           MOVE QDS-UNEARNED-AMT-1-2-2 TO DS-UNEARNED-AMT(1 2 2).
           MOVE QDS-UNEARNED-AMT-1-2-3 TO DS-UNEARNED-AMT(1 2 3).
           MOVE QDS-UNEARNED-AMT-1-2-4 TO DS-UNEARNED-AMT(1 2 4).
           MOVE QDS-UNEARNED-AMT-1-2-5 TO DS-UNEARNED-AMT(1 2 5).
           MOVE QDS-UNEARNED-AMT-1-2-6 TO DS-UNEARNED-AMT(1 2 6).
           MOVE QDS-UNEARNED-AMT-1-2-7 TO DS-UNEARNED-AMT(1 2 7).
           MOVE QDS-UNEARNED-AMT-1-2-8 TO DS-UNEARNED-AMT(1 2 8).
           MOVE QDS-UNEARNED-AMT-1-2-9 TO DS-UNEARNED-AMT(1 2 9).
           MOVE QDS-UNEARNED-AMT-1-2-10 TO DS-UNEARNED-AMT(1 2 10).
           MOVE QDS-UNEARNED-AMT-2-1-1 TO DS-UNEARNED-AMT(2 1 1).
           MOVE QDS-UNEARNED-AMT-2-1-2 TO DS-UNEARNED-AMT(2 1 2).
           MOVE QDS-UNEARNED-AMT-2-1-3 TO DS-UNEARNED-AMT(2 1 3).
           MOVE QDS-UNEARNED-AMT-2-1-4 TO DS-UNEARNED-AMT(2 1 4).
           MOVE QDS-UNEARNED-AMT-2-1-5 TO DS-UNEARNED-AMT(2 1 5).
           MOVE QDS-UNEARNED-AMT-2-1-6 TO DS-UNEARNED-AMT(2 1 6).
           MOVE QDS-UNEARNED-AMT-2-1-7 TO DS-UNEARNED-AMT(2 1 7).
           MOVE QDS-UNEARNED-AMT-2-1-8 TO DS-UNEARNED-AMT(2 1 8).
           MOVE QDS-UNEARNED-AMT-2-1-9 TO DS-UNEARNED-AMT(2 1 9).
           MOVE QDS-UNEARNED-AMT-2-1-10 TO DS-UNEARNED-AMT(2 1 10).
           MOVE QDS-UNEARNED-AMT-2-2-1 TO DS-UNEARNED-AMT(2 2 1).
           MOVE QDS-UNEARNED-AMT-2-2-2 TO DS-UNEARNED-AMT(2 2 2).
           MOVE QDS-UNEARNED-AMT-2-2-3 TO DS-UNEARNED-AMT(2 2 3).
           MOVE QDS-UNEARNED-AMT-2-2-4 TO DS-UNEARNED-AMT(2 2 4).
           MOVE QDS-UNEARNED-AMT-2-2-5 TO DS-UNEARNED-AMT(2 2 5).
           MOVE QDS-UNEARNED-AMT-2-2-6 TO DS-UNEARNED-AMT(2 2 6).
           MOVE QDS-UNEARNED-AMT-2-2-7 TO DS-UNEARNED-AMT(2 2 7).
           MOVE QDS-UNEARNED-AMT-2-2-8 TO DS-UNEARNED-AMT(2 2 8).
           MOVE QDS-UNEARNED-AMT-2-2-9 TO DS-UNEARNED-AMT(2 2 9).
           MOVE QDS-UNEARNED-AMT-2-2-10 TO DS-UNEARNED-AMT(2 2 10).
           MOVE QDS-UNEARNED-AMT-3-1-1 TO DS-UNEARNED-AMT(3 1 1).
           MOVE QDS-UNEARNED-AMT-3-1-2 TO DS-UNEARNED-AMT(3 1 2).
           MOVE QDS-UNEARNED-AMT-3-1-3 TO DS-UNEARNED-AMT(3 1 3).
           MOVE QDS-UNEARNED-AMT-3-1-4 TO DS-UNEARNED-AMT(3 1 4).
           MOVE QDS-UNEARNED-AMT-3-1-5 TO DS-UNEARNED-AMT(3 1 5).
           MOVE QDS-UNEARNED-AMT-3-1-6 TO DS-UNEARNED-AMT(3 1 6).
           MOVE QDS-UNEARNED-AMT-3-1-7 TO DS-UNEARNED-AMT(3 1 7).
           MOVE QDS-UNEARNED-AMT-3-1-8 TO DS-UNEARNED-AMT(3 1 8).
           MOVE QDS-UNEARNED-AMT-3-1-9 TO DS-UNEARNED-AMT(3 1 9).
           MOVE QDS-UNEARNED-AMT-3-1-10 TO DS-UNEARNED-AMT(3 1 10).
           MOVE QDS-UNEARNED-AMT-3-2-1 TO DS-UNEARNED-AMT(3 2 1).
           MOVE QDS-UNEARNED-AMT-3-2-2 TO DS-UNEARNED-AMT(3 2 2).
           MOVE QDS-UNEARNED-AMT-3-2-3 TO DS-UNEARNED-AMT(3 2 3).
           MOVE QDS-UNEARNED-AMT-3-2-4 TO DS-UNEARNED-AMT(3 2 4).
           MOVE QDS-UNEARNED-AMT-3-2-5 TO DS-UNEARNED-AMT(3 2 5).
           MOVE QDS-UNEARNED-AMT-3-2-6 TO DS-UNEARNED-AMT(3 2 6).
           MOVE QDS-UNEARNED-AMT-3-2-7 TO DS-UNEARNED-AMT(3 2 7).
           MOVE QDS-UNEARNED-AMT-3-2-8 TO DS-UNEARNED-AMT(3 2 8).
           MOVE QDS-UNEARNED-AMT-3-2-9 TO DS-UNEARNED-AMT(3 2 9).
           MOVE QDS-UNEARNED-AMT-3-2-10 TO DS-UNEARNED-AMT(3 2 10).
           MOVE QDS-DLDISC-VOL-1 TO DS-DLDISC-VOL(1).
           MOVE QDS-DLDISC-VOL-2 TO DS-DLDISC-VOL(2).
           MOVE QDS-DLDISC-VOL-3 TO DS-DLDISC-VOL(3).
           MOVE QDS-AFPDUEDL-MEMO TO DS-AFPDUEDL-MEMO.
           MOVE QDS-UNPAID-MANUAL TO DS-UNPAID-MANUAL.

           IF ( QDS-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO DS-LTOUCH-DATE
           ELSE
              MOVE QDS-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO DS-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QDS-REC FROM DS-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-DS-FIELDS.
           MOVE DS-DATE-CCYYMM TO QDS-DATE-CCYYMM.
           MOVE DS-BRANCH TO QDS-BRANCH.
           MOVE DS-NUM TO QDS-NUM.
           MOVE DS-CLASS TO QDS-CLASS.
           MOVE DS-DLRVPRIN(1) TO QDS-DLRVPRIN-1.
           MOVE DS-DLRVPRIN(2) TO QDS-DLRVPRIN-2.
           MOVE DS-DLRVPRIN(3) TO QDS-DLRVPRIN-3.
           MOVE DS-DLRVCHGS(1) TO QDS-DLRVCHGS-1.
           MOVE DS-DLRVCHGS(2) TO QDS-DLRVCHGS-2.
           MOVE DS-DLRVCHGS(3) TO QDS-DLRVCHGS-3.
           MOVE DS-DLPARVHELD TO QDS-DLPARVHELD.
           MOVE DS-UNPAIDCK TO QDS-UNPAIDCK.
           MOVE DS-DLDISC TO QDS-DLDISC.
           MOVE DS-DLPARVPAID TO QDS-DLPARVPAID.
           MOVE DS-SUMWGTTM TO QDS-SUMWGTTM.
           MOVE DS-SUMWGTAV TO QDS-SUMWGTAV.
           MOVE DS-SUMWGTTA TO QDS-SUMWGTTA.
           MOVE DS-RCOURSE-UNITS(1) TO QDS-RCOURSE-UNITS-1.
           MOVE DS-RCOURSE-UNITS(2) TO QDS-RCOURSE-UNITS-2.
           MOVE DS-RCOURSE-UNITS(3) TO QDS-RCOURSE-UNITS-3.
           MOVE DS-RCOURSE-AMT(1) TO QDS-RCOURSE-AMT-1.
           MOVE DS-RCOURSE-AMT(2) TO QDS-RCOURSE-AMT-2.
           MOVE DS-RCOURSE-AMT(3) TO QDS-RCOURSE-AMT-3.
           MOVE DS-NPURUNIT(1) TO QDS-NPURUNIT-1.
           MOVE DS-NPURUNIT(2) TO QDS-NPURUNIT-2.
           MOVE DS-NPURUNIT(3) TO QDS-NPURUNIT-3.
           MOVE DS-NPURAMT(1) TO QDS-NPURAMT-1.
           MOVE DS-NPURAMT(2) TO QDS-NPURAMT-2.
           MOVE DS-NPURAMT(3) TO QDS-NPURAMT-3.
           MOVE DS-OPURUNIT(1) TO QDS-OPURUNIT-1.
           MOVE DS-OPURUNIT(2) TO QDS-OPURUNIT-2.
           MOVE DS-OPURUNIT(3) TO QDS-OPURUNIT-3.
           MOVE DS-OPURAMT(1) TO QDS-OPURAMT-1.
           MOVE DS-OPURAMT(2) TO QDS-OPURAMT-2.
           MOVE DS-OPURAMT(3) TO QDS-OPURAMT-3.
           MOVE DS-RNEWUNIT(1) TO QDS-RNEWUNIT-1.
           MOVE DS-RNEWUNIT(2) TO QDS-RNEWUNIT-2.
           MOVE DS-RNEWUNIT(3) TO QDS-RNEWUNIT-3.
           MOVE DS-RNEWAMT(1) TO QDS-RNEWAMT-1.
           MOVE DS-RNEWAMT(2) TO QDS-RNEWAMT-2.
           MOVE DS-RNEWAMT(3) TO QDS-RNEWAMT-3.
           MOVE DS-PAIDUNIT(1) TO QDS-PAIDUNIT-1.
           MOVE DS-PAIDUNIT(2) TO QDS-PAIDUNIT-2.
           MOVE DS-PAIDUNIT(3) TO QDS-PAIDUNIT-3.
           MOVE DS-PAIDAMT(1) TO QDS-PAIDAMT-1.
           MOVE DS-PAIDAMT(2) TO QDS-PAIDAMT-2.
           MOVE DS-PAIDAMT(3) TO QDS-PAIDAMT-3.
           MOVE DS-TODLUNIT(1) TO QDS-TODLUNIT-1.
           MOVE DS-TODLUNIT(2) TO QDS-TODLUNIT-2.
           MOVE DS-TODLUNIT(3) TO QDS-TODLUNIT-3.
           MOVE DS-TODLAMT(1) TO QDS-TODLAMT-1.
           MOVE DS-TODLAMT(2) TO QDS-TODLAMT-2.
           MOVE DS-TODLAMT(3) TO QDS-TODLAMT-3.
           MOVE DS-OUTSUNIT(1) TO QDS-OUTSUNIT-1.
           MOVE DS-OUTSUNIT(2) TO QDS-OUTSUNIT-2.
           MOVE DS-OUTSUNIT(3) TO QDS-OUTSUNIT-3.
           MOVE DS-OUTSAMT(1) TO QDS-OUTSAMT-1.
           MOVE DS-OUTSAMT(2) TO QDS-OUTSAMT-2.
           MOVE DS-OUTSAMT(3) TO QDS-OUTSAMT-3.
           MOVE DS-PLOSUNIT(1) TO QDS-PLOSUNIT-1.
           MOVE DS-PLOSUNIT(2) TO QDS-PLOSUNIT-2.
           MOVE DS-PLOSUNIT(3) TO QDS-PLOSUNIT-3.
           MOVE DS-PLOSAMT(1) TO QDS-PLOSAMT-1.
           MOVE DS-PLOSAMT(2) TO QDS-PLOSAMT-2.
           MOVE DS-PLOSAMT(3) TO QDS-PLOSAMT-3.
           MOVE DS-RCPLUNIT(1) TO QDS-RCPLUNIT-1.
           MOVE DS-RCPLUNIT(2) TO QDS-RCPLUNIT-2.
           MOVE DS-RCPLUNIT(3) TO QDS-RCPLUNIT-3.
           MOVE DS-RCPLAMT(1) TO QDS-RCPLAMT-1.
           MOVE DS-RCPLAMT(2) TO QDS-RCPLAMT-2.
           MOVE DS-RCPLAMT(3) TO QDS-RCPLAMT-3.
           MOVE DS-JDOSUNIT(1) TO QDS-JDOSUNIT-1.
           MOVE DS-JDOSUNIT(2) TO QDS-JDOSUNIT-2.
           MOVE DS-JDOSUNIT(3) TO QDS-JDOSUNIT-3.
           MOVE DS-JDOSAMT(1) TO QDS-JDOSAMT-1.
           MOVE DS-JDOSAMT(2) TO QDS-JDOSAMT-2.
           MOVE DS-JDOSAMT(3) TO QDS-JDOSAMT-3.

           IF ( DS-DELUNEARN-DATE = ZEROES )
              MOVE "1900-01-01" TO QDS-DELUNEARN-DATE
           ELSE
              MOVE DS-DELUNEARN-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QDS-DELUNEARN-DATE.

           MOVE DS-PROCEEDS(1) TO QDS-PROCEEDS-1.
           MOVE DS-PROCEEDS(2) TO QDS-PROCEEDS-2.
           MOVE DS-NO-REPO(1 1 1) TO QDS-NO-REPO-1-1-1.
           MOVE DS-AMT-REPO(1 1 1) TO QDS-AMT-REPO-1-1-1.
           MOVE DS-NO-REPO(1 1 2) TO QDS-NO-REPO-1-1-2.
           MOVE DS-AMT-REPO(1 1 2) TO QDS-AMT-REPO-1-1-2.
           MOVE DS-NO-REPO(1 1 3) TO QDS-NO-REPO-1-1-3.
           MOVE DS-AMT-REPO(1 1 3) TO QDS-AMT-REPO-1-1-3.
           MOVE DS-NO-REPO(1 1 4) TO QDS-NO-REPO-1-1-4.
           MOVE DS-AMT-REPO(1 1 4) TO QDS-AMT-REPO-1-1-4.
           MOVE DS-NO-REPO(1 1 5) TO QDS-NO-REPO-1-1-5.
           MOVE DS-AMT-REPO(1 1 5) TO QDS-AMT-REPO-1-1-5.
           MOVE DS-NO-REPO(1 2 1) TO QDS-NO-REPO-1-2-1.
           MOVE DS-AMT-REPO(1 2 1) TO QDS-AMT-REPO-1-2-1.
           MOVE DS-NO-REPO(1 2 2) TO QDS-NO-REPO-1-2-2.
           MOVE DS-AMT-REPO(1 2 2) TO QDS-AMT-REPO-1-2-2.
           MOVE DS-NO-REPO(1 2 3) TO QDS-NO-REPO-1-2-3.
           MOVE DS-AMT-REPO(1 2 3) TO QDS-AMT-REPO-1-2-3.
           MOVE DS-NO-REPO(1 2 4) TO QDS-NO-REPO-1-2-4.
           MOVE DS-AMT-REPO(1 2 4) TO QDS-AMT-REPO-1-2-4.
           MOVE DS-NO-REPO(1 2 5) TO QDS-NO-REPO-1-2-5.
           MOVE DS-AMT-REPO(1 2 5) TO QDS-AMT-REPO-1-2-5.
           MOVE DS-NO-REPO(2 1 1) TO QDS-NO-REPO-2-1-1.
           MOVE DS-AMT-REPO(2 1 1) TO QDS-AMT-REPO-2-1-1.
           MOVE DS-NO-REPO(2 1 2) TO QDS-NO-REPO-2-1-2.
           MOVE DS-AMT-REPO(2 1 2) TO QDS-AMT-REPO-2-1-2.
           MOVE DS-NO-REPO(2 1 3) TO QDS-NO-REPO-2-1-3.
           MOVE DS-AMT-REPO(2 1 3) TO QDS-AMT-REPO-2-1-3.
           MOVE DS-NO-REPO(2 1 4) TO QDS-NO-REPO-2-1-4.
           MOVE DS-AMT-REPO(2 1 4) TO QDS-AMT-REPO-2-1-4.
           MOVE DS-NO-REPO(2 1 5) TO QDS-NO-REPO-2-1-5.
           MOVE DS-AMT-REPO(2 1 5) TO QDS-AMT-REPO-2-1-5.
           MOVE DS-NO-REPO(2 2 1) TO QDS-NO-REPO-2-2-1.
           MOVE DS-AMT-REPO(2 2 1) TO QDS-AMT-REPO-2-2-1.
           MOVE DS-NO-REPO(2 2 2) TO QDS-NO-REPO-2-2-2.
           MOVE DS-AMT-REPO(2 2 2) TO QDS-AMT-REPO-2-2-2.
           MOVE DS-NO-REPO(2 2 3) TO QDS-NO-REPO-2-2-3.
           MOVE DS-AMT-REPO(2 2 3) TO QDS-AMT-REPO-2-2-3.
           MOVE DS-NO-REPO(2 2 4) TO QDS-NO-REPO-2-2-4.
           MOVE DS-AMT-REPO(2 2 4) TO QDS-AMT-REPO-2-2-4.
           MOVE DS-NO-REPO(2 2 5) TO QDS-NO-REPO-2-2-5.
           MOVE DS-AMT-REPO(2 2 5) TO QDS-AMT-REPO-2-2-5.
           MOVE DS-NO-REPO(3 1 1) TO QDS-NO-REPO-3-1-1.
           MOVE DS-AMT-REPO(3 1 1) TO QDS-AMT-REPO-3-1-1.
           MOVE DS-NO-REPO(3 1 2) TO QDS-NO-REPO-3-1-2.
           MOVE DS-AMT-REPO(3 1 2) TO QDS-AMT-REPO-3-1-2.
           MOVE DS-NO-REPO(3 1 3) TO QDS-NO-REPO-3-1-3.
           MOVE DS-AMT-REPO(3 1 3) TO QDS-AMT-REPO-3-1-3.
           MOVE DS-NO-REPO(3 1 4) TO QDS-NO-REPO-3-1-4.
           MOVE DS-AMT-REPO(3 1 4) TO QDS-AMT-REPO-3-1-4.
           MOVE DS-NO-REPO(3 1 5) TO QDS-NO-REPO-3-1-5.
           MOVE DS-AMT-REPO(3 1 5) TO QDS-AMT-REPO-3-1-5.
           MOVE DS-NO-REPO(3 2 1) TO QDS-NO-REPO-3-2-1.
           MOVE DS-AMT-REPO(3 2 1) TO QDS-AMT-REPO-3-2-1.
           MOVE DS-NO-REPO(3 2 2) TO QDS-NO-REPO-3-2-2.
           MOVE DS-AMT-REPO(3 2 2) TO QDS-AMT-REPO-3-2-2.
           MOVE DS-NO-REPO(3 2 3) TO QDS-NO-REPO-3-2-3.
           MOVE DS-AMT-REPO(3 2 3) TO QDS-AMT-REPO-3-2-3.
           MOVE DS-NO-REPO(3 2 4) TO QDS-NO-REPO-3-2-4.
           MOVE DS-AMT-REPO(3 2 4) TO QDS-AMT-REPO-3-2-4.
           MOVE DS-NO-REPO(3 2 5) TO QDS-NO-REPO-3-2-5.
           MOVE DS-AMT-REPO(3 2 5) TO QDS-AMT-REPO-3-2-5.
           MOVE DS-UNITS(1 1 1) TO QDS-UNITS-1-1-1.
           MOVE DS-OUTBAL(1 1 1) TO QDS-OUTBAL-1-1-1.
           MOVE DS-UNITS(1 1 2) TO QDS-UNITS-1-1-2.
           MOVE DS-OUTBAL(1 1 2) TO QDS-OUTBAL-1-1-2.
           MOVE DS-UNITS(1 1 3) TO QDS-UNITS-1-1-3.
           MOVE DS-OUTBAL(1 1 3) TO QDS-OUTBAL-1-1-3.
           MOVE DS-UNITS(1 1 4) TO QDS-UNITS-1-1-4.
           MOVE DS-OUTBAL(1 1 4) TO QDS-OUTBAL-1-1-4.
           MOVE DS-UNITS(1 1 5) TO QDS-UNITS-1-1-5.
           MOVE DS-OUTBAL(1 1 5) TO QDS-OUTBAL-1-1-5.
           MOVE DS-UNITS(1 1 6) TO QDS-UNITS-1-1-6.
           MOVE DS-OUTBAL(1 1 6) TO QDS-OUTBAL-1-1-6.
           MOVE DS-UNITS(1 1 7) TO QDS-UNITS-1-1-7.
           MOVE DS-OUTBAL(1 1 7) TO QDS-OUTBAL-1-1-7.
           MOVE DS-UNITS(1 1 8) TO QDS-UNITS-1-1-8.
           MOVE DS-OUTBAL(1 1 8) TO QDS-OUTBAL-1-1-8.
           MOVE DS-UNITS(1 1 9) TO QDS-UNITS-1-1-9.
           MOVE DS-OUTBAL(1 1 9) TO QDS-OUTBAL-1-1-9.
           MOVE DS-UNITS(1 1 10) TO QDS-UNITS-1-1-10.
           MOVE DS-OUTBAL(1 1 10) TO QDS-OUTBAL-1-1-10.
           MOVE DS-UNITS(1 2 1) TO QDS-UNITS-1-2-1.
           MOVE DS-OUTBAL(1 2 1) TO QDS-OUTBAL-1-2-1.
           MOVE DS-UNITS(1 2 2) TO QDS-UNITS-1-2-2.
           MOVE DS-OUTBAL(1 2 2) TO QDS-OUTBAL-1-2-2.
           MOVE DS-UNITS(1 2 3) TO QDS-UNITS-1-2-3.
           MOVE DS-OUTBAL(1 2 3) TO QDS-OUTBAL-1-2-3.
           MOVE DS-UNITS(1 2 4) TO QDS-UNITS-1-2-4.
           MOVE DS-OUTBAL(1 2 4) TO QDS-OUTBAL-1-2-4.
           MOVE DS-UNITS(1 2 5) TO QDS-UNITS-1-2-5.
           MOVE DS-OUTBAL(1 2 5) TO QDS-OUTBAL-1-2-5.
           MOVE DS-UNITS(1 2 6) TO QDS-UNITS-1-2-6.
           MOVE DS-OUTBAL(1 2 6) TO QDS-OUTBAL-1-2-6.
           MOVE DS-UNITS(1 2 7) TO QDS-UNITS-1-2-7.
           MOVE DS-OUTBAL(1 2 7) TO QDS-OUTBAL-1-2-7.
           MOVE DS-UNITS(1 2 8) TO QDS-UNITS-1-2-8.
           MOVE DS-OUTBAL(1 2 8) TO QDS-OUTBAL-1-2-8.
           MOVE DS-UNITS(1 2 9) TO QDS-UNITS-1-2-9.
           MOVE DS-OUTBAL(1 2 9) TO QDS-OUTBAL-1-2-9.
           MOVE DS-UNITS(1 2 10) TO QDS-UNITS-1-2-10.
           MOVE DS-OUTBAL(1 2 10) TO QDS-OUTBAL-1-2-10.
           MOVE DS-UNITS(2 1 1) TO QDS-UNITS-2-1-1.
           MOVE DS-OUTBAL(2 1 1) TO QDS-OUTBAL-2-1-1.
           MOVE DS-UNITS(2 1 2) TO QDS-UNITS-2-1-2.
           MOVE DS-OUTBAL(2 1 2) TO QDS-OUTBAL-2-1-2.
           MOVE DS-UNITS(2 1 3) TO QDS-UNITS-2-1-3.
           MOVE DS-OUTBAL(2 1 3) TO QDS-OUTBAL-2-1-3.
           MOVE DS-UNITS(2 1 4) TO QDS-UNITS-2-1-4.
           MOVE DS-OUTBAL(2 1 4) TO QDS-OUTBAL-2-1-4.
           MOVE DS-UNITS(2 1 5) TO QDS-UNITS-2-1-5.
           MOVE DS-OUTBAL(2 1 5) TO QDS-OUTBAL-2-1-5.
           MOVE DS-UNITS(2 1 6) TO QDS-UNITS-2-1-6.
           MOVE DS-OUTBAL(2 1 6) TO QDS-OUTBAL-2-1-6.
           MOVE DS-UNITS(2 1 7) TO QDS-UNITS-2-1-7.
           MOVE DS-OUTBAL(2 1 7) TO QDS-OUTBAL-2-1-7.
           MOVE DS-UNITS(2 1 8) TO QDS-UNITS-2-1-8.
           MOVE DS-OUTBAL(2 1 8) TO QDS-OUTBAL-2-1-8.
           MOVE DS-UNITS(2 1 9) TO QDS-UNITS-2-1-9.
           MOVE DS-OUTBAL(2 1 9) TO QDS-OUTBAL-2-1-9.
           MOVE DS-UNITS(2 1 10) TO QDS-UNITS-2-1-10.
           MOVE DS-OUTBAL(2 1 10) TO QDS-OUTBAL-2-1-10.
           MOVE DS-UNITS(2 2 1) TO QDS-UNITS-2-2-1.
           MOVE DS-OUTBAL(2 2 1) TO QDS-OUTBAL-2-2-1.
           MOVE DS-UNITS(2 2 2) TO QDS-UNITS-2-2-2.
           MOVE DS-OUTBAL(2 2 2) TO QDS-OUTBAL-2-2-2.
           MOVE DS-UNITS(2 2 3) TO QDS-UNITS-2-2-3.
           MOVE DS-OUTBAL(2 2 3) TO QDS-OUTBAL-2-2-3.
           MOVE DS-UNITS(2 2 4) TO QDS-UNITS-2-2-4.
           MOVE DS-OUTBAL(2 2 4) TO QDS-OUTBAL-2-2-4.
           MOVE DS-UNITS(2 2 5) TO QDS-UNITS-2-2-5.
           MOVE DS-OUTBAL(2 2 5) TO QDS-OUTBAL-2-2-5.
           MOVE DS-UNITS(2 2 6) TO QDS-UNITS-2-2-6.
           MOVE DS-OUTBAL(2 2 6) TO QDS-OUTBAL-2-2-6.
           MOVE DS-UNITS(2 2 7) TO QDS-UNITS-2-2-7.
           MOVE DS-OUTBAL(2 2 7) TO QDS-OUTBAL-2-2-7.
           MOVE DS-UNITS(2 2 8) TO QDS-UNITS-2-2-8.
           MOVE DS-OUTBAL(2 2 8) TO QDS-OUTBAL-2-2-8.
           MOVE DS-UNITS(2 2 9) TO QDS-UNITS-2-2-9.
           MOVE DS-OUTBAL(2 2 9) TO QDS-OUTBAL-2-2-9.
           MOVE DS-UNITS(2 2 10) TO QDS-UNITS-2-2-10.
           MOVE DS-OUTBAL(2 2 10) TO QDS-OUTBAL-2-2-10.
           MOVE DS-UNITS(3 1 1) TO QDS-UNITS-3-1-1.
           MOVE DS-OUTBAL(3 1 1) TO QDS-OUTBAL-3-1-1.
           MOVE DS-UNITS(3 1 2) TO QDS-UNITS-3-1-2.
           MOVE DS-OUTBAL(3 1 2) TO QDS-OUTBAL-3-1-2.
           MOVE DS-UNITS(3 1 3) TO QDS-UNITS-3-1-3.
           MOVE DS-OUTBAL(3 1 3) TO QDS-OUTBAL-3-1-3.
           MOVE DS-UNITS(3 1 4) TO QDS-UNITS-3-1-4.
           MOVE DS-OUTBAL(3 1 4) TO QDS-OUTBAL-3-1-4.
           MOVE DS-UNITS(3 1 5) TO QDS-UNITS-3-1-5.
           MOVE DS-OUTBAL(3 1 5) TO QDS-OUTBAL-3-1-5.
           MOVE DS-UNITS(3 1 6) TO QDS-UNITS-3-1-6.
           MOVE DS-OUTBAL(3 1 6) TO QDS-OUTBAL-3-1-6.
           MOVE DS-UNITS(3 1 7) TO QDS-UNITS-3-1-7.
           MOVE DS-OUTBAL(3 1 7) TO QDS-OUTBAL-3-1-7.
           MOVE DS-UNITS(3 1 8) TO QDS-UNITS-3-1-8.
           MOVE DS-OUTBAL(3 1 8) TO QDS-OUTBAL-3-1-8.
           MOVE DS-UNITS(3 1 9) TO QDS-UNITS-3-1-9.
           MOVE DS-OUTBAL(3 1 9) TO QDS-OUTBAL-3-1-9.
           MOVE DS-UNITS(3 1 10) TO QDS-UNITS-3-1-10.
           MOVE DS-OUTBAL(3 1 10) TO QDS-OUTBAL-3-1-10.
           MOVE DS-UNITS(3 2 1) TO QDS-UNITS-3-2-1.
           MOVE DS-OUTBAL(3 2 1) TO QDS-OUTBAL-3-2-1.
           MOVE DS-UNITS(3 2 2) TO QDS-UNITS-3-2-2.
           MOVE DS-OUTBAL(3 2 2) TO QDS-OUTBAL-3-2-2.
           MOVE DS-UNITS(3 2 3) TO QDS-UNITS-3-2-3.
           MOVE DS-OUTBAL(3 2 3) TO QDS-OUTBAL-3-2-3.
           MOVE DS-UNITS(3 2 4) TO QDS-UNITS-3-2-4.
           MOVE DS-OUTBAL(3 2 4) TO QDS-OUTBAL-3-2-4.
           MOVE DS-UNITS(3 2 5) TO QDS-UNITS-3-2-5.
           MOVE DS-OUTBAL(3 2 5) TO QDS-OUTBAL-3-2-5.
           MOVE DS-UNITS(3 2 6) TO QDS-UNITS-3-2-6.
           MOVE DS-OUTBAL(3 2 6) TO QDS-OUTBAL-3-2-6.
           MOVE DS-UNITS(3 2 7) TO QDS-UNITS-3-2-7.
           MOVE DS-OUTBAL(3 2 7) TO QDS-OUTBAL-3-2-7.
           MOVE DS-UNITS(3 2 8) TO QDS-UNITS-3-2-8.
           MOVE DS-OUTBAL(3 2 8) TO QDS-OUTBAL-3-2-8.
           MOVE DS-UNITS(3 2 9) TO QDS-UNITS-3-2-9.
           MOVE DS-OUTBAL(3 2 9) TO QDS-OUTBAL-3-2-9.
           MOVE DS-UNITS(3 2 10) TO QDS-UNITS-3-2-10.
           MOVE DS-OUTBAL(3 2 10) TO QDS-OUTBAL-3-2-10.
           MOVE DS-UNITS-BNKRPT(1 1 1) TO QDS-UNITS-BNKRPT-1-1-1.
           MOVE DS-OUTBAL-BNKRPT(1 1 1) TO QDS-OUTBAL-BNKRPT-1-1-1.
           MOVE DS-UNITS-BNKRPT(1 1 2) TO QDS-UNITS-BNKRPT-1-1-2.
           MOVE DS-OUTBAL-BNKRPT(1 1 2) TO QDS-OUTBAL-BNKRPT-1-1-2.
           MOVE DS-UNITS-BNKRPT(1 1 3) TO QDS-UNITS-BNKRPT-1-1-3.
           MOVE DS-OUTBAL-BNKRPT(1 1 3) TO QDS-OUTBAL-BNKRPT-1-1-3.
           MOVE DS-UNITS-BNKRPT(1 1 4) TO QDS-UNITS-BNKRPT-1-1-4.
           MOVE DS-OUTBAL-BNKRPT(1 1 4) TO QDS-OUTBAL-BNKRPT-1-1-4.
           MOVE DS-UNITS-BNKRPT(1 1 5) TO QDS-UNITS-BNKRPT-1-1-5.
           MOVE DS-OUTBAL-BNKRPT(1 1 5) TO QDS-OUTBAL-BNKRPT-1-1-5.
           MOVE DS-UNITS-BNKRPT(1 1 6) TO QDS-UNITS-BNKRPT-1-1-6.
           MOVE DS-OUTBAL-BNKRPT(1 1 6) TO QDS-OUTBAL-BNKRPT-1-1-6.
           MOVE DS-UNITS-BNKRPT(1 1 7) TO QDS-UNITS-BNKRPT-1-1-7.
           MOVE DS-OUTBAL-BNKRPT(1 1 7) TO QDS-OUTBAL-BNKRPT-1-1-7.
           MOVE DS-UNITS-BNKRPT(1 1 8) TO QDS-UNITS-BNKRPT-1-1-8.
           MOVE DS-OUTBAL-BNKRPT(1 1 8) TO QDS-OUTBAL-BNKRPT-1-1-8.
           MOVE DS-UNITS-BNKRPT(1 1 9) TO QDS-UNITS-BNKRPT-1-1-9.
           MOVE DS-OUTBAL-BNKRPT(1 1 9) TO QDS-OUTBAL-BNKRPT-1-1-9.
           MOVE DS-UNITS-BNKRPT(1 1 10) TO QDS-UNITS-BNKRPT-1-1-10.
           MOVE DS-OUTBAL-BNKRPT(1 1 10) TO QDS-OUTBAL-BNKRPT-1-1-10.
           MOVE DS-UNITS-BNKRPT(1 2 1) TO QDS-UNITS-BNKRPT-1-2-1.
           MOVE DS-OUTBAL-BNKRPT(1 2 1) TO QDS-OUTBAL-BNKRPT-1-2-1.
           MOVE DS-UNITS-BNKRPT(1 2 2) TO QDS-UNITS-BNKRPT-1-2-2.
           MOVE DS-OUTBAL-BNKRPT(1 2 2) TO QDS-OUTBAL-BNKRPT-1-2-2.
           MOVE DS-UNITS-BNKRPT(1 2 3) TO QDS-UNITS-BNKRPT-1-2-3.
           MOVE DS-OUTBAL-BNKRPT(1 2 3) TO QDS-OUTBAL-BNKRPT-1-2-3.
           MOVE DS-UNITS-BNKRPT(1 2 4) TO QDS-UNITS-BNKRPT-1-2-4.
           MOVE DS-OUTBAL-BNKRPT(1 2 4) TO QDS-OUTBAL-BNKRPT-1-2-4.
           MOVE DS-UNITS-BNKRPT(1 2 5) TO QDS-UNITS-BNKRPT-1-2-5.
           MOVE DS-OUTBAL-BNKRPT(1 2 5) TO QDS-OUTBAL-BNKRPT-1-2-5.
           MOVE DS-UNITS-BNKRPT(1 2 6) TO QDS-UNITS-BNKRPT-1-2-6.
           MOVE DS-OUTBAL-BNKRPT(1 2 6) TO QDS-OUTBAL-BNKRPT-1-2-6.
           MOVE DS-UNITS-BNKRPT(1 2 7) TO QDS-UNITS-BNKRPT-1-2-7.
           MOVE DS-OUTBAL-BNKRPT(1 2 7) TO QDS-OUTBAL-BNKRPT-1-2-7.
           MOVE DS-UNITS-BNKRPT(1 2 8) TO QDS-UNITS-BNKRPT-1-2-8.
           MOVE DS-OUTBAL-BNKRPT(1 2 8) TO QDS-OUTBAL-BNKRPT-1-2-8.
           MOVE DS-UNITS-BNKRPT(1 2 9) TO QDS-UNITS-BNKRPT-1-2-9.
           MOVE DS-OUTBAL-BNKRPT(1 2 9) TO QDS-OUTBAL-BNKRPT-1-2-9.
           MOVE DS-UNITS-BNKRPT(1 2 10) TO QDS-UNITS-BNKRPT-1-2-10.
           MOVE DS-OUTBAL-BNKRPT(1 2 10) TO QDS-OUTBAL-BNKRPT-1-2-10.
           MOVE DS-UNITS-BNKRPT(2 1 1) TO QDS-UNITS-BNKRPT-2-1-1.
           MOVE DS-OUTBAL-BNKRPT(2 1 1) TO QDS-OUTBAL-BNKRPT-2-1-1.
           MOVE DS-UNITS-BNKRPT(2 1 2) TO QDS-UNITS-BNKRPT-2-1-2.
           MOVE DS-OUTBAL-BNKRPT(2 1 2) TO QDS-OUTBAL-BNKRPT-2-1-2.
           MOVE DS-UNITS-BNKRPT(2 1 3) TO QDS-UNITS-BNKRPT-2-1-3.
           MOVE DS-OUTBAL-BNKRPT(2 1 3) TO QDS-OUTBAL-BNKRPT-2-1-3.
           MOVE DS-UNITS-BNKRPT(2 1 4) TO QDS-UNITS-BNKRPT-2-1-4.
           MOVE DS-OUTBAL-BNKRPT(2 1 4) TO QDS-OUTBAL-BNKRPT-2-1-4.
           MOVE DS-UNITS-BNKRPT(2 1 5) TO QDS-UNITS-BNKRPT-2-1-5.
           MOVE DS-OUTBAL-BNKRPT(2 1 5) TO QDS-OUTBAL-BNKRPT-2-1-5.
           MOVE DS-UNITS-BNKRPT(2 1 6) TO QDS-UNITS-BNKRPT-2-1-6.
           MOVE DS-OUTBAL-BNKRPT(2 1 6) TO QDS-OUTBAL-BNKRPT-2-1-6.
           MOVE DS-UNITS-BNKRPT(2 1 7) TO QDS-UNITS-BNKRPT-2-1-7.
           MOVE DS-OUTBAL-BNKRPT(2 1 7) TO QDS-OUTBAL-BNKRPT-2-1-7.
           MOVE DS-UNITS-BNKRPT(2 1 8) TO QDS-UNITS-BNKRPT-2-1-8.
           MOVE DS-OUTBAL-BNKRPT(2 1 8) TO QDS-OUTBAL-BNKRPT-2-1-8.
           MOVE DS-UNITS-BNKRPT(2 1 9) TO QDS-UNITS-BNKRPT-2-1-9.
           MOVE DS-OUTBAL-BNKRPT(2 1 9) TO QDS-OUTBAL-BNKRPT-2-1-9.
           MOVE DS-UNITS-BNKRPT(2 1 10) TO QDS-UNITS-BNKRPT-2-1-10.
           MOVE DS-OUTBAL-BNKRPT(2 1 10) TO QDS-OUTBAL-BNKRPT-2-1-10.
           MOVE DS-UNITS-BNKRPT(2 2 1) TO QDS-UNITS-BNKRPT-2-2-1.
           MOVE DS-OUTBAL-BNKRPT(2 2 1) TO QDS-OUTBAL-BNKRPT-2-2-1.
           MOVE DS-UNITS-BNKRPT(2 2 2) TO QDS-UNITS-BNKRPT-2-2-2.
           MOVE DS-OUTBAL-BNKRPT(2 2 2) TO QDS-OUTBAL-BNKRPT-2-2-2.
           MOVE DS-UNITS-BNKRPT(2 2 3) TO QDS-UNITS-BNKRPT-2-2-3.
           MOVE DS-OUTBAL-BNKRPT(2 2 3) TO QDS-OUTBAL-BNKRPT-2-2-3.
           MOVE DS-UNITS-BNKRPT(2 2 4) TO QDS-UNITS-BNKRPT-2-2-4.
           MOVE DS-OUTBAL-BNKRPT(2 2 4) TO QDS-OUTBAL-BNKRPT-2-2-4.
           MOVE DS-UNITS-BNKRPT(2 2 5) TO QDS-UNITS-BNKRPT-2-2-5.
           MOVE DS-OUTBAL-BNKRPT(2 2 5) TO QDS-OUTBAL-BNKRPT-2-2-5.
           MOVE DS-UNITS-BNKRPT(2 2 6) TO QDS-UNITS-BNKRPT-2-2-6.
           MOVE DS-OUTBAL-BNKRPT(2 2 6) TO QDS-OUTBAL-BNKRPT-2-2-6.
           MOVE DS-UNITS-BNKRPT(2 2 7) TO QDS-UNITS-BNKRPT-2-2-7.
           MOVE DS-OUTBAL-BNKRPT(2 2 7) TO QDS-OUTBAL-BNKRPT-2-2-7.
           MOVE DS-UNITS-BNKRPT(2 2 8) TO QDS-UNITS-BNKRPT-2-2-8.
           MOVE DS-OUTBAL-BNKRPT(2 2 8) TO QDS-OUTBAL-BNKRPT-2-2-8.
           MOVE DS-UNITS-BNKRPT(2 2 9) TO QDS-UNITS-BNKRPT-2-2-9.
           MOVE DS-OUTBAL-BNKRPT(2 2 9) TO QDS-OUTBAL-BNKRPT-2-2-9.
           MOVE DS-UNITS-BNKRPT(2 2 10) TO QDS-UNITS-BNKRPT-2-2-10.
           MOVE DS-OUTBAL-BNKRPT(2 2 10) TO QDS-OUTBAL-BNKRPT-2-2-10.
           MOVE DS-UNITS-BNKRPT(3 1 1) TO QDS-UNITS-BNKRPT-3-1-1.
           MOVE DS-OUTBAL-BNKRPT(3 1 1) TO QDS-OUTBAL-BNKRPT-3-1-1.
           MOVE DS-UNITS-BNKRPT(3 1 2) TO QDS-UNITS-BNKRPT-3-1-2.
           MOVE DS-OUTBAL-BNKRPT(3 1 2) TO QDS-OUTBAL-BNKRPT-3-1-2.
           MOVE DS-UNITS-BNKRPT(3 1 3) TO QDS-UNITS-BNKRPT-3-1-3.
           MOVE DS-OUTBAL-BNKRPT(3 1 3) TO QDS-OUTBAL-BNKRPT-3-1-3.
           MOVE DS-UNITS-BNKRPT(3 1 4) TO QDS-UNITS-BNKRPT-3-1-4.
           MOVE DS-OUTBAL-BNKRPT(3 1 4) TO QDS-OUTBAL-BNKRPT-3-1-4.
           MOVE DS-UNITS-BNKRPT(3 1 5) TO QDS-UNITS-BNKRPT-3-1-5.
           MOVE DS-OUTBAL-BNKRPT(3 1 5) TO QDS-OUTBAL-BNKRPT-3-1-5.
           MOVE DS-UNITS-BNKRPT(3 1 6) TO QDS-UNITS-BNKRPT-3-1-6.
           MOVE DS-OUTBAL-BNKRPT(3 1 6) TO QDS-OUTBAL-BNKRPT-3-1-6.
           MOVE DS-UNITS-BNKRPT(3 1 7) TO QDS-UNITS-BNKRPT-3-1-7.
           MOVE DS-OUTBAL-BNKRPT(3 1 7) TO QDS-OUTBAL-BNKRPT-3-1-7.
           MOVE DS-UNITS-BNKRPT(3 1 8) TO QDS-UNITS-BNKRPT-3-1-8.
           MOVE DS-OUTBAL-BNKRPT(3 1 8) TO QDS-OUTBAL-BNKRPT-3-1-8.
           MOVE DS-UNITS-BNKRPT(3 1 9) TO QDS-UNITS-BNKRPT-3-1-9.
           MOVE DS-OUTBAL-BNKRPT(3 1 9) TO QDS-OUTBAL-BNKRPT-3-1-9.
           MOVE DS-UNITS-BNKRPT(3 1 10) TO QDS-UNITS-BNKRPT-3-1-10.
           MOVE DS-OUTBAL-BNKRPT(3 1 10) TO QDS-OUTBAL-BNKRPT-3-1-10.
           MOVE DS-UNITS-BNKRPT(3 2 1) TO QDS-UNITS-BNKRPT-3-2-1.
           MOVE DS-OUTBAL-BNKRPT(3 2 1) TO QDS-OUTBAL-BNKRPT-3-2-1.
           MOVE DS-UNITS-BNKRPT(3 2 2) TO QDS-UNITS-BNKRPT-3-2-2.
           MOVE DS-OUTBAL-BNKRPT(3 2 2) TO QDS-OUTBAL-BNKRPT-3-2-2.
           MOVE DS-UNITS-BNKRPT(3 2 3) TO QDS-UNITS-BNKRPT-3-2-3.
           MOVE DS-OUTBAL-BNKRPT(3 2 3) TO QDS-OUTBAL-BNKRPT-3-2-3.
           MOVE DS-UNITS-BNKRPT(3 2 4) TO QDS-UNITS-BNKRPT-3-2-4.
           MOVE DS-OUTBAL-BNKRPT(3 2 4) TO QDS-OUTBAL-BNKRPT-3-2-4.
           MOVE DS-UNITS-BNKRPT(3 2 5) TO QDS-UNITS-BNKRPT-3-2-5.
           MOVE DS-OUTBAL-BNKRPT(3 2 5) TO QDS-OUTBAL-BNKRPT-3-2-5.
           MOVE DS-UNITS-BNKRPT(3 2 6) TO QDS-UNITS-BNKRPT-3-2-6.
           MOVE DS-OUTBAL-BNKRPT(3 2 6) TO QDS-OUTBAL-BNKRPT-3-2-6.
           MOVE DS-UNITS-BNKRPT(3 2 7) TO QDS-UNITS-BNKRPT-3-2-7.
           MOVE DS-OUTBAL-BNKRPT(3 2 7) TO QDS-OUTBAL-BNKRPT-3-2-7.
           MOVE DS-UNITS-BNKRPT(3 2 8) TO QDS-UNITS-BNKRPT-3-2-8.
           MOVE DS-OUTBAL-BNKRPT(3 2 8) TO QDS-OUTBAL-BNKRPT-3-2-8.
           MOVE DS-UNITS-BNKRPT(3 2 9) TO QDS-UNITS-BNKRPT-3-2-9.
           MOVE DS-OUTBAL-BNKRPT(3 2 9) TO QDS-OUTBAL-BNKRPT-3-2-9.
           MOVE DS-UNITS-BNKRPT(3 2 10) TO QDS-UNITS-BNKRPT-3-2-10.
           MOVE DS-OUTBAL-BNKRPT(3 2 10) TO QDS-OUTBAL-BNKRPT-3-2-10.
           MOVE DS-UNEARNED-AMT(1 1 1) TO QDS-UNEARNED-AMT-1-1-1.
           MOVE DS-UNEARNED-AMT(1 1 2) TO QDS-UNEARNED-AMT-1-1-2.
           MOVE DS-UNEARNED-AMT(1 1 3) TO QDS-UNEARNED-AMT-1-1-3.
           MOVE DS-UNEARNED-AMT(1 1 4) TO QDS-UNEARNED-AMT-1-1-4.
           MOVE DS-UNEARNED-AMT(1 1 5) TO QDS-UNEARNED-AMT-1-1-5.
           MOVE DS-UNEARNED-AMT(1 1 6) TO QDS-UNEARNED-AMT-1-1-6.
           MOVE DS-UNEARNED-AMT(1 1 7) TO QDS-UNEARNED-AMT-1-1-7.
           MOVE DS-UNEARNED-AMT(1 1 8) TO QDS-UNEARNED-AMT-1-1-8.
           MOVE DS-UNEARNED-AMT(1 1 9) TO QDS-UNEARNED-AMT-1-1-9.
           MOVE DS-UNEARNED-AMT(1 1 10) TO QDS-UNEARNED-AMT-1-1-10.
           MOVE DS-UNEARNED-AMT(1 2 1) TO QDS-UNEARNED-AMT-1-2-1.
           MOVE DS-UNEARNED-AMT(1 2 2) TO QDS-UNEARNED-AMT-1-2-2.
           MOVE DS-UNEARNED-AMT(1 2 3) TO QDS-UNEARNED-AMT-1-2-3.
           MOVE DS-UNEARNED-AMT(1 2 4) TO QDS-UNEARNED-AMT-1-2-4.
           MOVE DS-UNEARNED-AMT(1 2 5) TO QDS-UNEARNED-AMT-1-2-5.
           MOVE DS-UNEARNED-AMT(1 2 6) TO QDS-UNEARNED-AMT-1-2-6.
           MOVE DS-UNEARNED-AMT(1 2 7) TO QDS-UNEARNED-AMT-1-2-7.
           MOVE DS-UNEARNED-AMT(1 2 8) TO QDS-UNEARNED-AMT-1-2-8.
           MOVE DS-UNEARNED-AMT(1 2 9) TO QDS-UNEARNED-AMT-1-2-9.
           MOVE DS-UNEARNED-AMT(1 2 10) TO QDS-UNEARNED-AMT-1-2-10.
           MOVE DS-UNEARNED-AMT(2 1 1) TO QDS-UNEARNED-AMT-2-1-1.
           MOVE DS-UNEARNED-AMT(2 1 2) TO QDS-UNEARNED-AMT-2-1-2.
           MOVE DS-UNEARNED-AMT(2 1 3) TO QDS-UNEARNED-AMT-2-1-3.
           MOVE DS-UNEARNED-AMT(2 1 4) TO QDS-UNEARNED-AMT-2-1-4.
           MOVE DS-UNEARNED-AMT(2 1 5) TO QDS-UNEARNED-AMT-2-1-5.
           MOVE DS-UNEARNED-AMT(2 1 6) TO QDS-UNEARNED-AMT-2-1-6.
           MOVE DS-UNEARNED-AMT(2 1 7) TO QDS-UNEARNED-AMT-2-1-7.
           MOVE DS-UNEARNED-AMT(2 1 8) TO QDS-UNEARNED-AMT-2-1-8.
           MOVE DS-UNEARNED-AMT(2 1 9) TO QDS-UNEARNED-AMT-2-1-9.
           MOVE DS-UNEARNED-AMT(2 1 10) TO QDS-UNEARNED-AMT-2-1-10.
           MOVE DS-UNEARNED-AMT(2 2 1) TO QDS-UNEARNED-AMT-2-2-1.
           MOVE DS-UNEARNED-AMT(2 2 2) TO QDS-UNEARNED-AMT-2-2-2.
           MOVE DS-UNEARNED-AMT(2 2 3) TO QDS-UNEARNED-AMT-2-2-3.
           MOVE DS-UNEARNED-AMT(2 2 4) TO QDS-UNEARNED-AMT-2-2-4.
           MOVE DS-UNEARNED-AMT(2 2 5) TO QDS-UNEARNED-AMT-2-2-5.
           MOVE DS-UNEARNED-AMT(2 2 6) TO QDS-UNEARNED-AMT-2-2-6.
           MOVE DS-UNEARNED-AMT(2 2 7) TO QDS-UNEARNED-AMT-2-2-7.
           MOVE DS-UNEARNED-AMT(2 2 8) TO QDS-UNEARNED-AMT-2-2-8.
           MOVE DS-UNEARNED-AMT(2 2 9) TO QDS-UNEARNED-AMT-2-2-9.
           MOVE DS-UNEARNED-AMT(2 2 10) TO QDS-UNEARNED-AMT-2-2-10.
           MOVE DS-UNEARNED-AMT(3 1 1) TO QDS-UNEARNED-AMT-3-1-1.
           MOVE DS-UNEARNED-AMT(3 1 2) TO QDS-UNEARNED-AMT-3-1-2.
           MOVE DS-UNEARNED-AMT(3 1 3) TO QDS-UNEARNED-AMT-3-1-3.
           MOVE DS-UNEARNED-AMT(3 1 4) TO QDS-UNEARNED-AMT-3-1-4.
           MOVE DS-UNEARNED-AMT(3 1 5) TO QDS-UNEARNED-AMT-3-1-5.
           MOVE DS-UNEARNED-AMT(3 1 6) TO QDS-UNEARNED-AMT-3-1-6.
           MOVE DS-UNEARNED-AMT(3 1 7) TO QDS-UNEARNED-AMT-3-1-7.
           MOVE DS-UNEARNED-AMT(3 1 8) TO QDS-UNEARNED-AMT-3-1-8.
           MOVE DS-UNEARNED-AMT(3 1 9) TO QDS-UNEARNED-AMT-3-1-9.
           MOVE DS-UNEARNED-AMT(3 1 10) TO QDS-UNEARNED-AMT-3-1-10.
           MOVE DS-UNEARNED-AMT(3 2 1) TO QDS-UNEARNED-AMT-3-2-1.
           MOVE DS-UNEARNED-AMT(3 2 2) TO QDS-UNEARNED-AMT-3-2-2.
           MOVE DS-UNEARNED-AMT(3 2 3) TO QDS-UNEARNED-AMT-3-2-3.
           MOVE DS-UNEARNED-AMT(3 2 4) TO QDS-UNEARNED-AMT-3-2-4.
           MOVE DS-UNEARNED-AMT(3 2 5) TO QDS-UNEARNED-AMT-3-2-5.
           MOVE DS-UNEARNED-AMT(3 2 6) TO QDS-UNEARNED-AMT-3-2-6.
           MOVE DS-UNEARNED-AMT(3 2 7) TO QDS-UNEARNED-AMT-3-2-7.
           MOVE DS-UNEARNED-AMT(3 2 8) TO QDS-UNEARNED-AMT-3-2-8.
           MOVE DS-UNEARNED-AMT(3 2 9) TO QDS-UNEARNED-AMT-3-2-9.
           MOVE DS-UNEARNED-AMT(3 2 10) TO QDS-UNEARNED-AMT-3-2-10.
           MOVE DS-DLDISC-VOL(1) TO QDS-DLDISC-VOL-1.
           MOVE DS-DLDISC-VOL(2) TO QDS-DLDISC-VOL-2.
           MOVE DS-DLDISC-VOL(3) TO QDS-DLDISC-VOL-3.
           MOVE DS-AFPDUEDL-MEMO TO QDS-AFPDUEDL-MEMO.
           MOVE DS-UNPAID-MANUAL TO QDS-UNPAID-MANUAL.

           IF ( DS-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QDS-LTOUCH-DATE
           ELSE
              MOVE DS-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QDS-LTOUCH-DATE.

 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-DS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( DS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( DS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO DS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO DS-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO DS-ALL-DATA
              MOVE DS-ALL-PATH             TO DS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( DS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO DS-ALL-BASE
              MOVE FILPATH-MACHINE         TO DS-ALL-MACHINE
              MOVE FILPATH-DATA            TO DS-ALL-DATA
              MOVE DS-ALL-PATH             TO DS-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-DSFILE NOT = "S" )
              MOVE DS-PATH-BEG             TO DS-SELECT-PATH-BEG
              MOVE DS-PATH-FILE            TO DS-SELECT-PATH-FILE
           ELSE
              MOVE DS-PATH-SQL             TO DS-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-DS1-FILE.
           PERFORM LOAD-DS1-FILE.
           PERFORM OPEN-IT.
           MOVE DS-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-DS1-FILE.
           PERFORM START-IT.
           MOVE DS-PATH-OWNBR  TO DS-BRANCH.
           MOVE DS-SELECT-PATH TO E-FILE.
           MOVE DS1-KEY        TO E-KEYX.

           IF ( DS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-DS1-FILE.

           IF ( QDS1-WEND-CLASS = ZEROES    ) OR
              ( QDS1-WEND-CLASS NOT NUMERIC )
              MOVE DS-PATH-OWNBR        TO QDS1-WBEG-BRANCH
              MOVE DS-PATH-OWNBR        TO QDS1-WEND-BRANCH
              MOVE DS-DATE-CCYYMM       TO QDS1-WBEG-DATE-CCYYMM
              MOVE ALL "9"              TO QDS1-WEND-DATE-CCYYMM
                                           QDS1-WEND-NUM
                                           QDS1-WEND-CLASS
              MOVE DS-NUM               TO QDS1-WBEG-NUM
              MOVE DS-CLASS             TO QDS1-WBEG-CLASS.


           EXEC SQL
            DECLARE DS1_CURSOR CURSOR FOR
            SELECT DSFILE.DS_DATE_CCYYMM,
                   DSFILE.DS_BRANCH,
                   DSFILE.DS_NUM,
                   DSFILE.DS_CLASS,
                   DSFILE.DS_DLRVPRIN_1,
                   DSFILE.DS_DLRVPRIN_2,
                   DSFILE.DS_DLRVPRIN_3,
                   DSFILE.DS_DLRVCHGS_1,
                   DSFILE.DS_DLRVCHGS_2,
                   DSFILE.DS_DLRVCHGS_3,
                   DSFILE.DS_DLPARVHELD,
                   DSFILE.DS_UNPAIDCK,
                   DSFILE.DS_DLDISC,
                   DSFILE.DS_DLPARVPAID,
                   DSFILE.DS_SUMWGTTM,
                   DSFILE.DS_SUMWGTAV,
                   DSFILE.DS_SUMWGTTA,
                   DSFILE.DS_RCOURSE_UNITS_1,
                   DSFILE.DS_RCOURSE_UNITS_2,
                   DSFILE.DS_RCOURSE_UNITS_3,
                   DSFILE.DS_RCOURSE_AMT_1,
                   DSFILE.DS_RCOURSE_AMT_2,
                   DSFILE.DS_RCOURSE_AMT_3,
                   DSFILE.DS_NPURUNIT_1,
                   DSFILE.DS_NPURUNIT_2,
                   DSFILE.DS_NPURUNIT_3,
                   DSFILE.DS_NPURAMT_1,
                   DSFILE.DS_NPURAMT_2,
                   DSFILE.DS_NPURAMT_3,
                   DSFILE.DS_OPURUNIT_1,
                   DSFILE.DS_OPURUNIT_2,
                   DSFILE.DS_OPURUNIT_3,
                   DSFILE.DS_OPURAMT_1,
                   DSFILE.DS_OPURAMT_2,
                   DSFILE.DS_OPURAMT_3,
                   DSFILE.DS_RNEWUNIT_1,
                   DSFILE.DS_RNEWUNIT_2,
                   DSFILE.DS_RNEWUNIT_3,
                   DSFILE.DS_RNEWAMT_1,
                   DSFILE.DS_RNEWAMT_2,
                   DSFILE.DS_RNEWAMT_3,
                   DSFILE.DS_PAIDUNIT_1,
                   DSFILE.DS_PAIDUNIT_2,
                   DSFILE.DS_PAIDUNIT_3,
                   DSFILE.DS_PAIDAMT_1,
                   DSFILE.DS_PAIDAMT_2,
                   DSFILE.DS_PAIDAMT_3,
                   DSFILE.DS_TODLUNIT_1,
                   DSFILE.DS_TODLUNIT_2,
                   DSFILE.DS_TODLUNIT_3,
                   DSFILE.DS_TODLAMT_1,
                   DSFILE.DS_TODLAMT_2,
                   DSFILE.DS_TODLAMT_3,
                   DSFILE.DS_OUTSUNIT_1,
                   DSFILE.DS_OUTSUNIT_2,
                   DSFILE.DS_OUTSUNIT_3,
                   DSFILE.DS_OUTSAMT_1,
                   DSFILE.DS_OUTSAMT_2,
                   DSFILE.DS_OUTSAMT_3,
                   DSFILE.DS_PLOSUNIT_1,
                   DSFILE.DS_PLOSUNIT_2,
                   DSFILE.DS_PLOSUNIT_3,
                   DSFILE.DS_PLOSAMT_1,
                   DSFILE.DS_PLOSAMT_2,
                   DSFILE.DS_PLOSAMT_3,
                   DSFILE.DS_RCPLUNIT_1,
                   DSFILE.DS_RCPLUNIT_2,
                   DSFILE.DS_RCPLUNIT_3,
                   DSFILE.DS_RCPLAMT_1,
                   DSFILE.DS_RCPLAMT_2,
                   DSFILE.DS_RCPLAMT_3,
                   DSFILE.DS_JDOSUNIT_1,
                   DSFILE.DS_JDOSUNIT_2,
                   DSFILE.DS_JDOSUNIT_3,
                   DSFILE.DS_JDOSAMT_1,
                   DSFILE.DS_JDOSAMT_2,
                   DSFILE.DS_JDOSAMT_3,
                   ISNULL(CAST(DSFILE.DS_DELUNEARN_DATE
                                                  AS VARCHAR(10)),'0'),
                   DSFILE.DS_PROCEEDS_1,
                   DSFILE.DS_PROCEEDS_2,
                   DSFILE.DS_NO_REPO_1_1_1,
                   DSFILE.DS_AMT_REPO_1_1_1,
                   DSFILE.DS_NO_REPO_1_1_2,
                   DSFILE.DS_AMT_REPO_1_1_2,
                   DSFILE.DS_NO_REPO_1_1_3,
                   DSFILE.DS_AMT_REPO_1_1_3,
                   DSFILE.DS_NO_REPO_1_1_4,
                   DSFILE.DS_AMT_REPO_1_1_4,
                   DSFILE.DS_NO_REPO_1_1_5,
                   DSFILE.DS_AMT_REPO_1_1_5,
                   DSFILE.DS_NO_REPO_1_2_1,
                   DSFILE.DS_AMT_REPO_1_2_1,
                   DSFILE.DS_NO_REPO_1_2_2,
                   DSFILE.DS_AMT_REPO_1_2_2,
                   DSFILE.DS_NO_REPO_1_2_3,
                   DSFILE.DS_AMT_REPO_1_2_3,
                   DSFILE.DS_NO_REPO_1_2_4,
                   DSFILE.DS_AMT_REPO_1_2_4,
                   DSFILE.DS_NO_REPO_1_2_5,
                   DSFILE.DS_AMT_REPO_1_2_5,
                   DSFILE.DS_NO_REPO_2_1_1,
                   DSFILE.DS_AMT_REPO_2_1_1,
                   DSFILE.DS_NO_REPO_2_1_2,
                   DSFILE.DS_AMT_REPO_2_1_2,
                   DSFILE.DS_NO_REPO_2_1_3,
                   DSFILE.DS_AMT_REPO_2_1_3,
                   DSFILE.DS_NO_REPO_2_1_4,
                   DSFILE.DS_AMT_REPO_2_1_4,
                   DSFILE.DS_NO_REPO_2_1_5,
                   DSFILE.DS_AMT_REPO_2_1_5,
                   DSFILE.DS_NO_REPO_2_2_1,
                   DSFILE.DS_AMT_REPO_2_2_1,
                   DSFILE.DS_NO_REPO_2_2_2,
                   DSFILE.DS_AMT_REPO_2_2_2,
                   DSFILE.DS_NO_REPO_2_2_3,
                   DSFILE.DS_AMT_REPO_2_2_3,
                   DSFILE.DS_NO_REPO_2_2_4,
                   DSFILE.DS_AMT_REPO_2_2_4,
                   DSFILE.DS_NO_REPO_2_2_5,
                   DSFILE.DS_AMT_REPO_2_2_5,
                   DSFILE.DS_NO_REPO_3_1_1,
                   DSFILE.DS_AMT_REPO_3_1_1,
                   DSFILE.DS_NO_REPO_3_1_2,
                   DSFILE.DS_AMT_REPO_3_1_2,
                   DSFILE.DS_NO_REPO_3_1_3,
                   DSFILE.DS_AMT_REPO_3_1_3,
                   DSFILE.DS_NO_REPO_3_1_4,
                   DSFILE.DS_AMT_REPO_3_1_4,
                   DSFILE.DS_NO_REPO_3_1_5,
                   DSFILE.DS_AMT_REPO_3_1_5,
                   DSFILE.DS_NO_REPO_3_2_1,
                   DSFILE.DS_AMT_REPO_3_2_1,
                   DSFILE.DS_NO_REPO_3_2_2,
                   DSFILE.DS_AMT_REPO_3_2_2,
                   DSFILE.DS_NO_REPO_3_2_3,
                   DSFILE.DS_AMT_REPO_3_2_3,
                   DSFILE.DS_NO_REPO_3_2_4,
                   DSFILE.DS_AMT_REPO_3_2_4,
                   DSFILE.DS_NO_REPO_3_2_5,
                   DSFILE.DS_AMT_REPO_3_2_5,
                   DSFILE.DS_UNITS_1_1_1,
                   DSFILE.DS_OUTBAL_1_1_1,
                   DSFILE.DS_UNITS_1_1_2,
                   DSFILE.DS_OUTBAL_1_1_2,
                   DSFILE.DS_UNITS_1_1_3,
                   DSFILE.DS_OUTBAL_1_1_3,
                   DSFILE.DS_UNITS_1_1_4,
                   DSFILE.DS_OUTBAL_1_1_4,
                   DSFILE.DS_UNITS_1_1_5,
                   DSFILE.DS_OUTBAL_1_1_5,
                   DSFILE.DS_UNITS_1_1_6,
                   DSFILE.DS_OUTBAL_1_1_6,
                   DSFILE.DS_UNITS_1_1_7,
                   DSFILE.DS_OUTBAL_1_1_7,
                   DSFILE.DS_UNITS_1_1_8,
                   DSFILE.DS_OUTBAL_1_1_8,
                   DSFILE.DS_UNITS_1_1_9,
                   DSFILE.DS_OUTBAL_1_1_9,
                   DSFILE.DS_UNITS_1_1_10,
                   DSFILE.DS_OUTBAL_1_1_10,
                   DSFILE.DS_UNITS_1_2_1,
                   DSFILE.DS_OUTBAL_1_2_1,
                   DSFILE.DS_UNITS_1_2_2,
                   DSFILE.DS_OUTBAL_1_2_2,
                   DSFILE.DS_UNITS_1_2_3,
                   DSFILE.DS_OUTBAL_1_2_3,
                   DSFILE.DS_UNITS_1_2_4,
                   DSFILE.DS_OUTBAL_1_2_4,
                   DSFILE.DS_UNITS_1_2_5,
                   DSFILE.DS_OUTBAL_1_2_5,
                   DSFILE.DS_UNITS_1_2_6,
                   DSFILE.DS_OUTBAL_1_2_6,
                   DSFILE.DS_UNITS_1_2_7,
                   DSFILE.DS_OUTBAL_1_2_7,
                   DSFILE.DS_UNITS_1_2_8,
                   DSFILE.DS_OUTBAL_1_2_8,
                   DSFILE.DS_UNITS_1_2_9,
                   DSFILE.DS_OUTBAL_1_2_9,
                   DSFILE.DS_UNITS_1_2_10,
                   DSFILE.DS_OUTBAL_1_2_10,
                   DSFILE.DS_UNITS_2_1_1,
                   DSFILE.DS_OUTBAL_2_1_1,
                   DSFILE.DS_UNITS_2_1_2,
                   DSFILE.DS_OUTBAL_2_1_2,
                   DSFILE.DS_UNITS_2_1_3,
                   DSFILE.DS_OUTBAL_2_1_3,
                   DSFILE.DS_UNITS_2_1_4,
                   DSFILE.DS_OUTBAL_2_1_4,
                   DSFILE.DS_UNITS_2_1_5,
                   DSFILE.DS_OUTBAL_2_1_5,
                   DSFILE.DS_UNITS_2_1_6,
                   DSFILE.DS_OUTBAL_2_1_6,
                   DSFILE.DS_UNITS_2_1_7,
                   DSFILE.DS_OUTBAL_2_1_7,
                   DSFILE.DS_UNITS_2_1_8,
                   DSFILE.DS_OUTBAL_2_1_8,
                   DSFILE.DS_UNITS_2_1_9,
                   DSFILE.DS_OUTBAL_2_1_9,
                   DSFILE.DS_UNITS_2_1_10,
                   DSFILE.DS_OUTBAL_2_1_10,
                   DSFILE.DS_UNITS_2_2_1,
                   DSFILE.DS_OUTBAL_2_2_1,
                   DSFILE.DS_UNITS_2_2_2,
                   DSFILE.DS_OUTBAL_2_2_2,
                   DSFILE.DS_UNITS_2_2_3,
                   DSFILE.DS_OUTBAL_2_2_3,
                   DSFILE.DS_UNITS_2_2_4,
                   DSFILE.DS_OUTBAL_2_2_4,
                   DSFILE.DS_UNITS_2_2_5,
                   DSFILE.DS_OUTBAL_2_2_5,
                   DSFILE.DS_UNITS_2_2_6,
                   DSFILE.DS_OUTBAL_2_2_6,
                   DSFILE.DS_UNITS_2_2_7,
                   DSFILE.DS_OUTBAL_2_2_7,
                   DSFILE.DS_UNITS_2_2_8,
                   DSFILE.DS_OUTBAL_2_2_8,
                   DSFILE.DS_UNITS_2_2_9,
                   DSFILE.DS_OUTBAL_2_2_9,
                   DSFILE.DS_UNITS_2_2_10,
                   DSFILE.DS_OUTBAL_2_2_10,
                   DSFILE.DS_UNITS_3_1_1,
                   DSFILE.DS_OUTBAL_3_1_1,
                   DSFILE.DS_UNITS_3_1_2,
                   DSFILE.DS_OUTBAL_3_1_2,
                   DSFILE.DS_UNITS_3_1_3,
                   DSFILE.DS_OUTBAL_3_1_3,
                   DSFILE.DS_UNITS_3_1_4,
                   DSFILE.DS_OUTBAL_3_1_4,
                   DSFILE.DS_UNITS_3_1_5,
                   DSFILE.DS_OUTBAL_3_1_5,
                   DSFILE.DS_UNITS_3_1_6,
                   DSFILE.DS_OUTBAL_3_1_6,
                   DSFILE.DS_UNITS_3_1_7,
                   DSFILE.DS_OUTBAL_3_1_7,
                   DSFILE.DS_UNITS_3_1_8,
                   DSFILE.DS_OUTBAL_3_1_8,
                   DSFILE.DS_UNITS_3_1_9,
                   DSFILE.DS_OUTBAL_3_1_9,
                   DSFILE.DS_UNITS_3_1_10,
                   DSFILE.DS_OUTBAL_3_1_10,
                   DSFILE.DS_UNITS_3_2_1,
                   DSFILE.DS_OUTBAL_3_2_1,
                   DSFILE.DS_UNITS_3_2_2,
                   DSFILE.DS_OUTBAL_3_2_2,
                   DSFILE.DS_UNITS_3_2_3,
                   DSFILE.DS_OUTBAL_3_2_3,
                   DSFILE.DS_UNITS_3_2_4,
                   DSFILE.DS_OUTBAL_3_2_4,
                   DSFILE.DS_UNITS_3_2_5,
                   DSFILE.DS_OUTBAL_3_2_5,
                   DSFILE.DS_UNITS_3_2_6,
                   DSFILE.DS_OUTBAL_3_2_6,
                   DSFILE.DS_UNITS_3_2_7,
                   DSFILE.DS_OUTBAL_3_2_7,
                   DSFILE.DS_UNITS_3_2_8,
                   DSFILE.DS_OUTBAL_3_2_8,
                   DSFILE.DS_UNITS_3_2_9,
                   DSFILE.DS_OUTBAL_3_2_9,
                   DSFILE.DS_UNITS_3_2_10,
                   DSFILE.DS_OUTBAL_3_2_10,
                   DSFILE.DS_UNITS_BNKRPT_1_1_1,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_1,
                   DSFILE.DS_UNITS_BNKRPT_1_1_2,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_2,
                   DSFILE.DS_UNITS_BNKRPT_1_1_3,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_3,
                   DSFILE.DS_UNITS_BNKRPT_1_1_4,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_4,
                   DSFILE.DS_UNITS_BNKRPT_1_1_5,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_5,
                   DSFILE.DS_UNITS_BNKRPT_1_1_6,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_6,
                   DSFILE.DS_UNITS_BNKRPT_1_1_7,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_7,
                   DSFILE.DS_UNITS_BNKRPT_1_1_8,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_8,
                   DSFILE.DS_UNITS_BNKRPT_1_1_9,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_9,
                   DSFILE.DS_UNITS_BNKRPT_1_1_10,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_10,
                   DSFILE.DS_UNITS_BNKRPT_1_2_1,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_1,
                   DSFILE.DS_UNITS_BNKRPT_1_2_2,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_2,
                   DSFILE.DS_UNITS_BNKRPT_1_2_3,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_3,
                   DSFILE.DS_UNITS_BNKRPT_1_2_4,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_4,
                   DSFILE.DS_UNITS_BNKRPT_1_2_5,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_5,
                   DSFILE.DS_UNITS_BNKRPT_1_2_6,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_6,
                   DSFILE.DS_UNITS_BNKRPT_1_2_7,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_7,
                   DSFILE.DS_UNITS_BNKRPT_1_2_8,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_8,
                   DSFILE.DS_UNITS_BNKRPT_1_2_9,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_9,
                   DSFILE.DS_UNITS_BNKRPT_1_2_10,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_10,
                   DSFILE.DS_UNITS_BNKRPT_2_1_1,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_1,
                   DSFILE.DS_UNITS_BNKRPT_2_1_2,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_2,
                   DSFILE.DS_UNITS_BNKRPT_2_1_3,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_3,
                   DSFILE.DS_UNITS_BNKRPT_2_1_4,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_4,
                   DSFILE.DS_UNITS_BNKRPT_2_1_5,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_5,
                   DSFILE.DS_UNITS_BNKRPT_2_1_6,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_6,
                   DSFILE.DS_UNITS_BNKRPT_2_1_7,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_7,
                   DSFILE.DS_UNITS_BNKRPT_2_1_8,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_8,
                   DSFILE.DS_UNITS_BNKRPT_2_1_9,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_9,
                   DSFILE.DS_UNITS_BNKRPT_2_1_10,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_10,
                   DSFILE.DS_UNITS_BNKRPT_2_2_1,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_1,
                   DSFILE.DS_UNITS_BNKRPT_2_2_2,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_2,
                   DSFILE.DS_UNITS_BNKRPT_2_2_3,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_3,
                   DSFILE.DS_UNITS_BNKRPT_2_2_4,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_4,
                   DSFILE.DS_UNITS_BNKRPT_2_2_5,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_5,
                   DSFILE.DS_UNITS_BNKRPT_2_2_6,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_6,
                   DSFILE.DS_UNITS_BNKRPT_2_2_7,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_7,
                   DSFILE.DS_UNITS_BNKRPT_2_2_8,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_8,
                   DSFILE.DS_UNITS_BNKRPT_2_2_9,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_9,
                   DSFILE.DS_UNITS_BNKRPT_2_2_10,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_10,
                   DSFILE.DS_UNITS_BNKRPT_3_1_1,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_1,
                   DSFILE.DS_UNITS_BNKRPT_3_1_2,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_2,
                   DSFILE.DS_UNITS_BNKRPT_3_1_3,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_3,
                   DSFILE.DS_UNITS_BNKRPT_3_1_4,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_4,
                   DSFILE.DS_UNITS_BNKRPT_3_1_5,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_5,
                   DSFILE.DS_UNITS_BNKRPT_3_1_6,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_6,
                   DSFILE.DS_UNITS_BNKRPT_3_1_7,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_7,
                   DSFILE.DS_UNITS_BNKRPT_3_1_8,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_8,
                   DSFILE.DS_UNITS_BNKRPT_3_1_9,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_9,
                   DSFILE.DS_UNITS_BNKRPT_3_1_10,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_10,
                   DSFILE.DS_UNITS_BNKRPT_3_2_1,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_1,
                   DSFILE.DS_UNITS_BNKRPT_3_2_2,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_2,
                   DSFILE.DS_UNITS_BNKRPT_3_2_3,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_3,
                   DSFILE.DS_UNITS_BNKRPT_3_2_4,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_4,
                   DSFILE.DS_UNITS_BNKRPT_3_2_5,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_5,
                   DSFILE.DS_UNITS_BNKRPT_3_2_6,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_6,
                   DSFILE.DS_UNITS_BNKRPT_3_2_7,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_7,
                   DSFILE.DS_UNITS_BNKRPT_3_2_8,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_8,
                   DSFILE.DS_UNITS_BNKRPT_3_2_9,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_9,
                   DSFILE.DS_UNITS_BNKRPT_3_2_10,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_10,
                   DSFILE.DS_UNEARNED_AMT_1_1_1,
                   DSFILE.DS_UNEARNED_AMT_1_1_2,
                   DSFILE.DS_UNEARNED_AMT_1_1_3,
                   DSFILE.DS_UNEARNED_AMT_1_1_4,
                   DSFILE.DS_UNEARNED_AMT_1_1_5,
                   DSFILE.DS_UNEARNED_AMT_1_1_6,
                   DSFILE.DS_UNEARNED_AMT_1_1_7,
                   DSFILE.DS_UNEARNED_AMT_1_1_8,
                   DSFILE.DS_UNEARNED_AMT_1_1_9,
                   DSFILE.DS_UNEARNED_AMT_1_1_10,
                   DSFILE.DS_UNEARNED_AMT_1_2_1,
                   DSFILE.DS_UNEARNED_AMT_1_2_2,
                   DSFILE.DS_UNEARNED_AMT_1_2_3,
                   DSFILE.DS_UNEARNED_AMT_1_2_4,
                   DSFILE.DS_UNEARNED_AMT_1_2_5,
                   DSFILE.DS_UNEARNED_AMT_1_2_6,
                   DSFILE.DS_UNEARNED_AMT_1_2_7,
                   DSFILE.DS_UNEARNED_AMT_1_2_8,
                   DSFILE.DS_UNEARNED_AMT_1_2_9,
                   DSFILE.DS_UNEARNED_AMT_1_2_10,
                   DSFILE.DS_UNEARNED_AMT_2_1_1,
                   DSFILE.DS_UNEARNED_AMT_2_1_2,
                   DSFILE.DS_UNEARNED_AMT_2_1_3,
                   DSFILE.DS_UNEARNED_AMT_2_1_4,
                   DSFILE.DS_UNEARNED_AMT_2_1_5,
                   DSFILE.DS_UNEARNED_AMT_2_1_6,
                   DSFILE.DS_UNEARNED_AMT_2_1_7,
                   DSFILE.DS_UNEARNED_AMT_2_1_8,
                   DSFILE.DS_UNEARNED_AMT_2_1_9,
                   DSFILE.DS_UNEARNED_AMT_2_1_10,
                   DSFILE.DS_UNEARNED_AMT_2_2_1,
                   DSFILE.DS_UNEARNED_AMT_2_2_2,
                   DSFILE.DS_UNEARNED_AMT_2_2_3,
                   DSFILE.DS_UNEARNED_AMT_2_2_4,
                   DSFILE.DS_UNEARNED_AMT_2_2_5,
                   DSFILE.DS_UNEARNED_AMT_2_2_6,
                   DSFILE.DS_UNEARNED_AMT_2_2_7,
                   DSFILE.DS_UNEARNED_AMT_2_2_8,
                   DSFILE.DS_UNEARNED_AMT_2_2_9,
                   DSFILE.DS_UNEARNED_AMT_2_2_10,
                   DSFILE.DS_UNEARNED_AMT_3_1_1,
                   DSFILE.DS_UNEARNED_AMT_3_1_2,
                   DSFILE.DS_UNEARNED_AMT_3_1_3,
                   DSFILE.DS_UNEARNED_AMT_3_1_4,
                   DSFILE.DS_UNEARNED_AMT_3_1_5,
                   DSFILE.DS_UNEARNED_AMT_3_1_6,
                   DSFILE.DS_UNEARNED_AMT_3_1_7,
                   DSFILE.DS_UNEARNED_AMT_3_1_8,
                   DSFILE.DS_UNEARNED_AMT_3_1_9,
                   DSFILE.DS_UNEARNED_AMT_3_1_10,
                   DSFILE.DS_UNEARNED_AMT_3_2_1,
                   DSFILE.DS_UNEARNED_AMT_3_2_2,
                   DSFILE.DS_UNEARNED_AMT_3_2_3,
                   DSFILE.DS_UNEARNED_AMT_3_2_4,
                   DSFILE.DS_UNEARNED_AMT_3_2_5,
                   DSFILE.DS_UNEARNED_AMT_3_2_6,
                   DSFILE.DS_UNEARNED_AMT_3_2_7,
                   DSFILE.DS_UNEARNED_AMT_3_2_8,
                   DSFILE.DS_UNEARNED_AMT_3_2_9,
                   DSFILE.DS_UNEARNED_AMT_3_2_10,
                   DSFILE.DS_DLDISC_VOL_1,
                   DSFILE.DS_DLDISC_VOL_2,
                   DSFILE.DS_DLDISC_VOL_3,
                   DSFILE.DS_AFPDUEDL_MEMO,
                   DSFILE.DS_UNPAID_MANUAL,
                   ISNULL(CAST(DSFILE.DS_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            FROM DBO.DSFILE
            WHERE DSFILE.DS_DATE_CCYYMM >= :QDS1-WBEG-DATE-CCYYMM
              AND DSFILE.DS_DATE_CCYYMM <= :QDS1-WEND-DATE-CCYYMM
              AND DSFILE.DS_BRANCH >= :QDS1-WBEG-BRANCH
              AND DSFILE.DS_BRANCH <= :QDS1-WEND-BRANCH
              AND DSFILE.DS_NUM >= :QDS1-WBEG-NUM
              AND DSFILE.DS_NUM <= :QDS1-WEND-NUM
              AND DSFILE.DS_CLASS >= :QDS1-WBEG-CLASS
              AND DSFILE.DS_CLASS <= :QDS1-WEND-CLASS
            ORDER BY DSFILE.DS_DATE_CCYYMM,
                     DSFILE.DS_BRANCH,
                     DSFILE.DS_NUM,
                     DSFILE.DS_CLASS
           END-EXEC.

           EXEC SQL
               OPEN DS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QDS1-WBEG-BRANCH 
                          QDS1-WEND-BRANCH
                          QDS1-WBEG-DATE-CCYYMM
                          QDS1-WEND-DATE-CCYYMM
                          QDS1-WBEG-NUM
                          QDS1-WEND-NUM
                          QDS1-WBEG-CLASS
                          QDS1-WEND-CLASS.
 
           MOVE STAT---GOOD TO DS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-DS1-FILE.
           PERFORM READ-IT.
           MOVE DS-PATH-OWNBR  TO DS-BRANCH. 
           MOVE DS-SELECT-PATH TO E-FILE.
           MOVE DS1-KEY        TO E-KEYX.

           MOVE DS-DATE-CCYYMM  TO QDS-DATE-CCYYMM.
           MOVE DS-BRANCH       TO QDS-BRANCH.
           MOVE DS-NUM          TO QDS-NUM.
           MOVE DS-CLASS        TO QDS-CLASS.

           EXEC SQL
            SELECT DSFILE.DS_DATE_CCYYMM,
                   DSFILE.DS_BRANCH,
                   DSFILE.DS_NUM,
                   DSFILE.DS_CLASS,
                   DSFILE.DS_DLRVPRIN_1,
                   DSFILE.DS_DLRVPRIN_2,
                   DSFILE.DS_DLRVPRIN_3,
                   DSFILE.DS_DLRVCHGS_1,
                   DSFILE.DS_DLRVCHGS_2,
                   DSFILE.DS_DLRVCHGS_3,
                   DSFILE.DS_DLPARVHELD,
                   DSFILE.DS_UNPAIDCK,
                   DSFILE.DS_DLDISC,
                   DSFILE.DS_DLPARVPAID,
                   DSFILE.DS_SUMWGTTM,
                   DSFILE.DS_SUMWGTAV,
                   DSFILE.DS_SUMWGTTA,
                   DSFILE.DS_RCOURSE_UNITS_1,
                   DSFILE.DS_RCOURSE_UNITS_2,
                   DSFILE.DS_RCOURSE_UNITS_3,
                   DSFILE.DS_RCOURSE_AMT_1,
                   DSFILE.DS_RCOURSE_AMT_2,
                   DSFILE.DS_RCOURSE_AMT_3,
                   DSFILE.DS_NPURUNIT_1,
                   DSFILE.DS_NPURUNIT_2,
                   DSFILE.DS_NPURUNIT_3,
                   DSFILE.DS_NPURAMT_1,
                   DSFILE.DS_NPURAMT_2,
                   DSFILE.DS_NPURAMT_3,
                   DSFILE.DS_OPURUNIT_1,
                   DSFILE.DS_OPURUNIT_2,
                   DSFILE.DS_OPURUNIT_3,
                   DSFILE.DS_OPURAMT_1,
                   DSFILE.DS_OPURAMT_2,
                   DSFILE.DS_OPURAMT_3,
                   DSFILE.DS_RNEWUNIT_1,
                   DSFILE.DS_RNEWUNIT_2,
                   DSFILE.DS_RNEWUNIT_3,
                   DSFILE.DS_RNEWAMT_1,
                   DSFILE.DS_RNEWAMT_2,
                   DSFILE.DS_RNEWAMT_3,
                   DSFILE.DS_PAIDUNIT_1,
                   DSFILE.DS_PAIDUNIT_2,
                   DSFILE.DS_PAIDUNIT_3,
                   DSFILE.DS_PAIDAMT_1,
                   DSFILE.DS_PAIDAMT_2,
                   DSFILE.DS_PAIDAMT_3,
                   DSFILE.DS_TODLUNIT_1,
                   DSFILE.DS_TODLUNIT_2,
                   DSFILE.DS_TODLUNIT_3,
                   DSFILE.DS_TODLAMT_1,
                   DSFILE.DS_TODLAMT_2,
                   DSFILE.DS_TODLAMT_3,
                   DSFILE.DS_OUTSUNIT_1,
                   DSFILE.DS_OUTSUNIT_2,
                   DSFILE.DS_OUTSUNIT_3,
                   DSFILE.DS_OUTSAMT_1,
                   DSFILE.DS_OUTSAMT_2,
                   DSFILE.DS_OUTSAMT_3,
                   DSFILE.DS_PLOSUNIT_1,
                   DSFILE.DS_PLOSUNIT_2,
                   DSFILE.DS_PLOSUNIT_3,
                   DSFILE.DS_PLOSAMT_1,
                   DSFILE.DS_PLOSAMT_2,
                   DSFILE.DS_PLOSAMT_3,
                   DSFILE.DS_RCPLUNIT_1,
                   DSFILE.DS_RCPLUNIT_2,
                   DSFILE.DS_RCPLUNIT_3,
                   DSFILE.DS_RCPLAMT_1,
                   DSFILE.DS_RCPLAMT_2,
                   DSFILE.DS_RCPLAMT_3,
                   DSFILE.DS_JDOSUNIT_1,
                   DSFILE.DS_JDOSUNIT_2,
                   DSFILE.DS_JDOSUNIT_3,
                   DSFILE.DS_JDOSAMT_1,
                   DSFILE.DS_JDOSAMT_2,
                   DSFILE.DS_JDOSAMT_3,
                   ISNULL(CAST(DSFILE.DS_DELUNEARN_DATE
                                                  AS VARCHAR(10)),'0'),
                   DSFILE.DS_PROCEEDS_1,
                   DSFILE.DS_PROCEEDS_2,
                   DSFILE.DS_NO_REPO_1_1_1,
                   DSFILE.DS_AMT_REPO_1_1_1,
                   DSFILE.DS_NO_REPO_1_1_2,
                   DSFILE.DS_AMT_REPO_1_1_2,
                   DSFILE.DS_NO_REPO_1_1_3,
                   DSFILE.DS_AMT_REPO_1_1_3,
                   DSFILE.DS_NO_REPO_1_1_4,
                   DSFILE.DS_AMT_REPO_1_1_4,
                   DSFILE.DS_NO_REPO_1_1_5,
                   DSFILE.DS_AMT_REPO_1_1_5,
                   DSFILE.DS_NO_REPO_1_2_1,
                   DSFILE.DS_AMT_REPO_1_2_1,
                   DSFILE.DS_NO_REPO_1_2_2,
                   DSFILE.DS_AMT_REPO_1_2_2,
                   DSFILE.DS_NO_REPO_1_2_3,
                   DSFILE.DS_AMT_REPO_1_2_3,
                   DSFILE.DS_NO_REPO_1_2_4,
                   DSFILE.DS_AMT_REPO_1_2_4,
                   DSFILE.DS_NO_REPO_1_2_5,
                   DSFILE.DS_AMT_REPO_1_2_5,
                   DSFILE.DS_NO_REPO_2_1_1,
                   DSFILE.DS_AMT_REPO_2_1_1,
                   DSFILE.DS_NO_REPO_2_1_2,
                   DSFILE.DS_AMT_REPO_2_1_2,
                   DSFILE.DS_NO_REPO_2_1_3,
                   DSFILE.DS_AMT_REPO_2_1_3,
                   DSFILE.DS_NO_REPO_2_1_4,
                   DSFILE.DS_AMT_REPO_2_1_4,
                   DSFILE.DS_NO_REPO_2_1_5,
                   DSFILE.DS_AMT_REPO_2_1_5,
                   DSFILE.DS_NO_REPO_2_2_1,
                   DSFILE.DS_AMT_REPO_2_2_1,
                   DSFILE.DS_NO_REPO_2_2_2,
                   DSFILE.DS_AMT_REPO_2_2_2,
                   DSFILE.DS_NO_REPO_2_2_3,
                   DSFILE.DS_AMT_REPO_2_2_3,
                   DSFILE.DS_NO_REPO_2_2_4,
                   DSFILE.DS_AMT_REPO_2_2_4,
                   DSFILE.DS_NO_REPO_2_2_5,
                   DSFILE.DS_AMT_REPO_2_2_5,
                   DSFILE.DS_NO_REPO_3_1_1,
                   DSFILE.DS_AMT_REPO_3_1_1,
                   DSFILE.DS_NO_REPO_3_1_2,
                   DSFILE.DS_AMT_REPO_3_1_2,
                   DSFILE.DS_NO_REPO_3_1_3,
                   DSFILE.DS_AMT_REPO_3_1_3,
                   DSFILE.DS_NO_REPO_3_1_4,
                   DSFILE.DS_AMT_REPO_3_1_4,
                   DSFILE.DS_NO_REPO_3_1_5,
                   DSFILE.DS_AMT_REPO_3_1_5,
                   DSFILE.DS_NO_REPO_3_2_1,
                   DSFILE.DS_AMT_REPO_3_2_1,
                   DSFILE.DS_NO_REPO_3_2_2,
                   DSFILE.DS_AMT_REPO_3_2_2,
                   DSFILE.DS_NO_REPO_3_2_3,
                   DSFILE.DS_AMT_REPO_3_2_3,
                   DSFILE.DS_NO_REPO_3_2_4,
                   DSFILE.DS_AMT_REPO_3_2_4,
                   DSFILE.DS_NO_REPO_3_2_5,
                   DSFILE.DS_AMT_REPO_3_2_5,
                   DSFILE.DS_UNITS_1_1_1,
                   DSFILE.DS_OUTBAL_1_1_1,
                   DSFILE.DS_UNITS_1_1_2,
                   DSFILE.DS_OUTBAL_1_1_2,
                   DSFILE.DS_UNITS_1_1_3,
                   DSFILE.DS_OUTBAL_1_1_3,
                   DSFILE.DS_UNITS_1_1_4,
                   DSFILE.DS_OUTBAL_1_1_4,
                   DSFILE.DS_UNITS_1_1_5,
                   DSFILE.DS_OUTBAL_1_1_5,
                   DSFILE.DS_UNITS_1_1_6,
                   DSFILE.DS_OUTBAL_1_1_6,
                   DSFILE.DS_UNITS_1_1_7,
                   DSFILE.DS_OUTBAL_1_1_7,
                   DSFILE.DS_UNITS_1_1_8,
                   DSFILE.DS_OUTBAL_1_1_8,
                   DSFILE.DS_UNITS_1_1_9,
                   DSFILE.DS_OUTBAL_1_1_9,
                   DSFILE.DS_UNITS_1_1_10,
                   DSFILE.DS_OUTBAL_1_1_10,
                   DSFILE.DS_UNITS_1_2_1,
                   DSFILE.DS_OUTBAL_1_2_1,
                   DSFILE.DS_UNITS_1_2_2,
                   DSFILE.DS_OUTBAL_1_2_2,
                   DSFILE.DS_UNITS_1_2_3,
                   DSFILE.DS_OUTBAL_1_2_3,
                   DSFILE.DS_UNITS_1_2_4,
                   DSFILE.DS_OUTBAL_1_2_4,
                   DSFILE.DS_UNITS_1_2_5,
                   DSFILE.DS_OUTBAL_1_2_5,
                   DSFILE.DS_UNITS_1_2_6,
                   DSFILE.DS_OUTBAL_1_2_6,
                   DSFILE.DS_UNITS_1_2_7,
                   DSFILE.DS_OUTBAL_1_2_7,
                   DSFILE.DS_UNITS_1_2_8,
                   DSFILE.DS_OUTBAL_1_2_8,
                   DSFILE.DS_UNITS_1_2_9,
                   DSFILE.DS_OUTBAL_1_2_9,
                   DSFILE.DS_UNITS_1_2_10,
                   DSFILE.DS_OUTBAL_1_2_10,
                   DSFILE.DS_UNITS_2_1_1,
                   DSFILE.DS_OUTBAL_2_1_1,
                   DSFILE.DS_UNITS_2_1_2,
                   DSFILE.DS_OUTBAL_2_1_2,
                   DSFILE.DS_UNITS_2_1_3,
                   DSFILE.DS_OUTBAL_2_1_3,
                   DSFILE.DS_UNITS_2_1_4,
                   DSFILE.DS_OUTBAL_2_1_4,
                   DSFILE.DS_UNITS_2_1_5,
                   DSFILE.DS_OUTBAL_2_1_5,
                   DSFILE.DS_UNITS_2_1_6,
                   DSFILE.DS_OUTBAL_2_1_6,
                   DSFILE.DS_UNITS_2_1_7,
                   DSFILE.DS_OUTBAL_2_1_7,
                   DSFILE.DS_UNITS_2_1_8,
                   DSFILE.DS_OUTBAL_2_1_8,
                   DSFILE.DS_UNITS_2_1_9,
                   DSFILE.DS_OUTBAL_2_1_9,
                   DSFILE.DS_UNITS_2_1_10,
                   DSFILE.DS_OUTBAL_2_1_10,
                   DSFILE.DS_UNITS_2_2_1,
                   DSFILE.DS_OUTBAL_2_2_1,
                   DSFILE.DS_UNITS_2_2_2,
                   DSFILE.DS_OUTBAL_2_2_2,
                   DSFILE.DS_UNITS_2_2_3,
                   DSFILE.DS_OUTBAL_2_2_3,
                   DSFILE.DS_UNITS_2_2_4,
                   DSFILE.DS_OUTBAL_2_2_4,
                   DSFILE.DS_UNITS_2_2_5,
                   DSFILE.DS_OUTBAL_2_2_5,
                   DSFILE.DS_UNITS_2_2_6,
                   DSFILE.DS_OUTBAL_2_2_6,
                   DSFILE.DS_UNITS_2_2_7,
                   DSFILE.DS_OUTBAL_2_2_7,
                   DSFILE.DS_UNITS_2_2_8,
                   DSFILE.DS_OUTBAL_2_2_8,
                   DSFILE.DS_UNITS_2_2_9,
                   DSFILE.DS_OUTBAL_2_2_9,
                   DSFILE.DS_UNITS_2_2_10,
                   DSFILE.DS_OUTBAL_2_2_10,
                   DSFILE.DS_UNITS_3_1_1,
                   DSFILE.DS_OUTBAL_3_1_1,
                   DSFILE.DS_UNITS_3_1_2,
                   DSFILE.DS_OUTBAL_3_1_2,
                   DSFILE.DS_UNITS_3_1_3,
                   DSFILE.DS_OUTBAL_3_1_3,
                   DSFILE.DS_UNITS_3_1_4,
                   DSFILE.DS_OUTBAL_3_1_4,
                   DSFILE.DS_UNITS_3_1_5,
                   DSFILE.DS_OUTBAL_3_1_5,
                   DSFILE.DS_UNITS_3_1_6,
                   DSFILE.DS_OUTBAL_3_1_6,
                   DSFILE.DS_UNITS_3_1_7,
                   DSFILE.DS_OUTBAL_3_1_7,
                   DSFILE.DS_UNITS_3_1_8,
                   DSFILE.DS_OUTBAL_3_1_8,
                   DSFILE.DS_UNITS_3_1_9,
                   DSFILE.DS_OUTBAL_3_1_9,
                   DSFILE.DS_UNITS_3_1_10,
                   DSFILE.DS_OUTBAL_3_1_10,
                   DSFILE.DS_UNITS_3_2_1,
                   DSFILE.DS_OUTBAL_3_2_1,
                   DSFILE.DS_UNITS_3_2_2,
                   DSFILE.DS_OUTBAL_3_2_2,
                   DSFILE.DS_UNITS_3_2_3,
                   DSFILE.DS_OUTBAL_3_2_3,
                   DSFILE.DS_UNITS_3_2_4,
                   DSFILE.DS_OUTBAL_3_2_4,
                   DSFILE.DS_UNITS_3_2_5,
                   DSFILE.DS_OUTBAL_3_2_5,
                   DSFILE.DS_UNITS_3_2_6,
                   DSFILE.DS_OUTBAL_3_2_6,
                   DSFILE.DS_UNITS_3_2_7,
                   DSFILE.DS_OUTBAL_3_2_7,
                   DSFILE.DS_UNITS_3_2_8,
                   DSFILE.DS_OUTBAL_3_2_8,
                   DSFILE.DS_UNITS_3_2_9,
                   DSFILE.DS_OUTBAL_3_2_9,
                   DSFILE.DS_UNITS_3_2_10,
                   DSFILE.DS_OUTBAL_3_2_10,
                   DSFILE.DS_UNITS_BNKRPT_1_1_1,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_1,
                   DSFILE.DS_UNITS_BNKRPT_1_1_2,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_2,
                   DSFILE.DS_UNITS_BNKRPT_1_1_3,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_3,
                   DSFILE.DS_UNITS_BNKRPT_1_1_4,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_4,
                   DSFILE.DS_UNITS_BNKRPT_1_1_5,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_5,
                   DSFILE.DS_UNITS_BNKRPT_1_1_6,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_6,
                   DSFILE.DS_UNITS_BNKRPT_1_1_7,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_7,
                   DSFILE.DS_UNITS_BNKRPT_1_1_8,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_8,
                   DSFILE.DS_UNITS_BNKRPT_1_1_9,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_9,
                   DSFILE.DS_UNITS_BNKRPT_1_1_10,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_10,
                   DSFILE.DS_UNITS_BNKRPT_1_2_1,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_1,
                   DSFILE.DS_UNITS_BNKRPT_1_2_2,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_2,
                   DSFILE.DS_UNITS_BNKRPT_1_2_3,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_3,
                   DSFILE.DS_UNITS_BNKRPT_1_2_4,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_4,
                   DSFILE.DS_UNITS_BNKRPT_1_2_5,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_5,
                   DSFILE.DS_UNITS_BNKRPT_1_2_6,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_6,
                   DSFILE.DS_UNITS_BNKRPT_1_2_7,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_7,
                   DSFILE.DS_UNITS_BNKRPT_1_2_8,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_8,
                   DSFILE.DS_UNITS_BNKRPT_1_2_9,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_9,
                   DSFILE.DS_UNITS_BNKRPT_1_2_10,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_10,
                   DSFILE.DS_UNITS_BNKRPT_2_1_1,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_1,
                   DSFILE.DS_UNITS_BNKRPT_2_1_2,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_2,
                   DSFILE.DS_UNITS_BNKRPT_2_1_3,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_3,
                   DSFILE.DS_UNITS_BNKRPT_2_1_4,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_4,
                   DSFILE.DS_UNITS_BNKRPT_2_1_5,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_5,
                   DSFILE.DS_UNITS_BNKRPT_2_1_6,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_6,
                   DSFILE.DS_UNITS_BNKRPT_2_1_7,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_7,
                   DSFILE.DS_UNITS_BNKRPT_2_1_8,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_8,
                   DSFILE.DS_UNITS_BNKRPT_2_1_9,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_9,
                   DSFILE.DS_UNITS_BNKRPT_2_1_10,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_10,
                   DSFILE.DS_UNITS_BNKRPT_2_2_1,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_1,
                   DSFILE.DS_UNITS_BNKRPT_2_2_2,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_2,
                   DSFILE.DS_UNITS_BNKRPT_2_2_3,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_3,
                   DSFILE.DS_UNITS_BNKRPT_2_2_4,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_4,
                   DSFILE.DS_UNITS_BNKRPT_2_2_5,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_5,
                   DSFILE.DS_UNITS_BNKRPT_2_2_6,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_6,
                   DSFILE.DS_UNITS_BNKRPT_2_2_7,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_7,
                   DSFILE.DS_UNITS_BNKRPT_2_2_8,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_8,
                   DSFILE.DS_UNITS_BNKRPT_2_2_9,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_9,
                   DSFILE.DS_UNITS_BNKRPT_2_2_10,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_10,
                   DSFILE.DS_UNITS_BNKRPT_3_1_1,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_1,
                   DSFILE.DS_UNITS_BNKRPT_3_1_2,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_2,
                   DSFILE.DS_UNITS_BNKRPT_3_1_3,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_3,
                   DSFILE.DS_UNITS_BNKRPT_3_1_4,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_4,
                   DSFILE.DS_UNITS_BNKRPT_3_1_5,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_5,
                   DSFILE.DS_UNITS_BNKRPT_3_1_6,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_6,
                   DSFILE.DS_UNITS_BNKRPT_3_1_7,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_7,
                   DSFILE.DS_UNITS_BNKRPT_3_1_8,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_8,
                   DSFILE.DS_UNITS_BNKRPT_3_1_9,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_9,
                   DSFILE.DS_UNITS_BNKRPT_3_1_10,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_10,
                   DSFILE.DS_UNITS_BNKRPT_3_2_1,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_1,
                   DSFILE.DS_UNITS_BNKRPT_3_2_2,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_2,
                   DSFILE.DS_UNITS_BNKRPT_3_2_3,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_3,
                   DSFILE.DS_UNITS_BNKRPT_3_2_4,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_4,
                   DSFILE.DS_UNITS_BNKRPT_3_2_5,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_5,
                   DSFILE.DS_UNITS_BNKRPT_3_2_6,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_6,
                   DSFILE.DS_UNITS_BNKRPT_3_2_7,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_7,
                   DSFILE.DS_UNITS_BNKRPT_3_2_8,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_8,
                   DSFILE.DS_UNITS_BNKRPT_3_2_9,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_9,
                   DSFILE.DS_UNITS_BNKRPT_3_2_10,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_10,
                   DSFILE.DS_UNEARNED_AMT_1_1_1,
                   DSFILE.DS_UNEARNED_AMT_1_1_2,
                   DSFILE.DS_UNEARNED_AMT_1_1_3,
                   DSFILE.DS_UNEARNED_AMT_1_1_4,
                   DSFILE.DS_UNEARNED_AMT_1_1_5,
                   DSFILE.DS_UNEARNED_AMT_1_1_6,
                   DSFILE.DS_UNEARNED_AMT_1_1_7,
                   DSFILE.DS_UNEARNED_AMT_1_1_8,
                   DSFILE.DS_UNEARNED_AMT_1_1_9,
                   DSFILE.DS_UNEARNED_AMT_1_1_10,
                   DSFILE.DS_UNEARNED_AMT_1_2_1,
                   DSFILE.DS_UNEARNED_AMT_1_2_2,
                   DSFILE.DS_UNEARNED_AMT_1_2_3,
                   DSFILE.DS_UNEARNED_AMT_1_2_4,
                   DSFILE.DS_UNEARNED_AMT_1_2_5,
                   DSFILE.DS_UNEARNED_AMT_1_2_6,
                   DSFILE.DS_UNEARNED_AMT_1_2_7,
                   DSFILE.DS_UNEARNED_AMT_1_2_8,
                   DSFILE.DS_UNEARNED_AMT_1_2_9,
                   DSFILE.DS_UNEARNED_AMT_1_2_10,
                   DSFILE.DS_UNEARNED_AMT_2_1_1,
                   DSFILE.DS_UNEARNED_AMT_2_1_2,
                   DSFILE.DS_UNEARNED_AMT_2_1_3,
                   DSFILE.DS_UNEARNED_AMT_2_1_4,
                   DSFILE.DS_UNEARNED_AMT_2_1_5,
                   DSFILE.DS_UNEARNED_AMT_2_1_6,
                   DSFILE.DS_UNEARNED_AMT_2_1_7,
                   DSFILE.DS_UNEARNED_AMT_2_1_8,
                   DSFILE.DS_UNEARNED_AMT_2_1_9,
                   DSFILE.DS_UNEARNED_AMT_2_1_10,
                   DSFILE.DS_UNEARNED_AMT_2_2_1,
                   DSFILE.DS_UNEARNED_AMT_2_2_2,
                   DSFILE.DS_UNEARNED_AMT_2_2_3,
                   DSFILE.DS_UNEARNED_AMT_2_2_4,
                   DSFILE.DS_UNEARNED_AMT_2_2_5,
                   DSFILE.DS_UNEARNED_AMT_2_2_6,
                   DSFILE.DS_UNEARNED_AMT_2_2_7,
                   DSFILE.DS_UNEARNED_AMT_2_2_8,
                   DSFILE.DS_UNEARNED_AMT_2_2_9,
                   DSFILE.DS_UNEARNED_AMT_2_2_10,
                   DSFILE.DS_UNEARNED_AMT_3_1_1,
                   DSFILE.DS_UNEARNED_AMT_3_1_2,
                   DSFILE.DS_UNEARNED_AMT_3_1_3,
                   DSFILE.DS_UNEARNED_AMT_3_1_4,
                   DSFILE.DS_UNEARNED_AMT_3_1_5,
                   DSFILE.DS_UNEARNED_AMT_3_1_6,
                   DSFILE.DS_UNEARNED_AMT_3_1_7,
                   DSFILE.DS_UNEARNED_AMT_3_1_8,
                   DSFILE.DS_UNEARNED_AMT_3_1_9,
                   DSFILE.DS_UNEARNED_AMT_3_1_10,
                   DSFILE.DS_UNEARNED_AMT_3_2_1,
                   DSFILE.DS_UNEARNED_AMT_3_2_2,
                   DSFILE.DS_UNEARNED_AMT_3_2_3,
                   DSFILE.DS_UNEARNED_AMT_3_2_4,
                   DSFILE.DS_UNEARNED_AMT_3_2_5,
                   DSFILE.DS_UNEARNED_AMT_3_2_6,
                   DSFILE.DS_UNEARNED_AMT_3_2_7,
                   DSFILE.DS_UNEARNED_AMT_3_2_8,
                   DSFILE.DS_UNEARNED_AMT_3_2_9,
                   DSFILE.DS_UNEARNED_AMT_3_2_10,
                   DSFILE.DS_DLDISC_VOL_1,
                   DSFILE.DS_DLDISC_VOL_2,
                   DSFILE.DS_DLDISC_VOL_3,
                   DSFILE.DS_AFPDUEDL_MEMO,
                   DSFILE.DS_UNPAID_MANUAL,
                   ISNULL(CAST(DSFILE.DS_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            INTO :QDS-REC
            FROM DBO.DSFILE
            WHERE DSFILE.DS_DATE_CCYYMM = :QDS-DATE-CCYYMM
              AND DSFILE.DS_BRANCH = :QDS-BRANCH
              AND DSFILE.DS_NUM = :QDS-NUM
              AND DSFILE.DS_CLASS = :QDS-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DS1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-DS-FIELDS.

      *-----------------------------------------------------------------
       READ-DS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE DS-PATH-OWNBR  TO DS-BRANCH.
           MOVE DS-SELECT-PATH TO E-FILE.
           MOVE DS1-KEY        TO E-KEYX.
           INITIALIZE QDS-REC.

           IF ( DS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH DS1_CURSOR INTO :QDS-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-DS-FIELDS.

      *-----------------------------------------------------------------
       WRITE-DS1-FILE.

           MOVE DS-PATH-OWNBR  TO DS-BRANCH.
           ACCEPT DS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE DS-SELECT-PATH TO E-FILE.
           MOVE DS1-KEY        TO E-KEYX.
           PERFORM SET-DS-FIELDS.

           EXEC SQL
            INSERT INTO DBO.DSFILE
             (DSFILE.DS_DATE_CCYYMM,
              DSFILE.DS_BRANCH,
              DSFILE.DS_NUM,
              DSFILE.DS_CLASS,
              DSFILE.DS_DLRVPRIN_1,
              DSFILE.DS_DLRVPRIN_2,
              DSFILE.DS_DLRVPRIN_3,
              DSFILE.DS_DLRVCHGS_1,
              DSFILE.DS_DLRVCHGS_2,
              DSFILE.DS_DLRVCHGS_3,
              DSFILE.DS_DLPARVHELD,
              DSFILE.DS_UNPAIDCK,
              DSFILE.DS_DLDISC,
              DSFILE.DS_DLPARVPAID,
              DSFILE.DS_SUMWGTTM,
              DSFILE.DS_SUMWGTAV,
              DSFILE.DS_SUMWGTTA,
              DSFILE.DS_RCOURSE_UNITS_1,
              DSFILE.DS_RCOURSE_UNITS_2,
              DSFILE.DS_RCOURSE_UNITS_3,
              DSFILE.DS_RCOURSE_AMT_1,
              DSFILE.DS_RCOURSE_AMT_2,
              DSFILE.DS_RCOURSE_AMT_3,
              DSFILE.DS_NPURUNIT_1,
              DSFILE.DS_NPURUNIT_2,
              DSFILE.DS_NPURUNIT_3,
              DSFILE.DS_NPURAMT_1,
              DSFILE.DS_NPURAMT_2,
              DSFILE.DS_NPURAMT_3,
              DSFILE.DS_OPURUNIT_1,
              DSFILE.DS_OPURUNIT_2,
              DSFILE.DS_OPURUNIT_3,
              DSFILE.DS_OPURAMT_1,
              DSFILE.DS_OPURAMT_2,
              DSFILE.DS_OPURAMT_3,
              DSFILE.DS_RNEWUNIT_1,
              DSFILE.DS_RNEWUNIT_2,
              DSFILE.DS_RNEWUNIT_3,
              DSFILE.DS_RNEWAMT_1,
              DSFILE.DS_RNEWAMT_2,
              DSFILE.DS_RNEWAMT_3,
              DSFILE.DS_PAIDUNIT_1,
              DSFILE.DS_PAIDUNIT_2,
              DSFILE.DS_PAIDUNIT_3,
              DSFILE.DS_PAIDAMT_1,
              DSFILE.DS_PAIDAMT_2,
              DSFILE.DS_PAIDAMT_3,
              DSFILE.DS_TODLUNIT_1,
              DSFILE.DS_TODLUNIT_2,
              DSFILE.DS_TODLUNIT_3,
              DSFILE.DS_TODLAMT_1,
              DSFILE.DS_TODLAMT_2,
              DSFILE.DS_TODLAMT_3,
              DSFILE.DS_OUTSUNIT_1,
              DSFILE.DS_OUTSUNIT_2,
              DSFILE.DS_OUTSUNIT_3,
              DSFILE.DS_OUTSAMT_1,
              DSFILE.DS_OUTSAMT_2,
              DSFILE.DS_OUTSAMT_3,
              DSFILE.DS_PLOSUNIT_1,
              DSFILE.DS_PLOSUNIT_2,
              DSFILE.DS_PLOSUNIT_3,
              DSFILE.DS_PLOSAMT_1,
              DSFILE.DS_PLOSAMT_2,
              DSFILE.DS_PLOSAMT_3,
              DSFILE.DS_RCPLUNIT_1,
              DSFILE.DS_RCPLUNIT_2,
              DSFILE.DS_RCPLUNIT_3,
              DSFILE.DS_RCPLAMT_1,
              DSFILE.DS_RCPLAMT_2,
              DSFILE.DS_RCPLAMT_3,
              DSFILE.DS_JDOSUNIT_1,
              DSFILE.DS_JDOSUNIT_2,
              DSFILE.DS_JDOSUNIT_3,
              DSFILE.DS_JDOSAMT_1,
              DSFILE.DS_JDOSAMT_2,
              DSFILE.DS_JDOSAMT_3,
              DSFILE.DS_DELUNEARN_DATE,
              DSFILE.DS_PROCEEDS_1,
              DSFILE.DS_PROCEEDS_2,
              DSFILE.DS_NO_REPO_1_1_1,
              DSFILE.DS_AMT_REPO_1_1_1,
              DSFILE.DS_NO_REPO_1_1_2,
              DSFILE.DS_AMT_REPO_1_1_2,
              DSFILE.DS_NO_REPO_1_1_3,
              DSFILE.DS_AMT_REPO_1_1_3,
              DSFILE.DS_NO_REPO_1_1_4,
              DSFILE.DS_AMT_REPO_1_1_4,
              DSFILE.DS_NO_REPO_1_1_5,
              DSFILE.DS_AMT_REPO_1_1_5,
              DSFILE.DS_NO_REPO_1_2_1,
              DSFILE.DS_AMT_REPO_1_2_1,
              DSFILE.DS_NO_REPO_1_2_2,
              DSFILE.DS_AMT_REPO_1_2_2,
              DSFILE.DS_NO_REPO_1_2_3,
              DSFILE.DS_AMT_REPO_1_2_3,
              DSFILE.DS_NO_REPO_1_2_4,
              DSFILE.DS_AMT_REPO_1_2_4,
              DSFILE.DS_NO_REPO_1_2_5,
              DSFILE.DS_AMT_REPO_1_2_5,
              DSFILE.DS_NO_REPO_2_1_1,
              DSFILE.DS_AMT_REPO_2_1_1,
              DSFILE.DS_NO_REPO_2_1_2,
              DSFILE.DS_AMT_REPO_2_1_2,
              DSFILE.DS_NO_REPO_2_1_3,
              DSFILE.DS_AMT_REPO_2_1_3,
              DSFILE.DS_NO_REPO_2_1_4,
              DSFILE.DS_AMT_REPO_2_1_4,
              DSFILE.DS_NO_REPO_2_1_5,
              DSFILE.DS_AMT_REPO_2_1_5,
              DSFILE.DS_NO_REPO_2_2_1,
              DSFILE.DS_AMT_REPO_2_2_1,
              DSFILE.DS_NO_REPO_2_2_2,
              DSFILE.DS_AMT_REPO_2_2_2,
              DSFILE.DS_NO_REPO_2_2_3,
              DSFILE.DS_AMT_REPO_2_2_3,
              DSFILE.DS_NO_REPO_2_2_4,
              DSFILE.DS_AMT_REPO_2_2_4,
              DSFILE.DS_NO_REPO_2_2_5,
              DSFILE.DS_AMT_REPO_2_2_5,
              DSFILE.DS_NO_REPO_3_1_1,
              DSFILE.DS_AMT_REPO_3_1_1,
              DSFILE.DS_NO_REPO_3_1_2,
              DSFILE.DS_AMT_REPO_3_1_2,
              DSFILE.DS_NO_REPO_3_1_3,
              DSFILE.DS_AMT_REPO_3_1_3,
              DSFILE.DS_NO_REPO_3_1_4,
              DSFILE.DS_AMT_REPO_3_1_4,
              DSFILE.DS_NO_REPO_3_1_5,
              DSFILE.DS_AMT_REPO_3_1_5,
              DSFILE.DS_NO_REPO_3_2_1,
              DSFILE.DS_AMT_REPO_3_2_1,
              DSFILE.DS_NO_REPO_3_2_2,
              DSFILE.DS_AMT_REPO_3_2_2,
              DSFILE.DS_NO_REPO_3_2_3,
              DSFILE.DS_AMT_REPO_3_2_3,
              DSFILE.DS_NO_REPO_3_2_4,
              DSFILE.DS_AMT_REPO_3_2_4,
              DSFILE.DS_NO_REPO_3_2_5,
              DSFILE.DS_AMT_REPO_3_2_5,
              DSFILE.DS_UNITS_1_1_1,
              DSFILE.DS_OUTBAL_1_1_1,
              DSFILE.DS_UNITS_1_1_2,
              DSFILE.DS_OUTBAL_1_1_2,
              DSFILE.DS_UNITS_1_1_3,
              DSFILE.DS_OUTBAL_1_1_3,
              DSFILE.DS_UNITS_1_1_4,
              DSFILE.DS_OUTBAL_1_1_4,
              DSFILE.DS_UNITS_1_1_5,
              DSFILE.DS_OUTBAL_1_1_5,
              DSFILE.DS_UNITS_1_1_6,
              DSFILE.DS_OUTBAL_1_1_6,
              DSFILE.DS_UNITS_1_1_7,
              DSFILE.DS_OUTBAL_1_1_7,
              DSFILE.DS_UNITS_1_1_8,
              DSFILE.DS_OUTBAL_1_1_8,
              DSFILE.DS_UNITS_1_1_9,
              DSFILE.DS_OUTBAL_1_1_9,
              DSFILE.DS_UNITS_1_1_10,
              DSFILE.DS_OUTBAL_1_1_10,
              DSFILE.DS_UNITS_1_2_1,
              DSFILE.DS_OUTBAL_1_2_1,
              DSFILE.DS_UNITS_1_2_2,
              DSFILE.DS_OUTBAL_1_2_2,
              DSFILE.DS_UNITS_1_2_3,
              DSFILE.DS_OUTBAL_1_2_3,
              DSFILE.DS_UNITS_1_2_4,
              DSFILE.DS_OUTBAL_1_2_4,
              DSFILE.DS_UNITS_1_2_5,
              DSFILE.DS_OUTBAL_1_2_5,
              DSFILE.DS_UNITS_1_2_6,
              DSFILE.DS_OUTBAL_1_2_6,
              DSFILE.DS_UNITS_1_2_7,
              DSFILE.DS_OUTBAL_1_2_7,
              DSFILE.DS_UNITS_1_2_8,
              DSFILE.DS_OUTBAL_1_2_8,
              DSFILE.DS_UNITS_1_2_9,
              DSFILE.DS_OUTBAL_1_2_9,
              DSFILE.DS_UNITS_1_2_10,
              DSFILE.DS_OUTBAL_1_2_10,
              DSFILE.DS_UNITS_2_1_1,
              DSFILE.DS_OUTBAL_2_1_1,
              DSFILE.DS_UNITS_2_1_2,
              DSFILE.DS_OUTBAL_2_1_2,
              DSFILE.DS_UNITS_2_1_3,
              DSFILE.DS_OUTBAL_2_1_3,
              DSFILE.DS_UNITS_2_1_4,
              DSFILE.DS_OUTBAL_2_1_4,
              DSFILE.DS_UNITS_2_1_5,
              DSFILE.DS_OUTBAL_2_1_5,
              DSFILE.DS_UNITS_2_1_6,
              DSFILE.DS_OUTBAL_2_1_6,
              DSFILE.DS_UNITS_2_1_7,
              DSFILE.DS_OUTBAL_2_1_7,
              DSFILE.DS_UNITS_2_1_8,
              DSFILE.DS_OUTBAL_2_1_8,
              DSFILE.DS_UNITS_2_1_9,
              DSFILE.DS_OUTBAL_2_1_9,
              DSFILE.DS_UNITS_2_1_10,
              DSFILE.DS_OUTBAL_2_1_10,
              DSFILE.DS_UNITS_2_2_1,
              DSFILE.DS_OUTBAL_2_2_1,
              DSFILE.DS_UNITS_2_2_2,
              DSFILE.DS_OUTBAL_2_2_2,
              DSFILE.DS_UNITS_2_2_3,
              DSFILE.DS_OUTBAL_2_2_3,
              DSFILE.DS_UNITS_2_2_4,
              DSFILE.DS_OUTBAL_2_2_4,
              DSFILE.DS_UNITS_2_2_5,
              DSFILE.DS_OUTBAL_2_2_5,
              DSFILE.DS_UNITS_2_2_6,
              DSFILE.DS_OUTBAL_2_2_6,
              DSFILE.DS_UNITS_2_2_7,
              DSFILE.DS_OUTBAL_2_2_7,
              DSFILE.DS_UNITS_2_2_8,
              DSFILE.DS_OUTBAL_2_2_8,
              DSFILE.DS_UNITS_2_2_9,
              DSFILE.DS_OUTBAL_2_2_9,
              DSFILE.DS_UNITS_2_2_10,
              DSFILE.DS_OUTBAL_2_2_10,
              DSFILE.DS_UNITS_3_1_1,
              DSFILE.DS_OUTBAL_3_1_1,
              DSFILE.DS_UNITS_3_1_2,
              DSFILE.DS_OUTBAL_3_1_2,
              DSFILE.DS_UNITS_3_1_3,
              DSFILE.DS_OUTBAL_3_1_3,
              DSFILE.DS_UNITS_3_1_4,
              DSFILE.DS_OUTBAL_3_1_4,
              DSFILE.DS_UNITS_3_1_5,
              DSFILE.DS_OUTBAL_3_1_5,
              DSFILE.DS_UNITS_3_1_6,
              DSFILE.DS_OUTBAL_3_1_6,
              DSFILE.DS_UNITS_3_1_7,
              DSFILE.DS_OUTBAL_3_1_7,
              DSFILE.DS_UNITS_3_1_8,
              DSFILE.DS_OUTBAL_3_1_8,
              DSFILE.DS_UNITS_3_1_9,
              DSFILE.DS_OUTBAL_3_1_9,
              DSFILE.DS_UNITS_3_1_10,
              DSFILE.DS_OUTBAL_3_1_10,
              DSFILE.DS_UNITS_3_2_1,
              DSFILE.DS_OUTBAL_3_2_1,
              DSFILE.DS_UNITS_3_2_2,
              DSFILE.DS_OUTBAL_3_2_2,
              DSFILE.DS_UNITS_3_2_3,
              DSFILE.DS_OUTBAL_3_2_3,
              DSFILE.DS_UNITS_3_2_4,
              DSFILE.DS_OUTBAL_3_2_4,
              DSFILE.DS_UNITS_3_2_5,
              DSFILE.DS_OUTBAL_3_2_5,
              DSFILE.DS_UNITS_3_2_6,
              DSFILE.DS_OUTBAL_3_2_6,
              DSFILE.DS_UNITS_3_2_7,
              DSFILE.DS_OUTBAL_3_2_7,
              DSFILE.DS_UNITS_3_2_8,
              DSFILE.DS_OUTBAL_3_2_8,
              DSFILE.DS_UNITS_3_2_9,
              DSFILE.DS_OUTBAL_3_2_9,
              DSFILE.DS_UNITS_3_2_10,
              DSFILE.DS_OUTBAL_3_2_10,
              DSFILE.DS_UNITS_BNKRPT_1_1_1,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_1,
              DSFILE.DS_UNITS_BNKRPT_1_1_2,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_2,
              DSFILE.DS_UNITS_BNKRPT_1_1_3,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_3,
              DSFILE.DS_UNITS_BNKRPT_1_1_4,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_4,
              DSFILE.DS_UNITS_BNKRPT_1_1_5,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_5,
              DSFILE.DS_UNITS_BNKRPT_1_1_6,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_6,
              DSFILE.DS_UNITS_BNKRPT_1_1_7,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_7,
              DSFILE.DS_UNITS_BNKRPT_1_1_8,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_8,
              DSFILE.DS_UNITS_BNKRPT_1_1_9,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_9,
              DSFILE.DS_UNITS_BNKRPT_1_1_10,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_10,
              DSFILE.DS_UNITS_BNKRPT_1_2_1,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_1,
              DSFILE.DS_UNITS_BNKRPT_1_2_2,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_2,
              DSFILE.DS_UNITS_BNKRPT_1_2_3,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_3,
              DSFILE.DS_UNITS_BNKRPT_1_2_4,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_4,
              DSFILE.DS_UNITS_BNKRPT_1_2_5,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_5,
              DSFILE.DS_UNITS_BNKRPT_1_2_6,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_6,
              DSFILE.DS_UNITS_BNKRPT_1_2_7,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_7,
              DSFILE.DS_UNITS_BNKRPT_1_2_8,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_8,
              DSFILE.DS_UNITS_BNKRPT_1_2_9,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_9,
              DSFILE.DS_UNITS_BNKRPT_1_2_10,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_10,
              DSFILE.DS_UNITS_BNKRPT_2_1_1,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_1,
              DSFILE.DS_UNITS_BNKRPT_2_1_2,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_2,
              DSFILE.DS_UNITS_BNKRPT_2_1_3,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_3,
              DSFILE.DS_UNITS_BNKRPT_2_1_4,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_4,
              DSFILE.DS_UNITS_BNKRPT_2_1_5,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_5,
              DSFILE.DS_UNITS_BNKRPT_2_1_6,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_6,
              DSFILE.DS_UNITS_BNKRPT_2_1_7,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_7,
              DSFILE.DS_UNITS_BNKRPT_2_1_8,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_8,
              DSFILE.DS_UNITS_BNKRPT_2_1_9,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_9,
              DSFILE.DS_UNITS_BNKRPT_2_1_10,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_10,
              DSFILE.DS_UNITS_BNKRPT_2_2_1,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_1,
              DSFILE.DS_UNITS_BNKRPT_2_2_2,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_2,
              DSFILE.DS_UNITS_BNKRPT_2_2_3,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_3,
              DSFILE.DS_UNITS_BNKRPT_2_2_4,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_4,
              DSFILE.DS_UNITS_BNKRPT_2_2_5,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_5,
              DSFILE.DS_UNITS_BNKRPT_2_2_6,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_6,
              DSFILE.DS_UNITS_BNKRPT_2_2_7,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_7,
              DSFILE.DS_UNITS_BNKRPT_2_2_8,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_8,
              DSFILE.DS_UNITS_BNKRPT_2_2_9,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_9,
              DSFILE.DS_UNITS_BNKRPT_2_2_10,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_10,
              DSFILE.DS_UNITS_BNKRPT_3_1_1,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_1,
              DSFILE.DS_UNITS_BNKRPT_3_1_2,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_2,
              DSFILE.DS_UNITS_BNKRPT_3_1_3,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_3,
              DSFILE.DS_UNITS_BNKRPT_3_1_4,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_4,
              DSFILE.DS_UNITS_BNKRPT_3_1_5,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_5,
              DSFILE.DS_UNITS_BNKRPT_3_1_6,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_6,
              DSFILE.DS_UNITS_BNKRPT_3_1_7,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_7,
              DSFILE.DS_UNITS_BNKRPT_3_1_8,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_8,
              DSFILE.DS_UNITS_BNKRPT_3_1_9,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_9,
              DSFILE.DS_UNITS_BNKRPT_3_1_10,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_10,
              DSFILE.DS_UNITS_BNKRPT_3_2_1,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_1,
              DSFILE.DS_UNITS_BNKRPT_3_2_2,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_2,
              DSFILE.DS_UNITS_BNKRPT_3_2_3,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_3,
              DSFILE.DS_UNITS_BNKRPT_3_2_4,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_4,
              DSFILE.DS_UNITS_BNKRPT_3_2_5,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_5,
              DSFILE.DS_UNITS_BNKRPT_3_2_6,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_6,
              DSFILE.DS_UNITS_BNKRPT_3_2_7,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_7,
              DSFILE.DS_UNITS_BNKRPT_3_2_8,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_8,
              DSFILE.DS_UNITS_BNKRPT_3_2_9,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_9,
              DSFILE.DS_UNITS_BNKRPT_3_2_10,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_10,
              DSFILE.DS_UNEARNED_AMT_1_1_1,
              DSFILE.DS_UNEARNED_AMT_1_1_2,
              DSFILE.DS_UNEARNED_AMT_1_1_3,
              DSFILE.DS_UNEARNED_AMT_1_1_4,
              DSFILE.DS_UNEARNED_AMT_1_1_5,
              DSFILE.DS_UNEARNED_AMT_1_1_6,
              DSFILE.DS_UNEARNED_AMT_1_1_7,
              DSFILE.DS_UNEARNED_AMT_1_1_8,
              DSFILE.DS_UNEARNED_AMT_1_1_9,
              DSFILE.DS_UNEARNED_AMT_1_1_10,
              DSFILE.DS_UNEARNED_AMT_1_2_1,
              DSFILE.DS_UNEARNED_AMT_1_2_2,
              DSFILE.DS_UNEARNED_AMT_1_2_3,
              DSFILE.DS_UNEARNED_AMT_1_2_4,
              DSFILE.DS_UNEARNED_AMT_1_2_5,
              DSFILE.DS_UNEARNED_AMT_1_2_6,
              DSFILE.DS_UNEARNED_AMT_1_2_7,
              DSFILE.DS_UNEARNED_AMT_1_2_8,
              DSFILE.DS_UNEARNED_AMT_1_2_9,
              DSFILE.DS_UNEARNED_AMT_1_2_10,
              DSFILE.DS_UNEARNED_AMT_2_1_1,
              DSFILE.DS_UNEARNED_AMT_2_1_2,
              DSFILE.DS_UNEARNED_AMT_2_1_3,
              DSFILE.DS_UNEARNED_AMT_2_1_4,
              DSFILE.DS_UNEARNED_AMT_2_1_5,
              DSFILE.DS_UNEARNED_AMT_2_1_6,
              DSFILE.DS_UNEARNED_AMT_2_1_7,
              DSFILE.DS_UNEARNED_AMT_2_1_8,
              DSFILE.DS_UNEARNED_AMT_2_1_9,
              DSFILE.DS_UNEARNED_AMT_2_1_10,
              DSFILE.DS_UNEARNED_AMT_2_2_1,
              DSFILE.DS_UNEARNED_AMT_2_2_2,
              DSFILE.DS_UNEARNED_AMT_2_2_3,
              DSFILE.DS_UNEARNED_AMT_2_2_4,
              DSFILE.DS_UNEARNED_AMT_2_2_5,
              DSFILE.DS_UNEARNED_AMT_2_2_6,
              DSFILE.DS_UNEARNED_AMT_2_2_7,
              DSFILE.DS_UNEARNED_AMT_2_2_8,
              DSFILE.DS_UNEARNED_AMT_2_2_9,
              DSFILE.DS_UNEARNED_AMT_2_2_10,
              DSFILE.DS_UNEARNED_AMT_3_1_1,
              DSFILE.DS_UNEARNED_AMT_3_1_2,
              DSFILE.DS_UNEARNED_AMT_3_1_3,
              DSFILE.DS_UNEARNED_AMT_3_1_4,
              DSFILE.DS_UNEARNED_AMT_3_1_5,
              DSFILE.DS_UNEARNED_AMT_3_1_6,
              DSFILE.DS_UNEARNED_AMT_3_1_7,
              DSFILE.DS_UNEARNED_AMT_3_1_8,
              DSFILE.DS_UNEARNED_AMT_3_1_9,
              DSFILE.DS_UNEARNED_AMT_3_1_10,
              DSFILE.DS_UNEARNED_AMT_3_2_1,
              DSFILE.DS_UNEARNED_AMT_3_2_2,
              DSFILE.DS_UNEARNED_AMT_3_2_3,
              DSFILE.DS_UNEARNED_AMT_3_2_4,
              DSFILE.DS_UNEARNED_AMT_3_2_5,
              DSFILE.DS_UNEARNED_AMT_3_2_6,
              DSFILE.DS_UNEARNED_AMT_3_2_7,
              DSFILE.DS_UNEARNED_AMT_3_2_8,
              DSFILE.DS_UNEARNED_AMT_3_2_9,
              DSFILE.DS_UNEARNED_AMT_3_2_10,
              DSFILE.DS_DLDISC_VOL_1,
              DSFILE.DS_DLDISC_VOL_2,
              DSFILE.DS_DLDISC_VOL_3,
              DSFILE.DS_AFPDUEDL_MEMO,
              DSFILE.DS_UNPAID_MANUAL,
              DSFILE.DS_LTOUCH_DATE)
            VALUES
             (:QDS-DATE-CCYYMM,
              :QDS-BRANCH,
              :QDS-NUM,
              :QDS-CLASS,
              :QDS-DLRVPRIN-1,
              :QDS-DLRVPRIN-2,
              :QDS-DLRVPRIN-3,
              :QDS-DLRVCHGS-1,
              :QDS-DLRVCHGS-2,
              :QDS-DLRVCHGS-3,
              :QDS-DLPARVHELD,
              :QDS-UNPAIDCK,
              :QDS-DLDISC,
              :QDS-DLPARVPAID,
              :QDS-SUMWGTTM,
              :QDS-SUMWGTAV,
              :QDS-SUMWGTTA,
              :QDS-RCOURSE-UNITS-1,
              :QDS-RCOURSE-UNITS-2,
              :QDS-RCOURSE-UNITS-3,
              :QDS-RCOURSE-AMT-1,
              :QDS-RCOURSE-AMT-2,
              :QDS-RCOURSE-AMT-3,
              :QDS-NPURUNIT-1,
              :QDS-NPURUNIT-2,
              :QDS-NPURUNIT-3,
              :QDS-NPURAMT-1,
              :QDS-NPURAMT-2,
              :QDS-NPURAMT-3,
              :QDS-OPURUNIT-1,
              :QDS-OPURUNIT-2,
              :QDS-OPURUNIT-3,
              :QDS-OPURAMT-1,
              :QDS-OPURAMT-2,
              :QDS-OPURAMT-3,
              :QDS-RNEWUNIT-1,
              :QDS-RNEWUNIT-2,
              :QDS-RNEWUNIT-3,
              :QDS-RNEWAMT-1,
              :QDS-RNEWAMT-2,
              :QDS-RNEWAMT-3,
              :QDS-PAIDUNIT-1,
              :QDS-PAIDUNIT-2,
              :QDS-PAIDUNIT-3,
              :QDS-PAIDAMT-1,
              :QDS-PAIDAMT-2,
              :QDS-PAIDAMT-3,
              :QDS-TODLUNIT-1,
              :QDS-TODLUNIT-2,
              :QDS-TODLUNIT-3,
              :QDS-TODLAMT-1,
              :QDS-TODLAMT-2,
              :QDS-TODLAMT-3,
              :QDS-OUTSUNIT-1,
              :QDS-OUTSUNIT-2,
              :QDS-OUTSUNIT-3,
              :QDS-OUTSAMT-1,
              :QDS-OUTSAMT-2,
              :QDS-OUTSAMT-3,
              :QDS-PLOSUNIT-1,
              :QDS-PLOSUNIT-2,
              :QDS-PLOSUNIT-3,
              :QDS-PLOSAMT-1,
              :QDS-PLOSAMT-2,
              :QDS-PLOSAMT-3,
              :QDS-RCPLUNIT-1,
              :QDS-RCPLUNIT-2,
              :QDS-RCPLUNIT-3,
              :QDS-RCPLAMT-1,
              :QDS-RCPLAMT-2,
              :QDS-RCPLAMT-3,
              :QDS-JDOSUNIT-1,
              :QDS-JDOSUNIT-2,
              :QDS-JDOSUNIT-3,
              :QDS-JDOSAMT-1,
              :QDS-JDOSAMT-2,
              :QDS-JDOSAMT-3,
              NULLIF(:QDS-DELUNEARN-DATE,'1900-01-01'),
              :QDS-PROCEEDS-1,
              :QDS-PROCEEDS-2,
              :QDS-NO-REPO-1-1-1,
              :QDS-AMT-REPO-1-1-1,
              :QDS-NO-REPO-1-1-2,
              :QDS-AMT-REPO-1-1-2,
              :QDS-NO-REPO-1-1-3,
              :QDS-AMT-REPO-1-1-3,
              :QDS-NO-REPO-1-1-4,
              :QDS-AMT-REPO-1-1-4,
              :QDS-NO-REPO-1-1-5,
              :QDS-AMT-REPO-1-1-5,
              :QDS-NO-REPO-1-2-1,
              :QDS-AMT-REPO-1-2-1,
              :QDS-NO-REPO-1-2-2,
              :QDS-AMT-REPO-1-2-2,
              :QDS-NO-REPO-1-2-3,
              :QDS-AMT-REPO-1-2-3,
              :QDS-NO-REPO-1-2-4,
              :QDS-AMT-REPO-1-2-4,
              :QDS-NO-REPO-1-2-5,
              :QDS-AMT-REPO-1-2-5,
              :QDS-NO-REPO-2-1-1,
              :QDS-AMT-REPO-2-1-1,
              :QDS-NO-REPO-2-1-2,
              :QDS-AMT-REPO-2-1-2,
              :QDS-NO-REPO-2-1-3,
              :QDS-AMT-REPO-2-1-3,
              :QDS-NO-REPO-2-1-4,
              :QDS-AMT-REPO-2-1-4,
              :QDS-NO-REPO-2-1-5,
              :QDS-AMT-REPO-2-1-5,
              :QDS-NO-REPO-2-2-1,
              :QDS-AMT-REPO-2-2-1,
              :QDS-NO-REPO-2-2-2,
              :QDS-AMT-REPO-2-2-2,
              :QDS-NO-REPO-2-2-3,
              :QDS-AMT-REPO-2-2-3,
              :QDS-NO-REPO-2-2-4,
              :QDS-AMT-REPO-2-2-4,
              :QDS-NO-REPO-2-2-5,
              :QDS-AMT-REPO-2-2-5,
              :QDS-NO-REPO-3-1-1,
              :QDS-AMT-REPO-3-1-1,
              :QDS-NO-REPO-3-1-2,
              :QDS-AMT-REPO-3-1-2,
              :QDS-NO-REPO-3-1-3,
              :QDS-AMT-REPO-3-1-3,
              :QDS-NO-REPO-3-1-4,
              :QDS-AMT-REPO-3-1-4,
              :QDS-NO-REPO-3-1-5,
              :QDS-AMT-REPO-3-1-5,
              :QDS-NO-REPO-3-2-1,
              :QDS-AMT-REPO-3-2-1,
              :QDS-NO-REPO-3-2-2,
              :QDS-AMT-REPO-3-2-2,
              :QDS-NO-REPO-3-2-3,
              :QDS-AMT-REPO-3-2-3,
              :QDS-NO-REPO-3-2-4,
              :QDS-AMT-REPO-3-2-4,
              :QDS-NO-REPO-3-2-5,
              :QDS-AMT-REPO-3-2-5,
              :QDS-UNITS-1-1-1,
              :QDS-OUTBAL-1-1-1,
              :QDS-UNITS-1-1-2,
              :QDS-OUTBAL-1-1-2,
              :QDS-UNITS-1-1-3,
              :QDS-OUTBAL-1-1-3,
              :QDS-UNITS-1-1-4,
              :QDS-OUTBAL-1-1-4,
              :QDS-UNITS-1-1-5,
              :QDS-OUTBAL-1-1-5,
              :QDS-UNITS-1-1-6,
              :QDS-OUTBAL-1-1-6,
              :QDS-UNITS-1-1-7,
              :QDS-OUTBAL-1-1-7,
              :QDS-UNITS-1-1-8,
              :QDS-OUTBAL-1-1-8,
              :QDS-UNITS-1-1-9,
              :QDS-OUTBAL-1-1-9,
              :QDS-UNITS-1-1-10,
              :QDS-OUTBAL-1-1-10,
              :QDS-UNITS-1-2-1,
              :QDS-OUTBAL-1-2-1,
              :QDS-UNITS-1-2-2,
              :QDS-OUTBAL-1-2-2,
              :QDS-UNITS-1-2-3,
              :QDS-OUTBAL-1-2-3,
              :QDS-UNITS-1-2-4,
              :QDS-OUTBAL-1-2-4,
              :QDS-UNITS-1-2-5,
              :QDS-OUTBAL-1-2-5,
              :QDS-UNITS-1-2-6,
              :QDS-OUTBAL-1-2-6,
              :QDS-UNITS-1-2-7,
              :QDS-OUTBAL-1-2-7,
              :QDS-UNITS-1-2-8,
              :QDS-OUTBAL-1-2-8,
              :QDS-UNITS-1-2-9,
              :QDS-OUTBAL-1-2-9,
              :QDS-UNITS-1-2-10,
              :QDS-OUTBAL-1-2-10,
              :QDS-UNITS-2-1-1,
              :QDS-OUTBAL-2-1-1,
              :QDS-UNITS-2-1-2,
              :QDS-OUTBAL-2-1-2,
              :QDS-UNITS-2-1-3,
              :QDS-OUTBAL-2-1-3,
              :QDS-UNITS-2-1-4,
              :QDS-OUTBAL-2-1-4,
              :QDS-UNITS-2-1-5,
              :QDS-OUTBAL-2-1-5,
              :QDS-UNITS-2-1-6,
              :QDS-OUTBAL-2-1-6,
              :QDS-UNITS-2-1-7,
              :QDS-OUTBAL-2-1-7,
              :QDS-UNITS-2-1-8,
              :QDS-OUTBAL-2-1-8,
              :QDS-UNITS-2-1-9,
              :QDS-OUTBAL-2-1-9,
              :QDS-UNITS-2-1-10,
              :QDS-OUTBAL-2-1-10,
              :QDS-UNITS-2-2-1,
              :QDS-OUTBAL-2-2-1,
              :QDS-UNITS-2-2-2,
              :QDS-OUTBAL-2-2-2,
              :QDS-UNITS-2-2-3,
              :QDS-OUTBAL-2-2-3,
              :QDS-UNITS-2-2-4,
              :QDS-OUTBAL-2-2-4,
              :QDS-UNITS-2-2-5,
              :QDS-OUTBAL-2-2-5,
              :QDS-UNITS-2-2-6,
              :QDS-OUTBAL-2-2-6,
              :QDS-UNITS-2-2-7,
              :QDS-OUTBAL-2-2-7,
              :QDS-UNITS-2-2-8,
              :QDS-OUTBAL-2-2-8,
              :QDS-UNITS-2-2-9,
              :QDS-OUTBAL-2-2-9,
              :QDS-UNITS-2-2-10,
              :QDS-OUTBAL-2-2-10,
              :QDS-UNITS-3-1-1,
              :QDS-OUTBAL-3-1-1,
              :QDS-UNITS-3-1-2,
              :QDS-OUTBAL-3-1-2,
              :QDS-UNITS-3-1-3,
              :QDS-OUTBAL-3-1-3,
              :QDS-UNITS-3-1-4,
              :QDS-OUTBAL-3-1-4,
              :QDS-UNITS-3-1-5,
              :QDS-OUTBAL-3-1-5,
              :QDS-UNITS-3-1-6,
              :QDS-OUTBAL-3-1-6,
              :QDS-UNITS-3-1-7,
              :QDS-OUTBAL-3-1-7,
              :QDS-UNITS-3-1-8,
              :QDS-OUTBAL-3-1-8,
              :QDS-UNITS-3-1-9,
              :QDS-OUTBAL-3-1-9,
              :QDS-UNITS-3-1-10,
              :QDS-OUTBAL-3-1-10,
              :QDS-UNITS-3-2-1,
              :QDS-OUTBAL-3-2-1,
              :QDS-UNITS-3-2-2,
              :QDS-OUTBAL-3-2-2,
              :QDS-UNITS-3-2-3,
              :QDS-OUTBAL-3-2-3,
              :QDS-UNITS-3-2-4,
              :QDS-OUTBAL-3-2-4,
              :QDS-UNITS-3-2-5,
              :QDS-OUTBAL-3-2-5,
              :QDS-UNITS-3-2-6,
              :QDS-OUTBAL-3-2-6,
              :QDS-UNITS-3-2-7,
              :QDS-OUTBAL-3-2-7,
              :QDS-UNITS-3-2-8,
              :QDS-OUTBAL-3-2-8,
              :QDS-UNITS-3-2-9,
              :QDS-OUTBAL-3-2-9,
              :QDS-UNITS-3-2-10,
              :QDS-OUTBAL-3-2-10,
              :QDS-UNITS-BNKRPT-1-1-1,
              :QDS-OUTBAL-BNKRPT-1-1-1,
              :QDS-UNITS-BNKRPT-1-1-2,
              :QDS-OUTBAL-BNKRPT-1-1-2,
              :QDS-UNITS-BNKRPT-1-1-3,
              :QDS-OUTBAL-BNKRPT-1-1-3,
              :QDS-UNITS-BNKRPT-1-1-4,
              :QDS-OUTBAL-BNKRPT-1-1-4,
              :QDS-UNITS-BNKRPT-1-1-5,
              :QDS-OUTBAL-BNKRPT-1-1-5,
              :QDS-UNITS-BNKRPT-1-1-6,
              :QDS-OUTBAL-BNKRPT-1-1-6,
              :QDS-UNITS-BNKRPT-1-1-7,
              :QDS-OUTBAL-BNKRPT-1-1-7,
              :QDS-UNITS-BNKRPT-1-1-8,
              :QDS-OUTBAL-BNKRPT-1-1-8,
              :QDS-UNITS-BNKRPT-1-1-9,
              :QDS-OUTBAL-BNKRPT-1-1-9,
              :QDS-UNITS-BNKRPT-1-1-10,
              :QDS-OUTBAL-BNKRPT-1-1-10,
              :QDS-UNITS-BNKRPT-1-2-1,
              :QDS-OUTBAL-BNKRPT-1-2-1,
              :QDS-UNITS-BNKRPT-1-2-2,
              :QDS-OUTBAL-BNKRPT-1-2-2,
              :QDS-UNITS-BNKRPT-1-2-3,
              :QDS-OUTBAL-BNKRPT-1-2-3,
              :QDS-UNITS-BNKRPT-1-2-4,
              :QDS-OUTBAL-BNKRPT-1-2-4,
              :QDS-UNITS-BNKRPT-1-2-5,
              :QDS-OUTBAL-BNKRPT-1-2-5,
              :QDS-UNITS-BNKRPT-1-2-6,
              :QDS-OUTBAL-BNKRPT-1-2-6,
              :QDS-UNITS-BNKRPT-1-2-7,
              :QDS-OUTBAL-BNKRPT-1-2-7,
              :QDS-UNITS-BNKRPT-1-2-8,
              :QDS-OUTBAL-BNKRPT-1-2-8,
              :QDS-UNITS-BNKRPT-1-2-9,
              :QDS-OUTBAL-BNKRPT-1-2-9,
              :QDS-UNITS-BNKRPT-1-2-10,
              :QDS-OUTBAL-BNKRPT-1-2-10,
              :QDS-UNITS-BNKRPT-2-1-1,
              :QDS-OUTBAL-BNKRPT-2-1-1,
              :QDS-UNITS-BNKRPT-2-1-2,
              :QDS-OUTBAL-BNKRPT-2-1-2,
              :QDS-UNITS-BNKRPT-2-1-3,
              :QDS-OUTBAL-BNKRPT-2-1-3,
              :QDS-UNITS-BNKRPT-2-1-4,
              :QDS-OUTBAL-BNKRPT-2-1-4,
              :QDS-UNITS-BNKRPT-2-1-5,
              :QDS-OUTBAL-BNKRPT-2-1-5,
              :QDS-UNITS-BNKRPT-2-1-6,
              :QDS-OUTBAL-BNKRPT-2-1-6,
              :QDS-UNITS-BNKRPT-2-1-7,
              :QDS-OUTBAL-BNKRPT-2-1-7,
              :QDS-UNITS-BNKRPT-2-1-8,
              :QDS-OUTBAL-BNKRPT-2-1-8,
              :QDS-UNITS-BNKRPT-2-1-9,
              :QDS-OUTBAL-BNKRPT-2-1-9,
              :QDS-UNITS-BNKRPT-2-1-10,
              :QDS-OUTBAL-BNKRPT-2-1-10,
              :QDS-UNITS-BNKRPT-2-2-1,
              :QDS-OUTBAL-BNKRPT-2-2-1,
              :QDS-UNITS-BNKRPT-2-2-2,
              :QDS-OUTBAL-BNKRPT-2-2-2,
              :QDS-UNITS-BNKRPT-2-2-3,
              :QDS-OUTBAL-BNKRPT-2-2-3,
              :QDS-UNITS-BNKRPT-2-2-4,
              :QDS-OUTBAL-BNKRPT-2-2-4,
              :QDS-UNITS-BNKRPT-2-2-5,
              :QDS-OUTBAL-BNKRPT-2-2-5,
              :QDS-UNITS-BNKRPT-2-2-6,
              :QDS-OUTBAL-BNKRPT-2-2-6,
              :QDS-UNITS-BNKRPT-2-2-7,
              :QDS-OUTBAL-BNKRPT-2-2-7,
              :QDS-UNITS-BNKRPT-2-2-8,
              :QDS-OUTBAL-BNKRPT-2-2-8,
              :QDS-UNITS-BNKRPT-2-2-9,
              :QDS-OUTBAL-BNKRPT-2-2-9,
              :QDS-UNITS-BNKRPT-2-2-10,
              :QDS-OUTBAL-BNKRPT-2-2-10,
              :QDS-UNITS-BNKRPT-3-1-1,
              :QDS-OUTBAL-BNKRPT-3-1-1,
              :QDS-UNITS-BNKRPT-3-1-2,
              :QDS-OUTBAL-BNKRPT-3-1-2,
              :QDS-UNITS-BNKRPT-3-1-3,
              :QDS-OUTBAL-BNKRPT-3-1-3,
              :QDS-UNITS-BNKRPT-3-1-4,
              :QDS-OUTBAL-BNKRPT-3-1-4,
              :QDS-UNITS-BNKRPT-3-1-5,
              :QDS-OUTBAL-BNKRPT-3-1-5,
              :QDS-UNITS-BNKRPT-3-1-6,
              :QDS-OUTBAL-BNKRPT-3-1-6,
              :QDS-UNITS-BNKRPT-3-1-7,
              :QDS-OUTBAL-BNKRPT-3-1-7,
              :QDS-UNITS-BNKRPT-3-1-8,
              :QDS-OUTBAL-BNKRPT-3-1-8,
              :QDS-UNITS-BNKRPT-3-1-9,
              :QDS-OUTBAL-BNKRPT-3-1-9,
              :QDS-UNITS-BNKRPT-3-1-10,
              :QDS-OUTBAL-BNKRPT-3-1-10,
              :QDS-UNITS-BNKRPT-3-2-1,
              :QDS-OUTBAL-BNKRPT-3-2-1,
              :QDS-UNITS-BNKRPT-3-2-2,
              :QDS-OUTBAL-BNKRPT-3-2-2,
              :QDS-UNITS-BNKRPT-3-2-3,
              :QDS-OUTBAL-BNKRPT-3-2-3,
              :QDS-UNITS-BNKRPT-3-2-4,
              :QDS-OUTBAL-BNKRPT-3-2-4,
              :QDS-UNITS-BNKRPT-3-2-5,
              :QDS-OUTBAL-BNKRPT-3-2-5,
              :QDS-UNITS-BNKRPT-3-2-6,
              :QDS-OUTBAL-BNKRPT-3-2-6,
              :QDS-UNITS-BNKRPT-3-2-7,
              :QDS-OUTBAL-BNKRPT-3-2-7,
              :QDS-UNITS-BNKRPT-3-2-8,
              :QDS-OUTBAL-BNKRPT-3-2-8,
              :QDS-UNITS-BNKRPT-3-2-9,
              :QDS-OUTBAL-BNKRPT-3-2-9,
              :QDS-UNITS-BNKRPT-3-2-10,
              :QDS-OUTBAL-BNKRPT-3-2-10,
              :QDS-UNEARNED-AMT-1-1-1,
              :QDS-UNEARNED-AMT-1-1-2,
              :QDS-UNEARNED-AMT-1-1-3,
              :QDS-UNEARNED-AMT-1-1-4,
              :QDS-UNEARNED-AMT-1-1-5,
              :QDS-UNEARNED-AMT-1-1-6,
              :QDS-UNEARNED-AMT-1-1-7,
              :QDS-UNEARNED-AMT-1-1-8,
              :QDS-UNEARNED-AMT-1-1-9,
              :QDS-UNEARNED-AMT-1-1-10,
              :QDS-UNEARNED-AMT-1-2-1,
              :QDS-UNEARNED-AMT-1-2-2,
              :QDS-UNEARNED-AMT-1-2-3,
              :QDS-UNEARNED-AMT-1-2-4,
              :QDS-UNEARNED-AMT-1-2-5,
              :QDS-UNEARNED-AMT-1-2-6,
              :QDS-UNEARNED-AMT-1-2-7,
              :QDS-UNEARNED-AMT-1-2-8,
              :QDS-UNEARNED-AMT-1-2-9,
              :QDS-UNEARNED-AMT-1-2-10,
              :QDS-UNEARNED-AMT-2-1-1,
              :QDS-UNEARNED-AMT-2-1-2,
              :QDS-UNEARNED-AMT-2-1-3,
              :QDS-UNEARNED-AMT-2-1-4,
              :QDS-UNEARNED-AMT-2-1-5,
              :QDS-UNEARNED-AMT-2-1-6,
              :QDS-UNEARNED-AMT-2-1-7,
              :QDS-UNEARNED-AMT-2-1-8,
              :QDS-UNEARNED-AMT-2-1-9,
              :QDS-UNEARNED-AMT-2-1-10,
              :QDS-UNEARNED-AMT-2-2-1,
              :QDS-UNEARNED-AMT-2-2-2,
              :QDS-UNEARNED-AMT-2-2-3,
              :QDS-UNEARNED-AMT-2-2-4,
              :QDS-UNEARNED-AMT-2-2-5,
              :QDS-UNEARNED-AMT-2-2-6,
              :QDS-UNEARNED-AMT-2-2-7,
              :QDS-UNEARNED-AMT-2-2-8,
              :QDS-UNEARNED-AMT-2-2-9,
              :QDS-UNEARNED-AMT-2-2-10,
              :QDS-UNEARNED-AMT-3-1-1,
              :QDS-UNEARNED-AMT-3-1-2,
              :QDS-UNEARNED-AMT-3-1-3,
              :QDS-UNEARNED-AMT-3-1-4,
              :QDS-UNEARNED-AMT-3-1-5,
              :QDS-UNEARNED-AMT-3-1-6,
              :QDS-UNEARNED-AMT-3-1-7,
              :QDS-UNEARNED-AMT-3-1-8,
              :QDS-UNEARNED-AMT-3-1-9,
              :QDS-UNEARNED-AMT-3-1-10,
              :QDS-UNEARNED-AMT-3-2-1,
              :QDS-UNEARNED-AMT-3-2-2,
              :QDS-UNEARNED-AMT-3-2-3,
              :QDS-UNEARNED-AMT-3-2-4,
              :QDS-UNEARNED-AMT-3-2-5,
              :QDS-UNEARNED-AMT-3-2-6,
              :QDS-UNEARNED-AMT-3-2-7,
              :QDS-UNEARNED-AMT-3-2-8,
              :QDS-UNEARNED-AMT-3-2-9,
              :QDS-UNEARNED-AMT-3-2-10,
              :QDS-DLDISC-VOL-1,
              :QDS-DLDISC-VOL-2,
              :QDS-DLDISC-VOL-3,
              :QDS-AFPDUEDL-MEMO,
              :QDS-UNPAID-MANUAL,
              NULLIF(:QDS-LTOUCH-DATE,'1900-01-01'))
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-DS1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-DS1-FILE.

           MOVE DS-PATH-OWNBR  TO DS-BRANCH.
           ACCEPT DS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE DS-SELECT-PATH TO E-FILE.
           MOVE DS1-KEY        TO E-KEYX.
           PERFORM SET-DS-FIELDS.

           EXEC SQL
            UPDATE DBO.DSFILE
             SET
              DSFILE.DS_DATE_CCYYMM = :QDS-DATE-CCYYMM,
              DSFILE.DS_BRANCH = :QDS-BRANCH,
              DSFILE.DS_NUM = :QDS-NUM,
              DSFILE.DS_CLASS = :QDS-CLASS,
              DSFILE.DS_DLRVPRIN_1 = :QDS-DLRVPRIN-1,
              DSFILE.DS_DLRVPRIN_2 = :QDS-DLRVPRIN-2,
              DSFILE.DS_DLRVPRIN_3 = :QDS-DLRVPRIN-3,
              DSFILE.DS_DLRVCHGS_1 = :QDS-DLRVCHGS-1,
              DSFILE.DS_DLRVCHGS_2 = :QDS-DLRVCHGS-2,
              DSFILE.DS_DLRVCHGS_3 = :QDS-DLRVCHGS-3,
              DSFILE.DS_DLPARVHELD = :QDS-DLPARVHELD,
              DSFILE.DS_UNPAIDCK = :QDS-UNPAIDCK,
              DSFILE.DS_DLDISC = :QDS-DLDISC,
              DSFILE.DS_DLPARVPAID = :QDS-DLPARVPAID,
              DSFILE.DS_SUMWGTTM = :QDS-SUMWGTTM,
              DSFILE.DS_SUMWGTAV = :QDS-SUMWGTAV,
              DSFILE.DS_SUMWGTTA = :QDS-SUMWGTTA,
              DSFILE.DS_RCOURSE_UNITS_1 = :QDS-RCOURSE-UNITS-1,
              DSFILE.DS_RCOURSE_UNITS_2 = :QDS-RCOURSE-UNITS-2,
              DSFILE.DS_RCOURSE_UNITS_3 = :QDS-RCOURSE-UNITS-3,
              DSFILE.DS_RCOURSE_AMT_1 = :QDS-RCOURSE-AMT-1,
              DSFILE.DS_RCOURSE_AMT_2 = :QDS-RCOURSE-AMT-2,
              DSFILE.DS_RCOURSE_AMT_3 = :QDS-RCOURSE-AMT-3,
              DSFILE.DS_NPURUNIT_1 = :QDS-NPURUNIT-1,
              DSFILE.DS_NPURUNIT_2 = :QDS-NPURUNIT-2,
              DSFILE.DS_NPURUNIT_3 = :QDS-NPURUNIT-3,
              DSFILE.DS_NPURAMT_1 = :QDS-NPURAMT-1,
              DSFILE.DS_NPURAMT_2 = :QDS-NPURAMT-2,
              DSFILE.DS_NPURAMT_3 = :QDS-NPURAMT-3,
              DSFILE.DS_OPURUNIT_1 = :QDS-OPURUNIT-1,
              DSFILE.DS_OPURUNIT_2 = :QDS-OPURUNIT-2,
              DSFILE.DS_OPURUNIT_3 = :QDS-OPURUNIT-3,
              DSFILE.DS_OPURAMT_1 = :QDS-OPURAMT-1,
              DSFILE.DS_OPURAMT_2 = :QDS-OPURAMT-2,
              DSFILE.DS_OPURAMT_3 = :QDS-OPURAMT-3,
              DSFILE.DS_RNEWUNIT_1 = :QDS-RNEWUNIT-1,
              DSFILE.DS_RNEWUNIT_2 = :QDS-RNEWUNIT-2,
              DSFILE.DS_RNEWUNIT_3 = :QDS-RNEWUNIT-3,
              DSFILE.DS_RNEWAMT_1 = :QDS-RNEWAMT-1,
              DSFILE.DS_RNEWAMT_2 = :QDS-RNEWAMT-2,
              DSFILE.DS_RNEWAMT_3 = :QDS-RNEWAMT-3,
              DSFILE.DS_PAIDUNIT_1 = :QDS-PAIDUNIT-1,
              DSFILE.DS_PAIDUNIT_2 = :QDS-PAIDUNIT-2,
              DSFILE.DS_PAIDUNIT_3 = :QDS-PAIDUNIT-3,
              DSFILE.DS_PAIDAMT_1 = :QDS-PAIDAMT-1,
              DSFILE.DS_PAIDAMT_2 = :QDS-PAIDAMT-2,
              DSFILE.DS_PAIDAMT_3 = :QDS-PAIDAMT-3,
              DSFILE.DS_TODLUNIT_1 = :QDS-TODLUNIT-1,
              DSFILE.DS_TODLUNIT_2 = :QDS-TODLUNIT-2,
              DSFILE.DS_TODLUNIT_3 = :QDS-TODLUNIT-3,
              DSFILE.DS_TODLAMT_1 = :QDS-TODLAMT-1,
              DSFILE.DS_TODLAMT_2 = :QDS-TODLAMT-2,
              DSFILE.DS_TODLAMT_3 = :QDS-TODLAMT-3,
              DSFILE.DS_OUTSUNIT_1 = :QDS-OUTSUNIT-1,
              DSFILE.DS_OUTSUNIT_2 = :QDS-OUTSUNIT-2,
              DSFILE.DS_OUTSUNIT_3 = :QDS-OUTSUNIT-3,
              DSFILE.DS_OUTSAMT_1 = :QDS-OUTSAMT-1,
              DSFILE.DS_OUTSAMT_2 = :QDS-OUTSAMT-2,
              DSFILE.DS_OUTSAMT_3 = :QDS-OUTSAMT-3,
              DSFILE.DS_PLOSUNIT_1 = :QDS-PLOSUNIT-1,
              DSFILE.DS_PLOSUNIT_2 = :QDS-PLOSUNIT-2,
              DSFILE.DS_PLOSUNIT_3 = :QDS-PLOSUNIT-3,
              DSFILE.DS_PLOSAMT_1 = :QDS-PLOSAMT-1,
              DSFILE.DS_PLOSAMT_2 = :QDS-PLOSAMT-2,
              DSFILE.DS_PLOSAMT_3 = :QDS-PLOSAMT-3,
              DSFILE.DS_RCPLUNIT_1 = :QDS-RCPLUNIT-1,
              DSFILE.DS_RCPLUNIT_2 = :QDS-RCPLUNIT-2,
              DSFILE.DS_RCPLUNIT_3 = :QDS-RCPLUNIT-3,
              DSFILE.DS_RCPLAMT_1 = :QDS-RCPLAMT-1,
              DSFILE.DS_RCPLAMT_2 = :QDS-RCPLAMT-2,
              DSFILE.DS_RCPLAMT_3 = :QDS-RCPLAMT-3,
              DSFILE.DS_JDOSUNIT_1 = :QDS-JDOSUNIT-1,
              DSFILE.DS_JDOSUNIT_2 = :QDS-JDOSUNIT-2,
              DSFILE.DS_JDOSUNIT_3 = :QDS-JDOSUNIT-3,
              DSFILE.DS_JDOSAMT_1 = :QDS-JDOSAMT-1,
              DSFILE.DS_JDOSAMT_2 = :QDS-JDOSAMT-2,
              DSFILE.DS_JDOSAMT_3 = :QDS-JDOSAMT-3,
              DSFILE.DS_DELUNEARN_DATE =
                 NULLIF(:QDS-DELUNEARN-DATE,'1900-01-01'),
              DSFILE.DS_PROCEEDS_1 = :QDS-PROCEEDS-1,
              DSFILE.DS_PROCEEDS_2 = :QDS-PROCEEDS-2,
              DSFILE.DS_NO_REPO_1_1_1 = :QDS-NO-REPO-1-1-1,
              DSFILE.DS_AMT_REPO_1_1_1 = :QDS-AMT-REPO-1-1-1,
              DSFILE.DS_NO_REPO_1_1_2 = :QDS-NO-REPO-1-1-2,
              DSFILE.DS_AMT_REPO_1_1_2 = :QDS-AMT-REPO-1-1-2,
              DSFILE.DS_NO_REPO_1_1_3 = :QDS-NO-REPO-1-1-3,
              DSFILE.DS_AMT_REPO_1_1_3 = :QDS-AMT-REPO-1-1-3,
              DSFILE.DS_NO_REPO_1_1_4 = :QDS-NO-REPO-1-1-4,
              DSFILE.DS_AMT_REPO_1_1_4 = :QDS-AMT-REPO-1-1-4,
              DSFILE.DS_NO_REPO_1_1_5 = :QDS-NO-REPO-1-1-5,
              DSFILE.DS_AMT_REPO_1_1_5 = :QDS-AMT-REPO-1-1-5,
              DSFILE.DS_NO_REPO_1_2_1 = :QDS-NO-REPO-1-2-1,
              DSFILE.DS_AMT_REPO_1_2_1 = :QDS-AMT-REPO-1-2-1,
              DSFILE.DS_NO_REPO_1_2_2 = :QDS-NO-REPO-1-2-2,
              DSFILE.DS_AMT_REPO_1_2_2 = :QDS-AMT-REPO-1-2-2,
              DSFILE.DS_NO_REPO_1_2_3 = :QDS-NO-REPO-1-2-3,
              DSFILE.DS_AMT_REPO_1_2_3 = :QDS-AMT-REPO-1-2-3,
              DSFILE.DS_NO_REPO_1_2_4 = :QDS-NO-REPO-1-2-4,
              DSFILE.DS_AMT_REPO_1_2_4 = :QDS-AMT-REPO-1-2-4,
              DSFILE.DS_NO_REPO_1_2_5 = :QDS-NO-REPO-1-2-5,
              DSFILE.DS_AMT_REPO_1_2_5 = :QDS-AMT-REPO-1-2-5,
              DSFILE.DS_NO_REPO_2_1_1 = :QDS-NO-REPO-2-1-1,
              DSFILE.DS_AMT_REPO_2_1_1 = :QDS-AMT-REPO-2-1-1,
              DSFILE.DS_NO_REPO_2_1_2 = :QDS-NO-REPO-2-1-2,
              DSFILE.DS_AMT_REPO_2_1_2 = :QDS-AMT-REPO-2-1-2,
              DSFILE.DS_NO_REPO_2_1_3 = :QDS-NO-REPO-2-1-3,
              DSFILE.DS_AMT_REPO_2_1_3 = :QDS-AMT-REPO-2-1-3,
              DSFILE.DS_NO_REPO_2_1_4 = :QDS-NO-REPO-2-1-4,
              DSFILE.DS_AMT_REPO_2_1_4 = :QDS-AMT-REPO-2-1-4,
              DSFILE.DS_NO_REPO_2_1_5 = :QDS-NO-REPO-2-1-5,
              DSFILE.DS_AMT_REPO_2_1_5 = :QDS-AMT-REPO-2-1-5,
              DSFILE.DS_NO_REPO_2_2_1 = :QDS-NO-REPO-2-2-1,
              DSFILE.DS_AMT_REPO_2_2_1 = :QDS-AMT-REPO-2-2-1,
              DSFILE.DS_NO_REPO_2_2_2 = :QDS-NO-REPO-2-2-2,
              DSFILE.DS_AMT_REPO_2_2_2 = :QDS-AMT-REPO-2-2-2,
              DSFILE.DS_NO_REPO_2_2_3 = :QDS-NO-REPO-2-2-3,
              DSFILE.DS_AMT_REPO_2_2_3 = :QDS-AMT-REPO-2-2-3,
              DSFILE.DS_NO_REPO_2_2_4 = :QDS-NO-REPO-2-2-4,
              DSFILE.DS_AMT_REPO_2_2_4 = :QDS-AMT-REPO-2-2-4,
              DSFILE.DS_NO_REPO_2_2_5 = :QDS-NO-REPO-2-2-5,
              DSFILE.DS_AMT_REPO_2_2_5 = :QDS-AMT-REPO-2-2-5,
              DSFILE.DS_NO_REPO_3_1_1 = :QDS-NO-REPO-3-1-1,
              DSFILE.DS_AMT_REPO_3_1_1 = :QDS-AMT-REPO-3-1-1,
              DSFILE.DS_NO_REPO_3_1_2 = :QDS-NO-REPO-3-1-2,
              DSFILE.DS_AMT_REPO_3_1_2 = :QDS-AMT-REPO-3-1-2,
              DSFILE.DS_NO_REPO_3_1_3 = :QDS-NO-REPO-3-1-3,
              DSFILE.DS_AMT_REPO_3_1_3 = :QDS-AMT-REPO-3-1-3,
              DSFILE.DS_NO_REPO_3_1_4 = :QDS-NO-REPO-3-1-4,
              DSFILE.DS_AMT_REPO_3_1_4 = :QDS-AMT-REPO-3-1-4,
              DSFILE.DS_NO_REPO_3_1_5 = :QDS-NO-REPO-3-1-5,
              DSFILE.DS_AMT_REPO_3_1_5 = :QDS-AMT-REPO-3-1-5,
              DSFILE.DS_NO_REPO_3_2_1 = :QDS-NO-REPO-3-2-1,
              DSFILE.DS_AMT_REPO_3_2_1 = :QDS-AMT-REPO-3-2-1,
              DSFILE.DS_NO_REPO_3_2_2 = :QDS-NO-REPO-3-2-2,
              DSFILE.DS_AMT_REPO_3_2_2 = :QDS-AMT-REPO-3-2-2,
              DSFILE.DS_NO_REPO_3_2_3 = :QDS-NO-REPO-3-2-3,
              DSFILE.DS_AMT_REPO_3_2_3 = :QDS-AMT-REPO-3-2-3,
              DSFILE.DS_NO_REPO_3_2_4 = :QDS-NO-REPO-3-2-4,
              DSFILE.DS_AMT_REPO_3_2_4 = :QDS-AMT-REPO-3-2-4,
              DSFILE.DS_NO_REPO_3_2_5 = :QDS-NO-REPO-3-2-5,
              DSFILE.DS_AMT_REPO_3_2_5 = :QDS-AMT-REPO-3-2-5,
              DSFILE.DS_UNITS_1_1_1 = :QDS-UNITS-1-1-1,
              DSFILE.DS_OUTBAL_1_1_1 = :QDS-OUTBAL-1-1-1,
              DSFILE.DS_UNITS_1_1_2 = :QDS-UNITS-1-1-2,
              DSFILE.DS_OUTBAL_1_1_2 = :QDS-OUTBAL-1-1-2,
              DSFILE.DS_UNITS_1_1_3 = :QDS-UNITS-1-1-3,
              DSFILE.DS_OUTBAL_1_1_3 = :QDS-OUTBAL-1-1-3,
              DSFILE.DS_UNITS_1_1_4 = :QDS-UNITS-1-1-4,
              DSFILE.DS_OUTBAL_1_1_4 = :QDS-OUTBAL-1-1-4,
              DSFILE.DS_UNITS_1_1_5 = :QDS-UNITS-1-1-5,
              DSFILE.DS_OUTBAL_1_1_5 = :QDS-OUTBAL-1-1-5,
              DSFILE.DS_UNITS_1_1_6 = :QDS-UNITS-1-1-6,
              DSFILE.DS_OUTBAL_1_1_6 = :QDS-OUTBAL-1-1-6,
              DSFILE.DS_UNITS_1_1_7 = :QDS-UNITS-1-1-7,
              DSFILE.DS_OUTBAL_1_1_7 = :QDS-OUTBAL-1-1-7,
              DSFILE.DS_UNITS_1_1_8 = :QDS-UNITS-1-1-8,
              DSFILE.DS_OUTBAL_1_1_8 = :QDS-OUTBAL-1-1-8,
              DSFILE.DS_UNITS_1_1_9 = :QDS-UNITS-1-1-9,
              DSFILE.DS_OUTBAL_1_1_9 = :QDS-OUTBAL-1-1-9,
              DSFILE.DS_UNITS_1_1_10 = :QDS-UNITS-1-1-10,
              DSFILE.DS_OUTBAL_1_1_10 = :QDS-OUTBAL-1-1-10,
              DSFILE.DS_UNITS_1_2_1 = :QDS-UNITS-1-2-1,
              DSFILE.DS_OUTBAL_1_2_1 = :QDS-OUTBAL-1-2-1,
              DSFILE.DS_UNITS_1_2_2 = :QDS-UNITS-1-2-2,
              DSFILE.DS_OUTBAL_1_2_2 = :QDS-OUTBAL-1-2-2,
              DSFILE.DS_UNITS_1_2_3 = :QDS-UNITS-1-2-3,
              DSFILE.DS_OUTBAL_1_2_3 = :QDS-OUTBAL-1-2-3,
              DSFILE.DS_UNITS_1_2_4 = :QDS-UNITS-1-2-4,
              DSFILE.DS_OUTBAL_1_2_4 = :QDS-OUTBAL-1-2-4,
              DSFILE.DS_UNITS_1_2_5 = :QDS-UNITS-1-2-5,
              DSFILE.DS_OUTBAL_1_2_5 = :QDS-OUTBAL-1-2-5,
              DSFILE.DS_UNITS_1_2_6 = :QDS-UNITS-1-2-6,
              DSFILE.DS_OUTBAL_1_2_6 = :QDS-OUTBAL-1-2-6,
              DSFILE.DS_UNITS_1_2_7 = :QDS-UNITS-1-2-7,
              DSFILE.DS_OUTBAL_1_2_7 = :QDS-OUTBAL-1-2-7,
              DSFILE.DS_UNITS_1_2_8 = :QDS-UNITS-1-2-8,
              DSFILE.DS_OUTBAL_1_2_8 = :QDS-OUTBAL-1-2-8,
              DSFILE.DS_UNITS_1_2_9 = :QDS-UNITS-1-2-9,
              DSFILE.DS_OUTBAL_1_2_9 = :QDS-OUTBAL-1-2-9,
              DSFILE.DS_UNITS_1_2_10 = :QDS-UNITS-1-2-10,
              DSFILE.DS_OUTBAL_1_2_10 = :QDS-OUTBAL-1-2-10,
              DSFILE.DS_UNITS_2_1_1 = :QDS-UNITS-2-1-1,
              DSFILE.DS_OUTBAL_2_1_1 = :QDS-OUTBAL-2-1-1,
              DSFILE.DS_UNITS_2_1_2 = :QDS-UNITS-2-1-2,
              DSFILE.DS_OUTBAL_2_1_2 = :QDS-OUTBAL-2-1-2,
              DSFILE.DS_UNITS_2_1_3 = :QDS-UNITS-2-1-3,
              DSFILE.DS_OUTBAL_2_1_3 = :QDS-OUTBAL-2-1-3,
              DSFILE.DS_UNITS_2_1_4 = :QDS-UNITS-2-1-4,
              DSFILE.DS_OUTBAL_2_1_4 = :QDS-OUTBAL-2-1-4,
              DSFILE.DS_UNITS_2_1_5 = :QDS-UNITS-2-1-5,
              DSFILE.DS_OUTBAL_2_1_5 = :QDS-OUTBAL-2-1-5,
              DSFILE.DS_UNITS_2_1_6 = :QDS-UNITS-2-1-6,
              DSFILE.DS_OUTBAL_2_1_6 = :QDS-OUTBAL-2-1-6,
              DSFILE.DS_UNITS_2_1_7 = :QDS-UNITS-2-1-7,
              DSFILE.DS_OUTBAL_2_1_7 = :QDS-OUTBAL-2-1-7,
              DSFILE.DS_UNITS_2_1_8 = :QDS-UNITS-2-1-8,
              DSFILE.DS_OUTBAL_2_1_8 = :QDS-OUTBAL-2-1-8,
              DSFILE.DS_UNITS_2_1_9 = :QDS-UNITS-2-1-9,
              DSFILE.DS_OUTBAL_2_1_9 = :QDS-OUTBAL-2-1-9,
              DSFILE.DS_UNITS_2_1_10 = :QDS-UNITS-2-1-10,
              DSFILE.DS_OUTBAL_2_1_10 = :QDS-OUTBAL-2-1-10,
              DSFILE.DS_UNITS_2_2_1 = :QDS-UNITS-2-2-1,
              DSFILE.DS_OUTBAL_2_2_1 = :QDS-OUTBAL-2-2-1,
              DSFILE.DS_UNITS_2_2_2 = :QDS-UNITS-2-2-2,
              DSFILE.DS_OUTBAL_2_2_2 = :QDS-OUTBAL-2-2-2,
              DSFILE.DS_UNITS_2_2_3 = :QDS-UNITS-2-2-3,
              DSFILE.DS_OUTBAL_2_2_3 = :QDS-OUTBAL-2-2-3,
              DSFILE.DS_UNITS_2_2_4 = :QDS-UNITS-2-2-4,
              DSFILE.DS_OUTBAL_2_2_4 = :QDS-OUTBAL-2-2-4,
              DSFILE.DS_UNITS_2_2_5 = :QDS-UNITS-2-2-5,
              DSFILE.DS_OUTBAL_2_2_5 = :QDS-OUTBAL-2-2-5,
              DSFILE.DS_UNITS_2_2_6 = :QDS-UNITS-2-2-6,
              DSFILE.DS_OUTBAL_2_2_6 = :QDS-OUTBAL-2-2-6,
              DSFILE.DS_UNITS_2_2_7 = :QDS-UNITS-2-2-7,
              DSFILE.DS_OUTBAL_2_2_7 = :QDS-OUTBAL-2-2-7,
              DSFILE.DS_UNITS_2_2_8 = :QDS-UNITS-2-2-8,
              DSFILE.DS_OUTBAL_2_2_8 = :QDS-OUTBAL-2-2-8,
              DSFILE.DS_UNITS_2_2_9 = :QDS-UNITS-2-2-9,
              DSFILE.DS_OUTBAL_2_2_9 = :QDS-OUTBAL-2-2-9,
              DSFILE.DS_UNITS_2_2_10 = :QDS-UNITS-2-2-10,
              DSFILE.DS_OUTBAL_2_2_10 = :QDS-OUTBAL-2-2-10,
              DSFILE.DS_UNITS_3_1_1 = :QDS-UNITS-3-1-1,
              DSFILE.DS_OUTBAL_3_1_1 = :QDS-OUTBAL-3-1-1,
              DSFILE.DS_UNITS_3_1_2 = :QDS-UNITS-3-1-2,
              DSFILE.DS_OUTBAL_3_1_2 = :QDS-OUTBAL-3-1-2,
              DSFILE.DS_UNITS_3_1_3 = :QDS-UNITS-3-1-3,
              DSFILE.DS_OUTBAL_3_1_3 = :QDS-OUTBAL-3-1-3,
              DSFILE.DS_UNITS_3_1_4 = :QDS-UNITS-3-1-4,
              DSFILE.DS_OUTBAL_3_1_4 = :QDS-OUTBAL-3-1-4,
              DSFILE.DS_UNITS_3_1_5 = :QDS-UNITS-3-1-5,
              DSFILE.DS_OUTBAL_3_1_5 = :QDS-OUTBAL-3-1-5,
              DSFILE.DS_UNITS_3_1_6 = :QDS-UNITS-3-1-6,
              DSFILE.DS_OUTBAL_3_1_6 = :QDS-OUTBAL-3-1-6,
              DSFILE.DS_UNITS_3_1_7 = :QDS-UNITS-3-1-7,
              DSFILE.DS_OUTBAL_3_1_7 = :QDS-OUTBAL-3-1-7,
              DSFILE.DS_UNITS_3_1_8 = :QDS-UNITS-3-1-8,
              DSFILE.DS_OUTBAL_3_1_8 = :QDS-OUTBAL-3-1-8,
              DSFILE.DS_UNITS_3_1_9 = :QDS-UNITS-3-1-9,
              DSFILE.DS_OUTBAL_3_1_9 = :QDS-OUTBAL-3-1-9,
              DSFILE.DS_UNITS_3_1_10 = :QDS-UNITS-3-1-10,
              DSFILE.DS_OUTBAL_3_1_10 = :QDS-OUTBAL-3-1-10,
              DSFILE.DS_UNITS_3_2_1 = :QDS-UNITS-3-2-1,
              DSFILE.DS_OUTBAL_3_2_1 = :QDS-OUTBAL-3-2-1,
              DSFILE.DS_UNITS_3_2_2 = :QDS-UNITS-3-2-2,
              DSFILE.DS_OUTBAL_3_2_2 = :QDS-OUTBAL-3-2-2,
              DSFILE.DS_UNITS_3_2_3 = :QDS-UNITS-3-2-3,
              DSFILE.DS_OUTBAL_3_2_3 = :QDS-OUTBAL-3-2-3,
              DSFILE.DS_UNITS_3_2_4 = :QDS-UNITS-3-2-4,
              DSFILE.DS_OUTBAL_3_2_4 = :QDS-OUTBAL-3-2-4,
              DSFILE.DS_UNITS_3_2_5 = :QDS-UNITS-3-2-5,
              DSFILE.DS_OUTBAL_3_2_5 = :QDS-OUTBAL-3-2-5,
              DSFILE.DS_UNITS_3_2_6 = :QDS-UNITS-3-2-6,
              DSFILE.DS_OUTBAL_3_2_6 = :QDS-OUTBAL-3-2-6,
              DSFILE.DS_UNITS_3_2_7 = :QDS-UNITS-3-2-7,
              DSFILE.DS_OUTBAL_3_2_7 = :QDS-OUTBAL-3-2-7,
              DSFILE.DS_UNITS_3_2_8 = :QDS-UNITS-3-2-8,
              DSFILE.DS_OUTBAL_3_2_8 = :QDS-OUTBAL-3-2-8,
              DSFILE.DS_UNITS_3_2_9 = :QDS-UNITS-3-2-9,
              DSFILE.DS_OUTBAL_3_2_9 = :QDS-OUTBAL-3-2-9,
              DSFILE.DS_UNITS_3_2_10 = :QDS-UNITS-3-2-10,
              DSFILE.DS_OUTBAL_3_2_10 = :QDS-OUTBAL-3-2-10,
              DSFILE.DS_UNITS_BNKRPT_1_1_1 =
                   :QDS-UNITS-BNKRPT-1-1-1,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_1 =
                   :QDS-OUTBAL-BNKRPT-1-1-1,
              DSFILE.DS_UNITS_BNKRPT_1_1_2 =
                   :QDS-UNITS-BNKRPT-1-1-2,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_2 =
                   :QDS-OUTBAL-BNKRPT-1-1-2,
              DSFILE.DS_UNITS_BNKRPT_1_1_3 =
                   :QDS-UNITS-BNKRPT-1-1-3,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_3 =
                   :QDS-OUTBAL-BNKRPT-1-1-3,
              DSFILE.DS_UNITS_BNKRPT_1_1_4 =
                   :QDS-UNITS-BNKRPT-1-1-4,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_4 =
                   :QDS-OUTBAL-BNKRPT-1-1-4,
              DSFILE.DS_UNITS_BNKRPT_1_1_5 =
                   :QDS-UNITS-BNKRPT-1-1-5,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_5 =
                   :QDS-OUTBAL-BNKRPT-1-1-5,
              DSFILE.DS_UNITS_BNKRPT_1_1_6 =
                   :QDS-UNITS-BNKRPT-1-1-6,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_6 =
                   :QDS-OUTBAL-BNKRPT-1-1-6,
              DSFILE.DS_UNITS_BNKRPT_1_1_7 =
                   :QDS-UNITS-BNKRPT-1-1-7,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_7 =
                   :QDS-OUTBAL-BNKRPT-1-1-7,
              DSFILE.DS_UNITS_BNKRPT_1_1_8 =
                   :QDS-UNITS-BNKRPT-1-1-8,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_8 =
                   :QDS-OUTBAL-BNKRPT-1-1-8,
              DSFILE.DS_UNITS_BNKRPT_1_1_9 =
                   :QDS-UNITS-BNKRPT-1-1-9,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_9 =
                   :QDS-OUTBAL-BNKRPT-1-1-9,
              DSFILE.DS_UNITS_BNKRPT_1_1_10 =
                   :QDS-UNITS-BNKRPT-1-1-10,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_10 =
                   :QDS-OUTBAL-BNKRPT-1-1-10,
              DSFILE.DS_UNITS_BNKRPT_1_2_1 =
                   :QDS-UNITS-BNKRPT-1-2-1,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_1 =
                   :QDS-OUTBAL-BNKRPT-1-2-1,
              DSFILE.DS_UNITS_BNKRPT_1_2_2 =
                   :QDS-UNITS-BNKRPT-1-2-2,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_2 =
                   :QDS-OUTBAL-BNKRPT-1-2-2,
              DSFILE.DS_UNITS_BNKRPT_1_2_3 =
                   :QDS-UNITS-BNKRPT-1-2-3,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_3 =
                   :QDS-OUTBAL-BNKRPT-1-2-3,
              DSFILE.DS_UNITS_BNKRPT_1_2_4 =
                   :QDS-UNITS-BNKRPT-1-2-4,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_4 =
                   :QDS-OUTBAL-BNKRPT-1-2-4,
              DSFILE.DS_UNITS_BNKRPT_1_2_5 =
                   :QDS-UNITS-BNKRPT-1-2-5,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_5 =
                   :QDS-OUTBAL-BNKRPT-1-2-5,
              DSFILE.DS_UNITS_BNKRPT_1_2_6 =
                   :QDS-UNITS-BNKRPT-1-2-6,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_6 =
                   :QDS-OUTBAL-BNKRPT-1-2-6,
              DSFILE.DS_UNITS_BNKRPT_1_2_7 =
                   :QDS-UNITS-BNKRPT-1-2-7,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_7 =
                   :QDS-OUTBAL-BNKRPT-1-2-7,
              DSFILE.DS_UNITS_BNKRPT_1_2_8 =
                   :QDS-UNITS-BNKRPT-1-2-8,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_8 =
                   :QDS-OUTBAL-BNKRPT-1-2-8,
              DSFILE.DS_UNITS_BNKRPT_1_2_9 =
                   :QDS-UNITS-BNKRPT-1-2-9,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_9 =
                   :QDS-OUTBAL-BNKRPT-1-2-9,
              DSFILE.DS_UNITS_BNKRPT_1_2_10 =
                   :QDS-UNITS-BNKRPT-1-2-10,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_10 =
                   :QDS-OUTBAL-BNKRPT-1-2-10,
              DSFILE.DS_UNITS_BNKRPT_2_1_1 =
                   :QDS-UNITS-BNKRPT-2-1-1,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_1 =
                   :QDS-OUTBAL-BNKRPT-2-1-1,
              DSFILE.DS_UNITS_BNKRPT_2_1_2 =
                   :QDS-UNITS-BNKRPT-2-1-2,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_2 =
                   :QDS-OUTBAL-BNKRPT-2-1-2,
              DSFILE.DS_UNITS_BNKRPT_2_1_3 =
                   :QDS-UNITS-BNKRPT-2-1-3,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_3 =
                   :QDS-OUTBAL-BNKRPT-2-1-3,
              DSFILE.DS_UNITS_BNKRPT_2_1_4 =
                   :QDS-UNITS-BNKRPT-2-1-4,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_4 =
                   :QDS-OUTBAL-BNKRPT-2-1-4,
              DSFILE.DS_UNITS_BNKRPT_2_1_5 =
                   :QDS-UNITS-BNKRPT-2-1-5,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_5 =
                   :QDS-OUTBAL-BNKRPT-2-1-5,
              DSFILE.DS_UNITS_BNKRPT_2_1_6 =
                   :QDS-UNITS-BNKRPT-2-1-6,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_6 =
                   :QDS-OUTBAL-BNKRPT-2-1-6,
              DSFILE.DS_UNITS_BNKRPT_2_1_7 =
                   :QDS-UNITS-BNKRPT-2-1-7,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_7 =
                   :QDS-OUTBAL-BNKRPT-2-1-7,
              DSFILE.DS_UNITS_BNKRPT_2_1_8 =
                   :QDS-UNITS-BNKRPT-2-1-8,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_8 =
                   :QDS-OUTBAL-BNKRPT-2-1-8,
              DSFILE.DS_UNITS_BNKRPT_2_1_9 =
                   :QDS-UNITS-BNKRPT-2-1-9,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_9 =
                   :QDS-OUTBAL-BNKRPT-2-1-9,
              DSFILE.DS_UNITS_BNKRPT_2_1_10 =
                   :QDS-UNITS-BNKRPT-2-1-10,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_10 =
                   :QDS-OUTBAL-BNKRPT-2-1-10,
              DSFILE.DS_UNITS_BNKRPT_2_2_1 =
                   :QDS-UNITS-BNKRPT-2-2-1,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_1 =
                   :QDS-OUTBAL-BNKRPT-2-2-1,
              DSFILE.DS_UNITS_BNKRPT_2_2_2 =
                   :QDS-UNITS-BNKRPT-2-2-2,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_2 =
                   :QDS-OUTBAL-BNKRPT-2-2-2,
              DSFILE.DS_UNITS_BNKRPT_2_2_3 =
                   :QDS-UNITS-BNKRPT-2-2-3,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_3 =
                   :QDS-OUTBAL-BNKRPT-2-2-3,
              DSFILE.DS_UNITS_BNKRPT_2_2_4 =
                   :QDS-UNITS-BNKRPT-2-2-4,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_4 =
                   :QDS-OUTBAL-BNKRPT-2-2-4,
              DSFILE.DS_UNITS_BNKRPT_2_2_5 =
                   :QDS-UNITS-BNKRPT-2-2-5,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_5 =
                   :QDS-OUTBAL-BNKRPT-2-2-5,
              DSFILE.DS_UNITS_BNKRPT_2_2_6 =
                   :QDS-UNITS-BNKRPT-2-2-6,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_6 =
                   :QDS-OUTBAL-BNKRPT-2-2-6,
              DSFILE.DS_UNITS_BNKRPT_2_2_7 =
                   :QDS-UNITS-BNKRPT-2-2-7,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_7 =
                   :QDS-OUTBAL-BNKRPT-2-2-7,
              DSFILE.DS_UNITS_BNKRPT_2_2_8 =
                   :QDS-UNITS-BNKRPT-2-2-8,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_8 =
                   :QDS-OUTBAL-BNKRPT-2-2-8,
              DSFILE.DS_UNITS_BNKRPT_2_2_9 =
                   :QDS-UNITS-BNKRPT-2-2-9,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_9 =
                   :QDS-OUTBAL-BNKRPT-2-2-9,
              DSFILE.DS_UNITS_BNKRPT_2_2_10 =
                   :QDS-UNITS-BNKRPT-2-2-10,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_10 =
                   :QDS-OUTBAL-BNKRPT-2-2-10,
              DSFILE.DS_UNITS_BNKRPT_3_1_1 =
                   :QDS-UNITS-BNKRPT-3-1-1,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_1 =
                   :QDS-OUTBAL-BNKRPT-3-1-1,
              DSFILE.DS_UNITS_BNKRPT_3_1_2 =
                   :QDS-UNITS-BNKRPT-3-1-2,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_2 =
                   :QDS-OUTBAL-BNKRPT-3-1-2,
              DSFILE.DS_UNITS_BNKRPT_3_1_3 =
                   :QDS-UNITS-BNKRPT-3-1-3,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_3 =
                   :QDS-OUTBAL-BNKRPT-3-1-3,
              DSFILE.DS_UNITS_BNKRPT_3_1_4 =
                   :QDS-UNITS-BNKRPT-3-1-4,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_4 =
                   :QDS-OUTBAL-BNKRPT-3-1-4,
              DSFILE.DS_UNITS_BNKRPT_3_1_5 =
                   :QDS-UNITS-BNKRPT-3-1-5,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_5 =
                   :QDS-OUTBAL-BNKRPT-3-1-5,
              DSFILE.DS_UNITS_BNKRPT_3_1_6 =
                   :QDS-UNITS-BNKRPT-3-1-6,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_6 =
                   :QDS-OUTBAL-BNKRPT-3-1-6,
              DSFILE.DS_UNITS_BNKRPT_3_1_7 =
                   :QDS-UNITS-BNKRPT-3-1-7,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_7 =
                   :QDS-OUTBAL-BNKRPT-3-1-7,
              DSFILE.DS_UNITS_BNKRPT_3_1_8 =
                   :QDS-UNITS-BNKRPT-3-1-8,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_8 =
                   :QDS-OUTBAL-BNKRPT-3-1-8,
              DSFILE.DS_UNITS_BNKRPT_3_1_9 =
                   :QDS-UNITS-BNKRPT-3-1-9,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_9 =
                   :QDS-OUTBAL-BNKRPT-3-1-9,
              DSFILE.DS_UNITS_BNKRPT_3_1_10 =
                   :QDS-UNITS-BNKRPT-3-1-10,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_10 =
                   :QDS-OUTBAL-BNKRPT-3-1-10,
              DSFILE.DS_UNITS_BNKRPT_3_2_1 =
                   :QDS-UNITS-BNKRPT-3-2-1,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_1 =
                   :QDS-OUTBAL-BNKRPT-3-2-1,
              DSFILE.DS_UNITS_BNKRPT_3_2_2 =
                   :QDS-UNITS-BNKRPT-3-2-2,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_2 =
                   :QDS-OUTBAL-BNKRPT-3-2-2,
              DSFILE.DS_UNITS_BNKRPT_3_2_3 =
                   :QDS-UNITS-BNKRPT-3-2-3,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_3 =
                   :QDS-OUTBAL-BNKRPT-3-2-3,
              DSFILE.DS_UNITS_BNKRPT_3_2_4 =
                   :QDS-UNITS-BNKRPT-3-2-4,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_4 =
                   :QDS-OUTBAL-BNKRPT-3-2-4,
              DSFILE.DS_UNITS_BNKRPT_3_2_5 =
                   :QDS-UNITS-BNKRPT-3-2-5,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_5 =
                   :QDS-OUTBAL-BNKRPT-3-2-5,
              DSFILE.DS_UNITS_BNKRPT_3_2_6 =
                   :QDS-UNITS-BNKRPT-3-2-6,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_6 =
                   :QDS-OUTBAL-BNKRPT-3-2-6,
              DSFILE.DS_UNITS_BNKRPT_3_2_7 =
                   :QDS-UNITS-BNKRPT-3-2-7,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_7 =
                   :QDS-OUTBAL-BNKRPT-3-2-7,
              DSFILE.DS_UNITS_BNKRPT_3_2_8 =
                   :QDS-UNITS-BNKRPT-3-2-8,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_8 =
                   :QDS-OUTBAL-BNKRPT-3-2-8,
              DSFILE.DS_UNITS_BNKRPT_3_2_9 =
                   :QDS-UNITS-BNKRPT-3-2-9,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_9 =
                   :QDS-OUTBAL-BNKRPT-3-2-9,
              DSFILE.DS_UNITS_BNKRPT_3_2_10 =
                   :QDS-UNITS-BNKRPT-3-2-10,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_10 =
                   :QDS-OUTBAL-BNKRPT-3-2-10,
              DSFILE.DS_UNEARNED_AMT_1_1_1 =
                   :QDS-UNEARNED-AMT-1-1-1,
              DSFILE.DS_UNEARNED_AMT_1_1_2 =
                   :QDS-UNEARNED-AMT-1-1-2,
              DSFILE.DS_UNEARNED_AMT_1_1_3 =
                   :QDS-UNEARNED-AMT-1-1-3,
              DSFILE.DS_UNEARNED_AMT_1_1_4 =
                   :QDS-UNEARNED-AMT-1-1-4,
              DSFILE.DS_UNEARNED_AMT_1_1_5 =
                   :QDS-UNEARNED-AMT-1-1-5,
              DSFILE.DS_UNEARNED_AMT_1_1_6 =
                   :QDS-UNEARNED-AMT-1-1-6,
              DSFILE.DS_UNEARNED_AMT_1_1_7 =
                   :QDS-UNEARNED-AMT-1-1-7,
              DSFILE.DS_UNEARNED_AMT_1_1_8 =
                   :QDS-UNEARNED-AMT-1-1-8,
              DSFILE.DS_UNEARNED_AMT_1_1_9 =
                   :QDS-UNEARNED-AMT-1-1-9,
              DSFILE.DS_UNEARNED_AMT_1_1_10 =
                   :QDS-UNEARNED-AMT-1-1-10,
              DSFILE.DS_UNEARNED_AMT_1_2_1 =
                   :QDS-UNEARNED-AMT-1-2-1,
              DSFILE.DS_UNEARNED_AMT_1_2_2 =
                   :QDS-UNEARNED-AMT-1-2-2,
              DSFILE.DS_UNEARNED_AMT_1_2_3 =
                   :QDS-UNEARNED-AMT-1-2-3,
              DSFILE.DS_UNEARNED_AMT_1_2_4 =
                   :QDS-UNEARNED-AMT-1-2-4,
              DSFILE.DS_UNEARNED_AMT_1_2_5 =
                   :QDS-UNEARNED-AMT-1-2-5,
              DSFILE.DS_UNEARNED_AMT_1_2_6 =
                   :QDS-UNEARNED-AMT-1-2-6,
              DSFILE.DS_UNEARNED_AMT_1_2_7 =
                   :QDS-UNEARNED-AMT-1-2-7,
              DSFILE.DS_UNEARNED_AMT_1_2_8 =
                   :QDS-UNEARNED-AMT-1-2-8,
              DSFILE.DS_UNEARNED_AMT_1_2_9 =
                   :QDS-UNEARNED-AMT-1-2-9,
              DSFILE.DS_UNEARNED_AMT_1_2_10 =
                   :QDS-UNEARNED-AMT-1-2-10,
              DSFILE.DS_UNEARNED_AMT_2_1_1 =
                   :QDS-UNEARNED-AMT-2-1-1,
              DSFILE.DS_UNEARNED_AMT_2_1_2 =
                   :QDS-UNEARNED-AMT-2-1-2,
              DSFILE.DS_UNEARNED_AMT_2_1_3 =
                   :QDS-UNEARNED-AMT-2-1-3,
              DSFILE.DS_UNEARNED_AMT_2_1_4 =
                   :QDS-UNEARNED-AMT-2-1-4,
              DSFILE.DS_UNEARNED_AMT_2_1_5 =
                   :QDS-UNEARNED-AMT-2-1-5,
              DSFILE.DS_UNEARNED_AMT_2_1_6 =
                   :QDS-UNEARNED-AMT-2-1-6,
              DSFILE.DS_UNEARNED_AMT_2_1_7 =
                   :QDS-UNEARNED-AMT-2-1-7,
              DSFILE.DS_UNEARNED_AMT_2_1_8 =
                   :QDS-UNEARNED-AMT-2-1-8,
              DSFILE.DS_UNEARNED_AMT_2_1_9 =
                   :QDS-UNEARNED-AMT-2-1-9,
              DSFILE.DS_UNEARNED_AMT_2_1_10 =
                   :QDS-UNEARNED-AMT-2-1-10,
              DSFILE.DS_UNEARNED_AMT_2_2_1 =
                   :QDS-UNEARNED-AMT-2-2-1,
              DSFILE.DS_UNEARNED_AMT_2_2_2 =
                   :QDS-UNEARNED-AMT-2-2-2,
              DSFILE.DS_UNEARNED_AMT_2_2_3 =
                   :QDS-UNEARNED-AMT-2-2-3,
              DSFILE.DS_UNEARNED_AMT_2_2_4 =
                   :QDS-UNEARNED-AMT-2-2-4,
              DSFILE.DS_UNEARNED_AMT_2_2_5 =
                   :QDS-UNEARNED-AMT-2-2-5,
              DSFILE.DS_UNEARNED_AMT_2_2_6 =
                   :QDS-UNEARNED-AMT-2-2-6,
              DSFILE.DS_UNEARNED_AMT_2_2_7 =
                   :QDS-UNEARNED-AMT-2-2-7,
              DSFILE.DS_UNEARNED_AMT_2_2_8 =
                   :QDS-UNEARNED-AMT-2-2-8,
              DSFILE.DS_UNEARNED_AMT_2_2_9 =
                   :QDS-UNEARNED-AMT-2-2-9,
              DSFILE.DS_UNEARNED_AMT_2_2_10 =
                   :QDS-UNEARNED-AMT-2-2-10,
              DSFILE.DS_UNEARNED_AMT_3_1_1 =
                   :QDS-UNEARNED-AMT-3-1-1,
              DSFILE.DS_UNEARNED_AMT_3_1_2 =
                   :QDS-UNEARNED-AMT-3-1-2,
              DSFILE.DS_UNEARNED_AMT_3_1_3 =
                   :QDS-UNEARNED-AMT-3-1-3,
              DSFILE.DS_UNEARNED_AMT_3_1_4 =
                   :QDS-UNEARNED-AMT-3-1-4,
              DSFILE.DS_UNEARNED_AMT_3_1_5 =
                   :QDS-UNEARNED-AMT-3-1-5,
              DSFILE.DS_UNEARNED_AMT_3_1_6 =
                   :QDS-UNEARNED-AMT-3-1-6,
              DSFILE.DS_UNEARNED_AMT_3_1_7 =
                   :QDS-UNEARNED-AMT-3-1-7,
              DSFILE.DS_UNEARNED_AMT_3_1_8 =
                   :QDS-UNEARNED-AMT-3-1-8,
              DSFILE.DS_UNEARNED_AMT_3_1_9 =
                   :QDS-UNEARNED-AMT-3-1-9,
              DSFILE.DS_UNEARNED_AMT_3_1_10 =
                   :QDS-UNEARNED-AMT-3-1-10,
              DSFILE.DS_UNEARNED_AMT_3_2_1 =
                   :QDS-UNEARNED-AMT-3-2-1,
              DSFILE.DS_UNEARNED_AMT_3_2_2 =
                   :QDS-UNEARNED-AMT-3-2-2,
              DSFILE.DS_UNEARNED_AMT_3_2_3 =
                   :QDS-UNEARNED-AMT-3-2-3,
              DSFILE.DS_UNEARNED_AMT_3_2_4 =
                   :QDS-UNEARNED-AMT-3-2-4,
              DSFILE.DS_UNEARNED_AMT_3_2_5 =
                   :QDS-UNEARNED-AMT-3-2-5,
              DSFILE.DS_UNEARNED_AMT_3_2_6 =
                   :QDS-UNEARNED-AMT-3-2-6,
              DSFILE.DS_UNEARNED_AMT_3_2_7 =
                   :QDS-UNEARNED-AMT-3-2-7,
              DSFILE.DS_UNEARNED_AMT_3_2_8 =
                   :QDS-UNEARNED-AMT-3-2-8,
              DSFILE.DS_UNEARNED_AMT_3_2_9 =
                   :QDS-UNEARNED-AMT-3-2-9,
              DSFILE.DS_UNEARNED_AMT_3_2_10 =
                   :QDS-UNEARNED-AMT-3-2-10,
              DSFILE.DS_DLDISC_VOL_1 = :QDS-DLDISC-VOL-1,
              DSFILE.DS_DLDISC_VOL_2 = :QDS-DLDISC-VOL-2,
              DSFILE.DS_DLDISC_VOL_3 = :QDS-DLDISC-VOL-3,
              DSFILE.DS_AFPDUEDL_MEMO = :QDS-AFPDUEDL-MEMO,
              DSFILE.DS_UNPAID_MANUAL = :QDS-UNPAID-MANUAL,
              DSFILE.DS_LTOUCH_DATE =
                 NULLIF(:QDS-LTOUCH-DATE,'1900-01-01')
             WHERE DSFILE.DS_DATE_CCYYMM = :QDS-DATE-CCYYMM
               AND DSFILE.DS_BRANCH = :QDS-BRANCH
               AND DSFILE.DS_NUM = :QDS-NUM
               AND DSFILE.DS_CLASS = :QDS-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-DS1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-DS1-FILE.

           MOVE DS-PATH-OWNBR  TO DS-BRANCH.
           PERFORM DELETE-IT.
           MOVE DS-SELECT-PATH TO E-FILE.
           MOVE DS1-KEY        TO E-KEYX.
           PERFORM SET-DS-FIELDS.

           EXEC SQL
            DELETE FROM DBO.DSFILE
             WHERE DSFILE.DS_DATE_CCYYMM = :QDS-DATE-CCYYMM
               AND DSFILE.DS_BRANCH = :QDS-BRANCH
               AND DSFILE.DS_NUM = :QDS-NUM
               AND DSFILE.DS_CLASS = :QDS-CLASS
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-DS1-FILE.

      *-----------------------------------------------------------------
       CLOSE-DS1-FILE.

           IF ( DS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE DS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO DS1-CURSOR-STAT.

      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
 
      *=================================================================
      *=================================================================
      *=================================================================
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
      *-----------------------------------------------------------------
       END-RUN.

           PERFORM CLOSE-FILES.

      *    MOVE WK1-PATH TO ACCESS-BUF.
      *    PERFORM REMOVE-WORKFILE.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND         ) AND
              ( EXT-ROUTE-BUF = "00" OR "70" )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF ( SV-BEG-BRANCH NOT = 9999 )
              IF ( EXT-EOBUF-GROUP-FG = "Y"  )
                 MOVE SV-BEG-BRANCH     TO BEG-BRANCH
                 MOVE SV-END-BRANCH     TO END-BRANCH
                 MOVE SV-GROUP-FLAG     TO GROUP-FLAG
                 MOVE SV-ENT-GROUP      TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BRANCH TO BEG-BRANCH
                 MOVE SV-END-BRANCH TO END-BRANCH.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IODSIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
