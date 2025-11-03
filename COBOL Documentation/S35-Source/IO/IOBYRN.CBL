       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOBYRN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  03-24-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOBYRN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : BYFILE 
      * TABLE DESC    : BY FILE
      * OUTPUT FILE   : XPC/IOBYRN.PRE
      * USED FOR      : LIBLP/LPWSBY.CPY  --- QBYA-REC
      *                 LIBLP/LPBY1R.CPY  --- READ
      *                 LIBLP/LPBY1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0324 ***NEW***
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
           "@(#)IO/IOBYRN.CBL S34 05/24/21-12:38:15 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LPWSBY.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIBLP/LPWSBY.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QBY1-WHERE-BEG-KEY.
           03  QBY1-WBEG-DATE-CCYYMM     PIC 9(6).
           03  QBY1-WBEG-BRNO            PIC 9(4).
           03  QBY1-WBEG-CLASS           PIC 9(3).
 
       01  QBY1-WHERE-END-KEY.
           03  QBY1-WEND-DATE-CCYYMM     PIC 9(6).
           03  QBY1-WEND-BRNO            PIC 9(4).
           03  QBY1-WEND-CLASS           PIC 9(3).
 
       01  QBYA-REC.
           03  QBYA-DATE-CCYYMM     PIC 9(6).
           03  QBYA-BRNO            PIC 9(4).
           03  QBYA-CLASS           PIC 9(3).
           03  QBYA-LTOUCH-DATE     PIC X(10).
           03  QBYA-NO-1-1-1        PIC S9(7).
           03  QBYA-AMT-1-1-1       PIC S9(9)V99.
           03  QBYA-O2-1-1-1        PIC S9(9)V99.
           03  QBYA-INT-1-1-1       PIC S9(9)V99.
           03  QBYA-NO-1-1-2        PIC S9(7).
           03  QBYA-AMT-1-1-2       PIC S9(9)V99.
           03  QBYA-O2-1-1-2        PIC S9(9)V99.
           03  QBYA-INT-1-1-2       PIC S9(9)V99.
           03  QBYA-NO-1-1-3        PIC S9(7).
           03  QBYA-AMT-1-1-3       PIC S9(9)V99.
           03  QBYA-O2-1-1-3        PIC S9(9)V99.
           03  QBYA-INT-1-1-3       PIC S9(9)V99.
           03  QBYA-NO-1-1-4        PIC S9(7).
           03  QBYA-AMT-1-1-4       PIC S9(9)V99.
           03  QBYA-O2-1-1-4        PIC S9(9)V99.
           03  QBYA-INT-1-1-4       PIC S9(9)V99.
           03  QBYA-NO-1-1-5        PIC S9(7).
           03  QBYA-AMT-1-1-5       PIC S9(9)V99.
           03  QBYA-O2-1-1-5        PIC S9(9)V99.
           03  QBYA-INT-1-1-5       PIC S9(9)V99.
           03  QBYA-NO-1-1-6        PIC S9(7).
           03  QBYA-AMT-1-1-6       PIC S9(9)V99.
           03  QBYA-O2-1-1-6        PIC S9(9)V99.
           03  QBYA-INT-1-1-6       PIC S9(9)V99.
           03  QBYA-NO-1-1-7        PIC S9(7).
           03  QBYA-AMT-1-1-7       PIC S9(9)V99.
           03  QBYA-O2-1-1-7        PIC S9(9)V99.
           03  QBYA-INT-1-1-7       PIC S9(9)V99.
           03  QBYA-NO-1-1-8        PIC S9(7).
           03  QBYA-AMT-1-1-8       PIC S9(9)V99.
           03  QBYA-O2-1-1-8        PIC S9(9)V99.
           03  QBYA-INT-1-1-8       PIC S9(9)V99.
           03  QBYA-NO-1-1-9        PIC S9(7).
           03  QBYA-AMT-1-1-9       PIC S9(9)V99.
           03  QBYA-O2-1-1-9        PIC S9(9)V99.
           03  QBYA-INT-1-1-9       PIC S9(9)V99.
           03  QBYA-NO-1-1-10       PIC S9(7).
           03  QBYA-AMT-1-1-10      PIC S9(9)V99.
           03  QBYA-O2-1-1-10       PIC S9(9)V99.
           03  QBYA-INT-1-1-10      PIC S9(9)V99.
           03  QBYA-NO-1-1-11       PIC S9(7).
           03  QBYA-AMT-1-1-11      PIC S9(9)V99.
           03  QBYA-O2-1-1-11       PIC S9(9)V99.
           03  QBYA-INT-1-1-11      PIC S9(9)V99.
           03  QBYA-NO-1-2-1        PIC S9(7).
           03  QBYA-AMT-1-2-1       PIC S9(9)V99.
           03  QBYA-O2-1-2-1        PIC S9(9)V99.
           03  QBYA-INT-1-2-1       PIC S9(9)V99.
           03  QBYA-NO-1-2-2        PIC S9(7).
           03  QBYA-AMT-1-2-2       PIC S9(9)V99.
           03  QBYA-O2-1-2-2        PIC S9(9)V99.
           03  QBYA-INT-1-2-2       PIC S9(9)V99.
           03  QBYA-NO-1-2-3        PIC S9(7).
           03  QBYA-AMT-1-2-3       PIC S9(9)V99.
           03  QBYA-O2-1-2-3        PIC S9(9)V99.
           03  QBYA-INT-1-2-3       PIC S9(9)V99.
           03  QBYA-NO-1-2-4        PIC S9(7).
           03  QBYA-AMT-1-2-4       PIC S9(9)V99.
           03  QBYA-O2-1-2-4        PIC S9(9)V99.
           03  QBYA-INT-1-2-4       PIC S9(9)V99.
           03  QBYA-NO-1-2-5        PIC S9(7).
           03  QBYA-AMT-1-2-5       PIC S9(9)V99.
           03  QBYA-O2-1-2-5        PIC S9(9)V99.
           03  QBYA-INT-1-2-5       PIC S9(9)V99.
           03  QBYA-NO-1-2-6        PIC S9(7).
           03  QBYA-AMT-1-2-6       PIC S9(9)V99.
           03  QBYA-O2-1-2-6        PIC S9(9)V99.
           03  QBYA-INT-1-2-6       PIC S9(9)V99.
           03  QBYA-NO-1-2-7        PIC S9(7).
           03  QBYA-AMT-1-2-7       PIC S9(9)V99.
           03  QBYA-O2-1-2-7        PIC S9(9)V99.
           03  QBYA-INT-1-2-7       PIC S9(9)V99.
           03  QBYA-NO-1-2-8        PIC S9(7).
           03  QBYA-AMT-1-2-8       PIC S9(9)V99.
           03  QBYA-O2-1-2-8        PIC S9(9)V99.
           03  QBYA-INT-1-2-8       PIC S9(9)V99.
           03  QBYA-NO-1-2-9        PIC S9(7).
           03  QBYA-AMT-1-2-9       PIC S9(9)V99.
           03  QBYA-O2-1-2-9        PIC S9(9)V99.
           03  QBYA-INT-1-2-9       PIC S9(9)V99.
           03  QBYA-NO-1-2-10       PIC S9(7).
           03  QBYA-AMT-1-2-10      PIC S9(9)V99.
           03  QBYA-O2-1-2-10       PIC S9(9)V99.
           03  QBYA-INT-1-2-10      PIC S9(9)V99.
           03  QBYA-NO-1-2-11       PIC S9(7).
           03  QBYA-AMT-1-2-11      PIC S9(9)V99.
           03  QBYA-O2-1-2-11       PIC S9(9)V99.
           03  QBYA-INT-1-2-11      PIC S9(9)V99.
           03  QBYA-NO-2-1-1        PIC S9(7).
           03  QBYA-AMT-2-1-1       PIC S9(9)V99.
           03  QBYA-O2-2-1-1        PIC S9(9)V99.
           03  QBYA-INT-2-1-1       PIC S9(9)V99.
           03  QBYA-NO-2-1-2        PIC S9(7).
           03  QBYA-AMT-2-1-2       PIC S9(9)V99.
           03  QBYA-O2-2-1-2        PIC S9(9)V99.
           03  QBYA-INT-2-1-2       PIC S9(9)V99.
           03  QBYA-NO-2-1-3        PIC S9(7).
           03  QBYA-AMT-2-1-3       PIC S9(9)V99.
           03  QBYA-O2-2-1-3        PIC S9(9)V99.
           03  QBYA-INT-2-1-3       PIC S9(9)V99.
           03  QBYA-NO-2-1-4        PIC S9(7).
           03  QBYA-AMT-2-1-4       PIC S9(9)V99.
           03  QBYA-O2-2-1-4        PIC S9(9)V99.
           03  QBYA-INT-2-1-4       PIC S9(9)V99.
           03  QBYA-NO-2-1-5        PIC S9(7).
           03  QBYA-AMT-2-1-5       PIC S9(9)V99.
           03  QBYA-O2-2-1-5        PIC S9(9)V99.
           03  QBYA-INT-2-1-5       PIC S9(9)V99.
           03  QBYA-NO-2-1-6        PIC S9(7).
           03  QBYA-AMT-2-1-6       PIC S9(9)V99.
           03  QBYA-O2-2-1-6        PIC S9(9)V99.
           03  QBYA-INT-2-1-6       PIC S9(9)V99.
           03  QBYA-NO-2-1-7        PIC S9(7).
           03  QBYA-AMT-2-1-7       PIC S9(9)V99.
           03  QBYA-O2-2-1-7        PIC S9(9)V99.
           03  QBYA-INT-2-1-7       PIC S9(9)V99.
           03  QBYA-NO-2-1-8        PIC S9(7).
           03  QBYA-AMT-2-1-8       PIC S9(9)V99.
           03  QBYA-O2-2-1-8        PIC S9(9)V99.
           03  QBYA-INT-2-1-8       PIC S9(9)V99.
           03  QBYA-NO-2-1-9        PIC S9(7).
           03  QBYA-AMT-2-1-9       PIC S9(9)V99.
           03  QBYA-O2-2-1-9        PIC S9(9)V99.
           03  QBYA-INT-2-1-9       PIC S9(9)V99.
           03  QBYA-NO-2-1-10       PIC S9(7).
           03  QBYA-AMT-2-1-10      PIC S9(9)V99.
           03  QBYA-O2-2-1-10       PIC S9(9)V99.
           03  QBYA-INT-2-1-10      PIC S9(9)V99.
           03  QBYA-NO-2-1-11       PIC S9(7).
           03  QBYA-AMT-2-1-11      PIC S9(9)V99.
           03  QBYA-O2-2-1-11       PIC S9(9)V99.
           03  QBYA-INT-2-1-11      PIC S9(9)V99.
           03  QBYA-NO-2-2-1        PIC S9(7).
           03  QBYA-AMT-2-2-1       PIC S9(9)V99.
           03  QBYA-O2-2-2-1        PIC S9(9)V99.
           03  QBYA-INT-2-2-1       PIC S9(9)V99.
           03  QBYA-NO-2-2-2        PIC S9(7).
           03  QBYA-AMT-2-2-2       PIC S9(9)V99.
           03  QBYA-O2-2-2-2        PIC S9(9)V99.
           03  QBYA-INT-2-2-2       PIC S9(9)V99.
           03  QBYA-NO-2-2-3        PIC S9(7).
           03  QBYA-AMT-2-2-3       PIC S9(9)V99.
           03  QBYA-O2-2-2-3        PIC S9(9)V99.
           03  QBYA-INT-2-2-3       PIC S9(9)V99.
           03  QBYA-NO-2-2-4        PIC S9(7).
           03  QBYA-AMT-2-2-4       PIC S9(9)V99.
           03  QBYA-O2-2-2-4        PIC S9(9)V99.
           03  QBYA-INT-2-2-4       PIC S9(9)V99.
           03  QBYA-NO-2-2-5        PIC S9(7).
           03  QBYA-AMT-2-2-5       PIC S9(9)V99.
           03  QBYA-O2-2-2-5        PIC S9(9)V99.
           03  QBYA-INT-2-2-5       PIC S9(9)V99.
           03  QBYA-NO-2-2-6        PIC S9(7).
           03  QBYA-AMT-2-2-6       PIC S9(9)V99.
           03  QBYA-O2-2-2-6        PIC S9(9)V99.
           03  QBYA-INT-2-2-6       PIC S9(9)V99.
           03  QBYA-NO-2-2-7        PIC S9(7).
           03  QBYA-AMT-2-2-7       PIC S9(9)V99.
           03  QBYA-O2-2-2-7        PIC S9(9)V99.
           03  QBYA-INT-2-2-7       PIC S9(9)V99.
           03  QBYA-NO-2-2-8        PIC S9(7).
           03  QBYA-AMT-2-2-8       PIC S9(9)V99.
           03  QBYA-O2-2-2-8        PIC S9(9)V99.
           03  QBYA-INT-2-2-8       PIC S9(9)V99.
           03  QBYA-NO-2-2-9        PIC S9(7).
           03  QBYA-AMT-2-2-9       PIC S9(9)V99.
           03  QBYA-O2-2-2-9        PIC S9(9)V99.
           03  QBYA-INT-2-2-9       PIC S9(9)V99.
           03  QBYA-NO-2-2-10       PIC S9(7).
           03  QBYA-AMT-2-2-10      PIC S9(9)V99.
           03  QBYA-O2-2-2-10       PIC S9(9)V99.
           03  QBYA-INT-2-2-10      PIC S9(9)V99.
           03  QBYA-NO-2-2-11       PIC S9(7).
           03  QBYA-AMT-2-2-11      PIC S9(9)V99.
           03  QBYA-O2-2-2-11       PIC S9(9)V99.
           03  QBYA-INT-2-2-11      PIC S9(9)V99.
           03  QBYA-NO-3-1-1        PIC S9(7).
           03  QBYA-AMT-3-1-1       PIC S9(9)V99.
           03  QBYA-O2-3-1-1        PIC S9(9)V99.
           03  QBYA-INT-3-1-1       PIC S9(9)V99.
           03  QBYA-NO-3-1-2        PIC S9(7).
           03  QBYA-AMT-3-1-2       PIC S9(9)V99.
           03  QBYA-O2-3-1-2        PIC S9(9)V99.
           03  QBYA-INT-3-1-2       PIC S9(9)V99.
           03  QBYA-NO-3-1-3        PIC S9(7).
           03  QBYA-AMT-3-1-3       PIC S9(9)V99.
           03  QBYA-O2-3-1-3        PIC S9(9)V99.
           03  QBYA-INT-3-1-3       PIC S9(9)V99.
           03  QBYA-NO-3-1-4        PIC S9(7).
           03  QBYA-AMT-3-1-4       PIC S9(9)V99.
           03  QBYA-O2-3-1-4        PIC S9(9)V99.
           03  QBYA-INT-3-1-4       PIC S9(9)V99.
           03  QBYA-NO-3-1-5        PIC S9(7).
           03  QBYA-AMT-3-1-5       PIC S9(9)V99.
           03  QBYA-O2-3-1-5        PIC S9(9)V99.
           03  QBYA-INT-3-1-5       PIC S9(9)V99.
           03  QBYA-NO-3-1-6        PIC S9(7).
           03  QBYA-AMT-3-1-6       PIC S9(9)V99.
           03  QBYA-O2-3-1-6        PIC S9(9)V99.
           03  QBYA-INT-3-1-6       PIC S9(9)V99.
           03  QBYA-NO-3-1-7        PIC S9(7).
           03  QBYA-AMT-3-1-7       PIC S9(9)V99.
           03  QBYA-O2-3-1-7        PIC S9(9)V99.
           03  QBYA-INT-3-1-7       PIC S9(9)V99.
           03  QBYA-NO-3-1-8        PIC S9(7).
           03  QBYA-AMT-3-1-8       PIC S9(9)V99.
           03  QBYA-O2-3-1-8        PIC S9(9)V99.
           03  QBYA-INT-3-1-8       PIC S9(9)V99.
           03  QBYA-NO-3-1-9        PIC S9(7).
           03  QBYA-AMT-3-1-9       PIC S9(9)V99.
           03  QBYA-O2-3-1-9        PIC S9(9)V99.
           03  QBYA-INT-3-1-9       PIC S9(9)V99.
           03  QBYA-NO-3-1-10       PIC S9(7).
           03  QBYA-AMT-3-1-10      PIC S9(9)V99.
           03  QBYA-O2-3-1-10       PIC S9(9)V99.
           03  QBYA-INT-3-1-10      PIC S9(9)V99.
           03  QBYA-NO-3-1-11       PIC S9(7).
           03  QBYA-AMT-3-1-11      PIC S9(9)V99.
           03  QBYA-O2-3-1-11       PIC S9(9)V99.
           03  QBYA-INT-3-1-11      PIC S9(9)V99.
           03  QBYA-NO-3-2-1        PIC S9(7).
           03  QBYA-AMT-3-2-1       PIC S9(9)V99.
           03  QBYA-O2-3-2-1        PIC S9(9)V99.
           03  QBYA-INT-3-2-1       PIC S9(9)V99.
           03  QBYA-NO-3-2-2        PIC S9(7).
           03  QBYA-AMT-3-2-2       PIC S9(9)V99.
           03  QBYA-O2-3-2-2        PIC S9(9)V99.
           03  QBYA-INT-3-2-2       PIC S9(9)V99.
           03  QBYA-NO-3-2-3        PIC S9(7).
           03  QBYA-AMT-3-2-3       PIC S9(9)V99.
           03  QBYA-O2-3-2-3        PIC S9(9)V99.
           03  QBYA-INT-3-2-3       PIC S9(9)V99.
           03  QBYA-NO-3-2-4        PIC S9(7).
           03  QBYA-AMT-3-2-4       PIC S9(9)V99.
           03  QBYA-O2-3-2-4        PIC S9(9)V99.
           03  QBYA-INT-3-2-4       PIC S9(9)V99.
           03  QBYA-NO-3-2-5        PIC S9(7).
           03  QBYA-AMT-3-2-5       PIC S9(9)V99.
           03  QBYA-O2-3-2-5        PIC S9(9)V99.
           03  QBYA-INT-3-2-5       PIC S9(9)V99.
           03  QBYA-NO-3-2-6        PIC S9(7).
           03  QBYA-AMT-3-2-6       PIC S9(9)V99.
           03  QBYA-O2-3-2-6        PIC S9(9)V99.
           03  QBYA-INT-3-2-6       PIC S9(9)V99.
           03  QBYA-NO-3-2-7        PIC S9(7).
           03  QBYA-AMT-3-2-7       PIC S9(9)V99.
           03  QBYA-O2-3-2-7        PIC S9(9)V99.
           03  QBYA-INT-3-2-7       PIC S9(9)V99.
           03  QBYA-NO-3-2-8        PIC S9(7).
           03  QBYA-AMT-3-2-8       PIC S9(9)V99.
           03  QBYA-O2-3-2-8        PIC S9(9)V99.
           03  QBYA-INT-3-2-8       PIC S9(9)V99.
           03  QBYA-NO-3-2-9        PIC S9(7).
           03  QBYA-AMT-3-2-9       PIC S9(9)V99.
           03  QBYA-O2-3-2-9        PIC S9(9)V99.
           03  QBYA-INT-3-2-9       PIC S9(9)V99.
           03  QBYA-NO-3-2-10       PIC S9(7).
           03  QBYA-AMT-3-2-10      PIC S9(9)V99.
           03  QBYA-O2-3-2-10       PIC S9(9)V99.
           03  QBYA-INT-3-2-10      PIC S9(9)V99.
           03  QBYA-NO-3-2-11       PIC S9(7).
           03  QBYA-AMT-3-2-11      PIC S9(9)V99.
           03  QBYA-O2-3-2-11       PIC S9(9)V99.
           03  QBYA-INT-3-2-11      PIC S9(9)V99.
           03  QBYA-NO-REPO-1-1-1     PIC S9(7).
           03  QBYA-AMT-REPO-1-1-1    PIC S9(9)V99.
           03  QBYA-O2-REPO-1-1-1     PIC S9(9)V99.
           03  QBYA-INT-REPO-1-1-1    PIC S9(9)V99.
           03  QBYA-NO-REPO-1-1-2     PIC S9(7).
           03  QBYA-AMT-REPO-1-1-2    PIC S9(9)V99.
           03  QBYA-O2-REPO-1-1-2     PIC S9(9)V99.
           03  QBYA-INT-REPO-1-1-2    PIC S9(9)V99.
           03  QBYA-NO-REPO-1-1-3     PIC S9(7).
           03  QBYA-AMT-REPO-1-1-3    PIC S9(9)V99.
           03  QBYA-O2-REPO-1-1-3     PIC S9(9)V99.
           03  QBYA-INT-REPO-1-1-3    PIC S9(9)V99.
           03  QBYA-NO-REPO-1-1-4     PIC S9(7).
           03  QBYA-AMT-REPO-1-1-4    PIC S9(9)V99.
           03  QBYA-O2-REPO-1-1-4     PIC S9(9)V99.
           03  QBYA-INT-REPO-1-1-4    PIC S9(9)V99.
           03  QBYA-NO-REPO-1-1-5     PIC S9(7).
           03  QBYA-AMT-REPO-1-1-5    PIC S9(9)V99.
           03  QBYA-O2-REPO-1-1-5     PIC S9(9)V99.
           03  QBYA-INT-REPO-1-1-5    PIC S9(9)V99.
           03  QBYA-NO-REPO-1-2-1     PIC S9(7).
           03  QBYA-AMT-REPO-1-2-1    PIC S9(9)V99.
           03  QBYA-O2-REPO-1-2-1     PIC S9(9)V99.
           03  QBYA-INT-REPO-1-2-1    PIC S9(9)V99.
           03  QBYA-NO-REPO-1-2-2     PIC S9(7).
           03  QBYA-AMT-REPO-1-2-2    PIC S9(9)V99.
           03  QBYA-O2-REPO-1-2-2     PIC S9(9)V99.
           03  QBYA-INT-REPO-1-2-2    PIC S9(9)V99.
           03  QBYA-NO-REPO-1-2-3     PIC S9(7).
           03  QBYA-AMT-REPO-1-2-3    PIC S9(9)V99.
           03  QBYA-O2-REPO-1-2-3     PIC S9(9)V99.
           03  QBYA-INT-REPO-1-2-3    PIC S9(9)V99.
           03  QBYA-NO-REPO-1-2-4     PIC S9(7).
           03  QBYA-AMT-REPO-1-2-4    PIC S9(9)V99.
           03  QBYA-O2-REPO-1-2-4     PIC S9(9)V99.
           03  QBYA-INT-REPO-1-2-4    PIC S9(9)V99.
           03  QBYA-NO-REPO-1-2-5     PIC S9(7).
           03  QBYA-AMT-REPO-1-2-5    PIC S9(9)V99.
           03  QBYA-O2-REPO-1-2-5     PIC S9(9)V99.
           03  QBYA-INT-REPO-1-2-5    PIC S9(9)V99.
           03  QBYA-NO-REPO-2-1-1     PIC S9(7).
           03  QBYA-AMT-REPO-2-1-1    PIC S9(9)V99.
           03  QBYA-O2-REPO-2-1-1     PIC S9(9)V99.
           03  QBYA-INT-REPO-2-1-1    PIC S9(9)V99.
           03  QBYA-NO-REPO-2-1-2     PIC S9(7).
           03  QBYA-AMT-REPO-2-1-2    PIC S9(9)V99.
           03  QBYA-O2-REPO-2-1-2     PIC S9(9)V99.
           03  QBYA-INT-REPO-2-1-2    PIC S9(9)V99.
           03  QBYA-NO-REPO-2-1-3     PIC S9(7).
           03  QBYA-AMT-REPO-2-1-3    PIC S9(9)V99.
           03  QBYA-O2-REPO-2-1-3     PIC S9(9)V99.
           03  QBYA-INT-REPO-2-1-3    PIC S9(9)V99.
           03  QBYA-NO-REPO-2-1-4     PIC S9(7).
           03  QBYA-AMT-REPO-2-1-4    PIC S9(9)V99.
           03  QBYA-O2-REPO-2-1-4     PIC S9(9)V99.
           03  QBYA-INT-REPO-2-1-4    PIC S9(9)V99.
           03  QBYA-NO-REPO-2-1-5     PIC S9(7).
           03  QBYA-AMT-REPO-2-1-5    PIC S9(9)V99.
           03  QBYA-O2-REPO-2-1-5     PIC S9(9)V99.
           03  QBYA-INT-REPO-2-1-5    PIC S9(9)V99.
           03  QBYA-NO-REPO-2-2-1     PIC S9(7).
           03  QBYA-AMT-REPO-2-2-1    PIC S9(9)V99.
           03  QBYA-O2-REPO-2-2-1     PIC S9(9)V99.
           03  QBYA-INT-REPO-2-2-1    PIC S9(9)V99.
           03  QBYA-NO-REPO-2-2-2     PIC S9(7).
           03  QBYA-AMT-REPO-2-2-2    PIC S9(9)V99.
           03  QBYA-O2-REPO-2-2-2     PIC S9(9)V99.
           03  QBYA-INT-REPO-2-2-2    PIC S9(9)V99.
           03  QBYA-NO-REPO-2-2-3     PIC S9(7).
           03  QBYA-AMT-REPO-2-2-3    PIC S9(9)V99.
           03  QBYA-O2-REPO-2-2-3     PIC S9(9)V99.
           03  QBYA-INT-REPO-2-2-3    PIC S9(9)V99.
           03  QBYA-NO-REPO-2-2-4     PIC S9(7).
           03  QBYA-AMT-REPO-2-2-4    PIC S9(9)V99.
           03  QBYA-O2-REPO-2-2-4     PIC S9(9)V99.
           03  QBYA-INT-REPO-2-2-4    PIC S9(9)V99.
           03  QBYA-NO-REPO-2-2-5     PIC S9(7).
           03  QBYA-AMT-REPO-2-2-5    PIC S9(9)V99.
           03  QBYA-O2-REPO-2-2-5     PIC S9(9)V99.
           03  QBYA-INT-REPO-2-2-5    PIC S9(9)V99.
           03  QBYA-NO-REPO-3-1-1     PIC S9(7).
           03  QBYA-AMT-REPO-3-1-1    PIC S9(9)V99.
           03  QBYA-O2-REPO-3-1-1     PIC S9(9)V99.
           03  QBYA-INT-REPO-3-1-1    PIC S9(9)V99.
           03  QBYA-NO-REPO-3-1-2     PIC S9(7).
           03  QBYA-AMT-REPO-3-1-2    PIC S9(9)V99.
           03  QBYA-O2-REPO-3-1-2     PIC S9(9)V99.
           03  QBYA-INT-REPO-3-1-2    PIC S9(9)V99.
           03  QBYA-NO-REPO-3-1-3     PIC S9(7).
           03  QBYA-AMT-REPO-3-1-3    PIC S9(9)V99.
           03  QBYA-O2-REPO-3-1-3     PIC S9(9)V99.
           03  QBYA-INT-REPO-3-1-3    PIC S9(9)V99.
           03  QBYA-NO-REPO-3-1-4     PIC S9(7).
           03  QBYA-AMT-REPO-3-1-4    PIC S9(9)V99.
           03  QBYA-O2-REPO-3-1-4     PIC S9(9)V99.
           03  QBYA-INT-REPO-3-1-4    PIC S9(9)V99.
           03  QBYA-NO-REPO-3-1-5     PIC S9(7).
           03  QBYA-AMT-REPO-3-1-5    PIC S9(9)V99.
           03  QBYA-O2-REPO-3-1-5     PIC S9(9)V99.
           03  QBYA-INT-REPO-3-1-5    PIC S9(9)V99.
           03  QBYA-NO-REPO-3-2-1     PIC S9(7).
           03  QBYA-AMT-REPO-3-2-1    PIC S9(9)V99.
           03  QBYA-O2-REPO-3-2-1     PIC S9(9)V99.
           03  QBYA-INT-REPO-3-2-1    PIC S9(9)V99.
           03  QBYA-NO-REPO-3-2-2     PIC S9(7).
           03  QBYA-AMT-REPO-3-2-2    PIC S9(9)V99.
           03  QBYA-O2-REPO-3-2-2     PIC S9(9)V99.
           03  QBYA-INT-REPO-3-2-2    PIC S9(9)V99.
           03  QBYA-NO-REPO-3-2-3     PIC S9(7).
           03  QBYA-AMT-REPO-3-2-3    PIC S9(9)V99.
           03  QBYA-O2-REPO-3-2-3     PIC S9(9)V99.
           03  QBYA-INT-REPO-3-2-3    PIC S9(9)V99.
           03  QBYA-NO-REPO-3-2-4     PIC S9(7).
           03  QBYA-AMT-REPO-3-2-4    PIC S9(9)V99.
           03  QBYA-O2-REPO-3-2-4     PIC S9(9)V99.
           03  QBYA-INT-REPO-3-2-4    PIC S9(9)V99.
           03  QBYA-NO-REPO-3-2-5     PIC S9(7).
           03  QBYA-AMT-REPO-3-2-5    PIC S9(9)V99.
           03  QBYA-O2-REPO-3-2-5     PIC S9(9)V99.
           03  QBYA-INT-REPO-3-2-5    PIC S9(9)V99.
           03  QBYA-NO-PL-1-1-1          PIC S9(7).
           03  QBYA-AMT-PL-1-1-1         PIC S9(9)V99.
           03  QBYA-O2-PL-1-1-1          PIC S9(9)V99.
           03  QBYA-INT-PL-1-1-1         PIC S9(9)V99.
           03  QBYA-NO-PL-1-1-2          PIC S9(7).
           03  QBYA-AMT-PL-1-1-2         PIC S9(9)V99.
           03  QBYA-O2-PL-1-1-2          PIC S9(9)V99.
           03  QBYA-INT-PL-1-1-2         PIC S9(9)V99.
           03  QBYA-NO-PL-1-1-3          PIC S9(7).
           03  QBYA-AMT-PL-1-1-3         PIC S9(9)V99.
           03  QBYA-O2-PL-1-1-3          PIC S9(9)V99.
           03  QBYA-INT-PL-1-1-3         PIC S9(9)V99.
           03  QBYA-NO-PL-1-1-4          PIC S9(7).
           03  QBYA-AMT-PL-1-1-4         PIC S9(9)V99.
           03  QBYA-O2-PL-1-1-4          PIC S9(9)V99.
           03  QBYA-INT-PL-1-1-4         PIC S9(9)V99.
           03  QBYA-NO-PL-1-1-5          PIC S9(7).
           03  QBYA-AMT-PL-1-1-5         PIC S9(9)V99.
           03  QBYA-O2-PL-1-1-5          PIC S9(9)V99.
           03  QBYA-INT-PL-1-1-5         PIC S9(9)V99.
           03  QBYA-NO-PL-1-1-6          PIC S9(7).
           03  QBYA-AMT-PL-1-1-6         PIC S9(9)V99.
           03  QBYA-O2-PL-1-1-6          PIC S9(9)V99.
           03  QBYA-INT-PL-1-1-6         PIC S9(9)V99.
           03  QBYA-NO-PL-1-1-7          PIC S9(7).
           03  QBYA-AMT-PL-1-1-7         PIC S9(9)V99.
           03  QBYA-O2-PL-1-1-7          PIC S9(9)V99.
           03  QBYA-INT-PL-1-1-7         PIC S9(9)V99.
           03  QBYA-NO-PL-1-1-8          PIC S9(7).
           03  QBYA-AMT-PL-1-1-8         PIC S9(9)V99.
           03  QBYA-O2-PL-1-1-8          PIC S9(9)V99.
           03  QBYA-INT-PL-1-1-8         PIC S9(9)V99.
           03  QBYA-NO-PL-1-1-9          PIC S9(7).
           03  QBYA-AMT-PL-1-1-9         PIC S9(9)V99.
           03  QBYA-O2-PL-1-1-9          PIC S9(9)V99.
           03  QBYA-INT-PL-1-1-9         PIC S9(9)V99.
           03  QBYA-NO-PL-1-1-10         PIC S9(7).
           03  QBYA-AMT-PL-1-1-10        PIC S9(9)V99.
           03  QBYA-O2-PL-1-1-10         PIC S9(9)V99.
           03  QBYA-INT-PL-1-1-10        PIC S9(9)V99.
           03  QBYA-NO-PL-1-2-1          PIC S9(7).
           03  QBYA-AMT-PL-1-2-1         PIC S9(9)V99.
           03  QBYA-O2-PL-1-2-1          PIC S9(9)V99.
           03  QBYA-INT-PL-1-2-1         PIC S9(9)V99.
           03  QBYA-NO-PL-1-2-2          PIC S9(7).
           03  QBYA-AMT-PL-1-2-2         PIC S9(9)V99.
           03  QBYA-O2-PL-1-2-2          PIC S9(9)V99.
           03  QBYA-INT-PL-1-2-2         PIC S9(9)V99.
           03  QBYA-NO-PL-1-2-3          PIC S9(7).
           03  QBYA-AMT-PL-1-2-3         PIC S9(9)V99.
           03  QBYA-O2-PL-1-2-3          PIC S9(9)V99.
           03  QBYA-INT-PL-1-2-3         PIC S9(9)V99.
           03  QBYA-NO-PL-1-2-4          PIC S9(7).
           03  QBYA-AMT-PL-1-2-4         PIC S9(9)V99.
           03  QBYA-O2-PL-1-2-4          PIC S9(9)V99.
           03  QBYA-INT-PL-1-2-4         PIC S9(9)V99.
           03  QBYA-NO-PL-1-2-5          PIC S9(7).
           03  QBYA-AMT-PL-1-2-5         PIC S9(9)V99.
           03  QBYA-O2-PL-1-2-5          PIC S9(9)V99.
           03  QBYA-INT-PL-1-2-5         PIC S9(9)V99.
           03  QBYA-NO-PL-1-2-6          PIC S9(7).
           03  QBYA-AMT-PL-1-2-6         PIC S9(9)V99.
           03  QBYA-O2-PL-1-2-6          PIC S9(9)V99.
           03  QBYA-INT-PL-1-2-6         PIC S9(9)V99.
           03  QBYA-NO-PL-1-2-7          PIC S9(7).
           03  QBYA-AMT-PL-1-2-7         PIC S9(9)V99.
           03  QBYA-O2-PL-1-2-7          PIC S9(9)V99.
           03  QBYA-INT-PL-1-2-7         PIC S9(9)V99.
           03  QBYA-NO-PL-1-2-8          PIC S9(7).
           03  QBYA-AMT-PL-1-2-8         PIC S9(9)V99.
           03  QBYA-O2-PL-1-2-8          PIC S9(9)V99.
           03  QBYA-INT-PL-1-2-8         PIC S9(9)V99.
           03  QBYA-NO-PL-1-2-9          PIC S9(7).
           03  QBYA-AMT-PL-1-2-9         PIC S9(9)V99.
           03  QBYA-O2-PL-1-2-9          PIC S9(9)V99.
           03  QBYA-INT-PL-1-2-9         PIC S9(9)V99.
           03  QBYA-NO-PL-1-2-10         PIC S9(7).
           03  QBYA-AMT-PL-1-2-10        PIC S9(9)V99.
           03  QBYA-O2-PL-1-2-10         PIC S9(9)V99.
           03  QBYA-INT-PL-1-2-10        PIC S9(9)V99.
           03  QBYA-NO-PL-2-1-1          PIC S9(7).
           03  QBYA-AMT-PL-2-1-1         PIC S9(9)V99.
           03  QBYA-O2-PL-2-1-1          PIC S9(9)V99.
           03  QBYA-INT-PL-2-1-1         PIC S9(9)V99.
           03  QBYA-NO-PL-2-1-2          PIC S9(7).
           03  QBYA-AMT-PL-2-1-2         PIC S9(9)V99.
           03  QBYA-O2-PL-2-1-2          PIC S9(9)V99.
           03  QBYA-INT-PL-2-1-2         PIC S9(9)V99.
           03  QBYA-NO-PL-2-1-3          PIC S9(7).
           03  QBYA-AMT-PL-2-1-3         PIC S9(9)V99.
           03  QBYA-O2-PL-2-1-3          PIC S9(9)V99.
           03  QBYA-INT-PL-2-1-3         PIC S9(9)V99.
           03  QBYA-NO-PL-2-1-4          PIC S9(7).
           03  QBYA-AMT-PL-2-1-4         PIC S9(9)V99.
           03  QBYA-O2-PL-2-1-4          PIC S9(9)V99.
           03  QBYA-INT-PL-2-1-4         PIC S9(9)V99.
           03  QBYA-NO-PL-2-1-5          PIC S9(7).
           03  QBYA-AMT-PL-2-1-5         PIC S9(9)V99.
           03  QBYA-O2-PL-2-1-5          PIC S9(9)V99.
           03  QBYA-INT-PL-2-1-5         PIC S9(9)V99.
           03  QBYA-NO-PL-2-1-6          PIC S9(7).
           03  QBYA-AMT-PL-2-1-6         PIC S9(9)V99.
           03  QBYA-O2-PL-2-1-6          PIC S9(9)V99.
           03  QBYA-INT-PL-2-1-6         PIC S9(9)V99.
           03  QBYA-NO-PL-2-1-7          PIC S9(7).
           03  QBYA-AMT-PL-2-1-7         PIC S9(9)V99.
           03  QBYA-O2-PL-2-1-7          PIC S9(9)V99.
           03  QBYA-INT-PL-2-1-7         PIC S9(9)V99.
           03  QBYA-NO-PL-2-1-8          PIC S9(7).
           03  QBYA-AMT-PL-2-1-8         PIC S9(9)V99.
           03  QBYA-O2-PL-2-1-8          PIC S9(9)V99.
           03  QBYA-INT-PL-2-1-8         PIC S9(9)V99.
           03  QBYA-NO-PL-2-1-9          PIC S9(7).
           03  QBYA-AMT-PL-2-1-9         PIC S9(9)V99.
           03  QBYA-O2-PL-2-1-9          PIC S9(9)V99.
           03  QBYA-INT-PL-2-1-9         PIC S9(9)V99.
           03  QBYA-NO-PL-2-1-10         PIC S9(7).
           03  QBYA-AMT-PL-2-1-10        PIC S9(9)V99.
           03  QBYA-O2-PL-2-1-10         PIC S9(9)V99.
           03  QBYA-INT-PL-2-1-10        PIC S9(9)V99.
           03  QBYA-NO-PL-2-2-1          PIC S9(7).
           03  QBYA-AMT-PL-2-2-1         PIC S9(9)V99.
           03  QBYA-O2-PL-2-2-1          PIC S9(9)V99.
           03  QBYA-INT-PL-2-2-1         PIC S9(9)V99.
           03  QBYA-NO-PL-2-2-2          PIC S9(7).
           03  QBYA-AMT-PL-2-2-2         PIC S9(9)V99.
           03  QBYA-O2-PL-2-2-2          PIC S9(9)V99.
           03  QBYA-INT-PL-2-2-2         PIC S9(9)V99.
           03  QBYA-NO-PL-2-2-3          PIC S9(7).
           03  QBYA-AMT-PL-2-2-3         PIC S9(9)V99.
           03  QBYA-O2-PL-2-2-3          PIC S9(9)V99.
           03  QBYA-INT-PL-2-2-3         PIC S9(9)V99.
           03  QBYA-NO-PL-2-2-4          PIC S9(7).
           03  QBYA-AMT-PL-2-2-4         PIC S9(9)V99.
           03  QBYA-O2-PL-2-2-4          PIC S9(9)V99.
           03  QBYA-INT-PL-2-2-4         PIC S9(9)V99.
           03  QBYA-NO-PL-2-2-5          PIC S9(7).
           03  QBYA-AMT-PL-2-2-5         PIC S9(9)V99.
           03  QBYA-O2-PL-2-2-5          PIC S9(9)V99.
           03  QBYA-INT-PL-2-2-5         PIC S9(9)V99.
           03  QBYA-NO-PL-2-2-6          PIC S9(7).
           03  QBYA-AMT-PL-2-2-6         PIC S9(9)V99.
           03  QBYA-O2-PL-2-2-6          PIC S9(9)V99.
           03  QBYA-INT-PL-2-2-6         PIC S9(9)V99.
           03  QBYA-NO-PL-2-2-7          PIC S9(7).
           03  QBYA-AMT-PL-2-2-7         PIC S9(9)V99.
           03  QBYA-O2-PL-2-2-7          PIC S9(9)V99.
           03  QBYA-INT-PL-2-2-7         PIC S9(9)V99.
           03  QBYA-NO-PL-2-2-8          PIC S9(7).
           03  QBYA-AMT-PL-2-2-8         PIC S9(9)V99.
           03  QBYA-O2-PL-2-2-8          PIC S9(9)V99.
           03  QBYA-INT-PL-2-2-8         PIC S9(9)V99.
           03  QBYA-NO-PL-2-2-9          PIC S9(7).
           03  QBYA-AMT-PL-2-2-9         PIC S9(9)V99.
           03  QBYA-O2-PL-2-2-9          PIC S9(9)V99.
           03  QBYA-INT-PL-2-2-9         PIC S9(9)V99.
           03  QBYA-NO-PL-2-2-10         PIC S9(7).
           03  QBYA-AMT-PL-2-2-10        PIC S9(9)V99.
           03  QBYA-O2-PL-2-2-10         PIC S9(9)V99.
           03  QBYA-INT-PL-2-2-10        PIC S9(9)V99.
           03  QBYA-NO-PL-3-1-1          PIC S9(7).
           03  QBYA-AMT-PL-3-1-1         PIC S9(9)V99.
           03  QBYA-O2-PL-3-1-1          PIC S9(9)V99.
           03  QBYA-INT-PL-3-1-1         PIC S9(9)V99.
           03  QBYA-NO-PL-3-1-2          PIC S9(7).
           03  QBYA-AMT-PL-3-1-2         PIC S9(9)V99.
           03  QBYA-O2-PL-3-1-2          PIC S9(9)V99.
           03  QBYA-INT-PL-3-1-2         PIC S9(9)V99.
           03  QBYA-NO-PL-3-1-3          PIC S9(7).
           03  QBYA-AMT-PL-3-1-3         PIC S9(9)V99.
           03  QBYA-O2-PL-3-1-3          PIC S9(9)V99.
           03  QBYA-INT-PL-3-1-3         PIC S9(9)V99.
           03  QBYA-NO-PL-3-1-4          PIC S9(7).
           03  QBYA-AMT-PL-3-1-4         PIC S9(9)V99.
           03  QBYA-O2-PL-3-1-4          PIC S9(9)V99.
           03  QBYA-INT-PL-3-1-4         PIC S9(9)V99.
           03  QBYA-NO-PL-3-1-5          PIC S9(7).
           03  QBYA-AMT-PL-3-1-5         PIC S9(9)V99.
           03  QBYA-O2-PL-3-1-5          PIC S9(9)V99.
           03  QBYA-INT-PL-3-1-5         PIC S9(9)V99.
           03  QBYA-NO-PL-3-1-6          PIC S9(7).
           03  QBYA-AMT-PL-3-1-6         PIC S9(9)V99.
           03  QBYA-O2-PL-3-1-6          PIC S9(9)V99.
           03  QBYA-INT-PL-3-1-6         PIC S9(9)V99.
           03  QBYA-NO-PL-3-1-7          PIC S9(7).
           03  QBYA-AMT-PL-3-1-7         PIC S9(9)V99.
           03  QBYA-O2-PL-3-1-7          PIC S9(9)V99.
           03  QBYA-INT-PL-3-1-7         PIC S9(9)V99.
           03  QBYA-NO-PL-3-1-8          PIC S9(7).
           03  QBYA-AMT-PL-3-1-8         PIC S9(9)V99.
           03  QBYA-O2-PL-3-1-8          PIC S9(9)V99.
           03  QBYA-INT-PL-3-1-8         PIC S9(9)V99.
           03  QBYA-NO-PL-3-1-9          PIC S9(7).
           03  QBYA-AMT-PL-3-1-9         PIC S9(9)V99.
           03  QBYA-O2-PL-3-1-9          PIC S9(9)V99.
           03  QBYA-INT-PL-3-1-9         PIC S9(9)V99.
           03  QBYA-NO-PL-3-1-10         PIC S9(7).
           03  QBYA-AMT-PL-3-1-10        PIC S9(9)V99.
           03  QBYA-O2-PL-3-1-10         PIC S9(9)V99.
           03  QBYA-INT-PL-3-1-10        PIC S9(9)V99.
           03  QBYA-NO-PL-3-2-1          PIC S9(7).
           03  QBYA-AMT-PL-3-2-1         PIC S9(9)V99.
           03  QBYA-O2-PL-3-2-1          PIC S9(9)V99.
           03  QBYA-INT-PL-3-2-1         PIC S9(9)V99.
           03  QBYA-NO-PL-3-2-2          PIC S9(7).
           03  QBYA-AMT-PL-3-2-2         PIC S9(9)V99.
           03  QBYA-O2-PL-3-2-2          PIC S9(9)V99.
           03  QBYA-INT-PL-3-2-2         PIC S9(9)V99.
           03  QBYA-NO-PL-3-2-3          PIC S9(7).
           03  QBYA-AMT-PL-3-2-3         PIC S9(9)V99.
           03  QBYA-O2-PL-3-2-3          PIC S9(9)V99.
           03  QBYA-INT-PL-3-2-3         PIC S9(9)V99.
           03  QBYA-NO-PL-3-2-4          PIC S9(7).
           03  QBYA-AMT-PL-3-2-4         PIC S9(9)V99.
           03  QBYA-O2-PL-3-2-4          PIC S9(9)V99.
           03  QBYA-INT-PL-3-2-4         PIC S9(9)V99.
           03  QBYA-NO-PL-3-2-5          PIC S9(7).
           03  QBYA-AMT-PL-3-2-5         PIC S9(9)V99.
           03  QBYA-O2-PL-3-2-5          PIC S9(9)V99.
           03  QBYA-INT-PL-3-2-5         PIC S9(9)V99.
           03  QBYA-NO-PL-3-2-6          PIC S9(7).
           03  QBYA-AMT-PL-3-2-6         PIC S9(9)V99.
           03  QBYA-O2-PL-3-2-6          PIC S9(9)V99.
           03  QBYA-INT-PL-3-2-6         PIC S9(9)V99.
           03  QBYA-NO-PL-3-2-7          PIC S9(7).
           03  QBYA-AMT-PL-3-2-7         PIC S9(9)V99.
           03  QBYA-O2-PL-3-2-7          PIC S9(9)V99.
           03  QBYA-INT-PL-3-2-7         PIC S9(9)V99.
           03  QBYA-NO-PL-3-2-8          PIC S9(7).
           03  QBYA-AMT-PL-3-2-8         PIC S9(9)V99.
           03  QBYA-O2-PL-3-2-8          PIC S9(9)V99.
           03  QBYA-INT-PL-3-2-8         PIC S9(9)V99.
           03  QBYA-NO-PL-3-2-9          PIC S9(7).
           03  QBYA-AMT-PL-3-2-9         PIC S9(9)V99.
           03  QBYA-O2-PL-3-2-9          PIC S9(9)V99.
           03  QBYA-INT-PL-3-2-9         PIC S9(9)V99.
           03  QBYA-NO-PL-3-2-10         PIC S9(7).
           03  QBYA-AMT-PL-3-2-10        PIC S9(9)V99.
           03  QBYA-O2-PL-3-2-10         PIC S9(9)V99.
           03  QBYA-INT-PL-3-2-10        PIC S9(9)V99.
           03  QBYA-DLRVPRIN-1-1-1     PIC S9(9)V99.
           03  QBYA-DLRVPRIN-1-1-2     PIC S9(9)V99.
           03  QBYA-DLRVPRIN-1-1-3     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-1-1-1     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-1-1-2     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-1-1-3     PIC S9(9)V99.
           03  QBYA-DLPARVHELD-1-1     PIC S9(9)V99.
           03  QBYA-DLRVPRIN-1-2-1     PIC S9(9)V99.
           03  QBYA-DLRVPRIN-1-2-2     PIC S9(9)V99.
           03  QBYA-DLRVPRIN-1-2-3     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-1-2-1     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-1-2-2     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-1-2-3     PIC S9(9)V99.
           03  QBYA-DLPARVHELD-1-2     PIC S9(9)V99.
           03  QBYA-DLRVPRIN-2-1-1     PIC S9(9)V99.
           03  QBYA-DLRVPRIN-2-1-2     PIC S9(9)V99.
           03  QBYA-DLRVPRIN-2-1-3     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-2-1-1     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-2-1-2     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-2-1-3     PIC S9(9)V99.
           03  QBYA-DLPARVHELD-2-1     PIC S9(9)V99.
           03  QBYA-DLRVPRIN-2-2-1     PIC S9(9)V99.
           03  QBYA-DLRVPRIN-2-2-2     PIC S9(9)V99.
           03  QBYA-DLRVPRIN-2-2-3     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-2-2-1     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-2-2-2     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-2-2-3     PIC S9(9)V99.
           03  QBYA-DLPARVHELD-2-2     PIC S9(9)V99.
           03  QBYA-DLRVPRIN-3-1-1     PIC S9(9)V99.
           03  QBYA-DLRVPRIN-3-1-2     PIC S9(9)V99.
           03  QBYA-DLRVPRIN-3-1-3     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-3-1-1     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-3-1-2     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-3-1-3     PIC S9(9)V99.
           03  QBYA-DLPARVHELD-3-1     PIC S9(9)V99.
           03  QBYA-DLRVPRIN-3-2-1     PIC S9(9)V99.
           03  QBYA-DLRVPRIN-3-2-2     PIC S9(9)V99.
           03  QBYA-DLRVPRIN-3-2-3     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-3-2-1     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-3-2-2     PIC S9(9)V99.
           03  QBYA-DLRVCHGS-3-2-3     PIC S9(9)V99.
           03  QBYA-DLPARVHELD-3-2     PIC S9(9)V99.

       01  QBYB-REC.
           03  QBYB-DATE-CCYYMM        PIC 9(6).
           03  QBYB-BRNO               PIC 9(4).
           03  QBYB-CLASS              PIC 9(3).
           03  QBYB-PL-PRINCOL-1-1     PIC S9(9)V99.
           03  QBYB-PL-INTCOL-1-1      PIC S9(9)V99.
           03  QBYB-PL-LATECOL-1-1     PIC S9(9)V99.
           03  QBYB-PL-O2COL-1-1       PIC S9(9)V99.
           03  QBYB-PL-PRINCOL-1-2     PIC S9(9)V99.
           03  QBYB-PL-INTCOL-1-2      PIC S9(9)V99.
           03  QBYB-PL-LATECOL-1-2     PIC S9(9)V99.
           03  QBYB-PL-O2COL-1-2       PIC S9(9)V99.
           03  QBYB-PL-PRINCOL-1-3     PIC S9(9)V99.
           03  QBYB-PL-INTCOL-1-3      PIC S9(9)V99.
           03  QBYB-PL-LATECOL-1-3     PIC S9(9)V99.
           03  QBYB-PL-O2COL-1-3       PIC S9(9)V99.
           03  QBYB-PL-PRINCOL-1-4     PIC S9(9)V99.
           03  QBYB-PL-INTCOL-1-4      PIC S9(9)V99.
           03  QBYB-PL-LATECOL-1-4     PIC S9(9)V99.
           03  QBYB-PL-O2COL-1-4       PIC S9(9)V99.
           03  QBYB-PL-PRINCOL-1-5     PIC S9(9)V99.
           03  QBYB-PL-INTCOL-1-5      PIC S9(9)V99.
           03  QBYB-PL-LATECOL-1-5     PIC S9(9)V99.
           03  QBYB-PL-O2COL-1-5       PIC S9(9)V99.
           03  QBYB-AMT-SOLD-1         PIC S9(9)V99.
           03  QBYB-AMT-TRANS-1        PIC S9(9)V99.
           03  QBYB-AMT-PURCH-1        PIC S9(9)V99.
           03  QBYB-NO-SOLD-1          PIC S9(7).
           03  QBYB-NO-TRANS-1         PIC S9(7).
           03  QBYB-NO-PURCH-1         PIC S9(7).
           03  QBYB-OTHCOL-1           PIC S9(9)V99.
           03  QBYB-OT2COL-1           PIC S9(9)V99.
           03  QBYB-INTCOL-1           PIC S9(9)V99.
           03  QBYB-LATECOL-1          PIC S9(9)V99.
           03  QBYB-PRINCOL-1          PIC S9(9)V99.
           03  QBYB-DEFCOL-1            PIC S9(9)V99.
           03  QBYB-RNPRIN-1            PIC S9(9)V99.
           03  QBYB-INTREB-1            PIC S9(9)V99.
           03  QBYB-CLREB1-1            PIC S9(9)V99.
           03  QBYB-CLREB2-1            PIC S9(9)V99.
           03  QBYB-CLREB3-1            PIC S9(9)V99.
           03  QBYB-CLREB4-1            PIC S9(9)V99.
           03  QBYB-AHREB-1             PIC S9(9)V99.
           03  QBYB-PPREB-1             PIC S9(9)V99.
           03  QBYB-O1REB-1             PIC S9(9)V99.
           03  QBYB-O2REB-1             PIC S9(9)V99.
           03  QBYB-O3REB-1             PIC S9(9)V99.
           03  QBYB-O4REB-1             PIC S9(9)V99.
           03  QBYB-O5REB-1             PIC S9(9)V99.
           03  QBYB-O6REB-1             PIC S9(9)V99.
           03  QBYB-O7REB-1             PIC S9(9)V99.
           03  QBYB-SCREB-1             PIC S9(9)V99.
           03  QBYB-MFREB-1             PIC S9(9)V99.
           03  QBYB-DFREB-1             PIC S9(9)V99.
           03  QBYB-UN7-REB-1           PIC S9(9)V99.
           03  QBYB-UN8-REB-1           PIC S9(9)V99.
           03  QBYB-UN9-REB-1           PIC S9(9)V99.
           03  QBYB-UN10-REB-1          PIC S9(9)V99.
           03  QBYB-PART-CHGOFF-1       PIC S9(9)V99.
           03  QBYB-ALLOTMENTS-1        PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-1-1     PIC S9(7).
           03  QBYB-INS-NO-WROTE-1-1    PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-1-1 PIC S9(7).
           03  QBYB-INS-AMT-WROTE-1-1   PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-1-2     PIC S9(7).
           03  QBYB-INS-NO-WROTE-1-2    PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-1-2 PIC S9(7).
           03  QBYB-INS-AMT-WROTE-1-2   PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-1-3     PIC S9(7).
           03  QBYB-INS-NO-WROTE-1-3    PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-1-3 PIC S9(7).
           03  QBYB-INS-AMT-WROTE-1-3   PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-1-4     PIC S9(7).
           03  QBYB-INS-NO-WROTE-1-4    PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-1-4 PIC S9(7).
           03  QBYB-INS-AMT-WROTE-1-4   PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-1-5     PIC S9(7).
           03  QBYB-INS-NO-WROTE-1-5    PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-1-5 PIC S9(7).
           03  QBYB-INS-AMT-WROTE-1-5   PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-1-6     PIC S9(7).
           03  QBYB-INS-NO-WROTE-1-6    PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-1-6 PIC S9(7).
           03  QBYB-INS-AMT-WROTE-1-6   PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-1-7     PIC S9(7).
           03  QBYB-INS-NO-WROTE-1-7    PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-1-7 PIC S9(7).
           03  QBYB-INS-AMT-WROTE-1-7   PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-1-8     PIC S9(7).
           03  QBYB-INS-NO-WROTE-1-8    PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-1-8 PIC S9(7).
           03  QBYB-INS-AMT-WROTE-1-8   PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-1-9     PIC S9(7).
           03  QBYB-INS-NO-WROTE-1-9    PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-1-9 PIC S9(7).
           03  QBYB-INS-AMT-WROTE-1-9   PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-1-10     PIC S9(7).
           03  QBYB-INS-NO-WROTE-1-10    PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-1-10 PIC S9(7).
           03  QBYB-INS-AMT-WROTE-1-10   PIC S9(9)V99.
           03  QBYB-FEE-NO-ELIG-1-1     PIC S9(7).
           03  QBYB-FEE-NO-WROTE-1-1    PIC S9(7).
           03  QBYB-FEE-NO-NOTWROTE-1-1 PIC S9(7).
           03  QBYB-FEE-AMT-WROTE-1-1   PIC S9(9)V99.
           03  QBYB-FEE-NO-ELIG-1-2     PIC S9(7).
           03  QBYB-FEE-NO-WROTE-1-2    PIC S9(7).
           03  QBYB-FEE-NO-NOTWROTE-1-2 PIC S9(7).
           03  QBYB-FEE-AMT-WROTE-1-2   PIC S9(9)V99.
           03  QBYB-FEE-NO-ELIG-1-3     PIC S9(7).
           03  QBYB-FEE-NO-WROTE-1-3    PIC S9(7).
           03  QBYB-FEE-NO-NOTWROTE-1-3 PIC S9(7).
           03  QBYB-FEE-AMT-WROTE-1-3   PIC S9(9)V99.
           03  QBYB-OTH-NO-COL-1          PIC S9(7).
           03  QBYB-OT2-NO-COL-1          PIC S9(7).
           03  QBYB-INT-NO-COL-1          PIC S9(7).
           03  QBYB-LATE-NO-COL-1         PIC S9(7).
           03  QBYB-OTH-NO-WAIVED-1       PIC S9(7).
           03  QBYB-OT2-NO-WAIVED-1       PIC S9(7).
           03  QBYB-INT-NO-WAIVED-1       PIC S9(7).
           03  QBYB-LATE-NO-WAIVED-1      PIC S9(7).
           03  QBYB-OTH-AMT-WAIVED-1      PIC S9(9)V99.
           03  QBYB-OT2-AMT-WAIVED-1      PIC S9(9)V99.
           03  QBYB-INT-AMT-WAIVED-1      PIC S9(9)V99.
           03  QBYB-LATE-AMT-WAIVED-1     PIC S9(9)V99.
           03  QBYB-PL-PRINCOL-2-1        PIC S9(9)V99.
           03  QBYB-PL-INTCOL-2-1         PIC S9(9)V99.
           03  QBYB-PL-LATECOL-2-1        PIC S9(9)V99.
           03  QBYB-PL-O2COL-2-1          PIC S9(9)V99.
           03  QBYB-PL-PRINCOL-2-2        PIC S9(9)V99.
           03  QBYB-PL-INTCOL-2-2         PIC S9(9)V99.
           03  QBYB-PL-LATECOL-2-2        PIC S9(9)V99.
           03  QBYB-PL-O2COL-2-2          PIC S9(9)V99.
           03  QBYB-PL-PRINCOL-2-3        PIC S9(9)V99.
           03  QBYB-PL-INTCOL-2-3         PIC S9(9)V99.
           03  QBYB-PL-LATECOL-2-3        PIC S9(9)V99.
           03  QBYB-PL-O2COL-2-3          PIC S9(9)V99.
           03  QBYB-PL-PRINCOL-2-4        PIC S9(9)V99.
           03  QBYB-PL-INTCOL-2-4         PIC S9(9)V99.
           03  QBYB-PL-LATECOL-2-4        PIC S9(9)V99.
           03  QBYB-PL-O2COL-2-4          PIC S9(9)V99.
           03  QBYB-PL-PRINCOL-2-5        PIC S9(9)V99.
           03  QBYB-PL-INTCOL-2-5         PIC S9(9)V99.
           03  QBYB-PL-LATECOL-2-5        PIC S9(9)V99.
           03  QBYB-PL-O2COL-2-5          PIC S9(9)V99.
           03  QBYB-AMT-SOLD-2            PIC S9(9)V99.
           03  QBYB-AMT-TRANS-2           PIC S9(9)V99.
           03  QBYB-AMT-PURCH-2           PIC S9(9)V99.
           03  QBYB-NO-SOLD-2             PIC S9(7).
           03  QBYB-NO-TRANS-2            PIC S9(7).
           03  QBYB-NO-PURCH-2            PIC S9(7).
           03  QBYB-OTHCOL-2              PIC S9(9)V99.
           03  QBYB-OT2COL-2              PIC S9(9)V99.
           03  QBYB-INTCOL-2              PIC S9(9)V99.
           03  QBYB-LATECOL-2             PIC S9(9)V99.
           03  QBYB-PRINCOL-2             PIC S9(9)V99.
           03  QBYB-DEFCOL-2              PIC S9(9)V99.
           03  QBYB-RNPRIN-2              PIC S9(9)V99.
           03  QBYB-INTREB-2              PIC S9(9)V99.
           03  QBYB-CLREB1-2              PIC S9(9)V99.
           03  QBYB-CLREB2-2              PIC S9(9)V99.
           03  QBYB-CLREB3-2              PIC S9(9)V99.
           03  QBYB-CLREB4-2              PIC S9(9)V99.
           03  QBYB-AHREB-2               PIC S9(9)V99.
           03  QBYB-PPREB-2               PIC S9(9)V99.
           03  QBYB-O1REB-2               PIC S9(9)V99.
           03  QBYB-O2REB-2               PIC S9(9)V99.
           03  QBYB-O3REB-2               PIC S9(9)V99.
           03  QBYB-O4REB-2               PIC S9(9)V99.
           03  QBYB-O5REB-2               PIC S9(9)V99.
           03  QBYB-O6REB-2               PIC S9(9)V99.
           03  QBYB-O7REB-2               PIC S9(9)V99.
           03  QBYB-SCREB-2               PIC S9(9)V99.
           03  QBYB-MFREB-2               PIC S9(9)V99.
           03  QBYB-DFREB-2               PIC S9(9)V99.
           03  QBYB-UN7-REB-2             PIC S9(9)V99.
           03  QBYB-UN8-REB-2             PIC S9(9)V99.
           03  QBYB-UN9-REB-2             PIC S9(9)V99.
           03  QBYB-UN10-REB-2            PIC S9(9)V99.
           03  QBYB-PART-CHGOFF-2         PIC S9(9)V99.
           03  QBYB-ALLOTMENTS-2          PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-2-1       PIC S9(7).
           03  QBYB-INS-NO-WROTE-2-1      PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-2-1   PIC S9(7).
           03  QBYB-INS-AMT-WROTE-2-1     PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-2-2       PIC S9(7).
           03  QBYB-INS-NO-WROTE-2-2      PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-2-2   PIC S9(7).
           03  QBYB-INS-AMT-WROTE-2-2     PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-2-3       PIC S9(7).
           03  QBYB-INS-NO-WROTE-2-3      PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-2-3   PIC S9(7).
           03  QBYB-INS-AMT-WROTE-2-3     PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-2-4       PIC S9(7).
           03  QBYB-INS-NO-WROTE-2-4      PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-2-4   PIC S9(7).
           03  QBYB-INS-AMT-WROTE-2-4     PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-2-5       PIC S9(7).
           03  QBYB-INS-NO-WROTE-2-5      PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-2-5   PIC S9(7).
           03  QBYB-INS-AMT-WROTE-2-5     PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-2-6       PIC S9(7).
           03  QBYB-INS-NO-WROTE-2-6      PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-2-6   PIC S9(7).
           03  QBYB-INS-AMT-WROTE-2-6     PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-2-7       PIC S9(7).
           03  QBYB-INS-NO-WROTE-2-7      PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-2-7   PIC S9(7).
           03  QBYB-INS-AMT-WROTE-2-7     PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-2-8       PIC S9(7).
           03  QBYB-INS-NO-WROTE-2-8      PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-2-8   PIC S9(7).
           03  QBYB-INS-AMT-WROTE-2-8     PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-2-9       PIC S9(7).
           03  QBYB-INS-NO-WROTE-2-9      PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-2-9   PIC S9(7).
           03  QBYB-INS-AMT-WROTE-2-9     PIC S9(9)V99.
           03  QBYB-INS-NO-ELIG-2-10      PIC S9(7).
           03  QBYB-INS-NO-WROTE-2-10     PIC S9(7).
           03  QBYB-INS-NO-NOTWROTE-2-10  PIC S9(7).
           03  QBYB-INS-AMT-WROTE-2-10    PIC S9(9)V99.
           03  QBYB-FEE-NO-ELIG-2-1       PIC S9(7).
           03  QBYB-FEE-NO-WROTE-2-1      PIC S9(7).
           03  QBYB-FEE-NO-NOTWROTE-2-1   PIC S9(7).
           03  QBYB-FEE-AMT-WROTE-2-1     PIC S9(9)V99.
           03  QBYB-FEE-NO-ELIG-2-2       PIC S9(7).
           03  QBYB-FEE-NO-WROTE-2-2      PIC S9(7).
           03  QBYB-FEE-NO-NOTWROTE-2-2   PIC S9(7).
           03  QBYB-FEE-AMT-WROTE-2-2     PIC S9(9)V99.
           03  QBYB-FEE-NO-ELIG-2-3       PIC S9(7).
           03  QBYB-FEE-NO-WROTE-2-3      PIC S9(7).
           03  QBYB-FEE-NO-NOTWROTE-2-3   PIC S9(7).
           03  QBYB-FEE-AMT-WROTE-2-3     PIC S9(9)V99.
           03  QBYB-OTH-NO-COL-2          PIC S9(7).
           03  QBYB-OT2-NO-COL-2          PIC S9(7).
           03  QBYB-INT-NO-COL-2          PIC S9(7).
           03  QBYB-LATE-NO-COL-2         PIC S9(7).
           03  QBYB-OTH-NO-WAIVED-2       PIC S9(7).
           03  QBYB-OT2-NO-WAIVED-2       PIC S9(7).
           03  QBYB-INT-NO-WAIVED-2       PIC S9(7).
           03  QBYB-LATE-NO-WAIVED-2      PIC S9(7).
           03  QBYB-OTH-AMT-WAIVED-2      PIC S9(9)V99.
           03  QBYB-OT2-AMT-WAIVED-2      PIC S9(9)V99.
           03  QBYB-INT-AMT-WAIVED-2      PIC S9(9)V99.
           03  QBYB-LATE-AMT-WAIVED-2     PIC S9(9)V99.
           03  QBYB-UNITS-1-1-1           PIC S9(7).
           03  QBYB-OUTBAL-1-1-1          PIC S9(9)V99.
           03  QBYB-UNITS-1-1-2           PIC S9(7).
           03  QBYB-OUTBAL-1-1-2          PIC S9(9)V99.
           03  QBYB-UNITS-1-1-3           PIC S9(7).
           03  QBYB-OUTBAL-1-1-3          PIC S9(9)V99.
           03  QBYB-UNITS-1-1-4           PIC S9(7).
           03  QBYB-OUTBAL-1-1-4          PIC S9(9)V99.
           03  QBYB-UNITS-1-1-5           PIC S9(7).
           03  QBYB-OUTBAL-1-1-5          PIC S9(9)V99.
           03  QBYB-UNITS-1-1-6           PIC S9(7).
           03  QBYB-OUTBAL-1-1-6          PIC S9(9)V99.
           03  QBYB-UNITS-1-1-7           PIC S9(7).
           03  QBYB-OUTBAL-1-1-7          PIC S9(9)V99.
           03  QBYB-UNITS-1-1-8           PIC S9(7).
           03  QBYB-OUTBAL-1-1-8          PIC S9(9)V99.
           03  QBYB-UNITS-1-1-9           PIC S9(7).
           03  QBYB-OUTBAL-1-1-9          PIC S9(9)V99.
           03  QBYB-UNITS-1-1-10          PIC S9(7).
           03  QBYB-OUTBAL-1-1-10         PIC S9(9)V99.
           03  QBYB-UNITS-1-2-1           PIC S9(7).
           03  QBYB-OUTBAL-1-2-1          PIC S9(9)V99.
           03  QBYB-UNITS-1-2-2           PIC S9(7).
           03  QBYB-OUTBAL-1-2-2          PIC S9(9)V99.
           03  QBYB-UNITS-1-2-3           PIC S9(7).
           03  QBYB-OUTBAL-1-2-3          PIC S9(9)V99.
           03  QBYB-UNITS-1-2-4           PIC S9(7).
           03  QBYB-OUTBAL-1-2-4          PIC S9(9)V99.
           03  QBYB-UNITS-1-2-5           PIC S9(7).
           03  QBYB-OUTBAL-1-2-5          PIC S9(9)V99.
           03  QBYB-UNITS-1-2-6           PIC S9(7).
           03  QBYB-OUTBAL-1-2-6          PIC S9(9)V99.
           03  QBYB-UNITS-1-2-7           PIC S9(7).
           03  QBYB-OUTBAL-1-2-7          PIC S9(9)V99.
           03  QBYB-UNITS-1-2-8           PIC S9(7).
           03  QBYB-OUTBAL-1-2-8          PIC S9(9)V99.
           03  QBYB-UNITS-1-2-9           PIC S9(7).
           03  QBYB-OUTBAL-1-2-9          PIC S9(9)V99.
           03  QBYB-UNITS-1-2-10          PIC S9(7).
           03  QBYB-OUTBAL-1-2-10         PIC S9(9)V99.
           03  QBYB-UNITS-2-1-1           PIC S9(7).
           03  QBYB-OUTBAL-2-1-1          PIC S9(9)V99.
           03  QBYB-UNITS-2-1-2           PIC S9(7).
           03  QBYB-OUTBAL-2-1-2          PIC S9(9)V99.
           03  QBYB-UNITS-2-1-3           PIC S9(7).
           03  QBYB-OUTBAL-2-1-3          PIC S9(9)V99.
           03  QBYB-UNITS-2-1-4           PIC S9(7).
           03  QBYB-OUTBAL-2-1-4          PIC S9(9)V99.
           03  QBYB-UNITS-2-1-5           PIC S9(7).
           03  QBYB-OUTBAL-2-1-5          PIC S9(9)V99.
           03  QBYB-UNITS-2-1-6           PIC S9(7).
           03  QBYB-OUTBAL-2-1-6          PIC S9(9)V99.
           03  QBYB-UNITS-2-1-7           PIC S9(7).
           03  QBYB-OUTBAL-2-1-7          PIC S9(9)V99.
           03  QBYB-UNITS-2-1-8           PIC S9(7).
           03  QBYB-OUTBAL-2-1-8          PIC S9(9)V99.
           03  QBYB-UNITS-2-1-9           PIC S9(7).
           03  QBYB-OUTBAL-2-1-9          PIC S9(9)V99.
           03  QBYB-UNITS-2-1-10          PIC S9(7).
           03  QBYB-OUTBAL-2-1-10         PIC S9(9)V99.
           03  QBYB-UNITS-2-2-1           PIC S9(7).
           03  QBYB-OUTBAL-2-2-1          PIC S9(9)V99.
           03  QBYB-UNITS-2-2-2           PIC S9(7).
           03  QBYB-OUTBAL-2-2-2          PIC S9(9)V99.
           03  QBYB-UNITS-2-2-3           PIC S9(7).
           03  QBYB-OUTBAL-2-2-3          PIC S9(9)V99.
           03  QBYB-UNITS-2-2-4           PIC S9(7).
           03  QBYB-OUTBAL-2-2-4          PIC S9(9)V99.
           03  QBYB-UNITS-2-2-5           PIC S9(7).
           03  QBYB-OUTBAL-2-2-5          PIC S9(9)V99.
           03  QBYB-UNITS-2-2-6           PIC S9(7).
           03  QBYB-OUTBAL-2-2-6          PIC S9(9)V99.
           03  QBYB-UNITS-2-2-7           PIC S9(7).
           03  QBYB-OUTBAL-2-2-7          PIC S9(9)V99.
           03  QBYB-UNITS-2-2-8           PIC S9(7).
           03  QBYB-OUTBAL-2-2-8          PIC S9(9)V99.
           03  QBYB-UNITS-2-2-9           PIC S9(7).
           03  QBYB-OUTBAL-2-2-9          PIC S9(9)V99.
           03  QBYB-UNITS-2-2-10          PIC S9(7).
           03  QBYB-OUTBAL-2-2-10         PIC S9(9)V99.
           03  QBYB-UNITS-3-1-1           PIC S9(7).
           03  QBYB-OUTBAL-3-1-1          PIC S9(9)V99.
           03  QBYB-UNITS-3-1-2           PIC S9(7).
           03  QBYB-OUTBAL-3-1-2          PIC S9(9)V99.
           03  QBYB-UNITS-3-1-3           PIC S9(7).
           03  QBYB-OUTBAL-3-1-3          PIC S9(9)V99.
           03  QBYB-UNITS-3-1-4           PIC S9(7).
           03  QBYB-OUTBAL-3-1-4          PIC S9(9)V99.
           03  QBYB-UNITS-3-1-5           PIC S9(7).
           03  QBYB-OUTBAL-3-1-5          PIC S9(9)V99.
           03  QBYB-UNITS-3-1-6           PIC S9(7).
           03  QBYB-OUTBAL-3-1-6          PIC S9(9)V99.
           03  QBYB-UNITS-3-1-7           PIC S9(7).
           03  QBYB-OUTBAL-3-1-7          PIC S9(9)V99.
           03  QBYB-UNITS-3-1-8           PIC S9(7).
           03  QBYB-OUTBAL-3-1-8          PIC S9(9)V99.
           03  QBYB-UNITS-3-1-9           PIC S9(7).
           03  QBYB-OUTBAL-3-1-9          PIC S9(9)V99.
           03  QBYB-UNITS-3-1-10          PIC S9(7).
           03  QBYB-OUTBAL-3-1-10         PIC S9(9)V99.
           03  QBYB-UNITS-3-2-1           PIC S9(7).
           03  QBYB-OUTBAL-3-2-1          PIC S9(9)V99.
           03  QBYB-UNITS-3-2-2           PIC S9(7).
           03  QBYB-OUTBAL-3-2-2          PIC S9(9)V99.
           03  QBYB-UNITS-3-2-3           PIC S9(7).
           03  QBYB-OUTBAL-3-2-3          PIC S9(9)V99.
           03  QBYB-UNITS-3-2-4           PIC S9(7).
           03  QBYB-OUTBAL-3-2-4          PIC S9(9)V99.
           03  QBYB-UNITS-3-2-5           PIC S9(7).
           03  QBYB-OUTBAL-3-2-5          PIC S9(9)V99.
           03  QBYB-UNITS-3-2-6           PIC S9(7).
           03  QBYB-OUTBAL-3-2-6          PIC S9(9)V99.
           03  QBYB-UNITS-3-2-7           PIC S9(7).
           03  QBYB-OUTBAL-3-2-7          PIC S9(9)V99.
           03  QBYB-UNITS-3-2-8           PIC S9(7).
           03  QBYB-OUTBAL-3-2-8          PIC S9(9)V99.
           03  QBYB-UNITS-3-2-9           PIC S9(7).
           03  QBYB-OUTBAL-3-2-9          PIC S9(9)V99.
           03  QBYB-UNITS-3-2-10          PIC S9(7).
           03  QBYB-OUTBAL-3-2-10         PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-1-1    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-1-1   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-1-2    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-1-2   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-1-3    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-1-3   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-1-4    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-1-4   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-1-5    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-1-5   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-1-6    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-1-6   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-1-7    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-1-7   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-1-8    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-1-8   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-1-9    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-1-9   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-1-10   PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-1-10  PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-2-1    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-2-1   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-2-2    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-2-2   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-2-3    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-2-3   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-2-4    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-2-4   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-2-5    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-2-5   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-2-6    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-2-6   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-2-7    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-2-7   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-2-8    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-2-8   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-2-9    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-2-9   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-1-2-10   PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-1-2-10  PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-1-1    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-1-1   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-1-2    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-1-2   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-1-3    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-1-3   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-1-4    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-1-4   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-1-5    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-1-5   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-1-6    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-1-6   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-1-7    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-1-7   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-1-8    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-1-8   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-1-9    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-1-9   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-1-10   PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-1-10  PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-2-1    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-2-1   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-2-2    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-2-2   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-2-3    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-2-3   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-2-4    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-2-4   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-2-5    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-2-5   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-2-6    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-2-6   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-2-7    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-2-7   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-2-8    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-2-8   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-2-9    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-2-9   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-2-2-10   PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-2-2-10  PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-1-1    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-1-1   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-1-2    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-1-2   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-1-3    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-1-3   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-1-4    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-1-4   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-1-5    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-1-5   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-1-6    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-1-6   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-1-7    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-1-7   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-1-8    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-1-8   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-1-9    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-1-9   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-1-10   PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-1-10  PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-2-1    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-2-1   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-2-2    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-2-2   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-2-3    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-2-3   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-2-4    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-2-4   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-2-5    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-2-5   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-2-6    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-2-6   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-2-7    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-2-7   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-2-8    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-2-8   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-2-9    PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-2-9   PIC S9(9)V99.
           03  QBYB-UNITS-BNKRPT-3-2-10   PIC S9(7).
           03  QBYB-OUTBAL-BNKRPT-3-2-10  PIC S9(9)V99.
           03  QBYB-SUMWGTTM              PIC S9(8).
           03  QBYB-SUMWGTAV              PIC S9(9)V99.
           03  QBYB-SUMWGTTA              PIC S9(11)V99.
           03  QBYB-IB-ACCRUED            PIC S9(7)V99.
           03  QBYB-DLDISC-1              PIC S9(9)V99.
           03  QBYB-DLDISC-2              PIC S9(9)V99.
           03  QBYB-DLDISC-3              PIC S9(9)V99.
           03  QBYB-STD-POS               PIC S9(7).
           03  QBYB-30-DAY-MOVED          PIC S9(7).
           03  QBYB-PROJ-INCREASE         PIC S9(7).
           03  QBYB-PROJ-PROFIT           PIC S9(7).
           03  QBYB-PROCEEDS-1            PIC S9(9)V99.
           03  QBYB-PROCEEDS-2            PIC S9(9)V99.
           03  QBYB-LOCKBOX-1             PIC S9(9)V99.
           03  QBYB-LOCKBOX-2             PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-1-1    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-1-2    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-1-3    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-1-4    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-1-5    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-1-6    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-1-7    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-1-8    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-1-9    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-1-10   PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-2-1    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-2-2    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-2-3    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-2-4    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-2-5    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-2-6    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-2-7    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-2-8    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-2-9    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-1-2-10   PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-1-1    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-1-2    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-1-3    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-1-4    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-1-5    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-1-6    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-1-7    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-1-8    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-1-9    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-1-10   PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-2-1    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-2-2    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-2-3    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-2-4    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-2-5    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-2-6    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-2-7    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-2-8    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-2-9    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-2-2-10   PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-1-1    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-1-2    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-1-3    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-1-4    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-1-5    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-1-6    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-1-7    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-1-8    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-1-9    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-1-10   PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-2-1    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-2-2    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-2-3    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-2-4    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-2-5    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-2-6    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-2-7    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-2-8    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-2-9    PIC S9(9)V99.
           03  QBYB-UNEARNED-AMT-3-2-10   PIC S9(9)V99.

       EXEC SQL END DECLARE SECTION END-EXEC.
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S WS COPY MEMBERS
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

       01  LEGEND-BEG-DATE.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING DATE       (F3-ALL) ".

       01  LEGEND-END-DATE.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING DATE       ".

       01  LEGEND-BEG-BRNO.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING BRNO       (F3-ALL) ".

       01  LEGEND-END-BRNO.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING BRNO       ".

       01  LEGEND-BEG-CLASS.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING CLASS      (F3-ALL) ".

       01  LEGEND-END-CLASS.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING CLASS      ".

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
           03  BEG-DATE                PIC 9(06).
           03  END-DATE                PIC 9(06).
           03  BEG-BRNO                PIC 9(04).
           03  END-BRNO                PIC 9(04).
           03  BEG-CLASS               PIC 9(03).
           03  END-CLASS               PIC 9(03).

           03  PROCESS-KEY             PIC 9(01).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

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
           03  FILLER PIC X(30) VALUE "DATE   BRNO CLASS           ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-DATE-CCYYMM                PIC 9(6).
           03  FILLER                         PIC X(01).
           03  D01-BRNO                       PIC 9(4).
           03  FILLER                         PIC X(01).
           03  D01-CLASS                      PIC 9(3).
           03  FILLER                         PIC X(01).
           03  D01-BYA-NO-1-1-1               PIC 9(3).
           03  FILLER                         PIC X(01).
           03  D01-BYB-PL-PRINCOL-1-1         PIC 9(3).
      *%%

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
      *COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
      *COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
      *COPY "LIBGB/GRWORKW.CPY".
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
      *COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBIO/IOBYRN_DEF.CPY".
       COPY "LIBIO/IOBYRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOBYRN_SCN.CPY".

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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      D    STOP FORM-PATHNAME.
           MOVE "B"                  TO BY-PATH-OVERRIDE.

           MOVE BEG-DATE             TO QBY1-WBEG-DATE-CCYYMM.
           MOVE END-DATE             TO QBY1-WEND-DATE-CCYYMM.

           MOVE BEG-BRNO             TO QBY1-WBEG-BRNO.
           MOVE END-BRNO             TO QBY1-WEND-BRNO.

           MOVE BEG-CLASS            TO QBY1-WBEG-CLASS.
           MOVE END-CLASS            TO QBY1-WEND-CLASS.

           PERFORM OPEN-BY1-FILE.
           PERFORM START-BY1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-BY1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( BY-DATE-CCYYMM < BEG-DATE )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( BY-DATE-CCYYMM > END-DATE )
              GO TO MAIN-PROGRAM-END
           ELSE
           IF ( BY-BRNO < BEG-BRNO ) OR
              ( BY-BRNO > END-BRNO )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( BY-CLASS < BEG-CLASS ) OR
              ( BY-CLASS > END-CLASS )
              GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE BY-DATE-CCYYMM            TO D01-DATE-CCYYMM.
           MOVE BY-BRNO                   TO D01-BRNO.
           MOVE BY-CLASS                  TO D01-CLASS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.

           GO TO MAIN-PROGRAM-NEXT.

      *=================================================================
       MAIN-PROGRAM-END.

           PERFORM WRITE-SUMMARY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY SUMMARY TO SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROGRAM COMPLETED!!!" TO SENDMESS-HEAD.
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
           MOVE "IOBYRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOBYRN-QUEUE-POS  TO Q-POS.
           MOVE IOBYRN-COPIES-POS TO C-POS.
           MOVE IOBYRN-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROCESSING..." TO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

           PERFORM GET-GPENV.
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
              GO TO ENTER-PROCESS-KEY.

      *=================================================================
       ENTER-BEG-DATE.

           MOVE LEGEND-BEG-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE LEGEND-END-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-DATE.

           IF ( BEG-DATE > END-DATE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "SNNN060BEG-DATE." TO VDU.
           MOVE ZEROES             TO BEG-DATE VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-DATE." TO VDU.
           MOVE ALL "9"            TO END-DATE VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-BRNO.

           MOVE LEGEND-BEG-BRNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BEG-BRNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRNO.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-BRNO.

      *-----------------------------------------------------------------
       ENTER-END-BRNO.

           MOVE LEGEND-END-BRNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-BRNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRNO.

           IF ( BEG-BRNO > END-BRNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRNO.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRNO.

           MOVE "SNNN040BEG-BRNO." TO VDU.
           MOVE ZEROES             TO BEG-BRNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRNO." TO VDU.
           MOVE ALL "9"            TO END-BRNO VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-CLASS.

           MOVE LEGEND-BEG-CLASS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN030BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-END-CLASS.

           MOVE LEGEND-END-CLASS TO LEGEND.
           PERFORM SEND-LEGEND.

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

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-CLASS.

           MOVE "SNNN030BEG-CLASS." TO VDU.
           MOVE ZEROES             TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-CLASS." TO VDU.
           MOVE ALL "9"            TO END-DATE VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < ZEROES ) OR
              ( VDUNUM0 > 1      )     
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

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

           MOVE "SNNN060BEG-DATE."    TO VDU.
           MOVE BEG-DATE              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-DATE."    TO VDU.
           MOVE END-DATE              TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BEG-BRNO."    TO VDU.
           MOVE BEG-BRNO              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRNO."    TO VDU.
           MOVE END-BRNO              TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN030BEG-CLASS."   TO VDU.
           MOVE BEG-CLASS             TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-CLASS."   TO VDU.
           MOVE END-CLASS             TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN010PROCESS-KEY." TO VDU
           MOVE PROCESS-KEY           TO VDUNUM0.
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

           MOVE BYA1-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING DATE                : "    TO R01-PROMPT.
           MOVE BEG-DATE                              TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE                : "    TO R01-PROMPT.
           MOVE END-DATE                              TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING BRNO                : "    TO R01-PROMPT.
           MOVE BEG-BRNO                              TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    BRNO                : "    TO R01-PROMPT.
           MOVE END-BRNO                              TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS               : "    TO R01-PROMPT.
           MOVE BEG-CLASS                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CLASS               : "    TO R01-PROMPT.
           MOVE END-CLASS                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "BEGINNING ACCTNO              : "    TO R01-PROMPT.
      *    MOVE BEG-ACCTNO                            TO R01-NUMERIC.
      *    MOVE 1                                     TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

      *    MOVE "ENDING    ACCTNO              : "    TO R01-PROMPT.
      *    MOVE END-ACCTNO                            TO R01-NUMERIC.
      *    MOVE 1                                     TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

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
           MOVE "SQL TABLE IO UTILITY - BYFILE"  TO H02-TITLE.
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{04}
           MOVE ALL "="                          TO PR-REC.
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

           MOVE EXT-CONAME-CONAME TO IOBYRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOBYRN-SYSDATE.
           DISPLAY IOBYRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOBYRN-FORM-FIELDS TO WR-BUF.
           MOVE IOBYRN-FORM-LIST TO WR-LIST.
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

           MOVE IOBYRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOBYRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOBYRN_EVA.CPY".
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
      *COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
      *COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".
      *COPY "LIBGB/GRSERCH.CPY".
      *COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
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
       COPY "LIBGB/GBPRINT.CPY".

 
      *-----------------------------------------------------------------
      *    LOAD BYA-REC FROM QBYA-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-BYA-FIELDS.
           MOVE QBYA-DATE-CCYYMM TO BYA-DATE-CCYYMM.
           MOVE QBYA-BRNO TO BYA-BRNO.
           MOVE QBYA-CLASS TO BYA-CLASS.

           IF ( QBYA-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO BYA-LTOUCH-DATE
           ELSE
              MOVE QBYA-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BYA-LTOUCH-DATE.

           MOVE QBYA-NO-1-1-1 TO BYA-NO(1 1 1).
           MOVE QBYA-AMT-1-1-1 TO BYA-AMT(1 1 1).
           MOVE QBYA-O2-1-1-1 TO BYA-O2(1 1 1).
           MOVE QBYA-INT-1-1-1 TO BYA-INT(1 1 1).
           MOVE QBYA-NO-1-1-2 TO BYA-NO(1 1 2).
           MOVE QBYA-AMT-1-1-2 TO BYA-AMT(1 1 2).
           MOVE QBYA-O2-1-1-2 TO BYA-O2(1 1 2).
           MOVE QBYA-INT-1-1-2 TO BYA-INT(1 1 2).
           MOVE QBYA-NO-1-1-3 TO BYA-NO(1 1 3).
           MOVE QBYA-AMT-1-1-3 TO BYA-AMT(1 1 3).
           MOVE QBYA-O2-1-1-3 TO BYA-O2(1 1 3).
           MOVE QBYA-INT-1-1-3 TO BYA-INT(1 1 3).
           MOVE QBYA-NO-1-1-4 TO BYA-NO(1 1 4).
           MOVE QBYA-AMT-1-1-4 TO BYA-AMT(1 1 4).
           MOVE QBYA-O2-1-1-4 TO BYA-O2(1 1 4).
           MOVE QBYA-INT-1-1-4 TO BYA-INT(1 1 4).
           MOVE QBYA-NO-1-1-5 TO BYA-NO(1 1 5).
           MOVE QBYA-AMT-1-1-5 TO BYA-AMT(1 1 5).
           MOVE QBYA-O2-1-1-5 TO BYA-O2(1 1 5).
           MOVE QBYA-INT-1-1-5 TO BYA-INT(1 1 5).
           MOVE QBYA-NO-1-1-6 TO BYA-NO(1 1 6).
           MOVE QBYA-AMT-1-1-6 TO BYA-AMT(1 1 6).
           MOVE QBYA-O2-1-1-6 TO BYA-O2(1 1 6).
           MOVE QBYA-INT-1-1-6 TO BYA-INT(1 1 6).
           MOVE QBYA-NO-1-1-7 TO BYA-NO(1 1 7).
           MOVE QBYA-AMT-1-1-7 TO BYA-AMT(1 1 7).
           MOVE QBYA-O2-1-1-7 TO BYA-O2(1 1 7).
           MOVE QBYA-INT-1-1-7 TO BYA-INT(1 1 7).
           MOVE QBYA-NO-1-1-8 TO BYA-NO(1 1 8).
           MOVE QBYA-AMT-1-1-8 TO BYA-AMT(1 1 8).
           MOVE QBYA-O2-1-1-8 TO BYA-O2(1 1 8).
           MOVE QBYA-INT-1-1-8 TO BYA-INT(1 1 8).
           MOVE QBYA-NO-1-1-9 TO BYA-NO(1 1 9).
           MOVE QBYA-AMT-1-1-9 TO BYA-AMT(1 1 9).
           MOVE QBYA-O2-1-1-9 TO BYA-O2(1 1 9).
           MOVE QBYA-INT-1-1-9 TO BYA-INT(1 1 9).
           MOVE QBYA-NO-1-1-10 TO BYA-NO(1 1 10).
           MOVE QBYA-AMT-1-1-10 TO BYA-AMT(1 1 10).
           MOVE QBYA-O2-1-1-10 TO BYA-O2(1 1 10).
           MOVE QBYA-INT-1-1-10 TO BYA-INT(1 1 10).
           MOVE QBYA-NO-1-1-11 TO BYA-NO(1 1 11).
           MOVE QBYA-AMT-1-1-11 TO BYA-AMT(1 1 11).
           MOVE QBYA-O2-1-1-11 TO BYA-O2(1 1 11).
           MOVE QBYA-INT-1-1-11 TO BYA-INT(1 1 11).
           MOVE QBYA-NO-1-2-1 TO BYA-NO(1 2 1).
           MOVE QBYA-AMT-1-2-1 TO BYA-AMT(1 2 1).
           MOVE QBYA-O2-1-2-1 TO BYA-O2(1 2 1).
           MOVE QBYA-INT-1-2-1 TO BYA-INT(1 2 1).
           MOVE QBYA-NO-1-2-2 TO BYA-NO(1 2 2).
           MOVE QBYA-AMT-1-2-2 TO BYA-AMT(1 2 2).
           MOVE QBYA-O2-1-2-2 TO BYA-O2(1 2 2).
           MOVE QBYA-INT-1-2-2 TO BYA-INT(1 2 2).
           MOVE QBYA-NO-1-2-3 TO BYA-NO(1 2 3).
           MOVE QBYA-AMT-1-2-3 TO BYA-AMT(1 2 3).
           MOVE QBYA-O2-1-2-3 TO BYA-O2(1 2 3).
           MOVE QBYA-INT-1-2-3 TO BYA-INT(1 2 3).
           MOVE QBYA-NO-1-2-4 TO BYA-NO(1 2 4).
           MOVE QBYA-AMT-1-2-4 TO BYA-AMT(1 2 4).
           MOVE QBYA-O2-1-2-4 TO BYA-O2(1 2 4).
           MOVE QBYA-INT-1-2-4 TO BYA-INT(1 2 4).
           MOVE QBYA-NO-1-2-5 TO BYA-NO(1 2 5).
           MOVE QBYA-AMT-1-2-5 TO BYA-AMT(1 2 5).
           MOVE QBYA-O2-1-2-5 TO BYA-O2(1 2 5).
           MOVE QBYA-INT-1-2-5 TO BYA-INT(1 2 5).
           MOVE QBYA-NO-1-2-6 TO BYA-NO(1 2 6).
           MOVE QBYA-AMT-1-2-6 TO BYA-AMT(1 2 6).
           MOVE QBYA-O2-1-2-6 TO BYA-O2(1 2 6).
           MOVE QBYA-INT-1-2-6 TO BYA-INT(1 2 6).
           MOVE QBYA-NO-1-2-7 TO BYA-NO(1 2 7).
           MOVE QBYA-AMT-1-2-7 TO BYA-AMT(1 2 7).
           MOVE QBYA-O2-1-2-7 TO BYA-O2(1 2 7).
           MOVE QBYA-INT-1-2-7 TO BYA-INT(1 2 7).
           MOVE QBYA-NO-1-2-8 TO BYA-NO(1 2 8).
           MOVE QBYA-AMT-1-2-8 TO BYA-AMT(1 2 8).
           MOVE QBYA-O2-1-2-8 TO BYA-O2(1 2 8).
           MOVE QBYA-INT-1-2-8 TO BYA-INT(1 2 8).
           MOVE QBYA-NO-1-2-9 TO BYA-NO(1 2 9).
           MOVE QBYA-AMT-1-2-9 TO BYA-AMT(1 2 9).
           MOVE QBYA-O2-1-2-9 TO BYA-O2(1 2 9).
           MOVE QBYA-INT-1-2-9 TO BYA-INT(1 2 9).
           MOVE QBYA-NO-1-2-10 TO BYA-NO(1 2 10).
           MOVE QBYA-AMT-1-2-10 TO BYA-AMT(1 2 10).
           MOVE QBYA-O2-1-2-10 TO BYA-O2(1 2 10).
           MOVE QBYA-INT-1-2-10 TO BYA-INT(1 2 10).
           MOVE QBYA-NO-1-2-11 TO BYA-NO(1 2 11).
           MOVE QBYA-AMT-1-2-11 TO BYA-AMT(1 2 11).
           MOVE QBYA-O2-1-2-11 TO BYA-O2(1 2 11).
           MOVE QBYA-INT-1-2-11 TO BYA-INT(1 2 11).
           MOVE QBYA-NO-2-1-1 TO BYA-NO(2 1 1).
           MOVE QBYA-AMT-2-1-1 TO BYA-AMT(2 1 1).
           MOVE QBYA-O2-2-1-1 TO BYA-O2(2 1 1).
           MOVE QBYA-INT-2-1-1 TO BYA-INT(2 1 1).
           MOVE QBYA-NO-2-1-2 TO BYA-NO(2 1 2).
           MOVE QBYA-AMT-2-1-2 TO BYA-AMT(2 1 2).
           MOVE QBYA-O2-2-1-2 TO BYA-O2(2 1 2).
           MOVE QBYA-INT-2-1-2 TO BYA-INT(2 1 2).
           MOVE QBYA-NO-2-1-3 TO BYA-NO(2 1 3).
           MOVE QBYA-AMT-2-1-3 TO BYA-AMT(2 1 3).
           MOVE QBYA-O2-2-1-3 TO BYA-O2(2 1 3).
           MOVE QBYA-INT-2-1-3 TO BYA-INT(2 1 3).
           MOVE QBYA-NO-2-1-4 TO BYA-NO(2 1 4).
           MOVE QBYA-AMT-2-1-4 TO BYA-AMT(2 1 4).
           MOVE QBYA-O2-2-1-4 TO BYA-O2(2 1 4).
           MOVE QBYA-INT-2-1-4 TO BYA-INT(2 1 4).
           MOVE QBYA-NO-2-1-5 TO BYA-NO(2 1 5).
           MOVE QBYA-AMT-2-1-5 TO BYA-AMT(2 1 5).
           MOVE QBYA-O2-2-1-5 TO BYA-O2(2 1 5).
           MOVE QBYA-INT-2-1-5 TO BYA-INT(2 1 5).
           MOVE QBYA-NO-2-1-6 TO BYA-NO(2 1 6).
           MOVE QBYA-AMT-2-1-6 TO BYA-AMT(2 1 6).
           MOVE QBYA-O2-2-1-6 TO BYA-O2(2 1 6).
           MOVE QBYA-INT-2-1-6 TO BYA-INT(2 1 6).
           MOVE QBYA-NO-2-1-7 TO BYA-NO(2 1 7).
           MOVE QBYA-AMT-2-1-7 TO BYA-AMT(2 1 7).
           MOVE QBYA-O2-2-1-7 TO BYA-O2(2 1 7).
           MOVE QBYA-INT-2-1-7 TO BYA-INT(2 1 7).
           MOVE QBYA-NO-2-1-8 TO BYA-NO(2 1 8).
           MOVE QBYA-AMT-2-1-8 TO BYA-AMT(2 1 8).
           MOVE QBYA-O2-2-1-8 TO BYA-O2(2 1 8).
           MOVE QBYA-INT-2-1-8 TO BYA-INT(2 1 8).
           MOVE QBYA-NO-2-1-9 TO BYA-NO(2 1 9).
           MOVE QBYA-AMT-2-1-9 TO BYA-AMT(2 1 9).
           MOVE QBYA-O2-2-1-9 TO BYA-O2(2 1 9).
           MOVE QBYA-INT-2-1-9 TO BYA-INT(2 1 9).
           MOVE QBYA-NO-2-1-10 TO BYA-NO(2 1 10).
           MOVE QBYA-AMT-2-1-10 TO BYA-AMT(2 1 10).
           MOVE QBYA-O2-2-1-10 TO BYA-O2(2 1 10).
           MOVE QBYA-INT-2-1-10 TO BYA-INT(2 1 10).
           MOVE QBYA-NO-2-1-11 TO BYA-NO(2 1 11).
           MOVE QBYA-AMT-2-1-11 TO BYA-AMT(2 1 11).
           MOVE QBYA-O2-2-1-11 TO BYA-O2(2 1 11).
           MOVE QBYA-INT-2-1-11 TO BYA-INT(2 1 11).
           MOVE QBYA-NO-2-2-1 TO BYA-NO(2 2 1).
           MOVE QBYA-AMT-2-2-1 TO BYA-AMT(2 2 1).
           MOVE QBYA-O2-2-2-1 TO BYA-O2(2 2 1).
           MOVE QBYA-INT-2-2-1 TO BYA-INT(2 2 1).
           MOVE QBYA-NO-2-2-2 TO BYA-NO(2 2 2).
           MOVE QBYA-AMT-2-2-2 TO BYA-AMT(2 2 2).
           MOVE QBYA-O2-2-2-2 TO BYA-O2(2 2 2).
           MOVE QBYA-INT-2-2-2 TO BYA-INT(2 2 2).
           MOVE QBYA-NO-2-2-3 TO BYA-NO(2 2 3).
           MOVE QBYA-AMT-2-2-3 TO BYA-AMT(2 2 3).
           MOVE QBYA-O2-2-2-3 TO BYA-O2(2 2 3).
           MOVE QBYA-INT-2-2-3 TO BYA-INT(2 2 3).
           MOVE QBYA-NO-2-2-4 TO BYA-NO(2 2 4).
           MOVE QBYA-AMT-2-2-4 TO BYA-AMT(2 2 4).
           MOVE QBYA-O2-2-2-4 TO BYA-O2(2 2 4).
           MOVE QBYA-INT-2-2-4 TO BYA-INT(2 2 4).
           MOVE QBYA-NO-2-2-5 TO BYA-NO(2 2 5).
           MOVE QBYA-AMT-2-2-5 TO BYA-AMT(2 2 5).
           MOVE QBYA-O2-2-2-5 TO BYA-O2(2 2 5).
           MOVE QBYA-INT-2-2-5 TO BYA-INT(2 2 5).
           MOVE QBYA-NO-2-2-6 TO BYA-NO(2 2 6).
           MOVE QBYA-AMT-2-2-6 TO BYA-AMT(2 2 6).
           MOVE QBYA-O2-2-2-6 TO BYA-O2(2 2 6).
           MOVE QBYA-INT-2-2-6 TO BYA-INT(2 2 6).
           MOVE QBYA-NO-2-2-7 TO BYA-NO(2 2 7).
           MOVE QBYA-AMT-2-2-7 TO BYA-AMT(2 2 7).
           MOVE QBYA-O2-2-2-7 TO BYA-O2(2 2 7).
           MOVE QBYA-INT-2-2-7 TO BYA-INT(2 2 7).
           MOVE QBYA-NO-2-2-8 TO BYA-NO(2 2 8).
           MOVE QBYA-AMT-2-2-8 TO BYA-AMT(2 2 8).
           MOVE QBYA-O2-2-2-8 TO BYA-O2(2 2 8).
           MOVE QBYA-INT-2-2-8 TO BYA-INT(2 2 8).
           MOVE QBYA-NO-2-2-9 TO BYA-NO(2 2 9).
           MOVE QBYA-AMT-2-2-9 TO BYA-AMT(2 2 9).
           MOVE QBYA-O2-2-2-9 TO BYA-O2(2 2 9).
           MOVE QBYA-INT-2-2-9 TO BYA-INT(2 2 9).
           MOVE QBYA-NO-2-2-10 TO BYA-NO(2 2 10).
           MOVE QBYA-AMT-2-2-10 TO BYA-AMT(2 2 10).
           MOVE QBYA-O2-2-2-10 TO BYA-O2(2 2 10).
           MOVE QBYA-INT-2-2-10 TO BYA-INT(2 2 10).
           MOVE QBYA-NO-2-2-11 TO BYA-NO(2 2 11).
           MOVE QBYA-AMT-2-2-11 TO BYA-AMT(2 2 11).
           MOVE QBYA-O2-2-2-11 TO BYA-O2(2 2 11).
           MOVE QBYA-INT-2-2-11 TO BYA-INT(2 2 11).
           MOVE QBYA-NO-3-1-1 TO BYA-NO(3 1 1).
           MOVE QBYA-AMT-3-1-1 TO BYA-AMT(3 1 1).
           MOVE QBYA-O2-3-1-1 TO BYA-O2(3 1 1).
           MOVE QBYA-INT-3-1-1 TO BYA-INT(3 1 1).
           MOVE QBYA-NO-3-1-2 TO BYA-NO(3 1 2).
           MOVE QBYA-AMT-3-1-2 TO BYA-AMT(3 1 2).
           MOVE QBYA-O2-3-1-2 TO BYA-O2(3 1 2).
           MOVE QBYA-INT-3-1-2 TO BYA-INT(3 1 2).
           MOVE QBYA-NO-3-1-3 TO BYA-NO(3 1 3).
           MOVE QBYA-AMT-3-1-3 TO BYA-AMT(3 1 3).
           MOVE QBYA-O2-3-1-3 TO BYA-O2(3 1 3).
           MOVE QBYA-INT-3-1-3 TO BYA-INT(3 1 3).
           MOVE QBYA-NO-3-1-4 TO BYA-NO(3 1 4).
           MOVE QBYA-AMT-3-1-4 TO BYA-AMT(3 1 4).
           MOVE QBYA-O2-3-1-4 TO BYA-O2(3 1 4).
           MOVE QBYA-INT-3-1-4 TO BYA-INT(3 1 4).
           MOVE QBYA-NO-3-1-5 TO BYA-NO(3 1 5).
           MOVE QBYA-AMT-3-1-5 TO BYA-AMT(3 1 5).
           MOVE QBYA-O2-3-1-5 TO BYA-O2(3 1 5).
           MOVE QBYA-INT-3-1-5 TO BYA-INT(3 1 5).
           MOVE QBYA-NO-3-1-6 TO BYA-NO(3 1 6).
           MOVE QBYA-AMT-3-1-6 TO BYA-AMT(3 1 6).
           MOVE QBYA-O2-3-1-6 TO BYA-O2(3 1 6).
           MOVE QBYA-INT-3-1-6 TO BYA-INT(3 1 6).
           MOVE QBYA-NO-3-1-7 TO BYA-NO(3 1 7).
           MOVE QBYA-AMT-3-1-7 TO BYA-AMT(3 1 7).
           MOVE QBYA-O2-3-1-7 TO BYA-O2(3 1 7).
           MOVE QBYA-INT-3-1-7 TO BYA-INT(3 1 7).
           MOVE QBYA-NO-3-1-8 TO BYA-NO(3 1 8).
           MOVE QBYA-AMT-3-1-8 TO BYA-AMT(3 1 8).
           MOVE QBYA-O2-3-1-8 TO BYA-O2(3 1 8).
           MOVE QBYA-INT-3-1-8 TO BYA-INT(3 1 8).
           MOVE QBYA-NO-3-1-9 TO BYA-NO(3 1 9).
           MOVE QBYA-AMT-3-1-9 TO BYA-AMT(3 1 9).
           MOVE QBYA-O2-3-1-9 TO BYA-O2(3 1 9).
           MOVE QBYA-INT-3-1-9 TO BYA-INT(3 1 9).
           MOVE QBYA-NO-3-1-10 TO BYA-NO(3 1 10).
           MOVE QBYA-AMT-3-1-10 TO BYA-AMT(3 1 10).
           MOVE QBYA-O2-3-1-10 TO BYA-O2(3 1 10).
           MOVE QBYA-INT-3-1-10 TO BYA-INT(3 1 10).
           MOVE QBYA-NO-3-1-11 TO BYA-NO(3 1 11).
           MOVE QBYA-AMT-3-1-11 TO BYA-AMT(3 1 11).
           MOVE QBYA-O2-3-1-11 TO BYA-O2(3 1 11).
           MOVE QBYA-INT-3-1-11 TO BYA-INT(3 1 11).
           MOVE QBYA-NO-3-2-1 TO BYA-NO(3 2 1).
           MOVE QBYA-AMT-3-2-1 TO BYA-AMT(3 2 1).
           MOVE QBYA-O2-3-2-1 TO BYA-O2(3 2 1).
           MOVE QBYA-INT-3-2-1 TO BYA-INT(3 2 1).
           MOVE QBYA-NO-3-2-2 TO BYA-NO(3 2 2).
           MOVE QBYA-AMT-3-2-2 TO BYA-AMT(3 2 2).
           MOVE QBYA-O2-3-2-2 TO BYA-O2(3 2 2).
           MOVE QBYA-INT-3-2-2 TO BYA-INT(3 2 2).
           MOVE QBYA-NO-3-2-3 TO BYA-NO(3 2 3).
           MOVE QBYA-AMT-3-2-3 TO BYA-AMT(3 2 3).
           MOVE QBYA-O2-3-2-3 TO BYA-O2(3 2 3).
           MOVE QBYA-INT-3-2-3 TO BYA-INT(3 2 3).
           MOVE QBYA-NO-3-2-4 TO BYA-NO(3 2 4).
           MOVE QBYA-AMT-3-2-4 TO BYA-AMT(3 2 4).
           MOVE QBYA-O2-3-2-4 TO BYA-O2(3 2 4).
           MOVE QBYA-INT-3-2-4 TO BYA-INT(3 2 4).
           MOVE QBYA-NO-3-2-5 TO BYA-NO(3 2 5).
           MOVE QBYA-AMT-3-2-5 TO BYA-AMT(3 2 5).
           MOVE QBYA-O2-3-2-5 TO BYA-O2(3 2 5).
           MOVE QBYA-INT-3-2-5 TO BYA-INT(3 2 5).
           MOVE QBYA-NO-3-2-6 TO BYA-NO(3 2 6).
           MOVE QBYA-AMT-3-2-6 TO BYA-AMT(3 2 6).
           MOVE QBYA-O2-3-2-6 TO BYA-O2(3 2 6).
           MOVE QBYA-INT-3-2-6 TO BYA-INT(3 2 6).
           MOVE QBYA-NO-3-2-7 TO BYA-NO(3 2 7).
           MOVE QBYA-AMT-3-2-7 TO BYA-AMT(3 2 7).
           MOVE QBYA-O2-3-2-7 TO BYA-O2(3 2 7).
           MOVE QBYA-INT-3-2-7 TO BYA-INT(3 2 7).
           MOVE QBYA-NO-3-2-8 TO BYA-NO(3 2 8).
           MOVE QBYA-AMT-3-2-8 TO BYA-AMT(3 2 8).
           MOVE QBYA-O2-3-2-8 TO BYA-O2(3 2 8).
           MOVE QBYA-INT-3-2-8 TO BYA-INT(3 2 8).
           MOVE QBYA-NO-3-2-9 TO BYA-NO(3 2 9).
           MOVE QBYA-AMT-3-2-9 TO BYA-AMT(3 2 9).
           MOVE QBYA-O2-3-2-9 TO BYA-O2(3 2 9).
           MOVE QBYA-INT-3-2-9 TO BYA-INT(3 2 9).
           MOVE QBYA-NO-3-2-10 TO BYA-NO(3 2 10).
           MOVE QBYA-AMT-3-2-10 TO BYA-AMT(3 2 10).
           MOVE QBYA-O2-3-2-10 TO BYA-O2(3 2 10).
           MOVE QBYA-INT-3-2-10 TO BYA-INT(3 2 10).
           MOVE QBYA-NO-3-2-11 TO BYA-NO(3 2 11).
           MOVE QBYA-AMT-3-2-11 TO BYA-AMT(3 2 11).
           MOVE QBYA-O2-3-2-11 TO BYA-O2(3 2 11).
           MOVE QBYA-INT-3-2-11 TO BYA-INT(3 2 11).
           MOVE QBYA-NO-REPO-1-1-1 TO BYA-NO-REPO(1 1 1).
           MOVE QBYA-AMT-REPO-1-1-1 TO BYA-AMT-REPO(1 1 1).
           MOVE QBYA-O2-REPO-1-1-1 TO BYA-O2-REPO(1 1 1).
           MOVE QBYA-INT-REPO-1-1-1 TO BYA-INT-REPO(1 1 1).
           MOVE QBYA-NO-REPO-1-1-2 TO BYA-NO-REPO(1 1 2).
           MOVE QBYA-AMT-REPO-1-1-2 TO BYA-AMT-REPO(1 1 2).
           MOVE QBYA-O2-REPO-1-1-2 TO BYA-O2-REPO(1 1 2).
           MOVE QBYA-INT-REPO-1-1-2 TO BYA-INT-REPO(1 1 2).
           MOVE QBYA-NO-REPO-1-1-3 TO BYA-NO-REPO(1 1 3).
           MOVE QBYA-AMT-REPO-1-1-3 TO BYA-AMT-REPO(1 1 3).
           MOVE QBYA-O2-REPO-1-1-3 TO BYA-O2-REPO(1 1 3).
           MOVE QBYA-INT-REPO-1-1-3 TO BYA-INT-REPO(1 1 3).
           MOVE QBYA-NO-REPO-1-1-4 TO BYA-NO-REPO(1 1 4).
           MOVE QBYA-AMT-REPO-1-1-4 TO BYA-AMT-REPO(1 1 4).
           MOVE QBYA-O2-REPO-1-1-4 TO BYA-O2-REPO(1 1 4).
           MOVE QBYA-INT-REPO-1-1-4 TO BYA-INT-REPO(1 1 4).
           MOVE QBYA-NO-REPO-1-1-5 TO BYA-NO-REPO(1 1 5).
           MOVE QBYA-AMT-REPO-1-1-5 TO BYA-AMT-REPO(1 1 5).
           MOVE QBYA-O2-REPO-1-1-5 TO BYA-O2-REPO(1 1 5).
           MOVE QBYA-INT-REPO-1-1-5 TO BYA-INT-REPO(1 1 5).
           MOVE QBYA-NO-REPO-1-2-1 TO BYA-NO-REPO(1 2 1).
           MOVE QBYA-AMT-REPO-1-2-1 TO BYA-AMT-REPO(1 2 1).
           MOVE QBYA-O2-REPO-1-2-1 TO BYA-O2-REPO(1 2 1).
           MOVE QBYA-INT-REPO-1-2-1 TO BYA-INT-REPO(1 2 1).
           MOVE QBYA-NO-REPO-1-2-2 TO BYA-NO-REPO(1 2 2).
           MOVE QBYA-AMT-REPO-1-2-2 TO BYA-AMT-REPO(1 2 2).
           MOVE QBYA-O2-REPO-1-2-2 TO BYA-O2-REPO(1 2 2).
           MOVE QBYA-INT-REPO-1-2-2 TO BYA-INT-REPO(1 2 2).
           MOVE QBYA-NO-REPO-1-2-3 TO BYA-NO-REPO(1 2 3).
           MOVE QBYA-AMT-REPO-1-2-3 TO BYA-AMT-REPO(1 2 3).
           MOVE QBYA-O2-REPO-1-2-3 TO BYA-O2-REPO(1 2 3).
           MOVE QBYA-INT-REPO-1-2-3 TO BYA-INT-REPO(1 2 3).
           MOVE QBYA-NO-REPO-1-2-4 TO BYA-NO-REPO(1 2 4).
           MOVE QBYA-AMT-REPO-1-2-4 TO BYA-AMT-REPO(1 2 4).
           MOVE QBYA-O2-REPO-1-2-4 TO BYA-O2-REPO(1 2 4).
           MOVE QBYA-INT-REPO-1-2-4 TO BYA-INT-REPO(1 2 4).
           MOVE QBYA-NO-REPO-1-2-5 TO BYA-NO-REPO(1 2 5).
           MOVE QBYA-AMT-REPO-1-2-5 TO BYA-AMT-REPO(1 2 5).
           MOVE QBYA-O2-REPO-1-2-5 TO BYA-O2-REPO(1 2 5).
           MOVE QBYA-INT-REPO-1-2-5 TO BYA-INT-REPO(1 2 5).
           MOVE QBYA-NO-REPO-2-1-1 TO BYA-NO-REPO(2 1 1).
           MOVE QBYA-AMT-REPO-2-1-1 TO BYA-AMT-REPO(2 1 1).
           MOVE QBYA-O2-REPO-2-1-1 TO BYA-O2-REPO(2 1 1).
           MOVE QBYA-INT-REPO-2-1-1 TO BYA-INT-REPO(2 1 1).
           MOVE QBYA-NO-REPO-2-1-2 TO BYA-NO-REPO(2 1 2).
           MOVE QBYA-AMT-REPO-2-1-2 TO BYA-AMT-REPO(2 1 2).
           MOVE QBYA-O2-REPO-2-1-2 TO BYA-O2-REPO(2 1 2).
           MOVE QBYA-INT-REPO-2-1-2 TO BYA-INT-REPO(2 1 2).
           MOVE QBYA-NO-REPO-2-1-3 TO BYA-NO-REPO(2 1 3).
           MOVE QBYA-AMT-REPO-2-1-3 TO BYA-AMT-REPO(2 1 3).
           MOVE QBYA-O2-REPO-2-1-3 TO BYA-O2-REPO(2 1 3).
           MOVE QBYA-INT-REPO-2-1-3 TO BYA-INT-REPO(2 1 3).
           MOVE QBYA-NO-REPO-2-1-4 TO BYA-NO-REPO(2 1 4).
           MOVE QBYA-AMT-REPO-2-1-4 TO BYA-AMT-REPO(2 1 4).
           MOVE QBYA-O2-REPO-2-1-4 TO BYA-O2-REPO(2 1 4).
           MOVE QBYA-INT-REPO-2-1-4 TO BYA-INT-REPO(2 1 4).
           MOVE QBYA-NO-REPO-2-1-5 TO BYA-NO-REPO(2 1 5).
           MOVE QBYA-AMT-REPO-2-1-5 TO BYA-AMT-REPO(2 1 5).
           MOVE QBYA-O2-REPO-2-1-5 TO BYA-O2-REPO(2 1 5).
           MOVE QBYA-INT-REPO-2-1-5 TO BYA-INT-REPO(2 1 5).
           MOVE QBYA-NO-REPO-2-2-1 TO BYA-NO-REPO(2 2 1).
           MOVE QBYA-AMT-REPO-2-2-1 TO BYA-AMT-REPO(2 2 1).
           MOVE QBYA-O2-REPO-2-2-1 TO BYA-O2-REPO(2 2 1).
           MOVE QBYA-INT-REPO-2-2-1 TO BYA-INT-REPO(2 2 1).
           MOVE QBYA-NO-REPO-2-2-2 TO BYA-NO-REPO(2 2 2).
           MOVE QBYA-AMT-REPO-2-2-2 TO BYA-AMT-REPO(2 2 2).
           MOVE QBYA-O2-REPO-2-2-2 TO BYA-O2-REPO(2 2 2).
           MOVE QBYA-INT-REPO-2-2-2 TO BYA-INT-REPO(2 2 2).
           MOVE QBYA-NO-REPO-2-2-3 TO BYA-NO-REPO(2 2 3).
           MOVE QBYA-AMT-REPO-2-2-3 TO BYA-AMT-REPO(2 2 3).
           MOVE QBYA-O2-REPO-2-2-3 TO BYA-O2-REPO(2 2 3).
           MOVE QBYA-INT-REPO-2-2-3 TO BYA-INT-REPO(2 2 3).
           MOVE QBYA-NO-REPO-2-2-4 TO BYA-NO-REPO(2 2 4).
           MOVE QBYA-AMT-REPO-2-2-4 TO BYA-AMT-REPO(2 2 4).
           MOVE QBYA-O2-REPO-2-2-4 TO BYA-O2-REPO(2 2 4).
           MOVE QBYA-INT-REPO-2-2-4 TO BYA-INT-REPO(2 2 4).
           MOVE QBYA-NO-REPO-2-2-5 TO BYA-NO-REPO(2 2 5).
           MOVE QBYA-AMT-REPO-2-2-5 TO BYA-AMT-REPO(2 2 5).
           MOVE QBYA-O2-REPO-2-2-5 TO BYA-O2-REPO(2 2 5).
           MOVE QBYA-INT-REPO-2-2-5 TO BYA-INT-REPO(2 2 5).
           MOVE QBYA-NO-REPO-3-1-1 TO BYA-NO-REPO(3 1 1).
           MOVE QBYA-AMT-REPO-3-1-1 TO BYA-AMT-REPO(3 1 1).
           MOVE QBYA-O2-REPO-3-1-1 TO BYA-O2-REPO(3 1 1).
           MOVE QBYA-INT-REPO-3-1-1 TO BYA-INT-REPO(3 1 1).
           MOVE QBYA-NO-REPO-3-1-2 TO BYA-NO-REPO(3 1 2).
           MOVE QBYA-AMT-REPO-3-1-2 TO BYA-AMT-REPO(3 1 2).
           MOVE QBYA-O2-REPO-3-1-2 TO BYA-O2-REPO(3 1 2).
           MOVE QBYA-INT-REPO-3-1-2 TO BYA-INT-REPO(3 1 2).
           MOVE QBYA-NO-REPO-3-1-3 TO BYA-NO-REPO(3 1 3).
           MOVE QBYA-AMT-REPO-3-1-3 TO BYA-AMT-REPO(3 1 3).
           MOVE QBYA-O2-REPO-3-1-3 TO BYA-O2-REPO(3 1 3).
           MOVE QBYA-INT-REPO-3-1-3 TO BYA-INT-REPO(3 1 3).
           MOVE QBYA-NO-REPO-3-1-4 TO BYA-NO-REPO(3 1 4).
           MOVE QBYA-AMT-REPO-3-1-4 TO BYA-AMT-REPO(3 1 4).
           MOVE QBYA-O2-REPO-3-1-4 TO BYA-O2-REPO(3 1 4).
           MOVE QBYA-INT-REPO-3-1-4 TO BYA-INT-REPO(3 1 4).
           MOVE QBYA-NO-REPO-3-1-5 TO BYA-NO-REPO(3 1 5).
           MOVE QBYA-AMT-REPO-3-1-5 TO BYA-AMT-REPO(3 1 5).
           MOVE QBYA-O2-REPO-3-1-5 TO BYA-O2-REPO(3 1 5).
           MOVE QBYA-INT-REPO-3-1-5 TO BYA-INT-REPO(3 1 5).
           MOVE QBYA-NO-REPO-3-2-1 TO BYA-NO-REPO(3 2 1).
           MOVE QBYA-AMT-REPO-3-2-1 TO BYA-AMT-REPO(3 2 1).
           MOVE QBYA-O2-REPO-3-2-1 TO BYA-O2-REPO(3 2 1).
           MOVE QBYA-INT-REPO-3-2-1 TO BYA-INT-REPO(3 2 1).
           MOVE QBYA-NO-REPO-3-2-2 TO BYA-NO-REPO(3 2 2).
           MOVE QBYA-AMT-REPO-3-2-2 TO BYA-AMT-REPO(3 2 2).
           MOVE QBYA-O2-REPO-3-2-2 TO BYA-O2-REPO(3 2 2).
           MOVE QBYA-INT-REPO-3-2-2 TO BYA-INT-REPO(3 2 2).
           MOVE QBYA-NO-REPO-3-2-3 TO BYA-NO-REPO(3 2 3).
           MOVE QBYA-AMT-REPO-3-2-3 TO BYA-AMT-REPO(3 2 3).
           MOVE QBYA-O2-REPO-3-2-3 TO BYA-O2-REPO(3 2 3).
           MOVE QBYA-INT-REPO-3-2-3 TO BYA-INT-REPO(3 2 3).
           MOVE QBYA-NO-REPO-3-2-4 TO BYA-NO-REPO(3 2 4).
           MOVE QBYA-AMT-REPO-3-2-4 TO BYA-AMT-REPO(3 2 4).
           MOVE QBYA-O2-REPO-3-2-4 TO BYA-O2-REPO(3 2 4).
           MOVE QBYA-INT-REPO-3-2-4 TO BYA-INT-REPO(3 2 4).
           MOVE QBYA-NO-REPO-3-2-5 TO BYA-NO-REPO(3 2 5).
           MOVE QBYA-AMT-REPO-3-2-5 TO BYA-AMT-REPO(3 2 5).
           MOVE QBYA-O2-REPO-3-2-5 TO BYA-O2-REPO(3 2 5).
           MOVE QBYA-INT-REPO-3-2-5 TO BYA-INT-REPO(3 2 5).
           MOVE QBYA-NO-PL-1-1-1 TO BYA-NO-PL(1 1 1).
           MOVE QBYA-AMT-PL-1-1-1 TO BYA-AMT-PL(1 1 1).
           MOVE QBYA-O2-PL-1-1-1 TO BYA-O2-PL(1 1 1).
           MOVE QBYA-INT-PL-1-1-1 TO BYA-INT-PL(1 1 1).
           MOVE QBYA-NO-PL-1-1-2 TO BYA-NO-PL(1 1 2).
           MOVE QBYA-AMT-PL-1-1-2 TO BYA-AMT-PL(1 1 2).
           MOVE QBYA-O2-PL-1-1-2 TO BYA-O2-PL(1 1 2).
           MOVE QBYA-INT-PL-1-1-2 TO BYA-INT-PL(1 1 2).
           MOVE QBYA-NO-PL-1-1-3 TO BYA-NO-PL(1 1 3).
           MOVE QBYA-AMT-PL-1-1-3 TO BYA-AMT-PL(1 1 3).
           MOVE QBYA-O2-PL-1-1-3 TO BYA-O2-PL(1 1 3).
           MOVE QBYA-INT-PL-1-1-3 TO BYA-INT-PL(1 1 3).
           MOVE QBYA-NO-PL-1-1-4 TO BYA-NO-PL(1 1 4).
           MOVE QBYA-AMT-PL-1-1-4 TO BYA-AMT-PL(1 1 4).
           MOVE QBYA-O2-PL-1-1-4 TO BYA-O2-PL(1 1 4).
           MOVE QBYA-INT-PL-1-1-4 TO BYA-INT-PL(1 1 4).
           MOVE QBYA-NO-PL-1-1-5 TO BYA-NO-PL(1 1 5).
           MOVE QBYA-AMT-PL-1-1-5 TO BYA-AMT-PL(1 1 5).
           MOVE QBYA-O2-PL-1-1-5 TO BYA-O2-PL(1 1 5).
           MOVE QBYA-INT-PL-1-1-5 TO BYA-INT-PL(1 1 5).
           MOVE QBYA-NO-PL-1-1-6 TO BYA-NO-PL(1 1 6).
           MOVE QBYA-AMT-PL-1-1-6 TO BYA-AMT-PL(1 1 6).
           MOVE QBYA-O2-PL-1-1-6 TO BYA-O2-PL(1 1 6).
           MOVE QBYA-INT-PL-1-1-6 TO BYA-INT-PL(1 1 6).
           MOVE QBYA-NO-PL-1-1-7 TO BYA-NO-PL(1 1 7).
           MOVE QBYA-AMT-PL-1-1-7 TO BYA-AMT-PL(1 1 7).
           MOVE QBYA-O2-PL-1-1-7 TO BYA-O2-PL(1 1 7).
           MOVE QBYA-INT-PL-1-1-7 TO BYA-INT-PL(1 1 7).
           MOVE QBYA-NO-PL-1-1-8 TO BYA-NO-PL(1 1 8).
           MOVE QBYA-AMT-PL-1-1-8 TO BYA-AMT-PL(1 1 8).
           MOVE QBYA-O2-PL-1-1-8 TO BYA-O2-PL(1 1 8).
           MOVE QBYA-INT-PL-1-1-8 TO BYA-INT-PL(1 1 8).
           MOVE QBYA-NO-PL-1-1-9 TO BYA-NO-PL(1 1 9).
           MOVE QBYA-AMT-PL-1-1-9 TO BYA-AMT-PL(1 1 9).
           MOVE QBYA-O2-PL-1-1-9 TO BYA-O2-PL(1 1 9).
           MOVE QBYA-INT-PL-1-1-9 TO BYA-INT-PL(1 1 9).
           MOVE QBYA-NO-PL-1-1-10 TO BYA-NO-PL(1 1 10).
           MOVE QBYA-AMT-PL-1-1-10 TO BYA-AMT-PL(1 1 10).
           MOVE QBYA-O2-PL-1-1-10 TO BYA-O2-PL(1 1 10).
           MOVE QBYA-INT-PL-1-1-10 TO BYA-INT-PL(1 1 10).
           MOVE QBYA-NO-PL-1-2-1 TO BYA-NO-PL(1 2 1).
           MOVE QBYA-AMT-PL-1-2-1 TO BYA-AMT-PL(1 2 1).
           MOVE QBYA-O2-PL-1-2-1 TO BYA-O2-PL(1 2 1).
           MOVE QBYA-INT-PL-1-2-1 TO BYA-INT-PL(1 2 1).
           MOVE QBYA-NO-PL-1-2-2 TO BYA-NO-PL(1 2 2).
           MOVE QBYA-AMT-PL-1-2-2 TO BYA-AMT-PL(1 2 2).
           MOVE QBYA-O2-PL-1-2-2 TO BYA-O2-PL(1 2 2).
           MOVE QBYA-INT-PL-1-2-2 TO BYA-INT-PL(1 2 2).
           MOVE QBYA-NO-PL-1-2-3 TO BYA-NO-PL(1 2 3).
           MOVE QBYA-AMT-PL-1-2-3 TO BYA-AMT-PL(1 2 3).
           MOVE QBYA-O2-PL-1-2-3 TO BYA-O2-PL(1 2 3).
           MOVE QBYA-INT-PL-1-2-3 TO BYA-INT-PL(1 2 3).
           MOVE QBYA-NO-PL-1-2-4 TO BYA-NO-PL(1 2 4).
           MOVE QBYA-AMT-PL-1-2-4 TO BYA-AMT-PL(1 2 4).
           MOVE QBYA-O2-PL-1-2-4 TO BYA-O2-PL(1 2 4).
           MOVE QBYA-INT-PL-1-2-4 TO BYA-INT-PL(1 2 4).
           MOVE QBYA-NO-PL-1-2-5 TO BYA-NO-PL(1 2 5).
           MOVE QBYA-AMT-PL-1-2-5 TO BYA-AMT-PL(1 2 5).
           MOVE QBYA-O2-PL-1-2-5 TO BYA-O2-PL(1 2 5).
           MOVE QBYA-INT-PL-1-2-5 TO BYA-INT-PL(1 2 5).
           MOVE QBYA-NO-PL-1-2-6 TO BYA-NO-PL(1 2 6).
           MOVE QBYA-AMT-PL-1-2-6 TO BYA-AMT-PL(1 2 6).
           MOVE QBYA-O2-PL-1-2-6 TO BYA-O2-PL(1 2 6).
           MOVE QBYA-INT-PL-1-2-6 TO BYA-INT-PL(1 2 6).
           MOVE QBYA-NO-PL-1-2-7 TO BYA-NO-PL(1 2 7).
           MOVE QBYA-AMT-PL-1-2-7 TO BYA-AMT-PL(1 2 7).
           MOVE QBYA-O2-PL-1-2-7 TO BYA-O2-PL(1 2 7).
           MOVE QBYA-INT-PL-1-2-7 TO BYA-INT-PL(1 2 7).
           MOVE QBYA-NO-PL-1-2-8 TO BYA-NO-PL(1 2 8).
           MOVE QBYA-AMT-PL-1-2-8 TO BYA-AMT-PL(1 2 8).
           MOVE QBYA-O2-PL-1-2-8 TO BYA-O2-PL(1 2 8).
           MOVE QBYA-INT-PL-1-2-8 TO BYA-INT-PL(1 2 8).
           MOVE QBYA-NO-PL-1-2-9 TO BYA-NO-PL(1 2 9).
           MOVE QBYA-AMT-PL-1-2-9 TO BYA-AMT-PL(1 2 9).
           MOVE QBYA-O2-PL-1-2-9 TO BYA-O2-PL(1 2 9).
           MOVE QBYA-INT-PL-1-2-9 TO BYA-INT-PL(1 2 9).
           MOVE QBYA-NO-PL-1-2-10 TO BYA-NO-PL(1 2 10).
           MOVE QBYA-AMT-PL-1-2-10 TO BYA-AMT-PL(1 2 10).
           MOVE QBYA-O2-PL-1-2-10 TO BYA-O2-PL(1 2 10).
           MOVE QBYA-INT-PL-1-2-10 TO BYA-INT-PL(1 2 10).
           MOVE QBYA-NO-PL-2-1-1 TO BYA-NO-PL(2 1 1).
           MOVE QBYA-AMT-PL-2-1-1 TO BYA-AMT-PL(2 1 1).
           MOVE QBYA-O2-PL-2-1-1 TO BYA-O2-PL(2 1 1).
           MOVE QBYA-INT-PL-2-1-1 TO BYA-INT-PL(2 1 1).
           MOVE QBYA-NO-PL-2-1-2 TO BYA-NO-PL(2 1 2).
           MOVE QBYA-AMT-PL-2-1-2 TO BYA-AMT-PL(2 1 2).
           MOVE QBYA-O2-PL-2-1-2 TO BYA-O2-PL(2 1 2).
           MOVE QBYA-INT-PL-2-1-2 TO BYA-INT-PL(2 1 2).
           MOVE QBYA-NO-PL-2-1-3 TO BYA-NO-PL(2 1 3).
           MOVE QBYA-AMT-PL-2-1-3 TO BYA-AMT-PL(2 1 3).
           MOVE QBYA-O2-PL-2-1-3 TO BYA-O2-PL(2 1 3).
           MOVE QBYA-INT-PL-2-1-3 TO BYA-INT-PL(2 1 3).
           MOVE QBYA-NO-PL-2-1-4 TO BYA-NO-PL(2 1 4).
           MOVE QBYA-AMT-PL-2-1-4 TO BYA-AMT-PL(2 1 4).
           MOVE QBYA-O2-PL-2-1-4 TO BYA-O2-PL(2 1 4).
           MOVE QBYA-INT-PL-2-1-4 TO BYA-INT-PL(2 1 4).
           MOVE QBYA-NO-PL-2-1-5 TO BYA-NO-PL(2 1 5).
           MOVE QBYA-AMT-PL-2-1-5 TO BYA-AMT-PL(2 1 5).
           MOVE QBYA-O2-PL-2-1-5 TO BYA-O2-PL(2 1 5).
           MOVE QBYA-INT-PL-2-1-5 TO BYA-INT-PL(2 1 5).
           MOVE QBYA-NO-PL-2-1-6 TO BYA-NO-PL(2 1 6).
           MOVE QBYA-AMT-PL-2-1-6 TO BYA-AMT-PL(2 1 6).
           MOVE QBYA-O2-PL-2-1-6 TO BYA-O2-PL(2 1 6).
           MOVE QBYA-INT-PL-2-1-6 TO BYA-INT-PL(2 1 6).
           MOVE QBYA-NO-PL-2-1-7 TO BYA-NO-PL(2 1 7).
           MOVE QBYA-AMT-PL-2-1-7 TO BYA-AMT-PL(2 1 7).
           MOVE QBYA-O2-PL-2-1-7 TO BYA-O2-PL(2 1 7).
           MOVE QBYA-INT-PL-2-1-7 TO BYA-INT-PL(2 1 7).
           MOVE QBYA-NO-PL-2-1-8 TO BYA-NO-PL(2 1 8).
           MOVE QBYA-AMT-PL-2-1-8 TO BYA-AMT-PL(2 1 8).
           MOVE QBYA-O2-PL-2-1-8 TO BYA-O2-PL(2 1 8).
           MOVE QBYA-INT-PL-2-1-8 TO BYA-INT-PL(2 1 8).
           MOVE QBYA-NO-PL-2-1-9 TO BYA-NO-PL(2 1 9).
           MOVE QBYA-AMT-PL-2-1-9 TO BYA-AMT-PL(2 1 9).
           MOVE QBYA-O2-PL-2-1-9 TO BYA-O2-PL(2 1 9).
           MOVE QBYA-INT-PL-2-1-9 TO BYA-INT-PL(2 1 9).
           MOVE QBYA-NO-PL-2-1-10 TO BYA-NO-PL(2 1 10).
           MOVE QBYA-AMT-PL-2-1-10 TO BYA-AMT-PL(2 1 10).
           MOVE QBYA-O2-PL-2-1-10 TO BYA-O2-PL(2 1 10).
           MOVE QBYA-INT-PL-2-1-10 TO BYA-INT-PL(2 1 10).
           MOVE QBYA-NO-PL-2-2-1 TO BYA-NO-PL(2 2 1).
           MOVE QBYA-AMT-PL-2-2-1 TO BYA-AMT-PL(2 2 1).
           MOVE QBYA-O2-PL-2-2-1 TO BYA-O2-PL(2 2 1).
           MOVE QBYA-INT-PL-2-2-1 TO BYA-INT-PL(2 2 1).
           MOVE QBYA-NO-PL-2-2-2 TO BYA-NO-PL(2 2 2).
           MOVE QBYA-AMT-PL-2-2-2 TO BYA-AMT-PL(2 2 2).
           MOVE QBYA-O2-PL-2-2-2 TO BYA-O2-PL(2 2 2).
           MOVE QBYA-INT-PL-2-2-2 TO BYA-INT-PL(2 2 2).
           MOVE QBYA-NO-PL-2-2-3 TO BYA-NO-PL(2 2 3).
           MOVE QBYA-AMT-PL-2-2-3 TO BYA-AMT-PL(2 2 3).
           MOVE QBYA-O2-PL-2-2-3 TO BYA-O2-PL(2 2 3).
           MOVE QBYA-INT-PL-2-2-3 TO BYA-INT-PL(2 2 3).
           MOVE QBYA-NO-PL-2-2-4 TO BYA-NO-PL(2 2 4).
           MOVE QBYA-AMT-PL-2-2-4 TO BYA-AMT-PL(2 2 4).
           MOVE QBYA-O2-PL-2-2-4 TO BYA-O2-PL(2 2 4).
           MOVE QBYA-INT-PL-2-2-4 TO BYA-INT-PL(2 2 4).
           MOVE QBYA-NO-PL-2-2-5 TO BYA-NO-PL(2 2 5).
           MOVE QBYA-AMT-PL-2-2-5 TO BYA-AMT-PL(2 2 5).
           MOVE QBYA-O2-PL-2-2-5 TO BYA-O2-PL(2 2 5).
           MOVE QBYA-INT-PL-2-2-5 TO BYA-INT-PL(2 2 5).
           MOVE QBYA-NO-PL-2-2-6 TO BYA-NO-PL(2 2 6).
           MOVE QBYA-AMT-PL-2-2-6 TO BYA-AMT-PL(2 2 6).
           MOVE QBYA-O2-PL-2-2-6 TO BYA-O2-PL(2 2 6).
           MOVE QBYA-INT-PL-2-2-6 TO BYA-INT-PL(2 2 6).
           MOVE QBYA-NO-PL-2-2-7 TO BYA-NO-PL(2 2 7).
           MOVE QBYA-AMT-PL-2-2-7 TO BYA-AMT-PL(2 2 7).
           MOVE QBYA-O2-PL-2-2-7 TO BYA-O2-PL(2 2 7).
           MOVE QBYA-INT-PL-2-2-7 TO BYA-INT-PL(2 2 7).
           MOVE QBYA-NO-PL-2-2-8 TO BYA-NO-PL(2 2 8).
           MOVE QBYA-AMT-PL-2-2-8 TO BYA-AMT-PL(2 2 8).
           MOVE QBYA-O2-PL-2-2-8 TO BYA-O2-PL(2 2 8).
           MOVE QBYA-INT-PL-2-2-8 TO BYA-INT-PL(2 2 8).
           MOVE QBYA-NO-PL-2-2-9 TO BYA-NO-PL(2 2 9).
           MOVE QBYA-AMT-PL-2-2-9 TO BYA-AMT-PL(2 2 9).
           MOVE QBYA-O2-PL-2-2-9 TO BYA-O2-PL(2 2 9).
           MOVE QBYA-INT-PL-2-2-9 TO BYA-INT-PL(2 2 9).
           MOVE QBYA-NO-PL-2-2-10 TO BYA-NO-PL(2 2 10).
           MOVE QBYA-AMT-PL-2-2-10 TO BYA-AMT-PL(2 2 10).
           MOVE QBYA-O2-PL-2-2-10 TO BYA-O2-PL(2 2 10).
           MOVE QBYA-INT-PL-2-2-10 TO BYA-INT-PL(2 2 10).
           MOVE QBYA-NO-PL-3-1-1 TO BYA-NO-PL(3 1 1).
           MOVE QBYA-AMT-PL-3-1-1 TO BYA-AMT-PL(3 1 1).
           MOVE QBYA-O2-PL-3-1-1 TO BYA-O2-PL(3 1 1).
           MOVE QBYA-INT-PL-3-1-1 TO BYA-INT-PL(3 1 1).
           MOVE QBYA-NO-PL-3-1-2 TO BYA-NO-PL(3 1 2).
           MOVE QBYA-AMT-PL-3-1-2 TO BYA-AMT-PL(3 1 2).
           MOVE QBYA-O2-PL-3-1-2 TO BYA-O2-PL(3 1 2).
           MOVE QBYA-INT-PL-3-1-2 TO BYA-INT-PL(3 1 2).
           MOVE QBYA-NO-PL-3-1-3 TO BYA-NO-PL(3 1 3).
           MOVE QBYA-AMT-PL-3-1-3 TO BYA-AMT-PL(3 1 3).
           MOVE QBYA-O2-PL-3-1-3 TO BYA-O2-PL(3 1 3).
           MOVE QBYA-INT-PL-3-1-3 TO BYA-INT-PL(3 1 3).
           MOVE QBYA-NO-PL-3-1-4 TO BYA-NO-PL(3 1 4).
           MOVE QBYA-AMT-PL-3-1-4 TO BYA-AMT-PL(3 1 4).
           MOVE QBYA-O2-PL-3-1-4 TO BYA-O2-PL(3 1 4).
           MOVE QBYA-INT-PL-3-1-4 TO BYA-INT-PL(3 1 4).
           MOVE QBYA-NO-PL-3-1-5 TO BYA-NO-PL(3 1 5).
           MOVE QBYA-AMT-PL-3-1-5 TO BYA-AMT-PL(3 1 5).
           MOVE QBYA-O2-PL-3-1-5 TO BYA-O2-PL(3 1 5).
           MOVE QBYA-INT-PL-3-1-5 TO BYA-INT-PL(3 1 5).
           MOVE QBYA-NO-PL-3-1-6 TO BYA-NO-PL(3 1 6).
           MOVE QBYA-AMT-PL-3-1-6 TO BYA-AMT-PL(3 1 6).
           MOVE QBYA-O2-PL-3-1-6 TO BYA-O2-PL(3 1 6).
           MOVE QBYA-INT-PL-3-1-6 TO BYA-INT-PL(3 1 6).
           MOVE QBYA-NO-PL-3-1-7 TO BYA-NO-PL(3 1 7).
           MOVE QBYA-AMT-PL-3-1-7 TO BYA-AMT-PL(3 1 7).
           MOVE QBYA-O2-PL-3-1-7 TO BYA-O2-PL(3 1 7).
           MOVE QBYA-INT-PL-3-1-7 TO BYA-INT-PL(3 1 7).
           MOVE QBYA-NO-PL-3-1-8 TO BYA-NO-PL(3 1 8).
           MOVE QBYA-AMT-PL-3-1-8 TO BYA-AMT-PL(3 1 8).
           MOVE QBYA-O2-PL-3-1-8 TO BYA-O2-PL(3 1 8).
           MOVE QBYA-INT-PL-3-1-8 TO BYA-INT-PL(3 1 8).
           MOVE QBYA-NO-PL-3-1-9 TO BYA-NO-PL(3 1 9).
           MOVE QBYA-AMT-PL-3-1-9 TO BYA-AMT-PL(3 1 9).
           MOVE QBYA-O2-PL-3-1-9 TO BYA-O2-PL(3 1 9).
           MOVE QBYA-INT-PL-3-1-9 TO BYA-INT-PL(3 1 9).
           MOVE QBYA-NO-PL-3-1-10 TO BYA-NO-PL(3 1 10).
           MOVE QBYA-AMT-PL-3-1-10 TO BYA-AMT-PL(3 1 10).
           MOVE QBYA-O2-PL-3-1-10 TO BYA-O2-PL(3 1 10).
           MOVE QBYA-INT-PL-3-1-10 TO BYA-INT-PL(3 1 10).
           MOVE QBYA-NO-PL-3-2-1 TO BYA-NO-PL(3 2 1).
           MOVE QBYA-AMT-PL-3-2-1 TO BYA-AMT-PL(3 2 1).
           MOVE QBYA-O2-PL-3-2-1 TO BYA-O2-PL(3 2 1).
           MOVE QBYA-INT-PL-3-2-1 TO BYA-INT-PL(3 2 1).
           MOVE QBYA-NO-PL-3-2-2 TO BYA-NO-PL(3 2 2).
           MOVE QBYA-AMT-PL-3-2-2 TO BYA-AMT-PL(3 2 2).
           MOVE QBYA-O2-PL-3-2-2 TO BYA-O2-PL(3 2 2).
           MOVE QBYA-INT-PL-3-2-2 TO BYA-INT-PL(3 2 2).
           MOVE QBYA-NO-PL-3-2-3 TO BYA-NO-PL(3 2 3).
           MOVE QBYA-AMT-PL-3-2-3 TO BYA-AMT-PL(3 2 3).
           MOVE QBYA-O2-PL-3-2-3 TO BYA-O2-PL(3 2 3).
           MOVE QBYA-INT-PL-3-2-3 TO BYA-INT-PL(3 2 3).
           MOVE QBYA-NO-PL-3-2-4 TO BYA-NO-PL(3 2 4).
           MOVE QBYA-AMT-PL-3-2-4 TO BYA-AMT-PL(3 2 4).
           MOVE QBYA-O2-PL-3-2-4 TO BYA-O2-PL(3 2 4).
           MOVE QBYA-INT-PL-3-2-4 TO BYA-INT-PL(3 2 4).
           MOVE QBYA-NO-PL-3-2-5 TO BYA-NO-PL(3 2 5).
           MOVE QBYA-AMT-PL-3-2-5 TO BYA-AMT-PL(3 2 5).
           MOVE QBYA-O2-PL-3-2-5 TO BYA-O2-PL(3 2 5).
           MOVE QBYA-INT-PL-3-2-5 TO BYA-INT-PL(3 2 5).
           MOVE QBYA-NO-PL-3-2-6 TO BYA-NO-PL(3 2 6).
           MOVE QBYA-AMT-PL-3-2-6 TO BYA-AMT-PL(3 2 6).
           MOVE QBYA-O2-PL-3-2-6 TO BYA-O2-PL(3 2 6).
           MOVE QBYA-INT-PL-3-2-6 TO BYA-INT-PL(3 2 6).
           MOVE QBYA-NO-PL-3-2-7 TO BYA-NO-PL(3 2 7).
           MOVE QBYA-AMT-PL-3-2-7 TO BYA-AMT-PL(3 2 7).
           MOVE QBYA-O2-PL-3-2-7 TO BYA-O2-PL(3 2 7).
           MOVE QBYA-INT-PL-3-2-7 TO BYA-INT-PL(3 2 7).
           MOVE QBYA-NO-PL-3-2-8 TO BYA-NO-PL(3 2 8).
           MOVE QBYA-AMT-PL-3-2-8 TO BYA-AMT-PL(3 2 8).
           MOVE QBYA-O2-PL-3-2-8 TO BYA-O2-PL(3 2 8).
           MOVE QBYA-INT-PL-3-2-8 TO BYA-INT-PL(3 2 8).
           MOVE QBYA-NO-PL-3-2-9 TO BYA-NO-PL(3 2 9).
           MOVE QBYA-AMT-PL-3-2-9 TO BYA-AMT-PL(3 2 9).
           MOVE QBYA-O2-PL-3-2-9 TO BYA-O2-PL(3 2 9).
           MOVE QBYA-INT-PL-3-2-9 TO BYA-INT-PL(3 2 9).
           MOVE QBYA-NO-PL-3-2-10 TO BYA-NO-PL(3 2 10).
           MOVE QBYA-AMT-PL-3-2-10 TO BYA-AMT-PL(3 2 10).
           MOVE QBYA-O2-PL-3-2-10 TO BYA-O2-PL(3 2 10).
           MOVE QBYA-INT-PL-3-2-10 TO BYA-INT-PL(3 2 10).
           MOVE QBYA-DLRVPRIN-1-1-1 TO BYA-DLRVPRIN(1 1 1).
           MOVE QBYA-DLRVPRIN-1-1-2 TO BYA-DLRVPRIN(1 1 2).
           MOVE QBYA-DLRVPRIN-1-1-3 TO BYA-DLRVPRIN(1 1 3).
           MOVE QBYA-DLRVCHGS-1-1-1 TO BYA-DLRVCHGS(1 1 1).
           MOVE QBYA-DLRVCHGS-1-1-2 TO BYA-DLRVCHGS(1 1 2).
           MOVE QBYA-DLRVCHGS-1-1-3 TO BYA-DLRVCHGS(1 1 3).
           MOVE QBYA-DLPARVHELD-1-1 TO BYA-DLPARVHELD(1 1).
           MOVE QBYA-DLRVPRIN-1-2-1 TO BYA-DLRVPRIN(1 2 1).
           MOVE QBYA-DLRVPRIN-1-2-2 TO BYA-DLRVPRIN(1 2 2).
           MOVE QBYA-DLRVPRIN-1-2-3 TO BYA-DLRVPRIN(1 2 3).
           MOVE QBYA-DLRVCHGS-1-2-1 TO BYA-DLRVCHGS(1 2 1).
           MOVE QBYA-DLRVCHGS-1-2-2 TO BYA-DLRVCHGS(1 2 2).
           MOVE QBYA-DLRVCHGS-1-2-3 TO BYA-DLRVCHGS(1 2 3).
           MOVE QBYA-DLPARVHELD-1-2 TO BYA-DLPARVHELD(1 2).
           MOVE QBYA-DLRVPRIN-2-1-1 TO BYA-DLRVPRIN(2 1 1).
           MOVE QBYA-DLRVPRIN-2-1-2 TO BYA-DLRVPRIN(2 1 2).
           MOVE QBYA-DLRVPRIN-2-1-3 TO BYA-DLRVPRIN(2 1 3).
           MOVE QBYA-DLRVCHGS-2-1-1 TO BYA-DLRVCHGS(2 1 1).
           MOVE QBYA-DLRVCHGS-2-1-2 TO BYA-DLRVCHGS(2 1 2).
           MOVE QBYA-DLRVCHGS-2-1-3 TO BYA-DLRVCHGS(2 1 3).
           MOVE QBYA-DLPARVHELD-2-1 TO BYA-DLPARVHELD(2 1).
           MOVE QBYA-DLRVPRIN-2-2-1 TO BYA-DLRVPRIN(2 2 1).
           MOVE QBYA-DLRVPRIN-2-2-2 TO BYA-DLRVPRIN(2 2 2).
           MOVE QBYA-DLRVPRIN-2-2-3 TO BYA-DLRVPRIN(2 2 3).
           MOVE QBYA-DLRVCHGS-2-2-1 TO BYA-DLRVCHGS(2 2 1).
           MOVE QBYA-DLRVCHGS-2-2-2 TO BYA-DLRVCHGS(2 2 2).
           MOVE QBYA-DLRVCHGS-2-2-3 TO BYA-DLRVCHGS(2 2 3).
           MOVE QBYA-DLPARVHELD-2-2 TO BYA-DLPARVHELD(2 2).
           MOVE QBYA-DLRVPRIN-3-1-1 TO BYA-DLRVPRIN(3 1 1).
           MOVE QBYA-DLRVPRIN-3-1-2 TO BYA-DLRVPRIN(3 1 2).
           MOVE QBYA-DLRVPRIN-3-1-3 TO BYA-DLRVPRIN(3 1 3).
           MOVE QBYA-DLRVCHGS-3-1-1 TO BYA-DLRVCHGS(3 1 1).
           MOVE QBYA-DLRVCHGS-3-1-2 TO BYA-DLRVCHGS(3 1 2).
           MOVE QBYA-DLRVCHGS-3-1-3 TO BYA-DLRVCHGS(3 1 3).
           MOVE QBYA-DLPARVHELD-3-1 TO BYA-DLPARVHELD(3 1).
           MOVE QBYA-DLRVPRIN-3-2-1 TO BYA-DLRVPRIN(3 2 1).
           MOVE QBYA-DLRVPRIN-3-2-2 TO BYA-DLRVPRIN(3 2 2).
           MOVE QBYA-DLRVPRIN-3-2-3 TO BYA-DLRVPRIN(3 2 3).
           MOVE QBYA-DLRVCHGS-3-2-1 TO BYA-DLRVCHGS(3 2 1).
           MOVE QBYA-DLRVCHGS-3-2-2 TO BYA-DLRVCHGS(3 2 2).
           MOVE QBYA-DLRVCHGS-3-2-3 TO BYA-DLRVCHGS(3 2 3).
           MOVE QBYA-DLPARVHELD-3-2 TO BYA-DLPARVHELD(3 2).
 
      *-----------------------------------------------------------------
      *    LOAD QBYA-REC FROM BYA-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-BYA-FIELDS.
           MOVE BYA-DATE-CCYYMM TO QBYA-DATE-CCYYMM.
           MOVE BYA-BRNO TO QBYA-BRNO.
           MOVE BYA-CLASS TO QBYA-CLASS.

           IF ( BYA-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QBYA-LTOUCH-DATE
           ELSE
              MOVE BYA-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBYA-LTOUCH-DATE.

           MOVE BYA-NO(1 1 1) TO QBYA-NO-1-1-1.
           MOVE BYA-AMT(1 1 1) TO QBYA-AMT-1-1-1.
           MOVE BYA-O2(1 1 1) TO QBYA-O2-1-1-1.
           MOVE BYA-INT(1 1 1) TO QBYA-INT-1-1-1.
           MOVE BYA-NO(1 1 2) TO QBYA-NO-1-1-2.
           MOVE BYA-AMT(1 1 2) TO QBYA-AMT-1-1-2.
           MOVE BYA-O2(1 1 2) TO QBYA-O2-1-1-2.
           MOVE BYA-INT(1 1 2) TO QBYA-INT-1-1-2.
           MOVE BYA-NO(1 1 3) TO QBYA-NO-1-1-3.
           MOVE BYA-AMT(1 1 3) TO QBYA-AMT-1-1-3.
           MOVE BYA-O2(1 1 3) TO QBYA-O2-1-1-3.
           MOVE BYA-INT(1 1 3) TO QBYA-INT-1-1-3.
           MOVE BYA-NO(1 1 4) TO QBYA-NO-1-1-4.
           MOVE BYA-AMT(1 1 4) TO QBYA-AMT-1-1-4.
           MOVE BYA-O2(1 1 4) TO QBYA-O2-1-1-4.
           MOVE BYA-INT(1 1 4) TO QBYA-INT-1-1-4.
           MOVE BYA-NO(1 1 5) TO QBYA-NO-1-1-5.
           MOVE BYA-AMT(1 1 5) TO QBYA-AMT-1-1-5.
           MOVE BYA-O2(1 1 5) TO QBYA-O2-1-1-5.
           MOVE BYA-INT(1 1 5) TO QBYA-INT-1-1-5.
           MOVE BYA-NO(1 1 6) TO QBYA-NO-1-1-6.
           MOVE BYA-AMT(1 1 6) TO QBYA-AMT-1-1-6.
           MOVE BYA-O2(1 1 6) TO QBYA-O2-1-1-6.
           MOVE BYA-INT(1 1 6) TO QBYA-INT-1-1-6.
           MOVE BYA-NO(1 1 7) TO QBYA-NO-1-1-7.
           MOVE BYA-AMT(1 1 7) TO QBYA-AMT-1-1-7.
           MOVE BYA-O2(1 1 7) TO QBYA-O2-1-1-7.
           MOVE BYA-INT(1 1 7) TO QBYA-INT-1-1-7.
           MOVE BYA-NO(1 1 8) TO QBYA-NO-1-1-8.
           MOVE BYA-AMT(1 1 8) TO QBYA-AMT-1-1-8.
           MOVE BYA-O2(1 1 8) TO QBYA-O2-1-1-8.
           MOVE BYA-INT(1 1 8) TO QBYA-INT-1-1-8.
           MOVE BYA-NO(1 1 9) TO QBYA-NO-1-1-9.
           MOVE BYA-AMT(1 1 9) TO QBYA-AMT-1-1-9.
           MOVE BYA-O2(1 1 9) TO QBYA-O2-1-1-9.
           MOVE BYA-INT(1 1 9) TO QBYA-INT-1-1-9.
           MOVE BYA-NO(1 1 10) TO QBYA-NO-1-1-10.
           MOVE BYA-AMT(1 1 10) TO QBYA-AMT-1-1-10.
           MOVE BYA-O2(1 1 10) TO QBYA-O2-1-1-10.
           MOVE BYA-INT(1 1 10) TO QBYA-INT-1-1-10.
           MOVE BYA-NO(1 1 11) TO QBYA-NO-1-1-11.
           MOVE BYA-AMT(1 1 11) TO QBYA-AMT-1-1-11.
           MOVE BYA-O2(1 1 11) TO QBYA-O2-1-1-11.
           MOVE BYA-INT(1 1 11) TO QBYA-INT-1-1-11.
           MOVE BYA-NO(1 2 1) TO QBYA-NO-1-2-1.
           MOVE BYA-AMT(1 2 1) TO QBYA-AMT-1-2-1.
           MOVE BYA-O2(1 2 1) TO QBYA-O2-1-2-1.
           MOVE BYA-INT(1 2 1) TO QBYA-INT-1-2-1.
           MOVE BYA-NO(1 2 2) TO QBYA-NO-1-2-2.
           MOVE BYA-AMT(1 2 2) TO QBYA-AMT-1-2-2.
           MOVE BYA-O2(1 2 2) TO QBYA-O2-1-2-2.
           MOVE BYA-INT(1 2 2) TO QBYA-INT-1-2-2.
           MOVE BYA-NO(1 2 3) TO QBYA-NO-1-2-3.
           MOVE BYA-AMT(1 2 3) TO QBYA-AMT-1-2-3.
           MOVE BYA-O2(1 2 3) TO QBYA-O2-1-2-3.
           MOVE BYA-INT(1 2 3) TO QBYA-INT-1-2-3.
           MOVE BYA-NO(1 2 4) TO QBYA-NO-1-2-4.
           MOVE BYA-AMT(1 2 4) TO QBYA-AMT-1-2-4.
           MOVE BYA-O2(1 2 4) TO QBYA-O2-1-2-4.
           MOVE BYA-INT(1 2 4) TO QBYA-INT-1-2-4.
           MOVE BYA-NO(1 2 5) TO QBYA-NO-1-2-5.
           MOVE BYA-AMT(1 2 5) TO QBYA-AMT-1-2-5.
           MOVE BYA-O2(1 2 5) TO QBYA-O2-1-2-5.
           MOVE BYA-INT(1 2 5) TO QBYA-INT-1-2-5.
           MOVE BYA-NO(1 2 6) TO QBYA-NO-1-2-6.
           MOVE BYA-AMT(1 2 6) TO QBYA-AMT-1-2-6.
           MOVE BYA-O2(1 2 6) TO QBYA-O2-1-2-6.
           MOVE BYA-INT(1 2 6) TO QBYA-INT-1-2-6.
           MOVE BYA-NO(1 2 7) TO QBYA-NO-1-2-7.
           MOVE BYA-AMT(1 2 7) TO QBYA-AMT-1-2-7.
           MOVE BYA-O2(1 2 7) TO QBYA-O2-1-2-7.
           MOVE BYA-INT(1 2 7) TO QBYA-INT-1-2-7.
           MOVE BYA-NO(1 2 8) TO QBYA-NO-1-2-8.
           MOVE BYA-AMT(1 2 8) TO QBYA-AMT-1-2-8.
           MOVE BYA-O2(1 2 8) TO QBYA-O2-1-2-8.
           MOVE BYA-INT(1 2 8) TO QBYA-INT-1-2-8.
           MOVE BYA-NO(1 2 9) TO QBYA-NO-1-2-9.
           MOVE BYA-AMT(1 2 9) TO QBYA-AMT-1-2-9.
           MOVE BYA-O2(1 2 9) TO QBYA-O2-1-2-9.
           MOVE BYA-INT(1 2 9) TO QBYA-INT-1-2-9.
           MOVE BYA-NO(1 2 10) TO QBYA-NO-1-2-10.
           MOVE BYA-AMT(1 2 10) TO QBYA-AMT-1-2-10.
           MOVE BYA-O2(1 2 10) TO QBYA-O2-1-2-10.
           MOVE BYA-INT(1 2 10) TO QBYA-INT-1-2-10.
           MOVE BYA-NO(1 2 11) TO QBYA-NO-1-2-11.
           MOVE BYA-AMT(1 2 11) TO QBYA-AMT-1-2-11.
           MOVE BYA-O2(1 2 11) TO QBYA-O2-1-2-11.
           MOVE BYA-INT(1 2 11) TO QBYA-INT-1-2-11.
           MOVE BYA-NO(2 1 1) TO QBYA-NO-2-1-1.
           MOVE BYA-AMT(2 1 1) TO QBYA-AMT-2-1-1.
           MOVE BYA-O2(2 1 1) TO QBYA-O2-2-1-1.
           MOVE BYA-INT(2 1 1) TO QBYA-INT-2-1-1.
           MOVE BYA-NO(2 1 2) TO QBYA-NO-2-1-2.
           MOVE BYA-AMT(2 1 2) TO QBYA-AMT-2-1-2.
           MOVE BYA-O2(2 1 2) TO QBYA-O2-2-1-2.
           MOVE BYA-INT(2 1 2) TO QBYA-INT-2-1-2.
           MOVE BYA-NO(2 1 3) TO QBYA-NO-2-1-3.
           MOVE BYA-AMT(2 1 3) TO QBYA-AMT-2-1-3.
           MOVE BYA-O2(2 1 3) TO QBYA-O2-2-1-3.
           MOVE BYA-INT(2 1 3) TO QBYA-INT-2-1-3.
           MOVE BYA-NO(2 1 4) TO QBYA-NO-2-1-4.
           MOVE BYA-AMT(2 1 4) TO QBYA-AMT-2-1-4.
           MOVE BYA-O2(2 1 4) TO QBYA-O2-2-1-4.
           MOVE BYA-INT(2 1 4) TO QBYA-INT-2-1-4.
           MOVE BYA-NO(2 1 5) TO QBYA-NO-2-1-5.
           MOVE BYA-AMT(2 1 5) TO QBYA-AMT-2-1-5.
           MOVE BYA-O2(2 1 5) TO QBYA-O2-2-1-5.
           MOVE BYA-INT(2 1 5) TO QBYA-INT-2-1-5.
           MOVE BYA-NO(2 1 6) TO QBYA-NO-2-1-6.
           MOVE BYA-AMT(2 1 6) TO QBYA-AMT-2-1-6.
           MOVE BYA-O2(2 1 6) TO QBYA-O2-2-1-6.
           MOVE BYA-INT(2 1 6) TO QBYA-INT-2-1-6.
           MOVE BYA-NO(2 1 7) TO QBYA-NO-2-1-7.
           MOVE BYA-AMT(2 1 7) TO QBYA-AMT-2-1-7.
           MOVE BYA-O2(2 1 7) TO QBYA-O2-2-1-7.
           MOVE BYA-INT(2 1 7) TO QBYA-INT-2-1-7.
           MOVE BYA-NO(2 1 8) TO QBYA-NO-2-1-8.
           MOVE BYA-AMT(2 1 8) TO QBYA-AMT-2-1-8.
           MOVE BYA-O2(2 1 8) TO QBYA-O2-2-1-8.
           MOVE BYA-INT(2 1 8) TO QBYA-INT-2-1-8.
           MOVE BYA-NO(2 1 9) TO QBYA-NO-2-1-9.
           MOVE BYA-AMT(2 1 9) TO QBYA-AMT-2-1-9.
           MOVE BYA-O2(2 1 9) TO QBYA-O2-2-1-9.
           MOVE BYA-INT(2 1 9) TO QBYA-INT-2-1-9.
           MOVE BYA-NO(2 1 10) TO QBYA-NO-2-1-10.
           MOVE BYA-AMT(2 1 10) TO QBYA-AMT-2-1-10.
           MOVE BYA-O2(2 1 10) TO QBYA-O2-2-1-10.
           MOVE BYA-INT(2 1 10) TO QBYA-INT-2-1-10.
           MOVE BYA-NO(2 1 11) TO QBYA-NO-2-1-11.
           MOVE BYA-AMT(2 1 11) TO QBYA-AMT-2-1-11.
           MOVE BYA-O2(2 1 11) TO QBYA-O2-2-1-11.
           MOVE BYA-INT(2 1 11) TO QBYA-INT-2-1-11.
           MOVE BYA-NO(2 2 1) TO QBYA-NO-2-2-1.
           MOVE BYA-AMT(2 2 1) TO QBYA-AMT-2-2-1.
           MOVE BYA-O2(2 2 1) TO QBYA-O2-2-2-1.
           MOVE BYA-INT(2 2 1) TO QBYA-INT-2-2-1.
           MOVE BYA-NO(2 2 2) TO QBYA-NO-2-2-2.
           MOVE BYA-AMT(2 2 2) TO QBYA-AMT-2-2-2.
           MOVE BYA-O2(2 2 2) TO QBYA-O2-2-2-2.
           MOVE BYA-INT(2 2 2) TO QBYA-INT-2-2-2.
           MOVE BYA-NO(2 2 3) TO QBYA-NO-2-2-3.
           MOVE BYA-AMT(2 2 3) TO QBYA-AMT-2-2-3.
           MOVE BYA-O2(2 2 3) TO QBYA-O2-2-2-3.
           MOVE BYA-INT(2 2 3) TO QBYA-INT-2-2-3.
           MOVE BYA-NO(2 2 4) TO QBYA-NO-2-2-4.
           MOVE BYA-AMT(2 2 4) TO QBYA-AMT-2-2-4.
           MOVE BYA-O2(2 2 4) TO QBYA-O2-2-2-4.
           MOVE BYA-INT(2 2 4) TO QBYA-INT-2-2-4.
           MOVE BYA-NO(2 2 5) TO QBYA-NO-2-2-5.
           MOVE BYA-AMT(2 2 5) TO QBYA-AMT-2-2-5.
           MOVE BYA-O2(2 2 5) TO QBYA-O2-2-2-5.
           MOVE BYA-INT(2 2 5) TO QBYA-INT-2-2-5.
           MOVE BYA-NO(2 2 6) TO QBYA-NO-2-2-6.
           MOVE BYA-AMT(2 2 6) TO QBYA-AMT-2-2-6.
           MOVE BYA-O2(2 2 6) TO QBYA-O2-2-2-6.
           MOVE BYA-INT(2 2 6) TO QBYA-INT-2-2-6.
           MOVE BYA-NO(2 2 7) TO QBYA-NO-2-2-7.
           MOVE BYA-AMT(2 2 7) TO QBYA-AMT-2-2-7.
           MOVE BYA-O2(2 2 7) TO QBYA-O2-2-2-7.
           MOVE BYA-INT(2 2 7) TO QBYA-INT-2-2-7.
           MOVE BYA-NO(2 2 8) TO QBYA-NO-2-2-8.
           MOVE BYA-AMT(2 2 8) TO QBYA-AMT-2-2-8.
           MOVE BYA-O2(2 2 8) TO QBYA-O2-2-2-8.
           MOVE BYA-INT(2 2 8) TO QBYA-INT-2-2-8.
           MOVE BYA-NO(2 2 9) TO QBYA-NO-2-2-9.
           MOVE BYA-AMT(2 2 9) TO QBYA-AMT-2-2-9.
           MOVE BYA-O2(2 2 9) TO QBYA-O2-2-2-9.
           MOVE BYA-INT(2 2 9) TO QBYA-INT-2-2-9.
           MOVE BYA-NO(2 2 10) TO QBYA-NO-2-2-10.
           MOVE BYA-AMT(2 2 10) TO QBYA-AMT-2-2-10.
           MOVE BYA-O2(2 2 10) TO QBYA-O2-2-2-10.
           MOVE BYA-INT(2 2 10) TO QBYA-INT-2-2-10.
           MOVE BYA-NO(2 2 11) TO QBYA-NO-2-2-11.
           MOVE BYA-AMT(2 2 11) TO QBYA-AMT-2-2-11.
           MOVE BYA-O2(2 2 11) TO QBYA-O2-2-2-11.
           MOVE BYA-INT(2 2 11) TO QBYA-INT-2-2-11.
           MOVE BYA-NO(3 1 1) TO QBYA-NO-3-1-1.
           MOVE BYA-AMT(3 1 1) TO QBYA-AMT-3-1-1.
           MOVE BYA-O2(3 1 1) TO QBYA-O2-3-1-1.
           MOVE BYA-INT(3 1 1) TO QBYA-INT-3-1-1.
           MOVE BYA-NO(3 1 2) TO QBYA-NO-3-1-2.
           MOVE BYA-AMT(3 1 2) TO QBYA-AMT-3-1-2.
           MOVE BYA-O2(3 1 2) TO QBYA-O2-3-1-2.
           MOVE BYA-INT(3 1 2) TO QBYA-INT-3-1-2.
           MOVE BYA-NO(3 1 3) TO QBYA-NO-3-1-3.
           MOVE BYA-AMT(3 1 3) TO QBYA-AMT-3-1-3.
           MOVE BYA-O2(3 1 3) TO QBYA-O2-3-1-3.
           MOVE BYA-INT(3 1 3) TO QBYA-INT-3-1-3.
           MOVE BYA-NO(3 1 4) TO QBYA-NO-3-1-4.
           MOVE BYA-AMT(3 1 4) TO QBYA-AMT-3-1-4.
           MOVE BYA-O2(3 1 4) TO QBYA-O2-3-1-4.
           MOVE BYA-INT(3 1 4) TO QBYA-INT-3-1-4.
           MOVE BYA-NO(3 1 5) TO QBYA-NO-3-1-5.
           MOVE BYA-AMT(3 1 5) TO QBYA-AMT-3-1-5.
           MOVE BYA-O2(3 1 5) TO QBYA-O2-3-1-5.
           MOVE BYA-INT(3 1 5) TO QBYA-INT-3-1-5.
           MOVE BYA-NO(3 1 6) TO QBYA-NO-3-1-6.
           MOVE BYA-AMT(3 1 6) TO QBYA-AMT-3-1-6.
           MOVE BYA-O2(3 1 6) TO QBYA-O2-3-1-6.
           MOVE BYA-INT(3 1 6) TO QBYA-INT-3-1-6.
           MOVE BYA-NO(3 1 7) TO QBYA-NO-3-1-7.
           MOVE BYA-AMT(3 1 7) TO QBYA-AMT-3-1-7.
           MOVE BYA-O2(3 1 7) TO QBYA-O2-3-1-7.
           MOVE BYA-INT(3 1 7) TO QBYA-INT-3-1-7.
           MOVE BYA-NO(3 1 8) TO QBYA-NO-3-1-8.
           MOVE BYA-AMT(3 1 8) TO QBYA-AMT-3-1-8.
           MOVE BYA-O2(3 1 8) TO QBYA-O2-3-1-8.
           MOVE BYA-INT(3 1 8) TO QBYA-INT-3-1-8.
           MOVE BYA-NO(3 1 9) TO QBYA-NO-3-1-9.
           MOVE BYA-AMT(3 1 9) TO QBYA-AMT-3-1-9.
           MOVE BYA-O2(3 1 9) TO QBYA-O2-3-1-9.
           MOVE BYA-INT(3 1 9) TO QBYA-INT-3-1-9.
           MOVE BYA-NO(3 1 10) TO QBYA-NO-3-1-10.
           MOVE BYA-AMT(3 1 10) TO QBYA-AMT-3-1-10.
           MOVE BYA-O2(3 1 10) TO QBYA-O2-3-1-10.
           MOVE BYA-INT(3 1 10) TO QBYA-INT-3-1-10.
           MOVE BYA-NO(3 1 11) TO QBYA-NO-3-1-11.
           MOVE BYA-AMT(3 1 11) TO QBYA-AMT-3-1-11.
           MOVE BYA-O2(3 1 11) TO QBYA-O2-3-1-11.
           MOVE BYA-INT(3 1 11) TO QBYA-INT-3-1-11.
           MOVE BYA-NO(3 2 1) TO QBYA-NO-3-2-1.
           MOVE BYA-AMT(3 2 1) TO QBYA-AMT-3-2-1.
           MOVE BYA-O2(3 2 1) TO QBYA-O2-3-2-1.
           MOVE BYA-INT(3 2 1) TO QBYA-INT-3-2-1.
           MOVE BYA-NO(3 2 2) TO QBYA-NO-3-2-2.
           MOVE BYA-AMT(3 2 2) TO QBYA-AMT-3-2-2.
           MOVE BYA-O2(3 2 2) TO QBYA-O2-3-2-2.
           MOVE BYA-INT(3 2 2) TO QBYA-INT-3-2-2.
           MOVE BYA-NO(3 2 3) TO QBYA-NO-3-2-3.
           MOVE BYA-AMT(3 2 3) TO QBYA-AMT-3-2-3.
           MOVE BYA-O2(3 2 3) TO QBYA-O2-3-2-3.
           MOVE BYA-INT(3 2 3) TO QBYA-INT-3-2-3.
           MOVE BYA-NO(3 2 4) TO QBYA-NO-3-2-4.
           MOVE BYA-AMT(3 2 4) TO QBYA-AMT-3-2-4.
           MOVE BYA-O2(3 2 4) TO QBYA-O2-3-2-4.
           MOVE BYA-INT(3 2 4) TO QBYA-INT-3-2-4.
           MOVE BYA-NO(3 2 5) TO QBYA-NO-3-2-5.
           MOVE BYA-AMT(3 2 5) TO QBYA-AMT-3-2-5.
           MOVE BYA-O2(3 2 5) TO QBYA-O2-3-2-5.
           MOVE BYA-INT(3 2 5) TO QBYA-INT-3-2-5.
           MOVE BYA-NO(3 2 6) TO QBYA-NO-3-2-6.
           MOVE BYA-AMT(3 2 6) TO QBYA-AMT-3-2-6.
           MOVE BYA-O2(3 2 6) TO QBYA-O2-3-2-6.
           MOVE BYA-INT(3 2 6) TO QBYA-INT-3-2-6.
           MOVE BYA-NO(3 2 7) TO QBYA-NO-3-2-7.
           MOVE BYA-AMT(3 2 7) TO QBYA-AMT-3-2-7.
           MOVE BYA-O2(3 2 7) TO QBYA-O2-3-2-7.
           MOVE BYA-INT(3 2 7) TO QBYA-INT-3-2-7.
           MOVE BYA-NO(3 2 8) TO QBYA-NO-3-2-8.
           MOVE BYA-AMT(3 2 8) TO QBYA-AMT-3-2-8.
           MOVE BYA-O2(3 2 8) TO QBYA-O2-3-2-8.
           MOVE BYA-INT(3 2 8) TO QBYA-INT-3-2-8.
           MOVE BYA-NO(3 2 9) TO QBYA-NO-3-2-9.
           MOVE BYA-AMT(3 2 9) TO QBYA-AMT-3-2-9.
           MOVE BYA-O2(3 2 9) TO QBYA-O2-3-2-9.
           MOVE BYA-INT(3 2 9) TO QBYA-INT-3-2-9.
           MOVE BYA-NO(3 2 10) TO QBYA-NO-3-2-10.
           MOVE BYA-AMT(3 2 10) TO QBYA-AMT-3-2-10.
           MOVE BYA-O2(3 2 10) TO QBYA-O2-3-2-10.
           MOVE BYA-INT(3 2 10) TO QBYA-INT-3-2-10.
           MOVE BYA-NO(3 2 11) TO QBYA-NO-3-2-11.
           MOVE BYA-AMT(3 2 11) TO QBYA-AMT-3-2-11.
           MOVE BYA-O2(3 2 11) TO QBYA-O2-3-2-11.
           MOVE BYA-INT(3 2 11) TO QBYA-INT-3-2-11.
           MOVE BYA-NO-REPO(1 1 1) TO QBYA-NO-REPO-1-1-1.
           MOVE BYA-AMT-REPO(1 1 1) TO QBYA-AMT-REPO-1-1-1.
           MOVE BYA-O2-REPO(1 1 1) TO QBYA-O2-REPO-1-1-1.
           MOVE BYA-INT-REPO(1 1 1) TO QBYA-INT-REPO-1-1-1.
           MOVE BYA-NO-REPO(1 1 2) TO QBYA-NO-REPO-1-1-2.
           MOVE BYA-AMT-REPO(1 1 2) TO QBYA-AMT-REPO-1-1-2.
           MOVE BYA-O2-REPO(1 1 2) TO QBYA-O2-REPO-1-1-2.
           MOVE BYA-INT-REPO(1 1 2) TO QBYA-INT-REPO-1-1-2.
           MOVE BYA-NO-REPO(1 1 3) TO QBYA-NO-REPO-1-1-3.
           MOVE BYA-AMT-REPO(1 1 3) TO QBYA-AMT-REPO-1-1-3.
           MOVE BYA-O2-REPO(1 1 3) TO QBYA-O2-REPO-1-1-3.
           MOVE BYA-INT-REPO(1 1 3) TO QBYA-INT-REPO-1-1-3.
           MOVE BYA-NO-REPO(1 1 4) TO QBYA-NO-REPO-1-1-4.
           MOVE BYA-AMT-REPO(1 1 4) TO QBYA-AMT-REPO-1-1-4.
           MOVE BYA-O2-REPO(1 1 4) TO QBYA-O2-REPO-1-1-4.
           MOVE BYA-INT-REPO(1 1 4) TO QBYA-INT-REPO-1-1-4.
           MOVE BYA-NO-REPO(1 1 5) TO QBYA-NO-REPO-1-1-5.
           MOVE BYA-AMT-REPO(1 1 5) TO QBYA-AMT-REPO-1-1-5.
           MOVE BYA-O2-REPO(1 1 5) TO QBYA-O2-REPO-1-1-5.
           MOVE BYA-INT-REPO(1 1 5) TO QBYA-INT-REPO-1-1-5.
           MOVE BYA-NO-REPO(1 2 1) TO QBYA-NO-REPO-1-2-1.
           MOVE BYA-AMT-REPO(1 2 1) TO QBYA-AMT-REPO-1-2-1.
           MOVE BYA-O2-REPO(1 2 1) TO QBYA-O2-REPO-1-2-1.
           MOVE BYA-INT-REPO(1 2 1) TO QBYA-INT-REPO-1-2-1.
           MOVE BYA-NO-REPO(1 2 2) TO QBYA-NO-REPO-1-2-2.
           MOVE BYA-AMT-REPO(1 2 2) TO QBYA-AMT-REPO-1-2-2.
           MOVE BYA-O2-REPO(1 2 2) TO QBYA-O2-REPO-1-2-2.
           MOVE BYA-INT-REPO(1 2 2) TO QBYA-INT-REPO-1-2-2.
           MOVE BYA-NO-REPO(1 2 3) TO QBYA-NO-REPO-1-2-3.
           MOVE BYA-AMT-REPO(1 2 3) TO QBYA-AMT-REPO-1-2-3.
           MOVE BYA-O2-REPO(1 2 3) TO QBYA-O2-REPO-1-2-3.
           MOVE BYA-INT-REPO(1 2 3) TO QBYA-INT-REPO-1-2-3.
           MOVE BYA-NO-REPO(1 2 4) TO QBYA-NO-REPO-1-2-4.
           MOVE BYA-AMT-REPO(1 2 4) TO QBYA-AMT-REPO-1-2-4.
           MOVE BYA-O2-REPO(1 2 4) TO QBYA-O2-REPO-1-2-4.
           MOVE BYA-INT-REPO(1 2 4) TO QBYA-INT-REPO-1-2-4.
           MOVE BYA-NO-REPO(1 2 5) TO QBYA-NO-REPO-1-2-5.
           MOVE BYA-AMT-REPO(1 2 5) TO QBYA-AMT-REPO-1-2-5.
           MOVE BYA-O2-REPO(1 2 5) TO QBYA-O2-REPO-1-2-5.
           MOVE BYA-INT-REPO(1 2 5) TO QBYA-INT-REPO-1-2-5.
           MOVE BYA-NO-REPO(2 1 1) TO QBYA-NO-REPO-2-1-1.
           MOVE BYA-AMT-REPO(2 1 1) TO QBYA-AMT-REPO-2-1-1.
           MOVE BYA-O2-REPO(2 1 1) TO QBYA-O2-REPO-2-1-1.
           MOVE BYA-INT-REPO(2 1 1) TO QBYA-INT-REPO-2-1-1.
           MOVE BYA-NO-REPO(2 1 2) TO QBYA-NO-REPO-2-1-2.
           MOVE BYA-AMT-REPO(2 1 2) TO QBYA-AMT-REPO-2-1-2.
           MOVE BYA-O2-REPO(2 1 2) TO QBYA-O2-REPO-2-1-2.
           MOVE BYA-INT-REPO(2 1 2) TO QBYA-INT-REPO-2-1-2.
           MOVE BYA-NO-REPO(2 1 3) TO QBYA-NO-REPO-2-1-3.
           MOVE BYA-AMT-REPO(2 1 3) TO QBYA-AMT-REPO-2-1-3.
           MOVE BYA-O2-REPO(2 1 3) TO QBYA-O2-REPO-2-1-3.
           MOVE BYA-INT-REPO(2 1 3) TO QBYA-INT-REPO-2-1-3.
           MOVE BYA-NO-REPO(2 1 4) TO QBYA-NO-REPO-2-1-4.
           MOVE BYA-AMT-REPO(2 1 4) TO QBYA-AMT-REPO-2-1-4.
           MOVE BYA-O2-REPO(2 1 4) TO QBYA-O2-REPO-2-1-4.
           MOVE BYA-INT-REPO(2 1 4) TO QBYA-INT-REPO-2-1-4.
           MOVE BYA-NO-REPO(2 1 5) TO QBYA-NO-REPO-2-1-5.
           MOVE BYA-AMT-REPO(2 1 5) TO QBYA-AMT-REPO-2-1-5.
           MOVE BYA-O2-REPO(2 1 5) TO QBYA-O2-REPO-2-1-5.
           MOVE BYA-INT-REPO(2 1 5) TO QBYA-INT-REPO-2-1-5.
           MOVE BYA-NO-REPO(2 2 1) TO QBYA-NO-REPO-2-2-1.
           MOVE BYA-AMT-REPO(2 2 1) TO QBYA-AMT-REPO-2-2-1.
           MOVE BYA-O2-REPO(2 2 1) TO QBYA-O2-REPO-2-2-1.
           MOVE BYA-INT-REPO(2 2 1) TO QBYA-INT-REPO-2-2-1.
           MOVE BYA-NO-REPO(2 2 2) TO QBYA-NO-REPO-2-2-2.
           MOVE BYA-AMT-REPO(2 2 2) TO QBYA-AMT-REPO-2-2-2.
           MOVE BYA-O2-REPO(2 2 2) TO QBYA-O2-REPO-2-2-2.
           MOVE BYA-INT-REPO(2 2 2) TO QBYA-INT-REPO-2-2-2.
           MOVE BYA-NO-REPO(2 2 3) TO QBYA-NO-REPO-2-2-3.
           MOVE BYA-AMT-REPO(2 2 3) TO QBYA-AMT-REPO-2-2-3.
           MOVE BYA-O2-REPO(2 2 3) TO QBYA-O2-REPO-2-2-3.
           MOVE BYA-INT-REPO(2 2 3) TO QBYA-INT-REPO-2-2-3.
           MOVE BYA-NO-REPO(2 2 4) TO QBYA-NO-REPO-2-2-4.
           MOVE BYA-AMT-REPO(2 2 4) TO QBYA-AMT-REPO-2-2-4.
           MOVE BYA-O2-REPO(2 2 4) TO QBYA-O2-REPO-2-2-4.
           MOVE BYA-INT-REPO(2 2 4) TO QBYA-INT-REPO-2-2-4.
           MOVE BYA-NO-REPO(2 2 5) TO QBYA-NO-REPO-2-2-5.
           MOVE BYA-AMT-REPO(2 2 5) TO QBYA-AMT-REPO-2-2-5.
           MOVE BYA-O2-REPO(2 2 5) TO QBYA-O2-REPO-2-2-5.
           MOVE BYA-INT-REPO(2 2 5) TO QBYA-INT-REPO-2-2-5.
           MOVE BYA-NO-REPO(3 1 1) TO QBYA-NO-REPO-3-1-1.
           MOVE BYA-AMT-REPO(3 1 1) TO QBYA-AMT-REPO-3-1-1.
           MOVE BYA-O2-REPO(3 1 1) TO QBYA-O2-REPO-3-1-1.
           MOVE BYA-INT-REPO(3 1 1) TO QBYA-INT-REPO-3-1-1.
           MOVE BYA-NO-REPO(3 1 2) TO QBYA-NO-REPO-3-1-2.
           MOVE BYA-AMT-REPO(3 1 2) TO QBYA-AMT-REPO-3-1-2.
           MOVE BYA-O2-REPO(3 1 2) TO QBYA-O2-REPO-3-1-2.
           MOVE BYA-INT-REPO(3 1 2) TO QBYA-INT-REPO-3-1-2.
           MOVE BYA-NO-REPO(3 1 3) TO QBYA-NO-REPO-3-1-3.
           MOVE BYA-AMT-REPO(3 1 3) TO QBYA-AMT-REPO-3-1-3.
           MOVE BYA-O2-REPO(3 1 3) TO QBYA-O2-REPO-3-1-3.
           MOVE BYA-INT-REPO(3 1 3) TO QBYA-INT-REPO-3-1-3.
           MOVE BYA-NO-REPO(3 1 4) TO QBYA-NO-REPO-3-1-4.
           MOVE BYA-AMT-REPO(3 1 4) TO QBYA-AMT-REPO-3-1-4.
           MOVE BYA-O2-REPO(3 1 4) TO QBYA-O2-REPO-3-1-4.
           MOVE BYA-INT-REPO(3 1 4) TO QBYA-INT-REPO-3-1-4.
           MOVE BYA-NO-REPO(3 1 5) TO QBYA-NO-REPO-3-1-5.
           MOVE BYA-AMT-REPO(3 1 5) TO QBYA-AMT-REPO-3-1-5.
           MOVE BYA-O2-REPO(3 1 5) TO QBYA-O2-REPO-3-1-5.
           MOVE BYA-INT-REPO(3 1 5) TO QBYA-INT-REPO-3-1-5.
           MOVE BYA-NO-REPO(3 2 1) TO QBYA-NO-REPO-3-2-1.
           MOVE BYA-AMT-REPO(3 2 1) TO QBYA-AMT-REPO-3-2-1.
           MOVE BYA-O2-REPO(3 2 1) TO QBYA-O2-REPO-3-2-1.
           MOVE BYA-INT-REPO(3 2 1) TO QBYA-INT-REPO-3-2-1.
           MOVE BYA-NO-REPO(3 2 2) TO QBYA-NO-REPO-3-2-2.
           MOVE BYA-AMT-REPO(3 2 2) TO QBYA-AMT-REPO-3-2-2.
           MOVE BYA-O2-REPO(3 2 2) TO QBYA-O2-REPO-3-2-2.
           MOVE BYA-INT-REPO(3 2 2) TO QBYA-INT-REPO-3-2-2.
           MOVE BYA-NO-REPO(3 2 3) TO QBYA-NO-REPO-3-2-3.
           MOVE BYA-AMT-REPO(3 2 3) TO QBYA-AMT-REPO-3-2-3.
           MOVE BYA-O2-REPO(3 2 3) TO QBYA-O2-REPO-3-2-3.
           MOVE BYA-INT-REPO(3 2 3) TO QBYA-INT-REPO-3-2-3.
           MOVE BYA-NO-REPO(3 2 4) TO QBYA-NO-REPO-3-2-4.
           MOVE BYA-AMT-REPO(3 2 4) TO QBYA-AMT-REPO-3-2-4.
           MOVE BYA-O2-REPO(3 2 4) TO QBYA-O2-REPO-3-2-4.
           MOVE BYA-INT-REPO(3 2 4) TO QBYA-INT-REPO-3-2-4.
           MOVE BYA-NO-REPO(3 2 5) TO QBYA-NO-REPO-3-2-5.
           MOVE BYA-AMT-REPO(3 2 5) TO QBYA-AMT-REPO-3-2-5.
           MOVE BYA-O2-REPO(3 2 5) TO QBYA-O2-REPO-3-2-5.
           MOVE BYA-INT-REPO(3 2 5) TO QBYA-INT-REPO-3-2-5.
           MOVE BYA-NO-PL(1 1 1) TO QBYA-NO-PL-1-1-1.
           MOVE BYA-AMT-PL(1 1 1) TO QBYA-AMT-PL-1-1-1.
           MOVE BYA-O2-PL(1 1 1) TO QBYA-O2-PL-1-1-1.
           MOVE BYA-INT-PL(1 1 1) TO QBYA-INT-PL-1-1-1.
           MOVE BYA-NO-PL(1 1 2) TO QBYA-NO-PL-1-1-2.
           MOVE BYA-AMT-PL(1 1 2) TO QBYA-AMT-PL-1-1-2.
           MOVE BYA-O2-PL(1 1 2) TO QBYA-O2-PL-1-1-2.
           MOVE BYA-INT-PL(1 1 2) TO QBYA-INT-PL-1-1-2.
           MOVE BYA-NO-PL(1 1 3) TO QBYA-NO-PL-1-1-3.
           MOVE BYA-AMT-PL(1 1 3) TO QBYA-AMT-PL-1-1-3.
           MOVE BYA-O2-PL(1 1 3) TO QBYA-O2-PL-1-1-3.
           MOVE BYA-INT-PL(1 1 3) TO QBYA-INT-PL-1-1-3.
           MOVE BYA-NO-PL(1 1 4) TO QBYA-NO-PL-1-1-4.
           MOVE BYA-AMT-PL(1 1 4) TO QBYA-AMT-PL-1-1-4.
           MOVE BYA-O2-PL(1 1 4) TO QBYA-O2-PL-1-1-4.
           MOVE BYA-INT-PL(1 1 4) TO QBYA-INT-PL-1-1-4.
           MOVE BYA-NO-PL(1 1 5) TO QBYA-NO-PL-1-1-5.
           MOVE BYA-AMT-PL(1 1 5) TO QBYA-AMT-PL-1-1-5.
           MOVE BYA-O2-PL(1 1 5) TO QBYA-O2-PL-1-1-5.
           MOVE BYA-INT-PL(1 1 5) TO QBYA-INT-PL-1-1-5.
           MOVE BYA-NO-PL(1 1 6) TO QBYA-NO-PL-1-1-6.
           MOVE BYA-AMT-PL(1 1 6) TO QBYA-AMT-PL-1-1-6.
           MOVE BYA-O2-PL(1 1 6) TO QBYA-O2-PL-1-1-6.
           MOVE BYA-INT-PL(1 1 6) TO QBYA-INT-PL-1-1-6.
           MOVE BYA-NO-PL(1 1 7) TO QBYA-NO-PL-1-1-7.
           MOVE BYA-AMT-PL(1 1 7) TO QBYA-AMT-PL-1-1-7.
           MOVE BYA-O2-PL(1 1 7) TO QBYA-O2-PL-1-1-7.
           MOVE BYA-INT-PL(1 1 7) TO QBYA-INT-PL-1-1-7.
           MOVE BYA-NO-PL(1 1 8) TO QBYA-NO-PL-1-1-8.
           MOVE BYA-AMT-PL(1 1 8) TO QBYA-AMT-PL-1-1-8.
           MOVE BYA-O2-PL(1 1 8) TO QBYA-O2-PL-1-1-8.
           MOVE BYA-INT-PL(1 1 8) TO QBYA-INT-PL-1-1-8.
           MOVE BYA-NO-PL(1 1 9) TO QBYA-NO-PL-1-1-9.
           MOVE BYA-AMT-PL(1 1 9) TO QBYA-AMT-PL-1-1-9.
           MOVE BYA-O2-PL(1 1 9) TO QBYA-O2-PL-1-1-9.
           MOVE BYA-INT-PL(1 1 9) TO QBYA-INT-PL-1-1-9.
           MOVE BYA-NO-PL(1 1 10) TO QBYA-NO-PL-1-1-10.
           MOVE BYA-AMT-PL(1 1 10) TO QBYA-AMT-PL-1-1-10.
           MOVE BYA-O2-PL(1 1 10) TO QBYA-O2-PL-1-1-10.
           MOVE BYA-INT-PL(1 1 10) TO QBYA-INT-PL-1-1-10.
           MOVE BYA-NO-PL(1 2 1) TO QBYA-NO-PL-1-2-1.
           MOVE BYA-AMT-PL(1 2 1) TO QBYA-AMT-PL-1-2-1.
           MOVE BYA-O2-PL(1 2 1) TO QBYA-O2-PL-1-2-1.
           MOVE BYA-INT-PL(1 2 1) TO QBYA-INT-PL-1-2-1.
           MOVE BYA-NO-PL(1 2 2) TO QBYA-NO-PL-1-2-2.
           MOVE BYA-AMT-PL(1 2 2) TO QBYA-AMT-PL-1-2-2.
           MOVE BYA-O2-PL(1 2 2) TO QBYA-O2-PL-1-2-2.
           MOVE BYA-INT-PL(1 2 2) TO QBYA-INT-PL-1-2-2.
           MOVE BYA-NO-PL(1 2 3) TO QBYA-NO-PL-1-2-3.
           MOVE BYA-AMT-PL(1 2 3) TO QBYA-AMT-PL-1-2-3.
           MOVE BYA-O2-PL(1 2 3) TO QBYA-O2-PL-1-2-3.
           MOVE BYA-INT-PL(1 2 3) TO QBYA-INT-PL-1-2-3.
           MOVE BYA-NO-PL(1 2 4) TO QBYA-NO-PL-1-2-4.
           MOVE BYA-AMT-PL(1 2 4) TO QBYA-AMT-PL-1-2-4.
           MOVE BYA-O2-PL(1 2 4) TO QBYA-O2-PL-1-2-4.
           MOVE BYA-INT-PL(1 2 4) TO QBYA-INT-PL-1-2-4.
           MOVE BYA-NO-PL(1 2 5) TO QBYA-NO-PL-1-2-5.
           MOVE BYA-AMT-PL(1 2 5) TO QBYA-AMT-PL-1-2-5.
           MOVE BYA-O2-PL(1 2 5) TO QBYA-O2-PL-1-2-5.
           MOVE BYA-INT-PL(1 2 5) TO QBYA-INT-PL-1-2-5.
           MOVE BYA-NO-PL(1 2 6) TO QBYA-NO-PL-1-2-6.
           MOVE BYA-AMT-PL(1 2 6) TO QBYA-AMT-PL-1-2-6.
           MOVE BYA-O2-PL(1 2 6) TO QBYA-O2-PL-1-2-6.
           MOVE BYA-INT-PL(1 2 6) TO QBYA-INT-PL-1-2-6.
           MOVE BYA-NO-PL(1 2 7) TO QBYA-NO-PL-1-2-7.
           MOVE BYA-AMT-PL(1 2 7) TO QBYA-AMT-PL-1-2-7.
           MOVE BYA-O2-PL(1 2 7) TO QBYA-O2-PL-1-2-7.
           MOVE BYA-INT-PL(1 2 7) TO QBYA-INT-PL-1-2-7.
           MOVE BYA-NO-PL(1 2 8) TO QBYA-NO-PL-1-2-8.
           MOVE BYA-AMT-PL(1 2 8) TO QBYA-AMT-PL-1-2-8.
           MOVE BYA-O2-PL(1 2 8) TO QBYA-O2-PL-1-2-8.
           MOVE BYA-INT-PL(1 2 8) TO QBYA-INT-PL-1-2-8.
           MOVE BYA-NO-PL(1 2 9) TO QBYA-NO-PL-1-2-9.
           MOVE BYA-AMT-PL(1 2 9) TO QBYA-AMT-PL-1-2-9.
           MOVE BYA-O2-PL(1 2 9) TO QBYA-O2-PL-1-2-9.
           MOVE BYA-INT-PL(1 2 9) TO QBYA-INT-PL-1-2-9.
           MOVE BYA-NO-PL(1 2 10) TO QBYA-NO-PL-1-2-10.
           MOVE BYA-AMT-PL(1 2 10) TO QBYA-AMT-PL-1-2-10.
           MOVE BYA-O2-PL(1 2 10) TO QBYA-O2-PL-1-2-10.
           MOVE BYA-INT-PL(1 2 10) TO QBYA-INT-PL-1-2-10.
           MOVE BYA-NO-PL(2 1 1) TO QBYA-NO-PL-2-1-1.
           MOVE BYA-AMT-PL(2 1 1) TO QBYA-AMT-PL-2-1-1.
           MOVE BYA-O2-PL(2 1 1) TO QBYA-O2-PL-2-1-1.
           MOVE BYA-INT-PL(2 1 1) TO QBYA-INT-PL-2-1-1.
           MOVE BYA-NO-PL(2 1 2) TO QBYA-NO-PL-2-1-2.
           MOVE BYA-AMT-PL(2 1 2) TO QBYA-AMT-PL-2-1-2.
           MOVE BYA-O2-PL(2 1 2) TO QBYA-O2-PL-2-1-2.
           MOVE BYA-INT-PL(2 1 2) TO QBYA-INT-PL-2-1-2.
           MOVE BYA-NO-PL(2 1 3) TO QBYA-NO-PL-2-1-3.
           MOVE BYA-AMT-PL(2 1 3) TO QBYA-AMT-PL-2-1-3.
           MOVE BYA-O2-PL(2 1 3) TO QBYA-O2-PL-2-1-3.
           MOVE BYA-INT-PL(2 1 3) TO QBYA-INT-PL-2-1-3.
           MOVE BYA-NO-PL(2 1 4) TO QBYA-NO-PL-2-1-4.
           MOVE BYA-AMT-PL(2 1 4) TO QBYA-AMT-PL-2-1-4.
           MOVE BYA-O2-PL(2 1 4) TO QBYA-O2-PL-2-1-4.
           MOVE BYA-INT-PL(2 1 4) TO QBYA-INT-PL-2-1-4.
           MOVE BYA-NO-PL(2 1 5) TO QBYA-NO-PL-2-1-5.
           MOVE BYA-AMT-PL(2 1 5) TO QBYA-AMT-PL-2-1-5.
           MOVE BYA-O2-PL(2 1 5) TO QBYA-O2-PL-2-1-5.
           MOVE BYA-INT-PL(2 1 5) TO QBYA-INT-PL-2-1-5.
           MOVE BYA-NO-PL(2 1 6) TO QBYA-NO-PL-2-1-6.
           MOVE BYA-AMT-PL(2 1 6) TO QBYA-AMT-PL-2-1-6.
           MOVE BYA-O2-PL(2 1 6) TO QBYA-O2-PL-2-1-6.
           MOVE BYA-INT-PL(2 1 6) TO QBYA-INT-PL-2-1-6.
           MOVE BYA-NO-PL(2 1 7) TO QBYA-NO-PL-2-1-7.
           MOVE BYA-AMT-PL(2 1 7) TO QBYA-AMT-PL-2-1-7.
           MOVE BYA-O2-PL(2 1 7) TO QBYA-O2-PL-2-1-7.
           MOVE BYA-INT-PL(2 1 7) TO QBYA-INT-PL-2-1-7.
           MOVE BYA-NO-PL(2 1 8) TO QBYA-NO-PL-2-1-8.
           MOVE BYA-AMT-PL(2 1 8) TO QBYA-AMT-PL-2-1-8.
           MOVE BYA-O2-PL(2 1 8) TO QBYA-O2-PL-2-1-8.
           MOVE BYA-INT-PL(2 1 8) TO QBYA-INT-PL-2-1-8.
           MOVE BYA-NO-PL(2 1 9) TO QBYA-NO-PL-2-1-9.
           MOVE BYA-AMT-PL(2 1 9) TO QBYA-AMT-PL-2-1-9.
           MOVE BYA-O2-PL(2 1 9) TO QBYA-O2-PL-2-1-9.
           MOVE BYA-INT-PL(2 1 9) TO QBYA-INT-PL-2-1-9.
           MOVE BYA-NO-PL(2 1 10) TO QBYA-NO-PL-2-1-10.
           MOVE BYA-AMT-PL(2 1 10) TO QBYA-AMT-PL-2-1-10.
           MOVE BYA-O2-PL(2 1 10) TO QBYA-O2-PL-2-1-10.
           MOVE BYA-INT-PL(2 1 10) TO QBYA-INT-PL-2-1-10.
           MOVE BYA-NO-PL(2 2 1) TO QBYA-NO-PL-2-2-1.
           MOVE BYA-AMT-PL(2 2 1) TO QBYA-AMT-PL-2-2-1.
           MOVE BYA-O2-PL(2 2 1) TO QBYA-O2-PL-2-2-1.
           MOVE BYA-INT-PL(2 2 1) TO QBYA-INT-PL-2-2-1.
           MOVE BYA-NO-PL(2 2 2) TO QBYA-NO-PL-2-2-2.
           MOVE BYA-AMT-PL(2 2 2) TO QBYA-AMT-PL-2-2-2.
           MOVE BYA-O2-PL(2 2 2) TO QBYA-O2-PL-2-2-2.
           MOVE BYA-INT-PL(2 2 2) TO QBYA-INT-PL-2-2-2.
           MOVE BYA-NO-PL(2 2 3) TO QBYA-NO-PL-2-2-3.
           MOVE BYA-AMT-PL(2 2 3) TO QBYA-AMT-PL-2-2-3.
           MOVE BYA-O2-PL(2 2 3) TO QBYA-O2-PL-2-2-3.
           MOVE BYA-INT-PL(2 2 3) TO QBYA-INT-PL-2-2-3.
           MOVE BYA-NO-PL(2 2 4) TO QBYA-NO-PL-2-2-4.
           MOVE BYA-AMT-PL(2 2 4) TO QBYA-AMT-PL-2-2-4.
           MOVE BYA-O2-PL(2 2 4) TO QBYA-O2-PL-2-2-4.
           MOVE BYA-INT-PL(2 2 4) TO QBYA-INT-PL-2-2-4.
           MOVE BYA-NO-PL(2 2 5) TO QBYA-NO-PL-2-2-5.
           MOVE BYA-AMT-PL(2 2 5) TO QBYA-AMT-PL-2-2-5.
           MOVE BYA-O2-PL(2 2 5) TO QBYA-O2-PL-2-2-5.
           MOVE BYA-INT-PL(2 2 5) TO QBYA-INT-PL-2-2-5.
           MOVE BYA-NO-PL(2 2 6) TO QBYA-NO-PL-2-2-6.
           MOVE BYA-AMT-PL(2 2 6) TO QBYA-AMT-PL-2-2-6.
           MOVE BYA-O2-PL(2 2 6) TO QBYA-O2-PL-2-2-6.
           MOVE BYA-INT-PL(2 2 6) TO QBYA-INT-PL-2-2-6.
           MOVE BYA-NO-PL(2 2 7) TO QBYA-NO-PL-2-2-7.
           MOVE BYA-AMT-PL(2 2 7) TO QBYA-AMT-PL-2-2-7.
           MOVE BYA-O2-PL(2 2 7) TO QBYA-O2-PL-2-2-7.
           MOVE BYA-INT-PL(2 2 7) TO QBYA-INT-PL-2-2-7.
           MOVE BYA-NO-PL(2 2 8) TO QBYA-NO-PL-2-2-8.
           MOVE BYA-AMT-PL(2 2 8) TO QBYA-AMT-PL-2-2-8.
           MOVE BYA-O2-PL(2 2 8) TO QBYA-O2-PL-2-2-8.
           MOVE BYA-INT-PL(2 2 8) TO QBYA-INT-PL-2-2-8.
           MOVE BYA-NO-PL(2 2 9) TO QBYA-NO-PL-2-2-9.
           MOVE BYA-AMT-PL(2 2 9) TO QBYA-AMT-PL-2-2-9.
           MOVE BYA-O2-PL(2 2 9) TO QBYA-O2-PL-2-2-9.
           MOVE BYA-INT-PL(2 2 9) TO QBYA-INT-PL-2-2-9.
           MOVE BYA-NO-PL(2 2 10) TO QBYA-NO-PL-2-2-10.
           MOVE BYA-AMT-PL(2 2 10) TO QBYA-AMT-PL-2-2-10.
           MOVE BYA-O2-PL(2 2 10) TO QBYA-O2-PL-2-2-10.
           MOVE BYA-INT-PL(2 2 10) TO QBYA-INT-PL-2-2-10.
           MOVE BYA-NO-PL(3 1 1) TO QBYA-NO-PL-3-1-1.
           MOVE BYA-AMT-PL(3 1 1) TO QBYA-AMT-PL-3-1-1.
           MOVE BYA-O2-PL(3 1 1) TO QBYA-O2-PL-3-1-1.
           MOVE BYA-INT-PL(3 1 1) TO QBYA-INT-PL-3-1-1.
           MOVE BYA-NO-PL(3 1 2) TO QBYA-NO-PL-3-1-2.
           MOVE BYA-AMT-PL(3 1 2) TO QBYA-AMT-PL-3-1-2.
           MOVE BYA-O2-PL(3 1 2) TO QBYA-O2-PL-3-1-2.
           MOVE BYA-INT-PL(3 1 2) TO QBYA-INT-PL-3-1-2.
           MOVE BYA-NO-PL(3 1 3) TO QBYA-NO-PL-3-1-3.
           MOVE BYA-AMT-PL(3 1 3) TO QBYA-AMT-PL-3-1-3.
           MOVE BYA-O2-PL(3 1 3) TO QBYA-O2-PL-3-1-3.
           MOVE BYA-INT-PL(3 1 3) TO QBYA-INT-PL-3-1-3.
           MOVE BYA-NO-PL(3 1 4) TO QBYA-NO-PL-3-1-4.
           MOVE BYA-AMT-PL(3 1 4) TO QBYA-AMT-PL-3-1-4.
           MOVE BYA-O2-PL(3 1 4) TO QBYA-O2-PL-3-1-4.
           MOVE BYA-INT-PL(3 1 4) TO QBYA-INT-PL-3-1-4.
           MOVE BYA-NO-PL(3 1 5) TO QBYA-NO-PL-3-1-5.
           MOVE BYA-AMT-PL(3 1 5) TO QBYA-AMT-PL-3-1-5.
           MOVE BYA-O2-PL(3 1 5) TO QBYA-O2-PL-3-1-5.
           MOVE BYA-INT-PL(3 1 5) TO QBYA-INT-PL-3-1-5.
           MOVE BYA-NO-PL(3 1 6) TO QBYA-NO-PL-3-1-6.
           MOVE BYA-AMT-PL(3 1 6) TO QBYA-AMT-PL-3-1-6.
           MOVE BYA-O2-PL(3 1 6) TO QBYA-O2-PL-3-1-6.
           MOVE BYA-INT-PL(3 1 6) TO QBYA-INT-PL-3-1-6.
           MOVE BYA-NO-PL(3 1 7) TO QBYA-NO-PL-3-1-7.
           MOVE BYA-AMT-PL(3 1 7) TO QBYA-AMT-PL-3-1-7.
           MOVE BYA-O2-PL(3 1 7) TO QBYA-O2-PL-3-1-7.
           MOVE BYA-INT-PL(3 1 7) TO QBYA-INT-PL-3-1-7.
           MOVE BYA-NO-PL(3 1 8) TO QBYA-NO-PL-3-1-8.
           MOVE BYA-AMT-PL(3 1 8) TO QBYA-AMT-PL-3-1-8.
           MOVE BYA-O2-PL(3 1 8) TO QBYA-O2-PL-3-1-8.
           MOVE BYA-INT-PL(3 1 8) TO QBYA-INT-PL-3-1-8.
           MOVE BYA-NO-PL(3 1 9) TO QBYA-NO-PL-3-1-9.
           MOVE BYA-AMT-PL(3 1 9) TO QBYA-AMT-PL-3-1-9.
           MOVE BYA-O2-PL(3 1 9) TO QBYA-O2-PL-3-1-9.
           MOVE BYA-INT-PL(3 1 9) TO QBYA-INT-PL-3-1-9.
           MOVE BYA-NO-PL(3 1 10) TO QBYA-NO-PL-3-1-10.
           MOVE BYA-AMT-PL(3 1 10) TO QBYA-AMT-PL-3-1-10.
           MOVE BYA-O2-PL(3 1 10) TO QBYA-O2-PL-3-1-10.
           MOVE BYA-INT-PL(3 1 10) TO QBYA-INT-PL-3-1-10.
           MOVE BYA-NO-PL(3 2 1) TO QBYA-NO-PL-3-2-1.
           MOVE BYA-AMT-PL(3 2 1) TO QBYA-AMT-PL-3-2-1.
           MOVE BYA-O2-PL(3 2 1) TO QBYA-O2-PL-3-2-1.
           MOVE BYA-INT-PL(3 2 1) TO QBYA-INT-PL-3-2-1.
           MOVE BYA-NO-PL(3 2 2) TO QBYA-NO-PL-3-2-2.
           MOVE BYA-AMT-PL(3 2 2) TO QBYA-AMT-PL-3-2-2.
           MOVE BYA-O2-PL(3 2 2) TO QBYA-O2-PL-3-2-2.
           MOVE BYA-INT-PL(3 2 2) TO QBYA-INT-PL-3-2-2.
           MOVE BYA-NO-PL(3 2 3) TO QBYA-NO-PL-3-2-3.
           MOVE BYA-AMT-PL(3 2 3) TO QBYA-AMT-PL-3-2-3.
           MOVE BYA-O2-PL(3 2 3) TO QBYA-O2-PL-3-2-3.
           MOVE BYA-INT-PL(3 2 3) TO QBYA-INT-PL-3-2-3.
           MOVE BYA-NO-PL(3 2 4) TO QBYA-NO-PL-3-2-4.
           MOVE BYA-AMT-PL(3 2 4) TO QBYA-AMT-PL-3-2-4.
           MOVE BYA-O2-PL(3 2 4) TO QBYA-O2-PL-3-2-4.
           MOVE BYA-INT-PL(3 2 4) TO QBYA-INT-PL-3-2-4.
           MOVE BYA-NO-PL(3 2 5) TO QBYA-NO-PL-3-2-5.
           MOVE BYA-AMT-PL(3 2 5) TO QBYA-AMT-PL-3-2-5.
           MOVE BYA-O2-PL(3 2 5) TO QBYA-O2-PL-3-2-5.
           MOVE BYA-INT-PL(3 2 5) TO QBYA-INT-PL-3-2-5.
           MOVE BYA-NO-PL(3 2 6) TO QBYA-NO-PL-3-2-6.
           MOVE BYA-AMT-PL(3 2 6) TO QBYA-AMT-PL-3-2-6.
           MOVE BYA-O2-PL(3 2 6) TO QBYA-O2-PL-3-2-6.
           MOVE BYA-INT-PL(3 2 6) TO QBYA-INT-PL-3-2-6.
           MOVE BYA-NO-PL(3 2 7) TO QBYA-NO-PL-3-2-7.
           MOVE BYA-AMT-PL(3 2 7) TO QBYA-AMT-PL-3-2-7.
           MOVE BYA-O2-PL(3 2 7) TO QBYA-O2-PL-3-2-7.
           MOVE BYA-INT-PL(3 2 7) TO QBYA-INT-PL-3-2-7.
           MOVE BYA-NO-PL(3 2 8) TO QBYA-NO-PL-3-2-8.
           MOVE BYA-AMT-PL(3 2 8) TO QBYA-AMT-PL-3-2-8.
           MOVE BYA-O2-PL(3 2 8) TO QBYA-O2-PL-3-2-8.
           MOVE BYA-INT-PL(3 2 8) TO QBYA-INT-PL-3-2-8.
           MOVE BYA-NO-PL(3 2 9) TO QBYA-NO-PL-3-2-9.
           MOVE BYA-AMT-PL(3 2 9) TO QBYA-AMT-PL-3-2-9.
           MOVE BYA-O2-PL(3 2 9) TO QBYA-O2-PL-3-2-9.
           MOVE BYA-INT-PL(3 2 9) TO QBYA-INT-PL-3-2-9.
           MOVE BYA-NO-PL(3 2 10) TO QBYA-NO-PL-3-2-10.
           MOVE BYA-AMT-PL(3 2 10) TO QBYA-AMT-PL-3-2-10.
           MOVE BYA-O2-PL(3 2 10) TO QBYA-O2-PL-3-2-10.
           MOVE BYA-INT-PL(3 2 10) TO QBYA-INT-PL-3-2-10.
           MOVE BYA-DLRVPRIN(1 1 1) TO QBYA-DLRVPRIN-1-1-1.
           MOVE BYA-DLRVPRIN(1 1 2) TO QBYA-DLRVPRIN-1-1-2.
           MOVE BYA-DLRVPRIN(1 1 3) TO QBYA-DLRVPRIN-1-1-3.
           MOVE BYA-DLRVCHGS(1 1 1) TO QBYA-DLRVCHGS-1-1-1.
           MOVE BYA-DLRVCHGS(1 1 2) TO QBYA-DLRVCHGS-1-1-2.
           MOVE BYA-DLRVCHGS(1 1 3) TO QBYA-DLRVCHGS-1-1-3.
           MOVE BYA-DLPARVHELD(1 1) TO QBYA-DLPARVHELD-1-1.
           MOVE BYA-DLRVPRIN(1 2 1) TO QBYA-DLRVPRIN-1-2-1.
           MOVE BYA-DLRVPRIN(1 2 2) TO QBYA-DLRVPRIN-1-2-2.
           MOVE BYA-DLRVPRIN(1 2 3) TO QBYA-DLRVPRIN-1-2-3.
           MOVE BYA-DLRVCHGS(1 2 1) TO QBYA-DLRVCHGS-1-2-1.
           MOVE BYA-DLRVCHGS(1 2 2) TO QBYA-DLRVCHGS-1-2-2.
           MOVE BYA-DLRVCHGS(1 2 3) TO QBYA-DLRVCHGS-1-2-3.
           MOVE BYA-DLPARVHELD(1 2) TO QBYA-DLPARVHELD-1-2.
           MOVE BYA-DLRVPRIN(2 1 1) TO QBYA-DLRVPRIN-2-1-1.
           MOVE BYA-DLRVPRIN(2 1 2) TO QBYA-DLRVPRIN-2-1-2.
           MOVE BYA-DLRVPRIN(2 1 3) TO QBYA-DLRVPRIN-2-1-3.
           MOVE BYA-DLRVCHGS(2 1 1) TO QBYA-DLRVCHGS-2-1-1.
           MOVE BYA-DLRVCHGS(2 1 2) TO QBYA-DLRVCHGS-2-1-2.
           MOVE BYA-DLRVCHGS(2 1 3) TO QBYA-DLRVCHGS-2-1-3.
           MOVE BYA-DLPARVHELD(2 1) TO QBYA-DLPARVHELD-2-1.
           MOVE BYA-DLRVPRIN(2 2 1) TO QBYA-DLRVPRIN-2-2-1.
           MOVE BYA-DLRVPRIN(2 2 2) TO QBYA-DLRVPRIN-2-2-2.
           MOVE BYA-DLRVPRIN(2 2 3) TO QBYA-DLRVPRIN-2-2-3.
           MOVE BYA-DLRVCHGS(2 2 1) TO QBYA-DLRVCHGS-2-2-1.
           MOVE BYA-DLRVCHGS(2 2 2) TO QBYA-DLRVCHGS-2-2-2.
           MOVE BYA-DLRVCHGS(2 2 3) TO QBYA-DLRVCHGS-2-2-3.
           MOVE BYA-DLPARVHELD(2 2) TO QBYA-DLPARVHELD-2-2.
           MOVE BYA-DLRVPRIN(3 1 1) TO QBYA-DLRVPRIN-3-1-1.
           MOVE BYA-DLRVPRIN(3 1 2) TO QBYA-DLRVPRIN-3-1-2.
           MOVE BYA-DLRVPRIN(3 1 3) TO QBYA-DLRVPRIN-3-1-3.
           MOVE BYA-DLRVCHGS(3 1 1) TO QBYA-DLRVCHGS-3-1-1.
           MOVE BYA-DLRVCHGS(3 1 2) TO QBYA-DLRVCHGS-3-1-2.
           MOVE BYA-DLRVCHGS(3 1 3) TO QBYA-DLRVCHGS-3-1-3.
           MOVE BYA-DLPARVHELD(3 1) TO QBYA-DLPARVHELD-3-1.
           MOVE BYA-DLRVPRIN(3 2 1) TO QBYA-DLRVPRIN-3-2-1.
           MOVE BYA-DLRVPRIN(3 2 2) TO QBYA-DLRVPRIN-3-2-2.
           MOVE BYA-DLRVPRIN(3 2 3) TO QBYA-DLRVPRIN-3-2-3.
           MOVE BYA-DLRVCHGS(3 2 1) TO QBYA-DLRVCHGS-3-2-1.
           MOVE BYA-DLRVCHGS(3 2 2) TO QBYA-DLRVCHGS-3-2-2.
           MOVE BYA-DLRVCHGS(3 2 3) TO QBYA-DLRVCHGS-3-2-3.
           MOVE BYA-DLPARVHELD(3 2) TO QBYA-DLPARVHELD-3-2.
 
      *-----------------------------------------------------------------
      *    LOAD BYB-REC FROM QBYB-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-BYB-FIELDS.
           MOVE QBYB-DATE-CCYYMM TO BYB-DATE-CCYYMM.
           MOVE QBYB-BRNO TO BYB-BRNO.
           MOVE QBYB-CLASS TO BYB-CLASS.
           MOVE QBYB-PL-PRINCOL-1-1 TO BYB-PL-PRINCOL(1 1).
           MOVE QBYB-PL-INTCOL-1-1 TO BYB-PL-INTCOL(1 1).
           MOVE QBYB-PL-LATECOL-1-1 TO BYB-PL-LATECOL(1 1).
           MOVE QBYB-PL-O2COL-1-1 TO BYB-PL-O2COL(1 1).
           MOVE QBYB-PL-PRINCOL-1-2 TO BYB-PL-PRINCOL(1 2).
           MOVE QBYB-PL-INTCOL-1-2 TO BYB-PL-INTCOL(1 2).
           MOVE QBYB-PL-LATECOL-1-2 TO BYB-PL-LATECOL(1 2).
           MOVE QBYB-PL-O2COL-1-2 TO BYB-PL-O2COL(1 2).
           MOVE QBYB-PL-PRINCOL-1-3 TO BYB-PL-PRINCOL(1 3).
           MOVE QBYB-PL-INTCOL-1-3 TO BYB-PL-INTCOL(1 3).
           MOVE QBYB-PL-LATECOL-1-3 TO BYB-PL-LATECOL(1 3).
           MOVE QBYB-PL-O2COL-1-3 TO BYB-PL-O2COL(1 3).
           MOVE QBYB-PL-PRINCOL-1-4 TO BYB-PL-PRINCOL(1 4).
           MOVE QBYB-PL-INTCOL-1-4 TO BYB-PL-INTCOL(1 4).
           MOVE QBYB-PL-LATECOL-1-4 TO BYB-PL-LATECOL(1 4).
           MOVE QBYB-PL-O2COL-1-4 TO BYB-PL-O2COL(1 4).
           MOVE QBYB-PL-PRINCOL-1-5 TO BYB-PL-PRINCOL(1 5).
           MOVE QBYB-PL-INTCOL-1-5 TO BYB-PL-INTCOL(1 5).
           MOVE QBYB-PL-LATECOL-1-5 TO BYB-PL-LATECOL(1 5).
           MOVE QBYB-PL-O2COL-1-5 TO BYB-PL-O2COL(1 5).
           MOVE QBYB-AMT-SOLD-1 TO BYB-AMT-SOLD(1).
           MOVE QBYB-AMT-TRANS-1 TO BYB-AMT-TRANS(1).
           MOVE QBYB-AMT-PURCH-1 TO BYB-AMT-PURCH(1).
           MOVE QBYB-NO-SOLD-1 TO BYB-NO-SOLD(1).
           MOVE QBYB-NO-TRANS-1 TO BYB-NO-TRANS(1).
           MOVE QBYB-NO-PURCH-1 TO BYB-NO-PURCH(1).
           MOVE QBYB-OTHCOL-1 TO BYB-OTHCOL(1).
           MOVE QBYB-OT2COL-1 TO BYB-OT2COL(1).
           MOVE QBYB-INTCOL-1 TO BYB-INTCOL(1).
           MOVE QBYB-LATECOL-1 TO BYB-LATECOL(1).
           MOVE QBYB-PRINCOL-1 TO BYB-PRINCOL(1).
           MOVE QBYB-DEFCOL-1 TO BYB-DEFCOL(1).
           MOVE QBYB-RNPRIN-1 TO BYB-RNPRIN(1).
           MOVE QBYB-INTREB-1 TO BYB-INTREB(1).
           MOVE QBYB-CLREB1-1 TO BYB-CLREB1(1).
           MOVE QBYB-CLREB2-1 TO BYB-CLREB2(1).
           MOVE QBYB-CLREB3-1 TO BYB-CLREB3(1).
           MOVE QBYB-CLREB4-1 TO BYB-CLREB4(1).
           MOVE QBYB-AHREB-1 TO BYB-AHREB(1).
           MOVE QBYB-PPREB-1 TO BYB-PPREB(1).
           MOVE QBYB-O1REB-1 TO BYB-O1REB(1).
           MOVE QBYB-O2REB-1 TO BYB-O2REB(1).
           MOVE QBYB-O3REB-1 TO BYB-O3REB(1).
           MOVE QBYB-O4REB-1 TO BYB-O4REB(1).
           MOVE QBYB-O5REB-1 TO BYB-O5REB(1).
           MOVE QBYB-O6REB-1 TO BYB-O6REB(1).
           MOVE QBYB-O7REB-1 TO BYB-O7REB(1).
           MOVE QBYB-SCREB-1 TO BYB-SCREB(1).
           MOVE QBYB-MFREB-1 TO BYB-MFREB(1).
           MOVE QBYB-DFREB-1 TO BYB-DFREB(1).
           MOVE QBYB-UN7-REB-1 TO BYB-UN7-REB(1).
           MOVE QBYB-UN8-REB-1 TO BYB-UN8-REB(1).
           MOVE QBYB-UN9-REB-1 TO BYB-UN9-REB(1).
           MOVE QBYB-UN10-REB-1 TO BYB-UN10-REB(1).
           MOVE QBYB-PART-CHGOFF-1 TO BYB-PART-CHGOFF(1).
           MOVE QBYB-ALLOTMENTS-1 TO BYB-ALLOTMENTS(1).
           MOVE QBYB-INS-NO-ELIG-1-1 TO BYB-INS-NO-ELIG(1 1).
           MOVE QBYB-INS-NO-WROTE-1-1 TO BYB-INS-NO-WROTE(1 1).
           MOVE QBYB-INS-NO-NOTWROTE-1-1 TO BYB-INS-NO-NOTWROTE(1 1).
           MOVE QBYB-INS-AMT-WROTE-1-1 TO BYB-INS-AMT-WROTE(1 1).
           MOVE QBYB-INS-NO-ELIG-1-2 TO BYB-INS-NO-ELIG(1 2).
           MOVE QBYB-INS-NO-WROTE-1-2 TO BYB-INS-NO-WROTE(1 2).
           MOVE QBYB-INS-NO-NOTWROTE-1-2 TO BYB-INS-NO-NOTWROTE(1 2).
           MOVE QBYB-INS-AMT-WROTE-1-2 TO BYB-INS-AMT-WROTE(1 2).
           MOVE QBYB-INS-NO-ELIG-1-3 TO BYB-INS-NO-ELIG(1 3).
           MOVE QBYB-INS-NO-WROTE-1-3 TO BYB-INS-NO-WROTE(1 3).
           MOVE QBYB-INS-NO-NOTWROTE-1-3 TO BYB-INS-NO-NOTWROTE(1 3).
           MOVE QBYB-INS-AMT-WROTE-1-3 TO BYB-INS-AMT-WROTE(1 3).
           MOVE QBYB-INS-NO-ELIG-1-4 TO BYB-INS-NO-ELIG(1 4).
           MOVE QBYB-INS-NO-WROTE-1-4 TO BYB-INS-NO-WROTE(1 4).
           MOVE QBYB-INS-NO-NOTWROTE-1-4 TO BYB-INS-NO-NOTWROTE(1 4).
           MOVE QBYB-INS-AMT-WROTE-1-4 TO BYB-INS-AMT-WROTE(1 4).
           MOVE QBYB-INS-NO-ELIG-1-5 TO BYB-INS-NO-ELIG(1 5).
           MOVE QBYB-INS-NO-WROTE-1-5 TO BYB-INS-NO-WROTE(1 5).
           MOVE QBYB-INS-NO-NOTWROTE-1-5 TO BYB-INS-NO-NOTWROTE(1 5).
           MOVE QBYB-INS-AMT-WROTE-1-5 TO BYB-INS-AMT-WROTE(1 5).
           MOVE QBYB-INS-NO-ELIG-1-6 TO BYB-INS-NO-ELIG(1 6).
           MOVE QBYB-INS-NO-WROTE-1-6 TO BYB-INS-NO-WROTE(1 6).
           MOVE QBYB-INS-NO-NOTWROTE-1-6 TO BYB-INS-NO-NOTWROTE(1 6).
           MOVE QBYB-INS-AMT-WROTE-1-6 TO BYB-INS-AMT-WROTE(1 6).
           MOVE QBYB-INS-NO-ELIG-1-7 TO BYB-INS-NO-ELIG(1 7).
           MOVE QBYB-INS-NO-WROTE-1-7 TO BYB-INS-NO-WROTE(1 7).
           MOVE QBYB-INS-NO-NOTWROTE-1-7 TO BYB-INS-NO-NOTWROTE(1 7).
           MOVE QBYB-INS-AMT-WROTE-1-7 TO BYB-INS-AMT-WROTE(1 7).
           MOVE QBYB-INS-NO-ELIG-1-8 TO BYB-INS-NO-ELIG(1 8).
           MOVE QBYB-INS-NO-WROTE-1-8 TO BYB-INS-NO-WROTE(1 8).
           MOVE QBYB-INS-NO-NOTWROTE-1-8 TO BYB-INS-NO-NOTWROTE(1 8).
           MOVE QBYB-INS-AMT-WROTE-1-8 TO BYB-INS-AMT-WROTE(1 8).
           MOVE QBYB-INS-NO-ELIG-1-9 TO BYB-INS-NO-ELIG(1 9).
           MOVE QBYB-INS-NO-WROTE-1-9 TO BYB-INS-NO-WROTE(1 9).
           MOVE QBYB-INS-NO-NOTWROTE-1-9 TO BYB-INS-NO-NOTWROTE(1 9).
           MOVE QBYB-INS-AMT-WROTE-1-9 TO BYB-INS-AMT-WROTE(1 9).
           MOVE QBYB-INS-NO-ELIG-1-10 TO BYB-INS-NO-ELIG(1 10).
           MOVE QBYB-INS-NO-WROTE-1-10 TO BYB-INS-NO-WROTE(1 10).
           MOVE QBYB-INS-NO-NOTWROTE-1-10 TO BYB-INS-NO-NOTWROTE(1 10).
           MOVE QBYB-INS-AMT-WROTE-1-10 TO BYB-INS-AMT-WROTE(1 10).
           MOVE QBYB-FEE-NO-ELIG-1-1 TO BYB-FEE-NO-ELIG(1 1).
           MOVE QBYB-FEE-NO-WROTE-1-1 TO BYB-FEE-NO-WROTE(1 1).
           MOVE QBYB-FEE-NO-NOTWROTE-1-1 TO BYB-FEE-NO-NOTWROTE(1 1).
           MOVE QBYB-FEE-AMT-WROTE-1-1 TO BYB-FEE-AMT-WROTE(1 1).
           MOVE QBYB-FEE-NO-ELIG-1-2 TO BYB-FEE-NO-ELIG(1 2).
           MOVE QBYB-FEE-NO-WROTE-1-2 TO BYB-FEE-NO-WROTE(1 2).
           MOVE QBYB-FEE-NO-NOTWROTE-1-2 TO BYB-FEE-NO-NOTWROTE(1 2).
           MOVE QBYB-FEE-AMT-WROTE-1-2 TO BYB-FEE-AMT-WROTE(1 2).
           MOVE QBYB-FEE-NO-ELIG-1-3 TO BYB-FEE-NO-ELIG(1 3).
           MOVE QBYB-FEE-NO-WROTE-1-3 TO BYB-FEE-NO-WROTE(1 3).
           MOVE QBYB-FEE-NO-NOTWROTE-1-3 TO BYB-FEE-NO-NOTWROTE(1 3).
           MOVE QBYB-FEE-AMT-WROTE-1-3 TO BYB-FEE-AMT-WROTE(1 3).
           MOVE QBYB-OTH-NO-COL-1 TO BYB-OTH-NO-COL(1).
           MOVE QBYB-OT2-NO-COL-1 TO BYB-OT2-NO-COL(1).
           MOVE QBYB-INT-NO-COL-1 TO BYB-INT-NO-COL(1).
           MOVE QBYB-LATE-NO-COL-1 TO BYB-LATE-NO-COL(1).
           MOVE QBYB-OTH-NO-WAIVED-1 TO BYB-OTH-NO-WAIVED(1).
           MOVE QBYB-OT2-NO-WAIVED-1 TO BYB-OT2-NO-WAIVED(1).
           MOVE QBYB-INT-NO-WAIVED-1 TO BYB-INT-NO-WAIVED(1).
           MOVE QBYB-LATE-NO-WAIVED-1 TO BYB-LATE-NO-WAIVED(1).
           MOVE QBYB-OTH-AMT-WAIVED-1 TO BYB-OTH-AMT-WAIVED(1).
           MOVE QBYB-OT2-AMT-WAIVED-1 TO BYB-OT2-AMT-WAIVED(1).
           MOVE QBYB-INT-AMT-WAIVED-1 TO BYB-INT-AMT-WAIVED(1).
           MOVE QBYB-LATE-AMT-WAIVED-1 TO BYB-LATE-AMT-WAIVED(1).
           MOVE QBYB-PL-PRINCOL-2-1 TO BYB-PL-PRINCOL(2 1).
           MOVE QBYB-PL-INTCOL-2-1 TO BYB-PL-INTCOL(2 1).
           MOVE QBYB-PL-LATECOL-2-1 TO BYB-PL-LATECOL(2 1).
           MOVE QBYB-PL-O2COL-2-1 TO BYB-PL-O2COL(2 1).
           MOVE QBYB-PL-PRINCOL-2-2 TO BYB-PL-PRINCOL(2 2).
           MOVE QBYB-PL-INTCOL-2-2 TO BYB-PL-INTCOL(2 2).
           MOVE QBYB-PL-LATECOL-2-2 TO BYB-PL-LATECOL(2 2).
           MOVE QBYB-PL-O2COL-2-2 TO BYB-PL-O2COL(2 2).
           MOVE QBYB-PL-PRINCOL-2-3 TO BYB-PL-PRINCOL(2 3).
           MOVE QBYB-PL-INTCOL-2-3 TO BYB-PL-INTCOL(2 3).
           MOVE QBYB-PL-LATECOL-2-3 TO BYB-PL-LATECOL(2 3).
           MOVE QBYB-PL-O2COL-2-3 TO BYB-PL-O2COL(2 3).
           MOVE QBYB-PL-PRINCOL-2-4 TO BYB-PL-PRINCOL(2 4).
           MOVE QBYB-PL-INTCOL-2-4 TO BYB-PL-INTCOL(2 4).
           MOVE QBYB-PL-LATECOL-2-4 TO BYB-PL-LATECOL(2 4).
           MOVE QBYB-PL-O2COL-2-4 TO BYB-PL-O2COL(2 4).
           MOVE QBYB-PL-PRINCOL-2-5 TO BYB-PL-PRINCOL(2 5).
           MOVE QBYB-PL-INTCOL-2-5 TO BYB-PL-INTCOL(2 5).
           MOVE QBYB-PL-LATECOL-2-5 TO BYB-PL-LATECOL(2 5).
           MOVE QBYB-PL-O2COL-2-5 TO BYB-PL-O2COL(2 5).
           MOVE QBYB-AMT-SOLD-2 TO BYB-AMT-SOLD(2).
           MOVE QBYB-AMT-TRANS-2 TO BYB-AMT-TRANS(2).
           MOVE QBYB-AMT-PURCH-2 TO BYB-AMT-PURCH(2).
           MOVE QBYB-NO-SOLD-2 TO BYB-NO-SOLD(2).
           MOVE QBYB-NO-TRANS-2 TO BYB-NO-TRANS(2).
           MOVE QBYB-NO-PURCH-2 TO BYB-NO-PURCH(2).
           MOVE QBYB-OTHCOL-2 TO BYB-OTHCOL(2).
           MOVE QBYB-OT2COL-2 TO BYB-OT2COL(2).
           MOVE QBYB-INTCOL-2 TO BYB-INTCOL(2).
           MOVE QBYB-LATECOL-2 TO BYB-LATECOL(2).
           MOVE QBYB-PRINCOL-2 TO BYB-PRINCOL(2).
           MOVE QBYB-DEFCOL-2 TO BYB-DEFCOL(2).
           MOVE QBYB-RNPRIN-2 TO BYB-RNPRIN(2).
           MOVE QBYB-INTREB-2 TO BYB-INTREB(2).
           MOVE QBYB-CLREB1-2 TO BYB-CLREB1(2).
           MOVE QBYB-CLREB2-2 TO BYB-CLREB2(2).
           MOVE QBYB-CLREB3-2 TO BYB-CLREB3(2).
           MOVE QBYB-CLREB4-2 TO BYB-CLREB4(2).
           MOVE QBYB-AHREB-2 TO BYB-AHREB(2).
           MOVE QBYB-PPREB-2 TO BYB-PPREB(2).
           MOVE QBYB-O1REB-2 TO BYB-O1REB(2).
           MOVE QBYB-O2REB-2 TO BYB-O2REB(2).
           MOVE QBYB-O3REB-2 TO BYB-O3REB(2).
           MOVE QBYB-O4REB-2 TO BYB-O4REB(2).
           MOVE QBYB-O5REB-2 TO BYB-O5REB(2).
           MOVE QBYB-O6REB-2 TO BYB-O6REB(2).
           MOVE QBYB-O7REB-2 TO BYB-O7REB(2).
           MOVE QBYB-SCREB-2 TO BYB-SCREB(2).
           MOVE QBYB-MFREB-2 TO BYB-MFREB(2).
           MOVE QBYB-DFREB-2 TO BYB-DFREB(2).
           MOVE QBYB-UN7-REB-2 TO BYB-UN7-REB(2).
           MOVE QBYB-UN8-REB-2 TO BYB-UN8-REB(2).
           MOVE QBYB-UN9-REB-2 TO BYB-UN9-REB(2).
           MOVE QBYB-UN10-REB-2 TO BYB-UN10-REB(2).
           MOVE QBYB-PART-CHGOFF-2 TO BYB-PART-CHGOFF(2).
           MOVE QBYB-ALLOTMENTS-2 TO BYB-ALLOTMENTS(2).
           MOVE QBYB-INS-NO-ELIG-2-1 TO BYB-INS-NO-ELIG(2 1).
           MOVE QBYB-INS-NO-WROTE-2-1 TO BYB-INS-NO-WROTE(2 1).
           MOVE QBYB-INS-NO-NOTWROTE-2-1 TO BYB-INS-NO-NOTWROTE(2 1).
           MOVE QBYB-INS-AMT-WROTE-2-1 TO BYB-INS-AMT-WROTE(2 1).
           MOVE QBYB-INS-NO-ELIG-2-2 TO BYB-INS-NO-ELIG(2 2).
           MOVE QBYB-INS-NO-WROTE-2-2 TO BYB-INS-NO-WROTE(2 2).
           MOVE QBYB-INS-NO-NOTWROTE-2-2 TO BYB-INS-NO-NOTWROTE(2 2).
           MOVE QBYB-INS-AMT-WROTE-2-2 TO BYB-INS-AMT-WROTE(2 2).
           MOVE QBYB-INS-NO-ELIG-2-3 TO BYB-INS-NO-ELIG(2 3).
           MOVE QBYB-INS-NO-WROTE-2-3 TO BYB-INS-NO-WROTE(2 3).
           MOVE QBYB-INS-NO-NOTWROTE-2-3 TO BYB-INS-NO-NOTWROTE(2 3).
           MOVE QBYB-INS-AMT-WROTE-2-3 TO BYB-INS-AMT-WROTE(2 3).
           MOVE QBYB-INS-NO-ELIG-2-4 TO BYB-INS-NO-ELIG(2 4).
           MOVE QBYB-INS-NO-WROTE-2-4 TO BYB-INS-NO-WROTE(2 4).
           MOVE QBYB-INS-NO-NOTWROTE-2-4 TO BYB-INS-NO-NOTWROTE(2 4).
           MOVE QBYB-INS-AMT-WROTE-2-4 TO BYB-INS-AMT-WROTE(2 4).
           MOVE QBYB-INS-NO-ELIG-2-5 TO BYB-INS-NO-ELIG(2 5).
           MOVE QBYB-INS-NO-WROTE-2-5 TO BYB-INS-NO-WROTE(2 5).
           MOVE QBYB-INS-NO-NOTWROTE-2-5 TO BYB-INS-NO-NOTWROTE(2 5).
           MOVE QBYB-INS-AMT-WROTE-2-5 TO BYB-INS-AMT-WROTE(2 5).
           MOVE QBYB-INS-NO-ELIG-2-6 TO BYB-INS-NO-ELIG(2 6).
           MOVE QBYB-INS-NO-WROTE-2-6 TO BYB-INS-NO-WROTE(2 6).
           MOVE QBYB-INS-NO-NOTWROTE-2-6 TO BYB-INS-NO-NOTWROTE(2 6).
           MOVE QBYB-INS-AMT-WROTE-2-6 TO BYB-INS-AMT-WROTE(2 6).
           MOVE QBYB-INS-NO-ELIG-2-7 TO BYB-INS-NO-ELIG(2 7).
           MOVE QBYB-INS-NO-WROTE-2-7 TO BYB-INS-NO-WROTE(2 7).
           MOVE QBYB-INS-NO-NOTWROTE-2-7 TO BYB-INS-NO-NOTWROTE(2 7).
           MOVE QBYB-INS-AMT-WROTE-2-7 TO BYB-INS-AMT-WROTE(2 7).
           MOVE QBYB-INS-NO-ELIG-2-8 TO BYB-INS-NO-ELIG(2 8).
           MOVE QBYB-INS-NO-WROTE-2-8 TO BYB-INS-NO-WROTE(2 8).
           MOVE QBYB-INS-NO-NOTWROTE-2-8 TO BYB-INS-NO-NOTWROTE(2 8).
           MOVE QBYB-INS-AMT-WROTE-2-8 TO BYB-INS-AMT-WROTE(2 8).
           MOVE QBYB-INS-NO-ELIG-2-9 TO BYB-INS-NO-ELIG(2 9).
           MOVE QBYB-INS-NO-WROTE-2-9 TO BYB-INS-NO-WROTE(2 9).
           MOVE QBYB-INS-NO-NOTWROTE-2-9 TO BYB-INS-NO-NOTWROTE(2 9).
           MOVE QBYB-INS-AMT-WROTE-2-9 TO BYB-INS-AMT-WROTE(2 9).
           MOVE QBYB-INS-NO-ELIG-2-10 TO BYB-INS-NO-ELIG(2 10).
           MOVE QBYB-INS-NO-WROTE-2-10 TO BYB-INS-NO-WROTE(2 10).
           MOVE QBYB-INS-NO-NOTWROTE-2-10 TO BYB-INS-NO-NOTWROTE(2 10).
           MOVE QBYB-INS-AMT-WROTE-2-10 TO BYB-INS-AMT-WROTE(2 10).
           MOVE QBYB-FEE-NO-ELIG-2-1 TO BYB-FEE-NO-ELIG(2 1).
           MOVE QBYB-FEE-NO-WROTE-2-1 TO BYB-FEE-NO-WROTE(2 1).
           MOVE QBYB-FEE-NO-NOTWROTE-2-1 TO BYB-FEE-NO-NOTWROTE(2 1).
           MOVE QBYB-FEE-AMT-WROTE-2-1 TO BYB-FEE-AMT-WROTE(2 1).
           MOVE QBYB-FEE-NO-ELIG-2-2 TO BYB-FEE-NO-ELIG(2 2).
           MOVE QBYB-FEE-NO-WROTE-2-2 TO BYB-FEE-NO-WROTE(2 2).
           MOVE QBYB-FEE-NO-NOTWROTE-2-2 TO BYB-FEE-NO-NOTWROTE(2 2).
           MOVE QBYB-FEE-AMT-WROTE-2-2 TO BYB-FEE-AMT-WROTE(2 2).
           MOVE QBYB-FEE-NO-ELIG-2-3 TO BYB-FEE-NO-ELIG(2 3).
           MOVE QBYB-FEE-NO-WROTE-2-3 TO BYB-FEE-NO-WROTE(2 3).
           MOVE QBYB-FEE-NO-NOTWROTE-2-3 TO BYB-FEE-NO-NOTWROTE(2 3).
           MOVE QBYB-FEE-AMT-WROTE-2-3 TO BYB-FEE-AMT-WROTE(2 3).
           MOVE QBYB-OTH-NO-COL-2 TO BYB-OTH-NO-COL(2).
           MOVE QBYB-OT2-NO-COL-2 TO BYB-OT2-NO-COL(2).
           MOVE QBYB-INT-NO-COL-2 TO BYB-INT-NO-COL(2).
           MOVE QBYB-LATE-NO-COL-2 TO BYB-LATE-NO-COL(2).
           MOVE QBYB-OTH-NO-WAIVED-2 TO BYB-OTH-NO-WAIVED(2).
           MOVE QBYB-OT2-NO-WAIVED-2 TO BYB-OT2-NO-WAIVED(2).
           MOVE QBYB-INT-NO-WAIVED-2 TO BYB-INT-NO-WAIVED(2).
           MOVE QBYB-LATE-NO-WAIVED-2 TO BYB-LATE-NO-WAIVED(2).
           MOVE QBYB-OTH-AMT-WAIVED-2 TO BYB-OTH-AMT-WAIVED(2).
           MOVE QBYB-OT2-AMT-WAIVED-2 TO BYB-OT2-AMT-WAIVED(2).
           MOVE QBYB-INT-AMT-WAIVED-2 TO BYB-INT-AMT-WAIVED(2).
           MOVE QBYB-LATE-AMT-WAIVED-2 TO BYB-LATE-AMT-WAIVED(2).
           MOVE QBYB-UNITS-1-1-1 TO BYB-UNITS(1 1 1).
           MOVE QBYB-OUTBAL-1-1-1 TO BYB-OUTBAL(1 1 1).
           MOVE QBYB-UNITS-1-1-2 TO BYB-UNITS(1 1 2).
           MOVE QBYB-OUTBAL-1-1-2 TO BYB-OUTBAL(1 1 2).
           MOVE QBYB-UNITS-1-1-3 TO BYB-UNITS(1 1 3).
           MOVE QBYB-OUTBAL-1-1-3 TO BYB-OUTBAL(1 1 3).
           MOVE QBYB-UNITS-1-1-4 TO BYB-UNITS(1 1 4).
           MOVE QBYB-OUTBAL-1-1-4 TO BYB-OUTBAL(1 1 4).
           MOVE QBYB-UNITS-1-1-5 TO BYB-UNITS(1 1 5).
           MOVE QBYB-OUTBAL-1-1-5 TO BYB-OUTBAL(1 1 5).
           MOVE QBYB-UNITS-1-1-6 TO BYB-UNITS(1 1 6).
           MOVE QBYB-OUTBAL-1-1-6 TO BYB-OUTBAL(1 1 6).
           MOVE QBYB-UNITS-1-1-7 TO BYB-UNITS(1 1 7).
           MOVE QBYB-OUTBAL-1-1-7 TO BYB-OUTBAL(1 1 7).
           MOVE QBYB-UNITS-1-1-8 TO BYB-UNITS(1 1 8).
           MOVE QBYB-OUTBAL-1-1-8 TO BYB-OUTBAL(1 1 8).
           MOVE QBYB-UNITS-1-1-9 TO BYB-UNITS(1 1 9).
           MOVE QBYB-OUTBAL-1-1-9 TO BYB-OUTBAL(1 1 9).
           MOVE QBYB-UNITS-1-1-10 TO BYB-UNITS(1 1 10).
           MOVE QBYB-OUTBAL-1-1-10 TO BYB-OUTBAL(1 1 10).
           MOVE QBYB-UNITS-1-2-1 TO BYB-UNITS(1 2 1).
           MOVE QBYB-OUTBAL-1-2-1 TO BYB-OUTBAL(1 2 1).
           MOVE QBYB-UNITS-1-2-2 TO BYB-UNITS(1 2 2).
           MOVE QBYB-OUTBAL-1-2-2 TO BYB-OUTBAL(1 2 2).
           MOVE QBYB-UNITS-1-2-3 TO BYB-UNITS(1 2 3).
           MOVE QBYB-OUTBAL-1-2-3 TO BYB-OUTBAL(1 2 3).
           MOVE QBYB-UNITS-1-2-4 TO BYB-UNITS(1 2 4).
           MOVE QBYB-OUTBAL-1-2-4 TO BYB-OUTBAL(1 2 4).
           MOVE QBYB-UNITS-1-2-5 TO BYB-UNITS(1 2 5).
           MOVE QBYB-OUTBAL-1-2-5 TO BYB-OUTBAL(1 2 5).
           MOVE QBYB-UNITS-1-2-6 TO BYB-UNITS(1 2 6).
           MOVE QBYB-OUTBAL-1-2-6 TO BYB-OUTBAL(1 2 6).
           MOVE QBYB-UNITS-1-2-7 TO BYB-UNITS(1 2 7).
           MOVE QBYB-OUTBAL-1-2-7 TO BYB-OUTBAL(1 2 7).
           MOVE QBYB-UNITS-1-2-8 TO BYB-UNITS(1 2 8).
           MOVE QBYB-OUTBAL-1-2-8 TO BYB-OUTBAL(1 2 8).
           MOVE QBYB-UNITS-1-2-9 TO BYB-UNITS(1 2 9).
           MOVE QBYB-OUTBAL-1-2-9 TO BYB-OUTBAL(1 2 9).
           MOVE QBYB-UNITS-1-2-10 TO BYB-UNITS(1 2 10).
           MOVE QBYB-OUTBAL-1-2-10 TO BYB-OUTBAL(1 2 10).
           MOVE QBYB-UNITS-2-1-1 TO BYB-UNITS(2 1 1).
           MOVE QBYB-OUTBAL-2-1-1 TO BYB-OUTBAL(2 1 1).
           MOVE QBYB-UNITS-2-1-2 TO BYB-UNITS(2 1 2).
           MOVE QBYB-OUTBAL-2-1-2 TO BYB-OUTBAL(2 1 2).
           MOVE QBYB-UNITS-2-1-3 TO BYB-UNITS(2 1 3).
           MOVE QBYB-OUTBAL-2-1-3 TO BYB-OUTBAL(2 1 3).
           MOVE QBYB-UNITS-2-1-4 TO BYB-UNITS(2 1 4).
           MOVE QBYB-OUTBAL-2-1-4 TO BYB-OUTBAL(2 1 4).
           MOVE QBYB-UNITS-2-1-5 TO BYB-UNITS(2 1 5).
           MOVE QBYB-OUTBAL-2-1-5 TO BYB-OUTBAL(2 1 5).
           MOVE QBYB-UNITS-2-1-6 TO BYB-UNITS(2 1 6).
           MOVE QBYB-OUTBAL-2-1-6 TO BYB-OUTBAL(2 1 6).
           MOVE QBYB-UNITS-2-1-7 TO BYB-UNITS(2 1 7).
           MOVE QBYB-OUTBAL-2-1-7 TO BYB-OUTBAL(2 1 7).
           MOVE QBYB-UNITS-2-1-8 TO BYB-UNITS(2 1 8).
           MOVE QBYB-OUTBAL-2-1-8 TO BYB-OUTBAL(2 1 8).
           MOVE QBYB-UNITS-2-1-9 TO BYB-UNITS(2 1 9).
           MOVE QBYB-OUTBAL-2-1-9 TO BYB-OUTBAL(2 1 9).
           MOVE QBYB-UNITS-2-1-10 TO BYB-UNITS(2 1 10).
           MOVE QBYB-OUTBAL-2-1-10 TO BYB-OUTBAL(2 1 10).
           MOVE QBYB-UNITS-2-2-1 TO BYB-UNITS(2 2 1).
           MOVE QBYB-OUTBAL-2-2-1 TO BYB-OUTBAL(2 2 1).
           MOVE QBYB-UNITS-2-2-2 TO BYB-UNITS(2 2 2).
           MOVE QBYB-OUTBAL-2-2-2 TO BYB-OUTBAL(2 2 2).
           MOVE QBYB-UNITS-2-2-3 TO BYB-UNITS(2 2 3).
           MOVE QBYB-OUTBAL-2-2-3 TO BYB-OUTBAL(2 2 3).
           MOVE QBYB-UNITS-2-2-4 TO BYB-UNITS(2 2 4).
           MOVE QBYB-OUTBAL-2-2-4 TO BYB-OUTBAL(2 2 4).
           MOVE QBYB-UNITS-2-2-5 TO BYB-UNITS(2 2 5).
           MOVE QBYB-OUTBAL-2-2-5 TO BYB-OUTBAL(2 2 5).
           MOVE QBYB-UNITS-2-2-6 TO BYB-UNITS(2 2 6).
           MOVE QBYB-OUTBAL-2-2-6 TO BYB-OUTBAL(2 2 6).
           MOVE QBYB-UNITS-2-2-7 TO BYB-UNITS(2 2 7).
           MOVE QBYB-OUTBAL-2-2-7 TO BYB-OUTBAL(2 2 7).
           MOVE QBYB-UNITS-2-2-8 TO BYB-UNITS(2 2 8).
           MOVE QBYB-OUTBAL-2-2-8 TO BYB-OUTBAL(2 2 8).
           MOVE QBYB-UNITS-2-2-9 TO BYB-UNITS(2 2 9).
           MOVE QBYB-OUTBAL-2-2-9 TO BYB-OUTBAL(2 2 9).
           MOVE QBYB-UNITS-2-2-10 TO BYB-UNITS(2 2 10).
           MOVE QBYB-OUTBAL-2-2-10 TO BYB-OUTBAL(2 2 10).
           MOVE QBYB-UNITS-3-1-1 TO BYB-UNITS(3 1 1).
           MOVE QBYB-OUTBAL-3-1-1 TO BYB-OUTBAL(3 1 1).
           MOVE QBYB-UNITS-3-1-2 TO BYB-UNITS(3 1 2).
           MOVE QBYB-OUTBAL-3-1-2 TO BYB-OUTBAL(3 1 2).
           MOVE QBYB-UNITS-3-1-3 TO BYB-UNITS(3 1 3).
           MOVE QBYB-OUTBAL-3-1-3 TO BYB-OUTBAL(3 1 3).
           MOVE QBYB-UNITS-3-1-4 TO BYB-UNITS(3 1 4).
           MOVE QBYB-OUTBAL-3-1-4 TO BYB-OUTBAL(3 1 4).
           MOVE QBYB-UNITS-3-1-5 TO BYB-UNITS(3 1 5).
           MOVE QBYB-OUTBAL-3-1-5 TO BYB-OUTBAL(3 1 5).
           MOVE QBYB-UNITS-3-1-6 TO BYB-UNITS(3 1 6).
           MOVE QBYB-OUTBAL-3-1-6 TO BYB-OUTBAL(3 1 6).
           MOVE QBYB-UNITS-3-1-7 TO BYB-UNITS(3 1 7).
           MOVE QBYB-OUTBAL-3-1-7 TO BYB-OUTBAL(3 1 7).
           MOVE QBYB-UNITS-3-1-8 TO BYB-UNITS(3 1 8).
           MOVE QBYB-OUTBAL-3-1-8 TO BYB-OUTBAL(3 1 8).
           MOVE QBYB-UNITS-3-1-9 TO BYB-UNITS(3 1 9).
           MOVE QBYB-OUTBAL-3-1-9 TO BYB-OUTBAL(3 1 9).
           MOVE QBYB-UNITS-3-1-10 TO BYB-UNITS(3 1 10).
           MOVE QBYB-OUTBAL-3-1-10 TO BYB-OUTBAL(3 1 10).
           MOVE QBYB-UNITS-3-2-1 TO BYB-UNITS(3 2 1).
           MOVE QBYB-OUTBAL-3-2-1 TO BYB-OUTBAL(3 2 1).
           MOVE QBYB-UNITS-3-2-2 TO BYB-UNITS(3 2 2).
           MOVE QBYB-OUTBAL-3-2-2 TO BYB-OUTBAL(3 2 2).
           MOVE QBYB-UNITS-3-2-3 TO BYB-UNITS(3 2 3).
           MOVE QBYB-OUTBAL-3-2-3 TO BYB-OUTBAL(3 2 3).
           MOVE QBYB-UNITS-3-2-4 TO BYB-UNITS(3 2 4).
           MOVE QBYB-OUTBAL-3-2-4 TO BYB-OUTBAL(3 2 4).
           MOVE QBYB-UNITS-3-2-5 TO BYB-UNITS(3 2 5).
           MOVE QBYB-OUTBAL-3-2-5 TO BYB-OUTBAL(3 2 5).
           MOVE QBYB-UNITS-3-2-6 TO BYB-UNITS(3 2 6).
           MOVE QBYB-OUTBAL-3-2-6 TO BYB-OUTBAL(3 2 6).
           MOVE QBYB-UNITS-3-2-7 TO BYB-UNITS(3 2 7).
           MOVE QBYB-OUTBAL-3-2-7 TO BYB-OUTBAL(3 2 7).
           MOVE QBYB-UNITS-3-2-8 TO BYB-UNITS(3 2 8).
           MOVE QBYB-OUTBAL-3-2-8 TO BYB-OUTBAL(3 2 8).
           MOVE QBYB-UNITS-3-2-9 TO BYB-UNITS(3 2 9).
           MOVE QBYB-OUTBAL-3-2-9 TO BYB-OUTBAL(3 2 9).
           MOVE QBYB-UNITS-3-2-10 TO BYB-UNITS(3 2 10).
           MOVE QBYB-OUTBAL-3-2-10 TO BYB-OUTBAL(3 2 10).
           MOVE QBYB-UNITS-BNKRPT-1-1-1 TO BYB-UNITS-BNKRPT(1 1 1).
           MOVE QBYB-OUTBAL-BNKRPT-1-1-1 TO BYB-OUTBAL-BNKRPT(1 1 1).
           MOVE QBYB-UNITS-BNKRPT-1-1-2 TO BYB-UNITS-BNKRPT(1 1 2).
           MOVE QBYB-OUTBAL-BNKRPT-1-1-2 TO BYB-OUTBAL-BNKRPT(1 1 2).
           MOVE QBYB-UNITS-BNKRPT-1-1-3 TO BYB-UNITS-BNKRPT(1 1 3).
           MOVE QBYB-OUTBAL-BNKRPT-1-1-3 TO BYB-OUTBAL-BNKRPT(1 1 3).
           MOVE QBYB-UNITS-BNKRPT-1-1-4 TO BYB-UNITS-BNKRPT(1 1 4).
           MOVE QBYB-OUTBAL-BNKRPT-1-1-4 TO BYB-OUTBAL-BNKRPT(1 1 4).
           MOVE QBYB-UNITS-BNKRPT-1-1-5 TO BYB-UNITS-BNKRPT(1 1 5).
           MOVE QBYB-OUTBAL-BNKRPT-1-1-5 TO BYB-OUTBAL-BNKRPT(1 1 5).
           MOVE QBYB-UNITS-BNKRPT-1-1-6 TO BYB-UNITS-BNKRPT(1 1 6).
           MOVE QBYB-OUTBAL-BNKRPT-1-1-6 TO BYB-OUTBAL-BNKRPT(1 1 6).
           MOVE QBYB-UNITS-BNKRPT-1-1-7 TO BYB-UNITS-BNKRPT(1 1 7).
           MOVE QBYB-OUTBAL-BNKRPT-1-1-7 TO BYB-OUTBAL-BNKRPT(1 1 7).
           MOVE QBYB-UNITS-BNKRPT-1-1-8 TO BYB-UNITS-BNKRPT(1 1 8).
           MOVE QBYB-OUTBAL-BNKRPT-1-1-8 TO BYB-OUTBAL-BNKRPT(1 1 8).
           MOVE QBYB-UNITS-BNKRPT-1-1-9 TO BYB-UNITS-BNKRPT(1 1 9).
           MOVE QBYB-OUTBAL-BNKRPT-1-1-9 TO BYB-OUTBAL-BNKRPT(1 1 9).
           MOVE QBYB-UNITS-BNKRPT-1-1-10 TO BYB-UNITS-BNKRPT(1 1 10).
           MOVE QBYB-OUTBAL-BNKRPT-1-1-10 TO BYB-OUTBAL-BNKRPT(1 1 10).
           MOVE QBYB-UNITS-BNKRPT-1-2-1 TO BYB-UNITS-BNKRPT(1 2 1).
           MOVE QBYB-OUTBAL-BNKRPT-1-2-1 TO BYB-OUTBAL-BNKRPT(1 2 1).
           MOVE QBYB-UNITS-BNKRPT-1-2-2 TO BYB-UNITS-BNKRPT(1 2 2).
           MOVE QBYB-OUTBAL-BNKRPT-1-2-2 TO BYB-OUTBAL-BNKRPT(1 2 2).
           MOVE QBYB-UNITS-BNKRPT-1-2-3 TO BYB-UNITS-BNKRPT(1 2 3).
           MOVE QBYB-OUTBAL-BNKRPT-1-2-3 TO BYB-OUTBAL-BNKRPT(1 2 3).
           MOVE QBYB-UNITS-BNKRPT-1-2-4 TO BYB-UNITS-BNKRPT(1 2 4).
           MOVE QBYB-OUTBAL-BNKRPT-1-2-4 TO BYB-OUTBAL-BNKRPT(1 2 4).
           MOVE QBYB-UNITS-BNKRPT-1-2-5 TO BYB-UNITS-BNKRPT(1 2 5).
           MOVE QBYB-OUTBAL-BNKRPT-1-2-5 TO BYB-OUTBAL-BNKRPT(1 2 5).
           MOVE QBYB-UNITS-BNKRPT-1-2-6 TO BYB-UNITS-BNKRPT(1 2 6).
           MOVE QBYB-OUTBAL-BNKRPT-1-2-6 TO BYB-OUTBAL-BNKRPT(1 2 6).
           MOVE QBYB-UNITS-BNKRPT-1-2-7 TO BYB-UNITS-BNKRPT(1 2 7).
           MOVE QBYB-OUTBAL-BNKRPT-1-2-7 TO BYB-OUTBAL-BNKRPT(1 2 7).
           MOVE QBYB-UNITS-BNKRPT-1-2-8 TO BYB-UNITS-BNKRPT(1 2 8).
           MOVE QBYB-OUTBAL-BNKRPT-1-2-8 TO BYB-OUTBAL-BNKRPT(1 2 8).
           MOVE QBYB-UNITS-BNKRPT-1-2-9 TO BYB-UNITS-BNKRPT(1 2 9).
           MOVE QBYB-OUTBAL-BNKRPT-1-2-9 TO BYB-OUTBAL-BNKRPT(1 2 9).
           MOVE QBYB-UNITS-BNKRPT-1-2-10 TO BYB-UNITS-BNKRPT(1 2 10).
           MOVE QBYB-OUTBAL-BNKRPT-1-2-10 TO BYB-OUTBAL-BNKRPT(1 2 10).
           MOVE QBYB-UNITS-BNKRPT-2-1-1 TO BYB-UNITS-BNKRPT(2 1 1).
           MOVE QBYB-OUTBAL-BNKRPT-2-1-1 TO BYB-OUTBAL-BNKRPT(2 1 1).
           MOVE QBYB-UNITS-BNKRPT-2-1-2 TO BYB-UNITS-BNKRPT(2 1 2).
           MOVE QBYB-OUTBAL-BNKRPT-2-1-2 TO BYB-OUTBAL-BNKRPT(2 1 2).
           MOVE QBYB-UNITS-BNKRPT-2-1-3 TO BYB-UNITS-BNKRPT(2 1 3).
           MOVE QBYB-OUTBAL-BNKRPT-2-1-3 TO BYB-OUTBAL-BNKRPT(2 1 3).
           MOVE QBYB-UNITS-BNKRPT-2-1-4 TO BYB-UNITS-BNKRPT(2 1 4).
           MOVE QBYB-OUTBAL-BNKRPT-2-1-4 TO BYB-OUTBAL-BNKRPT(2 1 4).
           MOVE QBYB-UNITS-BNKRPT-2-1-5 TO BYB-UNITS-BNKRPT(2 1 5).
           MOVE QBYB-OUTBAL-BNKRPT-2-1-5 TO BYB-OUTBAL-BNKRPT(2 1 5).
           MOVE QBYB-UNITS-BNKRPT-2-1-6 TO BYB-UNITS-BNKRPT(2 1 6).
           MOVE QBYB-OUTBAL-BNKRPT-2-1-6 TO BYB-OUTBAL-BNKRPT(2 1 6).
           MOVE QBYB-UNITS-BNKRPT-2-1-7 TO BYB-UNITS-BNKRPT(2 1 7).
           MOVE QBYB-OUTBAL-BNKRPT-2-1-7 TO BYB-OUTBAL-BNKRPT(2 1 7).
           MOVE QBYB-UNITS-BNKRPT-2-1-8 TO BYB-UNITS-BNKRPT(2 1 8).
           MOVE QBYB-OUTBAL-BNKRPT-2-1-8 TO BYB-OUTBAL-BNKRPT(2 1 8).
           MOVE QBYB-UNITS-BNKRPT-2-1-9 TO BYB-UNITS-BNKRPT(2 1 9).
           MOVE QBYB-OUTBAL-BNKRPT-2-1-9 TO BYB-OUTBAL-BNKRPT(2 1 9).
           MOVE QBYB-UNITS-BNKRPT-2-1-10 TO BYB-UNITS-BNKRPT(2 1 10).
           MOVE QBYB-OUTBAL-BNKRPT-2-1-10 TO BYB-OUTBAL-BNKRPT(2 1 10).
           MOVE QBYB-UNITS-BNKRPT-2-2-1 TO BYB-UNITS-BNKRPT(2 2 1).
           MOVE QBYB-OUTBAL-BNKRPT-2-2-1 TO BYB-OUTBAL-BNKRPT(2 2 1).
           MOVE QBYB-UNITS-BNKRPT-2-2-2 TO BYB-UNITS-BNKRPT(2 2 2).
           MOVE QBYB-OUTBAL-BNKRPT-2-2-2 TO BYB-OUTBAL-BNKRPT(2 2 2).
           MOVE QBYB-UNITS-BNKRPT-2-2-3 TO BYB-UNITS-BNKRPT(2 2 3).
           MOVE QBYB-OUTBAL-BNKRPT-2-2-3 TO BYB-OUTBAL-BNKRPT(2 2 3).
           MOVE QBYB-UNITS-BNKRPT-2-2-4 TO BYB-UNITS-BNKRPT(2 2 4).
           MOVE QBYB-OUTBAL-BNKRPT-2-2-4 TO BYB-OUTBAL-BNKRPT(2 2 4).
           MOVE QBYB-UNITS-BNKRPT-2-2-5 TO BYB-UNITS-BNKRPT(2 2 5).
           MOVE QBYB-OUTBAL-BNKRPT-2-2-5 TO BYB-OUTBAL-BNKRPT(2 2 5).
           MOVE QBYB-UNITS-BNKRPT-2-2-6 TO BYB-UNITS-BNKRPT(2 2 6).
           MOVE QBYB-OUTBAL-BNKRPT-2-2-6 TO BYB-OUTBAL-BNKRPT(2 2 6).
           MOVE QBYB-UNITS-BNKRPT-2-2-7 TO BYB-UNITS-BNKRPT(2 2 7).
           MOVE QBYB-OUTBAL-BNKRPT-2-2-7 TO BYB-OUTBAL-BNKRPT(2 2 7).
           MOVE QBYB-UNITS-BNKRPT-2-2-8 TO BYB-UNITS-BNKRPT(2 2 8).
           MOVE QBYB-OUTBAL-BNKRPT-2-2-8 TO BYB-OUTBAL-BNKRPT(2 2 8).
           MOVE QBYB-UNITS-BNKRPT-2-2-9 TO BYB-UNITS-BNKRPT(2 2 9).
           MOVE QBYB-OUTBAL-BNKRPT-2-2-9 TO BYB-OUTBAL-BNKRPT(2 2 9).
           MOVE QBYB-UNITS-BNKRPT-2-2-10 TO BYB-UNITS-BNKRPT(2 2 10).
           MOVE QBYB-OUTBAL-BNKRPT-2-2-10 TO BYB-OUTBAL-BNKRPT(2 2 10).
           MOVE QBYB-UNITS-BNKRPT-3-1-1 TO BYB-UNITS-BNKRPT(3 1 1).
           MOVE QBYB-OUTBAL-BNKRPT-3-1-1 TO BYB-OUTBAL-BNKRPT(3 1 1).
           MOVE QBYB-UNITS-BNKRPT-3-1-2 TO BYB-UNITS-BNKRPT(3 1 2).
           MOVE QBYB-OUTBAL-BNKRPT-3-1-2 TO BYB-OUTBAL-BNKRPT(3 1 2).
           MOVE QBYB-UNITS-BNKRPT-3-1-3 TO BYB-UNITS-BNKRPT(3 1 3).
           MOVE QBYB-OUTBAL-BNKRPT-3-1-3 TO BYB-OUTBAL-BNKRPT(3 1 3).
           MOVE QBYB-UNITS-BNKRPT-3-1-4 TO BYB-UNITS-BNKRPT(3 1 4).
           MOVE QBYB-OUTBAL-BNKRPT-3-1-4 TO BYB-OUTBAL-BNKRPT(3 1 4).
           MOVE QBYB-UNITS-BNKRPT-3-1-5 TO BYB-UNITS-BNKRPT(3 1 5).
           MOVE QBYB-OUTBAL-BNKRPT-3-1-5 TO BYB-OUTBAL-BNKRPT(3 1 5).
           MOVE QBYB-UNITS-BNKRPT-3-1-6 TO BYB-UNITS-BNKRPT(3 1 6).
           MOVE QBYB-OUTBAL-BNKRPT-3-1-6 TO BYB-OUTBAL-BNKRPT(3 1 6).
           MOVE QBYB-UNITS-BNKRPT-3-1-7 TO BYB-UNITS-BNKRPT(3 1 7).
           MOVE QBYB-OUTBAL-BNKRPT-3-1-7 TO BYB-OUTBAL-BNKRPT(3 1 7).
           MOVE QBYB-UNITS-BNKRPT-3-1-8 TO BYB-UNITS-BNKRPT(3 1 8).
           MOVE QBYB-OUTBAL-BNKRPT-3-1-8 TO BYB-OUTBAL-BNKRPT(3 1 8).
           MOVE QBYB-UNITS-BNKRPT-3-1-9 TO BYB-UNITS-BNKRPT(3 1 9).
           MOVE QBYB-OUTBAL-BNKRPT-3-1-9 TO BYB-OUTBAL-BNKRPT(3 1 9).
           MOVE QBYB-UNITS-BNKRPT-3-1-10 TO BYB-UNITS-BNKRPT(3 1 10).
           MOVE QBYB-OUTBAL-BNKRPT-3-1-10 TO BYB-OUTBAL-BNKRPT(3 1 10).
           MOVE QBYB-UNITS-BNKRPT-3-2-1 TO BYB-UNITS-BNKRPT(3 2 1).
           MOVE QBYB-OUTBAL-BNKRPT-3-2-1 TO BYB-OUTBAL-BNKRPT(3 2 1).
           MOVE QBYB-UNITS-BNKRPT-3-2-2 TO BYB-UNITS-BNKRPT(3 2 2).
           MOVE QBYB-OUTBAL-BNKRPT-3-2-2 TO BYB-OUTBAL-BNKRPT(3 2 2).
           MOVE QBYB-UNITS-BNKRPT-3-2-3 TO BYB-UNITS-BNKRPT(3 2 3).
           MOVE QBYB-OUTBAL-BNKRPT-3-2-3 TO BYB-OUTBAL-BNKRPT(3 2 3).
           MOVE QBYB-UNITS-BNKRPT-3-2-4 TO BYB-UNITS-BNKRPT(3 2 4).
           MOVE QBYB-OUTBAL-BNKRPT-3-2-4 TO BYB-OUTBAL-BNKRPT(3 2 4).
           MOVE QBYB-UNITS-BNKRPT-3-2-5 TO BYB-UNITS-BNKRPT(3 2 5).
           MOVE QBYB-OUTBAL-BNKRPT-3-2-5 TO BYB-OUTBAL-BNKRPT(3 2 5).
           MOVE QBYB-UNITS-BNKRPT-3-2-6 TO BYB-UNITS-BNKRPT(3 2 6).
           MOVE QBYB-OUTBAL-BNKRPT-3-2-6 TO BYB-OUTBAL-BNKRPT(3 2 6).
           MOVE QBYB-UNITS-BNKRPT-3-2-7 TO BYB-UNITS-BNKRPT(3 2 7).
           MOVE QBYB-OUTBAL-BNKRPT-3-2-7 TO BYB-OUTBAL-BNKRPT(3 2 7).
           MOVE QBYB-UNITS-BNKRPT-3-2-8 TO BYB-UNITS-BNKRPT(3 2 8).
           MOVE QBYB-OUTBAL-BNKRPT-3-2-8 TO BYB-OUTBAL-BNKRPT(3 2 8).
           MOVE QBYB-UNITS-BNKRPT-3-2-9 TO BYB-UNITS-BNKRPT(3 2 9).
           MOVE QBYB-OUTBAL-BNKRPT-3-2-9 TO BYB-OUTBAL-BNKRPT(3 2 9).
           MOVE QBYB-UNITS-BNKRPT-3-2-10 TO BYB-UNITS-BNKRPT(3 2 10).
           MOVE QBYB-OUTBAL-BNKRPT-3-2-10 TO BYB-OUTBAL-BNKRPT(3 2 10).
           MOVE QBYB-SUMWGTTM TO BYB-SUMWGTTM.
           MOVE QBYB-SUMWGTAV TO BYB-SUMWGTAV.
           MOVE QBYB-SUMWGTTA TO BYB-SUMWGTTA.
           MOVE QBYB-IB-ACCRUED TO BYB-IB-ACCRUED.
           MOVE QBYB-DLDISC-1 TO BYB-DLDISC(1).
           MOVE QBYB-DLDISC-2 TO BYB-DLDISC(2).
           MOVE QBYB-DLDISC-3 TO BYB-DLDISC(3).
           MOVE QBYB-STD-POS TO BYB-STD-POS.
           MOVE QBYB-30-DAY-MOVED TO BYB-30-DAY-MOVED.
           MOVE QBYB-PROJ-INCREASE TO BYB-PROJ-INCREASE.
           MOVE QBYB-PROJ-PROFIT TO BYB-PROJ-PROFIT.
           MOVE QBYB-PROCEEDS-1 TO BYB-PROCEEDS(1).
           MOVE QBYB-PROCEEDS-2 TO BYB-PROCEEDS(2).
           MOVE QBYB-LOCKBOX-1 TO BYB-LOCKBOX(1).
           MOVE QBYB-LOCKBOX-2 TO BYB-LOCKBOX(2).
           MOVE QBYB-UNEARNED-AMT-1-1-1 TO BYB-UNEARNED-AMT(1 1 1).
           MOVE QBYB-UNEARNED-AMT-1-1-2 TO BYB-UNEARNED-AMT(1 1 2).
           MOVE QBYB-UNEARNED-AMT-1-1-3 TO BYB-UNEARNED-AMT(1 1 3).
           MOVE QBYB-UNEARNED-AMT-1-1-4 TO BYB-UNEARNED-AMT(1 1 4).
           MOVE QBYB-UNEARNED-AMT-1-1-5 TO BYB-UNEARNED-AMT(1 1 5).
           MOVE QBYB-UNEARNED-AMT-1-1-6 TO BYB-UNEARNED-AMT(1 1 6).
           MOVE QBYB-UNEARNED-AMT-1-1-7 TO BYB-UNEARNED-AMT(1 1 7).
           MOVE QBYB-UNEARNED-AMT-1-1-8 TO BYB-UNEARNED-AMT(1 1 8).
           MOVE QBYB-UNEARNED-AMT-1-1-9 TO BYB-UNEARNED-AMT(1 1 9).
           MOVE QBYB-UNEARNED-AMT-1-1-10 TO BYB-UNEARNED-AMT(1 1 10).
           MOVE QBYB-UNEARNED-AMT-1-2-1 TO BYB-UNEARNED-AMT(1 2 1).
           MOVE QBYB-UNEARNED-AMT-1-2-2 TO BYB-UNEARNED-AMT(1 2 2).
           MOVE QBYB-UNEARNED-AMT-1-2-3 TO BYB-UNEARNED-AMT(1 2 3).
           MOVE QBYB-UNEARNED-AMT-1-2-4 TO BYB-UNEARNED-AMT(1 2 4).
           MOVE QBYB-UNEARNED-AMT-1-2-5 TO BYB-UNEARNED-AMT(1 2 5).
           MOVE QBYB-UNEARNED-AMT-1-2-6 TO BYB-UNEARNED-AMT(1 2 6).
           MOVE QBYB-UNEARNED-AMT-1-2-7 TO BYB-UNEARNED-AMT(1 2 7).
           MOVE QBYB-UNEARNED-AMT-1-2-8 TO BYB-UNEARNED-AMT(1 2 8).
           MOVE QBYB-UNEARNED-AMT-1-2-9 TO BYB-UNEARNED-AMT(1 2 9).
           MOVE QBYB-UNEARNED-AMT-1-2-10 TO BYB-UNEARNED-AMT(1 2 10).
           MOVE QBYB-UNEARNED-AMT-2-1-1 TO BYB-UNEARNED-AMT(2 1 1).
           MOVE QBYB-UNEARNED-AMT-2-1-2 TO BYB-UNEARNED-AMT(2 1 2).
           MOVE QBYB-UNEARNED-AMT-2-1-3 TO BYB-UNEARNED-AMT(2 1 3).
           MOVE QBYB-UNEARNED-AMT-2-1-4 TO BYB-UNEARNED-AMT(2 1 4).
           MOVE QBYB-UNEARNED-AMT-2-1-5 TO BYB-UNEARNED-AMT(2 1 5).
           MOVE QBYB-UNEARNED-AMT-2-1-6 TO BYB-UNEARNED-AMT(2 1 6).
           MOVE QBYB-UNEARNED-AMT-2-1-7 TO BYB-UNEARNED-AMT(2 1 7).
           MOVE QBYB-UNEARNED-AMT-2-1-8 TO BYB-UNEARNED-AMT(2 1 8).
           MOVE QBYB-UNEARNED-AMT-2-1-9 TO BYB-UNEARNED-AMT(2 1 9).
           MOVE QBYB-UNEARNED-AMT-2-1-10 TO BYB-UNEARNED-AMT(2 1 10).
           MOVE QBYB-UNEARNED-AMT-2-2-1 TO BYB-UNEARNED-AMT(2 2 1).
           MOVE QBYB-UNEARNED-AMT-2-2-2 TO BYB-UNEARNED-AMT(2 2 2).
           MOVE QBYB-UNEARNED-AMT-2-2-3 TO BYB-UNEARNED-AMT(2 2 3).
           MOVE QBYB-UNEARNED-AMT-2-2-4 TO BYB-UNEARNED-AMT(2 2 4).
           MOVE QBYB-UNEARNED-AMT-2-2-5 TO BYB-UNEARNED-AMT(2 2 5).
           MOVE QBYB-UNEARNED-AMT-2-2-6 TO BYB-UNEARNED-AMT(2 2 6).
           MOVE QBYB-UNEARNED-AMT-2-2-7 TO BYB-UNEARNED-AMT(2 2 7).
           MOVE QBYB-UNEARNED-AMT-2-2-8 TO BYB-UNEARNED-AMT(2 2 8).
           MOVE QBYB-UNEARNED-AMT-2-2-9 TO BYB-UNEARNED-AMT(2 2 9).
           MOVE QBYB-UNEARNED-AMT-2-2-10 TO BYB-UNEARNED-AMT(2 2 10).
           MOVE QBYB-UNEARNED-AMT-3-1-1 TO BYB-UNEARNED-AMT(3 1 1).
           MOVE QBYB-UNEARNED-AMT-3-1-2 TO BYB-UNEARNED-AMT(3 1 2).
           MOVE QBYB-UNEARNED-AMT-3-1-3 TO BYB-UNEARNED-AMT(3 1 3).
           MOVE QBYB-UNEARNED-AMT-3-1-4 TO BYB-UNEARNED-AMT(3 1 4).
           MOVE QBYB-UNEARNED-AMT-3-1-5 TO BYB-UNEARNED-AMT(3 1 5).
           MOVE QBYB-UNEARNED-AMT-3-1-6 TO BYB-UNEARNED-AMT(3 1 6).
           MOVE QBYB-UNEARNED-AMT-3-1-7 TO BYB-UNEARNED-AMT(3 1 7).
           MOVE QBYB-UNEARNED-AMT-3-1-8 TO BYB-UNEARNED-AMT(3 1 8).
           MOVE QBYB-UNEARNED-AMT-3-1-9 TO BYB-UNEARNED-AMT(3 1 9).
           MOVE QBYB-UNEARNED-AMT-3-1-10 TO BYB-UNEARNED-AMT(3 1 10).
           MOVE QBYB-UNEARNED-AMT-3-2-1 TO BYB-UNEARNED-AMT(3 2 1).
           MOVE QBYB-UNEARNED-AMT-3-2-2 TO BYB-UNEARNED-AMT(3 2 2).
           MOVE QBYB-UNEARNED-AMT-3-2-3 TO BYB-UNEARNED-AMT(3 2 3).
           MOVE QBYB-UNEARNED-AMT-3-2-4 TO BYB-UNEARNED-AMT(3 2 4).
           MOVE QBYB-UNEARNED-AMT-3-2-5 TO BYB-UNEARNED-AMT(3 2 5).
           MOVE QBYB-UNEARNED-AMT-3-2-6 TO BYB-UNEARNED-AMT(3 2 6).
           MOVE QBYB-UNEARNED-AMT-3-2-7 TO BYB-UNEARNED-AMT(3 2 7).
           MOVE QBYB-UNEARNED-AMT-3-2-8 TO BYB-UNEARNED-AMT(3 2 8).
           MOVE QBYB-UNEARNED-AMT-3-2-9 TO BYB-UNEARNED-AMT(3 2 9).
           MOVE QBYB-UNEARNED-AMT-3-2-10 TO BYB-UNEARNED-AMT(3 2 10).
 
      *-----------------------------------------------------------------
      *    LOAD QBYB-REC FROM BYB-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-BYB-FIELDS.
           MOVE BYB-DATE-CCYYMM TO QBYB-DATE-CCYYMM.
           MOVE BYB-BRNO TO QBYB-BRNO.
           MOVE BYB-CLASS TO QBYB-CLASS.
           MOVE BYB-PL-PRINCOL(1 1) TO QBYB-PL-PRINCOL-1-1.
           MOVE BYB-PL-INTCOL(1 1) TO QBYB-PL-INTCOL-1-1.
           MOVE BYB-PL-LATECOL(1 1) TO QBYB-PL-LATECOL-1-1.
           MOVE BYB-PL-O2COL(1 1) TO QBYB-PL-O2COL-1-1.
           MOVE BYB-PL-PRINCOL(1 2) TO QBYB-PL-PRINCOL-1-2.
           MOVE BYB-PL-INTCOL(1 2) TO QBYB-PL-INTCOL-1-2.
           MOVE BYB-PL-LATECOL(1 2) TO QBYB-PL-LATECOL-1-2.
           MOVE BYB-PL-O2COL(1 2) TO QBYB-PL-O2COL-1-2.
           MOVE BYB-PL-PRINCOL(1 3) TO QBYB-PL-PRINCOL-1-3.
           MOVE BYB-PL-INTCOL(1 3) TO QBYB-PL-INTCOL-1-3.
           MOVE BYB-PL-LATECOL(1 3) TO QBYB-PL-LATECOL-1-3.
           MOVE BYB-PL-O2COL(1 3) TO QBYB-PL-O2COL-1-3.
           MOVE BYB-PL-PRINCOL(1 4) TO QBYB-PL-PRINCOL-1-4.
           MOVE BYB-PL-INTCOL(1 4) TO QBYB-PL-INTCOL-1-4.
           MOVE BYB-PL-LATECOL(1 4) TO QBYB-PL-LATECOL-1-4.
           MOVE BYB-PL-O2COL(1 4) TO QBYB-PL-O2COL-1-4.
           MOVE BYB-PL-PRINCOL(1 5) TO QBYB-PL-PRINCOL-1-5.
           MOVE BYB-PL-INTCOL(1 5) TO QBYB-PL-INTCOL-1-5.
           MOVE BYB-PL-LATECOL(1 5) TO QBYB-PL-LATECOL-1-5.
           MOVE BYB-PL-O2COL(1 5) TO QBYB-PL-O2COL-1-5.
           MOVE BYB-AMT-SOLD(1) TO QBYB-AMT-SOLD-1.
           MOVE BYB-AMT-TRANS(1) TO QBYB-AMT-TRANS-1.
           MOVE BYB-AMT-PURCH(1) TO QBYB-AMT-PURCH-1.
           MOVE BYB-NO-SOLD(1) TO QBYB-NO-SOLD-1.
           MOVE BYB-NO-TRANS(1) TO QBYB-NO-TRANS-1.
           MOVE BYB-NO-PURCH(1) TO QBYB-NO-PURCH-1.
           MOVE BYB-OTHCOL(1) TO QBYB-OTHCOL-1.
           MOVE BYB-OT2COL(1) TO QBYB-OT2COL-1.
           MOVE BYB-INTCOL(1) TO QBYB-INTCOL-1.
           MOVE BYB-LATECOL(1) TO QBYB-LATECOL-1.
           MOVE BYB-PRINCOL(1) TO QBYB-PRINCOL-1.
           MOVE BYB-DEFCOL(1) TO QBYB-DEFCOL-1.
           MOVE BYB-RNPRIN(1) TO QBYB-RNPRIN-1.
           MOVE BYB-INTREB(1) TO QBYB-INTREB-1.
           MOVE BYB-CLREB1(1) TO QBYB-CLREB1-1.
           MOVE BYB-CLREB2(1) TO QBYB-CLREB2-1.
           MOVE BYB-CLREB3(1) TO QBYB-CLREB3-1.
           MOVE BYB-CLREB4(1) TO QBYB-CLREB4-1.
           MOVE BYB-AHREB(1) TO QBYB-AHREB-1.
           MOVE BYB-PPREB(1) TO QBYB-PPREB-1.
           MOVE BYB-O1REB(1) TO QBYB-O1REB-1.
           MOVE BYB-O2REB(1) TO QBYB-O2REB-1.
           MOVE BYB-O3REB(1) TO QBYB-O3REB-1.
           MOVE BYB-O4REB(1) TO QBYB-O4REB-1.
           MOVE BYB-O5REB(1) TO QBYB-O5REB-1.
           MOVE BYB-O6REB(1) TO QBYB-O6REB-1.
           MOVE BYB-O7REB(1) TO QBYB-O7REB-1.
           MOVE BYB-SCREB(1) TO QBYB-SCREB-1.
           MOVE BYB-MFREB(1) TO QBYB-MFREB-1.
           MOVE BYB-DFREB(1) TO QBYB-DFREB-1.
           MOVE BYB-UN7-REB(1) TO QBYB-UN7-REB-1.
           MOVE BYB-UN8-REB(1) TO QBYB-UN8-REB-1.
           MOVE BYB-UN9-REB(1) TO QBYB-UN9-REB-1.
           MOVE BYB-UN10-REB(1) TO QBYB-UN10-REB-1.
           MOVE BYB-PART-CHGOFF(1) TO QBYB-PART-CHGOFF-1.
           MOVE BYB-ALLOTMENTS(1) TO QBYB-ALLOTMENTS-1.
           MOVE BYB-INS-NO-ELIG(1 1) TO QBYB-INS-NO-ELIG-1-1.
           MOVE BYB-INS-NO-WROTE(1 1) TO QBYB-INS-NO-WROTE-1-1.
           MOVE BYB-INS-NO-NOTWROTE(1 1) TO QBYB-INS-NO-NOTWROTE-1-1.
           MOVE BYB-INS-AMT-WROTE(1 1) TO QBYB-INS-AMT-WROTE-1-1.
           MOVE BYB-INS-NO-ELIG(1 2) TO QBYB-INS-NO-ELIG-1-2.
           MOVE BYB-INS-NO-WROTE(1 2) TO QBYB-INS-NO-WROTE-1-2.
           MOVE BYB-INS-NO-NOTWROTE(1 2) TO QBYB-INS-NO-NOTWROTE-1-2.
           MOVE BYB-INS-AMT-WROTE(1 2) TO QBYB-INS-AMT-WROTE-1-2.
           MOVE BYB-INS-NO-ELIG(1 3) TO QBYB-INS-NO-ELIG-1-3.
           MOVE BYB-INS-NO-WROTE(1 3) TO QBYB-INS-NO-WROTE-1-3.
           MOVE BYB-INS-NO-NOTWROTE(1 3) TO QBYB-INS-NO-NOTWROTE-1-3.
           MOVE BYB-INS-AMT-WROTE(1 3) TO QBYB-INS-AMT-WROTE-1-3.
           MOVE BYB-INS-NO-ELIG(1 4) TO QBYB-INS-NO-ELIG-1-4.
           MOVE BYB-INS-NO-WROTE(1 4) TO QBYB-INS-NO-WROTE-1-4.
           MOVE BYB-INS-NO-NOTWROTE(1 4) TO QBYB-INS-NO-NOTWROTE-1-4.
           MOVE BYB-INS-AMT-WROTE(1 4) TO QBYB-INS-AMT-WROTE-1-4.
           MOVE BYB-INS-NO-ELIG(1 5) TO QBYB-INS-NO-ELIG-1-5.
           MOVE BYB-INS-NO-WROTE(1 5) TO QBYB-INS-NO-WROTE-1-5.
           MOVE BYB-INS-NO-NOTWROTE(1 5) TO QBYB-INS-NO-NOTWROTE-1-5.
           MOVE BYB-INS-AMT-WROTE(1 5) TO QBYB-INS-AMT-WROTE-1-5.
           MOVE BYB-INS-NO-ELIG(1 6) TO QBYB-INS-NO-ELIG-1-6.
           MOVE BYB-INS-NO-WROTE(1 6) TO QBYB-INS-NO-WROTE-1-6.
           MOVE BYB-INS-NO-NOTWROTE(1 6) TO QBYB-INS-NO-NOTWROTE-1-6.
           MOVE BYB-INS-AMT-WROTE(1 6) TO QBYB-INS-AMT-WROTE-1-6.
           MOVE BYB-INS-NO-ELIG(1 7) TO QBYB-INS-NO-ELIG-1-7.
           MOVE BYB-INS-NO-WROTE(1 7) TO QBYB-INS-NO-WROTE-1-7.
           MOVE BYB-INS-NO-NOTWROTE(1 7) TO QBYB-INS-NO-NOTWROTE-1-7.
           MOVE BYB-INS-AMT-WROTE(1 7) TO QBYB-INS-AMT-WROTE-1-7.
           MOVE BYB-INS-NO-ELIG(1 8) TO QBYB-INS-NO-ELIG-1-8.
           MOVE BYB-INS-NO-WROTE(1 8) TO QBYB-INS-NO-WROTE-1-8.
           MOVE BYB-INS-NO-NOTWROTE(1 8) TO QBYB-INS-NO-NOTWROTE-1-8.
           MOVE BYB-INS-AMT-WROTE(1 8) TO QBYB-INS-AMT-WROTE-1-8.
           MOVE BYB-INS-NO-ELIG(1 9) TO QBYB-INS-NO-ELIG-1-9.
           MOVE BYB-INS-NO-WROTE(1 9) TO QBYB-INS-NO-WROTE-1-9.
           MOVE BYB-INS-NO-NOTWROTE(1 9) TO QBYB-INS-NO-NOTWROTE-1-9.
           MOVE BYB-INS-AMT-WROTE(1 9) TO QBYB-INS-AMT-WROTE-1-9.
           MOVE BYB-INS-NO-ELIG(1 10) TO QBYB-INS-NO-ELIG-1-10.
           MOVE BYB-INS-NO-WROTE(1 10) TO QBYB-INS-NO-WROTE-1-10.
           MOVE BYB-INS-NO-NOTWROTE(1 10) TO QBYB-INS-NO-NOTWROTE-1-10.
           MOVE BYB-INS-AMT-WROTE(1 10) TO QBYB-INS-AMT-WROTE-1-10.
           MOVE BYB-FEE-NO-ELIG(1 1) TO QBYB-FEE-NO-ELIG-1-1.
           MOVE BYB-FEE-NO-WROTE(1 1) TO QBYB-FEE-NO-WROTE-1-1.
           MOVE BYB-FEE-NO-NOTWROTE(1 1) TO QBYB-FEE-NO-NOTWROTE-1-1.
           MOVE BYB-FEE-AMT-WROTE(1 1) TO QBYB-FEE-AMT-WROTE-1-1.
           MOVE BYB-FEE-NO-ELIG(1 2) TO QBYB-FEE-NO-ELIG-1-2.
           MOVE BYB-FEE-NO-WROTE(1 2) TO QBYB-FEE-NO-WROTE-1-2.
           MOVE BYB-FEE-NO-NOTWROTE(1 2) TO QBYB-FEE-NO-NOTWROTE-1-2.
           MOVE BYB-FEE-AMT-WROTE(1 2) TO QBYB-FEE-AMT-WROTE-1-2.
           MOVE BYB-FEE-NO-ELIG(1 3) TO QBYB-FEE-NO-ELIG-1-3.
           MOVE BYB-FEE-NO-WROTE(1 3) TO QBYB-FEE-NO-WROTE-1-3.
           MOVE BYB-FEE-NO-NOTWROTE(1 3) TO QBYB-FEE-NO-NOTWROTE-1-3.
           MOVE BYB-FEE-AMT-WROTE(1 3) TO QBYB-FEE-AMT-WROTE-1-3.
           MOVE BYB-OTH-NO-COL(1) TO QBYB-OTH-NO-COL-1.
           MOVE BYB-OT2-NO-COL(1) TO QBYB-OT2-NO-COL-1.
           MOVE BYB-INT-NO-COL(1) TO QBYB-INT-NO-COL-1.
           MOVE BYB-LATE-NO-COL(1) TO QBYB-LATE-NO-COL-1.
           MOVE BYB-OTH-NO-WAIVED(1) TO QBYB-OTH-NO-WAIVED-1.
           MOVE BYB-OT2-NO-WAIVED(1) TO QBYB-OT2-NO-WAIVED-1.
           MOVE BYB-INT-NO-WAIVED(1) TO QBYB-INT-NO-WAIVED-1.
           MOVE BYB-LATE-NO-WAIVED(1) TO QBYB-LATE-NO-WAIVED-1.
           MOVE BYB-OTH-AMT-WAIVED(1) TO QBYB-OTH-AMT-WAIVED-1.
           MOVE BYB-OT2-AMT-WAIVED(1) TO QBYB-OT2-AMT-WAIVED-1.
           MOVE BYB-INT-AMT-WAIVED(1) TO QBYB-INT-AMT-WAIVED-1.
           MOVE BYB-LATE-AMT-WAIVED(1) TO QBYB-LATE-AMT-WAIVED-1.
           MOVE BYB-PL-PRINCOL(2 1) TO QBYB-PL-PRINCOL-2-1.
           MOVE BYB-PL-INTCOL(2 1) TO QBYB-PL-INTCOL-2-1.
           MOVE BYB-PL-LATECOL(2 1) TO QBYB-PL-LATECOL-2-1.
           MOVE BYB-PL-O2COL(2 1) TO QBYB-PL-O2COL-2-1.
           MOVE BYB-PL-PRINCOL(2 2) TO QBYB-PL-PRINCOL-2-2.
           MOVE BYB-PL-INTCOL(2 2) TO QBYB-PL-INTCOL-2-2.
           MOVE BYB-PL-LATECOL(2 2) TO QBYB-PL-LATECOL-2-2.
           MOVE BYB-PL-O2COL(2 2) TO QBYB-PL-O2COL-2-2.
           MOVE BYB-PL-PRINCOL(2 3) TO QBYB-PL-PRINCOL-2-3.
           MOVE BYB-PL-INTCOL(2 3) TO QBYB-PL-INTCOL-2-3.
           MOVE BYB-PL-LATECOL(2 3) TO QBYB-PL-LATECOL-2-3.
           MOVE BYB-PL-O2COL(2 3) TO QBYB-PL-O2COL-2-3.
           MOVE BYB-PL-PRINCOL(2 4) TO QBYB-PL-PRINCOL-2-4.
           MOVE BYB-PL-INTCOL(2 4) TO QBYB-PL-INTCOL-2-4.
           MOVE BYB-PL-LATECOL(2 4) TO QBYB-PL-LATECOL-2-4.
           MOVE BYB-PL-O2COL(2 4) TO QBYB-PL-O2COL-2-4.
           MOVE BYB-PL-PRINCOL(2 5) TO QBYB-PL-PRINCOL-2-5.
           MOVE BYB-PL-INTCOL(2 5) TO QBYB-PL-INTCOL-2-5.
           MOVE BYB-PL-LATECOL(2 5) TO QBYB-PL-LATECOL-2-5.
           MOVE BYB-PL-O2COL(2 5) TO QBYB-PL-O2COL-2-5.
           MOVE BYB-AMT-SOLD(2) TO QBYB-AMT-SOLD-2.
           MOVE BYB-AMT-TRANS(2) TO QBYB-AMT-TRANS-2.
           MOVE BYB-AMT-PURCH(2) TO QBYB-AMT-PURCH-2.
           MOVE BYB-NO-SOLD(2) TO QBYB-NO-SOLD-2.
           MOVE BYB-NO-TRANS(2) TO QBYB-NO-TRANS-2.
           MOVE BYB-NO-PURCH(2) TO QBYB-NO-PURCH-2.
           MOVE BYB-OTHCOL(2) TO QBYB-OTHCOL-2.
           MOVE BYB-OT2COL(2) TO QBYB-OT2COL-2.
           MOVE BYB-INTCOL(2) TO QBYB-INTCOL-2.
           MOVE BYB-LATECOL(2) TO QBYB-LATECOL-2.
           MOVE BYB-PRINCOL(2) TO QBYB-PRINCOL-2.
           MOVE BYB-DEFCOL(2) TO QBYB-DEFCOL-2.
           MOVE BYB-RNPRIN(2) TO QBYB-RNPRIN-2.
           MOVE BYB-INTREB(2) TO QBYB-INTREB-2.
           MOVE BYB-CLREB1(2) TO QBYB-CLREB1-2.
           MOVE BYB-CLREB2(2) TO QBYB-CLREB2-2.
           MOVE BYB-CLREB3(2) TO QBYB-CLREB3-2.
           MOVE BYB-CLREB4(2) TO QBYB-CLREB4-2.
           MOVE BYB-AHREB(2) TO QBYB-AHREB-2.
           MOVE BYB-PPREB(2) TO QBYB-PPREB-2.
           MOVE BYB-O1REB(2) TO QBYB-O1REB-2.
           MOVE BYB-O2REB(2) TO QBYB-O2REB-2.
           MOVE BYB-O3REB(2) TO QBYB-O3REB-2.
           MOVE BYB-O4REB(2) TO QBYB-O4REB-2.
           MOVE BYB-O5REB(2) TO QBYB-O5REB-2.
           MOVE BYB-O6REB(2) TO QBYB-O6REB-2.
           MOVE BYB-O7REB(2) TO QBYB-O7REB-2.
           MOVE BYB-SCREB(2) TO QBYB-SCREB-2.
           MOVE BYB-MFREB(2) TO QBYB-MFREB-2.
           MOVE BYB-DFREB(2) TO QBYB-DFREB-2.
           MOVE BYB-UN7-REB(2) TO QBYB-UN7-REB-2.
           MOVE BYB-UN8-REB(2) TO QBYB-UN8-REB-2.
           MOVE BYB-UN9-REB(2) TO QBYB-UN9-REB-2.
           MOVE BYB-UN10-REB(2) TO QBYB-UN10-REB-2.
           MOVE BYB-PART-CHGOFF(2) TO QBYB-PART-CHGOFF-2.
           MOVE BYB-ALLOTMENTS(2) TO QBYB-ALLOTMENTS-2.
           MOVE BYB-INS-NO-ELIG(2 1) TO QBYB-INS-NO-ELIG-2-1.
           MOVE BYB-INS-NO-WROTE(2 1) TO QBYB-INS-NO-WROTE-2-1.
           MOVE BYB-INS-NO-NOTWROTE(2 1) TO QBYB-INS-NO-NOTWROTE-2-1.
           MOVE BYB-INS-AMT-WROTE(2 1) TO QBYB-INS-AMT-WROTE-2-1.
           MOVE BYB-INS-NO-ELIG(2 2) TO QBYB-INS-NO-ELIG-2-2.
           MOVE BYB-INS-NO-WROTE(2 2) TO QBYB-INS-NO-WROTE-2-2.
           MOVE BYB-INS-NO-NOTWROTE(2 2) TO QBYB-INS-NO-NOTWROTE-2-2.
           MOVE BYB-INS-AMT-WROTE(2 2) TO QBYB-INS-AMT-WROTE-2-2.
           MOVE BYB-INS-NO-ELIG(2 3) TO QBYB-INS-NO-ELIG-2-3.
           MOVE BYB-INS-NO-WROTE(2 3) TO QBYB-INS-NO-WROTE-2-3.
           MOVE BYB-INS-NO-NOTWROTE(2 3) TO QBYB-INS-NO-NOTWROTE-2-3.
           MOVE BYB-INS-AMT-WROTE(2 3) TO QBYB-INS-AMT-WROTE-2-3.
           MOVE BYB-INS-NO-ELIG(2 4) TO QBYB-INS-NO-ELIG-2-4.
           MOVE BYB-INS-NO-WROTE(2 4) TO QBYB-INS-NO-WROTE-2-4.
           MOVE BYB-INS-NO-NOTWROTE(2 4) TO QBYB-INS-NO-NOTWROTE-2-4.
           MOVE BYB-INS-AMT-WROTE(2 4) TO QBYB-INS-AMT-WROTE-2-4.
           MOVE BYB-INS-NO-ELIG(2 5) TO QBYB-INS-NO-ELIG-2-5.
           MOVE BYB-INS-NO-WROTE(2 5) TO QBYB-INS-NO-WROTE-2-5.
           MOVE BYB-INS-NO-NOTWROTE(2 5) TO QBYB-INS-NO-NOTWROTE-2-5.
           MOVE BYB-INS-AMT-WROTE(2 5) TO QBYB-INS-AMT-WROTE-2-5.
           MOVE BYB-INS-NO-ELIG(2 6) TO QBYB-INS-NO-ELIG-2-6.
           MOVE BYB-INS-NO-WROTE(2 6) TO QBYB-INS-NO-WROTE-2-6.
           MOVE BYB-INS-NO-NOTWROTE(2 6) TO QBYB-INS-NO-NOTWROTE-2-6.
           MOVE BYB-INS-AMT-WROTE(2 6) TO QBYB-INS-AMT-WROTE-2-6.
           MOVE BYB-INS-NO-ELIG(2 7) TO QBYB-INS-NO-ELIG-2-7.
           MOVE BYB-INS-NO-WROTE(2 7) TO QBYB-INS-NO-WROTE-2-7.
           MOVE BYB-INS-NO-NOTWROTE(2 7) TO QBYB-INS-NO-NOTWROTE-2-7.
           MOVE BYB-INS-AMT-WROTE(2 7) TO QBYB-INS-AMT-WROTE-2-7.
           MOVE BYB-INS-NO-ELIG(2 8) TO QBYB-INS-NO-ELIG-2-8.
           MOVE BYB-INS-NO-WROTE(2 8) TO QBYB-INS-NO-WROTE-2-8.
           MOVE BYB-INS-NO-NOTWROTE(2 8) TO QBYB-INS-NO-NOTWROTE-2-8.
           MOVE BYB-INS-AMT-WROTE(2 8) TO QBYB-INS-AMT-WROTE-2-8.
           MOVE BYB-INS-NO-ELIG(2 9) TO QBYB-INS-NO-ELIG-2-9.
           MOVE BYB-INS-NO-WROTE(2 9) TO QBYB-INS-NO-WROTE-2-9.
           MOVE BYB-INS-NO-NOTWROTE(2 9) TO QBYB-INS-NO-NOTWROTE-2-9.
           MOVE BYB-INS-AMT-WROTE(2 9) TO QBYB-INS-AMT-WROTE-2-9.
           MOVE BYB-INS-NO-ELIG(2 10) TO QBYB-INS-NO-ELIG-2-10.
           MOVE BYB-INS-NO-WROTE(2 10) TO QBYB-INS-NO-WROTE-2-10.
           MOVE BYB-INS-NO-NOTWROTE(2 10) TO QBYB-INS-NO-NOTWROTE-2-10.
           MOVE BYB-INS-AMT-WROTE(2 10) TO QBYB-INS-AMT-WROTE-2-10.
           MOVE BYB-FEE-NO-ELIG(2 1) TO QBYB-FEE-NO-ELIG-2-1.
           MOVE BYB-FEE-NO-WROTE(2 1) TO QBYB-FEE-NO-WROTE-2-1.
           MOVE BYB-FEE-NO-NOTWROTE(2 1) TO QBYB-FEE-NO-NOTWROTE-2-1.
           MOVE BYB-FEE-AMT-WROTE(2 1) TO QBYB-FEE-AMT-WROTE-2-1.
           MOVE BYB-FEE-NO-ELIG(2 2) TO QBYB-FEE-NO-ELIG-2-2.
           MOVE BYB-FEE-NO-WROTE(2 2) TO QBYB-FEE-NO-WROTE-2-2.
           MOVE BYB-FEE-NO-NOTWROTE(2 2) TO QBYB-FEE-NO-NOTWROTE-2-2.
           MOVE BYB-FEE-AMT-WROTE(2 2) TO QBYB-FEE-AMT-WROTE-2-2.
           MOVE BYB-FEE-NO-ELIG(2 3) TO QBYB-FEE-NO-ELIG-2-3.
           MOVE BYB-FEE-NO-WROTE(2 3) TO QBYB-FEE-NO-WROTE-2-3.
           MOVE BYB-FEE-NO-NOTWROTE(2 3) TO QBYB-FEE-NO-NOTWROTE-2-3.
           MOVE BYB-FEE-AMT-WROTE(2 3) TO QBYB-FEE-AMT-WROTE-2-3.
           MOVE BYB-OTH-NO-COL(2) TO QBYB-OTH-NO-COL-2.
           MOVE BYB-OT2-NO-COL(2) TO QBYB-OT2-NO-COL-2.
           MOVE BYB-INT-NO-COL(2) TO QBYB-INT-NO-COL-2.
           MOVE BYB-LATE-NO-COL(2) TO QBYB-LATE-NO-COL-2.
           MOVE BYB-OTH-NO-WAIVED(2) TO QBYB-OTH-NO-WAIVED-2.
           MOVE BYB-OT2-NO-WAIVED(2) TO QBYB-OT2-NO-WAIVED-2.
           MOVE BYB-INT-NO-WAIVED(2) TO QBYB-INT-NO-WAIVED-2.
           MOVE BYB-LATE-NO-WAIVED(2) TO QBYB-LATE-NO-WAIVED-2.
           MOVE BYB-OTH-AMT-WAIVED(2) TO QBYB-OTH-AMT-WAIVED-2.
           MOVE BYB-OT2-AMT-WAIVED(2) TO QBYB-OT2-AMT-WAIVED-2.
           MOVE BYB-INT-AMT-WAIVED(2) TO QBYB-INT-AMT-WAIVED-2.
           MOVE BYB-LATE-AMT-WAIVED(2) TO QBYB-LATE-AMT-WAIVED-2.
           MOVE BYB-UNITS(1 1 1) TO QBYB-UNITS-1-1-1.
           MOVE BYB-OUTBAL(1 1 1) TO QBYB-OUTBAL-1-1-1.
           MOVE BYB-UNITS(1 1 2) TO QBYB-UNITS-1-1-2.
           MOVE BYB-OUTBAL(1 1 2) TO QBYB-OUTBAL-1-1-2.
           MOVE BYB-UNITS(1 1 3) TO QBYB-UNITS-1-1-3.
           MOVE BYB-OUTBAL(1 1 3) TO QBYB-OUTBAL-1-1-3.
           MOVE BYB-UNITS(1 1 4) TO QBYB-UNITS-1-1-4.
           MOVE BYB-OUTBAL(1 1 4) TO QBYB-OUTBAL-1-1-4.
           MOVE BYB-UNITS(1 1 5) TO QBYB-UNITS-1-1-5.
           MOVE BYB-OUTBAL(1 1 5) TO QBYB-OUTBAL-1-1-5.
           MOVE BYB-UNITS(1 1 6) TO QBYB-UNITS-1-1-6.
           MOVE BYB-OUTBAL(1 1 6) TO QBYB-OUTBAL-1-1-6.
           MOVE BYB-UNITS(1 1 7) TO QBYB-UNITS-1-1-7.
           MOVE BYB-OUTBAL(1 1 7) TO QBYB-OUTBAL-1-1-7.
           MOVE BYB-UNITS(1 1 8) TO QBYB-UNITS-1-1-8.
           MOVE BYB-OUTBAL(1 1 8) TO QBYB-OUTBAL-1-1-8.
           MOVE BYB-UNITS(1 1 9) TO QBYB-UNITS-1-1-9.
           MOVE BYB-OUTBAL(1 1 9) TO QBYB-OUTBAL-1-1-9.
           MOVE BYB-UNITS(1 1 10) TO QBYB-UNITS-1-1-10.
           MOVE BYB-OUTBAL(1 1 10) TO QBYB-OUTBAL-1-1-10.
           MOVE BYB-UNITS(1 2 1) TO QBYB-UNITS-1-2-1.
           MOVE BYB-OUTBAL(1 2 1) TO QBYB-OUTBAL-1-2-1.
           MOVE BYB-UNITS(1 2 2) TO QBYB-UNITS-1-2-2.
           MOVE BYB-OUTBAL(1 2 2) TO QBYB-OUTBAL-1-2-2.
           MOVE BYB-UNITS(1 2 3) TO QBYB-UNITS-1-2-3.
           MOVE BYB-OUTBAL(1 2 3) TO QBYB-OUTBAL-1-2-3.
           MOVE BYB-UNITS(1 2 4) TO QBYB-UNITS-1-2-4.
           MOVE BYB-OUTBAL(1 2 4) TO QBYB-OUTBAL-1-2-4.
           MOVE BYB-UNITS(1 2 5) TO QBYB-UNITS-1-2-5.
           MOVE BYB-OUTBAL(1 2 5) TO QBYB-OUTBAL-1-2-5.
           MOVE BYB-UNITS(1 2 6) TO QBYB-UNITS-1-2-6.
           MOVE BYB-OUTBAL(1 2 6) TO QBYB-OUTBAL-1-2-6.
           MOVE BYB-UNITS(1 2 7) TO QBYB-UNITS-1-2-7.
           MOVE BYB-OUTBAL(1 2 7) TO QBYB-OUTBAL-1-2-7.
           MOVE BYB-UNITS(1 2 8) TO QBYB-UNITS-1-2-8.
           MOVE BYB-OUTBAL(1 2 8) TO QBYB-OUTBAL-1-2-8.
           MOVE BYB-UNITS(1 2 9) TO QBYB-UNITS-1-2-9.
           MOVE BYB-OUTBAL(1 2 9) TO QBYB-OUTBAL-1-2-9.
           MOVE BYB-UNITS(1 2 10) TO QBYB-UNITS-1-2-10.
           MOVE BYB-OUTBAL(1 2 10) TO QBYB-OUTBAL-1-2-10.
           MOVE BYB-UNITS(2 1 1) TO QBYB-UNITS-2-1-1.
           MOVE BYB-OUTBAL(2 1 1) TO QBYB-OUTBAL-2-1-1.
           MOVE BYB-UNITS(2 1 2) TO QBYB-UNITS-2-1-2.
           MOVE BYB-OUTBAL(2 1 2) TO QBYB-OUTBAL-2-1-2.
           MOVE BYB-UNITS(2 1 3) TO QBYB-UNITS-2-1-3.
           MOVE BYB-OUTBAL(2 1 3) TO QBYB-OUTBAL-2-1-3.
           MOVE BYB-UNITS(2 1 4) TO QBYB-UNITS-2-1-4.
           MOVE BYB-OUTBAL(2 1 4) TO QBYB-OUTBAL-2-1-4.
           MOVE BYB-UNITS(2 1 5) TO QBYB-UNITS-2-1-5.
           MOVE BYB-OUTBAL(2 1 5) TO QBYB-OUTBAL-2-1-5.
           MOVE BYB-UNITS(2 1 6) TO QBYB-UNITS-2-1-6.
           MOVE BYB-OUTBAL(2 1 6) TO QBYB-OUTBAL-2-1-6.
           MOVE BYB-UNITS(2 1 7) TO QBYB-UNITS-2-1-7.
           MOVE BYB-OUTBAL(2 1 7) TO QBYB-OUTBAL-2-1-7.
           MOVE BYB-UNITS(2 1 8) TO QBYB-UNITS-2-1-8.
           MOVE BYB-OUTBAL(2 1 8) TO QBYB-OUTBAL-2-1-8.
           MOVE BYB-UNITS(2 1 9) TO QBYB-UNITS-2-1-9.
           MOVE BYB-OUTBAL(2 1 9) TO QBYB-OUTBAL-2-1-9.
           MOVE BYB-UNITS(2 1 10) TO QBYB-UNITS-2-1-10.
           MOVE BYB-OUTBAL(2 1 10) TO QBYB-OUTBAL-2-1-10.
           MOVE BYB-UNITS(2 2 1) TO QBYB-UNITS-2-2-1.
           MOVE BYB-OUTBAL(2 2 1) TO QBYB-OUTBAL-2-2-1.
           MOVE BYB-UNITS(2 2 2) TO QBYB-UNITS-2-2-2.
           MOVE BYB-OUTBAL(2 2 2) TO QBYB-OUTBAL-2-2-2.
           MOVE BYB-UNITS(2 2 3) TO QBYB-UNITS-2-2-3.
           MOVE BYB-OUTBAL(2 2 3) TO QBYB-OUTBAL-2-2-3.
           MOVE BYB-UNITS(2 2 4) TO QBYB-UNITS-2-2-4.
           MOVE BYB-OUTBAL(2 2 4) TO QBYB-OUTBAL-2-2-4.
           MOVE BYB-UNITS(2 2 5) TO QBYB-UNITS-2-2-5.
           MOVE BYB-OUTBAL(2 2 5) TO QBYB-OUTBAL-2-2-5.
           MOVE BYB-UNITS(2 2 6) TO QBYB-UNITS-2-2-6.
           MOVE BYB-OUTBAL(2 2 6) TO QBYB-OUTBAL-2-2-6.
           MOVE BYB-UNITS(2 2 7) TO QBYB-UNITS-2-2-7.
           MOVE BYB-OUTBAL(2 2 7) TO QBYB-OUTBAL-2-2-7.
           MOVE BYB-UNITS(2 2 8) TO QBYB-UNITS-2-2-8.
           MOVE BYB-OUTBAL(2 2 8) TO QBYB-OUTBAL-2-2-8.
           MOVE BYB-UNITS(2 2 9) TO QBYB-UNITS-2-2-9.
           MOVE BYB-OUTBAL(2 2 9) TO QBYB-OUTBAL-2-2-9.
           MOVE BYB-UNITS(2 2 10) TO QBYB-UNITS-2-2-10.
           MOVE BYB-OUTBAL(2 2 10) TO QBYB-OUTBAL-2-2-10.
           MOVE BYB-UNITS(3 1 1) TO QBYB-UNITS-3-1-1.
           MOVE BYB-OUTBAL(3 1 1) TO QBYB-OUTBAL-3-1-1.
           MOVE BYB-UNITS(3 1 2) TO QBYB-UNITS-3-1-2.
           MOVE BYB-OUTBAL(3 1 2) TO QBYB-OUTBAL-3-1-2.
           MOVE BYB-UNITS(3 1 3) TO QBYB-UNITS-3-1-3.
           MOVE BYB-OUTBAL(3 1 3) TO QBYB-OUTBAL-3-1-3.
           MOVE BYB-UNITS(3 1 4) TO QBYB-UNITS-3-1-4.
           MOVE BYB-OUTBAL(3 1 4) TO QBYB-OUTBAL-3-1-4.
           MOVE BYB-UNITS(3 1 5) TO QBYB-UNITS-3-1-5.
           MOVE BYB-OUTBAL(3 1 5) TO QBYB-OUTBAL-3-1-5.
           MOVE BYB-UNITS(3 1 6) TO QBYB-UNITS-3-1-6.
           MOVE BYB-OUTBAL(3 1 6) TO QBYB-OUTBAL-3-1-6.
           MOVE BYB-UNITS(3 1 7) TO QBYB-UNITS-3-1-7.
           MOVE BYB-OUTBAL(3 1 7) TO QBYB-OUTBAL-3-1-7.
           MOVE BYB-UNITS(3 1 8) TO QBYB-UNITS-3-1-8.
           MOVE BYB-OUTBAL(3 1 8) TO QBYB-OUTBAL-3-1-8.
           MOVE BYB-UNITS(3 1 9) TO QBYB-UNITS-3-1-9.
           MOVE BYB-OUTBAL(3 1 9) TO QBYB-OUTBAL-3-1-9.
           MOVE BYB-UNITS(3 1 10) TO QBYB-UNITS-3-1-10.
           MOVE BYB-OUTBAL(3 1 10) TO QBYB-OUTBAL-3-1-10.
           MOVE BYB-UNITS(3 2 1) TO QBYB-UNITS-3-2-1.
           MOVE BYB-OUTBAL(3 2 1) TO QBYB-OUTBAL-3-2-1.
           MOVE BYB-UNITS(3 2 2) TO QBYB-UNITS-3-2-2.
           MOVE BYB-OUTBAL(3 2 2) TO QBYB-OUTBAL-3-2-2.
           MOVE BYB-UNITS(3 2 3) TO QBYB-UNITS-3-2-3.
           MOVE BYB-OUTBAL(3 2 3) TO QBYB-OUTBAL-3-2-3.
           MOVE BYB-UNITS(3 2 4) TO QBYB-UNITS-3-2-4.
           MOVE BYB-OUTBAL(3 2 4) TO QBYB-OUTBAL-3-2-4.
           MOVE BYB-UNITS(3 2 5) TO QBYB-UNITS-3-2-5.
           MOVE BYB-OUTBAL(3 2 5) TO QBYB-OUTBAL-3-2-5.
           MOVE BYB-UNITS(3 2 6) TO QBYB-UNITS-3-2-6.
           MOVE BYB-OUTBAL(3 2 6) TO QBYB-OUTBAL-3-2-6.
           MOVE BYB-UNITS(3 2 7) TO QBYB-UNITS-3-2-7.
           MOVE BYB-OUTBAL(3 2 7) TO QBYB-OUTBAL-3-2-7.
           MOVE BYB-UNITS(3 2 8) TO QBYB-UNITS-3-2-8.
           MOVE BYB-OUTBAL(3 2 8) TO QBYB-OUTBAL-3-2-8.
           MOVE BYB-UNITS(3 2 9) TO QBYB-UNITS-3-2-9.
           MOVE BYB-OUTBAL(3 2 9) TO QBYB-OUTBAL-3-2-9.
           MOVE BYB-UNITS(3 2 10) TO QBYB-UNITS-3-2-10.
           MOVE BYB-OUTBAL(3 2 10) TO QBYB-OUTBAL-3-2-10.
           MOVE BYB-UNITS-BNKRPT(1 1 1) TO QBYB-UNITS-BNKRPT-1-1-1.
           MOVE BYB-OUTBAL-BNKRPT(1 1 1) TO QBYB-OUTBAL-BNKRPT-1-1-1.
           MOVE BYB-UNITS-BNKRPT(1 1 2) TO QBYB-UNITS-BNKRPT-1-1-2.
           MOVE BYB-OUTBAL-BNKRPT(1 1 2) TO QBYB-OUTBAL-BNKRPT-1-1-2.
           MOVE BYB-UNITS-BNKRPT(1 1 3) TO QBYB-UNITS-BNKRPT-1-1-3.
           MOVE BYB-OUTBAL-BNKRPT(1 1 3) TO QBYB-OUTBAL-BNKRPT-1-1-3.
           MOVE BYB-UNITS-BNKRPT(1 1 4) TO QBYB-UNITS-BNKRPT-1-1-4.
           MOVE BYB-OUTBAL-BNKRPT(1 1 4) TO QBYB-OUTBAL-BNKRPT-1-1-4.
           MOVE BYB-UNITS-BNKRPT(1 1 5) TO QBYB-UNITS-BNKRPT-1-1-5.
           MOVE BYB-OUTBAL-BNKRPT(1 1 5) TO QBYB-OUTBAL-BNKRPT-1-1-5.
           MOVE BYB-UNITS-BNKRPT(1 1 6) TO QBYB-UNITS-BNKRPT-1-1-6.
           MOVE BYB-OUTBAL-BNKRPT(1 1 6) TO QBYB-OUTBAL-BNKRPT-1-1-6.
           MOVE BYB-UNITS-BNKRPT(1 1 7) TO QBYB-UNITS-BNKRPT-1-1-7.
           MOVE BYB-OUTBAL-BNKRPT(1 1 7) TO QBYB-OUTBAL-BNKRPT-1-1-7.
           MOVE BYB-UNITS-BNKRPT(1 1 8) TO QBYB-UNITS-BNKRPT-1-1-8.
           MOVE BYB-OUTBAL-BNKRPT(1 1 8) TO QBYB-OUTBAL-BNKRPT-1-1-8.
           MOVE BYB-UNITS-BNKRPT(1 1 9) TO QBYB-UNITS-BNKRPT-1-1-9.
           MOVE BYB-OUTBAL-BNKRPT(1 1 9) TO QBYB-OUTBAL-BNKRPT-1-1-9.
           MOVE BYB-UNITS-BNKRPT(1 1 10) TO QBYB-UNITS-BNKRPT-1-1-10.
           MOVE BYB-OUTBAL-BNKRPT(1 1 10) TO QBYB-OUTBAL-BNKRPT-1-1-10.
           MOVE BYB-UNITS-BNKRPT(1 2 1) TO QBYB-UNITS-BNKRPT-1-2-1.
           MOVE BYB-OUTBAL-BNKRPT(1 2 1) TO QBYB-OUTBAL-BNKRPT-1-2-1.
           MOVE BYB-UNITS-BNKRPT(1 2 2) TO QBYB-UNITS-BNKRPT-1-2-2.
           MOVE BYB-OUTBAL-BNKRPT(1 2 2) TO QBYB-OUTBAL-BNKRPT-1-2-2.
           MOVE BYB-UNITS-BNKRPT(1 2 3) TO QBYB-UNITS-BNKRPT-1-2-3.
           MOVE BYB-OUTBAL-BNKRPT(1 2 3) TO QBYB-OUTBAL-BNKRPT-1-2-3.
           MOVE BYB-UNITS-BNKRPT(1 2 4) TO QBYB-UNITS-BNKRPT-1-2-4.
           MOVE BYB-OUTBAL-BNKRPT(1 2 4) TO QBYB-OUTBAL-BNKRPT-1-2-4.
           MOVE BYB-UNITS-BNKRPT(1 2 5) TO QBYB-UNITS-BNKRPT-1-2-5.
           MOVE BYB-OUTBAL-BNKRPT(1 2 5) TO QBYB-OUTBAL-BNKRPT-1-2-5.
           MOVE BYB-UNITS-BNKRPT(1 2 6) TO QBYB-UNITS-BNKRPT-1-2-6.
           MOVE BYB-OUTBAL-BNKRPT(1 2 6) TO QBYB-OUTBAL-BNKRPT-1-2-6.
           MOVE BYB-UNITS-BNKRPT(1 2 7) TO QBYB-UNITS-BNKRPT-1-2-7.
           MOVE BYB-OUTBAL-BNKRPT(1 2 7) TO QBYB-OUTBAL-BNKRPT-1-2-7.
           MOVE BYB-UNITS-BNKRPT(1 2 8) TO QBYB-UNITS-BNKRPT-1-2-8.
           MOVE BYB-OUTBAL-BNKRPT(1 2 8) TO QBYB-OUTBAL-BNKRPT-1-2-8.
           MOVE BYB-UNITS-BNKRPT(1 2 9) TO QBYB-UNITS-BNKRPT-1-2-9.
           MOVE BYB-OUTBAL-BNKRPT(1 2 9) TO QBYB-OUTBAL-BNKRPT-1-2-9.
           MOVE BYB-UNITS-BNKRPT(1 2 10) TO QBYB-UNITS-BNKRPT-1-2-10.
           MOVE BYB-OUTBAL-BNKRPT(1 2 10) TO QBYB-OUTBAL-BNKRPT-1-2-10.
           MOVE BYB-UNITS-BNKRPT(2 1 1) TO QBYB-UNITS-BNKRPT-2-1-1.
           MOVE BYB-OUTBAL-BNKRPT(2 1 1) TO QBYB-OUTBAL-BNKRPT-2-1-1.
           MOVE BYB-UNITS-BNKRPT(2 1 2) TO QBYB-UNITS-BNKRPT-2-1-2.
           MOVE BYB-OUTBAL-BNKRPT(2 1 2) TO QBYB-OUTBAL-BNKRPT-2-1-2.
           MOVE BYB-UNITS-BNKRPT(2 1 3) TO QBYB-UNITS-BNKRPT-2-1-3.
           MOVE BYB-OUTBAL-BNKRPT(2 1 3) TO QBYB-OUTBAL-BNKRPT-2-1-3.
           MOVE BYB-UNITS-BNKRPT(2 1 4) TO QBYB-UNITS-BNKRPT-2-1-4.
           MOVE BYB-OUTBAL-BNKRPT(2 1 4) TO QBYB-OUTBAL-BNKRPT-2-1-4.
           MOVE BYB-UNITS-BNKRPT(2 1 5) TO QBYB-UNITS-BNKRPT-2-1-5.
           MOVE BYB-OUTBAL-BNKRPT(2 1 5) TO QBYB-OUTBAL-BNKRPT-2-1-5.
           MOVE BYB-UNITS-BNKRPT(2 1 6) TO QBYB-UNITS-BNKRPT-2-1-6.
           MOVE BYB-OUTBAL-BNKRPT(2 1 6) TO QBYB-OUTBAL-BNKRPT-2-1-6.
           MOVE BYB-UNITS-BNKRPT(2 1 7) TO QBYB-UNITS-BNKRPT-2-1-7.
           MOVE BYB-OUTBAL-BNKRPT(2 1 7) TO QBYB-OUTBAL-BNKRPT-2-1-7.
           MOVE BYB-UNITS-BNKRPT(2 1 8) TO QBYB-UNITS-BNKRPT-2-1-8.
           MOVE BYB-OUTBAL-BNKRPT(2 1 8) TO QBYB-OUTBAL-BNKRPT-2-1-8.
           MOVE BYB-UNITS-BNKRPT(2 1 9) TO QBYB-UNITS-BNKRPT-2-1-9.
           MOVE BYB-OUTBAL-BNKRPT(2 1 9) TO QBYB-OUTBAL-BNKRPT-2-1-9.
           MOVE BYB-UNITS-BNKRPT(2 1 10) TO QBYB-UNITS-BNKRPT-2-1-10.
           MOVE BYB-OUTBAL-BNKRPT(2 1 10) TO QBYB-OUTBAL-BNKRPT-2-1-10.
           MOVE BYB-UNITS-BNKRPT(2 2 1) TO QBYB-UNITS-BNKRPT-2-2-1.
           MOVE BYB-OUTBAL-BNKRPT(2 2 1) TO QBYB-OUTBAL-BNKRPT-2-2-1.
           MOVE BYB-UNITS-BNKRPT(2 2 2) TO QBYB-UNITS-BNKRPT-2-2-2.
           MOVE BYB-OUTBAL-BNKRPT(2 2 2) TO QBYB-OUTBAL-BNKRPT-2-2-2.
           MOVE BYB-UNITS-BNKRPT(2 2 3) TO QBYB-UNITS-BNKRPT-2-2-3.
           MOVE BYB-OUTBAL-BNKRPT(2 2 3) TO QBYB-OUTBAL-BNKRPT-2-2-3.
           MOVE BYB-UNITS-BNKRPT(2 2 4) TO QBYB-UNITS-BNKRPT-2-2-4.
           MOVE BYB-OUTBAL-BNKRPT(2 2 4) TO QBYB-OUTBAL-BNKRPT-2-2-4.
           MOVE BYB-UNITS-BNKRPT(2 2 5) TO QBYB-UNITS-BNKRPT-2-2-5.
           MOVE BYB-OUTBAL-BNKRPT(2 2 5) TO QBYB-OUTBAL-BNKRPT-2-2-5.
           MOVE BYB-UNITS-BNKRPT(2 2 6) TO QBYB-UNITS-BNKRPT-2-2-6.
           MOVE BYB-OUTBAL-BNKRPT(2 2 6) TO QBYB-OUTBAL-BNKRPT-2-2-6.
           MOVE BYB-UNITS-BNKRPT(2 2 7) TO QBYB-UNITS-BNKRPT-2-2-7.
           MOVE BYB-OUTBAL-BNKRPT(2 2 7) TO QBYB-OUTBAL-BNKRPT-2-2-7.
           MOVE BYB-UNITS-BNKRPT(2 2 8) TO QBYB-UNITS-BNKRPT-2-2-8.
           MOVE BYB-OUTBAL-BNKRPT(2 2 8) TO QBYB-OUTBAL-BNKRPT-2-2-8.
           MOVE BYB-UNITS-BNKRPT(2 2 9) TO QBYB-UNITS-BNKRPT-2-2-9.
           MOVE BYB-OUTBAL-BNKRPT(2 2 9) TO QBYB-OUTBAL-BNKRPT-2-2-9.
           MOVE BYB-UNITS-BNKRPT(2 2 10) TO QBYB-UNITS-BNKRPT-2-2-10.
           MOVE BYB-OUTBAL-BNKRPT(2 2 10) TO QBYB-OUTBAL-BNKRPT-2-2-10.
           MOVE BYB-UNITS-BNKRPT(3 1 1) TO QBYB-UNITS-BNKRPT-3-1-1.
           MOVE BYB-OUTBAL-BNKRPT(3 1 1) TO QBYB-OUTBAL-BNKRPT-3-1-1.
           MOVE BYB-UNITS-BNKRPT(3 1 2) TO QBYB-UNITS-BNKRPT-3-1-2.
           MOVE BYB-OUTBAL-BNKRPT(3 1 2) TO QBYB-OUTBAL-BNKRPT-3-1-2.
           MOVE BYB-UNITS-BNKRPT(3 1 3) TO QBYB-UNITS-BNKRPT-3-1-3.
           MOVE BYB-OUTBAL-BNKRPT(3 1 3) TO QBYB-OUTBAL-BNKRPT-3-1-3.
           MOVE BYB-UNITS-BNKRPT(3 1 4) TO QBYB-UNITS-BNKRPT-3-1-4.
           MOVE BYB-OUTBAL-BNKRPT(3 1 4) TO QBYB-OUTBAL-BNKRPT-3-1-4.
           MOVE BYB-UNITS-BNKRPT(3 1 5) TO QBYB-UNITS-BNKRPT-3-1-5.
           MOVE BYB-OUTBAL-BNKRPT(3 1 5) TO QBYB-OUTBAL-BNKRPT-3-1-5.
           MOVE BYB-UNITS-BNKRPT(3 1 6) TO QBYB-UNITS-BNKRPT-3-1-6.
           MOVE BYB-OUTBAL-BNKRPT(3 1 6) TO QBYB-OUTBAL-BNKRPT-3-1-6.
           MOVE BYB-UNITS-BNKRPT(3 1 7) TO QBYB-UNITS-BNKRPT-3-1-7.
           MOVE BYB-OUTBAL-BNKRPT(3 1 7) TO QBYB-OUTBAL-BNKRPT-3-1-7.
           MOVE BYB-UNITS-BNKRPT(3 1 8) TO QBYB-UNITS-BNKRPT-3-1-8.
           MOVE BYB-OUTBAL-BNKRPT(3 1 8) TO QBYB-OUTBAL-BNKRPT-3-1-8.
           MOVE BYB-UNITS-BNKRPT(3 1 9) TO QBYB-UNITS-BNKRPT-3-1-9.
           MOVE BYB-OUTBAL-BNKRPT(3 1 9) TO QBYB-OUTBAL-BNKRPT-3-1-9.
           MOVE BYB-UNITS-BNKRPT(3 1 10) TO QBYB-UNITS-BNKRPT-3-1-10.
           MOVE BYB-OUTBAL-BNKRPT(3 1 10) TO QBYB-OUTBAL-BNKRPT-3-1-10.
           MOVE BYB-UNITS-BNKRPT(3 2 1) TO QBYB-UNITS-BNKRPT-3-2-1.
           MOVE BYB-OUTBAL-BNKRPT(3 2 1) TO QBYB-OUTBAL-BNKRPT-3-2-1.
           MOVE BYB-UNITS-BNKRPT(3 2 2) TO QBYB-UNITS-BNKRPT-3-2-2.
           MOVE BYB-OUTBAL-BNKRPT(3 2 2) TO QBYB-OUTBAL-BNKRPT-3-2-2.
           MOVE BYB-UNITS-BNKRPT(3 2 3) TO QBYB-UNITS-BNKRPT-3-2-3.
           MOVE BYB-OUTBAL-BNKRPT(3 2 3) TO QBYB-OUTBAL-BNKRPT-3-2-3.
           MOVE BYB-UNITS-BNKRPT(3 2 4) TO QBYB-UNITS-BNKRPT-3-2-4.
           MOVE BYB-OUTBAL-BNKRPT(3 2 4) TO QBYB-OUTBAL-BNKRPT-3-2-4.
           MOVE BYB-UNITS-BNKRPT(3 2 5) TO QBYB-UNITS-BNKRPT-3-2-5.
           MOVE BYB-OUTBAL-BNKRPT(3 2 5) TO QBYB-OUTBAL-BNKRPT-3-2-5.
           MOVE BYB-UNITS-BNKRPT(3 2 6) TO QBYB-UNITS-BNKRPT-3-2-6.
           MOVE BYB-OUTBAL-BNKRPT(3 2 6) TO QBYB-OUTBAL-BNKRPT-3-2-6.
           MOVE BYB-UNITS-BNKRPT(3 2 7) TO QBYB-UNITS-BNKRPT-3-2-7.
           MOVE BYB-OUTBAL-BNKRPT(3 2 7) TO QBYB-OUTBAL-BNKRPT-3-2-7.
           MOVE BYB-UNITS-BNKRPT(3 2 8) TO QBYB-UNITS-BNKRPT-3-2-8.
           MOVE BYB-OUTBAL-BNKRPT(3 2 8) TO QBYB-OUTBAL-BNKRPT-3-2-8.
           MOVE BYB-UNITS-BNKRPT(3 2 9) TO QBYB-UNITS-BNKRPT-3-2-9.
           MOVE BYB-OUTBAL-BNKRPT(3 2 9) TO QBYB-OUTBAL-BNKRPT-3-2-9.
           MOVE BYB-UNITS-BNKRPT(3 2 10) TO QBYB-UNITS-BNKRPT-3-2-10.
           MOVE BYB-OUTBAL-BNKRPT(3 2 10) TO QBYB-OUTBAL-BNKRPT-3-2-10.
           MOVE BYB-SUMWGTTM TO QBYB-SUMWGTTM.
           MOVE BYB-SUMWGTAV TO QBYB-SUMWGTAV.
           MOVE BYB-SUMWGTTA TO QBYB-SUMWGTTA.
           MOVE BYB-IB-ACCRUED TO QBYB-IB-ACCRUED.
           MOVE BYB-DLDISC(1) TO QBYB-DLDISC-1.
           MOVE BYB-DLDISC(2) TO QBYB-DLDISC-2.
           MOVE BYB-DLDISC(3) TO QBYB-DLDISC-3.
           MOVE BYB-STD-POS TO QBYB-STD-POS.
           MOVE BYB-30-DAY-MOVED TO QBYB-30-DAY-MOVED.
           MOVE BYB-PROJ-INCREASE TO QBYB-PROJ-INCREASE.
           MOVE BYB-PROJ-PROFIT TO QBYB-PROJ-PROFIT.
           MOVE BYB-PROCEEDS(1) TO QBYB-PROCEEDS-1.
           MOVE BYB-PROCEEDS(2) TO QBYB-PROCEEDS-2.
           MOVE BYB-LOCKBOX(1) TO QBYB-LOCKBOX-1.
           MOVE BYB-LOCKBOX(2) TO QBYB-LOCKBOX-2.
           MOVE BYB-UNEARNED-AMT(1 1 1) TO QBYB-UNEARNED-AMT-1-1-1.
           MOVE BYB-UNEARNED-AMT(1 1 2) TO QBYB-UNEARNED-AMT-1-1-2.
           MOVE BYB-UNEARNED-AMT(1 1 3) TO QBYB-UNEARNED-AMT-1-1-3.
           MOVE BYB-UNEARNED-AMT(1 1 4) TO QBYB-UNEARNED-AMT-1-1-4.
           MOVE BYB-UNEARNED-AMT(1 1 5) TO QBYB-UNEARNED-AMT-1-1-5.
           MOVE BYB-UNEARNED-AMT(1 1 6) TO QBYB-UNEARNED-AMT-1-1-6.
           MOVE BYB-UNEARNED-AMT(1 1 7) TO QBYB-UNEARNED-AMT-1-1-7.
           MOVE BYB-UNEARNED-AMT(1 1 8) TO QBYB-UNEARNED-AMT-1-1-8.
           MOVE BYB-UNEARNED-AMT(1 1 9) TO QBYB-UNEARNED-AMT-1-1-9.
           MOVE BYB-UNEARNED-AMT(1 1 10) TO QBYB-UNEARNED-AMT-1-1-10.
           MOVE BYB-UNEARNED-AMT(1 2 1) TO QBYB-UNEARNED-AMT-1-2-1.
           MOVE BYB-UNEARNED-AMT(1 2 2) TO QBYB-UNEARNED-AMT-1-2-2.
           MOVE BYB-UNEARNED-AMT(1 2 3) TO QBYB-UNEARNED-AMT-1-2-3.
           MOVE BYB-UNEARNED-AMT(1 2 4) TO QBYB-UNEARNED-AMT-1-2-4.
           MOVE BYB-UNEARNED-AMT(1 2 5) TO QBYB-UNEARNED-AMT-1-2-5.
           MOVE BYB-UNEARNED-AMT(1 2 6) TO QBYB-UNEARNED-AMT-1-2-6.
           MOVE BYB-UNEARNED-AMT(1 2 7) TO QBYB-UNEARNED-AMT-1-2-7.
           MOVE BYB-UNEARNED-AMT(1 2 8) TO QBYB-UNEARNED-AMT-1-2-8.
           MOVE BYB-UNEARNED-AMT(1 2 9) TO QBYB-UNEARNED-AMT-1-2-9.
           MOVE BYB-UNEARNED-AMT(1 2 10) TO QBYB-UNEARNED-AMT-1-2-10.
           MOVE BYB-UNEARNED-AMT(2 1 1) TO QBYB-UNEARNED-AMT-2-1-1.
           MOVE BYB-UNEARNED-AMT(2 1 2) TO QBYB-UNEARNED-AMT-2-1-2.
           MOVE BYB-UNEARNED-AMT(2 1 3) TO QBYB-UNEARNED-AMT-2-1-3.
           MOVE BYB-UNEARNED-AMT(2 1 4) TO QBYB-UNEARNED-AMT-2-1-4.
           MOVE BYB-UNEARNED-AMT(2 1 5) TO QBYB-UNEARNED-AMT-2-1-5.
           MOVE BYB-UNEARNED-AMT(2 1 6) TO QBYB-UNEARNED-AMT-2-1-6.
           MOVE BYB-UNEARNED-AMT(2 1 7) TO QBYB-UNEARNED-AMT-2-1-7.
           MOVE BYB-UNEARNED-AMT(2 1 8) TO QBYB-UNEARNED-AMT-2-1-8.
           MOVE BYB-UNEARNED-AMT(2 1 9) TO QBYB-UNEARNED-AMT-2-1-9.
           MOVE BYB-UNEARNED-AMT(2 1 10) TO QBYB-UNEARNED-AMT-2-1-10.
           MOVE BYB-UNEARNED-AMT(2 2 1) TO QBYB-UNEARNED-AMT-2-2-1.
           MOVE BYB-UNEARNED-AMT(2 2 2) TO QBYB-UNEARNED-AMT-2-2-2.
           MOVE BYB-UNEARNED-AMT(2 2 3) TO QBYB-UNEARNED-AMT-2-2-3.
           MOVE BYB-UNEARNED-AMT(2 2 4) TO QBYB-UNEARNED-AMT-2-2-4.
           MOVE BYB-UNEARNED-AMT(2 2 5) TO QBYB-UNEARNED-AMT-2-2-5.
           MOVE BYB-UNEARNED-AMT(2 2 6) TO QBYB-UNEARNED-AMT-2-2-6.
           MOVE BYB-UNEARNED-AMT(2 2 7) TO QBYB-UNEARNED-AMT-2-2-7.
           MOVE BYB-UNEARNED-AMT(2 2 8) TO QBYB-UNEARNED-AMT-2-2-8.
           MOVE BYB-UNEARNED-AMT(2 2 9) TO QBYB-UNEARNED-AMT-2-2-9.
           MOVE BYB-UNEARNED-AMT(2 2 10) TO QBYB-UNEARNED-AMT-2-2-10.
           MOVE BYB-UNEARNED-AMT(3 1 1) TO QBYB-UNEARNED-AMT-3-1-1.
           MOVE BYB-UNEARNED-AMT(3 1 2) TO QBYB-UNEARNED-AMT-3-1-2.
           MOVE BYB-UNEARNED-AMT(3 1 3) TO QBYB-UNEARNED-AMT-3-1-3.
           MOVE BYB-UNEARNED-AMT(3 1 4) TO QBYB-UNEARNED-AMT-3-1-4.
           MOVE BYB-UNEARNED-AMT(3 1 5) TO QBYB-UNEARNED-AMT-3-1-5.
           MOVE BYB-UNEARNED-AMT(3 1 6) TO QBYB-UNEARNED-AMT-3-1-6.
           MOVE BYB-UNEARNED-AMT(3 1 7) TO QBYB-UNEARNED-AMT-3-1-7.
           MOVE BYB-UNEARNED-AMT(3 1 8) TO QBYB-UNEARNED-AMT-3-1-8.
           MOVE BYB-UNEARNED-AMT(3 1 9) TO QBYB-UNEARNED-AMT-3-1-9.
           MOVE BYB-UNEARNED-AMT(3 1 10) TO QBYB-UNEARNED-AMT-3-1-10.
           MOVE BYB-UNEARNED-AMT(3 2 1) TO QBYB-UNEARNED-AMT-3-2-1.
           MOVE BYB-UNEARNED-AMT(3 2 2) TO QBYB-UNEARNED-AMT-3-2-2.
           MOVE BYB-UNEARNED-AMT(3 2 3) TO QBYB-UNEARNED-AMT-3-2-3.
           MOVE BYB-UNEARNED-AMT(3 2 4) TO QBYB-UNEARNED-AMT-3-2-4.
           MOVE BYB-UNEARNED-AMT(3 2 5) TO QBYB-UNEARNED-AMT-3-2-5.
           MOVE BYB-UNEARNED-AMT(3 2 6) TO QBYB-UNEARNED-AMT-3-2-6.
           MOVE BYB-UNEARNED-AMT(3 2 7) TO QBYB-UNEARNED-AMT-3-2-7.
           MOVE BYB-UNEARNED-AMT(3 2 8) TO QBYB-UNEARNED-AMT-3-2-8.
           MOVE BYB-UNEARNED-AMT(3 2 9) TO QBYB-UNEARNED-AMT-3-2-9.
           MOVE BYB-UNEARNED-AMT(3 2 10) TO QBYB-UNEARNED-AMT-3-2-10.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBLP/LPBYA1RN
       LOAD-BY1-FILE.
           PERFORM LOAD-BYA1-FILE.
           PERFORM LOAD-BYB1-FILE.

      *-----------------------------------------------------------------
       OPEN-BY1-FILE.
           PERFORM OPEN-BYA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM OPEN-BYB1-FILE
              IF ( IO-FG = 0 )
                 MOVE " " TO IO-MULTI-FG
              ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       START-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM START-BYA1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM READ-BYA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM READ-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-BY1-FILE-NEXT.
           PERFORM SET-BY1-TO-ALL.
           PERFORM READ-BYA1-FILE-NEXT.
           IF ( IO-FG = 0 )
              MOVE BYA1-KEY TO BYB1-KEY
              PERFORM READ-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       CLOSE-BY1-FILE.
           PERFORM CLOSE-BYA1-FILE.
           PERFORM CLOSE-BYB1-FILE.

      *=================================================================
       LOAD-BYA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BY-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BYA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BYA-ALL-MACHINE
              MOVE BYA-ALL-PATH            TO BYA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BYA-ALL-BASE
              MOVE FILPATH-MACHINE         TO BYA-ALL-MACHINE
              MOVE BYA-ALL-PATH            TO BYA-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BYAFILE NOT = "S" )
              MOVE BYA-PATH-BEG            TO BYA-SELECT-PATH-BEG
              MOVE BYA-PATH-FILE           TO BYA-SELECT-PATH-FILE
           ELSE
              MOVE BYA-PATH-SQL            TO BYA-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BYA1-FILE.
           PERFORM LOAD-BYA1-FILE.
           PERFORM OPEN-IT.
           MOVE BYA-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BYA1-FILE.
           PERFORM START-IT.
           MOVE BYA-SELECT-PATH TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.

           IF ( BYA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BYA1-FILE.

           IF ( QBY1-WEND-DATE-CCYYMM = ZEROES ) OR
              ( QBY1-WEND-DATE-CCYYMM NOT NUMERIC )
              MOVE BY-DATE-CCYYMM       TO QBY1-WBEG-DATE-CCYYMM
              MOVE ALL "9"              TO QBY1-WEND-DATE-CCYYMM 
              MOVE BY-BRNO              TO QBY1-WBEG-BRNO
              MOVE ALL "9"              TO QBY1-WEND-BRNO
              MOVE BY-CLASS             TO QBY1-WBEG-CLASS
              MOVE ALL "9"              TO QBY1-WEND-CLASS.

           EXEC SQL
            DECLARE BYA1_CURSOR CURSOR FOR
            SELECT BYAFILE.BYA_DATE_CCYYMM,
                   BYAFILE.BYA_BRNO,
                   BYAFILE.BYA_CLASS,
                   ISNULL(CAST(BYAFILE.BYA_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   BYAFILE.BYA_NO_1_1_1,
                   BYAFILE.BYA_AMT_1_1_1,
                   BYAFILE.BYA_O2_1_1_1,
                   BYAFILE.BYA_INT_1_1_1,
                   BYAFILE.BYA_NO_1_1_2,
                   BYAFILE.BYA_AMT_1_1_2,
                   BYAFILE.BYA_O2_1_1_2,
                   BYAFILE.BYA_INT_1_1_2,
                   BYAFILE.BYA_NO_1_1_3,
                   BYAFILE.BYA_AMT_1_1_3,
                   BYAFILE.BYA_O2_1_1_3,
                   BYAFILE.BYA_INT_1_1_3,
                   BYAFILE.BYA_NO_1_1_4,
                   BYAFILE.BYA_AMT_1_1_4,
                   BYAFILE.BYA_O2_1_1_4,
                   BYAFILE.BYA_INT_1_1_4,
                   BYAFILE.BYA_NO_1_1_5,
                   BYAFILE.BYA_AMT_1_1_5,
                   BYAFILE.BYA_O2_1_1_5,
                   BYAFILE.BYA_INT_1_1_5,
                   BYAFILE.BYA_NO_1_1_6,
                   BYAFILE.BYA_AMT_1_1_6,
                   BYAFILE.BYA_O2_1_1_6,
                   BYAFILE.BYA_INT_1_1_6,
                   BYAFILE.BYA_NO_1_1_7,
                   BYAFILE.BYA_AMT_1_1_7,
                   BYAFILE.BYA_O2_1_1_7,
                   BYAFILE.BYA_INT_1_1_7,
                   BYAFILE.BYA_NO_1_1_8,
                   BYAFILE.BYA_AMT_1_1_8,
                   BYAFILE.BYA_O2_1_1_8,
                   BYAFILE.BYA_INT_1_1_8,
                   BYAFILE.BYA_NO_1_1_9,
                   BYAFILE.BYA_AMT_1_1_9,
                   BYAFILE.BYA_O2_1_1_9,
                   BYAFILE.BYA_INT_1_1_9,
                   BYAFILE.BYA_NO_1_1_10,
                   BYAFILE.BYA_AMT_1_1_10,
                   BYAFILE.BYA_O2_1_1_10,
                   BYAFILE.BYA_INT_1_1_10,
                   BYAFILE.BYA_NO_1_1_11,
                   BYAFILE.BYA_AMT_1_1_11,
                   BYAFILE.BYA_O2_1_1_11,
                   BYAFILE.BYA_INT_1_1_11,
                   BYAFILE.BYA_NO_1_2_1,
                   BYAFILE.BYA_AMT_1_2_1,
                   BYAFILE.BYA_O2_1_2_1,
                   BYAFILE.BYA_INT_1_2_1,
                   BYAFILE.BYA_NO_1_2_2,
                   BYAFILE.BYA_AMT_1_2_2,
                   BYAFILE.BYA_O2_1_2_2,
                   BYAFILE.BYA_INT_1_2_2,
                   BYAFILE.BYA_NO_1_2_3,
                   BYAFILE.BYA_AMT_1_2_3,
                   BYAFILE.BYA_O2_1_2_3,
                   BYAFILE.BYA_INT_1_2_3,
                   BYAFILE.BYA_NO_1_2_4,
                   BYAFILE.BYA_AMT_1_2_4,
                   BYAFILE.BYA_O2_1_2_4,
                   BYAFILE.BYA_INT_1_2_4,
                   BYAFILE.BYA_NO_1_2_5,
                   BYAFILE.BYA_AMT_1_2_5,
                   BYAFILE.BYA_O2_1_2_5,
                   BYAFILE.BYA_INT_1_2_5,
                   BYAFILE.BYA_NO_1_2_6,
                   BYAFILE.BYA_AMT_1_2_6,
                   BYAFILE.BYA_O2_1_2_6,
                   BYAFILE.BYA_INT_1_2_6,
                   BYAFILE.BYA_NO_1_2_7,
                   BYAFILE.BYA_AMT_1_2_7,
                   BYAFILE.BYA_O2_1_2_7,
                   BYAFILE.BYA_INT_1_2_7,
                   BYAFILE.BYA_NO_1_2_8,
                   BYAFILE.BYA_AMT_1_2_8,
                   BYAFILE.BYA_O2_1_2_8,
                   BYAFILE.BYA_INT_1_2_8,
                   BYAFILE.BYA_NO_1_2_9,
                   BYAFILE.BYA_AMT_1_2_9,
                   BYAFILE.BYA_O2_1_2_9,
                   BYAFILE.BYA_INT_1_2_9,
                   BYAFILE.BYA_NO_1_2_10,
                   BYAFILE.BYA_AMT_1_2_10,
                   BYAFILE.BYA_O2_1_2_10,
                   BYAFILE.BYA_INT_1_2_10,
                   BYAFILE.BYA_NO_1_2_11,
                   BYAFILE.BYA_AMT_1_2_11,
                   BYAFILE.BYA_O2_1_2_11,
                   BYAFILE.BYA_INT_1_2_11,
                   BYAFILE.BYA_NO_2_1_1,
                   BYAFILE.BYA_AMT_2_1_1,
                   BYAFILE.BYA_O2_2_1_1,
                   BYAFILE.BYA_INT_2_1_1,
                   BYAFILE.BYA_NO_2_1_2,
                   BYAFILE.BYA_AMT_2_1_2,
                   BYAFILE.BYA_O2_2_1_2,
                   BYAFILE.BYA_INT_2_1_2,
                   BYAFILE.BYA_NO_2_1_3,
                   BYAFILE.BYA_AMT_2_1_3,
                   BYAFILE.BYA_O2_2_1_3,
                   BYAFILE.BYA_INT_2_1_3,
                   BYAFILE.BYA_NO_2_1_4,
                   BYAFILE.BYA_AMT_2_1_4,
                   BYAFILE.BYA_O2_2_1_4,
                   BYAFILE.BYA_INT_2_1_4,
                   BYAFILE.BYA_NO_2_1_5,
                   BYAFILE.BYA_AMT_2_1_5,
                   BYAFILE.BYA_O2_2_1_5,
                   BYAFILE.BYA_INT_2_1_5,
                   BYAFILE.BYA_NO_2_1_6,
                   BYAFILE.BYA_AMT_2_1_6,
                   BYAFILE.BYA_O2_2_1_6,
                   BYAFILE.BYA_INT_2_1_6,
                   BYAFILE.BYA_NO_2_1_7,
                   BYAFILE.BYA_AMT_2_1_7,
                   BYAFILE.BYA_O2_2_1_7,
                   BYAFILE.BYA_INT_2_1_7,
                   BYAFILE.BYA_NO_2_1_8,
                   BYAFILE.BYA_AMT_2_1_8,
                   BYAFILE.BYA_O2_2_1_8,
                   BYAFILE.BYA_INT_2_1_8,
                   BYAFILE.BYA_NO_2_1_9,
                   BYAFILE.BYA_AMT_2_1_9,
                   BYAFILE.BYA_O2_2_1_9,
                   BYAFILE.BYA_INT_2_1_9,
                   BYAFILE.BYA_NO_2_1_10,
                   BYAFILE.BYA_AMT_2_1_10,
                   BYAFILE.BYA_O2_2_1_10,
                   BYAFILE.BYA_INT_2_1_10,
                   BYAFILE.BYA_NO_2_1_11,
                   BYAFILE.BYA_AMT_2_1_11,
                   BYAFILE.BYA_O2_2_1_11,
                   BYAFILE.BYA_INT_2_1_11,
                   BYAFILE.BYA_NO_2_2_1,
                   BYAFILE.BYA_AMT_2_2_1,
                   BYAFILE.BYA_O2_2_2_1,
                   BYAFILE.BYA_INT_2_2_1,
                   BYAFILE.BYA_NO_2_2_2,
                   BYAFILE.BYA_AMT_2_2_2,
                   BYAFILE.BYA_O2_2_2_2,
                   BYAFILE.BYA_INT_2_2_2,
                   BYAFILE.BYA_NO_2_2_3,
                   BYAFILE.BYA_AMT_2_2_3,
                   BYAFILE.BYA_O2_2_2_3,
                   BYAFILE.BYA_INT_2_2_3,
                   BYAFILE.BYA_NO_2_2_4,
                   BYAFILE.BYA_AMT_2_2_4,
                   BYAFILE.BYA_O2_2_2_4,
                   BYAFILE.BYA_INT_2_2_4,
                   BYAFILE.BYA_NO_2_2_5,
                   BYAFILE.BYA_AMT_2_2_5,
                   BYAFILE.BYA_O2_2_2_5,
                   BYAFILE.BYA_INT_2_2_5,
                   BYAFILE.BYA_NO_2_2_6,
                   BYAFILE.BYA_AMT_2_2_6,
                   BYAFILE.BYA_O2_2_2_6,
                   BYAFILE.BYA_INT_2_2_6,
                   BYAFILE.BYA_NO_2_2_7,
                   BYAFILE.BYA_AMT_2_2_7,
                   BYAFILE.BYA_O2_2_2_7,
                   BYAFILE.BYA_INT_2_2_7,
                   BYAFILE.BYA_NO_2_2_8,
                   BYAFILE.BYA_AMT_2_2_8,
                   BYAFILE.BYA_O2_2_2_8,
                   BYAFILE.BYA_INT_2_2_8,
                   BYAFILE.BYA_NO_2_2_9,
                   BYAFILE.BYA_AMT_2_2_9,
                   BYAFILE.BYA_O2_2_2_9,
                   BYAFILE.BYA_INT_2_2_9,
                   BYAFILE.BYA_NO_2_2_10,
                   BYAFILE.BYA_AMT_2_2_10,
                   BYAFILE.BYA_O2_2_2_10,
                   BYAFILE.BYA_INT_2_2_10,
                   BYAFILE.BYA_NO_2_2_11,
                   BYAFILE.BYA_AMT_2_2_11,
                   BYAFILE.BYA_O2_2_2_11,
                   BYAFILE.BYA_INT_2_2_11,
                   BYAFILE.BYA_NO_3_1_1,
                   BYAFILE.BYA_AMT_3_1_1,
                   BYAFILE.BYA_O2_3_1_1,
                   BYAFILE.BYA_INT_3_1_1,
                   BYAFILE.BYA_NO_3_1_2,
                   BYAFILE.BYA_AMT_3_1_2,
                   BYAFILE.BYA_O2_3_1_2,
                   BYAFILE.BYA_INT_3_1_2,
                   BYAFILE.BYA_NO_3_1_3,
                   BYAFILE.BYA_AMT_3_1_3,
                   BYAFILE.BYA_O2_3_1_3,
                   BYAFILE.BYA_INT_3_1_3,
                   BYAFILE.BYA_NO_3_1_4,
                   BYAFILE.BYA_AMT_3_1_4,
                   BYAFILE.BYA_O2_3_1_4,
                   BYAFILE.BYA_INT_3_1_4,
                   BYAFILE.BYA_NO_3_1_5,
                   BYAFILE.BYA_AMT_3_1_5,
                   BYAFILE.BYA_O2_3_1_5,
                   BYAFILE.BYA_INT_3_1_5,
                   BYAFILE.BYA_NO_3_1_6,
                   BYAFILE.BYA_AMT_3_1_6,
                   BYAFILE.BYA_O2_3_1_6,
                   BYAFILE.BYA_INT_3_1_6,
                   BYAFILE.BYA_NO_3_1_7,
                   BYAFILE.BYA_AMT_3_1_7,
                   BYAFILE.BYA_O2_3_1_7,
                   BYAFILE.BYA_INT_3_1_7,
                   BYAFILE.BYA_NO_3_1_8,
                   BYAFILE.BYA_AMT_3_1_8,
                   BYAFILE.BYA_O2_3_1_8,
                   BYAFILE.BYA_INT_3_1_8,
                   BYAFILE.BYA_NO_3_1_9,
                   BYAFILE.BYA_AMT_3_1_9,
                   BYAFILE.BYA_O2_3_1_9,
                   BYAFILE.BYA_INT_3_1_9,
                   BYAFILE.BYA_NO_3_1_10,
                   BYAFILE.BYA_AMT_3_1_10,
                   BYAFILE.BYA_O2_3_1_10,
                   BYAFILE.BYA_INT_3_1_10,
                   BYAFILE.BYA_NO_3_1_11,
                   BYAFILE.BYA_AMT_3_1_11,
                   BYAFILE.BYA_O2_3_1_11,
                   BYAFILE.BYA_INT_3_1_11,
                   BYAFILE.BYA_NO_3_2_1,
                   BYAFILE.BYA_AMT_3_2_1,
                   BYAFILE.BYA_O2_3_2_1,
                   BYAFILE.BYA_INT_3_2_1,
                   BYAFILE.BYA_NO_3_2_2,
                   BYAFILE.BYA_AMT_3_2_2,
                   BYAFILE.BYA_O2_3_2_2,
                   BYAFILE.BYA_INT_3_2_2,
                   BYAFILE.BYA_NO_3_2_3,
                   BYAFILE.BYA_AMT_3_2_3,
                   BYAFILE.BYA_O2_3_2_3,
                   BYAFILE.BYA_INT_3_2_3,
                   BYAFILE.BYA_NO_3_2_4,
                   BYAFILE.BYA_AMT_3_2_4,
                   BYAFILE.BYA_O2_3_2_4,
                   BYAFILE.BYA_INT_3_2_4,
                   BYAFILE.BYA_NO_3_2_5,
                   BYAFILE.BYA_AMT_3_2_5,
                   BYAFILE.BYA_O2_3_2_5,
                   BYAFILE.BYA_INT_3_2_5,
                   BYAFILE.BYA_NO_3_2_6,
                   BYAFILE.BYA_AMT_3_2_6,
                   BYAFILE.BYA_O2_3_2_6,
                   BYAFILE.BYA_INT_3_2_6,
                   BYAFILE.BYA_NO_3_2_7,
                   BYAFILE.BYA_AMT_3_2_7,
                   BYAFILE.BYA_O2_3_2_7,
                   BYAFILE.BYA_INT_3_2_7,
                   BYAFILE.BYA_NO_3_2_8,
                   BYAFILE.BYA_AMT_3_2_8,
                   BYAFILE.BYA_O2_3_2_8,
                   BYAFILE.BYA_INT_3_2_8,
                   BYAFILE.BYA_NO_3_2_9,
                   BYAFILE.BYA_AMT_3_2_9,
                   BYAFILE.BYA_O2_3_2_9,
                   BYAFILE.BYA_INT_3_2_9,
                   BYAFILE.BYA_NO_3_2_10,
                   BYAFILE.BYA_AMT_3_2_10,
                   BYAFILE.BYA_O2_3_2_10,
                   BYAFILE.BYA_INT_3_2_10,
                   BYAFILE.BYA_NO_3_2_11,
                   BYAFILE.BYA_AMT_3_2_11,
                   BYAFILE.BYA_O2_3_2_11,
                   BYAFILE.BYA_INT_3_2_11,
                   BYAFILE.BYA_NO_REPO_1_1_1,
                   BYAFILE.BYA_AMT_REPO_1_1_1,
                   BYAFILE.BYA_O2_REPO_1_1_1,
                   BYAFILE.BYA_INT_REPO_1_1_1,
                   BYAFILE.BYA_NO_REPO_1_1_2,
                   BYAFILE.BYA_AMT_REPO_1_1_2,
                   BYAFILE.BYA_O2_REPO_1_1_2,
                   BYAFILE.BYA_INT_REPO_1_1_2,
                   BYAFILE.BYA_NO_REPO_1_1_3,
                   BYAFILE.BYA_AMT_REPO_1_1_3,
                   BYAFILE.BYA_O2_REPO_1_1_3,
                   BYAFILE.BYA_INT_REPO_1_1_3,
                   BYAFILE.BYA_NO_REPO_1_1_4,
                   BYAFILE.BYA_AMT_REPO_1_1_4,
                   BYAFILE.BYA_O2_REPO_1_1_4,
                   BYAFILE.BYA_INT_REPO_1_1_4,
                   BYAFILE.BYA_NO_REPO_1_1_5,
                   BYAFILE.BYA_AMT_REPO_1_1_5,
                   BYAFILE.BYA_O2_REPO_1_1_5,
                   BYAFILE.BYA_INT_REPO_1_1_5,
                   BYAFILE.BYA_NO_REPO_1_2_1,
                   BYAFILE.BYA_AMT_REPO_1_2_1,
                   BYAFILE.BYA_O2_REPO_1_2_1,
                   BYAFILE.BYA_INT_REPO_1_2_1,
                   BYAFILE.BYA_NO_REPO_1_2_2,
                   BYAFILE.BYA_AMT_REPO_1_2_2,
                   BYAFILE.BYA_O2_REPO_1_2_2,
                   BYAFILE.BYA_INT_REPO_1_2_2,
                   BYAFILE.BYA_NO_REPO_1_2_3,
                   BYAFILE.BYA_AMT_REPO_1_2_3,
                   BYAFILE.BYA_O2_REPO_1_2_3,
                   BYAFILE.BYA_INT_REPO_1_2_3,
                   BYAFILE.BYA_NO_REPO_1_2_4,
                   BYAFILE.BYA_AMT_REPO_1_2_4,
                   BYAFILE.BYA_O2_REPO_1_2_4,
                   BYAFILE.BYA_INT_REPO_1_2_4,
                   BYAFILE.BYA_NO_REPO_1_2_5,
                   BYAFILE.BYA_AMT_REPO_1_2_5,
                   BYAFILE.BYA_O2_REPO_1_2_5,
                   BYAFILE.BYA_INT_REPO_1_2_5,
                   BYAFILE.BYA_NO_REPO_2_1_1,
                   BYAFILE.BYA_AMT_REPO_2_1_1,
                   BYAFILE.BYA_O2_REPO_2_1_1,
                   BYAFILE.BYA_INT_REPO_2_1_1,
                   BYAFILE.BYA_NO_REPO_2_1_2,
                   BYAFILE.BYA_AMT_REPO_2_1_2,
                   BYAFILE.BYA_O2_REPO_2_1_2,
                   BYAFILE.BYA_INT_REPO_2_1_2,
                   BYAFILE.BYA_NO_REPO_2_1_3,
                   BYAFILE.BYA_AMT_REPO_2_1_3,
                   BYAFILE.BYA_O2_REPO_2_1_3,
                   BYAFILE.BYA_INT_REPO_2_1_3,
                   BYAFILE.BYA_NO_REPO_2_1_4,
                   BYAFILE.BYA_AMT_REPO_2_1_4,
                   BYAFILE.BYA_O2_REPO_2_1_4,
                   BYAFILE.BYA_INT_REPO_2_1_4,
                   BYAFILE.BYA_NO_REPO_2_1_5,
                   BYAFILE.BYA_AMT_REPO_2_1_5,
                   BYAFILE.BYA_O2_REPO_2_1_5,
                   BYAFILE.BYA_INT_REPO_2_1_5,
                   BYAFILE.BYA_NO_REPO_2_2_1,
                   BYAFILE.BYA_AMT_REPO_2_2_1,
                   BYAFILE.BYA_O2_REPO_2_2_1,
                   BYAFILE.BYA_INT_REPO_2_2_1,
                   BYAFILE.BYA_NO_REPO_2_2_2,
                   BYAFILE.BYA_AMT_REPO_2_2_2,
                   BYAFILE.BYA_O2_REPO_2_2_2,
                   BYAFILE.BYA_INT_REPO_2_2_2,
                   BYAFILE.BYA_NO_REPO_2_2_3,
                   BYAFILE.BYA_AMT_REPO_2_2_3,
                   BYAFILE.BYA_O2_REPO_2_2_3,
                   BYAFILE.BYA_INT_REPO_2_2_3,
                   BYAFILE.BYA_NO_REPO_2_2_4,
                   BYAFILE.BYA_AMT_REPO_2_2_4,
                   BYAFILE.BYA_O2_REPO_2_2_4,
                   BYAFILE.BYA_INT_REPO_2_2_4,
                   BYAFILE.BYA_NO_REPO_2_2_5,
                   BYAFILE.BYA_AMT_REPO_2_2_5,
                   BYAFILE.BYA_O2_REPO_2_2_5,
                   BYAFILE.BYA_INT_REPO_2_2_5,
                   BYAFILE.BYA_NO_REPO_3_1_1,
                   BYAFILE.BYA_AMT_REPO_3_1_1,
                   BYAFILE.BYA_O2_REPO_3_1_1,
                   BYAFILE.BYA_INT_REPO_3_1_1,
                   BYAFILE.BYA_NO_REPO_3_1_2,
                   BYAFILE.BYA_AMT_REPO_3_1_2,
                   BYAFILE.BYA_O2_REPO_3_1_2,
                   BYAFILE.BYA_INT_REPO_3_1_2,
                   BYAFILE.BYA_NO_REPO_3_1_3,
                   BYAFILE.BYA_AMT_REPO_3_1_3,
                   BYAFILE.BYA_O2_REPO_3_1_3,
                   BYAFILE.BYA_INT_REPO_3_1_3,
                   BYAFILE.BYA_NO_REPO_3_1_4,
                   BYAFILE.BYA_AMT_REPO_3_1_4,
                   BYAFILE.BYA_O2_REPO_3_1_4,
                   BYAFILE.BYA_INT_REPO_3_1_4,
                   BYAFILE.BYA_NO_REPO_3_1_5,
                   BYAFILE.BYA_AMT_REPO_3_1_5,
                   BYAFILE.BYA_O2_REPO_3_1_5,
                   BYAFILE.BYA_INT_REPO_3_1_5,
                   BYAFILE.BYA_NO_REPO_3_2_1,
                   BYAFILE.BYA_AMT_REPO_3_2_1,
                   BYAFILE.BYA_O2_REPO_3_2_1,
                   BYAFILE.BYA_INT_REPO_3_2_1,
                   BYAFILE.BYA_NO_REPO_3_2_2,
                   BYAFILE.BYA_AMT_REPO_3_2_2,
                   BYAFILE.BYA_O2_REPO_3_2_2,
                   BYAFILE.BYA_INT_REPO_3_2_2,
                   BYAFILE.BYA_NO_REPO_3_2_3,
                   BYAFILE.BYA_AMT_REPO_3_2_3,
                   BYAFILE.BYA_O2_REPO_3_2_3,
                   BYAFILE.BYA_INT_REPO_3_2_3,
                   BYAFILE.BYA_NO_REPO_3_2_4,
                   BYAFILE.BYA_AMT_REPO_3_2_4,
                   BYAFILE.BYA_O2_REPO_3_2_4,
                   BYAFILE.BYA_INT_REPO_3_2_4,
                   BYAFILE.BYA_NO_REPO_3_2_5,
                   BYAFILE.BYA_AMT_REPO_3_2_5,
                   BYAFILE.BYA_O2_REPO_3_2_5,
                   BYAFILE.BYA_INT_REPO_3_2_5,
                   BYAFILE.BYA_NO_PL_1_1_1,
                   BYAFILE.BYA_AMT_PL_1_1_1,
                   BYAFILE.BYA_O2_PL_1_1_1,
                   BYAFILE.BYA_INT_PL_1_1_1,
                   BYAFILE.BYA_NO_PL_1_1_2,
                   BYAFILE.BYA_AMT_PL_1_1_2,
                   BYAFILE.BYA_O2_PL_1_1_2,
                   BYAFILE.BYA_INT_PL_1_1_2,
                   BYAFILE.BYA_NO_PL_1_1_3,
                   BYAFILE.BYA_AMT_PL_1_1_3,
                   BYAFILE.BYA_O2_PL_1_1_3,
                   BYAFILE.BYA_INT_PL_1_1_3,
                   BYAFILE.BYA_NO_PL_1_1_4,
                   BYAFILE.BYA_AMT_PL_1_1_4,
                   BYAFILE.BYA_O2_PL_1_1_4,
                   BYAFILE.BYA_INT_PL_1_1_4,
                   BYAFILE.BYA_NO_PL_1_1_5,
                   BYAFILE.BYA_AMT_PL_1_1_5,
                   BYAFILE.BYA_O2_PL_1_1_5,
                   BYAFILE.BYA_INT_PL_1_1_5,
                   BYAFILE.BYA_NO_PL_1_1_6,
                   BYAFILE.BYA_AMT_PL_1_1_6,
                   BYAFILE.BYA_O2_PL_1_1_6,
                   BYAFILE.BYA_INT_PL_1_1_6,
                   BYAFILE.BYA_NO_PL_1_1_7,
                   BYAFILE.BYA_AMT_PL_1_1_7,
                   BYAFILE.BYA_O2_PL_1_1_7,
                   BYAFILE.BYA_INT_PL_1_1_7,
                   BYAFILE.BYA_NO_PL_1_1_8,
                   BYAFILE.BYA_AMT_PL_1_1_8,
                   BYAFILE.BYA_O2_PL_1_1_8,
                   BYAFILE.BYA_INT_PL_1_1_8,
                   BYAFILE.BYA_NO_PL_1_1_9,
                   BYAFILE.BYA_AMT_PL_1_1_9,
                   BYAFILE.BYA_O2_PL_1_1_9,
                   BYAFILE.BYA_INT_PL_1_1_9,
                   BYAFILE.BYA_NO_PL_1_1_10,
                   BYAFILE.BYA_AMT_PL_1_1_10,
                   BYAFILE.BYA_O2_PL_1_1_10,
                   BYAFILE.BYA_INT_PL_1_1_10,
                   BYAFILE.BYA_NO_PL_1_2_1,
                   BYAFILE.BYA_AMT_PL_1_2_1,
                   BYAFILE.BYA_O2_PL_1_2_1,
                   BYAFILE.BYA_INT_PL_1_2_1,
                   BYAFILE.BYA_NO_PL_1_2_2,
                   BYAFILE.BYA_AMT_PL_1_2_2,
                   BYAFILE.BYA_O2_PL_1_2_2,
                   BYAFILE.BYA_INT_PL_1_2_2,
                   BYAFILE.BYA_NO_PL_1_2_3,
                   BYAFILE.BYA_AMT_PL_1_2_3,
                   BYAFILE.BYA_O2_PL_1_2_3,
                   BYAFILE.BYA_INT_PL_1_2_3,
                   BYAFILE.BYA_NO_PL_1_2_4,
                   BYAFILE.BYA_AMT_PL_1_2_4,
                   BYAFILE.BYA_O2_PL_1_2_4,
                   BYAFILE.BYA_INT_PL_1_2_4,
                   BYAFILE.BYA_NO_PL_1_2_5,
                   BYAFILE.BYA_AMT_PL_1_2_5,
                   BYAFILE.BYA_O2_PL_1_2_5,
                   BYAFILE.BYA_INT_PL_1_2_5,
                   BYAFILE.BYA_NO_PL_1_2_6,
                   BYAFILE.BYA_AMT_PL_1_2_6,
                   BYAFILE.BYA_O2_PL_1_2_6,
                   BYAFILE.BYA_INT_PL_1_2_6,
                   BYAFILE.BYA_NO_PL_1_2_7,
                   BYAFILE.BYA_AMT_PL_1_2_7,
                   BYAFILE.BYA_O2_PL_1_2_7,
                   BYAFILE.BYA_INT_PL_1_2_7,
                   BYAFILE.BYA_NO_PL_1_2_8,
                   BYAFILE.BYA_AMT_PL_1_2_8,
                   BYAFILE.BYA_O2_PL_1_2_8,
                   BYAFILE.BYA_INT_PL_1_2_8,
                   BYAFILE.BYA_NO_PL_1_2_9,
                   BYAFILE.BYA_AMT_PL_1_2_9,
                   BYAFILE.BYA_O2_PL_1_2_9,
                   BYAFILE.BYA_INT_PL_1_2_9,
                   BYAFILE.BYA_NO_PL_1_2_10,
                   BYAFILE.BYA_AMT_PL_1_2_10,
                   BYAFILE.BYA_O2_PL_1_2_10,
                   BYAFILE.BYA_INT_PL_1_2_10,
                   BYAFILE.BYA_NO_PL_2_1_1,
                   BYAFILE.BYA_AMT_PL_2_1_1,
                   BYAFILE.BYA_O2_PL_2_1_1,
                   BYAFILE.BYA_INT_PL_2_1_1,
                   BYAFILE.BYA_NO_PL_2_1_2,
                   BYAFILE.BYA_AMT_PL_2_1_2,
                   BYAFILE.BYA_O2_PL_2_1_2,
                   BYAFILE.BYA_INT_PL_2_1_2,
                   BYAFILE.BYA_NO_PL_2_1_3,
                   BYAFILE.BYA_AMT_PL_2_1_3,
                   BYAFILE.BYA_O2_PL_2_1_3,
                   BYAFILE.BYA_INT_PL_2_1_3,
                   BYAFILE.BYA_NO_PL_2_1_4,
                   BYAFILE.BYA_AMT_PL_2_1_4,
                   BYAFILE.BYA_O2_PL_2_1_4,
                   BYAFILE.BYA_INT_PL_2_1_4,
                   BYAFILE.BYA_NO_PL_2_1_5,
                   BYAFILE.BYA_AMT_PL_2_1_5,
                   BYAFILE.BYA_O2_PL_2_1_5,
                   BYAFILE.BYA_INT_PL_2_1_5,
                   BYAFILE.BYA_NO_PL_2_1_6,
                   BYAFILE.BYA_AMT_PL_2_1_6,
                   BYAFILE.BYA_O2_PL_2_1_6,
                   BYAFILE.BYA_INT_PL_2_1_6,
                   BYAFILE.BYA_NO_PL_2_1_7,
                   BYAFILE.BYA_AMT_PL_2_1_7,
                   BYAFILE.BYA_O2_PL_2_1_7,
                   BYAFILE.BYA_INT_PL_2_1_7,
                   BYAFILE.BYA_NO_PL_2_1_8,
                   BYAFILE.BYA_AMT_PL_2_1_8,
                   BYAFILE.BYA_O2_PL_2_1_8,
                   BYAFILE.BYA_INT_PL_2_1_8,
                   BYAFILE.BYA_NO_PL_2_1_9,
                   BYAFILE.BYA_AMT_PL_2_1_9,
                   BYAFILE.BYA_O2_PL_2_1_9,
                   BYAFILE.BYA_INT_PL_2_1_9,
                   BYAFILE.BYA_NO_PL_2_1_10,
                   BYAFILE.BYA_AMT_PL_2_1_10,
                   BYAFILE.BYA_O2_PL_2_1_10,
                   BYAFILE.BYA_INT_PL_2_1_10,
                   BYAFILE.BYA_NO_PL_2_2_1,
                   BYAFILE.BYA_AMT_PL_2_2_1,
                   BYAFILE.BYA_O2_PL_2_2_1,
                   BYAFILE.BYA_INT_PL_2_2_1,
                   BYAFILE.BYA_NO_PL_2_2_2,
                   BYAFILE.BYA_AMT_PL_2_2_2,
                   BYAFILE.BYA_O2_PL_2_2_2,
                   BYAFILE.BYA_INT_PL_2_2_2,
                   BYAFILE.BYA_NO_PL_2_2_3,
                   BYAFILE.BYA_AMT_PL_2_2_3,
                   BYAFILE.BYA_O2_PL_2_2_3,
                   BYAFILE.BYA_INT_PL_2_2_3,
                   BYAFILE.BYA_NO_PL_2_2_4,
                   BYAFILE.BYA_AMT_PL_2_2_4,
                   BYAFILE.BYA_O2_PL_2_2_4,
                   BYAFILE.BYA_INT_PL_2_2_4,
                   BYAFILE.BYA_NO_PL_2_2_5,
                   BYAFILE.BYA_AMT_PL_2_2_5,
                   BYAFILE.BYA_O2_PL_2_2_5,
                   BYAFILE.BYA_INT_PL_2_2_5,
                   BYAFILE.BYA_NO_PL_2_2_6,
                   BYAFILE.BYA_AMT_PL_2_2_6,
                   BYAFILE.BYA_O2_PL_2_2_6,
                   BYAFILE.BYA_INT_PL_2_2_6,
                   BYAFILE.BYA_NO_PL_2_2_7,
                   BYAFILE.BYA_AMT_PL_2_2_7,
                   BYAFILE.BYA_O2_PL_2_2_7,
                   BYAFILE.BYA_INT_PL_2_2_7,
                   BYAFILE.BYA_NO_PL_2_2_8,
                   BYAFILE.BYA_AMT_PL_2_2_8,
                   BYAFILE.BYA_O2_PL_2_2_8,
                   BYAFILE.BYA_INT_PL_2_2_8,
                   BYAFILE.BYA_NO_PL_2_2_9,
                   BYAFILE.BYA_AMT_PL_2_2_9,
                   BYAFILE.BYA_O2_PL_2_2_9,
                   BYAFILE.BYA_INT_PL_2_2_9,
                   BYAFILE.BYA_NO_PL_2_2_10,
                   BYAFILE.BYA_AMT_PL_2_2_10,
                   BYAFILE.BYA_O2_PL_2_2_10,
                   BYAFILE.BYA_INT_PL_2_2_10,
                   BYAFILE.BYA_NO_PL_3_1_1,
                   BYAFILE.BYA_AMT_PL_3_1_1,
                   BYAFILE.BYA_O2_PL_3_1_1,
                   BYAFILE.BYA_INT_PL_3_1_1,
                   BYAFILE.BYA_NO_PL_3_1_2,
                   BYAFILE.BYA_AMT_PL_3_1_2,
                   BYAFILE.BYA_O2_PL_3_1_2,
                   BYAFILE.BYA_INT_PL_3_1_2,
                   BYAFILE.BYA_NO_PL_3_1_3,
                   BYAFILE.BYA_AMT_PL_3_1_3,
                   BYAFILE.BYA_O2_PL_3_1_3,
                   BYAFILE.BYA_INT_PL_3_1_3,
                   BYAFILE.BYA_NO_PL_3_1_4,
                   BYAFILE.BYA_AMT_PL_3_1_4,
                   BYAFILE.BYA_O2_PL_3_1_4,
                   BYAFILE.BYA_INT_PL_3_1_4,
                   BYAFILE.BYA_NO_PL_3_1_5,
                   BYAFILE.BYA_AMT_PL_3_1_5,
                   BYAFILE.BYA_O2_PL_3_1_5,
                   BYAFILE.BYA_INT_PL_3_1_5,
                   BYAFILE.BYA_NO_PL_3_1_6,
                   BYAFILE.BYA_AMT_PL_3_1_6,
                   BYAFILE.BYA_O2_PL_3_1_6,
                   BYAFILE.BYA_INT_PL_3_1_6,
                   BYAFILE.BYA_NO_PL_3_1_7,
                   BYAFILE.BYA_AMT_PL_3_1_7,
                   BYAFILE.BYA_O2_PL_3_1_7,
                   BYAFILE.BYA_INT_PL_3_1_7,
                   BYAFILE.BYA_NO_PL_3_1_8,
                   BYAFILE.BYA_AMT_PL_3_1_8,
                   BYAFILE.BYA_O2_PL_3_1_8,
                   BYAFILE.BYA_INT_PL_3_1_8,
                   BYAFILE.BYA_NO_PL_3_1_9,
                   BYAFILE.BYA_AMT_PL_3_1_9,
                   BYAFILE.BYA_O2_PL_3_1_9,
                   BYAFILE.BYA_INT_PL_3_1_9,
                   BYAFILE.BYA_NO_PL_3_1_10,
                   BYAFILE.BYA_AMT_PL_3_1_10,
                   BYAFILE.BYA_O2_PL_3_1_10,
                   BYAFILE.BYA_INT_PL_3_1_10,
                   BYAFILE.BYA_NO_PL_3_2_1,
                   BYAFILE.BYA_AMT_PL_3_2_1,
                   BYAFILE.BYA_O2_PL_3_2_1,
                   BYAFILE.BYA_INT_PL_3_2_1,
                   BYAFILE.BYA_NO_PL_3_2_2,
                   BYAFILE.BYA_AMT_PL_3_2_2,
                   BYAFILE.BYA_O2_PL_3_2_2,
                   BYAFILE.BYA_INT_PL_3_2_2,
                   BYAFILE.BYA_NO_PL_3_2_3,
                   BYAFILE.BYA_AMT_PL_3_2_3,
                   BYAFILE.BYA_O2_PL_3_2_3,
                   BYAFILE.BYA_INT_PL_3_2_3,
                   BYAFILE.BYA_NO_PL_3_2_4,
                   BYAFILE.BYA_AMT_PL_3_2_4,
                   BYAFILE.BYA_O2_PL_3_2_4,
                   BYAFILE.BYA_INT_PL_3_2_4,
                   BYAFILE.BYA_NO_PL_3_2_5,
                   BYAFILE.BYA_AMT_PL_3_2_5,
                   BYAFILE.BYA_O2_PL_3_2_5,
                   BYAFILE.BYA_INT_PL_3_2_5,
                   BYAFILE.BYA_NO_PL_3_2_6,
                   BYAFILE.BYA_AMT_PL_3_2_6,
                   BYAFILE.BYA_O2_PL_3_2_6,
                   BYAFILE.BYA_INT_PL_3_2_6,
                   BYAFILE.BYA_NO_PL_3_2_7,
                   BYAFILE.BYA_AMT_PL_3_2_7,
                   BYAFILE.BYA_O2_PL_3_2_7,
                   BYAFILE.BYA_INT_PL_3_2_7,
                   BYAFILE.BYA_NO_PL_3_2_8,
                   BYAFILE.BYA_AMT_PL_3_2_8,
                   BYAFILE.BYA_O2_PL_3_2_8,
                   BYAFILE.BYA_INT_PL_3_2_8,
                   BYAFILE.BYA_NO_PL_3_2_9,
                   BYAFILE.BYA_AMT_PL_3_2_9,
                   BYAFILE.BYA_O2_PL_3_2_9,
                   BYAFILE.BYA_INT_PL_3_2_9,
                   BYAFILE.BYA_NO_PL_3_2_10,
                   BYAFILE.BYA_AMT_PL_3_2_10,
                   BYAFILE.BYA_O2_PL_3_2_10,
                   BYAFILE.BYA_INT_PL_3_2_10,
                   BYAFILE.BYA_DLRVPRIN_1_1_1,
                   BYAFILE.BYA_DLRVPRIN_1_1_2,
                   BYAFILE.BYA_DLRVPRIN_1_1_3,
                   BYAFILE.BYA_DLRVCHGS_1_1_1,
                   BYAFILE.BYA_DLRVCHGS_1_1_2,
                   BYAFILE.BYA_DLRVCHGS_1_1_3,
                   BYAFILE.BYA_DLPARVHELD_1_1,
                   BYAFILE.BYA_DLRVPRIN_1_2_1,
                   BYAFILE.BYA_DLRVPRIN_1_2_2,
                   BYAFILE.BYA_DLRVPRIN_1_2_3,
                   BYAFILE.BYA_DLRVCHGS_1_2_1,
                   BYAFILE.BYA_DLRVCHGS_1_2_2,
                   BYAFILE.BYA_DLRVCHGS_1_2_3,
                   BYAFILE.BYA_DLPARVHELD_1_2,
                   BYAFILE.BYA_DLRVPRIN_2_1_1,
                   BYAFILE.BYA_DLRVPRIN_2_1_2,
                   BYAFILE.BYA_DLRVPRIN_2_1_3,
                   BYAFILE.BYA_DLRVCHGS_2_1_1,
                   BYAFILE.BYA_DLRVCHGS_2_1_2,
                   BYAFILE.BYA_DLRVCHGS_2_1_3,
                   BYAFILE.BYA_DLPARVHELD_2_1,
                   BYAFILE.BYA_DLRVPRIN_2_2_1,
                   BYAFILE.BYA_DLRVPRIN_2_2_2,
                   BYAFILE.BYA_DLRVPRIN_2_2_3,
                   BYAFILE.BYA_DLRVCHGS_2_2_1,
                   BYAFILE.BYA_DLRVCHGS_2_2_2,
                   BYAFILE.BYA_DLRVCHGS_2_2_3,
                   BYAFILE.BYA_DLPARVHELD_2_2,
                   BYAFILE.BYA_DLRVPRIN_3_1_1,
                   BYAFILE.BYA_DLRVPRIN_3_1_2,
                   BYAFILE.BYA_DLRVPRIN_3_1_3,
                   BYAFILE.BYA_DLRVCHGS_3_1_1,
                   BYAFILE.BYA_DLRVCHGS_3_1_2,
                   BYAFILE.BYA_DLRVCHGS_3_1_3,
                   BYAFILE.BYA_DLPARVHELD_3_1,
                   BYAFILE.BYA_DLRVPRIN_3_2_1,
                   BYAFILE.BYA_DLRVPRIN_3_2_2,
                   BYAFILE.BYA_DLRVPRIN_3_2_3,
                   BYAFILE.BYA_DLRVCHGS_3_2_1,
                   BYAFILE.BYA_DLRVCHGS_3_2_2,
                   BYAFILE.BYA_DLRVCHGS_3_2_3,
                   BYAFILE.BYA_DLPARVHELD_3_2
            FROM DBO.BYAFILE
            WHERE BYAFILE.BYA_DATE_CCYYMM >= :QBY1-WBEG-DATE-CCYYMM
              AND BYAFILE.BYA_DATE_CCYYMM <= :QBY1-WEND-DATE-CCYYMM
              AND BYAFILE.BYA_BRNO >= :QBY1-WBEG-BRNO
              AND BYAFILE.BYA_BRNO <= :QBY1-WEND-BRNO 
              AND BYAFILE.BYA_CLASS >= :QBY1-WBEG-CLASS
              AND BYAFILE.BYA_CLASS <= :QBY1-WEND-CLASS
           ORDER BY BYAFILE.BYA_DATE_CCYYMM,
                    BYAFILE.BYA_BRNO,
                    BYAFILE.BYA_CLASS
           END-EXEC.

           EXEC SQL
               OPEN BYA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBY1-WBEG-DATE-CCYYMM
                          QBY1-WEND-DATE-CCYYMM
                          QBY1-WBEG-BRNO
                          QBY1-WEND-BRNO
                          QBY1-WBEG-CLASS
                          QBY1-WEND-CLASS.

           MOVE STAT---GOOD TO BYA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BYA1-FILE.
           PERFORM READ-IT.
           MOVE BYA-SELECT-PATH TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.

           MOVE BYA-DATE-CCYYMM      TO QBYA-DATE-CCYYMM.
           MOVE BYA-BRNO             TO QBYA-BRNO.
           MOVE BYA-CLASS            TO QBYA-CLASS.

           EXEC SQL
            SELECT BYAFILE.BYA_DATE_CCYYMM,
                   BYAFILE.BYA_BRNO,
                   BYAFILE.BYA_CLASS,
                   ISNULL(CAST(BYAFILE.BYA_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   BYAFILE.BYA_NO_1_1_1,
                   BYAFILE.BYA_AMT_1_1_1,
                   BYAFILE.BYA_O2_1_1_1,
                   BYAFILE.BYA_INT_1_1_1,
                   BYAFILE.BYA_NO_1_1_2,
                   BYAFILE.BYA_AMT_1_1_2,
                   BYAFILE.BYA_O2_1_1_2,
                   BYAFILE.BYA_INT_1_1_2,
                   BYAFILE.BYA_NO_1_1_3,
                   BYAFILE.BYA_AMT_1_1_3,
                   BYAFILE.BYA_O2_1_1_3,
                   BYAFILE.BYA_INT_1_1_3,
                   BYAFILE.BYA_NO_1_1_4,
                   BYAFILE.BYA_AMT_1_1_4,
                   BYAFILE.BYA_O2_1_1_4,
                   BYAFILE.BYA_INT_1_1_4,
                   BYAFILE.BYA_NO_1_1_5,
                   BYAFILE.BYA_AMT_1_1_5,
                   BYAFILE.BYA_O2_1_1_5,
                   BYAFILE.BYA_INT_1_1_5,
                   BYAFILE.BYA_NO_1_1_6,
                   BYAFILE.BYA_AMT_1_1_6,
                   BYAFILE.BYA_O2_1_1_6,
                   BYAFILE.BYA_INT_1_1_6,
                   BYAFILE.BYA_NO_1_1_7,
                   BYAFILE.BYA_AMT_1_1_7,
                   BYAFILE.BYA_O2_1_1_7,
                   BYAFILE.BYA_INT_1_1_7,
                   BYAFILE.BYA_NO_1_1_8,
                   BYAFILE.BYA_AMT_1_1_8,
                   BYAFILE.BYA_O2_1_1_8,
                   BYAFILE.BYA_INT_1_1_8,
                   BYAFILE.BYA_NO_1_1_9,
                   BYAFILE.BYA_AMT_1_1_9,
                   BYAFILE.BYA_O2_1_1_9,
                   BYAFILE.BYA_INT_1_1_9,
                   BYAFILE.BYA_NO_1_1_10,
                   BYAFILE.BYA_AMT_1_1_10,
                   BYAFILE.BYA_O2_1_1_10,
                   BYAFILE.BYA_INT_1_1_10,
                   BYAFILE.BYA_NO_1_1_11,
                   BYAFILE.BYA_AMT_1_1_11,
                   BYAFILE.BYA_O2_1_1_11,
                   BYAFILE.BYA_INT_1_1_11,
                   BYAFILE.BYA_NO_1_2_1,
                   BYAFILE.BYA_AMT_1_2_1,
                   BYAFILE.BYA_O2_1_2_1,
                   BYAFILE.BYA_INT_1_2_1,
                   BYAFILE.BYA_NO_1_2_2,
                   BYAFILE.BYA_AMT_1_2_2,
                   BYAFILE.BYA_O2_1_2_2,
                   BYAFILE.BYA_INT_1_2_2,
                   BYAFILE.BYA_NO_1_2_3,
                   BYAFILE.BYA_AMT_1_2_3,
                   BYAFILE.BYA_O2_1_2_3,
                   BYAFILE.BYA_INT_1_2_3,
                   BYAFILE.BYA_NO_1_2_4,
                   BYAFILE.BYA_AMT_1_2_4,
                   BYAFILE.BYA_O2_1_2_4,
                   BYAFILE.BYA_INT_1_2_4,
                   BYAFILE.BYA_NO_1_2_5,
                   BYAFILE.BYA_AMT_1_2_5,
                   BYAFILE.BYA_O2_1_2_5,
                   BYAFILE.BYA_INT_1_2_5,
                   BYAFILE.BYA_NO_1_2_6,
                   BYAFILE.BYA_AMT_1_2_6,
                   BYAFILE.BYA_O2_1_2_6,
                   BYAFILE.BYA_INT_1_2_6,
                   BYAFILE.BYA_NO_1_2_7,
                   BYAFILE.BYA_AMT_1_2_7,
                   BYAFILE.BYA_O2_1_2_7,
                   BYAFILE.BYA_INT_1_2_7,
                   BYAFILE.BYA_NO_1_2_8,
                   BYAFILE.BYA_AMT_1_2_8,
                   BYAFILE.BYA_O2_1_2_8,
                   BYAFILE.BYA_INT_1_2_8,
                   BYAFILE.BYA_NO_1_2_9,
                   BYAFILE.BYA_AMT_1_2_9,
                   BYAFILE.BYA_O2_1_2_9,
                   BYAFILE.BYA_INT_1_2_9,
                   BYAFILE.BYA_NO_1_2_10,
                   BYAFILE.BYA_AMT_1_2_10,
                   BYAFILE.BYA_O2_1_2_10,
                   BYAFILE.BYA_INT_1_2_10,
                   BYAFILE.BYA_NO_1_2_11,
                   BYAFILE.BYA_AMT_1_2_11,
                   BYAFILE.BYA_O2_1_2_11,
                   BYAFILE.BYA_INT_1_2_11,
                   BYAFILE.BYA_NO_2_1_1,
                   BYAFILE.BYA_AMT_2_1_1,
                   BYAFILE.BYA_O2_2_1_1,
                   BYAFILE.BYA_INT_2_1_1,
                   BYAFILE.BYA_NO_2_1_2,
                   BYAFILE.BYA_AMT_2_1_2,
                   BYAFILE.BYA_O2_2_1_2,
                   BYAFILE.BYA_INT_2_1_2,
                   BYAFILE.BYA_NO_2_1_3,
                   BYAFILE.BYA_AMT_2_1_3,
                   BYAFILE.BYA_O2_2_1_3,
                   BYAFILE.BYA_INT_2_1_3,
                   BYAFILE.BYA_NO_2_1_4,
                   BYAFILE.BYA_AMT_2_1_4,
                   BYAFILE.BYA_O2_2_1_4,
                   BYAFILE.BYA_INT_2_1_4,
                   BYAFILE.BYA_NO_2_1_5,
                   BYAFILE.BYA_AMT_2_1_5,
                   BYAFILE.BYA_O2_2_1_5,
                   BYAFILE.BYA_INT_2_1_5,
                   BYAFILE.BYA_NO_2_1_6,
                   BYAFILE.BYA_AMT_2_1_6,
                   BYAFILE.BYA_O2_2_1_6,
                   BYAFILE.BYA_INT_2_1_6,
                   BYAFILE.BYA_NO_2_1_7,
                   BYAFILE.BYA_AMT_2_1_7,
                   BYAFILE.BYA_O2_2_1_7,
                   BYAFILE.BYA_INT_2_1_7,
                   BYAFILE.BYA_NO_2_1_8,
                   BYAFILE.BYA_AMT_2_1_8,
                   BYAFILE.BYA_O2_2_1_8,
                   BYAFILE.BYA_INT_2_1_8,
                   BYAFILE.BYA_NO_2_1_9,
                   BYAFILE.BYA_AMT_2_1_9,
                   BYAFILE.BYA_O2_2_1_9,
                   BYAFILE.BYA_INT_2_1_9,
                   BYAFILE.BYA_NO_2_1_10,
                   BYAFILE.BYA_AMT_2_1_10,
                   BYAFILE.BYA_O2_2_1_10,
                   BYAFILE.BYA_INT_2_1_10,
                   BYAFILE.BYA_NO_2_1_11,
                   BYAFILE.BYA_AMT_2_1_11,
                   BYAFILE.BYA_O2_2_1_11,
                   BYAFILE.BYA_INT_2_1_11,
                   BYAFILE.BYA_NO_2_2_1,
                   BYAFILE.BYA_AMT_2_2_1,
                   BYAFILE.BYA_O2_2_2_1,
                   BYAFILE.BYA_INT_2_2_1,
                   BYAFILE.BYA_NO_2_2_2,
                   BYAFILE.BYA_AMT_2_2_2,
                   BYAFILE.BYA_O2_2_2_2,
                   BYAFILE.BYA_INT_2_2_2,
                   BYAFILE.BYA_NO_2_2_3,
                   BYAFILE.BYA_AMT_2_2_3,
                   BYAFILE.BYA_O2_2_2_3,
                   BYAFILE.BYA_INT_2_2_3,
                   BYAFILE.BYA_NO_2_2_4,
                   BYAFILE.BYA_AMT_2_2_4,
                   BYAFILE.BYA_O2_2_2_4,
                   BYAFILE.BYA_INT_2_2_4,
                   BYAFILE.BYA_NO_2_2_5,
                   BYAFILE.BYA_AMT_2_2_5,
                   BYAFILE.BYA_O2_2_2_5,
                   BYAFILE.BYA_INT_2_2_5,
                   BYAFILE.BYA_NO_2_2_6,
                   BYAFILE.BYA_AMT_2_2_6,
                   BYAFILE.BYA_O2_2_2_6,
                   BYAFILE.BYA_INT_2_2_6,
                   BYAFILE.BYA_NO_2_2_7,
                   BYAFILE.BYA_AMT_2_2_7,
                   BYAFILE.BYA_O2_2_2_7,
                   BYAFILE.BYA_INT_2_2_7,
                   BYAFILE.BYA_NO_2_2_8,
                   BYAFILE.BYA_AMT_2_2_8,
                   BYAFILE.BYA_O2_2_2_8,
                   BYAFILE.BYA_INT_2_2_8,
                   BYAFILE.BYA_NO_2_2_9,
                   BYAFILE.BYA_AMT_2_2_9,
                   BYAFILE.BYA_O2_2_2_9,
                   BYAFILE.BYA_INT_2_2_9,
                   BYAFILE.BYA_NO_2_2_10,
                   BYAFILE.BYA_AMT_2_2_10,
                   BYAFILE.BYA_O2_2_2_10,
                   BYAFILE.BYA_INT_2_2_10,
                   BYAFILE.BYA_NO_2_2_11,
                   BYAFILE.BYA_AMT_2_2_11,
                   BYAFILE.BYA_O2_2_2_11,
                   BYAFILE.BYA_INT_2_2_11,
                   BYAFILE.BYA_NO_3_1_1,
                   BYAFILE.BYA_AMT_3_1_1,
                   BYAFILE.BYA_O2_3_1_1,
                   BYAFILE.BYA_INT_3_1_1,
                   BYAFILE.BYA_NO_3_1_2,
                   BYAFILE.BYA_AMT_3_1_2,
                   BYAFILE.BYA_O2_3_1_2,
                   BYAFILE.BYA_INT_3_1_2,
                   BYAFILE.BYA_NO_3_1_3,
                   BYAFILE.BYA_AMT_3_1_3,
                   BYAFILE.BYA_O2_3_1_3,
                   BYAFILE.BYA_INT_3_1_3,
                   BYAFILE.BYA_NO_3_1_4,
                   BYAFILE.BYA_AMT_3_1_4,
                   BYAFILE.BYA_O2_3_1_4,
                   BYAFILE.BYA_INT_3_1_4,
                   BYAFILE.BYA_NO_3_1_5,
                   BYAFILE.BYA_AMT_3_1_5,
                   BYAFILE.BYA_O2_3_1_5,
                   BYAFILE.BYA_INT_3_1_5,
                   BYAFILE.BYA_NO_3_1_6,
                   BYAFILE.BYA_AMT_3_1_6,
                   BYAFILE.BYA_O2_3_1_6,
                   BYAFILE.BYA_INT_3_1_6,
                   BYAFILE.BYA_NO_3_1_7,
                   BYAFILE.BYA_AMT_3_1_7,
                   BYAFILE.BYA_O2_3_1_7,
                   BYAFILE.BYA_INT_3_1_7,
                   BYAFILE.BYA_NO_3_1_8,
                   BYAFILE.BYA_AMT_3_1_8,
                   BYAFILE.BYA_O2_3_1_8,
                   BYAFILE.BYA_INT_3_1_8,
                   BYAFILE.BYA_NO_3_1_9,
                   BYAFILE.BYA_AMT_3_1_9,
                   BYAFILE.BYA_O2_3_1_9,
                   BYAFILE.BYA_INT_3_1_9,
                   BYAFILE.BYA_NO_3_1_10,
                   BYAFILE.BYA_AMT_3_1_10,
                   BYAFILE.BYA_O2_3_1_10,
                   BYAFILE.BYA_INT_3_1_10,
                   BYAFILE.BYA_NO_3_1_11,
                   BYAFILE.BYA_AMT_3_1_11,
                   BYAFILE.BYA_O2_3_1_11,
                   BYAFILE.BYA_INT_3_1_11,
                   BYAFILE.BYA_NO_3_2_1,
                   BYAFILE.BYA_AMT_3_2_1,
                   BYAFILE.BYA_O2_3_2_1,
                   BYAFILE.BYA_INT_3_2_1,
                   BYAFILE.BYA_NO_3_2_2,
                   BYAFILE.BYA_AMT_3_2_2,
                   BYAFILE.BYA_O2_3_2_2,
                   BYAFILE.BYA_INT_3_2_2,
                   BYAFILE.BYA_NO_3_2_3,
                   BYAFILE.BYA_AMT_3_2_3,
                   BYAFILE.BYA_O2_3_2_3,
                   BYAFILE.BYA_INT_3_2_3,
                   BYAFILE.BYA_NO_3_2_4,
                   BYAFILE.BYA_AMT_3_2_4,
                   BYAFILE.BYA_O2_3_2_4,
                   BYAFILE.BYA_INT_3_2_4,
                   BYAFILE.BYA_NO_3_2_5,
                   BYAFILE.BYA_AMT_3_2_5,
                   BYAFILE.BYA_O2_3_2_5,
                   BYAFILE.BYA_INT_3_2_5,
                   BYAFILE.BYA_NO_3_2_6,
                   BYAFILE.BYA_AMT_3_2_6,
                   BYAFILE.BYA_O2_3_2_6,
                   BYAFILE.BYA_INT_3_2_6,
                   BYAFILE.BYA_NO_3_2_7,
                   BYAFILE.BYA_AMT_3_2_7,
                   BYAFILE.BYA_O2_3_2_7,
                   BYAFILE.BYA_INT_3_2_7,
                   BYAFILE.BYA_NO_3_2_8,
                   BYAFILE.BYA_AMT_3_2_8,
                   BYAFILE.BYA_O2_3_2_8,
                   BYAFILE.BYA_INT_3_2_8,
                   BYAFILE.BYA_NO_3_2_9,
                   BYAFILE.BYA_AMT_3_2_9,
                   BYAFILE.BYA_O2_3_2_9,
                   BYAFILE.BYA_INT_3_2_9,
                   BYAFILE.BYA_NO_3_2_10,
                   BYAFILE.BYA_AMT_3_2_10,
                   BYAFILE.BYA_O2_3_2_10,
                   BYAFILE.BYA_INT_3_2_10,
                   BYAFILE.BYA_NO_3_2_11,
                   BYAFILE.BYA_AMT_3_2_11,
                   BYAFILE.BYA_O2_3_2_11,
                   BYAFILE.BYA_INT_3_2_11,
                   BYAFILE.BYA_NO_REPO_1_1_1,
                   BYAFILE.BYA_AMT_REPO_1_1_1,
                   BYAFILE.BYA_O2_REPO_1_1_1,
                   BYAFILE.BYA_INT_REPO_1_1_1,
                   BYAFILE.BYA_NO_REPO_1_1_2,
                   BYAFILE.BYA_AMT_REPO_1_1_2,
                   BYAFILE.BYA_O2_REPO_1_1_2,
                   BYAFILE.BYA_INT_REPO_1_1_2,
                   BYAFILE.BYA_NO_REPO_1_1_3,
                   BYAFILE.BYA_AMT_REPO_1_1_3,
                   BYAFILE.BYA_O2_REPO_1_1_3,
                   BYAFILE.BYA_INT_REPO_1_1_3,
                   BYAFILE.BYA_NO_REPO_1_1_4,
                   BYAFILE.BYA_AMT_REPO_1_1_4,
                   BYAFILE.BYA_O2_REPO_1_1_4,
                   BYAFILE.BYA_INT_REPO_1_1_4,
                   BYAFILE.BYA_NO_REPO_1_1_5,
                   BYAFILE.BYA_AMT_REPO_1_1_5,
                   BYAFILE.BYA_O2_REPO_1_1_5,
                   BYAFILE.BYA_INT_REPO_1_1_5,
                   BYAFILE.BYA_NO_REPO_1_2_1,
                   BYAFILE.BYA_AMT_REPO_1_2_1,
                   BYAFILE.BYA_O2_REPO_1_2_1,
                   BYAFILE.BYA_INT_REPO_1_2_1,
                   BYAFILE.BYA_NO_REPO_1_2_2,
                   BYAFILE.BYA_AMT_REPO_1_2_2,
                   BYAFILE.BYA_O2_REPO_1_2_2,
                   BYAFILE.BYA_INT_REPO_1_2_2,
                   BYAFILE.BYA_NO_REPO_1_2_3,
                   BYAFILE.BYA_AMT_REPO_1_2_3,
                   BYAFILE.BYA_O2_REPO_1_2_3,
                   BYAFILE.BYA_INT_REPO_1_2_3,
                   BYAFILE.BYA_NO_REPO_1_2_4,
                   BYAFILE.BYA_AMT_REPO_1_2_4,
                   BYAFILE.BYA_O2_REPO_1_2_4,
                   BYAFILE.BYA_INT_REPO_1_2_4,
                   BYAFILE.BYA_NO_REPO_1_2_5,
                   BYAFILE.BYA_AMT_REPO_1_2_5,
                   BYAFILE.BYA_O2_REPO_1_2_5,
                   BYAFILE.BYA_INT_REPO_1_2_5,
                   BYAFILE.BYA_NO_REPO_2_1_1,
                   BYAFILE.BYA_AMT_REPO_2_1_1,
                   BYAFILE.BYA_O2_REPO_2_1_1,
                   BYAFILE.BYA_INT_REPO_2_1_1,
                   BYAFILE.BYA_NO_REPO_2_1_2,
                   BYAFILE.BYA_AMT_REPO_2_1_2,
                   BYAFILE.BYA_O2_REPO_2_1_2,
                   BYAFILE.BYA_INT_REPO_2_1_2,
                   BYAFILE.BYA_NO_REPO_2_1_3,
                   BYAFILE.BYA_AMT_REPO_2_1_3,
                   BYAFILE.BYA_O2_REPO_2_1_3,
                   BYAFILE.BYA_INT_REPO_2_1_3,
                   BYAFILE.BYA_NO_REPO_2_1_4,
                   BYAFILE.BYA_AMT_REPO_2_1_4,
                   BYAFILE.BYA_O2_REPO_2_1_4,
                   BYAFILE.BYA_INT_REPO_2_1_4,
                   BYAFILE.BYA_NO_REPO_2_1_5,
                   BYAFILE.BYA_AMT_REPO_2_1_5,
                   BYAFILE.BYA_O2_REPO_2_1_5,
                   BYAFILE.BYA_INT_REPO_2_1_5,
                   BYAFILE.BYA_NO_REPO_2_2_1,
                   BYAFILE.BYA_AMT_REPO_2_2_1,
                   BYAFILE.BYA_O2_REPO_2_2_1,
                   BYAFILE.BYA_INT_REPO_2_2_1,
                   BYAFILE.BYA_NO_REPO_2_2_2,
                   BYAFILE.BYA_AMT_REPO_2_2_2,
                   BYAFILE.BYA_O2_REPO_2_2_2,
                   BYAFILE.BYA_INT_REPO_2_2_2,
                   BYAFILE.BYA_NO_REPO_2_2_3,
                   BYAFILE.BYA_AMT_REPO_2_2_3,
                   BYAFILE.BYA_O2_REPO_2_2_3,
                   BYAFILE.BYA_INT_REPO_2_2_3,
                   BYAFILE.BYA_NO_REPO_2_2_4,
                   BYAFILE.BYA_AMT_REPO_2_2_4,
                   BYAFILE.BYA_O2_REPO_2_2_4,
                   BYAFILE.BYA_INT_REPO_2_2_4,
                   BYAFILE.BYA_NO_REPO_2_2_5,
                   BYAFILE.BYA_AMT_REPO_2_2_5,
                   BYAFILE.BYA_O2_REPO_2_2_5,
                   BYAFILE.BYA_INT_REPO_2_2_5,
                   BYAFILE.BYA_NO_REPO_3_1_1,
                   BYAFILE.BYA_AMT_REPO_3_1_1,
                   BYAFILE.BYA_O2_REPO_3_1_1,
                   BYAFILE.BYA_INT_REPO_3_1_1,
                   BYAFILE.BYA_NO_REPO_3_1_2,
                   BYAFILE.BYA_AMT_REPO_3_1_2,
                   BYAFILE.BYA_O2_REPO_3_1_2,
                   BYAFILE.BYA_INT_REPO_3_1_2,
                   BYAFILE.BYA_NO_REPO_3_1_3,
                   BYAFILE.BYA_AMT_REPO_3_1_3,
                   BYAFILE.BYA_O2_REPO_3_1_3,
                   BYAFILE.BYA_INT_REPO_3_1_3,
                   BYAFILE.BYA_NO_REPO_3_1_4,
                   BYAFILE.BYA_AMT_REPO_3_1_4,
                   BYAFILE.BYA_O2_REPO_3_1_4,
                   BYAFILE.BYA_INT_REPO_3_1_4,
                   BYAFILE.BYA_NO_REPO_3_1_5,
                   BYAFILE.BYA_AMT_REPO_3_1_5,
                   BYAFILE.BYA_O2_REPO_3_1_5,
                   BYAFILE.BYA_INT_REPO_3_1_5,
                   BYAFILE.BYA_NO_REPO_3_2_1,
                   BYAFILE.BYA_AMT_REPO_3_2_1,
                   BYAFILE.BYA_O2_REPO_3_2_1,
                   BYAFILE.BYA_INT_REPO_3_2_1,
                   BYAFILE.BYA_NO_REPO_3_2_2,
                   BYAFILE.BYA_AMT_REPO_3_2_2,
                   BYAFILE.BYA_O2_REPO_3_2_2,
                   BYAFILE.BYA_INT_REPO_3_2_2,
                   BYAFILE.BYA_NO_REPO_3_2_3,
                   BYAFILE.BYA_AMT_REPO_3_2_3,
                   BYAFILE.BYA_O2_REPO_3_2_3,
                   BYAFILE.BYA_INT_REPO_3_2_3,
                   BYAFILE.BYA_NO_REPO_3_2_4,
                   BYAFILE.BYA_AMT_REPO_3_2_4,
                   BYAFILE.BYA_O2_REPO_3_2_4,
                   BYAFILE.BYA_INT_REPO_3_2_4,
                   BYAFILE.BYA_NO_REPO_3_2_5,
                   BYAFILE.BYA_AMT_REPO_3_2_5,
                   BYAFILE.BYA_O2_REPO_3_2_5,
                   BYAFILE.BYA_INT_REPO_3_2_5,
                   BYAFILE.BYA_NO_PL_1_1_1,
                   BYAFILE.BYA_AMT_PL_1_1_1,
                   BYAFILE.BYA_O2_PL_1_1_1,
                   BYAFILE.BYA_INT_PL_1_1_1,
                   BYAFILE.BYA_NO_PL_1_1_2,
                   BYAFILE.BYA_AMT_PL_1_1_2,
                   BYAFILE.BYA_O2_PL_1_1_2,
                   BYAFILE.BYA_INT_PL_1_1_2,
                   BYAFILE.BYA_NO_PL_1_1_3,
                   BYAFILE.BYA_AMT_PL_1_1_3,
                   BYAFILE.BYA_O2_PL_1_1_3,
                   BYAFILE.BYA_INT_PL_1_1_3,
                   BYAFILE.BYA_NO_PL_1_1_4,
                   BYAFILE.BYA_AMT_PL_1_1_4,
                   BYAFILE.BYA_O2_PL_1_1_4,
                   BYAFILE.BYA_INT_PL_1_1_4,
                   BYAFILE.BYA_NO_PL_1_1_5,
                   BYAFILE.BYA_AMT_PL_1_1_5,
                   BYAFILE.BYA_O2_PL_1_1_5,
                   BYAFILE.BYA_INT_PL_1_1_5,
                   BYAFILE.BYA_NO_PL_1_1_6,
                   BYAFILE.BYA_AMT_PL_1_1_6,
                   BYAFILE.BYA_O2_PL_1_1_6,
                   BYAFILE.BYA_INT_PL_1_1_6,
                   BYAFILE.BYA_NO_PL_1_1_7,
                   BYAFILE.BYA_AMT_PL_1_1_7,
                   BYAFILE.BYA_O2_PL_1_1_7,
                   BYAFILE.BYA_INT_PL_1_1_7,
                   BYAFILE.BYA_NO_PL_1_1_8,
                   BYAFILE.BYA_AMT_PL_1_1_8,
                   BYAFILE.BYA_O2_PL_1_1_8,
                   BYAFILE.BYA_INT_PL_1_1_8,
                   BYAFILE.BYA_NO_PL_1_1_9,
                   BYAFILE.BYA_AMT_PL_1_1_9,
                   BYAFILE.BYA_O2_PL_1_1_9,
                   BYAFILE.BYA_INT_PL_1_1_9,
                   BYAFILE.BYA_NO_PL_1_1_10,
                   BYAFILE.BYA_AMT_PL_1_1_10,
                   BYAFILE.BYA_O2_PL_1_1_10,
                   BYAFILE.BYA_INT_PL_1_1_10,
                   BYAFILE.BYA_NO_PL_1_2_1,
                   BYAFILE.BYA_AMT_PL_1_2_1,
                   BYAFILE.BYA_O2_PL_1_2_1,
                   BYAFILE.BYA_INT_PL_1_2_1,
                   BYAFILE.BYA_NO_PL_1_2_2,
                   BYAFILE.BYA_AMT_PL_1_2_2,
                   BYAFILE.BYA_O2_PL_1_2_2,
                   BYAFILE.BYA_INT_PL_1_2_2,
                   BYAFILE.BYA_NO_PL_1_2_3,
                   BYAFILE.BYA_AMT_PL_1_2_3,
                   BYAFILE.BYA_O2_PL_1_2_3,
                   BYAFILE.BYA_INT_PL_1_2_3,
                   BYAFILE.BYA_NO_PL_1_2_4,
                   BYAFILE.BYA_AMT_PL_1_2_4,
                   BYAFILE.BYA_O2_PL_1_2_4,
                   BYAFILE.BYA_INT_PL_1_2_4,
                   BYAFILE.BYA_NO_PL_1_2_5,
                   BYAFILE.BYA_AMT_PL_1_2_5,
                   BYAFILE.BYA_O2_PL_1_2_5,
                   BYAFILE.BYA_INT_PL_1_2_5,
                   BYAFILE.BYA_NO_PL_1_2_6,
                   BYAFILE.BYA_AMT_PL_1_2_6,
                   BYAFILE.BYA_O2_PL_1_2_6,
                   BYAFILE.BYA_INT_PL_1_2_6,
                   BYAFILE.BYA_NO_PL_1_2_7,
                   BYAFILE.BYA_AMT_PL_1_2_7,
                   BYAFILE.BYA_O2_PL_1_2_7,
                   BYAFILE.BYA_INT_PL_1_2_7,
                   BYAFILE.BYA_NO_PL_1_2_8,
                   BYAFILE.BYA_AMT_PL_1_2_8,
                   BYAFILE.BYA_O2_PL_1_2_8,
                   BYAFILE.BYA_INT_PL_1_2_8,
                   BYAFILE.BYA_NO_PL_1_2_9,
                   BYAFILE.BYA_AMT_PL_1_2_9,
                   BYAFILE.BYA_O2_PL_1_2_9,
                   BYAFILE.BYA_INT_PL_1_2_9,
                   BYAFILE.BYA_NO_PL_1_2_10,
                   BYAFILE.BYA_AMT_PL_1_2_10,
                   BYAFILE.BYA_O2_PL_1_2_10,
                   BYAFILE.BYA_INT_PL_1_2_10,
                   BYAFILE.BYA_NO_PL_2_1_1,
                   BYAFILE.BYA_AMT_PL_2_1_1,
                   BYAFILE.BYA_O2_PL_2_1_1,
                   BYAFILE.BYA_INT_PL_2_1_1,
                   BYAFILE.BYA_NO_PL_2_1_2,
                   BYAFILE.BYA_AMT_PL_2_1_2,
                   BYAFILE.BYA_O2_PL_2_1_2,
                   BYAFILE.BYA_INT_PL_2_1_2,
                   BYAFILE.BYA_NO_PL_2_1_3,
                   BYAFILE.BYA_AMT_PL_2_1_3,
                   BYAFILE.BYA_O2_PL_2_1_3,
                   BYAFILE.BYA_INT_PL_2_1_3,
                   BYAFILE.BYA_NO_PL_2_1_4,
                   BYAFILE.BYA_AMT_PL_2_1_4,
                   BYAFILE.BYA_O2_PL_2_1_4,
                   BYAFILE.BYA_INT_PL_2_1_4,
                   BYAFILE.BYA_NO_PL_2_1_5,
                   BYAFILE.BYA_AMT_PL_2_1_5,
                   BYAFILE.BYA_O2_PL_2_1_5,
                   BYAFILE.BYA_INT_PL_2_1_5,
                   BYAFILE.BYA_NO_PL_2_1_6,
                   BYAFILE.BYA_AMT_PL_2_1_6,
                   BYAFILE.BYA_O2_PL_2_1_6,
                   BYAFILE.BYA_INT_PL_2_1_6,
                   BYAFILE.BYA_NO_PL_2_1_7,
                   BYAFILE.BYA_AMT_PL_2_1_7,
                   BYAFILE.BYA_O2_PL_2_1_7,
                   BYAFILE.BYA_INT_PL_2_1_7,
                   BYAFILE.BYA_NO_PL_2_1_8,
                   BYAFILE.BYA_AMT_PL_2_1_8,
                   BYAFILE.BYA_O2_PL_2_1_8,
                   BYAFILE.BYA_INT_PL_2_1_8,
                   BYAFILE.BYA_NO_PL_2_1_9,
                   BYAFILE.BYA_AMT_PL_2_1_9,
                   BYAFILE.BYA_O2_PL_2_1_9,
                   BYAFILE.BYA_INT_PL_2_1_9,
                   BYAFILE.BYA_NO_PL_2_1_10,
                   BYAFILE.BYA_AMT_PL_2_1_10,
                   BYAFILE.BYA_O2_PL_2_1_10,
                   BYAFILE.BYA_INT_PL_2_1_10,
                   BYAFILE.BYA_NO_PL_2_2_1,
                   BYAFILE.BYA_AMT_PL_2_2_1,
                   BYAFILE.BYA_O2_PL_2_2_1,
                   BYAFILE.BYA_INT_PL_2_2_1,
                   BYAFILE.BYA_NO_PL_2_2_2,
                   BYAFILE.BYA_AMT_PL_2_2_2,
                   BYAFILE.BYA_O2_PL_2_2_2,
                   BYAFILE.BYA_INT_PL_2_2_2,
                   BYAFILE.BYA_NO_PL_2_2_3,
                   BYAFILE.BYA_AMT_PL_2_2_3,
                   BYAFILE.BYA_O2_PL_2_2_3,
                   BYAFILE.BYA_INT_PL_2_2_3,
                   BYAFILE.BYA_NO_PL_2_2_4,
                   BYAFILE.BYA_AMT_PL_2_2_4,
                   BYAFILE.BYA_O2_PL_2_2_4,
                   BYAFILE.BYA_INT_PL_2_2_4,
                   BYAFILE.BYA_NO_PL_2_2_5,
                   BYAFILE.BYA_AMT_PL_2_2_5,
                   BYAFILE.BYA_O2_PL_2_2_5,
                   BYAFILE.BYA_INT_PL_2_2_5,
                   BYAFILE.BYA_NO_PL_2_2_6,
                   BYAFILE.BYA_AMT_PL_2_2_6,
                   BYAFILE.BYA_O2_PL_2_2_6,
                   BYAFILE.BYA_INT_PL_2_2_6,
                   BYAFILE.BYA_NO_PL_2_2_7,
                   BYAFILE.BYA_AMT_PL_2_2_7,
                   BYAFILE.BYA_O2_PL_2_2_7,
                   BYAFILE.BYA_INT_PL_2_2_7,
                   BYAFILE.BYA_NO_PL_2_2_8,
                   BYAFILE.BYA_AMT_PL_2_2_8,
                   BYAFILE.BYA_O2_PL_2_2_8,
                   BYAFILE.BYA_INT_PL_2_2_8,
                   BYAFILE.BYA_NO_PL_2_2_9,
                   BYAFILE.BYA_AMT_PL_2_2_9,
                   BYAFILE.BYA_O2_PL_2_2_9,
                   BYAFILE.BYA_INT_PL_2_2_9,
                   BYAFILE.BYA_NO_PL_2_2_10,
                   BYAFILE.BYA_AMT_PL_2_2_10,
                   BYAFILE.BYA_O2_PL_2_2_10,
                   BYAFILE.BYA_INT_PL_2_2_10,
                   BYAFILE.BYA_NO_PL_3_1_1,
                   BYAFILE.BYA_AMT_PL_3_1_1,
                   BYAFILE.BYA_O2_PL_3_1_1,
                   BYAFILE.BYA_INT_PL_3_1_1,
                   BYAFILE.BYA_NO_PL_3_1_2,
                   BYAFILE.BYA_AMT_PL_3_1_2,
                   BYAFILE.BYA_O2_PL_3_1_2,
                   BYAFILE.BYA_INT_PL_3_1_2,
                   BYAFILE.BYA_NO_PL_3_1_3,
                   BYAFILE.BYA_AMT_PL_3_1_3,
                   BYAFILE.BYA_O2_PL_3_1_3,
                   BYAFILE.BYA_INT_PL_3_1_3,
                   BYAFILE.BYA_NO_PL_3_1_4,
                   BYAFILE.BYA_AMT_PL_3_1_4,
                   BYAFILE.BYA_O2_PL_3_1_4,
                   BYAFILE.BYA_INT_PL_3_1_4,
                   BYAFILE.BYA_NO_PL_3_1_5,
                   BYAFILE.BYA_AMT_PL_3_1_5,
                   BYAFILE.BYA_O2_PL_3_1_5,
                   BYAFILE.BYA_INT_PL_3_1_5,
                   BYAFILE.BYA_NO_PL_3_1_6,
                   BYAFILE.BYA_AMT_PL_3_1_6,
                   BYAFILE.BYA_O2_PL_3_1_6,
                   BYAFILE.BYA_INT_PL_3_1_6,
                   BYAFILE.BYA_NO_PL_3_1_7,
                   BYAFILE.BYA_AMT_PL_3_1_7,
                   BYAFILE.BYA_O2_PL_3_1_7,
                   BYAFILE.BYA_INT_PL_3_1_7,
                   BYAFILE.BYA_NO_PL_3_1_8,
                   BYAFILE.BYA_AMT_PL_3_1_8,
                   BYAFILE.BYA_O2_PL_3_1_8,
                   BYAFILE.BYA_INT_PL_3_1_8,
                   BYAFILE.BYA_NO_PL_3_1_9,
                   BYAFILE.BYA_AMT_PL_3_1_9,
                   BYAFILE.BYA_O2_PL_3_1_9,
                   BYAFILE.BYA_INT_PL_3_1_9,
                   BYAFILE.BYA_NO_PL_3_1_10,
                   BYAFILE.BYA_AMT_PL_3_1_10,
                   BYAFILE.BYA_O2_PL_3_1_10,
                   BYAFILE.BYA_INT_PL_3_1_10,
                   BYAFILE.BYA_NO_PL_3_2_1,
                   BYAFILE.BYA_AMT_PL_3_2_1,
                   BYAFILE.BYA_O2_PL_3_2_1,
                   BYAFILE.BYA_INT_PL_3_2_1,
                   BYAFILE.BYA_NO_PL_3_2_2,
                   BYAFILE.BYA_AMT_PL_3_2_2,
                   BYAFILE.BYA_O2_PL_3_2_2,
                   BYAFILE.BYA_INT_PL_3_2_2,
                   BYAFILE.BYA_NO_PL_3_2_3,
                   BYAFILE.BYA_AMT_PL_3_2_3,
                   BYAFILE.BYA_O2_PL_3_2_3,
                   BYAFILE.BYA_INT_PL_3_2_3,
                   BYAFILE.BYA_NO_PL_3_2_4,
                   BYAFILE.BYA_AMT_PL_3_2_4,
                   BYAFILE.BYA_O2_PL_3_2_4,
                   BYAFILE.BYA_INT_PL_3_2_4,
                   BYAFILE.BYA_NO_PL_3_2_5,
                   BYAFILE.BYA_AMT_PL_3_2_5,
                   BYAFILE.BYA_O2_PL_3_2_5,
                   BYAFILE.BYA_INT_PL_3_2_5,
                   BYAFILE.BYA_NO_PL_3_2_6,
                   BYAFILE.BYA_AMT_PL_3_2_6,
                   BYAFILE.BYA_O2_PL_3_2_6,
                   BYAFILE.BYA_INT_PL_3_2_6,
                   BYAFILE.BYA_NO_PL_3_2_7,
                   BYAFILE.BYA_AMT_PL_3_2_7,
                   BYAFILE.BYA_O2_PL_3_2_7,
                   BYAFILE.BYA_INT_PL_3_2_7,
                   BYAFILE.BYA_NO_PL_3_2_8,
                   BYAFILE.BYA_AMT_PL_3_2_8,
                   BYAFILE.BYA_O2_PL_3_2_8,
                   BYAFILE.BYA_INT_PL_3_2_8,
                   BYAFILE.BYA_NO_PL_3_2_9,
                   BYAFILE.BYA_AMT_PL_3_2_9,
                   BYAFILE.BYA_O2_PL_3_2_9,
                   BYAFILE.BYA_INT_PL_3_2_9,
                   BYAFILE.BYA_NO_PL_3_2_10,
                   BYAFILE.BYA_AMT_PL_3_2_10,
                   BYAFILE.BYA_O2_PL_3_2_10,
                   BYAFILE.BYA_INT_PL_3_2_10,
                   BYAFILE.BYA_DLRVPRIN_1_1_1,
                   BYAFILE.BYA_DLRVPRIN_1_1_2,
                   BYAFILE.BYA_DLRVPRIN_1_1_3,
                   BYAFILE.BYA_DLRVCHGS_1_1_1,
                   BYAFILE.BYA_DLRVCHGS_1_1_2,
                   BYAFILE.BYA_DLRVCHGS_1_1_3,
                   BYAFILE.BYA_DLPARVHELD_1_1,
                   BYAFILE.BYA_DLRVPRIN_1_2_1,
                   BYAFILE.BYA_DLRVPRIN_1_2_2,
                   BYAFILE.BYA_DLRVPRIN_1_2_3,
                   BYAFILE.BYA_DLRVCHGS_1_2_1,
                   BYAFILE.BYA_DLRVCHGS_1_2_2,
                   BYAFILE.BYA_DLRVCHGS_1_2_3,
                   BYAFILE.BYA_DLPARVHELD_1_2,
                   BYAFILE.BYA_DLRVPRIN_2_1_1,
                   BYAFILE.BYA_DLRVPRIN_2_1_2,
                   BYAFILE.BYA_DLRVPRIN_2_1_3,
                   BYAFILE.BYA_DLRVCHGS_2_1_1,
                   BYAFILE.BYA_DLRVCHGS_2_1_2,
                   BYAFILE.BYA_DLRVCHGS_2_1_3,
                   BYAFILE.BYA_DLPARVHELD_2_1,
                   BYAFILE.BYA_DLRVPRIN_2_2_1,
                   BYAFILE.BYA_DLRVPRIN_2_2_2,
                   BYAFILE.BYA_DLRVPRIN_2_2_3,
                   BYAFILE.BYA_DLRVCHGS_2_2_1,
                   BYAFILE.BYA_DLRVCHGS_2_2_2,
                   BYAFILE.BYA_DLRVCHGS_2_2_3,
                   BYAFILE.BYA_DLPARVHELD_2_2,
                   BYAFILE.BYA_DLRVPRIN_3_1_1,
                   BYAFILE.BYA_DLRVPRIN_3_1_2,
                   BYAFILE.BYA_DLRVPRIN_3_1_3,
                   BYAFILE.BYA_DLRVCHGS_3_1_1,
                   BYAFILE.BYA_DLRVCHGS_3_1_2,
                   BYAFILE.BYA_DLRVCHGS_3_1_3,
                   BYAFILE.BYA_DLPARVHELD_3_1,
                   BYAFILE.BYA_DLRVPRIN_3_2_1,
                   BYAFILE.BYA_DLRVPRIN_3_2_2,
                   BYAFILE.BYA_DLRVPRIN_3_2_3,
                   BYAFILE.BYA_DLRVCHGS_3_2_1,
                   BYAFILE.BYA_DLRVCHGS_3_2_2,
                   BYAFILE.BYA_DLRVCHGS_3_2_3,
                   BYAFILE.BYA_DLPARVHELD_3_2
            INTO :QBYA-REC
            FROM DBO.BYAFILE
            WHERE BYAFILE.BYA_DATE_CCYYMM = :QBYA-DATE-CCYYMM
              AND BYAFILE.BYA_BRNO = :QBYA-BRNO 
              AND BYAFILE.BYA_CLASS = :QBYA-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BYA-FIELDS.

      *-----------------------------------------------------------------
       READ-BYA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BYA-SELECT-PATH TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           INITIALIZE QBYA-REC.

           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BYA1_CURSOR INTO :QBYA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BYA-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BYA1-FILE.
           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BYA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BYA1-CURSOR-STAT.

      *=================================================================
       LOAD-BYB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BY-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BYB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BYB-ALL-MACHINE
              MOVE BYB-ALL-PATH            TO BYB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BYB-ALL-BASE
              MOVE FILPATH-MACHINE         TO BYB-ALL-MACHINE
              MOVE BYB-ALL-PATH            TO BYB-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BYBFILE NOT = "S" )
              MOVE BYB-PATH-BEG            TO BYB-SELECT-PATH-BEG
              MOVE BYB-PATH-FILE           TO BYB-SELECT-PATH-FILE
           ELSE
              MOVE BYB-PATH-SQL            TO BYB-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BYB1-FILE.
           PERFORM LOAD-BYB1-FILE.
           PERFORM OPEN-IT.
           MOVE BYB-SELECT-PATH TO E-FILE.

      *-----------------------------------------------------------------
       READ-BYB1-FILE.
           PERFORM READ-IT.
           MOVE BYB-SELECT-PATH TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.

           MOVE BYB-DATE-CCYYMM      TO QBYB-DATE-CCYYMM.
           MOVE BYB-BRNO             TO QBYB-BRNO.
           MOVE BYB-CLASS            TO QBYB-CLASS.

           EXEC SQL
            SELECT BYBFILE.BYB_DATE_CCYYMM,
                   BYBFILE.BYB_BRNO,
                   BYBFILE.BYB_CLASS,
                   BYBFILE.BYB_PL_PRINCOL_1_1,
                   BYBFILE.BYB_PL_INTCOL_1_1,
                   BYBFILE.BYB_PL_LATECOL_1_1,
                   BYBFILE.BYB_PL_O2COL_1_1,
                   BYBFILE.BYB_PL_PRINCOL_1_2,
                   BYBFILE.BYB_PL_INTCOL_1_2,
                   BYBFILE.BYB_PL_LATECOL_1_2,
                   BYBFILE.BYB_PL_O2COL_1_2,
                   BYBFILE.BYB_PL_PRINCOL_1_3,
                   BYBFILE.BYB_PL_INTCOL_1_3,
                   BYBFILE.BYB_PL_LATECOL_1_3,
                   BYBFILE.BYB_PL_O2COL_1_3,
                   BYBFILE.BYB_PL_PRINCOL_1_4,
                   BYBFILE.BYB_PL_INTCOL_1_4,
                   BYBFILE.BYB_PL_LATECOL_1_4,
                   BYBFILE.BYB_PL_O2COL_1_4,
                   BYBFILE.BYB_PL_PRINCOL_1_5,
                   BYBFILE.BYB_PL_INTCOL_1_5,
                   BYBFILE.BYB_PL_LATECOL_1_5,
                   BYBFILE.BYB_PL_O2COL_1_5,
                   BYBFILE.BYB_AMT_SOLD_1,
                   BYBFILE.BYB_AMT_TRANS_1,
                   BYBFILE.BYB_AMT_PURCH_1,
                   BYBFILE.BYB_NO_SOLD_1,
                   BYBFILE.BYB_NO_TRANS_1,
                   BYBFILE.BYB_NO_PURCH_1,
                   BYBFILE.BYB_OTHCOL_1,
                   BYBFILE.BYB_OT2COL_1,
                   BYBFILE.BYB_INTCOL_1,
                   BYBFILE.BYB_LATECOL_1,
                   BYBFILE.BYB_PRINCOL_1,
                   BYBFILE.BYB_DEFCOL_1,
                   BYBFILE.BYB_RNPRIN_1,
                   BYBFILE.BYB_INTREB_1,
                   BYBFILE.BYB_CLREB1_1,
                   BYBFILE.BYB_CLREB2_1,
                   BYBFILE.BYB_CLREB3_1,
                   BYBFILE.BYB_CLREB4_1,
                   BYBFILE.BYB_AHREB_1,
                   BYBFILE.BYB_PPREB_1,
                   BYBFILE.BYB_O1REB_1,
                   BYBFILE.BYB_O2REB_1,
                   BYBFILE.BYB_O3REB_1,
                   BYBFILE.BYB_O4REB_1,
                   BYBFILE.BYB_O5REB_1,
                   BYBFILE.BYB_O6REB_1,
                   BYBFILE.BYB_O7REB_1,
                   BYBFILE.BYB_SCREB_1,
                   BYBFILE.BYB_MFREB_1,
                   BYBFILE.BYB_DFREB_1,
                   BYBFILE.BYB_UN7_REB_1,
                   BYBFILE.BYB_UN8_REB_1,
                   BYBFILE.BYB_UN9_REB_1,
                   BYBFILE.BYB_UN10_REB_1,
                   BYBFILE.BYB_PART_CHGOFF_1,
                   BYBFILE.BYB_ALLOTMENTS_1,
                   BYBFILE.BYB_INS_NO_ELIG_1_1,
                   BYBFILE.BYB_INS_NO_WROTE_1_1,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_1,
                   BYBFILE.BYB_INS_AMT_WROTE_1_1,
                   BYBFILE.BYB_INS_NO_ELIG_1_2,
                   BYBFILE.BYB_INS_NO_WROTE_1_2,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_2,
                   BYBFILE.BYB_INS_AMT_WROTE_1_2,
                   BYBFILE.BYB_INS_NO_ELIG_1_3,
                   BYBFILE.BYB_INS_NO_WROTE_1_3,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_3,
                   BYBFILE.BYB_INS_AMT_WROTE_1_3,
                   BYBFILE.BYB_INS_NO_ELIG_1_4,
                   BYBFILE.BYB_INS_NO_WROTE_1_4,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_4,
                   BYBFILE.BYB_INS_AMT_WROTE_1_4,
                   BYBFILE.BYB_INS_NO_ELIG_1_5,
                   BYBFILE.BYB_INS_NO_WROTE_1_5,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_5,
                   BYBFILE.BYB_INS_AMT_WROTE_1_5,
                   BYBFILE.BYB_INS_NO_ELIG_1_6,
                   BYBFILE.BYB_INS_NO_WROTE_1_6,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_6,
                   BYBFILE.BYB_INS_AMT_WROTE_1_6,
                   BYBFILE.BYB_INS_NO_ELIG_1_7,
                   BYBFILE.BYB_INS_NO_WROTE_1_7,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_7,
                   BYBFILE.BYB_INS_AMT_WROTE_1_7,
                   BYBFILE.BYB_INS_NO_ELIG_1_8,
                   BYBFILE.BYB_INS_NO_WROTE_1_8,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_8,
                   BYBFILE.BYB_INS_AMT_WROTE_1_8,
                   BYBFILE.BYB_INS_NO_ELIG_1_9,
                   BYBFILE.BYB_INS_NO_WROTE_1_9,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_9,
                   BYBFILE.BYB_INS_AMT_WROTE_1_9,
                   BYBFILE.BYB_INS_NO_ELIG_1_10,
                   BYBFILE.BYB_INS_NO_WROTE_1_10,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_10,
                   BYBFILE.BYB_INS_AMT_WROTE_1_10,
                   BYBFILE.BYB_FEE_NO_ELIG_1_1,
                   BYBFILE.BYB_FEE_NO_WROTE_1_1,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_1_1,
                   BYBFILE.BYB_FEE_AMT_WROTE_1_1,
                   BYBFILE.BYB_FEE_NO_ELIG_1_2,
                   BYBFILE.BYB_FEE_NO_WROTE_1_2,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_1_2,
                   BYBFILE.BYB_FEE_AMT_WROTE_1_2,
                   BYBFILE.BYB_FEE_NO_ELIG_1_3,
                   BYBFILE.BYB_FEE_NO_WROTE_1_3,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_1_3,
                   BYBFILE.BYB_FEE_AMT_WROTE_1_3,
                   BYBFILE.BYB_OTH_NO_COL_1,
                   BYBFILE.BYB_OT2_NO_COL_1,
                   BYBFILE.BYB_INT_NO_COL_1,
                   BYBFILE.BYB_LATE_NO_COL_1,
                   BYBFILE.BYB_OTH_NO_WAIVED_1,
                   BYBFILE.BYB_OT2_NO_WAIVED_1,
                   BYBFILE.BYB_INT_NO_WAIVED_1,
                   BYBFILE.BYB_LATE_NO_WAIVED_1,
                   BYBFILE.BYB_OTH_AMT_WAIVED_1,
                   BYBFILE.BYB_OT2_AMT_WAIVED_1,
                   BYBFILE.BYB_INT_AMT_WAIVED_1,
                   BYBFILE.BYB_LATE_AMT_WAIVED_1,
                   BYBFILE.BYB_PL_PRINCOL_2_1,
                   BYBFILE.BYB_PL_INTCOL_2_1,
                   BYBFILE.BYB_PL_LATECOL_2_1,
                   BYBFILE.BYB_PL_O2COL_2_1,
                   BYBFILE.BYB_PL_PRINCOL_2_2,
                   BYBFILE.BYB_PL_INTCOL_2_2,
                   BYBFILE.BYB_PL_LATECOL_2_2,
                   BYBFILE.BYB_PL_O2COL_2_2,
                   BYBFILE.BYB_PL_PRINCOL_2_3,
                   BYBFILE.BYB_PL_INTCOL_2_3,
                   BYBFILE.BYB_PL_LATECOL_2_3,
                   BYBFILE.BYB_PL_O2COL_2_3,
                   BYBFILE.BYB_PL_PRINCOL_2_4,
                   BYBFILE.BYB_PL_INTCOL_2_4,
                   BYBFILE.BYB_PL_LATECOL_2_4,
                   BYBFILE.BYB_PL_O2COL_2_4,
                   BYBFILE.BYB_PL_PRINCOL_2_5,
                   BYBFILE.BYB_PL_INTCOL_2_5,
                   BYBFILE.BYB_PL_LATECOL_2_5,
                   BYBFILE.BYB_PL_O2COL_2_5,
                   BYBFILE.BYB_AMT_SOLD_2,
                   BYBFILE.BYB_AMT_TRANS_2,
                   BYBFILE.BYB_AMT_PURCH_2,
                   BYBFILE.BYB_NO_SOLD_2,
                   BYBFILE.BYB_NO_TRANS_2,
                   BYBFILE.BYB_NO_PURCH_2,
                   BYBFILE.BYB_OTHCOL_2,
                   BYBFILE.BYB_OT2COL_2,
                   BYBFILE.BYB_INTCOL_2,
                   BYBFILE.BYB_LATECOL_2,
                   BYBFILE.BYB_PRINCOL_2,
                   BYBFILE.BYB_DEFCOL_2,
                   BYBFILE.BYB_RNPRIN_2,
                   BYBFILE.BYB_INTREB_2,
                   BYBFILE.BYB_CLREB1_2,
                   BYBFILE.BYB_CLREB2_2,
                   BYBFILE.BYB_CLREB3_2,
                   BYBFILE.BYB_CLREB4_2,
                   BYBFILE.BYB_AHREB_2,
                   BYBFILE.BYB_PPREB_2,
                   BYBFILE.BYB_O1REB_2,
                   BYBFILE.BYB_O2REB_2,
                   BYBFILE.BYB_O3REB_2,
                   BYBFILE.BYB_O4REB_2,
                   BYBFILE.BYB_O5REB_2,
                   BYBFILE.BYB_O6REB_2,
                   BYBFILE.BYB_O7REB_2,
                   BYBFILE.BYB_SCREB_2,
                   BYBFILE.BYB_MFREB_2,
                   BYBFILE.BYB_DFREB_2,
                   BYBFILE.BYB_UN7_REB_2,
                   BYBFILE.BYB_UN8_REB_2,
                   BYBFILE.BYB_UN9_REB_2,
                   BYBFILE.BYB_UN10_REB_2,
                   BYBFILE.BYB_PART_CHGOFF_2,
                   BYBFILE.BYB_ALLOTMENTS_2,
                   BYBFILE.BYB_INS_NO_ELIG_2_1,
                   BYBFILE.BYB_INS_NO_WROTE_2_1,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_1,
                   BYBFILE.BYB_INS_AMT_WROTE_2_1,
                   BYBFILE.BYB_INS_NO_ELIG_2_2,
                   BYBFILE.BYB_INS_NO_WROTE_2_2,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_2,
                   BYBFILE.BYB_INS_AMT_WROTE_2_2,
                   BYBFILE.BYB_INS_NO_ELIG_2_3,
                   BYBFILE.BYB_INS_NO_WROTE_2_3,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_3,
                   BYBFILE.BYB_INS_AMT_WROTE_2_3,
                   BYBFILE.BYB_INS_NO_ELIG_2_4,
                   BYBFILE.BYB_INS_NO_WROTE_2_4,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_4,
                   BYBFILE.BYB_INS_AMT_WROTE_2_4,
                   BYBFILE.BYB_INS_NO_ELIG_2_5,
                   BYBFILE.BYB_INS_NO_WROTE_2_5,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_5,
                   BYBFILE.BYB_INS_AMT_WROTE_2_5,
                   BYBFILE.BYB_INS_NO_ELIG_2_6,
                   BYBFILE.BYB_INS_NO_WROTE_2_6,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_6,
                   BYBFILE.BYB_INS_AMT_WROTE_2_6,
                   BYBFILE.BYB_INS_NO_ELIG_2_7,
                   BYBFILE.BYB_INS_NO_WROTE_2_7,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_7,
                   BYBFILE.BYB_INS_AMT_WROTE_2_7,
                   BYBFILE.BYB_INS_NO_ELIG_2_8,
                   BYBFILE.BYB_INS_NO_WROTE_2_8,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_8,
                   BYBFILE.BYB_INS_AMT_WROTE_2_8,
                   BYBFILE.BYB_INS_NO_ELIG_2_9,
                   BYBFILE.BYB_INS_NO_WROTE_2_9,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_9,
                   BYBFILE.BYB_INS_AMT_WROTE_2_9,
                   BYBFILE.BYB_INS_NO_ELIG_2_10,
                   BYBFILE.BYB_INS_NO_WROTE_2_10,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_10,
                   BYBFILE.BYB_INS_AMT_WROTE_2_10,
                   BYBFILE.BYB_FEE_NO_ELIG_2_1,
                   BYBFILE.BYB_FEE_NO_WROTE_2_1,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_2_1,
                   BYBFILE.BYB_FEE_AMT_WROTE_2_1,
                   BYBFILE.BYB_FEE_NO_ELIG_2_2,
                   BYBFILE.BYB_FEE_NO_WROTE_2_2,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_2_2,
                   BYBFILE.BYB_FEE_AMT_WROTE_2_2,
                   BYBFILE.BYB_FEE_NO_ELIG_2_3,
                   BYBFILE.BYB_FEE_NO_WROTE_2_3,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_2_3,
                   BYBFILE.BYB_FEE_AMT_WROTE_2_3,
                   BYBFILE.BYB_OTH_NO_COL_2,
                   BYBFILE.BYB_OT2_NO_COL_2,
                   BYBFILE.BYB_INT_NO_COL_2,
                   BYBFILE.BYB_LATE_NO_COL_2,
                   BYBFILE.BYB_OTH_NO_WAIVED_2,
                   BYBFILE.BYB_OT2_NO_WAIVED_2,
                   BYBFILE.BYB_INT_NO_WAIVED_2,
                   BYBFILE.BYB_LATE_NO_WAIVED_2,
                   BYBFILE.BYB_OTH_AMT_WAIVED_2,
                   BYBFILE.BYB_OT2_AMT_WAIVED_2,
                   BYBFILE.BYB_INT_AMT_WAIVED_2,
                   BYBFILE.BYB_LATE_AMT_WAIVED_2,
                   BYBFILE.BYB_UNITS_1_1_1,
                   BYBFILE.BYB_OUTBAL_1_1_1,
                   BYBFILE.BYB_UNITS_1_1_2,
                   BYBFILE.BYB_OUTBAL_1_1_2,
                   BYBFILE.BYB_UNITS_1_1_3,
                   BYBFILE.BYB_OUTBAL_1_1_3,
                   BYBFILE.BYB_UNITS_1_1_4,
                   BYBFILE.BYB_OUTBAL_1_1_4,
                   BYBFILE.BYB_UNITS_1_1_5,
                   BYBFILE.BYB_OUTBAL_1_1_5,
                   BYBFILE.BYB_UNITS_1_1_6,
                   BYBFILE.BYB_OUTBAL_1_1_6,
                   BYBFILE.BYB_UNITS_1_1_7,
                   BYBFILE.BYB_OUTBAL_1_1_7,
                   BYBFILE.BYB_UNITS_1_1_8,
                   BYBFILE.BYB_OUTBAL_1_1_8,
                   BYBFILE.BYB_UNITS_1_1_9,
                   BYBFILE.BYB_OUTBAL_1_1_9,
                   BYBFILE.BYB_UNITS_1_1_10,
                   BYBFILE.BYB_OUTBAL_1_1_10,
                   BYBFILE.BYB_UNITS_1_2_1,
                   BYBFILE.BYB_OUTBAL_1_2_1,
                   BYBFILE.BYB_UNITS_1_2_2,
                   BYBFILE.BYB_OUTBAL_1_2_2,
                   BYBFILE.BYB_UNITS_1_2_3,
                   BYBFILE.BYB_OUTBAL_1_2_3,
                   BYBFILE.BYB_UNITS_1_2_4,
                   BYBFILE.BYB_OUTBAL_1_2_4,
                   BYBFILE.BYB_UNITS_1_2_5,
                   BYBFILE.BYB_OUTBAL_1_2_5,
                   BYBFILE.BYB_UNITS_1_2_6,
                   BYBFILE.BYB_OUTBAL_1_2_6,
                   BYBFILE.BYB_UNITS_1_2_7,
                   BYBFILE.BYB_OUTBAL_1_2_7,
                   BYBFILE.BYB_UNITS_1_2_8,
                   BYBFILE.BYB_OUTBAL_1_2_8,
                   BYBFILE.BYB_UNITS_1_2_9,
                   BYBFILE.BYB_OUTBAL_1_2_9,
                   BYBFILE.BYB_UNITS_1_2_10,
                   BYBFILE.BYB_OUTBAL_1_2_10,
                   BYBFILE.BYB_UNITS_2_1_1,
                   BYBFILE.BYB_OUTBAL_2_1_1,
                   BYBFILE.BYB_UNITS_2_1_2,
                   BYBFILE.BYB_OUTBAL_2_1_2,
                   BYBFILE.BYB_UNITS_2_1_3,
                   BYBFILE.BYB_OUTBAL_2_1_3,
                   BYBFILE.BYB_UNITS_2_1_4,
                   BYBFILE.BYB_OUTBAL_2_1_4,
                   BYBFILE.BYB_UNITS_2_1_5,
                   BYBFILE.BYB_OUTBAL_2_1_5,
                   BYBFILE.BYB_UNITS_2_1_6,
                   BYBFILE.BYB_OUTBAL_2_1_6,
                   BYBFILE.BYB_UNITS_2_1_7,
                   BYBFILE.BYB_OUTBAL_2_1_7,
                   BYBFILE.BYB_UNITS_2_1_8,
                   BYBFILE.BYB_OUTBAL_2_1_8,
                   BYBFILE.BYB_UNITS_2_1_9,
                   BYBFILE.BYB_OUTBAL_2_1_9,
                   BYBFILE.BYB_UNITS_2_1_10,
                   BYBFILE.BYB_OUTBAL_2_1_10,
                   BYBFILE.BYB_UNITS_2_2_1,
                   BYBFILE.BYB_OUTBAL_2_2_1,
                   BYBFILE.BYB_UNITS_2_2_2,
                   BYBFILE.BYB_OUTBAL_2_2_2,
                   BYBFILE.BYB_UNITS_2_2_3,
                   BYBFILE.BYB_OUTBAL_2_2_3,
                   BYBFILE.BYB_UNITS_2_2_4,
                   BYBFILE.BYB_OUTBAL_2_2_4,
                   BYBFILE.BYB_UNITS_2_2_5,
                   BYBFILE.BYB_OUTBAL_2_2_5,
                   BYBFILE.BYB_UNITS_2_2_6,
                   BYBFILE.BYB_OUTBAL_2_2_6,
                   BYBFILE.BYB_UNITS_2_2_7,
                   BYBFILE.BYB_OUTBAL_2_2_7,
                   BYBFILE.BYB_UNITS_2_2_8,
                   BYBFILE.BYB_OUTBAL_2_2_8,
                   BYBFILE.BYB_UNITS_2_2_9,
                   BYBFILE.BYB_OUTBAL_2_2_9,
                   BYBFILE.BYB_UNITS_2_2_10,
                   BYBFILE.BYB_OUTBAL_2_2_10,
                   BYBFILE.BYB_UNITS_3_1_1,
                   BYBFILE.BYB_OUTBAL_3_1_1,
                   BYBFILE.BYB_UNITS_3_1_2,
                   BYBFILE.BYB_OUTBAL_3_1_2,
                   BYBFILE.BYB_UNITS_3_1_3,
                   BYBFILE.BYB_OUTBAL_3_1_3,
                   BYBFILE.BYB_UNITS_3_1_4,
                   BYBFILE.BYB_OUTBAL_3_1_4,
                   BYBFILE.BYB_UNITS_3_1_5,
                   BYBFILE.BYB_OUTBAL_3_1_5,
                   BYBFILE.BYB_UNITS_3_1_6,
                   BYBFILE.BYB_OUTBAL_3_1_6,
                   BYBFILE.BYB_UNITS_3_1_7,
                   BYBFILE.BYB_OUTBAL_3_1_7,
                   BYBFILE.BYB_UNITS_3_1_8,
                   BYBFILE.BYB_OUTBAL_3_1_8,
                   BYBFILE.BYB_UNITS_3_1_9,
                   BYBFILE.BYB_OUTBAL_3_1_9,
                   BYBFILE.BYB_UNITS_3_1_10,
                   BYBFILE.BYB_OUTBAL_3_1_10,
                   BYBFILE.BYB_UNITS_3_2_1,
                   BYBFILE.BYB_OUTBAL_3_2_1,
                   BYBFILE.BYB_UNITS_3_2_2,
                   BYBFILE.BYB_OUTBAL_3_2_2,
                   BYBFILE.BYB_UNITS_3_2_3,
                   BYBFILE.BYB_OUTBAL_3_2_3,
                   BYBFILE.BYB_UNITS_3_2_4,
                   BYBFILE.BYB_OUTBAL_3_2_4,
                   BYBFILE.BYB_UNITS_3_2_5,
                   BYBFILE.BYB_OUTBAL_3_2_5,
                   BYBFILE.BYB_UNITS_3_2_6,
                   BYBFILE.BYB_OUTBAL_3_2_6,
                   BYBFILE.BYB_UNITS_3_2_7,
                   BYBFILE.BYB_OUTBAL_3_2_7,
                   BYBFILE.BYB_UNITS_3_2_8,
                   BYBFILE.BYB_OUTBAL_3_2_8,
                   BYBFILE.BYB_UNITS_3_2_9,
                   BYBFILE.BYB_OUTBAL_3_2_9,
                   BYBFILE.BYB_UNITS_3_2_10,
                   BYBFILE.BYB_OUTBAL_3_2_10,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_1,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_2,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_3,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_4,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_5,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_6,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_7,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_8,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_9,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_10,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_1,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_2,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_3,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_4,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_5,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_6,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_7,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_8,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_9,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_10,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_1,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_2,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_3,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_4,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_5,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_6,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_7,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_8,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_9,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_10,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_1,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_2,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_3,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_4,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_5,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_6,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_7,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_8,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_9,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_10,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_1,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_2,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_3,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_4,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_5,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_6,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_7,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_8,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_9,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_10,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_1,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_2,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_3,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_4,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_5,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_6,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_7,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_8,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_9,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_10,
                   BYBFILE.BYB_SUMWGTTM,
                   BYBFILE.BYB_SUMWGTAV,
                   BYBFILE.BYB_SUMWGTTA,
                   BYBFILE.BYB_IB_ACCRUED,
                   BYBFILE.BYB_DLDISC_1,
                   BYBFILE.BYB_DLDISC_2,
                   BYBFILE.BYB_DLDISC_3,
                   BYBFILE.BYB_STD_POS,
                   BYBFILE.BYB_30_DAY_MOVED,
                   BYBFILE.BYB_PROJ_INCREASE,
                   BYBFILE.BYB_PROJ_PROFIT,
                   BYBFILE.BYB_PROCEEDS_1,
                   BYBFILE.BYB_PROCEEDS_2,
                   BYBFILE.BYB_LOCKBOX_1,
                   BYBFILE.BYB_LOCKBOX_2,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_1,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_2,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_3,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_4,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_5,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_6,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_7,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_8,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_9,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_10,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_1,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_2,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_3,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_4,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_5,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_6,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_7,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_8,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_9,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_10,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_1,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_2,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_3,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_4,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_5,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_6,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_7,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_8,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_9,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_10,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_1,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_2,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_3,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_4,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_5,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_6,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_7,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_8,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_9,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_10,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_1,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_2,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_3,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_4,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_5,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_6,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_7,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_8,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_9,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_10,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_1,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_2,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_3,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_4,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_5,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_6,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_7,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_8,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_9,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_10
            INTO :QBYB-REC
            FROM DBO.BYBFILE
            WHERE BYBFILE.BYB_DATE_CCYYMM = :QBYB-DATE-CCYYMM
              AND BYBFILE.BYB_BRNO = :QBYB-BRNO
              AND BYBFILE.BYB_CLASS = :QBYB-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BYB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BYB1-FILE.

      *-----------------------------------------------------------------
       SET-ALL-TO-BY1.
           MOVE BYA1-KEY   TO BY1-KEY.
           MOVE BYA-AREA-A TO BY-AREA-A.
           MOVE BYB-AREA-B TO BY-AREA-B.

      *-----------------------------------------------------------------
       SET-BY1-TO-ALL.
           MOVE BY1-KEY    TO BYA1-KEY BYB1-KEY.
           MOVE BY-AREA-A  TO BYA-AREA-A.
           MOVE BY-AREA-B  TO BYB-AREA-B.

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

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IOBYRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
