       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOTYRN.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-14-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOTYRN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : TYFILE 
      * TABLE DESC    : TODAYS TOTAL FILE
      * OUTPUT FILE   : XPC/IOTYRN.PRE
      * USED FOR      : LIBLP/LPWSTY.CPY  --- QTY-REC
      *                 LIBLP/LPTY1R.CPY  --- READ
      *                 LIBLP/LPTY1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0414 ***NEW***
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./IO/IOTYRN.CBL S34 05/09/21-19:12:23 >".

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
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LPWSTY.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QTY1-WHERE-BEG-KEY.
           03  QTY1-WBEG-BRNO             PIC 9(4).
           03  QTY1-WBEG-DATE             PIC X(10).
           03  QTY1-WBEG-CLASS           PIC 9(03).

       01  QTY1-WHERE-END-KEY.
           03  QTY1-WEND-BRNO             PIC 9(4).
           03  QTY1-WEND-DATE             PIC X(10).
           03  QTY1-WEND-CLASS           PIC 9(03).

 
       01  QTY-REC.
           03  QTY-BRNO             PIC 9(4).
           03  QTY-DATE             PIC X(10).
           03  QTY-CLASS            PIC 9(3).
           03  QTY-LTOUCH-DATE      PIC X(10).
           03  QTY-NO-1-1              PIC S9(5).
           03  QTY-AMT-1-1             PIC S9(8)V99.
           03  QTY-O2-1-1              PIC S9(8)V99.
           03  QTY-INT-1-1             PIC S9(8)V99.
           03  QTY-NO-1-2              PIC S9(5).
           03  QTY-AMT-1-2             PIC S9(8)V99.
           03  QTY-O2-1-2              PIC S9(8)V99.
           03  QTY-INT-1-2             PIC S9(8)V99.
           03  QTY-NO-1-3              PIC S9(5).
           03  QTY-AMT-1-3             PIC S9(8)V99.
           03  QTY-O2-1-3              PIC S9(8)V99.
           03  QTY-INT-1-3             PIC S9(8)V99.
           03  QTY-NO-1-4              PIC S9(5).
           03  QTY-AMT-1-4             PIC S9(8)V99.
           03  QTY-O2-1-4              PIC S9(8)V99.
           03  QTY-INT-1-4             PIC S9(8)V99.
           03  QTY-NO-1-5              PIC S9(5).
           03  QTY-AMT-1-5             PIC S9(8)V99.
           03  QTY-O2-1-5              PIC S9(8)V99.
           03  QTY-INT-1-5             PIC S9(8)V99.
           03  QTY-NO-1-6              PIC S9(5).
           03  QTY-AMT-1-6             PIC S9(8)V99.
           03  QTY-O2-1-6              PIC S9(8)V99.
           03  QTY-INT-1-6             PIC S9(8)V99.
           03  QTY-NO-1-7              PIC S9(5).
           03  QTY-AMT-1-7             PIC S9(8)V99.
           03  QTY-O2-1-7              PIC S9(8)V99.
           03  QTY-INT-1-7             PIC S9(8)V99.
           03  QTY-NO-1-8              PIC S9(5).
           03  QTY-AMT-1-8             PIC S9(8)V99.
           03  QTY-O2-1-8              PIC S9(8)V99.
           03  QTY-INT-1-8             PIC S9(8)V99.
           03  QTY-NO-1-9              PIC S9(5).
           03  QTY-AMT-1-9             PIC S9(8)V99.
           03  QTY-O2-1-9              PIC S9(8)V99.
           03  QTY-INT-1-9             PIC S9(8)V99.
           03  QTY-NO-1-10             PIC S9(5).
           03  QTY-AMT-1-10            PIC S9(8)V99.
           03  QTY-O2-1-10             PIC S9(8)V99.
           03  QTY-INT-1-10            PIC S9(8)V99.
           03  QTY-NO-1-11             PIC S9(5).
           03  QTY-AMT-1-11            PIC S9(8)V99.
           03  QTY-O2-1-11             PIC S9(8)V99.
           03  QTY-INT-1-11            PIC S9(8)V99.
           03  QTY-NO-2-1              PIC S9(5).
           03  QTY-AMT-2-1             PIC S9(8)V99.
           03  QTY-O2-2-1              PIC S9(8)V99.
           03  QTY-INT-2-1             PIC S9(8)V99.
           03  QTY-NO-2-2              PIC S9(5).
           03  QTY-AMT-2-2             PIC S9(8)V99.
           03  QTY-O2-2-2              PIC S9(8)V99.
           03  QTY-INT-2-2             PIC S9(8)V99.
           03  QTY-NO-2-3              PIC S9(5).
           03  QTY-AMT-2-3             PIC S9(8)V99.
           03  QTY-O2-2-3              PIC S9(8)V99.
           03  QTY-INT-2-3             PIC S9(8)V99.
           03  QTY-NO-2-4              PIC S9(5).
           03  QTY-AMT-2-4             PIC S9(8)V99.
           03  QTY-O2-2-4              PIC S9(8)V99.
           03  QTY-INT-2-4             PIC S9(8)V99.
           03  QTY-NO-2-5              PIC S9(5).
           03  QTY-AMT-2-5             PIC S9(8)V99.
           03  QTY-O2-2-5              PIC S9(8)V99.
           03  QTY-INT-2-5             PIC S9(8)V99.
           03  QTY-NO-2-6              PIC S9(5).
           03  QTY-AMT-2-6             PIC S9(8)V99.
           03  QTY-O2-2-6              PIC S9(8)V99.
           03  QTY-INT-2-6             PIC S9(8)V99.
           03  QTY-NO-2-7              PIC S9(5).
           03  QTY-AMT-2-7             PIC S9(8)V99.
           03  QTY-O2-2-7              PIC S9(8)V99.
           03  QTY-INT-2-7             PIC S9(8)V99.
           03  QTY-NO-2-8              PIC S9(5).
           03  QTY-AMT-2-8             PIC S9(8)V99.
           03  QTY-O2-2-8              PIC S9(8)V99.
           03  QTY-INT-2-8             PIC S9(8)V99.
           03  QTY-NO-2-9              PIC S9(5).
           03  QTY-AMT-2-9             PIC S9(8)V99.
           03  QTY-O2-2-9              PIC S9(8)V99.
           03  QTY-INT-2-9             PIC S9(8)V99.
           03  QTY-NO-2-10             PIC S9(5).
           03  QTY-AMT-2-10            PIC S9(8)V99.
           03  QTY-O2-2-10             PIC S9(8)V99.
           03  QTY-INT-2-10            PIC S9(8)V99.
           03  QTY-NO-2-11             PIC S9(5).
           03  QTY-AMT-2-11            PIC S9(8)V99.
           03  QTY-O2-2-11             PIC S9(8)V99.
           03  QTY-INT-2-11            PIC S9(8)V99.
           03  QTY-NO-REPO-1-1         PIC S9(5).
           03  QTY-AMT-REPO-1-1        PIC S9(8)V99.
           03  QTY-O2-REPO-1-1         PIC S9(8)V99.
           03  QTY-INT-REPO-1-1        PIC S9(8)V99.
           03  QTY-NO-REPO-1-2         PIC S9(5).
           03  QTY-AMT-REPO-1-2        PIC S9(8)V99.
           03  QTY-O2-REPO-1-2         PIC S9(8)V99.
           03  QTY-INT-REPO-1-2        PIC S9(8)V99.
           03  QTY-NO-REPO-1-3         PIC S9(5).
           03  QTY-AMT-REPO-1-3        PIC S9(8)V99.
           03  QTY-O2-REPO-1-3         PIC S9(8)V99.
           03  QTY-INT-REPO-1-3        PIC S9(8)V99.
           03  QTY-NO-REPO-1-4         PIC S9(5).
           03  QTY-AMT-REPO-1-4        PIC S9(8)V99.
           03  QTY-O2-REPO-1-4         PIC S9(8)V99.
           03  QTY-INT-REPO-1-4        PIC S9(8)V99.
           03  QTY-NO-REPO-1-5         PIC S9(5).
           03  QTY-AMT-REPO-1-5        PIC S9(8)V99.
           03  QTY-O2-REPO-1-5         PIC S9(8)V99.
           03  QTY-INT-REPO-1-5        PIC S9(8)V99.
           03  QTY-NO-REPO-2-1         PIC S9(5).
           03  QTY-AMT-REPO-2-1        PIC S9(8)V99.
           03  QTY-O2-REPO-2-1         PIC S9(8)V99.
           03  QTY-INT-REPO-2-1        PIC S9(8)V99.
           03  QTY-NO-REPO-2-2         PIC S9(5).
           03  QTY-AMT-REPO-2-2        PIC S9(8)V99.
           03  QTY-O2-REPO-2-2         PIC S9(8)V99.
           03  QTY-INT-REPO-2-2        PIC S9(8)V99.
           03  QTY-NO-REPO-2-3         PIC S9(5).
           03  QTY-AMT-REPO-2-3        PIC S9(8)V99.
           03  QTY-O2-REPO-2-3         PIC S9(8)V99.
           03  QTY-INT-REPO-2-3        PIC S9(8)V99.
           03  QTY-NO-REPO-2-4         PIC S9(5).
           03  QTY-AMT-REPO-2-4        PIC S9(8)V99.
           03  QTY-O2-REPO-2-4         PIC S9(8)V99.
           03  QTY-INT-REPO-2-4        PIC S9(8)V99.
           03  QTY-NO-REPO-2-5         PIC S9(5).
           03  QTY-AMT-REPO-2-5        PIC S9(8)V99.
           03  QTY-O2-REPO-2-5         PIC S9(8)V99.
           03  QTY-INT-REPO-2-5        PIC S9(8)V99.
           03  QTY-NO-PL-1-1          PIC S9(5).
           03  QTY-AMT-PL-1-1         PIC S9(8)V99.
           03  QTY-O2-PL-1-1          PIC S9(8)V99.
           03  QTY-INT-PL-1-1         PIC S9(8)V99.
           03  QTY-NO-PL-1-2          PIC S9(5).
           03  QTY-AMT-PL-1-2         PIC S9(8)V99.
           03  QTY-O2-PL-1-2          PIC S9(8)V99.
           03  QTY-INT-PL-1-2         PIC S9(8)V99.
           03  QTY-NO-PL-1-3          PIC S9(5).
           03  QTY-AMT-PL-1-3         PIC S9(8)V99.
           03  QTY-O2-PL-1-3          PIC S9(8)V99.
           03  QTY-INT-PL-1-3         PIC S9(8)V99.
           03  QTY-NO-PL-1-4          PIC S9(5).
           03  QTY-AMT-PL-1-4         PIC S9(8)V99.
           03  QTY-O2-PL-1-4          PIC S9(8)V99.
           03  QTY-INT-PL-1-4         PIC S9(8)V99.
           03  QTY-NO-PL-1-5          PIC S9(5).
           03  QTY-AMT-PL-1-5         PIC S9(8)V99.
           03  QTY-O2-PL-1-5          PIC S9(8)V99.
           03  QTY-INT-PL-1-5         PIC S9(8)V99.
           03  QTY-NO-PL-1-6          PIC S9(5).
           03  QTY-AMT-PL-1-6         PIC S9(8)V99.
           03  QTY-O2-PL-1-6          PIC S9(8)V99.
           03  QTY-INT-PL-1-6         PIC S9(8)V99.
           03  QTY-NO-PL-1-7          PIC S9(5).
           03  QTY-AMT-PL-1-7         PIC S9(8)V99.
           03  QTY-O2-PL-1-7          PIC S9(8)V99.
           03  QTY-INT-PL-1-7         PIC S9(8)V99.
           03  QTY-NO-PL-1-8          PIC S9(5).
           03  QTY-AMT-PL-1-8         PIC S9(8)V99.
           03  QTY-O2-PL-1-8          PIC S9(8)V99.
           03  QTY-INT-PL-1-8         PIC S9(8)V99.
           03  QTY-NO-PL-1-9          PIC S9(5).
           03  QTY-AMT-PL-1-9         PIC S9(8)V99.
           03  QTY-O2-PL-1-9          PIC S9(8)V99.
           03  QTY-INT-PL-1-9         PIC S9(8)V99.
           03  QTY-NO-PL-1-10         PIC S9(5).
           03  QTY-AMT-PL-1-10        PIC S9(8)V99.
           03  QTY-O2-PL-1-10         PIC S9(8)V99.
           03  QTY-INT-PL-1-10        PIC S9(8)V99.
           03  QTY-NO-PL-2-1          PIC S9(5).
           03  QTY-AMT-PL-2-1         PIC S9(8)V99.
           03  QTY-O2-PL-2-1          PIC S9(8)V99.
           03  QTY-INT-PL-2-1         PIC S9(8)V99.
           03  QTY-NO-PL-2-2          PIC S9(5).
           03  QTY-AMT-PL-2-2         PIC S9(8)V99.
           03  QTY-O2-PL-2-2          PIC S9(8)V99.
           03  QTY-INT-PL-2-2         PIC S9(8)V99.
           03  QTY-NO-PL-2-3          PIC S9(5).
           03  QTY-AMT-PL-2-3         PIC S9(8)V99.
           03  QTY-O2-PL-2-3          PIC S9(8)V99.
           03  QTY-INT-PL-2-3         PIC S9(8)V99.
           03  QTY-NO-PL-2-4          PIC S9(5).
           03  QTY-AMT-PL-2-4         PIC S9(8)V99.
           03  QTY-O2-PL-2-4          PIC S9(8)V99.
           03  QTY-INT-PL-2-4         PIC S9(8)V99.
           03  QTY-NO-PL-2-5          PIC S9(5).
           03  QTY-AMT-PL-2-5         PIC S9(8)V99.
           03  QTY-O2-PL-2-5          PIC S9(8)V99.
           03  QTY-INT-PL-2-5         PIC S9(8)V99.
           03  QTY-NO-PL-2-6          PIC S9(5).
           03  QTY-AMT-PL-2-6         PIC S9(8)V99.
           03  QTY-O2-PL-2-6          PIC S9(8)V99.
           03  QTY-INT-PL-2-6         PIC S9(8)V99.
           03  QTY-NO-PL-2-7          PIC S9(5).
           03  QTY-AMT-PL-2-7         PIC S9(8)V99.
           03  QTY-O2-PL-2-7          PIC S9(8)V99.
           03  QTY-INT-PL-2-7         PIC S9(8)V99.
           03  QTY-NO-PL-2-8          PIC S9(5).
           03  QTY-AMT-PL-2-8         PIC S9(8)V99.
           03  QTY-O2-PL-2-8          PIC S9(8)V99.
           03  QTY-INT-PL-2-8         PIC S9(8)V99.
           03  QTY-NO-PL-2-9          PIC S9(5).
           03  QTY-AMT-PL-2-9         PIC S9(8)V99.
           03  QTY-O2-PL-2-9          PIC S9(8)V99.
           03  QTY-INT-PL-2-9         PIC S9(8)V99.
           03  QTY-NO-PL-2-10         PIC S9(5).
           03  QTY-AMT-PL-2-10        PIC S9(8)V99.
           03  QTY-O2-PL-2-10         PIC S9(8)V99.
           03  QTY-INT-PL-2-10        PIC S9(8)V99.
           03  QTY-DLRVPRIN-1-1         PIC S9(8)V99.
           03  QTY-DLRVPRIN-1-2         PIC S9(8)V99.
           03  QTY-DLRVPRIN-1-3         PIC S9(8)V99.
           03  QTY-DLRVCHGS-1-1         PIC S9(8)V99.
           03  QTY-DLRVCHGS-1-2         PIC S9(8)V99.
           03  QTY-DLRVCHGS-1-3         PIC S9(8)V99.
           03  QTY-DLPARVHELD-1         PIC S9(8)V99.
           03  QTY-DLRVPRIN-2-1         PIC S9(8)V99.
           03  QTY-DLRVPRIN-2-2         PIC S9(8)V99.
           03  QTY-DLRVPRIN-2-3         PIC S9(8)V99.
           03  QTY-DLRVCHGS-2-1         PIC S9(8)V99.
           03  QTY-DLRVCHGS-2-2         PIC S9(8)V99.
           03  QTY-DLRVCHGS-2-3         PIC S9(8)V99.
           03  QTY-DLPARVHELD-2         PIC S9(8)V99.
           03  QTY-PL-PRINCOL-1      PIC S9(8)V99.
           03  QTY-PL-INTCOL-1       PIC S9(8)V99.
           03  QTY-PL-LATECOL-1      PIC S9(8)V99.
           03  QTY-PL-O2COL-1        PIC S9(8)V99.
           03  QTY-PL-PRINCOL-2      PIC S9(8)V99.
           03  QTY-PL-INTCOL-2       PIC S9(8)V99.
           03  QTY-PL-LATECOL-2      PIC S9(8)V99.
           03  QTY-PL-O2COL-2        PIC S9(8)V99.
           03  QTY-PL-PRINCOL-3      PIC S9(8)V99.
           03  QTY-PL-INTCOL-3       PIC S9(8)V99.
           03  QTY-PL-LATECOL-3      PIC S9(8)V99.
           03  QTY-PL-O2COL-3        PIC S9(8)V99.
           03  QTY-PL-PRINCOL-4      PIC S9(8)V99.
           03  QTY-PL-INTCOL-4       PIC S9(8)V99.
           03  QTY-PL-LATECOL-4      PIC S9(8)V99.
           03  QTY-PL-O2COL-4        PIC S9(8)V99.
           03  QTY-PL-PRINCOL-5      PIC S9(8)V99.
           03  QTY-PL-INTCOL-5       PIC S9(8)V99.
           03  QTY-PL-LATECOL-5      PIC S9(8)V99.
           03  QTY-PL-O2COL-5        PIC S9(8)V99.
           03  QTY-AMT-SOLD            PIC S9(8)V99.
           03  QTY-AMT-TRANS           PIC S9(8)V99.
           03  QTY-AMT-PURCH           PIC S9(8)V99.
           03  QTY-NO-SOLD             PIC S9(5).
           03  QTY-NO-TRANS            PIC S9(5).
           03  QTY-NO-PURCH            PIC S9(5).
           03  QTY-OTHCOL              PIC S9(8)V99.
           03  QTY-OT2COL              PIC S9(8)V99.
           03  QTY-INTCOL              PIC S9(8)V99.
           03  QTY-LATECOL             PIC S9(8)V99.
           03  QTY-PRINCOL             PIC S9(8)V99.
           03  QTY-DEFCOL              PIC S9(8)V99.
           03  QTY-RNPRIN              PIC S9(8)V99.
           03  QTY-INTREB              PIC S9(8)V99.
           03  QTY-CLREB1              PIC S9(8)V99.
           03  QTY-CLREB2              PIC S9(8)V99.
           03  QTY-CLREB3              PIC S9(8)V99.
           03  QTY-CLREB4              PIC S9(8)V99.
           03  QTY-AHREB               PIC S9(8)V99.
           03  QTY-PPREB               PIC S9(8)V99.
           03  QTY-O1REB               PIC S9(8)V99.
           03  QTY-O2REB               PIC S9(8)V99.
           03  QTY-O3REB               PIC S9(8)V99.
           03  QTY-O4REB               PIC S9(8)V99.
           03  QTY-O5REB               PIC S9(8)V99.
           03  QTY-O6REB               PIC S9(8)V99.
           03  QTY-O7REB               PIC S9(8)V99.
           03  QTY-SCREB               PIC S9(8)V99.
           03  QTY-MFREB               PIC S9(8)V99.
           03  QTY-DFREB               PIC S9(8)V99.
           03  QTY-UN7-REB             PIC S9(8)V99.
           03  QTY-UN8-REB             PIC S9(8)V99.
           03  QTY-UN9-REB             PIC S9(8)V99.
           03  QTY-UN10-REB            PIC S9(8)V99.
           03  QTY-PART-CHGOFF         PIC S9(8)V99.
           03  QTY-INS-NO-ELIG-1       PIC S9(6).
           03  QTY-INS-NO-WROTE-1      PIC S9(6).
           03  QTY-INS-NO-NOTWROTE-1   PIC S9(6).
           03  QTY-INS-AMT-WROTE-1     PIC S9(8)V99.
           03  QTY-INS-NO-ELIG-2       PIC S9(6).
           03  QTY-INS-NO-WROTE-2      PIC S9(6).
           03  QTY-INS-NO-NOTWROTE-2   PIC S9(6).
           03  QTY-INS-AMT-WROTE-2     PIC S9(8)V99.
           03  QTY-INS-NO-ELIG-3       PIC S9(6).
           03  QTY-INS-NO-WROTE-3      PIC S9(6).
           03  QTY-INS-NO-NOTWROTE-3   PIC S9(6).
           03  QTY-INS-AMT-WROTE-3     PIC S9(8)V99.
           03  QTY-INS-NO-ELIG-4       PIC S9(6).
           03  QTY-INS-NO-WROTE-4      PIC S9(6).
           03  QTY-INS-NO-NOTWROTE-4   PIC S9(6).
           03  QTY-INS-AMT-WROTE-4     PIC S9(8)V99.
           03  QTY-INS-NO-ELIG-5       PIC S9(6).
           03  QTY-INS-NO-WROTE-5      PIC S9(6).
           03  QTY-INS-NO-NOTWROTE-5   PIC S9(6).
           03  QTY-INS-AMT-WROTE-5     PIC S9(8)V99.
           03  QTY-INS-NO-ELIG-6       PIC S9(6).
           03  QTY-INS-NO-WROTE-6      PIC S9(6).
           03  QTY-INS-NO-NOTWROTE-6   PIC S9(6).
           03  QTY-INS-AMT-WROTE-6     PIC S9(8)V99.
           03  QTY-INS-NO-ELIG-7       PIC S9(6).
           03  QTY-INS-NO-WROTE-7      PIC S9(6).
           03  QTY-INS-NO-NOTWROTE-7   PIC S9(6).
           03  QTY-INS-AMT-WROTE-7     PIC S9(8)V99.
           03  QTY-INS-NO-ELIG-8       PIC S9(6).
           03  QTY-INS-NO-WROTE-8      PIC S9(6).
           03  QTY-INS-NO-NOTWROTE-8   PIC S9(6).
           03  QTY-INS-AMT-WROTE-8     PIC S9(8)V99.
           03  QTY-INS-NO-ELIG-9       PIC S9(6).
           03  QTY-INS-NO-WROTE-9      PIC S9(6).
           03  QTY-INS-NO-NOTWROTE-9   PIC S9(6).
           03  QTY-INS-AMT-WROTE-9     PIC S9(8)V99.
           03  QTY-INS-NO-ELIG-10      PIC S9(6).
           03  QTY-INS-NO-WROTE-10     PIC S9(6).
           03  QTY-INS-NO-NOTWROTE-10  PIC S9(6).
           03  QTY-INS-AMT-WROTE-10    PIC S9(8)V99.
           03  QTY-FEE-NO-ELIG-1       PIC S9(6).
           03  QTY-FEE-NO-WROTE-1      PIC S9(6).
           03  QTY-FEE-NO-NOTWROTE-1   PIC S9(6).
           03  QTY-FEE-AMT-WROTE-1     PIC S9(8)V99.
           03  QTY-FEE-NO-ELIG-2       PIC S9(6).
           03  QTY-FEE-NO-WROTE-2      PIC S9(6).
           03  QTY-FEE-NO-NOTWROTE-2   PIC S9(6).
           03  QTY-FEE-AMT-WROTE-2     PIC S9(8)V99.
           03  QTY-FEE-NO-ELIG-3       PIC S9(6).
           03  QTY-FEE-NO-WROTE-3      PIC S9(6).
           03  QTY-FEE-NO-NOTWROTE-3   PIC S9(6).
           03  QTY-FEE-AMT-WROTE-3     PIC S9(8)V99.
           03  QTY-OTH-NO-COL          PIC S9(6).
           03  QTY-OT2-NO-COL          PIC S9(6).
           03  QTY-INT-NO-COL          PIC S9(6).
           03  QTY-LATE-NO-COL         PIC S9(6).
           03  QTY-OTH-NO-WAIVED       PIC S9(6).
           03  QTY-OT2-NO-WAIVED       PIC S9(6).
           03  QTY-INT-NO-WAIVED       PIC S9(6).
           03  QTY-LATE-NO-WAIVED      PIC S9(6).
           03  QTY-OTH-AMT-WAIVED      PIC S9(8)V99.
           03  QTY-OT2-AMT-WAIVED      PIC S9(8)V99.
           03  QTY-INT-AMT-WAIVED      PIC S9(8)V99.
           03  QTY-LATE-AMT-WAIVED     PIC S9(8)V99.
           03  QTY-CKWRITTEN-1         PIC S9(7)V99.
           03  QTY-CKWRITTEN-2         PIC S9(7)V99.
           03  QTY-CKWRITTEN-3         PIC S9(7)V99.
           03  QTY-CKWRITTEN-4         PIC S9(7)V99.
           03  QTY-CKWRITTEN-5         PIC S9(7)V99.
           03  QTY-CKWRITTEN-6         PIC S9(7)V99.
           03  QTY-CKWRITTEN-7         PIC S9(7)V99.
           03  QTY-CKWRITTEN-8         PIC S9(7)V99.
           03  QTY-CKWRITTEN-9         PIC S9(7)V99.
           03  QTY-CKWRITTEN-10        PIC S9(7)V99.
           03  QTY-CKWRITTEN-11        PIC S9(7)V99.
           03  QTY-CKWRITTEN-12        PIC S9(7)V99.
           03  QTY-CKWRITTEN-13        PIC S9(7)V99.
           03  QTY-CKWRITTEN-14        PIC S9(7)V99.
           03  QTY-CKWRITTEN-15        PIC S9(7)V99.
           03  QTY-CKWRITTEN-16        PIC S9(7)V99.
           03  QTY-UNITS-1-1           PIC S9(7).
           03  QTY-OUTBAL-1-1          PIC S9(9)V99.
           03  QTY-UNITS-1-2           PIC S9(7).
           03  QTY-OUTBAL-1-2          PIC S9(9)V99.
           03  QTY-UNITS-1-3           PIC S9(7).
           03  QTY-OUTBAL-1-3          PIC S9(9)V99.
           03  QTY-UNITS-1-4           PIC S9(7).
           03  QTY-OUTBAL-1-4          PIC S9(9)V99.
           03  QTY-UNITS-1-5           PIC S9(7).
           03  QTY-OUTBAL-1-5          PIC S9(9)V99.
           03  QTY-UNITS-1-6           PIC S9(7).
           03  QTY-OUTBAL-1-6          PIC S9(9)V99.
           03  QTY-UNITS-1-7           PIC S9(7).
           03  QTY-OUTBAL-1-7          PIC S9(9)V99.
           03  QTY-UNITS-1-8           PIC S9(7).
           03  QTY-OUTBAL-1-8          PIC S9(9)V99.
           03  QTY-UNITS-1-9           PIC S9(7).
           03  QTY-OUTBAL-1-9          PIC S9(9)V99.
           03  QTY-UNITS-1-10          PIC S9(7).
           03  QTY-OUTBAL-1-10         PIC S9(9)V99.
           03  QTY-UNITS-2-1           PIC S9(7).
           03  QTY-OUTBAL-2-1          PIC S9(9)V99.
           03  QTY-UNITS-2-2           PIC S9(7).
           03  QTY-OUTBAL-2-2          PIC S9(9)V99.
           03  QTY-UNITS-2-3           PIC S9(7).
           03  QTY-OUTBAL-2-3          PIC S9(9)V99.
           03  QTY-UNITS-2-4           PIC S9(7).
           03  QTY-OUTBAL-2-4          PIC S9(9)V99.
           03  QTY-UNITS-2-5           PIC S9(7).
           03  QTY-OUTBAL-2-5          PIC S9(9)V99.
           03  QTY-UNITS-2-6           PIC S9(7).
           03  QTY-OUTBAL-2-6          PIC S9(9)V99.
           03  QTY-UNITS-2-7           PIC S9(7).
           03  QTY-OUTBAL-2-7          PIC S9(9)V99.
           03  QTY-UNITS-2-8           PIC S9(7).
           03  QTY-OUTBAL-2-8          PIC S9(9)V99.
           03  QTY-UNITS-2-9           PIC S9(7).
           03  QTY-OUTBAL-2-9          PIC S9(9)V99.
           03  QTY-UNITS-2-10          PIC S9(7).
           03  QTY-OUTBAL-2-10         PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-1-1           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-1-1          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-1-2           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-1-2          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-1-3           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-1-3          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-1-4           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-1-4          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-1-5           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-1-5          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-1-6           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-1-6          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-1-7           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-1-7          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-1-8           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-1-8          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-1-9           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-1-9          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-1-10          PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-1-10         PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-2-1           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-2-1          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-2-2           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-2-2          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-2-3           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-2-3          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-2-4           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-2-4          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-2-5           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-2-5          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-2-6           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-2-6          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-2-7           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-2-7          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-2-8           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-2-8          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-2-9           PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-2-9          PIC S9(9)V99.
           03  QTY-UNITS-BNKRPT-2-10          PIC S9(7).
           03  QTY-OUTBAL-BNKRPT-2-10         PIC S9(9)V99.
           03  QTY-SUMWGTTM                PIC S9(8).
           03  QTY-SUMWGTAV                PIC S9(9)V99.
           03  QTY-SUMWGTTA                PIC S9(11)V99.
           03  QTY-ALLOTMENTS              PIC S9(7)V99.
           03  QTY-BRFG                    PIC X.
           03  QTY-DLDISC                  PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-1-1     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-1-2     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-1-3     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-1-4     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-1-5     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-1-6     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-1-7     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-1-8     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-1-9     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-1-10    PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-2-1     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-2-2     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-2-3     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-2-4     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-2-5     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-2-6     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-2-7     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-2-8     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-2-9     PIC S9(8)V99.
           03  QTY-UNEARNED-AMT-2-10    PIC S9(8)V99.
           03  QTY-PROCEEDS                 PIC S9(8)V99.
           03  QTY-LOCKBOX                  PIC S9(7)V99.
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
           03  BEG-DATE                PIC 9(08).
           03  END-DATE                PIC 9(08).
           03  BEG-CLASS              PIC 9(02).
           03  END-CLASS              PIC 9(02).

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
           03  FILLER PIC X(30) VALUE "BRNO      DATE     CLASS  NO 1".
           03  FILLER PIC X(30) VALUE " 1    AMT 1 1    NO 1 4   AMT ".
           03  FILLER PIC X(30) VALUE "1 4".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-TY-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-TY-DATE                   PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-TY-CLASS                   PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-TY-NO-1-1                  PIC 9(05).
           03  FILLER                         PIC X(03).
           03  D01-TY-AMT-1-1                  PIC Z(7)9.99.
           03  FILLER                         PIC X(03).
           03  D01-TY-NO-1-4                  PIC 9(05).
           03  FILLER                         PIC X(03).
           03  D01-TY-AMT-1-4                  PIC Z(7)9.99.

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
       COPY "LIBIO/IOTYRN_DEF.CPY".
       COPY "LIBIO/IOTYRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOTYRN_SCN.CPY".

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

           GO TO MAIN-PROGRAM-PROCESS-BRNO.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM READ-BR-FILE-NEXT.

           IF ( IO-BAD             ) OR
              ( BR-NO > END-BRANCH )
              GO TO MAIN-PROGRAM-END.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-PROCESS-BRNO.

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
           MOVE "IOTYRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOTYRN-QUEUE-POS  TO Q-POS.
           MOVE IOTYRN-COPIES-POS TO C-POS.
           MOVE IOTYRN-PRU-POS    TO P-POS.
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
       ENTER-BEG-DATE.

           MOVE SPACES  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE "ERNM060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF ( BEG-DATE > END-DATE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "S  8080BEG-DATE." TO VDU.
           MOVE JULIAN-BEG         TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE." TO VDU.
           MOVE JULIAN-END         TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           PERFORM SCREENIO.
      *=================================================================
       ENTER-BEG-CLASS.

           MOVE "ENNN030BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE.
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
           MOVE 999                 TO VDUNUM0  END-CLASS.
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

           MOVE "S  A000BEG-DATE."    TO VDU.
           MOVE BEG-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A000END-DATE."    TO VDU.
           MOVE END-DATE              TO VDU-DATE-EDIT.
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

           MOVE "B"                        TO TY-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QTY1-WBEG-BRNO.
           MOVE BR-NO     TO QTY1-WEND-BRNO.

           MOVE BEG-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTY1-WBEG-DATE.

           MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTY1-WEND-DATE.

           MOVE BEG-CLASS           TO QTY1-WBEG-CLASS.
           MOVE END-CLASS           TO QTY1-WEND-CLASS.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-TY1-FILE.
           PERFORM START-TY1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-TY1-FILE-NEXT.
           IF ( IO-BAD ) OR ( TY-BRNO NOT = TY-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.


           IF ( TY-DATE < BEG-DATE )
               GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( TY-DATE > END-DATE )
              GO TO PROCESS-BRANCH-MODULE-END.

           IF TY-CLASS < BEG-CLASS OR > END-CLASS
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE TY-BRNO                    TO D01-TY-BRNO.
           MOVE TY-DATE                  TO D01-TY-DATE.
           MOVE TY-CLASS                   TO D01-TY-CLASS.
           MOVE TY-NO(1,1)               TO D01-TY-NO-1-1.
           MOVE TY-AMT(1,1)               TO D01-TY-AMT-1-1.
           MOVE TY-NO(1,4)               TO D01-TY-NO-1-4.
           MOVE TY-AMT(1,4)               TO D01-TY-AMT-1-4.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-TY1-FILE.

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

           MOVE TY1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING DATE               : "    TO R01-PROMPT.
           MOVE BEG-DATE                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE               : "    TO R01-PROMPT.
           MOVE END-DATE                             TO R01-NUMERIC.
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
           MOVE "SQL TABLE IO UTILITY - TYFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOTYRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOTYRN-SYSDATE.
           DISPLAY IOTYRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOTYRN-FORM-FIELDS TO WR-BUF.
           MOVE IOTYRN-FORM-LIST TO WR-LIST.
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

           MOVE IOTYRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOTYRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOTYRN_EVA.CPY".
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
      *    LOAD TY-REC FROM QTY-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-TY-FIELDS.
           MOVE QTY-BRNO TO TY-BRNO.

           IF ( QTY-DATE = "1900-01-01" )
              MOVE ZEROES TO TY-DATE
           ELSE
              MOVE QTY-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TY-DATE.

           MOVE QTY-CLASS TO TY-CLASS.

           IF ( QTY-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO TY-LTOUCH-DATE
           ELSE
              MOVE QTY-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TY-LTOUCH-DATE.

           MOVE QTY-NO-1-1 TO TY-NO(1 1).
           MOVE QTY-AMT-1-1 TO TY-AMT(1 1).
           MOVE QTY-O2-1-1 TO TY-O2(1 1).
           MOVE QTY-INT-1-1 TO TY-INT(1 1).
           MOVE QTY-NO-1-2 TO TY-NO(1 2).
           MOVE QTY-AMT-1-2 TO TY-AMT(1 2).
           MOVE QTY-O2-1-2 TO TY-O2(1 2).
           MOVE QTY-INT-1-2 TO TY-INT(1 2).
           MOVE QTY-NO-1-3 TO TY-NO(1 3).
           MOVE QTY-AMT-1-3 TO TY-AMT(1 3).
           MOVE QTY-O2-1-3 TO TY-O2(1 3).
           MOVE QTY-INT-1-3 TO TY-INT(1 3).
           MOVE QTY-NO-1-4 TO TY-NO(1 4).
           MOVE QTY-AMT-1-4 TO TY-AMT(1 4).
           MOVE QTY-O2-1-4 TO TY-O2(1 4).
           MOVE QTY-INT-1-4 TO TY-INT(1 4).
           MOVE QTY-NO-1-5 TO TY-NO(1 5).
           MOVE QTY-AMT-1-5 TO TY-AMT(1 5).
           MOVE QTY-O2-1-5 TO TY-O2(1 5).
           MOVE QTY-INT-1-5 TO TY-INT(1 5).
           MOVE QTY-NO-1-6 TO TY-NO(1 6).
           MOVE QTY-AMT-1-6 TO TY-AMT(1 6).
           MOVE QTY-O2-1-6 TO TY-O2(1 6).
           MOVE QTY-INT-1-6 TO TY-INT(1 6).
           MOVE QTY-NO-1-7 TO TY-NO(1 7).
           MOVE QTY-AMT-1-7 TO TY-AMT(1 7).
           MOVE QTY-O2-1-7 TO TY-O2(1 7).
           MOVE QTY-INT-1-7 TO TY-INT(1 7).
           MOVE QTY-NO-1-8 TO TY-NO(1 8).
           MOVE QTY-AMT-1-8 TO TY-AMT(1 8).
           MOVE QTY-O2-1-8 TO TY-O2(1 8).
           MOVE QTY-INT-1-8 TO TY-INT(1 8).
           MOVE QTY-NO-1-9 TO TY-NO(1 9).
           MOVE QTY-AMT-1-9 TO TY-AMT(1 9).
           MOVE QTY-O2-1-9 TO TY-O2(1 9).
           MOVE QTY-INT-1-9 TO TY-INT(1 9).
           MOVE QTY-NO-1-10 TO TY-NO(1 10).
           MOVE QTY-AMT-1-10 TO TY-AMT(1 10).
           MOVE QTY-O2-1-10 TO TY-O2(1 10).
           MOVE QTY-INT-1-10 TO TY-INT(1 10).
           MOVE QTY-NO-1-11 TO TY-NO(1 11).
           MOVE QTY-AMT-1-11 TO TY-AMT(1 11).
           MOVE QTY-O2-1-11 TO TY-O2(1 11).
           MOVE QTY-INT-1-11 TO TY-INT(1 11).
           MOVE QTY-NO-2-1 TO TY-NO(2 1).
           MOVE QTY-AMT-2-1 TO TY-AMT(2 1).
           MOVE QTY-O2-2-1 TO TY-O2(2 1).
           MOVE QTY-INT-2-1 TO TY-INT(2 1).
           MOVE QTY-NO-2-2 TO TY-NO(2 2).
           MOVE QTY-AMT-2-2 TO TY-AMT(2 2).
           MOVE QTY-O2-2-2 TO TY-O2(2 2).
           MOVE QTY-INT-2-2 TO TY-INT(2 2).
           MOVE QTY-NO-2-3 TO TY-NO(2 3).
           MOVE QTY-AMT-2-3 TO TY-AMT(2 3).
           MOVE QTY-O2-2-3 TO TY-O2(2 3).
           MOVE QTY-INT-2-3 TO TY-INT(2 3).
           MOVE QTY-NO-2-4 TO TY-NO(2 4).
           MOVE QTY-AMT-2-4 TO TY-AMT(2 4).
           MOVE QTY-O2-2-4 TO TY-O2(2 4).
           MOVE QTY-INT-2-4 TO TY-INT(2 4).
           MOVE QTY-NO-2-5 TO TY-NO(2 5).
           MOVE QTY-AMT-2-5 TO TY-AMT(2 5).
           MOVE QTY-O2-2-5 TO TY-O2(2 5).
           MOVE QTY-INT-2-5 TO TY-INT(2 5).
           MOVE QTY-NO-2-6 TO TY-NO(2 6).
           MOVE QTY-AMT-2-6 TO TY-AMT(2 6).
           MOVE QTY-O2-2-6 TO TY-O2(2 6).
           MOVE QTY-INT-2-6 TO TY-INT(2 6).
           MOVE QTY-NO-2-7 TO TY-NO(2 7).
           MOVE QTY-AMT-2-7 TO TY-AMT(2 7).
           MOVE QTY-O2-2-7 TO TY-O2(2 7).
           MOVE QTY-INT-2-7 TO TY-INT(2 7).
           MOVE QTY-NO-2-8 TO TY-NO(2 8).
           MOVE QTY-AMT-2-8 TO TY-AMT(2 8).
           MOVE QTY-O2-2-8 TO TY-O2(2 8).
           MOVE QTY-INT-2-8 TO TY-INT(2 8).
           MOVE QTY-NO-2-9 TO TY-NO(2 9).
           MOVE QTY-AMT-2-9 TO TY-AMT(2 9).
           MOVE QTY-O2-2-9 TO TY-O2(2 9).
           MOVE QTY-INT-2-9 TO TY-INT(2 9).
           MOVE QTY-NO-2-10 TO TY-NO(2 10).
           MOVE QTY-AMT-2-10 TO TY-AMT(2 10).
           MOVE QTY-O2-2-10 TO TY-O2(2 10).
           MOVE QTY-INT-2-10 TO TY-INT(2 10).
           MOVE QTY-NO-2-11 TO TY-NO(2 11).
           MOVE QTY-AMT-2-11 TO TY-AMT(2 11).
           MOVE QTY-O2-2-11 TO TY-O2(2 11).
           MOVE QTY-INT-2-11 TO TY-INT(2 11).
           MOVE QTY-NO-REPO-1-1 TO TY-NO-REPO(1 1).
           MOVE QTY-AMT-REPO-1-1 TO TY-AMT-REPO(1 1).
           MOVE QTY-O2-REPO-1-1 TO TY-O2-REPO(1 1).
           MOVE QTY-INT-REPO-1-1 TO TY-INT-REPO(1 1).
           MOVE QTY-NO-REPO-1-2 TO TY-NO-REPO(1 2).
           MOVE QTY-AMT-REPO-1-2 TO TY-AMT-REPO(1 2).
           MOVE QTY-O2-REPO-1-2 TO TY-O2-REPO(1 2).
           MOVE QTY-INT-REPO-1-2 TO TY-INT-REPO(1 2).
           MOVE QTY-NO-REPO-1-3 TO TY-NO-REPO(1 3).
           MOVE QTY-AMT-REPO-1-3 TO TY-AMT-REPO(1 3).
           MOVE QTY-O2-REPO-1-3 TO TY-O2-REPO(1 3).
           MOVE QTY-INT-REPO-1-3 TO TY-INT-REPO(1 3).
           MOVE QTY-NO-REPO-1-4 TO TY-NO-REPO(1 4).
           MOVE QTY-AMT-REPO-1-4 TO TY-AMT-REPO(1 4).
           MOVE QTY-O2-REPO-1-4 TO TY-O2-REPO(1 4).
           MOVE QTY-INT-REPO-1-4 TO TY-INT-REPO(1 4).
           MOVE QTY-NO-REPO-1-5 TO TY-NO-REPO(1 5).
           MOVE QTY-AMT-REPO-1-5 TO TY-AMT-REPO(1 5).
           MOVE QTY-O2-REPO-1-5 TO TY-O2-REPO(1 5).
           MOVE QTY-INT-REPO-1-5 TO TY-INT-REPO(1 5).
           MOVE QTY-NO-REPO-2-1 TO TY-NO-REPO(2 1).
           MOVE QTY-AMT-REPO-2-1 TO TY-AMT-REPO(2 1).
           MOVE QTY-O2-REPO-2-1 TO TY-O2-REPO(2 1).
           MOVE QTY-INT-REPO-2-1 TO TY-INT-REPO(2 1).
           MOVE QTY-NO-REPO-2-2 TO TY-NO-REPO(2 2).
           MOVE QTY-AMT-REPO-2-2 TO TY-AMT-REPO(2 2).
           MOVE QTY-O2-REPO-2-2 TO TY-O2-REPO(2 2).
           MOVE QTY-INT-REPO-2-2 TO TY-INT-REPO(2 2).
           MOVE QTY-NO-REPO-2-3 TO TY-NO-REPO(2 3).
           MOVE QTY-AMT-REPO-2-3 TO TY-AMT-REPO(2 3).
           MOVE QTY-O2-REPO-2-3 TO TY-O2-REPO(2 3).
           MOVE QTY-INT-REPO-2-3 TO TY-INT-REPO(2 3).
           MOVE QTY-NO-REPO-2-4 TO TY-NO-REPO(2 4).
           MOVE QTY-AMT-REPO-2-4 TO TY-AMT-REPO(2 4).
           MOVE QTY-O2-REPO-2-4 TO TY-O2-REPO(2 4).
           MOVE QTY-INT-REPO-2-4 TO TY-INT-REPO(2 4).
           MOVE QTY-NO-REPO-2-5 TO TY-NO-REPO(2 5).
           MOVE QTY-AMT-REPO-2-5 TO TY-AMT-REPO(2 5).
           MOVE QTY-O2-REPO-2-5 TO TY-O2-REPO(2 5).
           MOVE QTY-INT-REPO-2-5 TO TY-INT-REPO(2 5).
           MOVE QTY-NO-PL-1-1 TO TY-NO-PL(1 1).
           MOVE QTY-AMT-PL-1-1 TO TY-AMT-PL(1 1).
           MOVE QTY-O2-PL-1-1 TO TY-O2-PL(1 1).
           MOVE QTY-INT-PL-1-1 TO TY-INT-PL(1 1).
           MOVE QTY-NO-PL-1-2 TO TY-NO-PL(1 2).
           MOVE QTY-AMT-PL-1-2 TO TY-AMT-PL(1 2).
           MOVE QTY-O2-PL-1-2 TO TY-O2-PL(1 2).
           MOVE QTY-INT-PL-1-2 TO TY-INT-PL(1 2).
           MOVE QTY-NO-PL-1-3 TO TY-NO-PL(1 3).
           MOVE QTY-AMT-PL-1-3 TO TY-AMT-PL(1 3).
           MOVE QTY-O2-PL-1-3 TO TY-O2-PL(1 3).
           MOVE QTY-INT-PL-1-3 TO TY-INT-PL(1 3).
           MOVE QTY-NO-PL-1-4 TO TY-NO-PL(1 4).
           MOVE QTY-AMT-PL-1-4 TO TY-AMT-PL(1 4).
           MOVE QTY-O2-PL-1-4 TO TY-O2-PL(1 4).
           MOVE QTY-INT-PL-1-4 TO TY-INT-PL(1 4).
           MOVE QTY-NO-PL-1-5 TO TY-NO-PL(1 5).
           MOVE QTY-AMT-PL-1-5 TO TY-AMT-PL(1 5).
           MOVE QTY-O2-PL-1-5 TO TY-O2-PL(1 5).
           MOVE QTY-INT-PL-1-5 TO TY-INT-PL(1 5).
           MOVE QTY-NO-PL-1-6 TO TY-NO-PL(1 6).
           MOVE QTY-AMT-PL-1-6 TO TY-AMT-PL(1 6).
           MOVE QTY-O2-PL-1-6 TO TY-O2-PL(1 6).
           MOVE QTY-INT-PL-1-6 TO TY-INT-PL(1 6).
           MOVE QTY-NO-PL-1-7 TO TY-NO-PL(1 7).
           MOVE QTY-AMT-PL-1-7 TO TY-AMT-PL(1 7).
           MOVE QTY-O2-PL-1-7 TO TY-O2-PL(1 7).
           MOVE QTY-INT-PL-1-7 TO TY-INT-PL(1 7).
           MOVE QTY-NO-PL-1-8 TO TY-NO-PL(1 8).
           MOVE QTY-AMT-PL-1-8 TO TY-AMT-PL(1 8).
           MOVE QTY-O2-PL-1-8 TO TY-O2-PL(1 8).
           MOVE QTY-INT-PL-1-8 TO TY-INT-PL(1 8).
           MOVE QTY-NO-PL-1-9 TO TY-NO-PL(1 9).
           MOVE QTY-AMT-PL-1-9 TO TY-AMT-PL(1 9).
           MOVE QTY-O2-PL-1-9 TO TY-O2-PL(1 9).
           MOVE QTY-INT-PL-1-9 TO TY-INT-PL(1 9).
           MOVE QTY-NO-PL-1-10 TO TY-NO-PL(1 10).
           MOVE QTY-AMT-PL-1-10 TO TY-AMT-PL(1 10).
           MOVE QTY-O2-PL-1-10 TO TY-O2-PL(1 10).
           MOVE QTY-INT-PL-1-10 TO TY-INT-PL(1 10).
           MOVE QTY-NO-PL-2-1 TO TY-NO-PL(2 1).
           MOVE QTY-AMT-PL-2-1 TO TY-AMT-PL(2 1).
           MOVE QTY-O2-PL-2-1 TO TY-O2-PL(2 1).
           MOVE QTY-INT-PL-2-1 TO TY-INT-PL(2 1).
           MOVE QTY-NO-PL-2-2 TO TY-NO-PL(2 2).
           MOVE QTY-AMT-PL-2-2 TO TY-AMT-PL(2 2).
           MOVE QTY-O2-PL-2-2 TO TY-O2-PL(2 2).
           MOVE QTY-INT-PL-2-2 TO TY-INT-PL(2 2).
           MOVE QTY-NO-PL-2-3 TO TY-NO-PL(2 3).
           MOVE QTY-AMT-PL-2-3 TO TY-AMT-PL(2 3).
           MOVE QTY-O2-PL-2-3 TO TY-O2-PL(2 3).
           MOVE QTY-INT-PL-2-3 TO TY-INT-PL(2 3).
           MOVE QTY-NO-PL-2-4 TO TY-NO-PL(2 4).
           MOVE QTY-AMT-PL-2-4 TO TY-AMT-PL(2 4).
           MOVE QTY-O2-PL-2-4 TO TY-O2-PL(2 4).
           MOVE QTY-INT-PL-2-4 TO TY-INT-PL(2 4).
           MOVE QTY-NO-PL-2-5 TO TY-NO-PL(2 5).
           MOVE QTY-AMT-PL-2-5 TO TY-AMT-PL(2 5).
           MOVE QTY-O2-PL-2-5 TO TY-O2-PL(2 5).
           MOVE QTY-INT-PL-2-5 TO TY-INT-PL(2 5).
           MOVE QTY-NO-PL-2-6 TO TY-NO-PL(2 6).
           MOVE QTY-AMT-PL-2-6 TO TY-AMT-PL(2 6).
           MOVE QTY-O2-PL-2-6 TO TY-O2-PL(2 6).
           MOVE QTY-INT-PL-2-6 TO TY-INT-PL(2 6).
           MOVE QTY-NO-PL-2-7 TO TY-NO-PL(2 7).
           MOVE QTY-AMT-PL-2-7 TO TY-AMT-PL(2 7).
           MOVE QTY-O2-PL-2-7 TO TY-O2-PL(2 7).
           MOVE QTY-INT-PL-2-7 TO TY-INT-PL(2 7).
           MOVE QTY-NO-PL-2-8 TO TY-NO-PL(2 8).
           MOVE QTY-AMT-PL-2-8 TO TY-AMT-PL(2 8).
           MOVE QTY-O2-PL-2-8 TO TY-O2-PL(2 8).
           MOVE QTY-INT-PL-2-8 TO TY-INT-PL(2 8).
           MOVE QTY-NO-PL-2-9 TO TY-NO-PL(2 9).
           MOVE QTY-AMT-PL-2-9 TO TY-AMT-PL(2 9).
           MOVE QTY-O2-PL-2-9 TO TY-O2-PL(2 9).
           MOVE QTY-INT-PL-2-9 TO TY-INT-PL(2 9).
           MOVE QTY-NO-PL-2-10 TO TY-NO-PL(2 10).
           MOVE QTY-AMT-PL-2-10 TO TY-AMT-PL(2 10).
           MOVE QTY-O2-PL-2-10 TO TY-O2-PL(2 10).
           MOVE QTY-INT-PL-2-10 TO TY-INT-PL(2 10).
           MOVE QTY-DLRVPRIN-1-1 TO TY-DLRVPRIN(1 1).
           MOVE QTY-DLRVPRIN-1-2 TO TY-DLRVPRIN(1 2).
           MOVE QTY-DLRVPRIN-1-3 TO TY-DLRVPRIN(1 3).
           MOVE QTY-DLRVCHGS-1-1 TO TY-DLRVCHGS(1 1).
           MOVE QTY-DLRVCHGS-1-2 TO TY-DLRVCHGS(1 2).
           MOVE QTY-DLRVCHGS-1-3 TO TY-DLRVCHGS(1 3).
           MOVE QTY-DLPARVHELD-1 TO TY-DLPARVHELD(1).
           MOVE QTY-DLRVPRIN-2-1 TO TY-DLRVPRIN(2 1).
           MOVE QTY-DLRVPRIN-2-2 TO TY-DLRVPRIN(2 2).
           MOVE QTY-DLRVPRIN-2-3 TO TY-DLRVPRIN(2 3).
           MOVE QTY-DLRVCHGS-2-1 TO TY-DLRVCHGS(2 1).
           MOVE QTY-DLRVCHGS-2-2 TO TY-DLRVCHGS(2 2).
           MOVE QTY-DLRVCHGS-2-3 TO TY-DLRVCHGS(2 3).
           MOVE QTY-DLPARVHELD-2 TO TY-DLPARVHELD(2).
           MOVE QTY-PL-PRINCOL-1 TO TY-PL-PRINCOL(1).
           MOVE QTY-PL-INTCOL-1 TO TY-PL-INTCOL(1).
           MOVE QTY-PL-LATECOL-1 TO TY-PL-LATECOL(1).
           MOVE QTY-PL-O2COL-1 TO TY-PL-O2COL(1).
           MOVE QTY-PL-PRINCOL-2 TO TY-PL-PRINCOL(2).
           MOVE QTY-PL-INTCOL-2 TO TY-PL-INTCOL(2).
           MOVE QTY-PL-LATECOL-2 TO TY-PL-LATECOL(2).
           MOVE QTY-PL-O2COL-2 TO TY-PL-O2COL(2).
           MOVE QTY-PL-PRINCOL-3 TO TY-PL-PRINCOL(3).
           MOVE QTY-PL-INTCOL-3 TO TY-PL-INTCOL(3).
           MOVE QTY-PL-LATECOL-3 TO TY-PL-LATECOL(3).
           MOVE QTY-PL-O2COL-3 TO TY-PL-O2COL(3).
           MOVE QTY-PL-PRINCOL-4 TO TY-PL-PRINCOL(4).
           MOVE QTY-PL-INTCOL-4 TO TY-PL-INTCOL(4).
           MOVE QTY-PL-LATECOL-4 TO TY-PL-LATECOL(4).
           MOVE QTY-PL-O2COL-4 TO TY-PL-O2COL(4).
           MOVE QTY-PL-PRINCOL-5 TO TY-PL-PRINCOL(5).
           MOVE QTY-PL-INTCOL-5 TO TY-PL-INTCOL(5).
           MOVE QTY-PL-LATECOL-5 TO TY-PL-LATECOL(5).
           MOVE QTY-PL-O2COL-5 TO TY-PL-O2COL(5).
           MOVE QTY-AMT-SOLD TO TY-AMT-SOLD.
           MOVE QTY-AMT-TRANS TO TY-AMT-TRANS.
           MOVE QTY-AMT-PURCH TO TY-AMT-PURCH.
           MOVE QTY-NO-SOLD TO TY-NO-SOLD.
           MOVE QTY-NO-TRANS TO TY-NO-TRANS.
           MOVE QTY-NO-PURCH TO TY-NO-PURCH.
           MOVE QTY-OTHCOL TO TY-OTHCOL.
           MOVE QTY-OT2COL TO TY-OT2COL.
           MOVE QTY-INTCOL TO TY-INTCOL.
           MOVE QTY-LATECOL TO TY-LATECOL.
           MOVE QTY-PRINCOL TO TY-PRINCOL.
           MOVE QTY-DEFCOL TO TY-DEFCOL.
           MOVE QTY-RNPRIN TO TY-RNPRIN.
           MOVE QTY-INTREB TO TY-INTREB.
           MOVE QTY-CLREB1 TO TY-CLREB1.
           MOVE QTY-CLREB2 TO TY-CLREB2.
           MOVE QTY-CLREB3 TO TY-CLREB3.
           MOVE QTY-CLREB4 TO TY-CLREB4.
           MOVE QTY-AHREB TO TY-AHREB.
           MOVE QTY-PPREB TO TY-PPREB.
           MOVE QTY-O1REB TO TY-O1REB.
           MOVE QTY-O2REB TO TY-O2REB.
           MOVE QTY-O3REB TO TY-O3REB.
           MOVE QTY-O4REB TO TY-O4REB.
           MOVE QTY-O5REB TO TY-O5REB.
           MOVE QTY-O6REB TO TY-O6REB.
           MOVE QTY-O7REB TO TY-O7REB.
           MOVE QTY-SCREB TO TY-SCREB.
           MOVE QTY-MFREB TO TY-MFREB.
           MOVE QTY-DFREB TO TY-DFREB.
           MOVE QTY-UN7-REB TO TY-UN7-REB.
           MOVE QTY-UN8-REB TO TY-UN8-REB.
           MOVE QTY-UN9-REB TO TY-UN9-REB.
           MOVE QTY-UN10-REB TO TY-UN10-REB.
           MOVE QTY-PART-CHGOFF TO TY-PART-CHGOFF.
           MOVE QTY-INS-NO-ELIG-1 TO TY-INS-NO-ELIG(1).
           MOVE QTY-INS-NO-WROTE-1 TO TY-INS-NO-WROTE(1).
           MOVE QTY-INS-NO-NOTWROTE-1 TO TY-INS-NO-NOTWROTE(1).
           MOVE QTY-INS-AMT-WROTE-1 TO TY-INS-AMT-WROTE(1).
           MOVE QTY-INS-NO-ELIG-2 TO TY-INS-NO-ELIG(2).
           MOVE QTY-INS-NO-WROTE-2 TO TY-INS-NO-WROTE(2).
           MOVE QTY-INS-NO-NOTWROTE-2 TO TY-INS-NO-NOTWROTE(2).
           MOVE QTY-INS-AMT-WROTE-2 TO TY-INS-AMT-WROTE(2).
           MOVE QTY-INS-NO-ELIG-3 TO TY-INS-NO-ELIG(3).
           MOVE QTY-INS-NO-WROTE-3 TO TY-INS-NO-WROTE(3).
           MOVE QTY-INS-NO-NOTWROTE-3 TO TY-INS-NO-NOTWROTE(3).
           MOVE QTY-INS-AMT-WROTE-3 TO TY-INS-AMT-WROTE(3).
           MOVE QTY-INS-NO-ELIG-4 TO TY-INS-NO-ELIG(4).
           MOVE QTY-INS-NO-WROTE-4 TO TY-INS-NO-WROTE(4).
           MOVE QTY-INS-NO-NOTWROTE-4 TO TY-INS-NO-NOTWROTE(4).
           MOVE QTY-INS-AMT-WROTE-4 TO TY-INS-AMT-WROTE(4).
           MOVE QTY-INS-NO-ELIG-5 TO TY-INS-NO-ELIG(5).
           MOVE QTY-INS-NO-WROTE-5 TO TY-INS-NO-WROTE(5).
           MOVE QTY-INS-NO-NOTWROTE-5 TO TY-INS-NO-NOTWROTE(5).
           MOVE QTY-INS-AMT-WROTE-5 TO TY-INS-AMT-WROTE(5).
           MOVE QTY-INS-NO-ELIG-6 TO TY-INS-NO-ELIG(6).
           MOVE QTY-INS-NO-WROTE-6 TO TY-INS-NO-WROTE(6).
           MOVE QTY-INS-NO-NOTWROTE-6 TO TY-INS-NO-NOTWROTE(6).
           MOVE QTY-INS-AMT-WROTE-6 TO TY-INS-AMT-WROTE(6).
           MOVE QTY-INS-NO-ELIG-7 TO TY-INS-NO-ELIG(7).
           MOVE QTY-INS-NO-WROTE-7 TO TY-INS-NO-WROTE(7).
           MOVE QTY-INS-NO-NOTWROTE-7 TO TY-INS-NO-NOTWROTE(7).
           MOVE QTY-INS-AMT-WROTE-7 TO TY-INS-AMT-WROTE(7).
           MOVE QTY-INS-NO-ELIG-8 TO TY-INS-NO-ELIG(8).
           MOVE QTY-INS-NO-WROTE-8 TO TY-INS-NO-WROTE(8).
           MOVE QTY-INS-NO-NOTWROTE-8 TO TY-INS-NO-NOTWROTE(8).
           MOVE QTY-INS-AMT-WROTE-8 TO TY-INS-AMT-WROTE(8).
           MOVE QTY-INS-NO-ELIG-9 TO TY-INS-NO-ELIG(9).
           MOVE QTY-INS-NO-WROTE-9 TO TY-INS-NO-WROTE(9).
           MOVE QTY-INS-NO-NOTWROTE-9 TO TY-INS-NO-NOTWROTE(9).
           MOVE QTY-INS-AMT-WROTE-9 TO TY-INS-AMT-WROTE(9).
           MOVE QTY-INS-NO-ELIG-10 TO TY-INS-NO-ELIG(10).
           MOVE QTY-INS-NO-WROTE-10 TO TY-INS-NO-WROTE(10).
           MOVE QTY-INS-NO-NOTWROTE-10 TO TY-INS-NO-NOTWROTE(10).
           MOVE QTY-INS-AMT-WROTE-10 TO TY-INS-AMT-WROTE(10).
           MOVE QTY-FEE-NO-ELIG-1 TO TY-FEE-NO-ELIG(1).
           MOVE QTY-FEE-NO-WROTE-1 TO TY-FEE-NO-WROTE(1).
           MOVE QTY-FEE-NO-NOTWROTE-1 TO TY-FEE-NO-NOTWROTE(1).
           MOVE QTY-FEE-AMT-WROTE-1 TO TY-FEE-AMT-WROTE(1).
           MOVE QTY-FEE-NO-ELIG-2 TO TY-FEE-NO-ELIG(2).
           MOVE QTY-FEE-NO-WROTE-2 TO TY-FEE-NO-WROTE(2).
           MOVE QTY-FEE-NO-NOTWROTE-2 TO TY-FEE-NO-NOTWROTE(2).
           MOVE QTY-FEE-AMT-WROTE-2 TO TY-FEE-AMT-WROTE(2).
           MOVE QTY-FEE-NO-ELIG-3 TO TY-FEE-NO-ELIG(3).
           MOVE QTY-FEE-NO-WROTE-3 TO TY-FEE-NO-WROTE(3).
           MOVE QTY-FEE-NO-NOTWROTE-3 TO TY-FEE-NO-NOTWROTE(3).
           MOVE QTY-FEE-AMT-WROTE-3 TO TY-FEE-AMT-WROTE(3).
           MOVE QTY-OTH-NO-COL TO TY-OTH-NO-COL.
           MOVE QTY-OT2-NO-COL TO TY-OT2-NO-COL.
           MOVE QTY-INT-NO-COL TO TY-INT-NO-COL.
           MOVE QTY-LATE-NO-COL TO TY-LATE-NO-COL.
           MOVE QTY-OTH-NO-WAIVED TO TY-OTH-NO-WAIVED.
           MOVE QTY-OT2-NO-WAIVED TO TY-OT2-NO-WAIVED.
           MOVE QTY-INT-NO-WAIVED TO TY-INT-NO-WAIVED.
           MOVE QTY-LATE-NO-WAIVED TO TY-LATE-NO-WAIVED.
           MOVE QTY-OTH-AMT-WAIVED TO TY-OTH-AMT-WAIVED.
           MOVE QTY-OT2-AMT-WAIVED TO TY-OT2-AMT-WAIVED.
           MOVE QTY-INT-AMT-WAIVED TO TY-INT-AMT-WAIVED.
           MOVE QTY-LATE-AMT-WAIVED TO TY-LATE-AMT-WAIVED.
           MOVE QTY-CKWRITTEN-1 TO TY-CKWRITTEN(1).
           MOVE QTY-CKWRITTEN-2 TO TY-CKWRITTEN(2).
           MOVE QTY-CKWRITTEN-3 TO TY-CKWRITTEN(3).
           MOVE QTY-CKWRITTEN-4 TO TY-CKWRITTEN(4).
           MOVE QTY-CKWRITTEN-5 TO TY-CKWRITTEN(5).
           MOVE QTY-CKWRITTEN-6 TO TY-CKWRITTEN(6).
           MOVE QTY-CKWRITTEN-7 TO TY-CKWRITTEN(7).
           MOVE QTY-CKWRITTEN-8 TO TY-CKWRITTEN(8).
           MOVE QTY-CKWRITTEN-9 TO TY-CKWRITTEN(9).
           MOVE QTY-CKWRITTEN-10 TO TY-CKWRITTEN(10).
           MOVE QTY-CKWRITTEN-11 TO TY-CKWRITTEN(11).
           MOVE QTY-CKWRITTEN-12 TO TY-CKWRITTEN(12).
           MOVE QTY-CKWRITTEN-13 TO TY-CKWRITTEN(13).
           MOVE QTY-CKWRITTEN-14 TO TY-CKWRITTEN(14).
           MOVE QTY-CKWRITTEN-15 TO TY-CKWRITTEN(15).
           MOVE QTY-CKWRITTEN-16 TO TY-CKWRITTEN(16).
           MOVE QTY-UNITS-1-1 TO TY-UNITS(1 1).
           MOVE QTY-OUTBAL-1-1 TO TY-OUTBAL(1 1).
           MOVE QTY-UNITS-1-2 TO TY-UNITS(1 2).
           MOVE QTY-OUTBAL-1-2 TO TY-OUTBAL(1 2).
           MOVE QTY-UNITS-1-3 TO TY-UNITS(1 3).
           MOVE QTY-OUTBAL-1-3 TO TY-OUTBAL(1 3).
           MOVE QTY-UNITS-1-4 TO TY-UNITS(1 4).
           MOVE QTY-OUTBAL-1-4 TO TY-OUTBAL(1 4).
           MOVE QTY-UNITS-1-5 TO TY-UNITS(1 5).
           MOVE QTY-OUTBAL-1-5 TO TY-OUTBAL(1 5).
           MOVE QTY-UNITS-1-6 TO TY-UNITS(1 6).
           MOVE QTY-OUTBAL-1-6 TO TY-OUTBAL(1 6).
           MOVE QTY-UNITS-1-7 TO TY-UNITS(1 7).
           MOVE QTY-OUTBAL-1-7 TO TY-OUTBAL(1 7).
           MOVE QTY-UNITS-1-8 TO TY-UNITS(1 8).
           MOVE QTY-OUTBAL-1-8 TO TY-OUTBAL(1 8).
           MOVE QTY-UNITS-1-9 TO TY-UNITS(1 9).
           MOVE QTY-OUTBAL-1-9 TO TY-OUTBAL(1 9).
           MOVE QTY-UNITS-1-10 TO TY-UNITS(1 10).
           MOVE QTY-OUTBAL-1-10 TO TY-OUTBAL(1 10).
           MOVE QTY-UNITS-2-1 TO TY-UNITS(2 1).
           MOVE QTY-OUTBAL-2-1 TO TY-OUTBAL(2 1).
           MOVE QTY-UNITS-2-2 TO TY-UNITS(2 2).
           MOVE QTY-OUTBAL-2-2 TO TY-OUTBAL(2 2).
           MOVE QTY-UNITS-2-3 TO TY-UNITS(2 3).
           MOVE QTY-OUTBAL-2-3 TO TY-OUTBAL(2 3).
           MOVE QTY-UNITS-2-4 TO TY-UNITS(2 4).
           MOVE QTY-OUTBAL-2-4 TO TY-OUTBAL(2 4).
           MOVE QTY-UNITS-2-5 TO TY-UNITS(2 5).
           MOVE QTY-OUTBAL-2-5 TO TY-OUTBAL(2 5).
           MOVE QTY-UNITS-2-6 TO TY-UNITS(2 6).
           MOVE QTY-OUTBAL-2-6 TO TY-OUTBAL(2 6).
           MOVE QTY-UNITS-2-7 TO TY-UNITS(2 7).
           MOVE QTY-OUTBAL-2-7 TO TY-OUTBAL(2 7).
           MOVE QTY-UNITS-2-8 TO TY-UNITS(2 8).
           MOVE QTY-OUTBAL-2-8 TO TY-OUTBAL(2 8).
           MOVE QTY-UNITS-2-9 TO TY-UNITS(2 9).
           MOVE QTY-OUTBAL-2-9 TO TY-OUTBAL(2 9).
           MOVE QTY-UNITS-2-10 TO TY-UNITS(2 10).
           MOVE QTY-OUTBAL-2-10 TO TY-OUTBAL(2 10).
           MOVE QTY-UNITS-BNKRPT-1-1 TO TY-UNITS-BNKRPT(1 1).
           MOVE QTY-OUTBAL-BNKRPT-1-1 TO TY-OUTBAL-BNKRPT(1 1).
           MOVE QTY-UNITS-BNKRPT-1-2 TO TY-UNITS-BNKRPT(1 2).
           MOVE QTY-OUTBAL-BNKRPT-1-2 TO TY-OUTBAL-BNKRPT(1 2).
           MOVE QTY-UNITS-BNKRPT-1-3 TO TY-UNITS-BNKRPT(1 3).
           MOVE QTY-OUTBAL-BNKRPT-1-3 TO TY-OUTBAL-BNKRPT(1 3).
           MOVE QTY-UNITS-BNKRPT-1-4 TO TY-UNITS-BNKRPT(1 4).
           MOVE QTY-OUTBAL-BNKRPT-1-4 TO TY-OUTBAL-BNKRPT(1 4).
           MOVE QTY-UNITS-BNKRPT-1-5 TO TY-UNITS-BNKRPT(1 5).
           MOVE QTY-OUTBAL-BNKRPT-1-5 TO TY-OUTBAL-BNKRPT(1 5).
           MOVE QTY-UNITS-BNKRPT-1-6 TO TY-UNITS-BNKRPT(1 6).
           MOVE QTY-OUTBAL-BNKRPT-1-6 TO TY-OUTBAL-BNKRPT(1 6).
           MOVE QTY-UNITS-BNKRPT-1-7 TO TY-UNITS-BNKRPT(1 7).
           MOVE QTY-OUTBAL-BNKRPT-1-7 TO TY-OUTBAL-BNKRPT(1 7).
           MOVE QTY-UNITS-BNKRPT-1-8 TO TY-UNITS-BNKRPT(1 8).
           MOVE QTY-OUTBAL-BNKRPT-1-8 TO TY-OUTBAL-BNKRPT(1 8).
           MOVE QTY-UNITS-BNKRPT-1-9 TO TY-UNITS-BNKRPT(1 9).
           MOVE QTY-OUTBAL-BNKRPT-1-9 TO TY-OUTBAL-BNKRPT(1 9).
           MOVE QTY-UNITS-BNKRPT-1-10 TO TY-UNITS-BNKRPT(1 10).
           MOVE QTY-OUTBAL-BNKRPT-1-10 TO TY-OUTBAL-BNKRPT(1 10).
           MOVE QTY-UNITS-BNKRPT-2-1 TO TY-UNITS-BNKRPT(2 1).
           MOVE QTY-OUTBAL-BNKRPT-2-1 TO TY-OUTBAL-BNKRPT(2 1).
           MOVE QTY-UNITS-BNKRPT-2-2 TO TY-UNITS-BNKRPT(2 2).
           MOVE QTY-OUTBAL-BNKRPT-2-2 TO TY-OUTBAL-BNKRPT(2 2).
           MOVE QTY-UNITS-BNKRPT-2-3 TO TY-UNITS-BNKRPT(2 3).
           MOVE QTY-OUTBAL-BNKRPT-2-3 TO TY-OUTBAL-BNKRPT(2 3).
           MOVE QTY-UNITS-BNKRPT-2-4 TO TY-UNITS-BNKRPT(2 4).
           MOVE QTY-OUTBAL-BNKRPT-2-4 TO TY-OUTBAL-BNKRPT(2 4).
           MOVE QTY-UNITS-BNKRPT-2-5 TO TY-UNITS-BNKRPT(2 5).
           MOVE QTY-OUTBAL-BNKRPT-2-5 TO TY-OUTBAL-BNKRPT(2 5).
           MOVE QTY-UNITS-BNKRPT-2-6 TO TY-UNITS-BNKRPT(2 6).
           MOVE QTY-OUTBAL-BNKRPT-2-6 TO TY-OUTBAL-BNKRPT(2 6).
           MOVE QTY-UNITS-BNKRPT-2-7 TO TY-UNITS-BNKRPT(2 7).
           MOVE QTY-OUTBAL-BNKRPT-2-7 TO TY-OUTBAL-BNKRPT(2 7).
           MOVE QTY-UNITS-BNKRPT-2-8 TO TY-UNITS-BNKRPT(2 8).
           MOVE QTY-OUTBAL-BNKRPT-2-8 TO TY-OUTBAL-BNKRPT(2 8).
           MOVE QTY-UNITS-BNKRPT-2-9 TO TY-UNITS-BNKRPT(2 9).
           MOVE QTY-OUTBAL-BNKRPT-2-9 TO TY-OUTBAL-BNKRPT(2 9).
           MOVE QTY-UNITS-BNKRPT-2-10 TO TY-UNITS-BNKRPT(2 10).
           MOVE QTY-OUTBAL-BNKRPT-2-10 TO TY-OUTBAL-BNKRPT(2 10).
           MOVE QTY-SUMWGTTM TO TY-SUMWGTTM.
           MOVE QTY-SUMWGTAV TO TY-SUMWGTAV.
           MOVE QTY-SUMWGTTA TO TY-SUMWGTTA.
           MOVE QTY-ALLOTMENTS TO TY-ALLOTMENTS.
           MOVE QTY-BRFG TO TY-BRFG.
           MOVE QTY-DLDISC TO TY-DLDISC.
           MOVE QTY-UNEARNED-AMT-1-1 TO TY-UNEARNED-AMT(1 1).
           MOVE QTY-UNEARNED-AMT-1-2 TO TY-UNEARNED-AMT(1 2).
           MOVE QTY-UNEARNED-AMT-1-3 TO TY-UNEARNED-AMT(1 3).
           MOVE QTY-UNEARNED-AMT-1-4 TO TY-UNEARNED-AMT(1 4).
           MOVE QTY-UNEARNED-AMT-1-5 TO TY-UNEARNED-AMT(1 5).
           MOVE QTY-UNEARNED-AMT-1-6 TO TY-UNEARNED-AMT(1 6).
           MOVE QTY-UNEARNED-AMT-1-7 TO TY-UNEARNED-AMT(1 7).
           MOVE QTY-UNEARNED-AMT-1-8 TO TY-UNEARNED-AMT(1 8).
           MOVE QTY-UNEARNED-AMT-1-9 TO TY-UNEARNED-AMT(1 9).
           MOVE QTY-UNEARNED-AMT-1-10 TO TY-UNEARNED-AMT(1 10).
           MOVE QTY-UNEARNED-AMT-2-1 TO TY-UNEARNED-AMT(2 1).
           MOVE QTY-UNEARNED-AMT-2-2 TO TY-UNEARNED-AMT(2 2).
           MOVE QTY-UNEARNED-AMT-2-3 TO TY-UNEARNED-AMT(2 3).
           MOVE QTY-UNEARNED-AMT-2-4 TO TY-UNEARNED-AMT(2 4).
           MOVE QTY-UNEARNED-AMT-2-5 TO TY-UNEARNED-AMT(2 5).
           MOVE QTY-UNEARNED-AMT-2-6 TO TY-UNEARNED-AMT(2 6).
           MOVE QTY-UNEARNED-AMT-2-7 TO TY-UNEARNED-AMT(2 7).
           MOVE QTY-UNEARNED-AMT-2-8 TO TY-UNEARNED-AMT(2 8).
           MOVE QTY-UNEARNED-AMT-2-9 TO TY-UNEARNED-AMT(2 9).
           MOVE QTY-UNEARNED-AMT-2-10 TO TY-UNEARNED-AMT(2 10).
           MOVE QTY-PROCEEDS TO TY-PROCEEDS.
           MOVE QTY-LOCKBOX TO TY-LOCKBOX.
 
      *-----------------------------------------------------------------
      *    LOAD QTY-REC FROM TY-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-TY-FIELDS.
           MOVE TY-BRNO TO QTY-BRNO.

           IF ( TY-DATE = ZEROES )
              MOVE "1900-01-01" TO QTY-DATE
           ELSE
              MOVE TY-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTY-DATE.

           MOVE TY-CLASS TO QTY-CLASS.

           IF ( TY-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTY-LTOUCH-DATE
           ELSE
              MOVE TY-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTY-LTOUCH-DATE.

           MOVE TY-NO(1 1) TO QTY-NO-1-1.
           MOVE TY-AMT(1 1) TO QTY-AMT-1-1.
           MOVE TY-O2(1 1) TO QTY-O2-1-1.
           MOVE TY-INT(1 1) TO QTY-INT-1-1.
           MOVE TY-NO(1 2) TO QTY-NO-1-2.
           MOVE TY-AMT(1 2) TO QTY-AMT-1-2.
           MOVE TY-O2(1 2) TO QTY-O2-1-2.
           MOVE TY-INT(1 2) TO QTY-INT-1-2.
           MOVE TY-NO(1 3) TO QTY-NO-1-3.
           MOVE TY-AMT(1 3) TO QTY-AMT-1-3.
           MOVE TY-O2(1 3) TO QTY-O2-1-3.
           MOVE TY-INT(1 3) TO QTY-INT-1-3.
           MOVE TY-NO(1 4) TO QTY-NO-1-4.
           MOVE TY-AMT(1 4) TO QTY-AMT-1-4.
           MOVE TY-O2(1 4) TO QTY-O2-1-4.
           MOVE TY-INT(1 4) TO QTY-INT-1-4.
           MOVE TY-NO(1 5) TO QTY-NO-1-5.
           MOVE TY-AMT(1 5) TO QTY-AMT-1-5.
           MOVE TY-O2(1 5) TO QTY-O2-1-5.
           MOVE TY-INT(1 5) TO QTY-INT-1-5.
           MOVE TY-NO(1 6) TO QTY-NO-1-6.
           MOVE TY-AMT(1 6) TO QTY-AMT-1-6.
           MOVE TY-O2(1 6) TO QTY-O2-1-6.
           MOVE TY-INT(1 6) TO QTY-INT-1-6.
           MOVE TY-NO(1 7) TO QTY-NO-1-7.
           MOVE TY-AMT(1 7) TO QTY-AMT-1-7.
           MOVE TY-O2(1 7) TO QTY-O2-1-7.
           MOVE TY-INT(1 7) TO QTY-INT-1-7.
           MOVE TY-NO(1 8) TO QTY-NO-1-8.
           MOVE TY-AMT(1 8) TO QTY-AMT-1-8.
           MOVE TY-O2(1 8) TO QTY-O2-1-8.
           MOVE TY-INT(1 8) TO QTY-INT-1-8.
           MOVE TY-NO(1 9) TO QTY-NO-1-9.
           MOVE TY-AMT(1 9) TO QTY-AMT-1-9.
           MOVE TY-O2(1 9) TO QTY-O2-1-9.
           MOVE TY-INT(1 9) TO QTY-INT-1-9.
           MOVE TY-NO(1 10) TO QTY-NO-1-10.
           MOVE TY-AMT(1 10) TO QTY-AMT-1-10.
           MOVE TY-O2(1 10) TO QTY-O2-1-10.
           MOVE TY-INT(1 10) TO QTY-INT-1-10.
           MOVE TY-NO(1 11) TO QTY-NO-1-11.
           MOVE TY-AMT(1 11) TO QTY-AMT-1-11.
           MOVE TY-O2(1 11) TO QTY-O2-1-11.
           MOVE TY-INT(1 11) TO QTY-INT-1-11.
           MOVE TY-NO(2 1) TO QTY-NO-2-1.
           MOVE TY-AMT(2 1) TO QTY-AMT-2-1.
           MOVE TY-O2(2 1) TO QTY-O2-2-1.
           MOVE TY-INT(2 1) TO QTY-INT-2-1.
           MOVE TY-NO(2 2) TO QTY-NO-2-2.
           MOVE TY-AMT(2 2) TO QTY-AMT-2-2.
           MOVE TY-O2(2 2) TO QTY-O2-2-2.
           MOVE TY-INT(2 2) TO QTY-INT-2-2.
           MOVE TY-NO(2 3) TO QTY-NO-2-3.
           MOVE TY-AMT(2 3) TO QTY-AMT-2-3.
           MOVE TY-O2(2 3) TO QTY-O2-2-3.
           MOVE TY-INT(2 3) TO QTY-INT-2-3.
           MOVE TY-NO(2 4) TO QTY-NO-2-4.
           MOVE TY-AMT(2 4) TO QTY-AMT-2-4.
           MOVE TY-O2(2 4) TO QTY-O2-2-4.
           MOVE TY-INT(2 4) TO QTY-INT-2-4.
           MOVE TY-NO(2 5) TO QTY-NO-2-5.
           MOVE TY-AMT(2 5) TO QTY-AMT-2-5.
           MOVE TY-O2(2 5) TO QTY-O2-2-5.
           MOVE TY-INT(2 5) TO QTY-INT-2-5.
           MOVE TY-NO(2 6) TO QTY-NO-2-6.
           MOVE TY-AMT(2 6) TO QTY-AMT-2-6.
           MOVE TY-O2(2 6) TO QTY-O2-2-6.
           MOVE TY-INT(2 6) TO QTY-INT-2-6.
           MOVE TY-NO(2 7) TO QTY-NO-2-7.
           MOVE TY-AMT(2 7) TO QTY-AMT-2-7.
           MOVE TY-O2(2 7) TO QTY-O2-2-7.
           MOVE TY-INT(2 7) TO QTY-INT-2-7.
           MOVE TY-NO(2 8) TO QTY-NO-2-8.
           MOVE TY-AMT(2 8) TO QTY-AMT-2-8.
           MOVE TY-O2(2 8) TO QTY-O2-2-8.
           MOVE TY-INT(2 8) TO QTY-INT-2-8.
           MOVE TY-NO(2 9) TO QTY-NO-2-9.
           MOVE TY-AMT(2 9) TO QTY-AMT-2-9.
           MOVE TY-O2(2 9) TO QTY-O2-2-9.
           MOVE TY-INT(2 9) TO QTY-INT-2-9.
           MOVE TY-NO(2 10) TO QTY-NO-2-10.
           MOVE TY-AMT(2 10) TO QTY-AMT-2-10.
           MOVE TY-O2(2 10) TO QTY-O2-2-10.
           MOVE TY-INT(2 10) TO QTY-INT-2-10.
           MOVE TY-NO(2 11) TO QTY-NO-2-11.
           MOVE TY-AMT(2 11) TO QTY-AMT-2-11.
           MOVE TY-O2(2 11) TO QTY-O2-2-11.
           MOVE TY-INT(2 11) TO QTY-INT-2-11.
           MOVE TY-NO-REPO(1 1) TO QTY-NO-REPO-1-1.
           MOVE TY-AMT-REPO(1 1) TO QTY-AMT-REPO-1-1.
           MOVE TY-O2-REPO(1 1) TO QTY-O2-REPO-1-1.
           MOVE TY-INT-REPO(1 1) TO QTY-INT-REPO-1-1.
           MOVE TY-NO-REPO(1 2) TO QTY-NO-REPO-1-2.
           MOVE TY-AMT-REPO(1 2) TO QTY-AMT-REPO-1-2.
           MOVE TY-O2-REPO(1 2) TO QTY-O2-REPO-1-2.
           MOVE TY-INT-REPO(1 2) TO QTY-INT-REPO-1-2.
           MOVE TY-NO-REPO(1 3) TO QTY-NO-REPO-1-3.
           MOVE TY-AMT-REPO(1 3) TO QTY-AMT-REPO-1-3.
           MOVE TY-O2-REPO(1 3) TO QTY-O2-REPO-1-3.
           MOVE TY-INT-REPO(1 3) TO QTY-INT-REPO-1-3.
           MOVE TY-NO-REPO(1 4) TO QTY-NO-REPO-1-4.
           MOVE TY-AMT-REPO(1 4) TO QTY-AMT-REPO-1-4.
           MOVE TY-O2-REPO(1 4) TO QTY-O2-REPO-1-4.
           MOVE TY-INT-REPO(1 4) TO QTY-INT-REPO-1-4.
           MOVE TY-NO-REPO(1 5) TO QTY-NO-REPO-1-5.
           MOVE TY-AMT-REPO(1 5) TO QTY-AMT-REPO-1-5.
           MOVE TY-O2-REPO(1 5) TO QTY-O2-REPO-1-5.
           MOVE TY-INT-REPO(1 5) TO QTY-INT-REPO-1-5.
           MOVE TY-NO-REPO(2 1) TO QTY-NO-REPO-2-1.
           MOVE TY-AMT-REPO(2 1) TO QTY-AMT-REPO-2-1.
           MOVE TY-O2-REPO(2 1) TO QTY-O2-REPO-2-1.
           MOVE TY-INT-REPO(2 1) TO QTY-INT-REPO-2-1.
           MOVE TY-NO-REPO(2 2) TO QTY-NO-REPO-2-2.
           MOVE TY-AMT-REPO(2 2) TO QTY-AMT-REPO-2-2.
           MOVE TY-O2-REPO(2 2) TO QTY-O2-REPO-2-2.
           MOVE TY-INT-REPO(2 2) TO QTY-INT-REPO-2-2.
           MOVE TY-NO-REPO(2 3) TO QTY-NO-REPO-2-3.
           MOVE TY-AMT-REPO(2 3) TO QTY-AMT-REPO-2-3.
           MOVE TY-O2-REPO(2 3) TO QTY-O2-REPO-2-3.
           MOVE TY-INT-REPO(2 3) TO QTY-INT-REPO-2-3.
           MOVE TY-NO-REPO(2 4) TO QTY-NO-REPO-2-4.
           MOVE TY-AMT-REPO(2 4) TO QTY-AMT-REPO-2-4.
           MOVE TY-O2-REPO(2 4) TO QTY-O2-REPO-2-4.
           MOVE TY-INT-REPO(2 4) TO QTY-INT-REPO-2-4.
           MOVE TY-NO-REPO(2 5) TO QTY-NO-REPO-2-5.
           MOVE TY-AMT-REPO(2 5) TO QTY-AMT-REPO-2-5.
           MOVE TY-O2-REPO(2 5) TO QTY-O2-REPO-2-5.
           MOVE TY-INT-REPO(2 5) TO QTY-INT-REPO-2-5.
           MOVE TY-NO-PL(1 1) TO QTY-NO-PL-1-1.
           MOVE TY-AMT-PL(1 1) TO QTY-AMT-PL-1-1.
           MOVE TY-O2-PL(1 1) TO QTY-O2-PL-1-1.
           MOVE TY-INT-PL(1 1) TO QTY-INT-PL-1-1.
           MOVE TY-NO-PL(1 2) TO QTY-NO-PL-1-2.
           MOVE TY-AMT-PL(1 2) TO QTY-AMT-PL-1-2.
           MOVE TY-O2-PL(1 2) TO QTY-O2-PL-1-2.
           MOVE TY-INT-PL(1 2) TO QTY-INT-PL-1-2.
           MOVE TY-NO-PL(1 3) TO QTY-NO-PL-1-3.
           MOVE TY-AMT-PL(1 3) TO QTY-AMT-PL-1-3.
           MOVE TY-O2-PL(1 3) TO QTY-O2-PL-1-3.
           MOVE TY-INT-PL(1 3) TO QTY-INT-PL-1-3.
           MOVE TY-NO-PL(1 4) TO QTY-NO-PL-1-4.
           MOVE TY-AMT-PL(1 4) TO QTY-AMT-PL-1-4.
           MOVE TY-O2-PL(1 4) TO QTY-O2-PL-1-4.
           MOVE TY-INT-PL(1 4) TO QTY-INT-PL-1-4.
           MOVE TY-NO-PL(1 5) TO QTY-NO-PL-1-5.
           MOVE TY-AMT-PL(1 5) TO QTY-AMT-PL-1-5.
           MOVE TY-O2-PL(1 5) TO QTY-O2-PL-1-5.
           MOVE TY-INT-PL(1 5) TO QTY-INT-PL-1-5.
           MOVE TY-NO-PL(1 6) TO QTY-NO-PL-1-6.
           MOVE TY-AMT-PL(1 6) TO QTY-AMT-PL-1-6.
           MOVE TY-O2-PL(1 6) TO QTY-O2-PL-1-6.
           MOVE TY-INT-PL(1 6) TO QTY-INT-PL-1-6.
           MOVE TY-NO-PL(1 7) TO QTY-NO-PL-1-7.
           MOVE TY-AMT-PL(1 7) TO QTY-AMT-PL-1-7.
           MOVE TY-O2-PL(1 7) TO QTY-O2-PL-1-7.
           MOVE TY-INT-PL(1 7) TO QTY-INT-PL-1-7.
           MOVE TY-NO-PL(1 8) TO QTY-NO-PL-1-8.
           MOVE TY-AMT-PL(1 8) TO QTY-AMT-PL-1-8.
           MOVE TY-O2-PL(1 8) TO QTY-O2-PL-1-8.
           MOVE TY-INT-PL(1 8) TO QTY-INT-PL-1-8.
           MOVE TY-NO-PL(1 9) TO QTY-NO-PL-1-9.
           MOVE TY-AMT-PL(1 9) TO QTY-AMT-PL-1-9.
           MOVE TY-O2-PL(1 9) TO QTY-O2-PL-1-9.
           MOVE TY-INT-PL(1 9) TO QTY-INT-PL-1-9.
           MOVE TY-NO-PL(1 10) TO QTY-NO-PL-1-10.
           MOVE TY-AMT-PL(1 10) TO QTY-AMT-PL-1-10.
           MOVE TY-O2-PL(1 10) TO QTY-O2-PL-1-10.
           MOVE TY-INT-PL(1 10) TO QTY-INT-PL-1-10.
           MOVE TY-NO-PL(2 1) TO QTY-NO-PL-2-1.
           MOVE TY-AMT-PL(2 1) TO QTY-AMT-PL-2-1.
           MOVE TY-O2-PL(2 1) TO QTY-O2-PL-2-1.
           MOVE TY-INT-PL(2 1) TO QTY-INT-PL-2-1.
           MOVE TY-NO-PL(2 2) TO QTY-NO-PL-2-2.
           MOVE TY-AMT-PL(2 2) TO QTY-AMT-PL-2-2.
           MOVE TY-O2-PL(2 2) TO QTY-O2-PL-2-2.
           MOVE TY-INT-PL(2 2) TO QTY-INT-PL-2-2.
           MOVE TY-NO-PL(2 3) TO QTY-NO-PL-2-3.
           MOVE TY-AMT-PL(2 3) TO QTY-AMT-PL-2-3.
           MOVE TY-O2-PL(2 3) TO QTY-O2-PL-2-3.
           MOVE TY-INT-PL(2 3) TO QTY-INT-PL-2-3.
           MOVE TY-NO-PL(2 4) TO QTY-NO-PL-2-4.
           MOVE TY-AMT-PL(2 4) TO QTY-AMT-PL-2-4.
           MOVE TY-O2-PL(2 4) TO QTY-O2-PL-2-4.
           MOVE TY-INT-PL(2 4) TO QTY-INT-PL-2-4.
           MOVE TY-NO-PL(2 5) TO QTY-NO-PL-2-5.
           MOVE TY-AMT-PL(2 5) TO QTY-AMT-PL-2-5.
           MOVE TY-O2-PL(2 5) TO QTY-O2-PL-2-5.
           MOVE TY-INT-PL(2 5) TO QTY-INT-PL-2-5.
           MOVE TY-NO-PL(2 6) TO QTY-NO-PL-2-6.
           MOVE TY-AMT-PL(2 6) TO QTY-AMT-PL-2-6.
           MOVE TY-O2-PL(2 6) TO QTY-O2-PL-2-6.
           MOVE TY-INT-PL(2 6) TO QTY-INT-PL-2-6.
           MOVE TY-NO-PL(2 7) TO QTY-NO-PL-2-7.
           MOVE TY-AMT-PL(2 7) TO QTY-AMT-PL-2-7.
           MOVE TY-O2-PL(2 7) TO QTY-O2-PL-2-7.
           MOVE TY-INT-PL(2 7) TO QTY-INT-PL-2-7.
           MOVE TY-NO-PL(2 8) TO QTY-NO-PL-2-8.
           MOVE TY-AMT-PL(2 8) TO QTY-AMT-PL-2-8.
           MOVE TY-O2-PL(2 8) TO QTY-O2-PL-2-8.
           MOVE TY-INT-PL(2 8) TO QTY-INT-PL-2-8.
           MOVE TY-NO-PL(2 9) TO QTY-NO-PL-2-9.
           MOVE TY-AMT-PL(2 9) TO QTY-AMT-PL-2-9.
           MOVE TY-O2-PL(2 9) TO QTY-O2-PL-2-9.
           MOVE TY-INT-PL(2 9) TO QTY-INT-PL-2-9.
           MOVE TY-NO-PL(2 10) TO QTY-NO-PL-2-10.
           MOVE TY-AMT-PL(2 10) TO QTY-AMT-PL-2-10.
           MOVE TY-O2-PL(2 10) TO QTY-O2-PL-2-10.
           MOVE TY-INT-PL(2 10) TO QTY-INT-PL-2-10.
           MOVE TY-DLRVPRIN(1 1) TO QTY-DLRVPRIN-1-1.
           MOVE TY-DLRVPRIN(1 2) TO QTY-DLRVPRIN-1-2.
           MOVE TY-DLRVPRIN(1 3) TO QTY-DLRVPRIN-1-3.
           MOVE TY-DLRVCHGS(1 1) TO QTY-DLRVCHGS-1-1.
           MOVE TY-DLRVCHGS(1 2) TO QTY-DLRVCHGS-1-2.
           MOVE TY-DLRVCHGS(1 3) TO QTY-DLRVCHGS-1-3.
           MOVE TY-DLPARVHELD(1) TO QTY-DLPARVHELD-1.
           MOVE TY-DLRVPRIN(2 1) TO QTY-DLRVPRIN-2-1.
           MOVE TY-DLRVPRIN(2 2) TO QTY-DLRVPRIN-2-2.
           MOVE TY-DLRVPRIN(2 3) TO QTY-DLRVPRIN-2-3.
           MOVE TY-DLRVCHGS(2 1) TO QTY-DLRVCHGS-2-1.
           MOVE TY-DLRVCHGS(2 2) TO QTY-DLRVCHGS-2-2.
           MOVE TY-DLRVCHGS(2 3) TO QTY-DLRVCHGS-2-3.
           MOVE TY-DLPARVHELD(2) TO QTY-DLPARVHELD-2.
           MOVE TY-PL-PRINCOL(1) TO QTY-PL-PRINCOL-1.
           MOVE TY-PL-INTCOL(1) TO QTY-PL-INTCOL-1.
           MOVE TY-PL-LATECOL(1) TO QTY-PL-LATECOL-1.
           MOVE TY-PL-O2COL(1) TO QTY-PL-O2COL-1.
           MOVE TY-PL-PRINCOL(2) TO QTY-PL-PRINCOL-2.
           MOVE TY-PL-INTCOL(2) TO QTY-PL-INTCOL-2.
           MOVE TY-PL-LATECOL(2) TO QTY-PL-LATECOL-2.
           MOVE TY-PL-O2COL(2) TO QTY-PL-O2COL-2.
           MOVE TY-PL-PRINCOL(3) TO QTY-PL-PRINCOL-3.
           MOVE TY-PL-INTCOL(3) TO QTY-PL-INTCOL-3.
           MOVE TY-PL-LATECOL(3) TO QTY-PL-LATECOL-3.
           MOVE TY-PL-O2COL(3) TO QTY-PL-O2COL-3.
           MOVE TY-PL-PRINCOL(4) TO QTY-PL-PRINCOL-4.
           MOVE TY-PL-INTCOL(4) TO QTY-PL-INTCOL-4.
           MOVE TY-PL-LATECOL(4) TO QTY-PL-LATECOL-4.
           MOVE TY-PL-O2COL(4) TO QTY-PL-O2COL-4.
           MOVE TY-PL-PRINCOL(5) TO QTY-PL-PRINCOL-5.
           MOVE TY-PL-INTCOL(5) TO QTY-PL-INTCOL-5.
           MOVE TY-PL-LATECOL(5) TO QTY-PL-LATECOL-5.
           MOVE TY-PL-O2COL(5) TO QTY-PL-O2COL-5.
           MOVE TY-AMT-SOLD TO QTY-AMT-SOLD.
           MOVE TY-AMT-TRANS TO QTY-AMT-TRANS.
           MOVE TY-AMT-PURCH TO QTY-AMT-PURCH.
           MOVE TY-NO-SOLD TO QTY-NO-SOLD.
           MOVE TY-NO-TRANS TO QTY-NO-TRANS.
           MOVE TY-NO-PURCH TO QTY-NO-PURCH.
           MOVE TY-OTHCOL TO QTY-OTHCOL.
           MOVE TY-OT2COL TO QTY-OT2COL.
           MOVE TY-INTCOL TO QTY-INTCOL.
           MOVE TY-LATECOL TO QTY-LATECOL.
           MOVE TY-PRINCOL TO QTY-PRINCOL.
           MOVE TY-DEFCOL TO QTY-DEFCOL.
           MOVE TY-RNPRIN TO QTY-RNPRIN.
           MOVE TY-INTREB TO QTY-INTREB.
           MOVE TY-CLREB1 TO QTY-CLREB1.
           MOVE TY-CLREB2 TO QTY-CLREB2.
           MOVE TY-CLREB3 TO QTY-CLREB3.
           MOVE TY-CLREB4 TO QTY-CLREB4.
           MOVE TY-AHREB TO QTY-AHREB.
           MOVE TY-PPREB TO QTY-PPREB.
           MOVE TY-O1REB TO QTY-O1REB.
           MOVE TY-O2REB TO QTY-O2REB.
           MOVE TY-O3REB TO QTY-O3REB.
           MOVE TY-O4REB TO QTY-O4REB.
           MOVE TY-O5REB TO QTY-O5REB.
           MOVE TY-O6REB TO QTY-O6REB.
           MOVE TY-O7REB TO QTY-O7REB.
           MOVE TY-SCREB TO QTY-SCREB.
           MOVE TY-MFREB TO QTY-MFREB.
           MOVE TY-DFREB TO QTY-DFREB.
           MOVE TY-UN7-REB TO QTY-UN7-REB.
           MOVE TY-UN8-REB TO QTY-UN8-REB.
           MOVE TY-UN9-REB TO QTY-UN9-REB.
           MOVE TY-UN10-REB TO QTY-UN10-REB.
           MOVE TY-PART-CHGOFF TO QTY-PART-CHGOFF.
           MOVE TY-INS-NO-ELIG(1) TO QTY-INS-NO-ELIG-1.
           MOVE TY-INS-NO-WROTE(1) TO QTY-INS-NO-WROTE-1.
           MOVE TY-INS-NO-NOTWROTE(1) TO QTY-INS-NO-NOTWROTE-1.
           MOVE TY-INS-AMT-WROTE(1) TO QTY-INS-AMT-WROTE-1.
           MOVE TY-INS-NO-ELIG(2) TO QTY-INS-NO-ELIG-2.
           MOVE TY-INS-NO-WROTE(2) TO QTY-INS-NO-WROTE-2.
           MOVE TY-INS-NO-NOTWROTE(2) TO QTY-INS-NO-NOTWROTE-2.
           MOVE TY-INS-AMT-WROTE(2) TO QTY-INS-AMT-WROTE-2.
           MOVE TY-INS-NO-ELIG(3) TO QTY-INS-NO-ELIG-3.
           MOVE TY-INS-NO-WROTE(3) TO QTY-INS-NO-WROTE-3.
           MOVE TY-INS-NO-NOTWROTE(3) TO QTY-INS-NO-NOTWROTE-3.
           MOVE TY-INS-AMT-WROTE(3) TO QTY-INS-AMT-WROTE-3.
           MOVE TY-INS-NO-ELIG(4) TO QTY-INS-NO-ELIG-4.
           MOVE TY-INS-NO-WROTE(4) TO QTY-INS-NO-WROTE-4.
           MOVE TY-INS-NO-NOTWROTE(4) TO QTY-INS-NO-NOTWROTE-4.
           MOVE TY-INS-AMT-WROTE(4) TO QTY-INS-AMT-WROTE-4.
           MOVE TY-INS-NO-ELIG(5) TO QTY-INS-NO-ELIG-5.
           MOVE TY-INS-NO-WROTE(5) TO QTY-INS-NO-WROTE-5.
           MOVE TY-INS-NO-NOTWROTE(5) TO QTY-INS-NO-NOTWROTE-5.
           MOVE TY-INS-AMT-WROTE(5) TO QTY-INS-AMT-WROTE-5.
           MOVE TY-INS-NO-ELIG(6) TO QTY-INS-NO-ELIG-6.
           MOVE TY-INS-NO-WROTE(6) TO QTY-INS-NO-WROTE-6.
           MOVE TY-INS-NO-NOTWROTE(6) TO QTY-INS-NO-NOTWROTE-6.
           MOVE TY-INS-AMT-WROTE(6) TO QTY-INS-AMT-WROTE-6.
           MOVE TY-INS-NO-ELIG(7) TO QTY-INS-NO-ELIG-7.
           MOVE TY-INS-NO-WROTE(7) TO QTY-INS-NO-WROTE-7.
           MOVE TY-INS-NO-NOTWROTE(7) TO QTY-INS-NO-NOTWROTE-7.
           MOVE TY-INS-AMT-WROTE(7) TO QTY-INS-AMT-WROTE-7.
           MOVE TY-INS-NO-ELIG(8) TO QTY-INS-NO-ELIG-8.
           MOVE TY-INS-NO-WROTE(8) TO QTY-INS-NO-WROTE-8.
           MOVE TY-INS-NO-NOTWROTE(8) TO QTY-INS-NO-NOTWROTE-8.
           MOVE TY-INS-AMT-WROTE(8) TO QTY-INS-AMT-WROTE-8.
           MOVE TY-INS-NO-ELIG(9) TO QTY-INS-NO-ELIG-9.
           MOVE TY-INS-NO-WROTE(9) TO QTY-INS-NO-WROTE-9.
           MOVE TY-INS-NO-NOTWROTE(9) TO QTY-INS-NO-NOTWROTE-9.
           MOVE TY-INS-AMT-WROTE(9) TO QTY-INS-AMT-WROTE-9.
           MOVE TY-INS-NO-ELIG(10) TO QTY-INS-NO-ELIG-10.
           MOVE TY-INS-NO-WROTE(10) TO QTY-INS-NO-WROTE-10.
           MOVE TY-INS-NO-NOTWROTE(10) TO QTY-INS-NO-NOTWROTE-10.
           MOVE TY-INS-AMT-WROTE(10) TO QTY-INS-AMT-WROTE-10.
           MOVE TY-FEE-NO-ELIG(1) TO QTY-FEE-NO-ELIG-1.
           MOVE TY-FEE-NO-WROTE(1) TO QTY-FEE-NO-WROTE-1.
           MOVE TY-FEE-NO-NOTWROTE(1) TO QTY-FEE-NO-NOTWROTE-1.
           MOVE TY-FEE-AMT-WROTE(1) TO QTY-FEE-AMT-WROTE-1.
           MOVE TY-FEE-NO-ELIG(2) TO QTY-FEE-NO-ELIG-2.
           MOVE TY-FEE-NO-WROTE(2) TO QTY-FEE-NO-WROTE-2.
           MOVE TY-FEE-NO-NOTWROTE(2) TO QTY-FEE-NO-NOTWROTE-2.
           MOVE TY-FEE-AMT-WROTE(2) TO QTY-FEE-AMT-WROTE-2.
           MOVE TY-FEE-NO-ELIG(3) TO QTY-FEE-NO-ELIG-3.
           MOVE TY-FEE-NO-WROTE(3) TO QTY-FEE-NO-WROTE-3.
           MOVE TY-FEE-NO-NOTWROTE(3) TO QTY-FEE-NO-NOTWROTE-3.
           MOVE TY-FEE-AMT-WROTE(3) TO QTY-FEE-AMT-WROTE-3.
           MOVE TY-OTH-NO-COL TO QTY-OTH-NO-COL.
           MOVE TY-OT2-NO-COL TO QTY-OT2-NO-COL.
           MOVE TY-INT-NO-COL TO QTY-INT-NO-COL.
           MOVE TY-LATE-NO-COL TO QTY-LATE-NO-COL.
           MOVE TY-OTH-NO-WAIVED TO QTY-OTH-NO-WAIVED.
           MOVE TY-OT2-NO-WAIVED TO QTY-OT2-NO-WAIVED.
           MOVE TY-INT-NO-WAIVED TO QTY-INT-NO-WAIVED.
           MOVE TY-LATE-NO-WAIVED TO QTY-LATE-NO-WAIVED.
           MOVE TY-OTH-AMT-WAIVED TO QTY-OTH-AMT-WAIVED.
           MOVE TY-OT2-AMT-WAIVED TO QTY-OT2-AMT-WAIVED.
           MOVE TY-INT-AMT-WAIVED TO QTY-INT-AMT-WAIVED.
           MOVE TY-LATE-AMT-WAIVED TO QTY-LATE-AMT-WAIVED.
           MOVE TY-CKWRITTEN(1) TO QTY-CKWRITTEN-1.
           MOVE TY-CKWRITTEN(2) TO QTY-CKWRITTEN-2.
           MOVE TY-CKWRITTEN(3) TO QTY-CKWRITTEN-3.
           MOVE TY-CKWRITTEN(4) TO QTY-CKWRITTEN-4.
           MOVE TY-CKWRITTEN(5) TO QTY-CKWRITTEN-5.
           MOVE TY-CKWRITTEN(6) TO QTY-CKWRITTEN-6.
           MOVE TY-CKWRITTEN(7) TO QTY-CKWRITTEN-7.
           MOVE TY-CKWRITTEN(8) TO QTY-CKWRITTEN-8.
           MOVE TY-CKWRITTEN(9) TO QTY-CKWRITTEN-9.
           MOVE TY-CKWRITTEN(10) TO QTY-CKWRITTEN-10.
           MOVE TY-CKWRITTEN(11) TO QTY-CKWRITTEN-11.
           MOVE TY-CKWRITTEN(12) TO QTY-CKWRITTEN-12.
           MOVE TY-CKWRITTEN(13) TO QTY-CKWRITTEN-13.
           MOVE TY-CKWRITTEN(14) TO QTY-CKWRITTEN-14.
           MOVE TY-CKWRITTEN(15) TO QTY-CKWRITTEN-15.
           MOVE TY-CKWRITTEN(16) TO QTY-CKWRITTEN-16.
           MOVE TY-UNITS(1 1) TO QTY-UNITS-1-1.
           MOVE TY-OUTBAL(1 1) TO QTY-OUTBAL-1-1.
           MOVE TY-UNITS(1 2) TO QTY-UNITS-1-2.
           MOVE TY-OUTBAL(1 2) TO QTY-OUTBAL-1-2.
           MOVE TY-UNITS(1 3) TO QTY-UNITS-1-3.
           MOVE TY-OUTBAL(1 3) TO QTY-OUTBAL-1-3.
           MOVE TY-UNITS(1 4) TO QTY-UNITS-1-4.
           MOVE TY-OUTBAL(1 4) TO QTY-OUTBAL-1-4.
           MOVE TY-UNITS(1 5) TO QTY-UNITS-1-5.
           MOVE TY-OUTBAL(1 5) TO QTY-OUTBAL-1-5.
           MOVE TY-UNITS(1 6) TO QTY-UNITS-1-6.
           MOVE TY-OUTBAL(1 6) TO QTY-OUTBAL-1-6.
           MOVE TY-UNITS(1 7) TO QTY-UNITS-1-7.
           MOVE TY-OUTBAL(1 7) TO QTY-OUTBAL-1-7.
           MOVE TY-UNITS(1 8) TO QTY-UNITS-1-8.
           MOVE TY-OUTBAL(1 8) TO QTY-OUTBAL-1-8.
           MOVE TY-UNITS(1 9) TO QTY-UNITS-1-9.
           MOVE TY-OUTBAL(1 9) TO QTY-OUTBAL-1-9.
           MOVE TY-UNITS(1 10) TO QTY-UNITS-1-10.
           MOVE TY-OUTBAL(1 10) TO QTY-OUTBAL-1-10.
           MOVE TY-UNITS(2 1) TO QTY-UNITS-2-1.
           MOVE TY-OUTBAL(2 1) TO QTY-OUTBAL-2-1.
           MOVE TY-UNITS(2 2) TO QTY-UNITS-2-2.
           MOVE TY-OUTBAL(2 2) TO QTY-OUTBAL-2-2.
           MOVE TY-UNITS(2 3) TO QTY-UNITS-2-3.
           MOVE TY-OUTBAL(2 3) TO QTY-OUTBAL-2-3.
           MOVE TY-UNITS(2 4) TO QTY-UNITS-2-4.
           MOVE TY-OUTBAL(2 4) TO QTY-OUTBAL-2-4.
           MOVE TY-UNITS(2 5) TO QTY-UNITS-2-5.
           MOVE TY-OUTBAL(2 5) TO QTY-OUTBAL-2-5.
           MOVE TY-UNITS(2 6) TO QTY-UNITS-2-6.
           MOVE TY-OUTBAL(2 6) TO QTY-OUTBAL-2-6.
           MOVE TY-UNITS(2 7) TO QTY-UNITS-2-7.
           MOVE TY-OUTBAL(2 7) TO QTY-OUTBAL-2-7.
           MOVE TY-UNITS(2 8) TO QTY-UNITS-2-8.
           MOVE TY-OUTBAL(2 8) TO QTY-OUTBAL-2-8.
           MOVE TY-UNITS(2 9) TO QTY-UNITS-2-9.
           MOVE TY-OUTBAL(2 9) TO QTY-OUTBAL-2-9.
           MOVE TY-UNITS(2 10) TO QTY-UNITS-2-10.
           MOVE TY-OUTBAL(2 10) TO QTY-OUTBAL-2-10.
           MOVE TY-UNITS-BNKRPT(1 1) TO QTY-UNITS-BNKRPT-1-1.
           MOVE TY-OUTBAL-BNKRPT(1 1) TO QTY-OUTBAL-BNKRPT-1-1.
           MOVE TY-UNITS-BNKRPT(1 2) TO QTY-UNITS-BNKRPT-1-2.
           MOVE TY-OUTBAL-BNKRPT(1 2) TO QTY-OUTBAL-BNKRPT-1-2.
           MOVE TY-UNITS-BNKRPT(1 3) TO QTY-UNITS-BNKRPT-1-3.
           MOVE TY-OUTBAL-BNKRPT(1 3) TO QTY-OUTBAL-BNKRPT-1-3.
           MOVE TY-UNITS-BNKRPT(1 4) TO QTY-UNITS-BNKRPT-1-4.
           MOVE TY-OUTBAL-BNKRPT(1 4) TO QTY-OUTBAL-BNKRPT-1-4.
           MOVE TY-UNITS-BNKRPT(1 5) TO QTY-UNITS-BNKRPT-1-5.
           MOVE TY-OUTBAL-BNKRPT(1 5) TO QTY-OUTBAL-BNKRPT-1-5.
           MOVE TY-UNITS-BNKRPT(1 6) TO QTY-UNITS-BNKRPT-1-6.
           MOVE TY-OUTBAL-BNKRPT(1 6) TO QTY-OUTBAL-BNKRPT-1-6.
           MOVE TY-UNITS-BNKRPT(1 7) TO QTY-UNITS-BNKRPT-1-7.
           MOVE TY-OUTBAL-BNKRPT(1 7) TO QTY-OUTBAL-BNKRPT-1-7.
           MOVE TY-UNITS-BNKRPT(1 8) TO QTY-UNITS-BNKRPT-1-8.
           MOVE TY-OUTBAL-BNKRPT(1 8) TO QTY-OUTBAL-BNKRPT-1-8.
           MOVE TY-UNITS-BNKRPT(1 9) TO QTY-UNITS-BNKRPT-1-9.
           MOVE TY-OUTBAL-BNKRPT(1 9) TO QTY-OUTBAL-BNKRPT-1-9.
           MOVE TY-UNITS-BNKRPT(1 10) TO QTY-UNITS-BNKRPT-1-10.
           MOVE TY-OUTBAL-BNKRPT(1 10) TO QTY-OUTBAL-BNKRPT-1-10.
           MOVE TY-UNITS-BNKRPT(2 1) TO QTY-UNITS-BNKRPT-2-1.
           MOVE TY-OUTBAL-BNKRPT(2 1) TO QTY-OUTBAL-BNKRPT-2-1.
           MOVE TY-UNITS-BNKRPT(2 2) TO QTY-UNITS-BNKRPT-2-2.
           MOVE TY-OUTBAL-BNKRPT(2 2) TO QTY-OUTBAL-BNKRPT-2-2.
           MOVE TY-UNITS-BNKRPT(2 3) TO QTY-UNITS-BNKRPT-2-3.
           MOVE TY-OUTBAL-BNKRPT(2 3) TO QTY-OUTBAL-BNKRPT-2-3.
           MOVE TY-UNITS-BNKRPT(2 4) TO QTY-UNITS-BNKRPT-2-4.
           MOVE TY-OUTBAL-BNKRPT(2 4) TO QTY-OUTBAL-BNKRPT-2-4.
           MOVE TY-UNITS-BNKRPT(2 5) TO QTY-UNITS-BNKRPT-2-5.
           MOVE TY-OUTBAL-BNKRPT(2 5) TO QTY-OUTBAL-BNKRPT-2-5.
           MOVE TY-UNITS-BNKRPT(2 6) TO QTY-UNITS-BNKRPT-2-6.
           MOVE TY-OUTBAL-BNKRPT(2 6) TO QTY-OUTBAL-BNKRPT-2-6.
           MOVE TY-UNITS-BNKRPT(2 7) TO QTY-UNITS-BNKRPT-2-7.
           MOVE TY-OUTBAL-BNKRPT(2 7) TO QTY-OUTBAL-BNKRPT-2-7.
           MOVE TY-UNITS-BNKRPT(2 8) TO QTY-UNITS-BNKRPT-2-8.
           MOVE TY-OUTBAL-BNKRPT(2 8) TO QTY-OUTBAL-BNKRPT-2-8.
           MOVE TY-UNITS-BNKRPT(2 9) TO QTY-UNITS-BNKRPT-2-9.
           MOVE TY-OUTBAL-BNKRPT(2 9) TO QTY-OUTBAL-BNKRPT-2-9.
           MOVE TY-UNITS-BNKRPT(2 10) TO QTY-UNITS-BNKRPT-2-10.
           MOVE TY-OUTBAL-BNKRPT(2 10) TO QTY-OUTBAL-BNKRPT-2-10.
           MOVE TY-SUMWGTTM TO QTY-SUMWGTTM.
           MOVE TY-SUMWGTAV TO QTY-SUMWGTAV.
           MOVE TY-SUMWGTTA TO QTY-SUMWGTTA.
           MOVE TY-ALLOTMENTS TO QTY-ALLOTMENTS.
           MOVE TY-BRFG TO QTY-BRFG.
           MOVE TY-DLDISC TO QTY-DLDISC.
           MOVE TY-UNEARNED-AMT(1 1) TO QTY-UNEARNED-AMT-1-1.
           MOVE TY-UNEARNED-AMT(1 2) TO QTY-UNEARNED-AMT-1-2.
           MOVE TY-UNEARNED-AMT(1 3) TO QTY-UNEARNED-AMT-1-3.
           MOVE TY-UNEARNED-AMT(1 4) TO QTY-UNEARNED-AMT-1-4.
           MOVE TY-UNEARNED-AMT(1 5) TO QTY-UNEARNED-AMT-1-5.
           MOVE TY-UNEARNED-AMT(1 6) TO QTY-UNEARNED-AMT-1-6.
           MOVE TY-UNEARNED-AMT(1 7) TO QTY-UNEARNED-AMT-1-7.
           MOVE TY-UNEARNED-AMT(1 8) TO QTY-UNEARNED-AMT-1-8.
           MOVE TY-UNEARNED-AMT(1 9) TO QTY-UNEARNED-AMT-1-9.
           MOVE TY-UNEARNED-AMT(1 10) TO QTY-UNEARNED-AMT-1-10.
           MOVE TY-UNEARNED-AMT(2 1) TO QTY-UNEARNED-AMT-2-1.
           MOVE TY-UNEARNED-AMT(2 2) TO QTY-UNEARNED-AMT-2-2.
           MOVE TY-UNEARNED-AMT(2 3) TO QTY-UNEARNED-AMT-2-3.
           MOVE TY-UNEARNED-AMT(2 4) TO QTY-UNEARNED-AMT-2-4.
           MOVE TY-UNEARNED-AMT(2 5) TO QTY-UNEARNED-AMT-2-5.
           MOVE TY-UNEARNED-AMT(2 6) TO QTY-UNEARNED-AMT-2-6.
           MOVE TY-UNEARNED-AMT(2 7) TO QTY-UNEARNED-AMT-2-7.
           MOVE TY-UNEARNED-AMT(2 8) TO QTY-UNEARNED-AMT-2-8.
           MOVE TY-UNEARNED-AMT(2 9) TO QTY-UNEARNED-AMT-2-9.
           MOVE TY-UNEARNED-AMT(2 10) TO QTY-UNEARNED-AMT-2-10.
           MOVE TY-PROCEEDS TO QTY-PROCEEDS.
           MOVE TY-LOCKBOX TO QTY-LOCKBOX.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-TY1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TY-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TY-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TY-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TY-ALL-DATA
              MOVE TY-ALL-PATH             TO TY-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TY-ALL-BASE
              MOVE FILPATH-MACHINE         TO TY-ALL-MACHINE
              MOVE FILPATH-DATA            TO TY-ALL-DATA
              MOVE TY-ALL-PATH             TO TY-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TYFILE NOT = "S" )
              MOVE TY-PATH-BEG             TO TY-SELECT-PATH-BEG
              MOVE TY-PATH-FILE            TO TY-SELECT-PATH-FILE
           ELSE
              MOVE TY-PATH-SQL             TO TY-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TY1-FILE.
           PERFORM LOAD-TY1-FILE.
           PERFORM OPEN-IT.
           MOVE TY-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TY1-FILE.
           PERFORM START-IT.
           MOVE TY-PATH-OWNBR  TO TY-BRNO.
           MOVE TY-SELECT-PATH TO E-FILE.
           MOVE TY1-KEY        TO E-KEYX.

           IF ( TY1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TY1-FILE.

           IF ( QTY1-WEND-CLASS = ZEROES    ) OR
              ( QTY1-WEND-CLASS NOT NUMERIC )
              MOVE TY-PATH-OWNBR        TO QTY1-WBEG-BRNO
              MOVE TY-PATH-OWNBR        TO QTY1-WEND-BRNO
              MOVE TY-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTY1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTY1-WEND-DATE
              MOVE TY-CLASS             TO QTY1-WBEG-CLASS
              MOVE ALL "9"              TO QTY1-WEND-CLASS.

           EXEC SQL
            DECLARE TY1_CURSOR CURSOR FOR
            SELECT TYFILE.TY_BRNO,
                   ISNULL(CAST(TYFILE.TY_DATE AS VARCHAR(10)),'0'),
                   TYFILE.TY_CLASS,
                   ISNULL(CAST(TYFILE.TY_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TYFILE.TY_NO_1_1,
                   TYFILE.TY_AMT_1_1,
                   TYFILE.TY_O2_1_1,
                   TYFILE.TY_INT_1_1,
                   TYFILE.TY_NO_1_2,
                   TYFILE.TY_AMT_1_2,
                   TYFILE.TY_O2_1_2,
                   TYFILE.TY_INT_1_2,
                   TYFILE.TY_NO_1_3,
                   TYFILE.TY_AMT_1_3,
                   TYFILE.TY_O2_1_3,
                   TYFILE.TY_INT_1_3,
                   TYFILE.TY_NO_1_4,
                   TYFILE.TY_AMT_1_4,
                   TYFILE.TY_O2_1_4,
                   TYFILE.TY_INT_1_4,
                   TYFILE.TY_NO_1_5,
                   TYFILE.TY_AMT_1_5,
                   TYFILE.TY_O2_1_5,
                   TYFILE.TY_INT_1_5,
                   TYFILE.TY_NO_1_6,
                   TYFILE.TY_AMT_1_6,
                   TYFILE.TY_O2_1_6,
                   TYFILE.TY_INT_1_6,
                   TYFILE.TY_NO_1_7,
                   TYFILE.TY_AMT_1_7,
                   TYFILE.TY_O2_1_7,
                   TYFILE.TY_INT_1_7,
                   TYFILE.TY_NO_1_8,
                   TYFILE.TY_AMT_1_8,
                   TYFILE.TY_O2_1_8,
                   TYFILE.TY_INT_1_8,
                   TYFILE.TY_NO_1_9,
                   TYFILE.TY_AMT_1_9,
                   TYFILE.TY_O2_1_9,
                   TYFILE.TY_INT_1_9,
                   TYFILE.TY_NO_1_10,
                   TYFILE.TY_AMT_1_10,
                   TYFILE.TY_O2_1_10,
                   TYFILE.TY_INT_1_10,
                   TYFILE.TY_NO_1_11,
                   TYFILE.TY_AMT_1_11,
                   TYFILE.TY_O2_1_11,
                   TYFILE.TY_INT_1_11,
                   TYFILE.TY_NO_2_1,
                   TYFILE.TY_AMT_2_1,
                   TYFILE.TY_O2_2_1,
                   TYFILE.TY_INT_2_1,
                   TYFILE.TY_NO_2_2,
                   TYFILE.TY_AMT_2_2,
                   TYFILE.TY_O2_2_2,
                   TYFILE.TY_INT_2_2,
                   TYFILE.TY_NO_2_3,
                   TYFILE.TY_AMT_2_3,
                   TYFILE.TY_O2_2_3,
                   TYFILE.TY_INT_2_3,
                   TYFILE.TY_NO_2_4,
                   TYFILE.TY_AMT_2_4,
                   TYFILE.TY_O2_2_4,
                   TYFILE.TY_INT_2_4,
                   TYFILE.TY_NO_2_5,
                   TYFILE.TY_AMT_2_5,
                   TYFILE.TY_O2_2_5,
                   TYFILE.TY_INT_2_5,
                   TYFILE.TY_NO_2_6,
                   TYFILE.TY_AMT_2_6,
                   TYFILE.TY_O2_2_6,
                   TYFILE.TY_INT_2_6,
                   TYFILE.TY_NO_2_7,
                   TYFILE.TY_AMT_2_7,
                   TYFILE.TY_O2_2_7,
                   TYFILE.TY_INT_2_7,
                   TYFILE.TY_NO_2_8,
                   TYFILE.TY_AMT_2_8,
                   TYFILE.TY_O2_2_8,
                   TYFILE.TY_INT_2_8,
                   TYFILE.TY_NO_2_9,
                   TYFILE.TY_AMT_2_9,
                   TYFILE.TY_O2_2_9,
                   TYFILE.TY_INT_2_9,
                   TYFILE.TY_NO_2_10,
                   TYFILE.TY_AMT_2_10,
                   TYFILE.TY_O2_2_10,
                   TYFILE.TY_INT_2_10,
                   TYFILE.TY_NO_2_11,
                   TYFILE.TY_AMT_2_11,
                   TYFILE.TY_O2_2_11,
                   TYFILE.TY_INT_2_11,
                   TYFILE.TY_NO_REPO_1_1,
                   TYFILE.TY_AMT_REPO_1_1,
                   TYFILE.TY_O2_REPO_1_1,
                   TYFILE.TY_INT_REPO_1_1,
                   TYFILE.TY_NO_REPO_1_2,
                   TYFILE.TY_AMT_REPO_1_2,
                   TYFILE.TY_O2_REPO_1_2,
                   TYFILE.TY_INT_REPO_1_2,
                   TYFILE.TY_NO_REPO_1_3,
                   TYFILE.TY_AMT_REPO_1_3,
                   TYFILE.TY_O2_REPO_1_3,
                   TYFILE.TY_INT_REPO_1_3,
                   TYFILE.TY_NO_REPO_1_4,
                   TYFILE.TY_AMT_REPO_1_4,
                   TYFILE.TY_O2_REPO_1_4,
                   TYFILE.TY_INT_REPO_1_4,
                   TYFILE.TY_NO_REPO_1_5,
                   TYFILE.TY_AMT_REPO_1_5,
                   TYFILE.TY_O2_REPO_1_5,
                   TYFILE.TY_INT_REPO_1_5,
                   TYFILE.TY_NO_REPO_2_1,
                   TYFILE.TY_AMT_REPO_2_1,
                   TYFILE.TY_O2_REPO_2_1,
                   TYFILE.TY_INT_REPO_2_1,
                   TYFILE.TY_NO_REPO_2_2,
                   TYFILE.TY_AMT_REPO_2_2,
                   TYFILE.TY_O2_REPO_2_2,
                   TYFILE.TY_INT_REPO_2_2,
                   TYFILE.TY_NO_REPO_2_3,
                   TYFILE.TY_AMT_REPO_2_3,
                   TYFILE.TY_O2_REPO_2_3,
                   TYFILE.TY_INT_REPO_2_3,
                   TYFILE.TY_NO_REPO_2_4,
                   TYFILE.TY_AMT_REPO_2_4,
                   TYFILE.TY_O2_REPO_2_4,
                   TYFILE.TY_INT_REPO_2_4,
                   TYFILE.TY_NO_REPO_2_5,
                   TYFILE.TY_AMT_REPO_2_5,
                   TYFILE.TY_O2_REPO_2_5,
                   TYFILE.TY_INT_REPO_2_5,
                   TYFILE.TY_NO_PL_1_1,
                   TYFILE.TY_AMT_PL_1_1,
                   TYFILE.TY_O2_PL_1_1,
                   TYFILE.TY_INT_PL_1_1,
                   TYFILE.TY_NO_PL_1_2,
                   TYFILE.TY_AMT_PL_1_2,
                   TYFILE.TY_O2_PL_1_2,
                   TYFILE.TY_INT_PL_1_2,
                   TYFILE.TY_NO_PL_1_3,
                   TYFILE.TY_AMT_PL_1_3,
                   TYFILE.TY_O2_PL_1_3,
                   TYFILE.TY_INT_PL_1_3,
                   TYFILE.TY_NO_PL_1_4,
                   TYFILE.TY_AMT_PL_1_4,
                   TYFILE.TY_O2_PL_1_4,
                   TYFILE.TY_INT_PL_1_4,
                   TYFILE.TY_NO_PL_1_5,
                   TYFILE.TY_AMT_PL_1_5,
                   TYFILE.TY_O2_PL_1_5,
                   TYFILE.TY_INT_PL_1_5,
                   TYFILE.TY_NO_PL_1_6,
                   TYFILE.TY_AMT_PL_1_6,
                   TYFILE.TY_O2_PL_1_6,
                   TYFILE.TY_INT_PL_1_6,
                   TYFILE.TY_NO_PL_1_7,
                   TYFILE.TY_AMT_PL_1_7,
                   TYFILE.TY_O2_PL_1_7,
                   TYFILE.TY_INT_PL_1_7,
                   TYFILE.TY_NO_PL_1_8,
                   TYFILE.TY_AMT_PL_1_8,
                   TYFILE.TY_O2_PL_1_8,
                   TYFILE.TY_INT_PL_1_8,
                   TYFILE.TY_NO_PL_1_9,
                   TYFILE.TY_AMT_PL_1_9,
                   TYFILE.TY_O2_PL_1_9,
                   TYFILE.TY_INT_PL_1_9,
                   TYFILE.TY_NO_PL_1_10,
                   TYFILE.TY_AMT_PL_1_10,
                   TYFILE.TY_O2_PL_1_10,
                   TYFILE.TY_INT_PL_1_10,
                   TYFILE.TY_NO_PL_2_1,
                   TYFILE.TY_AMT_PL_2_1,
                   TYFILE.TY_O2_PL_2_1,
                   TYFILE.TY_INT_PL_2_1,
                   TYFILE.TY_NO_PL_2_2,
                   TYFILE.TY_AMT_PL_2_2,
                   TYFILE.TY_O2_PL_2_2,
                   TYFILE.TY_INT_PL_2_2,
                   TYFILE.TY_NO_PL_2_3,
                   TYFILE.TY_AMT_PL_2_3,
                   TYFILE.TY_O2_PL_2_3,
                   TYFILE.TY_INT_PL_2_3,
                   TYFILE.TY_NO_PL_2_4,
                   TYFILE.TY_AMT_PL_2_4,
                   TYFILE.TY_O2_PL_2_4,
                   TYFILE.TY_INT_PL_2_4,
                   TYFILE.TY_NO_PL_2_5,
                   TYFILE.TY_AMT_PL_2_5,
                   TYFILE.TY_O2_PL_2_5,
                   TYFILE.TY_INT_PL_2_5,
                   TYFILE.TY_NO_PL_2_6,
                   TYFILE.TY_AMT_PL_2_6,
                   TYFILE.TY_O2_PL_2_6,
                   TYFILE.TY_INT_PL_2_6,
                   TYFILE.TY_NO_PL_2_7,
                   TYFILE.TY_AMT_PL_2_7,
                   TYFILE.TY_O2_PL_2_7,
                   TYFILE.TY_INT_PL_2_7,
                   TYFILE.TY_NO_PL_2_8,
                   TYFILE.TY_AMT_PL_2_8,
                   TYFILE.TY_O2_PL_2_8,
                   TYFILE.TY_INT_PL_2_8,
                   TYFILE.TY_NO_PL_2_9,
                   TYFILE.TY_AMT_PL_2_9,
                   TYFILE.TY_O2_PL_2_9,
                   TYFILE.TY_INT_PL_2_9,
                   TYFILE.TY_NO_PL_2_10,
                   TYFILE.TY_AMT_PL_2_10,
                   TYFILE.TY_O2_PL_2_10,
                   TYFILE.TY_INT_PL_2_10,
                   TYFILE.TY_DLRVPRIN_1_1,
                   TYFILE.TY_DLRVPRIN_1_2,
                   TYFILE.TY_DLRVPRIN_1_3,
                   TYFILE.TY_DLRVCHGS_1_1,
                   TYFILE.TY_DLRVCHGS_1_2,
                   TYFILE.TY_DLRVCHGS_1_3,
                   TYFILE.TY_DLPARVHELD_1,
                   TYFILE.TY_DLRVPRIN_2_1,
                   TYFILE.TY_DLRVPRIN_2_2,
                   TYFILE.TY_DLRVPRIN_2_3,
                   TYFILE.TY_DLRVCHGS_2_1,
                   TYFILE.TY_DLRVCHGS_2_2,
                   TYFILE.TY_DLRVCHGS_2_3,
                   TYFILE.TY_DLPARVHELD_2,
                   TYFILE.TY_PL_PRINCOL_1,
                   TYFILE.TY_PL_INTCOL_1,
                   TYFILE.TY_PL_LATECOL_1,
                   TYFILE.TY_PL_O2COL_1,
                   TYFILE.TY_PL_PRINCOL_2,
                   TYFILE.TY_PL_INTCOL_2,
                   TYFILE.TY_PL_LATECOL_2,
                   TYFILE.TY_PL_O2COL_2,
                   TYFILE.TY_PL_PRINCOL_3,
                   TYFILE.TY_PL_INTCOL_3,
                   TYFILE.TY_PL_LATECOL_3,
                   TYFILE.TY_PL_O2COL_3,
                   TYFILE.TY_PL_PRINCOL_4,
                   TYFILE.TY_PL_INTCOL_4,
                   TYFILE.TY_PL_LATECOL_4,
                   TYFILE.TY_PL_O2COL_4,
                   TYFILE.TY_PL_PRINCOL_5,
                   TYFILE.TY_PL_INTCOL_5,
                   TYFILE.TY_PL_LATECOL_5,
                   TYFILE.TY_PL_O2COL_5,
                   TYFILE.TY_AMT_SOLD,
                   TYFILE.TY_AMT_TRANS,
                   TYFILE.TY_AMT_PURCH,
                   TYFILE.TY_NO_SOLD,
                   TYFILE.TY_NO_TRANS,
                   TYFILE.TY_NO_PURCH,
                   TYFILE.TY_OTHCOL,
                   TYFILE.TY_OT2COL,
                   TYFILE.TY_INTCOL,
                   TYFILE.TY_LATECOL,
                   TYFILE.TY_PRINCOL,
                   TYFILE.TY_DEFCOL,
                   TYFILE.TY_RNPRIN,
                   TYFILE.TY_INTREB,
                   TYFILE.TY_CLREB1,
                   TYFILE.TY_CLREB2,
                   TYFILE.TY_CLREB3,
                   TYFILE.TY_CLREB4,
                   TYFILE.TY_AHREB,
                   TYFILE.TY_PPREB,
                   TYFILE.TY_O1REB,
                   TYFILE.TY_O2REB,
                   TYFILE.TY_O3REB,
                   TYFILE.TY_O4REB,
                   TYFILE.TY_O5REB,
                   TYFILE.TY_O6REB,
                   TYFILE.TY_O7REB,
                   TYFILE.TY_SCREB,
                   TYFILE.TY_MFREB,
                   TYFILE.TY_DFREB,
                   TYFILE.TY_UN7_REB,
                   TYFILE.TY_UN8_REB,
                   TYFILE.TY_UN9_REB,
                   TYFILE.TY_UN10_REB,
                   TYFILE.TY_PART_CHGOFF,
                   TYFILE.TY_INS_NO_ELIG_1,
                   TYFILE.TY_INS_NO_WROTE_1,
                   TYFILE.TY_INS_NO_NOTWROTE_1,
                   TYFILE.TY_INS_AMT_WROTE_1,
                   TYFILE.TY_INS_NO_ELIG_2,
                   TYFILE.TY_INS_NO_WROTE_2,
                   TYFILE.TY_INS_NO_NOTWROTE_2,
                   TYFILE.TY_INS_AMT_WROTE_2,
                   TYFILE.TY_INS_NO_ELIG_3,
                   TYFILE.TY_INS_NO_WROTE_3,
                   TYFILE.TY_INS_NO_NOTWROTE_3,
                   TYFILE.TY_INS_AMT_WROTE_3,
                   TYFILE.TY_INS_NO_ELIG_4,
                   TYFILE.TY_INS_NO_WROTE_4,
                   TYFILE.TY_INS_NO_NOTWROTE_4,
                   TYFILE.TY_INS_AMT_WROTE_4,
                   TYFILE.TY_INS_NO_ELIG_5,
                   TYFILE.TY_INS_NO_WROTE_5,
                   TYFILE.TY_INS_NO_NOTWROTE_5,
                   TYFILE.TY_INS_AMT_WROTE_5,
                   TYFILE.TY_INS_NO_ELIG_6,
                   TYFILE.TY_INS_NO_WROTE_6,
                   TYFILE.TY_INS_NO_NOTWROTE_6,
                   TYFILE.TY_INS_AMT_WROTE_6,
                   TYFILE.TY_INS_NO_ELIG_7,
                   TYFILE.TY_INS_NO_WROTE_7,
                   TYFILE.TY_INS_NO_NOTWROTE_7,
                   TYFILE.TY_INS_AMT_WROTE_7,
                   TYFILE.TY_INS_NO_ELIG_8,
                   TYFILE.TY_INS_NO_WROTE_8,
                   TYFILE.TY_INS_NO_NOTWROTE_8,
                   TYFILE.TY_INS_AMT_WROTE_8,
                   TYFILE.TY_INS_NO_ELIG_9,
                   TYFILE.TY_INS_NO_WROTE_9,
                   TYFILE.TY_INS_NO_NOTWROTE_9,
                   TYFILE.TY_INS_AMT_WROTE_9,
                   TYFILE.TY_INS_NO_ELIG_10,
                   TYFILE.TY_INS_NO_WROTE_10,
                   TYFILE.TY_INS_NO_NOTWROTE_10,
                   TYFILE.TY_INS_AMT_WROTE_10,
                   TYFILE.TY_FEE_NO_ELIG_1,
                   TYFILE.TY_FEE_NO_WROTE_1,
                   TYFILE.TY_FEE_NO_NOTWROTE_1,
                   TYFILE.TY_FEE_AMT_WROTE_1,
                   TYFILE.TY_FEE_NO_ELIG_2,
                   TYFILE.TY_FEE_NO_WROTE_2,
                   TYFILE.TY_FEE_NO_NOTWROTE_2,
                   TYFILE.TY_FEE_AMT_WROTE_2,
                   TYFILE.TY_FEE_NO_ELIG_3,
                   TYFILE.TY_FEE_NO_WROTE_3,
                   TYFILE.TY_FEE_NO_NOTWROTE_3,
                   TYFILE.TY_FEE_AMT_WROTE_3,
                   TYFILE.TY_OTH_NO_COL,
                   TYFILE.TY_OT2_NO_COL,
                   TYFILE.TY_INT_NO_COL,
                   TYFILE.TY_LATE_NO_COL,
                   TYFILE.TY_OTH_NO_WAIVED,
                   TYFILE.TY_OT2_NO_WAIVED,
                   TYFILE.TY_INT_NO_WAIVED,
                   TYFILE.TY_LATE_NO_WAIVED,
                   TYFILE.TY_OTH_AMT_WAIVED,
                   TYFILE.TY_OT2_AMT_WAIVED,
                   TYFILE.TY_INT_AMT_WAIVED,
                   TYFILE.TY_LATE_AMT_WAIVED,
                   TYFILE.TY_CKWRITTEN_1,
                   TYFILE.TY_CKWRITTEN_2,
                   TYFILE.TY_CKWRITTEN_3,
                   TYFILE.TY_CKWRITTEN_4,
                   TYFILE.TY_CKWRITTEN_5,
                   TYFILE.TY_CKWRITTEN_6,
                   TYFILE.TY_CKWRITTEN_7,
                   TYFILE.TY_CKWRITTEN_8,
                   TYFILE.TY_CKWRITTEN_9,
                   TYFILE.TY_CKWRITTEN_10,
                   TYFILE.TY_CKWRITTEN_11,
                   TYFILE.TY_CKWRITTEN_12,
                   TYFILE.TY_CKWRITTEN_13,
                   TYFILE.TY_CKWRITTEN_14,
                   TYFILE.TY_CKWRITTEN_15,
                   TYFILE.TY_CKWRITTEN_16,
                   TYFILE.TY_UNITS_1_1,
                   TYFILE.TY_OUTBAL_1_1,
                   TYFILE.TY_UNITS_1_2,
                   TYFILE.TY_OUTBAL_1_2,
                   TYFILE.TY_UNITS_1_3,
                   TYFILE.TY_OUTBAL_1_3,
                   TYFILE.TY_UNITS_1_4,
                   TYFILE.TY_OUTBAL_1_4,
                   TYFILE.TY_UNITS_1_5,
                   TYFILE.TY_OUTBAL_1_5,
                   TYFILE.TY_UNITS_1_6,
                   TYFILE.TY_OUTBAL_1_6,
                   TYFILE.TY_UNITS_1_7,
                   TYFILE.TY_OUTBAL_1_7,
                   TYFILE.TY_UNITS_1_8,
                   TYFILE.TY_OUTBAL_1_8,
                   TYFILE.TY_UNITS_1_9,
                   TYFILE.TY_OUTBAL_1_9,
                   TYFILE.TY_UNITS_1_10,
                   TYFILE.TY_OUTBAL_1_10,
                   TYFILE.TY_UNITS_2_1,
                   TYFILE.TY_OUTBAL_2_1,
                   TYFILE.TY_UNITS_2_2,
                   TYFILE.TY_OUTBAL_2_2,
                   TYFILE.TY_UNITS_2_3,
                   TYFILE.TY_OUTBAL_2_3,
                   TYFILE.TY_UNITS_2_4,
                   TYFILE.TY_OUTBAL_2_4,
                   TYFILE.TY_UNITS_2_5,
                   TYFILE.TY_OUTBAL_2_5,
                   TYFILE.TY_UNITS_2_6,
                   TYFILE.TY_OUTBAL_2_6,
                   TYFILE.TY_UNITS_2_7,
                   TYFILE.TY_OUTBAL_2_7,
                   TYFILE.TY_UNITS_2_8,
                   TYFILE.TY_OUTBAL_2_8,
                   TYFILE.TY_UNITS_2_9,
                   TYFILE.TY_OUTBAL_2_9,
                   TYFILE.TY_UNITS_2_10,
                   TYFILE.TY_OUTBAL_2_10,
                   TYFILE.TY_UNITS_BNKRPT_1_1,
                   TYFILE.TY_OUTBAL_BNKRPT_1_1,
                   TYFILE.TY_UNITS_BNKRPT_1_2,
                   TYFILE.TY_OUTBAL_BNKRPT_1_2,
                   TYFILE.TY_UNITS_BNKRPT_1_3,
                   TYFILE.TY_OUTBAL_BNKRPT_1_3,
                   TYFILE.TY_UNITS_BNKRPT_1_4,
                   TYFILE.TY_OUTBAL_BNKRPT_1_4,
                   TYFILE.TY_UNITS_BNKRPT_1_5,
                   TYFILE.TY_OUTBAL_BNKRPT_1_5,
                   TYFILE.TY_UNITS_BNKRPT_1_6,
                   TYFILE.TY_OUTBAL_BNKRPT_1_6,
                   TYFILE.TY_UNITS_BNKRPT_1_7,
                   TYFILE.TY_OUTBAL_BNKRPT_1_7,
                   TYFILE.TY_UNITS_BNKRPT_1_8,
                   TYFILE.TY_OUTBAL_BNKRPT_1_8,
                   TYFILE.TY_UNITS_BNKRPT_1_9,
                   TYFILE.TY_OUTBAL_BNKRPT_1_9,
                   TYFILE.TY_UNITS_BNKRPT_1_10,
                   TYFILE.TY_OUTBAL_BNKRPT_1_10,
                   TYFILE.TY_UNITS_BNKRPT_2_1,
                   TYFILE.TY_OUTBAL_BNKRPT_2_1,
                   TYFILE.TY_UNITS_BNKRPT_2_2,
                   TYFILE.TY_OUTBAL_BNKRPT_2_2,
                   TYFILE.TY_UNITS_BNKRPT_2_3,
                   TYFILE.TY_OUTBAL_BNKRPT_2_3,
                   TYFILE.TY_UNITS_BNKRPT_2_4,
                   TYFILE.TY_OUTBAL_BNKRPT_2_4,
                   TYFILE.TY_UNITS_BNKRPT_2_5,
                   TYFILE.TY_OUTBAL_BNKRPT_2_5,
                   TYFILE.TY_UNITS_BNKRPT_2_6,
                   TYFILE.TY_OUTBAL_BNKRPT_2_6,
                   TYFILE.TY_UNITS_BNKRPT_2_7,
                   TYFILE.TY_OUTBAL_BNKRPT_2_7,
                   TYFILE.TY_UNITS_BNKRPT_2_8,
                   TYFILE.TY_OUTBAL_BNKRPT_2_8,
                   TYFILE.TY_UNITS_BNKRPT_2_9,
                   TYFILE.TY_OUTBAL_BNKRPT_2_9,
                   TYFILE.TY_UNITS_BNKRPT_2_10,
                   TYFILE.TY_OUTBAL_BNKRPT_2_10,
                   TYFILE.TY_SUMWGTTM,
                   TYFILE.TY_SUMWGTAV,
                   TYFILE.TY_SUMWGTTA,
                   TYFILE.TY_ALLOTMENTS,
                   ISNULL(TYFILE.TY_BRFG,''),
                   TYFILE.TY_DLDISC,
                   TYFILE.TY_UNEARNED_AMT_1_1,
                   TYFILE.TY_UNEARNED_AMT_1_2,
                   TYFILE.TY_UNEARNED_AMT_1_3,
                   TYFILE.TY_UNEARNED_AMT_1_4,
                   TYFILE.TY_UNEARNED_AMT_1_5,
                   TYFILE.TY_UNEARNED_AMT_1_6,
                   TYFILE.TY_UNEARNED_AMT_1_7,
                   TYFILE.TY_UNEARNED_AMT_1_8,
                   TYFILE.TY_UNEARNED_AMT_1_9,
                   TYFILE.TY_UNEARNED_AMT_1_10,
                   TYFILE.TY_UNEARNED_AMT_2_1,
                   TYFILE.TY_UNEARNED_AMT_2_2,
                   TYFILE.TY_UNEARNED_AMT_2_3,
                   TYFILE.TY_UNEARNED_AMT_2_4,
                   TYFILE.TY_UNEARNED_AMT_2_5,
                   TYFILE.TY_UNEARNED_AMT_2_6,
                   TYFILE.TY_UNEARNED_AMT_2_7,
                   TYFILE.TY_UNEARNED_AMT_2_8,
                   TYFILE.TY_UNEARNED_AMT_2_9,
                   TYFILE.TY_UNEARNED_AMT_2_10,
                   TYFILE.TY_PROCEEDS,
                   TYFILE.TY_LOCKBOX
            FROM DBO.TYFILE
            WHERE TYFILE.TY_BRNO >= :QTY1-WBEG-BRNO
              AND TYFILE.TY_BRNO <= :QTY1-WEND-BRNO
              AND CAST(TYFILE.TY_DATE AS VARCHAR(10))
                          >= :QTY1-WBEG-DATE
              AND CAST(TYFILE.TY_DATE AS VARCHAR(10))
                          <= :QTY1-WEND-DATE
              AND TYFILE.TY_CLASS >= :QTY1-WBEG-CLASS
              AND TYFILE.TY_CLASS <= :QTY1-WEND-CLASS
            ORDER BY TYFILE.TY_BRNO,
                     TYFILE.TY_DATE,
                     TYFILE.TY_CLASS
           END-EXEC.

           EXEC SQL
               OPEN TY1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTY1-WBEG-BRNO
                          QTY1-WEND-BRNO
                          QTY1-WBEG-CLASS
                          QTY1-WEND-CLASS.

           MOVE SPACES TO QTY1-WBEG-DATE
                          QTY1-WEND-DATE.

           MOVE STAT---GOOD TO TY1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TY1-FILE.
           PERFORM READ-IT.
           MOVE TY-PATH-OWNBR  TO TY-BRNO. 
           MOVE TY-SELECT-PATH TO E-FILE.
           MOVE TY1-KEY        TO E-KEYX.

           MOVE TY-BRNO             TO QTY-BRNO.
           MOVE TY-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTY-DATE.
           MOVE TY-CLASS           TO QTY-CLASS.

           EXEC SQL
            SELECT TYFILE.TY_BRNO,
                   ISNULL(CAST(TYFILE.TY_DATE AS VARCHAR(10)),'0'),
                   TYFILE.TY_CLASS,
                   ISNULL(CAST(TYFILE.TY_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TYFILE.TY_NO_1_1,
                   TYFILE.TY_AMT_1_1,
                   TYFILE.TY_O2_1_1,
                   TYFILE.TY_INT_1_1,
                   TYFILE.TY_NO_1_2,
                   TYFILE.TY_AMT_1_2,
                   TYFILE.TY_O2_1_2,
                   TYFILE.TY_INT_1_2,
                   TYFILE.TY_NO_1_3,
                   TYFILE.TY_AMT_1_3,
                   TYFILE.TY_O2_1_3,
                   TYFILE.TY_INT_1_3,
                   TYFILE.TY_NO_1_4,
                   TYFILE.TY_AMT_1_4,
                   TYFILE.TY_O2_1_4,
                   TYFILE.TY_INT_1_4,
                   TYFILE.TY_NO_1_5,
                   TYFILE.TY_AMT_1_5,
                   TYFILE.TY_O2_1_5,
                   TYFILE.TY_INT_1_5,
                   TYFILE.TY_NO_1_6,
                   TYFILE.TY_AMT_1_6,
                   TYFILE.TY_O2_1_6,
                   TYFILE.TY_INT_1_6,
                   TYFILE.TY_NO_1_7,
                   TYFILE.TY_AMT_1_7,
                   TYFILE.TY_O2_1_7,
                   TYFILE.TY_INT_1_7,
                   TYFILE.TY_NO_1_8,
                   TYFILE.TY_AMT_1_8,
                   TYFILE.TY_O2_1_8,
                   TYFILE.TY_INT_1_8,
                   TYFILE.TY_NO_1_9,
                   TYFILE.TY_AMT_1_9,
                   TYFILE.TY_O2_1_9,
                   TYFILE.TY_INT_1_9,
                   TYFILE.TY_NO_1_10,
                   TYFILE.TY_AMT_1_10,
                   TYFILE.TY_O2_1_10,
                   TYFILE.TY_INT_1_10,
                   TYFILE.TY_NO_1_11,
                   TYFILE.TY_AMT_1_11,
                   TYFILE.TY_O2_1_11,
                   TYFILE.TY_INT_1_11,
                   TYFILE.TY_NO_2_1,
                   TYFILE.TY_AMT_2_1,
                   TYFILE.TY_O2_2_1,
                   TYFILE.TY_INT_2_1,
                   TYFILE.TY_NO_2_2,
                   TYFILE.TY_AMT_2_2,
                   TYFILE.TY_O2_2_2,
                   TYFILE.TY_INT_2_2,
                   TYFILE.TY_NO_2_3,
                   TYFILE.TY_AMT_2_3,
                   TYFILE.TY_O2_2_3,
                   TYFILE.TY_INT_2_3,
                   TYFILE.TY_NO_2_4,
                   TYFILE.TY_AMT_2_4,
                   TYFILE.TY_O2_2_4,
                   TYFILE.TY_INT_2_4,
                   TYFILE.TY_NO_2_5,
                   TYFILE.TY_AMT_2_5,
                   TYFILE.TY_O2_2_5,
                   TYFILE.TY_INT_2_5,
                   TYFILE.TY_NO_2_6,
                   TYFILE.TY_AMT_2_6,
                   TYFILE.TY_O2_2_6,
                   TYFILE.TY_INT_2_6,
                   TYFILE.TY_NO_2_7,
                   TYFILE.TY_AMT_2_7,
                   TYFILE.TY_O2_2_7,
                   TYFILE.TY_INT_2_7,
                   TYFILE.TY_NO_2_8,
                   TYFILE.TY_AMT_2_8,
                   TYFILE.TY_O2_2_8,
                   TYFILE.TY_INT_2_8,
                   TYFILE.TY_NO_2_9,
                   TYFILE.TY_AMT_2_9,
                   TYFILE.TY_O2_2_9,
                   TYFILE.TY_INT_2_9,
                   TYFILE.TY_NO_2_10,
                   TYFILE.TY_AMT_2_10,
                   TYFILE.TY_O2_2_10,
                   TYFILE.TY_INT_2_10,
                   TYFILE.TY_NO_2_11,
                   TYFILE.TY_AMT_2_11,
                   TYFILE.TY_O2_2_11,
                   TYFILE.TY_INT_2_11,
                   TYFILE.TY_NO_REPO_1_1,
                   TYFILE.TY_AMT_REPO_1_1,
                   TYFILE.TY_O2_REPO_1_1,
                   TYFILE.TY_INT_REPO_1_1,
                   TYFILE.TY_NO_REPO_1_2,
                   TYFILE.TY_AMT_REPO_1_2,
                   TYFILE.TY_O2_REPO_1_2,
                   TYFILE.TY_INT_REPO_1_2,
                   TYFILE.TY_NO_REPO_1_3,
                   TYFILE.TY_AMT_REPO_1_3,
                   TYFILE.TY_O2_REPO_1_3,
                   TYFILE.TY_INT_REPO_1_3,
                   TYFILE.TY_NO_REPO_1_4,
                   TYFILE.TY_AMT_REPO_1_4,
                   TYFILE.TY_O2_REPO_1_4,
                   TYFILE.TY_INT_REPO_1_4,
                   TYFILE.TY_NO_REPO_1_5,
                   TYFILE.TY_AMT_REPO_1_5,
                   TYFILE.TY_O2_REPO_1_5,
                   TYFILE.TY_INT_REPO_1_5,
                   TYFILE.TY_NO_REPO_2_1,
                   TYFILE.TY_AMT_REPO_2_1,
                   TYFILE.TY_O2_REPO_2_1,
                   TYFILE.TY_INT_REPO_2_1,
                   TYFILE.TY_NO_REPO_2_2,
                   TYFILE.TY_AMT_REPO_2_2,
                   TYFILE.TY_O2_REPO_2_2,
                   TYFILE.TY_INT_REPO_2_2,
                   TYFILE.TY_NO_REPO_2_3,
                   TYFILE.TY_AMT_REPO_2_3,
                   TYFILE.TY_O2_REPO_2_3,
                   TYFILE.TY_INT_REPO_2_3,
                   TYFILE.TY_NO_REPO_2_4,
                   TYFILE.TY_AMT_REPO_2_4,
                   TYFILE.TY_O2_REPO_2_4,
                   TYFILE.TY_INT_REPO_2_4,
                   TYFILE.TY_NO_REPO_2_5,
                   TYFILE.TY_AMT_REPO_2_5,
                   TYFILE.TY_O2_REPO_2_5,
                   TYFILE.TY_INT_REPO_2_5,
                   TYFILE.TY_NO_PL_1_1,
                   TYFILE.TY_AMT_PL_1_1,
                   TYFILE.TY_O2_PL_1_1,
                   TYFILE.TY_INT_PL_1_1,
                   TYFILE.TY_NO_PL_1_2,
                   TYFILE.TY_AMT_PL_1_2,
                   TYFILE.TY_O2_PL_1_2,
                   TYFILE.TY_INT_PL_1_2,
                   TYFILE.TY_NO_PL_1_3,
                   TYFILE.TY_AMT_PL_1_3,
                   TYFILE.TY_O2_PL_1_3,
                   TYFILE.TY_INT_PL_1_3,
                   TYFILE.TY_NO_PL_1_4,
                   TYFILE.TY_AMT_PL_1_4,
                   TYFILE.TY_O2_PL_1_4,
                   TYFILE.TY_INT_PL_1_4,
                   TYFILE.TY_NO_PL_1_5,
                   TYFILE.TY_AMT_PL_1_5,
                   TYFILE.TY_O2_PL_1_5,
                   TYFILE.TY_INT_PL_1_5,
                   TYFILE.TY_NO_PL_1_6,
                   TYFILE.TY_AMT_PL_1_6,
                   TYFILE.TY_O2_PL_1_6,
                   TYFILE.TY_INT_PL_1_6,
                   TYFILE.TY_NO_PL_1_7,
                   TYFILE.TY_AMT_PL_1_7,
                   TYFILE.TY_O2_PL_1_7,
                   TYFILE.TY_INT_PL_1_7,
                   TYFILE.TY_NO_PL_1_8,
                   TYFILE.TY_AMT_PL_1_8,
                   TYFILE.TY_O2_PL_1_8,
                   TYFILE.TY_INT_PL_1_8,
                   TYFILE.TY_NO_PL_1_9,
                   TYFILE.TY_AMT_PL_1_9,
                   TYFILE.TY_O2_PL_1_9,
                   TYFILE.TY_INT_PL_1_9,
                   TYFILE.TY_NO_PL_1_10,
                   TYFILE.TY_AMT_PL_1_10,
                   TYFILE.TY_O2_PL_1_10,
                   TYFILE.TY_INT_PL_1_10,
                   TYFILE.TY_NO_PL_2_1,
                   TYFILE.TY_AMT_PL_2_1,
                   TYFILE.TY_O2_PL_2_1,
                   TYFILE.TY_INT_PL_2_1,
                   TYFILE.TY_NO_PL_2_2,
                   TYFILE.TY_AMT_PL_2_2,
                   TYFILE.TY_O2_PL_2_2,
                   TYFILE.TY_INT_PL_2_2,
                   TYFILE.TY_NO_PL_2_3,
                   TYFILE.TY_AMT_PL_2_3,
                   TYFILE.TY_O2_PL_2_3,
                   TYFILE.TY_INT_PL_2_3,
                   TYFILE.TY_NO_PL_2_4,
                   TYFILE.TY_AMT_PL_2_4,
                   TYFILE.TY_O2_PL_2_4,
                   TYFILE.TY_INT_PL_2_4,
                   TYFILE.TY_NO_PL_2_5,
                   TYFILE.TY_AMT_PL_2_5,
                   TYFILE.TY_O2_PL_2_5,
                   TYFILE.TY_INT_PL_2_5,
                   TYFILE.TY_NO_PL_2_6,
                   TYFILE.TY_AMT_PL_2_6,
                   TYFILE.TY_O2_PL_2_6,
                   TYFILE.TY_INT_PL_2_6,
                   TYFILE.TY_NO_PL_2_7,
                   TYFILE.TY_AMT_PL_2_7,
                   TYFILE.TY_O2_PL_2_7,
                   TYFILE.TY_INT_PL_2_7,
                   TYFILE.TY_NO_PL_2_8,
                   TYFILE.TY_AMT_PL_2_8,
                   TYFILE.TY_O2_PL_2_8,
                   TYFILE.TY_INT_PL_2_8,
                   TYFILE.TY_NO_PL_2_9,
                   TYFILE.TY_AMT_PL_2_9,
                   TYFILE.TY_O2_PL_2_9,
                   TYFILE.TY_INT_PL_2_9,
                   TYFILE.TY_NO_PL_2_10,
                   TYFILE.TY_AMT_PL_2_10,
                   TYFILE.TY_O2_PL_2_10,
                   TYFILE.TY_INT_PL_2_10,
                   TYFILE.TY_DLRVPRIN_1_1,
                   TYFILE.TY_DLRVPRIN_1_2,
                   TYFILE.TY_DLRVPRIN_1_3,
                   TYFILE.TY_DLRVCHGS_1_1,
                   TYFILE.TY_DLRVCHGS_1_2,
                   TYFILE.TY_DLRVCHGS_1_3,
                   TYFILE.TY_DLPARVHELD_1,
                   TYFILE.TY_DLRVPRIN_2_1,
                   TYFILE.TY_DLRVPRIN_2_2,
                   TYFILE.TY_DLRVPRIN_2_3,
                   TYFILE.TY_DLRVCHGS_2_1,
                   TYFILE.TY_DLRVCHGS_2_2,
                   TYFILE.TY_DLRVCHGS_2_3,
                   TYFILE.TY_DLPARVHELD_2,
                   TYFILE.TY_PL_PRINCOL_1,
                   TYFILE.TY_PL_INTCOL_1,
                   TYFILE.TY_PL_LATECOL_1,
                   TYFILE.TY_PL_O2COL_1,
                   TYFILE.TY_PL_PRINCOL_2,
                   TYFILE.TY_PL_INTCOL_2,
                   TYFILE.TY_PL_LATECOL_2,
                   TYFILE.TY_PL_O2COL_2,
                   TYFILE.TY_PL_PRINCOL_3,
                   TYFILE.TY_PL_INTCOL_3,
                   TYFILE.TY_PL_LATECOL_3,
                   TYFILE.TY_PL_O2COL_3,
                   TYFILE.TY_PL_PRINCOL_4,
                   TYFILE.TY_PL_INTCOL_4,
                   TYFILE.TY_PL_LATECOL_4,
                   TYFILE.TY_PL_O2COL_4,
                   TYFILE.TY_PL_PRINCOL_5,
                   TYFILE.TY_PL_INTCOL_5,
                   TYFILE.TY_PL_LATECOL_5,
                   TYFILE.TY_PL_O2COL_5,
                   TYFILE.TY_AMT_SOLD,
                   TYFILE.TY_AMT_TRANS,
                   TYFILE.TY_AMT_PURCH,
                   TYFILE.TY_NO_SOLD,
                   TYFILE.TY_NO_TRANS,
                   TYFILE.TY_NO_PURCH,
                   TYFILE.TY_OTHCOL,
                   TYFILE.TY_OT2COL,
                   TYFILE.TY_INTCOL,
                   TYFILE.TY_LATECOL,
                   TYFILE.TY_PRINCOL,
                   TYFILE.TY_DEFCOL,
                   TYFILE.TY_RNPRIN,
                   TYFILE.TY_INTREB,
                   TYFILE.TY_CLREB1,
                   TYFILE.TY_CLREB2,
                   TYFILE.TY_CLREB3,
                   TYFILE.TY_CLREB4,
                   TYFILE.TY_AHREB,
                   TYFILE.TY_PPREB,
                   TYFILE.TY_O1REB,
                   TYFILE.TY_O2REB,
                   TYFILE.TY_O3REB,
                   TYFILE.TY_O4REB,
                   TYFILE.TY_O5REB,
                   TYFILE.TY_O6REB,
                   TYFILE.TY_O7REB,
                   TYFILE.TY_SCREB,
                   TYFILE.TY_MFREB,
                   TYFILE.TY_DFREB,
                   TYFILE.TY_UN7_REB,
                   TYFILE.TY_UN8_REB,
                   TYFILE.TY_UN9_REB,
                   TYFILE.TY_UN10_REB,
                   TYFILE.TY_PART_CHGOFF,
                   TYFILE.TY_INS_NO_ELIG_1,
                   TYFILE.TY_INS_NO_WROTE_1,
                   TYFILE.TY_INS_NO_NOTWROTE_1,
                   TYFILE.TY_INS_AMT_WROTE_1,
                   TYFILE.TY_INS_NO_ELIG_2,
                   TYFILE.TY_INS_NO_WROTE_2,
                   TYFILE.TY_INS_NO_NOTWROTE_2,
                   TYFILE.TY_INS_AMT_WROTE_2,
                   TYFILE.TY_INS_NO_ELIG_3,
                   TYFILE.TY_INS_NO_WROTE_3,
                   TYFILE.TY_INS_NO_NOTWROTE_3,
                   TYFILE.TY_INS_AMT_WROTE_3,
                   TYFILE.TY_INS_NO_ELIG_4,
                   TYFILE.TY_INS_NO_WROTE_4,
                   TYFILE.TY_INS_NO_NOTWROTE_4,
                   TYFILE.TY_INS_AMT_WROTE_4,
                   TYFILE.TY_INS_NO_ELIG_5,
                   TYFILE.TY_INS_NO_WROTE_5,
                   TYFILE.TY_INS_NO_NOTWROTE_5,
                   TYFILE.TY_INS_AMT_WROTE_5,
                   TYFILE.TY_INS_NO_ELIG_6,
                   TYFILE.TY_INS_NO_WROTE_6,
                   TYFILE.TY_INS_NO_NOTWROTE_6,
                   TYFILE.TY_INS_AMT_WROTE_6,
                   TYFILE.TY_INS_NO_ELIG_7,
                   TYFILE.TY_INS_NO_WROTE_7,
                   TYFILE.TY_INS_NO_NOTWROTE_7,
                   TYFILE.TY_INS_AMT_WROTE_7,
                   TYFILE.TY_INS_NO_ELIG_8,
                   TYFILE.TY_INS_NO_WROTE_8,
                   TYFILE.TY_INS_NO_NOTWROTE_8,
                   TYFILE.TY_INS_AMT_WROTE_8,
                   TYFILE.TY_INS_NO_ELIG_9,
                   TYFILE.TY_INS_NO_WROTE_9,
                   TYFILE.TY_INS_NO_NOTWROTE_9,
                   TYFILE.TY_INS_AMT_WROTE_9,
                   TYFILE.TY_INS_NO_ELIG_10,
                   TYFILE.TY_INS_NO_WROTE_10,
                   TYFILE.TY_INS_NO_NOTWROTE_10,
                   TYFILE.TY_INS_AMT_WROTE_10,
                   TYFILE.TY_FEE_NO_ELIG_1,
                   TYFILE.TY_FEE_NO_WROTE_1,
                   TYFILE.TY_FEE_NO_NOTWROTE_1,
                   TYFILE.TY_FEE_AMT_WROTE_1,
                   TYFILE.TY_FEE_NO_ELIG_2,
                   TYFILE.TY_FEE_NO_WROTE_2,
                   TYFILE.TY_FEE_NO_NOTWROTE_2,
                   TYFILE.TY_FEE_AMT_WROTE_2,
                   TYFILE.TY_FEE_NO_ELIG_3,
                   TYFILE.TY_FEE_NO_WROTE_3,
                   TYFILE.TY_FEE_NO_NOTWROTE_3,
                   TYFILE.TY_FEE_AMT_WROTE_3,
                   TYFILE.TY_OTH_NO_COL,
                   TYFILE.TY_OT2_NO_COL,
                   TYFILE.TY_INT_NO_COL,
                   TYFILE.TY_LATE_NO_COL,
                   TYFILE.TY_OTH_NO_WAIVED,
                   TYFILE.TY_OT2_NO_WAIVED,
                   TYFILE.TY_INT_NO_WAIVED,
                   TYFILE.TY_LATE_NO_WAIVED,
                   TYFILE.TY_OTH_AMT_WAIVED,
                   TYFILE.TY_OT2_AMT_WAIVED,
                   TYFILE.TY_INT_AMT_WAIVED,
                   TYFILE.TY_LATE_AMT_WAIVED,
                   TYFILE.TY_CKWRITTEN_1,
                   TYFILE.TY_CKWRITTEN_2,
                   TYFILE.TY_CKWRITTEN_3,
                   TYFILE.TY_CKWRITTEN_4,
                   TYFILE.TY_CKWRITTEN_5,
                   TYFILE.TY_CKWRITTEN_6,
                   TYFILE.TY_CKWRITTEN_7,
                   TYFILE.TY_CKWRITTEN_8,
                   TYFILE.TY_CKWRITTEN_9,
                   TYFILE.TY_CKWRITTEN_10,
                   TYFILE.TY_CKWRITTEN_11,
                   TYFILE.TY_CKWRITTEN_12,
                   TYFILE.TY_CKWRITTEN_13,
                   TYFILE.TY_CKWRITTEN_14,
                   TYFILE.TY_CKWRITTEN_15,
                   TYFILE.TY_CKWRITTEN_16,
                   TYFILE.TY_UNITS_1_1,
                   TYFILE.TY_OUTBAL_1_1,
                   TYFILE.TY_UNITS_1_2,
                   TYFILE.TY_OUTBAL_1_2,
                   TYFILE.TY_UNITS_1_3,
                   TYFILE.TY_OUTBAL_1_3,
                   TYFILE.TY_UNITS_1_4,
                   TYFILE.TY_OUTBAL_1_4,
                   TYFILE.TY_UNITS_1_5,
                   TYFILE.TY_OUTBAL_1_5,
                   TYFILE.TY_UNITS_1_6,
                   TYFILE.TY_OUTBAL_1_6,
                   TYFILE.TY_UNITS_1_7,
                   TYFILE.TY_OUTBAL_1_7,
                   TYFILE.TY_UNITS_1_8,
                   TYFILE.TY_OUTBAL_1_8,
                   TYFILE.TY_UNITS_1_9,
                   TYFILE.TY_OUTBAL_1_9,
                   TYFILE.TY_UNITS_1_10,
                   TYFILE.TY_OUTBAL_1_10,
                   TYFILE.TY_UNITS_2_1,
                   TYFILE.TY_OUTBAL_2_1,
                   TYFILE.TY_UNITS_2_2,
                   TYFILE.TY_OUTBAL_2_2,
                   TYFILE.TY_UNITS_2_3,
                   TYFILE.TY_OUTBAL_2_3,
                   TYFILE.TY_UNITS_2_4,
                   TYFILE.TY_OUTBAL_2_4,
                   TYFILE.TY_UNITS_2_5,
                   TYFILE.TY_OUTBAL_2_5,
                   TYFILE.TY_UNITS_2_6,
                   TYFILE.TY_OUTBAL_2_6,
                   TYFILE.TY_UNITS_2_7,
                   TYFILE.TY_OUTBAL_2_7,
                   TYFILE.TY_UNITS_2_8,
                   TYFILE.TY_OUTBAL_2_8,
                   TYFILE.TY_UNITS_2_9,
                   TYFILE.TY_OUTBAL_2_9,
                   TYFILE.TY_UNITS_2_10,
                   TYFILE.TY_OUTBAL_2_10,
                   TYFILE.TY_UNITS_BNKRPT_1_1,
                   TYFILE.TY_OUTBAL_BNKRPT_1_1,
                   TYFILE.TY_UNITS_BNKRPT_1_2,
                   TYFILE.TY_OUTBAL_BNKRPT_1_2,
                   TYFILE.TY_UNITS_BNKRPT_1_3,
                   TYFILE.TY_OUTBAL_BNKRPT_1_3,
                   TYFILE.TY_UNITS_BNKRPT_1_4,
                   TYFILE.TY_OUTBAL_BNKRPT_1_4,
                   TYFILE.TY_UNITS_BNKRPT_1_5,
                   TYFILE.TY_OUTBAL_BNKRPT_1_5,
                   TYFILE.TY_UNITS_BNKRPT_1_6,
                   TYFILE.TY_OUTBAL_BNKRPT_1_6,
                   TYFILE.TY_UNITS_BNKRPT_1_7,
                   TYFILE.TY_OUTBAL_BNKRPT_1_7,
                   TYFILE.TY_UNITS_BNKRPT_1_8,
                   TYFILE.TY_OUTBAL_BNKRPT_1_8,
                   TYFILE.TY_UNITS_BNKRPT_1_9,
                   TYFILE.TY_OUTBAL_BNKRPT_1_9,
                   TYFILE.TY_UNITS_BNKRPT_1_10,
                   TYFILE.TY_OUTBAL_BNKRPT_1_10,
                   TYFILE.TY_UNITS_BNKRPT_2_1,
                   TYFILE.TY_OUTBAL_BNKRPT_2_1,
                   TYFILE.TY_UNITS_BNKRPT_2_2,
                   TYFILE.TY_OUTBAL_BNKRPT_2_2,
                   TYFILE.TY_UNITS_BNKRPT_2_3,
                   TYFILE.TY_OUTBAL_BNKRPT_2_3,
                   TYFILE.TY_UNITS_BNKRPT_2_4,
                   TYFILE.TY_OUTBAL_BNKRPT_2_4,
                   TYFILE.TY_UNITS_BNKRPT_2_5,
                   TYFILE.TY_OUTBAL_BNKRPT_2_5,
                   TYFILE.TY_UNITS_BNKRPT_2_6,
                   TYFILE.TY_OUTBAL_BNKRPT_2_6,
                   TYFILE.TY_UNITS_BNKRPT_2_7,
                   TYFILE.TY_OUTBAL_BNKRPT_2_7,
                   TYFILE.TY_UNITS_BNKRPT_2_8,
                   TYFILE.TY_OUTBAL_BNKRPT_2_8,
                   TYFILE.TY_UNITS_BNKRPT_2_9,
                   TYFILE.TY_OUTBAL_BNKRPT_2_9,
                   TYFILE.TY_UNITS_BNKRPT_2_10,
                   TYFILE.TY_OUTBAL_BNKRPT_2_10,
                   TYFILE.TY_SUMWGTTM,
                   TYFILE.TY_SUMWGTAV,
                   TYFILE.TY_SUMWGTTA,
                   TYFILE.TY_ALLOTMENTS,
                   ISNULL(TYFILE.TY_BRFG,''),
                   TYFILE.TY_DLDISC,
                   TYFILE.TY_UNEARNED_AMT_1_1,
                   TYFILE.TY_UNEARNED_AMT_1_2,
                   TYFILE.TY_UNEARNED_AMT_1_3,
                   TYFILE.TY_UNEARNED_AMT_1_4,
                   TYFILE.TY_UNEARNED_AMT_1_5,
                   TYFILE.TY_UNEARNED_AMT_1_6,
                   TYFILE.TY_UNEARNED_AMT_1_7,
                   TYFILE.TY_UNEARNED_AMT_1_8,
                   TYFILE.TY_UNEARNED_AMT_1_9,
                   TYFILE.TY_UNEARNED_AMT_1_10,
                   TYFILE.TY_UNEARNED_AMT_2_1,
                   TYFILE.TY_UNEARNED_AMT_2_2,
                   TYFILE.TY_UNEARNED_AMT_2_3,
                   TYFILE.TY_UNEARNED_AMT_2_4,
                   TYFILE.TY_UNEARNED_AMT_2_5,
                   TYFILE.TY_UNEARNED_AMT_2_6,
                   TYFILE.TY_UNEARNED_AMT_2_7,
                   TYFILE.TY_UNEARNED_AMT_2_8,
                   TYFILE.TY_UNEARNED_AMT_2_9,
                   TYFILE.TY_UNEARNED_AMT_2_10,
                   TYFILE.TY_PROCEEDS,
                   TYFILE.TY_LOCKBOX
            INTO :QTY-REC
            FROM DBO.TYFILE
            WHERE TYFILE.TY_BRNO = :QTY-BRNO
              AND TYFILE.TY_DATE = :QTY-DATE
              AND TYFILE.TY_CLASS = :QTY-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TY1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TY-FIELDS.

      *-----------------------------------------------------------------
       READ-TY1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TY-PATH-OWNBR  TO TY-BRNO.
           MOVE TY-SELECT-PATH TO E-FILE.
           MOVE TY1-KEY        TO E-KEYX.
           INITIALIZE QTY-REC.

           IF ( TY1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TY1_CURSOR INTO :QTY-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TY1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TY-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TY1-FILE.

           IF ( TY1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TY1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TY1-CURSOR-STAT.

      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
 
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
              DESTROY IOTYRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
