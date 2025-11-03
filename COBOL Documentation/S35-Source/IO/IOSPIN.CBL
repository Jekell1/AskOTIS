       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOSPIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  03-18-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOSPIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : SPFILE 
      * TABLE DESC    : STATE PARAMETER FILE
      * OUTPUT FILE   : XPC/IOSPIN.PRE
      * USED FOR      : LIBLP/LPWSSP.CPY  --- QSPA-REC
      *                 LIBLP/LPSP1R.CPY  --- READ
      *                 LIBLP/LPSP1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0318 ***NEW***
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
           "@(#)IO/IOSPIN.CBL S34 05/18/21-11:28:15 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LPWSSP.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S 01 SQL COPY MEMBERS
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIBLP/LPWSSP.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
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

       01  LEGEND-BEG-ORGST. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING ORGST      (F3-ALL) ".

       01  LEGEND-END-ORGST. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING ORGST      ".

       01  LEGEND-BEG-SPRCLASS.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING SPRCLASS   (F3-ALL) ".

       01  LEGEND-END-SPRCLASS.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING SPRCLASS   ".

       01  LEGEND-BEG-SUBCLASS.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING SUBCLASS   (F3-ALL) ".

       01  LEGEND-END-SUBCLASS.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING SUBCLASS   ".

       01  LEGEND-BEG-LAWCODE.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING LAWCODE    (F3-ALL) ".

       01  LEGEND-END-LAWCODE.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING LAWCODE    ".

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
           03  BEG-ORGST               PIC XX.
           03  END-ORGST               PIC XX.
           03  BEG-SPRCLASS            PIC 999.
           03  END-SPRCLASS            PIC 999.
           03  BEG-SUBCLASS            PIC 99.
           03  END-SUBCLASS            PIC 99.
           03  BEG-LAWCODE             PIC 99.
           03  END-LAWCODE             PIC 99.

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
           03  FILLER PIC X(30) VALUE "ORGST  CLS  SUB  LAW  BANK".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-SPA-ORGST                  PIC X(02).
           03  FILLER                         PIC X(03).
           03  D01-SPA-SPRCLASS               PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-SPA-SUBCLASS               PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-SPA-LAWCODE                PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-SPA-BANKRULE               PIC X(01).

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
       COPY "LIBIO/IOSPIN_DEF.CPY".
       COPY "LIBIO/IOSPIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOSPIN_SCN.CPY".

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
           MOVE "B"                  TO SPA-PATH.

           MOVE BEG-ORGST            TO QSP1-WBEG-ORGST.
           MOVE END-ORGST            TO QSP1-WEND-ORGST.

           MOVE BEG-SPRCLASS         TO QSP1-WBEG-SPRCLASS.
           MOVE END-SPRCLASS         TO QSP1-WEND-SPRCLASS.

           MOVE BEG-SUBCLASS         TO QSP1-WBEG-SUBCLASS.
           MOVE END-SUBCLASS         TO QSP1-WEND-SUBCLASS.

           MOVE BEG-LAWCODE          TO QSP1-WBEG-LAWCODE.
           MOVE END-LAWCODE          TO QSP1-WEND-LAWCODE.

           PERFORM OPEN-SP1-FILE.
           PERFORM START-SP1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-SP1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( SP-ORGST < BEG-ORGST )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( SP-ORGST > END-ORGST )
              GO TO MAIN-PROGRAM-END.

           IF ( SP-SPRCLASS < BEG-SPRCLASS ) OR
              ( SP-SPRCLASS > END-SPRCLASS )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( SP-SUBCLASS < BEG-SUBCLASS ) OR
              ( SP-SUBCLASS > END-SUBCLASS )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( SP-LAWCODE < BEG-LAWCODE  ) OR
              ( SP-LAWCODE > END-LAWCODE  )
              GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPA-ORGST                  TO D01-SPA-ORGST.
           MOVE SPA-SPRCLASS               TO D01-SPA-SPRCLASS.
           MOVE SPA-SUBCLASS               TO D01-SPA-SUBCLASS.
           MOVE SPA-LAWCODE                TO D01-SPA-LAWCODE.
           MOVE SPA-BANKRULE               TO D01-SPA-BANKRULE.
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
           MOVE "IOSPIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOSPIN-QUEUE-POS  TO Q-POS.
           MOVE IOSPIN-COPIES-POS TO C-POS.
           MOVE IOSPIN-PRU-POS    TO P-POS.
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
       ENTER-BEG-ORGST.

           MOVE LEGEND-BEG-ORGST TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A020BEG-ORGST." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-ORGST.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ORGST.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-ORGST.

      *-----------------------------------------------------------------
       ENTER-END-ORGST.

           MOVE LEGEND-END-ORGST TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A020END-ORGST." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ORGST.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ORGST.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-ORGST.

           IF ( BEG-ORGST > END-ORGST )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ORGST.

           GO TO ENTER-BEG-SPRCLASS.

      *-----------------------------------------------------------------
       ENTER-ALL-ORGST.

           MOVE "S  A020BEG-ORGST." TO VDU.
           MOVE SPACES               TO BEG-ORGST VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020END-ORGST." TO VDU.
           MOVE ALL "Z"              TO END-ORGST VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SPRCLASS.

           MOVE LEGEND-BEG-SPRCLASS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN030BEG-SPRCLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ORGST.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SPRCLASS.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SPRCLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SPRCLASS.

      *-----------------------------------------------------------------
       ENTER-END-SPRCLASS.

           MOVE LEGEND-END-SPRCLASS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN030END-SPRCLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SPRCLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SPRCLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SPRCLASS.

           IF ( BEG-SPRCLASS > END-SPRCLASS )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SPRCLASS.

           GO TO ENTER-BEG-SUBCLASS.

      *-----------------------------------------------------------------
       ENTER-ALL-SPRCLASS.

           MOVE "SNNN030BEG-SPRCLASS." TO VDU.
           MOVE ZEROES               TO BEG-SPRCLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-SPRCLASS." TO VDU.
           MOVE ALL "9"              TO END-SPRCLASS VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SUBCLASS.

           MOVE LEGEND-BEG-SUBCLASS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020BEG-SUBCLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SPRCLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SUBCLASS.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SUBCLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SUBCLASS.

      *-----------------------------------------------------------------
       ENTER-END-SUBCLASS.

           MOVE LEGEND-END-SUBCLASS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020END-SUBCLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SUBCLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SUBCLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SUBCLASS.

           IF ( BEG-SUBCLASS > END-SUBCLASS )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SUBCLASS.

           GO TO ENTER-BEG-LAWCODE.

      *-----------------------------------------------------------------
       ENTER-ALL-SUBCLASS.

           MOVE "SNNN020BEG-SUBCLASS." TO VDU.
           MOVE ZEROES               TO BEG-SUBCLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-SUBCLASS." TO VDU.
           MOVE ALL "9"              TO END-SUBCLASS VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-LAWCODE.

           MOVE LEGEND-BEG-LAWCODE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020BEG-LAWCODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SUBCLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-LAWCODE.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LAWCODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-LAWCODE.

      *-----------------------------------------------------------------
       ENTER-END-LAWCODE.

           MOVE LEGEND-END-LAWCODE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020END-LAWCODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LAWCODE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-LAWCODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-LAWCODE.

           IF ( BEG-LAWCODE > END-LAWCODE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LAWCODE.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-LAWCODE.

           MOVE "SNNN020BEG-LAWCODE." TO VDU.
           MOVE ZEROES               TO BEG-LAWCODE VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-LAWCODE." TO VDU.
           MOVE ALL "9"              TO END-LAWCODE VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-LAWCODE.
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

           MOVE "S  A020BEG-ORGST."    TO VDU.
           MOVE BEG-ORGST              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020END-ORGST."    TO VDU.
           MOVE END-ORGST              TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN030BEG-SPRCLASS." TO VDU.
           MOVE BEG-SPRCLASS           TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-SPRCLASS." TO VDU.
           MOVE END-SPRCLASS           TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020BEG-SUBCLASS." TO VDU.
           MOVE BEG-SUBCLASS           TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-SUBCLASS." TO VDU.
           MOVE END-SUBCLASS           TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020BEG-LAWCODE." TO VDU.
           MOVE BEG-LAWCODE            TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-LAWCODE." TO VDU.
           MOVE END-LAWCODE            TO VDUNUM0.
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

           MOVE SPA1-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING ORGST               : "    TO R01-PROMPT.
           MOVE BEG-ORGST                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ORGST               : "    TO R01-PROMPT.
           MOVE END-ORGST                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SPRCLASS            : "    TO R01-PROMPT.
           MOVE BEG-SPRCLASS                          TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SPRCLASS            : "    TO R01-PROMPT.
           MOVE END-SPRCLASS                          TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SUBCLASS            : "    TO R01-PROMPT.
           MOVE BEG-SUBCLASS                          TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SUBCLASS            : "    TO R01-PROMPT.
           MOVE END-SUBCLASS                          TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING LAWCODE             : "    TO R01-PROMPT.
           MOVE BEG-LAWCODE                           TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    LAWCODE             : "    TO R01-PROMPT.
           MOVE END-LAWCODE                           TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY -SPAFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOSPIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOSPIN-SYSDATE.
           DISPLAY IOSPIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOSPIN-FORM-FIELDS TO WR-BUF.
           MOVE IOSPIN-FORM-LIST TO WR-LIST.
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

           MOVE IOSPIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOSPIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOSPIN_EVA.CPY".
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
       COPY "LIBLP/LPTBNO.CPY".

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
      *    LOAD SPA-REC FROM QSPA-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-SPA-FIELDS.
           MOVE QSPA-ORGST TO SPA-ORGST.
           MOVE QSPA-SPRCLASS TO SPA-SPRCLASS.
           MOVE QSPA-SUBCLASS TO SPA-SUBCLASS.
           MOVE QSPA-LAWCODE TO SPA-LAWCODE.

           IF ( QSPA-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO SPA-LTOUCH-DATE
           ELSE
              MOVE QSPA-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO SPA-LTOUCH-DATE.

           MOVE QSPA-DESC TO SPA-DESC.
           MOVE QSPA-NOTES TO SPA-NOTES.

           IF ( QSPA-EFFDATE = "1900-01-01" )
              MOVE ZEROES TO SPA-EFFDATE
           ELSE
              MOVE QSPA-EFFDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO SPA-EFFDATE.


           IF ( QSPA-EXPDATE = "1900-01-01" )
              MOVE ZEROES TO SPA-EXPDATE
           ELSE
              MOVE QSPA-EXPDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO SPA-EXPDATE.

           MOVE QSPA-EXPD-MONTHS TO SPA-EXPD-MONTHS.
           MOVE QSPA-EXPD-DAYS TO SPA-EXPD-DAYS.
           MOVE QSPA-EXPD-FRMLA TO SPA-EXPD-FRMLA.
           MOVE QSPA-EXPD-EXCEPT TO SPA-EXPD-EXCEPT.
           MOVE QSPA-EXPR-FRMLA TO SPA-EXPR-FRMLA.
           MOVE QSPA-EXPR-REDU-FRMLA TO SPA-EXPR-REDU-FRMLA.
           MOVE QSPA-EXPR-RATE-1 TO SPA-EXPR-RATE(1).
           MOVE QSPA-EXPR-BREAK-1 TO SPA-EXPR-BREAK(1).
           MOVE QSPA-EXPR-YRTYPE-1 TO SPA-EXPR-YRTYPE(1).
           MOVE QSPA-EXPR-INTMETH-1 TO SPA-EXPR-INTMETH(1).
           MOVE QSPA-EXPR-RATE-2 TO SPA-EXPR-RATE(2).
           MOVE QSPA-EXPR-BREAK-2 TO SPA-EXPR-BREAK(2).
           MOVE QSPA-EXPR-YRTYPE-2 TO SPA-EXPR-YRTYPE(2).
           MOVE QSPA-EXPR-INTMETH-2 TO SPA-EXPR-INTMETH(2).
           MOVE QSPA-EXPR-RATE-3 TO SPA-EXPR-RATE(3).
           MOVE QSPA-EXPR-BREAK-3 TO SPA-EXPR-BREAK(3).
           MOVE QSPA-EXPR-YRTYPE-3 TO SPA-EXPR-YRTYPE(3).
           MOVE QSPA-EXPR-INTMETH-3 TO SPA-EXPR-INTMETH(3).
           MOVE QSPA-EXPR-RATE-4 TO SPA-EXPR-RATE(4).
           MOVE QSPA-EXPR-BREAK-4 TO SPA-EXPR-BREAK(4).
           MOVE QSPA-EXPR-YRTYPE-4 TO SPA-EXPR-YRTYPE(4).
           MOVE QSPA-EXPR-INTMETH-4 TO SPA-EXPR-INTMETH(4).
           MOVE QSPA-EXPR-RATE-5 TO SPA-EXPR-RATE(5).
           MOVE QSPA-EXPR-BREAK-5 TO SPA-EXPR-BREAK(5).
           MOVE QSPA-EXPR-YRTYPE-5 TO SPA-EXPR-YRTYPE(5).
           MOVE QSPA-EXPR-INTMETH-5 TO SPA-EXPR-INTMETH(5).
           MOVE QSPA-EXPR-LCHG TO SPA-EXPR-LCHG.
           MOVE QSPA-JUDGFRMLA TO SPA-JUDGFRMLA.
           MOVE QSPA-JUDGRATE TO SPA-JUDGRATE.
           MOVE QSPA-JUDGYRTYPE TO SPA-JUDGYRTYPE.
           MOVE QSPA-JUDGINTMETH TO SPA-JUDGINTMETH.
           MOVE QSPA-ACC-FRMLA TO SPA-ACC-FRMLA.
           MOVE QSPA-ACC-YRTYPE TO SPA-ACC-YRTYPE.
           MOVE QSPA-ACC-INTMETH TO SPA-ACC-INTMETH.
           MOVE QSPA-ACC-RATE-1 TO SPA-ACC-RATE(1).
           MOVE QSPA-ACC-BREAK-1 TO SPA-ACC-BREAK(1).
           MOVE QSPA-ACC-RATE-2 TO SPA-ACC-RATE(2).
           MOVE QSPA-ACC-BREAK-2 TO SPA-ACC-BREAK(2).
           MOVE QSPA-ACC-RATE-3 TO SPA-ACC-RATE(3).
           MOVE QSPA-ACC-BREAK-3 TO SPA-ACC-BREAK(3).
           MOVE QSPA-ACC-RATE-4 TO SPA-ACC-RATE(4).
           MOVE QSPA-ACC-BREAK-4 TO SPA-ACC-BREAK(4).
           MOVE QSPA-ACC-RATE-5 TO SPA-ACC-RATE(5).
           MOVE QSPA-ACC-BREAK-5 TO SPA-ACC-BREAK(5).
           MOVE QSPA-1098FRMLA TO SPA-1098FRMLA.
           MOVE QSPA-DISFRMLA TO SPA-DISFRMLA.
           MOVE QSPA-CONTRFRMLA TO SPA-CONTRFRMLA.
           MOVE QSPA-BANKRULE TO SPA-BANKRULE.
           MOVE QSPA-CONTPDTH TO SPA-CONTPDTH.
           MOVE QSPA-FINCHG TO SPA-FINCHG.
           MOVE QSPA-BANKUNITPER-CD TO SPA-BANKUNITPER-CD.
           MOVE QSPA-TRWBANKRULE TO SPA-TRWBANKRULE.
           MOVE QSPA-TRWCONTPDTH TO SPA-TRWCONTPDTH.
           MOVE QSPA-TRWBANKUNITPER-CD TO SPA-TRWBANKUNITPER-CD.
           MOVE QSPA-RECFRMLA TO SPA-RECFRMLA.
           MOVE QSPA-RECPAYCD TO SPA-RECPAYCD.
           MOVE QSPA-RECPDTH TO SPA-RECPDTH.
           MOVE QSPA-DISFRMLA-SPAY-FG TO SPA-DISFRMLA-SPAY-FG.
           MOVE QSPA-ERNFRMLA-1 TO SPA-ERNFRMLA(1).
           MOVE QSPA-ERNAMT-1 TO SPA-ERNAMT(1).
           MOVE QSPA-ERNPERCT-1 TO SPA-ERNPERCT(1).
           MOVE QSPA-ERNPERCD-1 TO SPA-ERNPERCD(1).
           MOVE QSPA-ERNACCRL-FG-1 TO SPA-ERNACCRL-FG(1).
           MOVE QSPA-ERNACCRL-START-CD-1 TO SPA-ERNACCRL-START-CD(1).
           MOVE QSPA-ERNACCRL-DFCD-1 TO SPA-ERNACCRL-DFCD(1).
           MOVE QSPA-ERN-DAYS-1 TO SPA-ERN-DAYS(1).
           MOVE QSPA-ERN-TERM-1 TO SPA-ERN-TERM(1).
           MOVE QSPA-ERN-TYPE-1 TO SPA-ERN-TYPE(1).
           MOVE QSPA-ERNFRMLA-2 TO SPA-ERNFRMLA(2).
           MOVE QSPA-ERNAMT-2 TO SPA-ERNAMT(2).
           MOVE QSPA-ERNPERCT-2 TO SPA-ERNPERCT(2).
           MOVE QSPA-ERNPERCD-2 TO SPA-ERNPERCD(2).
           MOVE QSPA-ERNACCRL-FG-2 TO SPA-ERNACCRL-FG(2).
           MOVE QSPA-ERNACCRL-START-CD-2 TO SPA-ERNACCRL-START-CD(2).
           MOVE QSPA-ERNACCRL-DFCD-2 TO SPA-ERNACCRL-DFCD(2).
           MOVE QSPA-ERN-DAYS-2 TO SPA-ERN-DAYS(2).
           MOVE QSPA-ERN-TERM-2 TO SPA-ERN-TERM(2).
           MOVE QSPA-ERN-TYPE-2 TO SPA-ERN-TYPE(2).
           MOVE QSPA-ERNFRMLA-3 TO SPA-ERNFRMLA(3).
           MOVE QSPA-ERNAMT-3 TO SPA-ERNAMT(3).
           MOVE QSPA-ERNPERCT-3 TO SPA-ERNPERCT(3).
           MOVE QSPA-ERNPERCD-3 TO SPA-ERNPERCD(3).
           MOVE QSPA-ERNACCRL-FG-3 TO SPA-ERNACCRL-FG(3).
           MOVE QSPA-ERNACCRL-START-CD-3 TO SPA-ERNACCRL-START-CD(3).
           MOVE QSPA-ERNACCRL-DFCD-3 TO SPA-ERNACCRL-DFCD(3).
           MOVE QSPA-ERN-DAYS-3 TO SPA-ERN-DAYS(3).
           MOVE QSPA-ERN-TERM-3 TO SPA-ERN-TERM(3).
           MOVE QSPA-ERN-TYPE-3 TO SPA-ERN-TYPE(3).
           MOVE QSPA-ERNFRMLA-4 TO SPA-ERNFRMLA(4).
           MOVE QSPA-ERNAMT-4 TO SPA-ERNAMT(4).
           MOVE QSPA-ERNPERCT-4 TO SPA-ERNPERCT(4).
           MOVE QSPA-ERNPERCD-4 TO SPA-ERNPERCD(4).
           MOVE QSPA-ERNACCRL-FG-4 TO SPA-ERNACCRL-FG(4).
           MOVE QSPA-ERNACCRL-START-CD-4 TO SPA-ERNACCRL-START-CD(4).
           MOVE QSPA-ERNACCRL-DFCD-4 TO SPA-ERNACCRL-DFCD(4).
           MOVE QSPA-ERN-DAYS-4 TO SPA-ERN-DAYS(4).
           MOVE QSPA-ERN-TERM-4 TO SPA-ERN-TERM(4).
           MOVE QSPA-ERN-TYPE-4 TO SPA-ERN-TYPE(4).
           MOVE QSPA-ERNFRMLA-5 TO SPA-ERNFRMLA(5).
           MOVE QSPA-ERNAMT-5 TO SPA-ERNAMT(5).
           MOVE QSPA-ERNPERCT-5 TO SPA-ERNPERCT(5).
           MOVE QSPA-ERNPERCD-5 TO SPA-ERNPERCD(5).
           MOVE QSPA-ERNACCRL-FG-5 TO SPA-ERNACCRL-FG(5).
           MOVE QSPA-ERNACCRL-START-CD-5 TO SPA-ERNACCRL-START-CD(5).
           MOVE QSPA-ERNACCRL-DFCD-5 TO SPA-ERNACCRL-DFCD(5).
           MOVE QSPA-ERN-DAYS-5 TO SPA-ERN-DAYS(5).
           MOVE QSPA-ERN-TERM-5 TO SPA-ERN-TERM(5).
           MOVE QSPA-ERN-TYPE-5 TO SPA-ERN-TYPE(5).
           MOVE QSPA-ERNFRMLA-6 TO SPA-ERNFRMLA(6).
           MOVE QSPA-ERNAMT-6 TO SPA-ERNAMT(6).
           MOVE QSPA-ERNPERCT-6 TO SPA-ERNPERCT(6).
           MOVE QSPA-ERNPERCD-6 TO SPA-ERNPERCD(6).
           MOVE QSPA-ERNACCRL-FG-6 TO SPA-ERNACCRL-FG(6).
           MOVE QSPA-ERNACCRL-START-CD-6 TO SPA-ERNACCRL-START-CD(6).
           MOVE QSPA-ERNACCRL-DFCD-6 TO SPA-ERNACCRL-DFCD(6).
           MOVE QSPA-ERN-DAYS-6 TO SPA-ERN-DAYS(6).
           MOVE QSPA-ERN-TERM-6 TO SPA-ERN-TERM(6).
           MOVE QSPA-ERN-TYPE-6 TO SPA-ERN-TYPE(6).
           MOVE QSPA-ERNFRMLA-7 TO SPA-ERNFRMLA(7).
           MOVE QSPA-ERNAMT-7 TO SPA-ERNAMT(7).
           MOVE QSPA-ERNPERCT-7 TO SPA-ERNPERCT(7).
           MOVE QSPA-ERNPERCD-7 TO SPA-ERNPERCD(7).
           MOVE QSPA-ERNACCRL-FG-7 TO SPA-ERNACCRL-FG(7).
           MOVE QSPA-ERNACCRL-START-CD-7 TO SPA-ERNACCRL-START-CD(7).
           MOVE QSPA-ERNACCRL-DFCD-7 TO SPA-ERNACCRL-DFCD(7).
           MOVE QSPA-ERN-DAYS-7 TO SPA-ERN-DAYS(7).
           MOVE QSPA-ERN-TERM-7 TO SPA-ERN-TERM(7).
           MOVE QSPA-ERN-TYPE-7 TO SPA-ERN-TYPE(7).
           MOVE QSPA-ERNFRMLA-8 TO SPA-ERNFRMLA(8).
           MOVE QSPA-ERNAMT-8 TO SPA-ERNAMT(8).
           MOVE QSPA-ERNPERCT-8 TO SPA-ERNPERCT(8).
           MOVE QSPA-ERNPERCD-8 TO SPA-ERNPERCD(8).
           MOVE QSPA-ERNACCRL-FG-8 TO SPA-ERNACCRL-FG(8).
           MOVE QSPA-ERNACCRL-START-CD-8 TO SPA-ERNACCRL-START-CD(8).
           MOVE QSPA-ERNACCRL-DFCD-8 TO SPA-ERNACCRL-DFCD(8).
           MOVE QSPA-ERN-DAYS-8 TO SPA-ERN-DAYS(8).
           MOVE QSPA-ERN-TERM-8 TO SPA-ERN-TERM(8).
           MOVE QSPA-ERN-TYPE-8 TO SPA-ERN-TYPE(8).
           MOVE QSPA-ERNFRMLA-9 TO SPA-ERNFRMLA(9).
           MOVE QSPA-ERNAMT-9 TO SPA-ERNAMT(9).
           MOVE QSPA-ERNPERCT-9 TO SPA-ERNPERCT(9).
           MOVE QSPA-ERNPERCD-9 TO SPA-ERNPERCD(9).
           MOVE QSPA-ERNACCRL-FG-9 TO SPA-ERNACCRL-FG(9).
           MOVE QSPA-ERNACCRL-START-CD-9 TO SPA-ERNACCRL-START-CD(9).
           MOVE QSPA-ERNACCRL-DFCD-9 TO SPA-ERNACCRL-DFCD(9).
           MOVE QSPA-ERN-DAYS-9 TO SPA-ERN-DAYS(9).
           MOVE QSPA-ERN-TERM-9 TO SPA-ERN-TERM(9).
           MOVE QSPA-ERN-TYPE-9 TO SPA-ERN-TYPE(9).
           MOVE QSPA-ERNFRMLA-10 TO SPA-ERNFRMLA(10).
           MOVE QSPA-ERNAMT-10 TO SPA-ERNAMT(10).
           MOVE QSPA-ERNPERCT-10 TO SPA-ERNPERCT(10).
           MOVE QSPA-ERNPERCD-10 TO SPA-ERNPERCD(10).
           MOVE QSPA-ERNACCRL-FG-10 TO SPA-ERNACCRL-FG(10).
           MOVE QSPA-ERNACCRL-START-CD-10 TO SPA-ERNACCRL-START-CD(10).
           MOVE QSPA-ERNACCRL-DFCD-10 TO SPA-ERNACCRL-DFCD(10).
           MOVE QSPA-ERN-DAYS-10 TO SPA-ERN-DAYS(10).
           MOVE QSPA-ERN-TERM-10 TO SPA-ERN-TERM(10).
           MOVE QSPA-ERN-TYPE-10 TO SPA-ERN-TYPE(10).
           MOVE QSPA-ERNACCEL-CD TO SPA-ERNACCEL-CD.
           MOVE QSPA-ERNJD-CD TO SPA-ERNJD-CD.
           MOVE QSPA-MIDSTRERNCD TO SPA-MIDSTRERNCD.
           MOVE QSPA-EARN-ORDER-1 TO SPA-EARN-ORDER(1).
           MOVE QSPA-EARN-ORDER-2 TO SPA-EARN-ORDER(2).
           MOVE QSPA-EARN-ORDER-3 TO SPA-EARN-ORDER(3).
           MOVE QSPA-EARN-ORDER-4 TO SPA-EARN-ORDER(4).
           MOVE QSPA-EARN-ORDER-5 TO SPA-EARN-ORDER(5).
           MOVE QSPA-EARN-ORDER-6 TO SPA-EARN-ORDER(6).
           MOVE QSPA-EARN-ORDER-7 TO SPA-EARN-ORDER(7).
           MOVE QSPA-EARN-ORDER-8 TO SPA-EARN-ORDER(8).
           MOVE QSPA-EARN-ORDER-9 TO SPA-EARN-ORDER(9).
           MOVE QSPA-EARN-ORDER-10 TO SPA-EARN-ORDER(10).
           MOVE QSPA-ERNDELQFRMLA TO SPA-ERNDELQFRMLA.
           MOVE QSPA-ERNDELQ-START TO SPA-ERNDELQ-START.
           MOVE QSPA-ERNDELQ-STOP TO SPA-ERNDELQ-STOP.
           MOVE QSPA-1098-INC-LC TO SPA-1098-INC-LC.
           MOVE QSPA-MAX-IB-ACR-DAYS TO SPA-MAX-IB-ACR-DAYS.
           MOVE QSPA-LCGRACE TO SPA-LCGRACE.
           MOVE QSPA-LCGRACECD TO SPA-LCGRACECD.
           MOVE QSPA-LCTYPE TO SPA-LCTYPE.
           MOVE QSPA-LCRATE TO SPA-LCRATE.
           MOVE QSPA-LCFRMLA TO SPA-LCFRMLA.
           MOVE QSPA-LCMINCHG TO SPA-LCMINCHG.
           MOVE QSPA-LCMAXAMT TO SPA-LCMAXAMT.
           MOVE QSPA-LCDELFAC TO SPA-LCDELFAC.
           MOVE QSPA-LCYRTYPE TO SPA-LCYRTYPE.
           MOVE QSPA-LC-CODE TO SPA-LC-CODE.
           MOVE QSPA-LCUNITPER-CD TO SPA-LCUNITPER-CD.
           MOVE QSPA-DEFFRMLA TO SPA-DEFFRMLA.
           MOVE QSPA-DEFEXCEPT TO SPA-DEFEXCEPT.
           MOVE QSPA-DEFWORKER TO SPA-DEFWORKER.
           MOVE QSPA-DEFPERCD TO SPA-DEFPERCD.
           MOVE QSPA-DEFBASE TO SPA-DEFBASE.
           MOVE QSPA-DEFPOLCD TO SPA-DEFPOLCD.
           MOVE QSPA-DEFPOLMX-1 TO SPA-DEFPOLMX(1).
           MOVE QSPA-DEFPOLMX-2 TO SPA-DEFPOLMX(2).
           MOVE QSPA-DEFPOLMX-3 TO SPA-DEFPOLMX(3).
           MOVE QSPA-DEFMIN TO SPA-DEFMIN.
           MOVE QSPA-DEFMAX TO SPA-DEFMAX.
           MOVE QSPA-DEFUNITPER-CD TO SPA-DEFUNITPER-CD.
           MOVE QSPA-PRFORM-1 TO SPA-PRFORM(1).
           MOVE QSPA-PRFLAG-1 TO SPA-PRFLAG(1).
           MOVE QSPA-PRFORM-2 TO SPA-PRFORM(2).
           MOVE QSPA-PRFLAG-2 TO SPA-PRFLAG(2).
           MOVE QSPA-PRFORM-3 TO SPA-PRFORM(3).
           MOVE QSPA-PRFLAG-3 TO SPA-PRFLAG(3).
           MOVE QSPA-PRFORM-4 TO SPA-PRFORM(4).
           MOVE QSPA-PRFLAG-4 TO SPA-PRFLAG(4).
           MOVE QSPA-PRFORM-5 TO SPA-PRFORM(5).
           MOVE QSPA-PRFLAG-5 TO SPA-PRFLAG(5).
           MOVE QSPA-PRFORM-6 TO SPA-PRFORM(6).
           MOVE QSPA-PRFLAG-6 TO SPA-PRFLAG(6).
           MOVE QSPA-PRFORM-7 TO SPA-PRFORM(7).
           MOVE QSPA-PRFLAG-7 TO SPA-PRFLAG(7).
           MOVE QSPA-PRFORM-8 TO SPA-PRFORM(8).
           MOVE QSPA-PRFLAG-8 TO SPA-PRFLAG(8).
           MOVE QSPA-PRFORM-9 TO SPA-PRFORM(9).
           MOVE QSPA-PRFLAG-9 TO SPA-PRFLAG(9).
           MOVE QSPA-PRFORM-10 TO SPA-PRFORM(10).
           MOVE QSPA-PRFLAG-10 TO SPA-PRFLAG(10).
           MOVE QSPA-PRFORM-11 TO SPA-PRFORM(11).
           MOVE QSPA-PRFLAG-11 TO SPA-PRFLAG(11).
           MOVE QSPA-PRFORM-12 TO SPA-PRFORM(12).
           MOVE QSPA-PRFLAG-12 TO SPA-PRFLAG(12).
           MOVE QSPA-PRFORM-13 TO SPA-PRFORM(13).
           MOVE QSPA-PRFLAG-13 TO SPA-PRFLAG(13).
           MOVE QSPA-PRFORM-14 TO SPA-PRFORM(14).
           MOVE QSPA-PRFLAG-14 TO SPA-PRFLAG(14).
           MOVE QSPA-PRFORM-15 TO SPA-PRFORM(15).
           MOVE QSPA-PRFLAG-15 TO SPA-PRFLAG(15).
           MOVE QSPA-PRFORM-16 TO SPA-PRFORM(16).
           MOVE QSPA-PRFLAG-16 TO SPA-PRFLAG(16).
           MOVE QSPA-PRFORM-17 TO SPA-PRFORM(17).
           MOVE QSPA-PRFLAG-17 TO SPA-PRFLAG(17).
           MOVE QSPA-PRFORM-18 TO SPA-PRFORM(18).
           MOVE QSPA-PRFLAG-18 TO SPA-PRFLAG(18).
           MOVE QSPA-PRFORM-19 TO SPA-PRFORM(19).
           MOVE QSPA-PRFLAG-19 TO SPA-PRFLAG(19).
           MOVE QSPA-PRFORM-20 TO SPA-PRFORM(20).
           MOVE QSPA-PRFLAG-20 TO SPA-PRFLAG(20).
           MOVE QSPA-IMED-REC TO SPA-IMED-REC.
           MOVE QSPA-SCFRMLA TO SPA-SCFRMLA.
           MOVE QSPA-SCRATE TO SPA-SCRATE.
           MOVE QSPA-SCMIN TO SPA-SCMIN.
           MOVE QSPA-SCMAX TO SPA-SCMAX.
           MOVE QSPA-MFFRMLA TO SPA-MFFRMLA.
           MOVE QSPA-MFRATE TO SPA-MFRATE.
           MOVE QSPA-MFMIN TO SPA-MFMIN.
           MOVE QSPA-MFMAX TO SPA-MFMAX.
           MOVE QSPA-INTMETHOD TO SPA-INTMETHOD.
           MOVE QSPA-YEARTYPE TO SPA-YEARTYPE.
           MOVE QSPA-PYEXFRMLA TO SPA-PYEXFRMLA.
           MOVE QSPA-PYEXSPCODE TO SPA-PYEXSPCODE.
           MOVE QSPA-PYEXYEARTP TO SPA-PYEXYEARTP.
           MOVE QSPA-PYEXFMDATE TO SPA-PYEXFMDATE.
           MOVE QSPA-PYEXBASECD TO SPA-PYEXBASECD.
           MOVE QSPA-PYEXBASE TO SPA-PYEXBASE.
           MOVE QSPA-PYEXDAYS TO SPA-PYEXDAYS.
           MOVE QSPA-CAL-SPFRMLA TO SPA-CAL-SPFRMLA.
           MOVE QSPA-CAL-RATETYPE-1 TO SPA-CAL-RATETYPE(1).
           MOVE QSPA-CAL-BREAKTYPE-1 TO SPA-CAL-BREAKTYPE(1).
           MOVE QSPA-CAL-CHGABLE-1 TO SPA-CAL-CHGABLE(1).
           MOVE QSPA-CAL-NORATES-1 TO SPA-CAL-NORATES(1).
           MOVE QSPA-CAL-TBFRMLA-1 TO SPA-CAL-TBFRMLA(1).
           MOVE QSPA-CAL-RATE-1-1 TO SPA-CAL-RATE(1 1).
           MOVE QSPA-CAL-BREAK-1-1 TO SPA-CAL-BREAK(1 1).
           MOVE QSPA-CAL-RATE-1-2 TO SPA-CAL-RATE(1 2).
           MOVE QSPA-CAL-BREAK-1-2 TO SPA-CAL-BREAK(1 2).
           MOVE QSPA-CAL-RATE-1-3 TO SPA-CAL-RATE(1 3).
           MOVE QSPA-CAL-BREAK-1-3 TO SPA-CAL-BREAK(1 3).
           MOVE QSPA-CAL-RATE-1-4 TO SPA-CAL-RATE(1 4).
           MOVE QSPA-CAL-BREAK-1-4 TO SPA-CAL-BREAK(1 4).
           MOVE QSPA-CAL-RATE-1-5 TO SPA-CAL-RATE(1 5).
           MOVE QSPA-CAL-BREAK-1-5 TO SPA-CAL-BREAK(1 5).
           MOVE QSPA-CAL-RATETYPE-2 TO SPA-CAL-RATETYPE(2).
           MOVE QSPA-CAL-BREAKTYPE-2 TO SPA-CAL-BREAKTYPE(2).
           MOVE QSPA-CAL-CHGABLE-2 TO SPA-CAL-CHGABLE(2).
           MOVE QSPA-CAL-NORATES-2 TO SPA-CAL-NORATES(2).
           MOVE QSPA-CAL-TBFRMLA-2 TO SPA-CAL-TBFRMLA(2).
           MOVE QSPA-CAL-RATE-2-1 TO SPA-CAL-RATE(2 1).
           MOVE QSPA-CAL-BREAK-2-1 TO SPA-CAL-BREAK(2 1).
           MOVE QSPA-CAL-RATE-2-2 TO SPA-CAL-RATE(2 2).
           MOVE QSPA-CAL-BREAK-2-2 TO SPA-CAL-BREAK(2 2).
           MOVE QSPA-CAL-RATE-2-3 TO SPA-CAL-RATE(2 3).
           MOVE QSPA-CAL-BREAK-2-3 TO SPA-CAL-BREAK(2 3).
           MOVE QSPA-CAL-RATE-2-4 TO SPA-CAL-RATE(2 4).
           MOVE QSPA-CAL-BREAK-2-4 TO SPA-CAL-BREAK(2 4).
           MOVE QSPA-CAL-RATE-2-5 TO SPA-CAL-RATE(2 5).
           MOVE QSPA-CAL-BREAK-2-5 TO SPA-CAL-BREAK(2 5).
           MOVE QSPA-MINSF-FRMLA TO SPA-MINSF-FRMLA.
           MOVE QSPA-MINSF-CHG TO SPA-MINSF-CHG.
           MOVE QSPA-MAXAPR-FRMLA TO SPA-MAXAPR-FRMLA.
           MOVE QSPA-EDT-NOEDITS-1 TO SPA-EDT-NOEDITS(1).
           MOVE QSPA-EDT-TYPE-1-1 TO SPA-EDT-TYPE(1 1).
           MOVE QSPA-EDT-HL-1-1 TO SPA-EDT-HL(1 1).
           MOVE QSPA-EDT-COLL-1-1 TO SPA-EDT-COLL(1 1).
           MOVE QSPA-EDT-CD-1-1 TO SPA-EDT-CD(1 1).
           MOVE QSPA-EDT-RATE-1-1 TO SPA-EDT-RATE(1 1).
           MOVE QSPA-EDT-RATETYPE-1-1 TO SPA-EDT-RATETYPE(1 1).
           MOVE QSPA-EDT-AMT-1-1 TO SPA-EDT-AMT(1 1).
           MOVE QSPA-EDT-AMTYPE-1-1 TO SPA-EDT-AMTYPE(1 1).
           MOVE QSPA-EDT-TERM-1-1 TO SPA-EDT-TERM(1 1).
           MOVE QSPA-EDT-TYPE-1-2 TO SPA-EDT-TYPE(1 2).
           MOVE QSPA-EDT-HL-1-2 TO SPA-EDT-HL(1 2).
           MOVE QSPA-EDT-COLL-1-2 TO SPA-EDT-COLL(1 2).
           MOVE QSPA-EDT-CD-1-2 TO SPA-EDT-CD(1 2).
           MOVE QSPA-EDT-RATE-1-2 TO SPA-EDT-RATE(1 2).
           MOVE QSPA-EDT-RATETYPE-1-2 TO SPA-EDT-RATETYPE(1 2).
           MOVE QSPA-EDT-AMT-1-2 TO SPA-EDT-AMT(1 2).
           MOVE QSPA-EDT-AMTYPE-1-2 TO SPA-EDT-AMTYPE(1 2).
           MOVE QSPA-EDT-TERM-1-2 TO SPA-EDT-TERM(1 2).
           MOVE QSPA-EDT-TYPE-1-3 TO SPA-EDT-TYPE(1 3).
           MOVE QSPA-EDT-HL-1-3 TO SPA-EDT-HL(1 3).
           MOVE QSPA-EDT-COLL-1-3 TO SPA-EDT-COLL(1 3).
           MOVE QSPA-EDT-CD-1-3 TO SPA-EDT-CD(1 3).
           MOVE QSPA-EDT-RATE-1-3 TO SPA-EDT-RATE(1 3).
           MOVE QSPA-EDT-RATETYPE-1-3 TO SPA-EDT-RATETYPE(1 3).
           MOVE QSPA-EDT-AMT-1-3 TO SPA-EDT-AMT(1 3).
           MOVE QSPA-EDT-AMTYPE-1-3 TO SPA-EDT-AMTYPE(1 3).
           MOVE QSPA-EDT-TERM-1-3 TO SPA-EDT-TERM(1 3).
           MOVE QSPA-EDT-TYPE-1-4 TO SPA-EDT-TYPE(1 4).
           MOVE QSPA-EDT-HL-1-4 TO SPA-EDT-HL(1 4).
           MOVE QSPA-EDT-COLL-1-4 TO SPA-EDT-COLL(1 4).
           MOVE QSPA-EDT-CD-1-4 TO SPA-EDT-CD(1 4).
           MOVE QSPA-EDT-RATE-1-4 TO SPA-EDT-RATE(1 4).
           MOVE QSPA-EDT-RATETYPE-1-4 TO SPA-EDT-RATETYPE(1 4).
           MOVE QSPA-EDT-AMT-1-4 TO SPA-EDT-AMT(1 4).
           MOVE QSPA-EDT-AMTYPE-1-4 TO SPA-EDT-AMTYPE(1 4).
           MOVE QSPA-EDT-TERM-1-4 TO SPA-EDT-TERM(1 4).
           MOVE QSPA-EDT-TYPE-1-5 TO SPA-EDT-TYPE(1 5).
           MOVE QSPA-EDT-HL-1-5 TO SPA-EDT-HL(1 5).
           MOVE QSPA-EDT-COLL-1-5 TO SPA-EDT-COLL(1 5).
           MOVE QSPA-EDT-CD-1-5 TO SPA-EDT-CD(1 5).
           MOVE QSPA-EDT-RATE-1-5 TO SPA-EDT-RATE(1 5).
           MOVE QSPA-EDT-RATETYPE-1-5 TO SPA-EDT-RATETYPE(1 5).
           MOVE QSPA-EDT-AMT-1-5 TO SPA-EDT-AMT(1 5).
           MOVE QSPA-EDT-AMTYPE-1-5 TO SPA-EDT-AMTYPE(1 5).
           MOVE QSPA-EDT-TERM-1-5 TO SPA-EDT-TERM(1 5).
           MOVE QSPA-EDT-TYPE-1-6 TO SPA-EDT-TYPE(1 6).
           MOVE QSPA-EDT-HL-1-6 TO SPA-EDT-HL(1 6).
           MOVE QSPA-EDT-COLL-1-6 TO SPA-EDT-COLL(1 6).
           MOVE QSPA-EDT-CD-1-6 TO SPA-EDT-CD(1 6).
           MOVE QSPA-EDT-RATE-1-6 TO SPA-EDT-RATE(1 6).
           MOVE QSPA-EDT-RATETYPE-1-6 TO SPA-EDT-RATETYPE(1 6).
           MOVE QSPA-EDT-AMT-1-6 TO SPA-EDT-AMT(1 6).
           MOVE QSPA-EDT-AMTYPE-1-6 TO SPA-EDT-AMTYPE(1 6).
           MOVE QSPA-EDT-TERM-1-6 TO SPA-EDT-TERM(1 6).
           MOVE QSPA-EDT-NOEDITS-2 TO SPA-EDT-NOEDITS(2).
           MOVE QSPA-EDT-TYPE-2-1 TO SPA-EDT-TYPE(2 1).
           MOVE QSPA-EDT-HL-2-1 TO SPA-EDT-HL(2 1).
           MOVE QSPA-EDT-COLL-2-1 TO SPA-EDT-COLL(2 1).
           MOVE QSPA-EDT-CD-2-1 TO SPA-EDT-CD(2 1).
           MOVE QSPA-EDT-RATE-2-1 TO SPA-EDT-RATE(2 1).
           MOVE QSPA-EDT-RATETYPE-2-1 TO SPA-EDT-RATETYPE(2 1).
           MOVE QSPA-EDT-AMT-2-1 TO SPA-EDT-AMT(2 1).
           MOVE QSPA-EDT-AMTYPE-2-1 TO SPA-EDT-AMTYPE(2 1).
           MOVE QSPA-EDT-TERM-2-1 TO SPA-EDT-TERM(2 1).
           MOVE QSPA-EDT-TYPE-2-2 TO SPA-EDT-TYPE(2 2).
           MOVE QSPA-EDT-HL-2-2 TO SPA-EDT-HL(2 2).
           MOVE QSPA-EDT-COLL-2-2 TO SPA-EDT-COLL(2 2).
           MOVE QSPA-EDT-CD-2-2 TO SPA-EDT-CD(2 2).
           MOVE QSPA-EDT-RATE-2-2 TO SPA-EDT-RATE(2 2).
           MOVE QSPA-EDT-RATETYPE-2-2 TO SPA-EDT-RATETYPE(2 2).
           MOVE QSPA-EDT-AMT-2-2 TO SPA-EDT-AMT(2 2).
           MOVE QSPA-EDT-AMTYPE-2-2 TO SPA-EDT-AMTYPE(2 2).
           MOVE QSPA-EDT-TERM-2-2 TO SPA-EDT-TERM(2 2).
           MOVE QSPA-EDT-TYPE-2-3 TO SPA-EDT-TYPE(2 3).
           MOVE QSPA-EDT-HL-2-3 TO SPA-EDT-HL(2 3).
           MOVE QSPA-EDT-COLL-2-3 TO SPA-EDT-COLL(2 3).
           MOVE QSPA-EDT-CD-2-3 TO SPA-EDT-CD(2 3).
           MOVE QSPA-EDT-RATE-2-3 TO SPA-EDT-RATE(2 3).
           MOVE QSPA-EDT-RATETYPE-2-3 TO SPA-EDT-RATETYPE(2 3).
           MOVE QSPA-EDT-AMT-2-3 TO SPA-EDT-AMT(2 3).
           MOVE QSPA-EDT-AMTYPE-2-3 TO SPA-EDT-AMTYPE(2 3).
           MOVE QSPA-EDT-TERM-2-3 TO SPA-EDT-TERM(2 3).
           MOVE QSPA-EDT-TYPE-2-4 TO SPA-EDT-TYPE(2 4).
           MOVE QSPA-EDT-HL-2-4 TO SPA-EDT-HL(2 4).
           MOVE QSPA-EDT-COLL-2-4 TO SPA-EDT-COLL(2 4).
           MOVE QSPA-EDT-CD-2-4 TO SPA-EDT-CD(2 4).
           MOVE QSPA-EDT-RATE-2-4 TO SPA-EDT-RATE(2 4).
           MOVE QSPA-EDT-RATETYPE-2-4 TO SPA-EDT-RATETYPE(2 4).
           MOVE QSPA-EDT-AMT-2-4 TO SPA-EDT-AMT(2 4).
           MOVE QSPA-EDT-AMTYPE-2-4 TO SPA-EDT-AMTYPE(2 4).
           MOVE QSPA-EDT-TERM-2-4 TO SPA-EDT-TERM(2 4).
           MOVE QSPA-EDT-TYPE-2-5 TO SPA-EDT-TYPE(2 5).
           MOVE QSPA-EDT-HL-2-5 TO SPA-EDT-HL(2 5).
           MOVE QSPA-EDT-COLL-2-5 TO SPA-EDT-COLL(2 5).
           MOVE QSPA-EDT-CD-2-5 TO SPA-EDT-CD(2 5).
           MOVE QSPA-EDT-RATE-2-5 TO SPA-EDT-RATE(2 5).
           MOVE QSPA-EDT-RATETYPE-2-5 TO SPA-EDT-RATETYPE(2 5).
           MOVE QSPA-EDT-AMT-2-5 TO SPA-EDT-AMT(2 5).
           MOVE QSPA-EDT-AMTYPE-2-5 TO SPA-EDT-AMTYPE(2 5).
           MOVE QSPA-EDT-TERM-2-5 TO SPA-EDT-TERM(2 5).
           MOVE QSPA-EDT-TYPE-2-6 TO SPA-EDT-TYPE(2 6).
           MOVE QSPA-EDT-HL-2-6 TO SPA-EDT-HL(2 6).
           MOVE QSPA-EDT-COLL-2-6 TO SPA-EDT-COLL(2 6).
           MOVE QSPA-EDT-CD-2-6 TO SPA-EDT-CD(2 6).
           MOVE QSPA-EDT-RATE-2-6 TO SPA-EDT-RATE(2 6).
           MOVE QSPA-EDT-RATETYPE-2-6 TO SPA-EDT-RATETYPE(2 6).
           MOVE QSPA-EDT-AMT-2-6 TO SPA-EDT-AMT(2 6).
           MOVE QSPA-EDT-AMTYPE-2-6 TO SPA-EDT-AMTYPE(2 6).
           MOVE QSPA-EDT-TERM-2-6 TO SPA-EDT-TERM(2 6).
 
      *-----------------------------------------------------------------
      *    LOAD QSPA-REC FROM SPA-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-SPA-FIELDS.
           MOVE SPA-ORGST TO QSPA-ORGST.
           MOVE SPA-SPRCLASS TO QSPA-SPRCLASS.
           MOVE SPA-SUBCLASS TO QSPA-SUBCLASS.
           MOVE SPA-LAWCODE TO QSPA-LAWCODE.

           IF ( SPA-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QSPA-LTOUCH-DATE
           ELSE
              MOVE SPA-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QSPA-LTOUCH-DATE.

           MOVE SPA-DESC TO QSPA-DESC.
           MOVE SPA-NOTES TO QSPA-NOTES.

           IF ( SPA-EFFDATE = ZEROES )
              MOVE "1900-01-01" TO QSPA-EFFDATE
           ELSE
              MOVE SPA-EFFDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QSPA-EFFDATE.


           IF ( SPA-EXPDATE = ZEROES )
              MOVE "1900-01-01" TO QSPA-EXPDATE
           ELSE
              MOVE SPA-EXPDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QSPA-EXPDATE.

           MOVE SPA-EXPD-MONTHS TO QSPA-EXPD-MONTHS.
           MOVE SPA-EXPD-DAYS TO QSPA-EXPD-DAYS.
           MOVE SPA-EXPD-FRMLA TO QSPA-EXPD-FRMLA.
           MOVE SPA-EXPD-EXCEPT TO QSPA-EXPD-EXCEPT.
           MOVE SPA-EXPR-FRMLA TO QSPA-EXPR-FRMLA.
           MOVE SPA-EXPR-REDU-FRMLA TO QSPA-EXPR-REDU-FRMLA.
           MOVE SPA-EXPR-RATE(1) TO QSPA-EXPR-RATE-1.
           MOVE SPA-EXPR-BREAK(1) TO QSPA-EXPR-BREAK-1.
           MOVE SPA-EXPR-YRTYPE(1) TO QSPA-EXPR-YRTYPE-1.
           MOVE SPA-EXPR-INTMETH(1) TO QSPA-EXPR-INTMETH-1.
           MOVE SPA-EXPR-RATE(2) TO QSPA-EXPR-RATE-2.
           MOVE SPA-EXPR-BREAK(2) TO QSPA-EXPR-BREAK-2.
           MOVE SPA-EXPR-YRTYPE(2) TO QSPA-EXPR-YRTYPE-2.
           MOVE SPA-EXPR-INTMETH(2) TO QSPA-EXPR-INTMETH-2.
           MOVE SPA-EXPR-RATE(3) TO QSPA-EXPR-RATE-3.
           MOVE SPA-EXPR-BREAK(3) TO QSPA-EXPR-BREAK-3.
           MOVE SPA-EXPR-YRTYPE(3) TO QSPA-EXPR-YRTYPE-3.
           MOVE SPA-EXPR-INTMETH(3) TO QSPA-EXPR-INTMETH-3.
           MOVE SPA-EXPR-RATE(4) TO QSPA-EXPR-RATE-4.
           MOVE SPA-EXPR-BREAK(4) TO QSPA-EXPR-BREAK-4.
           MOVE SPA-EXPR-YRTYPE(4) TO QSPA-EXPR-YRTYPE-4.
           MOVE SPA-EXPR-INTMETH(4) TO QSPA-EXPR-INTMETH-4.
           MOVE SPA-EXPR-RATE(5) TO QSPA-EXPR-RATE-5.
           MOVE SPA-EXPR-BREAK(5) TO QSPA-EXPR-BREAK-5.
           MOVE SPA-EXPR-YRTYPE(5) TO QSPA-EXPR-YRTYPE-5.
           MOVE SPA-EXPR-INTMETH(5) TO QSPA-EXPR-INTMETH-5.
           MOVE SPA-EXPR-LCHG TO QSPA-EXPR-LCHG.
           MOVE SPA-JUDGFRMLA TO QSPA-JUDGFRMLA.
           MOVE SPA-JUDGRATE TO QSPA-JUDGRATE.
           MOVE SPA-JUDGYRTYPE TO QSPA-JUDGYRTYPE.
           MOVE SPA-JUDGINTMETH TO QSPA-JUDGINTMETH.
           MOVE SPA-ACC-FRMLA TO QSPA-ACC-FRMLA.
           MOVE SPA-ACC-YRTYPE TO QSPA-ACC-YRTYPE.
           MOVE SPA-ACC-INTMETH TO QSPA-ACC-INTMETH.
           MOVE SPA-ACC-RATE(1) TO QSPA-ACC-RATE-1.
           MOVE SPA-ACC-BREAK(1) TO QSPA-ACC-BREAK-1.
           MOVE SPA-ACC-RATE(2) TO QSPA-ACC-RATE-2.
           MOVE SPA-ACC-BREAK(2) TO QSPA-ACC-BREAK-2.
           MOVE SPA-ACC-RATE(3) TO QSPA-ACC-RATE-3.
           MOVE SPA-ACC-BREAK(3) TO QSPA-ACC-BREAK-3.
           MOVE SPA-ACC-RATE(4) TO QSPA-ACC-RATE-4.
           MOVE SPA-ACC-BREAK(4) TO QSPA-ACC-BREAK-4.
           MOVE SPA-ACC-RATE(5) TO QSPA-ACC-RATE-5.
           MOVE SPA-ACC-BREAK(5) TO QSPA-ACC-BREAK-5.
           MOVE SPA-1098FRMLA TO QSPA-1098FRMLA.
           MOVE SPA-DISFRMLA TO QSPA-DISFRMLA.
           MOVE SPA-CONTRFRMLA TO QSPA-CONTRFRMLA.
           MOVE SPA-BANKRULE TO QSPA-BANKRULE.
           MOVE SPA-CONTPDTH TO QSPA-CONTPDTH.
           MOVE SPA-FINCHG TO QSPA-FINCHG.
           MOVE SPA-BANKUNITPER-CD TO QSPA-BANKUNITPER-CD.
           MOVE SPA-TRWBANKRULE TO QSPA-TRWBANKRULE.
           MOVE SPA-TRWCONTPDTH TO QSPA-TRWCONTPDTH.
           MOVE SPA-TRWBANKUNITPER-CD TO QSPA-TRWBANKUNITPER-CD.
           MOVE SPA-RECFRMLA TO QSPA-RECFRMLA.
           MOVE SPA-RECPAYCD TO QSPA-RECPAYCD.
           MOVE SPA-RECPDTH TO QSPA-RECPDTH.
           MOVE SPA-DISFRMLA-SPAY-FG TO QSPA-DISFRMLA-SPAY-FG.
           MOVE SPA-ERNFRMLA(1) TO QSPA-ERNFRMLA-1.
           MOVE SPA-ERNAMT(1) TO QSPA-ERNAMT-1.
           MOVE SPA-ERNPERCT(1) TO QSPA-ERNPERCT-1.
           MOVE SPA-ERNPERCD(1) TO QSPA-ERNPERCD-1.
           MOVE SPA-ERNACCRL-FG(1) TO QSPA-ERNACCRL-FG-1.
           MOVE SPA-ERNACCRL-START-CD(1) TO QSPA-ERNACCRL-START-CD-1.
           MOVE SPA-ERNACCRL-DFCD(1) TO QSPA-ERNACCRL-DFCD-1.
           MOVE SPA-ERN-DAYS(1) TO QSPA-ERN-DAYS-1.
           MOVE SPA-ERN-TERM(1) TO QSPA-ERN-TERM-1.
           MOVE SPA-ERN-TYPE(1) TO QSPA-ERN-TYPE-1.
           MOVE SPA-ERNFRMLA(2) TO QSPA-ERNFRMLA-2.
           MOVE SPA-ERNAMT(2) TO QSPA-ERNAMT-2.
           MOVE SPA-ERNPERCT(2) TO QSPA-ERNPERCT-2.
           MOVE SPA-ERNPERCD(2) TO QSPA-ERNPERCD-2.
           MOVE SPA-ERNACCRL-FG(2) TO QSPA-ERNACCRL-FG-2.
           MOVE SPA-ERNACCRL-START-CD(2) TO QSPA-ERNACCRL-START-CD-2.
           MOVE SPA-ERNACCRL-DFCD(2) TO QSPA-ERNACCRL-DFCD-2.
           MOVE SPA-ERN-DAYS(2) TO QSPA-ERN-DAYS-2.
           MOVE SPA-ERN-TERM(2) TO QSPA-ERN-TERM-2.
           MOVE SPA-ERN-TYPE(2) TO QSPA-ERN-TYPE-2.
           MOVE SPA-ERNFRMLA(3) TO QSPA-ERNFRMLA-3.
           MOVE SPA-ERNAMT(3) TO QSPA-ERNAMT-3.
           MOVE SPA-ERNPERCT(3) TO QSPA-ERNPERCT-3.
           MOVE SPA-ERNPERCD(3) TO QSPA-ERNPERCD-3.
           MOVE SPA-ERNACCRL-FG(3) TO QSPA-ERNACCRL-FG-3.
           MOVE SPA-ERNACCRL-START-CD(3) TO QSPA-ERNACCRL-START-CD-3.
           MOVE SPA-ERNACCRL-DFCD(3) TO QSPA-ERNACCRL-DFCD-3.
           MOVE SPA-ERN-DAYS(3) TO QSPA-ERN-DAYS-3.
           MOVE SPA-ERN-TERM(3) TO QSPA-ERN-TERM-3.
           MOVE SPA-ERN-TYPE(3) TO QSPA-ERN-TYPE-3.
           MOVE SPA-ERNFRMLA(4) TO QSPA-ERNFRMLA-4.
           MOVE SPA-ERNAMT(4) TO QSPA-ERNAMT-4.
           MOVE SPA-ERNPERCT(4) TO QSPA-ERNPERCT-4.
           MOVE SPA-ERNPERCD(4) TO QSPA-ERNPERCD-4.
           MOVE SPA-ERNACCRL-FG(4) TO QSPA-ERNACCRL-FG-4.
           MOVE SPA-ERNACCRL-START-CD(4) TO QSPA-ERNACCRL-START-CD-4.
           MOVE SPA-ERNACCRL-DFCD(4) TO QSPA-ERNACCRL-DFCD-4.
           MOVE SPA-ERN-DAYS(4) TO QSPA-ERN-DAYS-4.
           MOVE SPA-ERN-TERM(4) TO QSPA-ERN-TERM-4.
           MOVE SPA-ERN-TYPE(4) TO QSPA-ERN-TYPE-4.
           MOVE SPA-ERNFRMLA(5) TO QSPA-ERNFRMLA-5.
           MOVE SPA-ERNAMT(5) TO QSPA-ERNAMT-5.
           MOVE SPA-ERNPERCT(5) TO QSPA-ERNPERCT-5.
           MOVE SPA-ERNPERCD(5) TO QSPA-ERNPERCD-5.
           MOVE SPA-ERNACCRL-FG(5) TO QSPA-ERNACCRL-FG-5.
           MOVE SPA-ERNACCRL-START-CD(5) TO QSPA-ERNACCRL-START-CD-5.
           MOVE SPA-ERNACCRL-DFCD(5) TO QSPA-ERNACCRL-DFCD-5.
           MOVE SPA-ERN-DAYS(5) TO QSPA-ERN-DAYS-5.
           MOVE SPA-ERN-TERM(5) TO QSPA-ERN-TERM-5.
           MOVE SPA-ERN-TYPE(5) TO QSPA-ERN-TYPE-5.
           MOVE SPA-ERNFRMLA(6) TO QSPA-ERNFRMLA-6.
           MOVE SPA-ERNAMT(6) TO QSPA-ERNAMT-6.
           MOVE SPA-ERNPERCT(6) TO QSPA-ERNPERCT-6.
           MOVE SPA-ERNPERCD(6) TO QSPA-ERNPERCD-6.
           MOVE SPA-ERNACCRL-FG(6) TO QSPA-ERNACCRL-FG-6.
           MOVE SPA-ERNACCRL-START-CD(6) TO QSPA-ERNACCRL-START-CD-6.
           MOVE SPA-ERNACCRL-DFCD(6) TO QSPA-ERNACCRL-DFCD-6.
           MOVE SPA-ERN-DAYS(6) TO QSPA-ERN-DAYS-6.
           MOVE SPA-ERN-TERM(6) TO QSPA-ERN-TERM-6.
           MOVE SPA-ERN-TYPE(6) TO QSPA-ERN-TYPE-6.
           MOVE SPA-ERNFRMLA(7) TO QSPA-ERNFRMLA-7.
           MOVE SPA-ERNAMT(7) TO QSPA-ERNAMT-7.
           MOVE SPA-ERNPERCT(7) TO QSPA-ERNPERCT-7.
           MOVE SPA-ERNPERCD(7) TO QSPA-ERNPERCD-7.
           MOVE SPA-ERNACCRL-FG(7) TO QSPA-ERNACCRL-FG-7.
           MOVE SPA-ERNACCRL-START-CD(7) TO QSPA-ERNACCRL-START-CD-7.
           MOVE SPA-ERNACCRL-DFCD(7) TO QSPA-ERNACCRL-DFCD-7.
           MOVE SPA-ERN-DAYS(7) TO QSPA-ERN-DAYS-7.
           MOVE SPA-ERN-TERM(7) TO QSPA-ERN-TERM-7.
           MOVE SPA-ERN-TYPE(7) TO QSPA-ERN-TYPE-7.
           MOVE SPA-ERNFRMLA(8) TO QSPA-ERNFRMLA-8.
           MOVE SPA-ERNAMT(8) TO QSPA-ERNAMT-8.
           MOVE SPA-ERNPERCT(8) TO QSPA-ERNPERCT-8.
           MOVE SPA-ERNPERCD(8) TO QSPA-ERNPERCD-8.
           MOVE SPA-ERNACCRL-FG(8) TO QSPA-ERNACCRL-FG-8.
           MOVE SPA-ERNACCRL-START-CD(8) TO QSPA-ERNACCRL-START-CD-8.
           MOVE SPA-ERNACCRL-DFCD(8) TO QSPA-ERNACCRL-DFCD-8.
           MOVE SPA-ERN-DAYS(8) TO QSPA-ERN-DAYS-8.
           MOVE SPA-ERN-TERM(8) TO QSPA-ERN-TERM-8.
           MOVE SPA-ERN-TYPE(8) TO QSPA-ERN-TYPE-8.
           MOVE SPA-ERNFRMLA(9) TO QSPA-ERNFRMLA-9.
           MOVE SPA-ERNAMT(9) TO QSPA-ERNAMT-9.
           MOVE SPA-ERNPERCT(9) TO QSPA-ERNPERCT-9.
           MOVE SPA-ERNPERCD(9) TO QSPA-ERNPERCD-9.
           MOVE SPA-ERNACCRL-FG(9) TO QSPA-ERNACCRL-FG-9.
           MOVE SPA-ERNACCRL-START-CD(9) TO QSPA-ERNACCRL-START-CD-9.
           MOVE SPA-ERNACCRL-DFCD(9) TO QSPA-ERNACCRL-DFCD-9.
           MOVE SPA-ERN-DAYS(9) TO QSPA-ERN-DAYS-9.
           MOVE SPA-ERN-TERM(9) TO QSPA-ERN-TERM-9.
           MOVE SPA-ERN-TYPE(9) TO QSPA-ERN-TYPE-9.
           MOVE SPA-ERNFRMLA(10) TO QSPA-ERNFRMLA-10.
           MOVE SPA-ERNAMT(10) TO QSPA-ERNAMT-10.
           MOVE SPA-ERNPERCT(10) TO QSPA-ERNPERCT-10.
           MOVE SPA-ERNPERCD(10) TO QSPA-ERNPERCD-10.
           MOVE SPA-ERNACCRL-FG(10) TO QSPA-ERNACCRL-FG-10.
           MOVE SPA-ERNACCRL-START-CD(10) TO QSPA-ERNACCRL-START-CD-10.
           MOVE SPA-ERNACCRL-DFCD(10) TO QSPA-ERNACCRL-DFCD-10.
           MOVE SPA-ERN-DAYS(10) TO QSPA-ERN-DAYS-10.
           MOVE SPA-ERN-TERM(10) TO QSPA-ERN-TERM-10.
           MOVE SPA-ERN-TYPE(10) TO QSPA-ERN-TYPE-10.
           MOVE SPA-ERNACCEL-CD TO QSPA-ERNACCEL-CD.
           MOVE SPA-ERNJD-CD TO QSPA-ERNJD-CD.
           MOVE SPA-MIDSTRERNCD TO QSPA-MIDSTRERNCD.
           MOVE SPA-EARN-ORDER(1) TO QSPA-EARN-ORDER-1.
           MOVE SPA-EARN-ORDER(2) TO QSPA-EARN-ORDER-2.
           MOVE SPA-EARN-ORDER(3) TO QSPA-EARN-ORDER-3.
           MOVE SPA-EARN-ORDER(4) TO QSPA-EARN-ORDER-4.
           MOVE SPA-EARN-ORDER(5) TO QSPA-EARN-ORDER-5.
           MOVE SPA-EARN-ORDER(6) TO QSPA-EARN-ORDER-6.
           MOVE SPA-EARN-ORDER(7) TO QSPA-EARN-ORDER-7.
           MOVE SPA-EARN-ORDER(8) TO QSPA-EARN-ORDER-8.
           MOVE SPA-EARN-ORDER(9) TO QSPA-EARN-ORDER-9.
           MOVE SPA-EARN-ORDER(10) TO QSPA-EARN-ORDER-10.
           MOVE SPA-ERNDELQFRMLA TO QSPA-ERNDELQFRMLA.
           MOVE SPA-ERNDELQ-START TO QSPA-ERNDELQ-START.
           MOVE SPA-ERNDELQ-STOP TO QSPA-ERNDELQ-STOP.
           MOVE SPA-1098-INC-LC TO QSPA-1098-INC-LC.
           MOVE SPA-MAX-IB-ACR-DAYS TO QSPA-MAX-IB-ACR-DAYS.
           MOVE SPA-LCGRACE TO QSPA-LCGRACE.
           MOVE SPA-LCGRACECD TO QSPA-LCGRACECD.
           MOVE SPA-LCTYPE TO QSPA-LCTYPE.
           MOVE SPA-LCRATE TO QSPA-LCRATE.
           MOVE SPA-LCFRMLA TO QSPA-LCFRMLA.
           MOVE SPA-LCMINCHG TO QSPA-LCMINCHG.
           MOVE SPA-LCMAXAMT TO QSPA-LCMAXAMT.
           MOVE SPA-LCDELFAC TO QSPA-LCDELFAC.
           MOVE SPA-LCYRTYPE TO QSPA-LCYRTYPE.
           MOVE SPA-LC-CODE TO QSPA-LC-CODE.
           MOVE SPA-LCUNITPER-CD TO QSPA-LCUNITPER-CD.
           MOVE SPA-DEFFRMLA TO QSPA-DEFFRMLA.
           MOVE SPA-DEFEXCEPT TO QSPA-DEFEXCEPT.
           MOVE SPA-DEFWORKER TO QSPA-DEFWORKER.
           MOVE SPA-DEFPERCD TO QSPA-DEFPERCD.
           MOVE SPA-DEFBASE TO QSPA-DEFBASE.
           MOVE SPA-DEFPOLCD TO QSPA-DEFPOLCD.
           MOVE SPA-DEFPOLMX(1) TO QSPA-DEFPOLMX-1.
           MOVE SPA-DEFPOLMX(2) TO QSPA-DEFPOLMX-2.
           MOVE SPA-DEFPOLMX(3) TO QSPA-DEFPOLMX-3.
           MOVE SPA-DEFMIN TO QSPA-DEFMIN.
           MOVE SPA-DEFMAX TO QSPA-DEFMAX.
           MOVE SPA-DEFUNITPER-CD TO QSPA-DEFUNITPER-CD.
           MOVE SPA-PRFORM(1) TO QSPA-PRFORM-1.
           MOVE SPA-PRFLAG(1) TO QSPA-PRFLAG-1.
           MOVE SPA-PRFORM(2) TO QSPA-PRFORM-2.
           MOVE SPA-PRFLAG(2) TO QSPA-PRFLAG-2.
           MOVE SPA-PRFORM(3) TO QSPA-PRFORM-3.
           MOVE SPA-PRFLAG(3) TO QSPA-PRFLAG-3.
           MOVE SPA-PRFORM(4) TO QSPA-PRFORM-4.
           MOVE SPA-PRFLAG(4) TO QSPA-PRFLAG-4.
           MOVE SPA-PRFORM(5) TO QSPA-PRFORM-5.
           MOVE SPA-PRFLAG(5) TO QSPA-PRFLAG-5.
           MOVE SPA-PRFORM(6) TO QSPA-PRFORM-6.
           MOVE SPA-PRFLAG(6) TO QSPA-PRFLAG-6.
           MOVE SPA-PRFORM(7) TO QSPA-PRFORM-7.
           MOVE SPA-PRFLAG(7) TO QSPA-PRFLAG-7.
           MOVE SPA-PRFORM(8) TO QSPA-PRFORM-8.
           MOVE SPA-PRFLAG(8) TO QSPA-PRFLAG-8.
           MOVE SPA-PRFORM(9) TO QSPA-PRFORM-9.
           MOVE SPA-PRFLAG(9) TO QSPA-PRFLAG-9.
           MOVE SPA-PRFORM(10) TO QSPA-PRFORM-10.
           MOVE SPA-PRFLAG(10) TO QSPA-PRFLAG-10.
           MOVE SPA-PRFORM(11) TO QSPA-PRFORM-11.
           MOVE SPA-PRFLAG(11) TO QSPA-PRFLAG-11.
           MOVE SPA-PRFORM(12) TO QSPA-PRFORM-12.
           MOVE SPA-PRFLAG(12) TO QSPA-PRFLAG-12.
           MOVE SPA-PRFORM(13) TO QSPA-PRFORM-13.
           MOVE SPA-PRFLAG(13) TO QSPA-PRFLAG-13.
           MOVE SPA-PRFORM(14) TO QSPA-PRFORM-14.
           MOVE SPA-PRFLAG(14) TO QSPA-PRFLAG-14.
           MOVE SPA-PRFORM(15) TO QSPA-PRFORM-15.
           MOVE SPA-PRFLAG(15) TO QSPA-PRFLAG-15.
           MOVE SPA-PRFORM(16) TO QSPA-PRFORM-16.
           MOVE SPA-PRFLAG(16) TO QSPA-PRFLAG-16.
           MOVE SPA-PRFORM(17) TO QSPA-PRFORM-17.
           MOVE SPA-PRFLAG(17) TO QSPA-PRFLAG-17.
           MOVE SPA-PRFORM(18) TO QSPA-PRFORM-18.
           MOVE SPA-PRFLAG(18) TO QSPA-PRFLAG-18.
           MOVE SPA-PRFORM(19) TO QSPA-PRFORM-19.
           MOVE SPA-PRFLAG(19) TO QSPA-PRFLAG-19.
           MOVE SPA-PRFORM(20) TO QSPA-PRFORM-20.
           MOVE SPA-PRFLAG(20) TO QSPA-PRFLAG-20.
           MOVE SPA-IMED-REC TO QSPA-IMED-REC.
           MOVE SPA-SCFRMLA TO QSPA-SCFRMLA.
           MOVE SPA-SCRATE TO QSPA-SCRATE.
           MOVE SPA-SCMIN TO QSPA-SCMIN.
           MOVE SPA-SCMAX TO QSPA-SCMAX.
           MOVE SPA-MFFRMLA TO QSPA-MFFRMLA.
           MOVE SPA-MFRATE TO QSPA-MFRATE.
           MOVE SPA-MFMIN TO QSPA-MFMIN.
           MOVE SPA-MFMAX TO QSPA-MFMAX.
           MOVE SPA-INTMETHOD TO QSPA-INTMETHOD.
           MOVE SPA-YEARTYPE TO QSPA-YEARTYPE.
           MOVE SPA-PYEXFRMLA TO QSPA-PYEXFRMLA.
           MOVE SPA-PYEXSPCODE TO QSPA-PYEXSPCODE.
           MOVE SPA-PYEXYEARTP TO QSPA-PYEXYEARTP.
           MOVE SPA-PYEXFMDATE TO QSPA-PYEXFMDATE.
           MOVE SPA-PYEXBASECD TO QSPA-PYEXBASECD.
           MOVE SPA-PYEXBASE TO QSPA-PYEXBASE.
           MOVE SPA-PYEXDAYS TO QSPA-PYEXDAYS.
           MOVE SPA-CAL-SPFRMLA TO QSPA-CAL-SPFRMLA.
           MOVE SPA-CAL-RATETYPE(1) TO QSPA-CAL-RATETYPE-1.
           MOVE SPA-CAL-BREAKTYPE(1) TO QSPA-CAL-BREAKTYPE-1.
           MOVE SPA-CAL-CHGABLE(1) TO QSPA-CAL-CHGABLE-1.
           MOVE SPA-CAL-NORATES(1) TO QSPA-CAL-NORATES-1.
           MOVE SPA-CAL-TBFRMLA(1) TO QSPA-CAL-TBFRMLA-1.
           MOVE SPA-CAL-RATE(1 1) TO QSPA-CAL-RATE-1-1.
           MOVE SPA-CAL-BREAK(1 1) TO QSPA-CAL-BREAK-1-1.
           MOVE SPA-CAL-RATE(1 2) TO QSPA-CAL-RATE-1-2.
           MOVE SPA-CAL-BREAK(1 2) TO QSPA-CAL-BREAK-1-2.
           MOVE SPA-CAL-RATE(1 3) TO QSPA-CAL-RATE-1-3.
           MOVE SPA-CAL-BREAK(1 3) TO QSPA-CAL-BREAK-1-3.
           MOVE SPA-CAL-RATE(1 4) TO QSPA-CAL-RATE-1-4.
           MOVE SPA-CAL-BREAK(1 4) TO QSPA-CAL-BREAK-1-4.
           MOVE SPA-CAL-RATE(1 5) TO QSPA-CAL-RATE-1-5.
           MOVE SPA-CAL-BREAK(1 5) TO QSPA-CAL-BREAK-1-5.
           MOVE SPA-CAL-RATETYPE(2) TO QSPA-CAL-RATETYPE-2.
           MOVE SPA-CAL-BREAKTYPE(2) TO QSPA-CAL-BREAKTYPE-2.
           MOVE SPA-CAL-CHGABLE(2) TO QSPA-CAL-CHGABLE-2.
           MOVE SPA-CAL-NORATES(2) TO QSPA-CAL-NORATES-2.
           MOVE SPA-CAL-TBFRMLA(2) TO QSPA-CAL-TBFRMLA-2.
           MOVE SPA-CAL-RATE(2 1) TO QSPA-CAL-RATE-2-1.
           MOVE SPA-CAL-BREAK(2 1) TO QSPA-CAL-BREAK-2-1.
           MOVE SPA-CAL-RATE(2 2) TO QSPA-CAL-RATE-2-2.
           MOVE SPA-CAL-BREAK(2 2) TO QSPA-CAL-BREAK-2-2.
           MOVE SPA-CAL-RATE(2 3) TO QSPA-CAL-RATE-2-3.
           MOVE SPA-CAL-BREAK(2 3) TO QSPA-CAL-BREAK-2-3.
           MOVE SPA-CAL-RATE(2 4) TO QSPA-CAL-RATE-2-4.
           MOVE SPA-CAL-BREAK(2 4) TO QSPA-CAL-BREAK-2-4.
           MOVE SPA-CAL-RATE(2 5) TO QSPA-CAL-RATE-2-5.
           MOVE SPA-CAL-BREAK(2 5) TO QSPA-CAL-BREAK-2-5.
           MOVE SPA-MINSF-FRMLA TO QSPA-MINSF-FRMLA.
           MOVE SPA-MINSF-CHG TO QSPA-MINSF-CHG.
           MOVE SPA-MAXAPR-FRMLA TO QSPA-MAXAPR-FRMLA.
           MOVE SPA-EDT-NOEDITS(1) TO QSPA-EDT-NOEDITS-1.
           MOVE SPA-EDT-TYPE(1 1) TO QSPA-EDT-TYPE-1-1.
           MOVE SPA-EDT-HL(1 1) TO QSPA-EDT-HL-1-1.
           MOVE SPA-EDT-COLL(1 1) TO QSPA-EDT-COLL-1-1.
           MOVE SPA-EDT-CD(1 1) TO QSPA-EDT-CD-1-1.
           MOVE SPA-EDT-RATE(1 1) TO QSPA-EDT-RATE-1-1.
           MOVE SPA-EDT-RATETYPE(1 1) TO QSPA-EDT-RATETYPE-1-1.
           MOVE SPA-EDT-AMT(1 1) TO QSPA-EDT-AMT-1-1.
           MOVE SPA-EDT-AMTYPE(1 1) TO QSPA-EDT-AMTYPE-1-1.
           MOVE SPA-EDT-TERM(1 1) TO QSPA-EDT-TERM-1-1.
           MOVE SPA-EDT-TYPE(1 2) TO QSPA-EDT-TYPE-1-2.
           MOVE SPA-EDT-HL(1 2) TO QSPA-EDT-HL-1-2.
           MOVE SPA-EDT-COLL(1 2) TO QSPA-EDT-COLL-1-2.
           MOVE SPA-EDT-CD(1 2) TO QSPA-EDT-CD-1-2.
           MOVE SPA-EDT-RATE(1 2) TO QSPA-EDT-RATE-1-2.
           MOVE SPA-EDT-RATETYPE(1 2) TO QSPA-EDT-RATETYPE-1-2.
           MOVE SPA-EDT-AMT(1 2) TO QSPA-EDT-AMT-1-2.
           MOVE SPA-EDT-AMTYPE(1 2) TO QSPA-EDT-AMTYPE-1-2.
           MOVE SPA-EDT-TERM(1 2) TO QSPA-EDT-TERM-1-2.
           MOVE SPA-EDT-TYPE(1 3) TO QSPA-EDT-TYPE-1-3.
           MOVE SPA-EDT-HL(1 3) TO QSPA-EDT-HL-1-3.
           MOVE SPA-EDT-COLL(1 3) TO QSPA-EDT-COLL-1-3.
           MOVE SPA-EDT-CD(1 3) TO QSPA-EDT-CD-1-3.
           MOVE SPA-EDT-RATE(1 3) TO QSPA-EDT-RATE-1-3.
           MOVE SPA-EDT-RATETYPE(1 3) TO QSPA-EDT-RATETYPE-1-3.
           MOVE SPA-EDT-AMT(1 3) TO QSPA-EDT-AMT-1-3.
           MOVE SPA-EDT-AMTYPE(1 3) TO QSPA-EDT-AMTYPE-1-3.
           MOVE SPA-EDT-TERM(1 3) TO QSPA-EDT-TERM-1-3.
           MOVE SPA-EDT-TYPE(1 4) TO QSPA-EDT-TYPE-1-4.
           MOVE SPA-EDT-HL(1 4) TO QSPA-EDT-HL-1-4.
           MOVE SPA-EDT-COLL(1 4) TO QSPA-EDT-COLL-1-4.
           MOVE SPA-EDT-CD(1 4) TO QSPA-EDT-CD-1-4.
           MOVE SPA-EDT-RATE(1 4) TO QSPA-EDT-RATE-1-4.
           MOVE SPA-EDT-RATETYPE(1 4) TO QSPA-EDT-RATETYPE-1-4.
           MOVE SPA-EDT-AMT(1 4) TO QSPA-EDT-AMT-1-4.
           MOVE SPA-EDT-AMTYPE(1 4) TO QSPA-EDT-AMTYPE-1-4.
           MOVE SPA-EDT-TERM(1 4) TO QSPA-EDT-TERM-1-4.
           MOVE SPA-EDT-TYPE(1 5) TO QSPA-EDT-TYPE-1-5.
           MOVE SPA-EDT-HL(1 5) TO QSPA-EDT-HL-1-5.
           MOVE SPA-EDT-COLL(1 5) TO QSPA-EDT-COLL-1-5.
           MOVE SPA-EDT-CD(1 5) TO QSPA-EDT-CD-1-5.
           MOVE SPA-EDT-RATE(1 5) TO QSPA-EDT-RATE-1-5.
           MOVE SPA-EDT-RATETYPE(1 5) TO QSPA-EDT-RATETYPE-1-5.
           MOVE SPA-EDT-AMT(1 5) TO QSPA-EDT-AMT-1-5.
           MOVE SPA-EDT-AMTYPE(1 5) TO QSPA-EDT-AMTYPE-1-5.
           MOVE SPA-EDT-TERM(1 5) TO QSPA-EDT-TERM-1-5.
           MOVE SPA-EDT-TYPE(1 6) TO QSPA-EDT-TYPE-1-6.
           MOVE SPA-EDT-HL(1 6) TO QSPA-EDT-HL-1-6.
           MOVE SPA-EDT-COLL(1 6) TO QSPA-EDT-COLL-1-6.
           MOVE SPA-EDT-CD(1 6) TO QSPA-EDT-CD-1-6.
           MOVE SPA-EDT-RATE(1 6) TO QSPA-EDT-RATE-1-6.
           MOVE SPA-EDT-RATETYPE(1 6) TO QSPA-EDT-RATETYPE-1-6.
           MOVE SPA-EDT-AMT(1 6) TO QSPA-EDT-AMT-1-6.
           MOVE SPA-EDT-AMTYPE(1 6) TO QSPA-EDT-AMTYPE-1-6.
           MOVE SPA-EDT-TERM(1 6) TO QSPA-EDT-TERM-1-6.
           MOVE SPA-EDT-NOEDITS(2) TO QSPA-EDT-NOEDITS-2.
           MOVE SPA-EDT-TYPE(2 1) TO QSPA-EDT-TYPE-2-1.
           MOVE SPA-EDT-HL(2 1) TO QSPA-EDT-HL-2-1.
           MOVE SPA-EDT-COLL(2 1) TO QSPA-EDT-COLL-2-1.
           MOVE SPA-EDT-CD(2 1) TO QSPA-EDT-CD-2-1.
           MOVE SPA-EDT-RATE(2 1) TO QSPA-EDT-RATE-2-1.
           MOVE SPA-EDT-RATETYPE(2 1) TO QSPA-EDT-RATETYPE-2-1.
           MOVE SPA-EDT-AMT(2 1) TO QSPA-EDT-AMT-2-1.
           MOVE SPA-EDT-AMTYPE(2 1) TO QSPA-EDT-AMTYPE-2-1.
           MOVE SPA-EDT-TERM(2 1) TO QSPA-EDT-TERM-2-1.
           MOVE SPA-EDT-TYPE(2 2) TO QSPA-EDT-TYPE-2-2.
           MOVE SPA-EDT-HL(2 2) TO QSPA-EDT-HL-2-2.
           MOVE SPA-EDT-COLL(2 2) TO QSPA-EDT-COLL-2-2.
           MOVE SPA-EDT-CD(2 2) TO QSPA-EDT-CD-2-2.
           MOVE SPA-EDT-RATE(2 2) TO QSPA-EDT-RATE-2-2.
           MOVE SPA-EDT-RATETYPE(2 2) TO QSPA-EDT-RATETYPE-2-2.
           MOVE SPA-EDT-AMT(2 2) TO QSPA-EDT-AMT-2-2.
           MOVE SPA-EDT-AMTYPE(2 2) TO QSPA-EDT-AMTYPE-2-2.
           MOVE SPA-EDT-TERM(2 2) TO QSPA-EDT-TERM-2-2.
           MOVE SPA-EDT-TYPE(2 3) TO QSPA-EDT-TYPE-2-3.
           MOVE SPA-EDT-HL(2 3) TO QSPA-EDT-HL-2-3.
           MOVE SPA-EDT-COLL(2 3) TO QSPA-EDT-COLL-2-3.
           MOVE SPA-EDT-CD(2 3) TO QSPA-EDT-CD-2-3.
           MOVE SPA-EDT-RATE(2 3) TO QSPA-EDT-RATE-2-3.
           MOVE SPA-EDT-RATETYPE(2 3) TO QSPA-EDT-RATETYPE-2-3.
           MOVE SPA-EDT-AMT(2 3) TO QSPA-EDT-AMT-2-3.
           MOVE SPA-EDT-AMTYPE(2 3) TO QSPA-EDT-AMTYPE-2-3.
           MOVE SPA-EDT-TERM(2 3) TO QSPA-EDT-TERM-2-3.
           MOVE SPA-EDT-TYPE(2 4) TO QSPA-EDT-TYPE-2-4.
           MOVE SPA-EDT-HL(2 4) TO QSPA-EDT-HL-2-4.
           MOVE SPA-EDT-COLL(2 4) TO QSPA-EDT-COLL-2-4.
           MOVE SPA-EDT-CD(2 4) TO QSPA-EDT-CD-2-4.
           MOVE SPA-EDT-RATE(2 4) TO QSPA-EDT-RATE-2-4.
           MOVE SPA-EDT-RATETYPE(2 4) TO QSPA-EDT-RATETYPE-2-4.
           MOVE SPA-EDT-AMT(2 4) TO QSPA-EDT-AMT-2-4.
           MOVE SPA-EDT-AMTYPE(2 4) TO QSPA-EDT-AMTYPE-2-4.
           MOVE SPA-EDT-TERM(2 4) TO QSPA-EDT-TERM-2-4.
           MOVE SPA-EDT-TYPE(2 5) TO QSPA-EDT-TYPE-2-5.
           MOVE SPA-EDT-HL(2 5) TO QSPA-EDT-HL-2-5.
           MOVE SPA-EDT-COLL(2 5) TO QSPA-EDT-COLL-2-5.
           MOVE SPA-EDT-CD(2 5) TO QSPA-EDT-CD-2-5.
           MOVE SPA-EDT-RATE(2 5) TO QSPA-EDT-RATE-2-5.
           MOVE SPA-EDT-RATETYPE(2 5) TO QSPA-EDT-RATETYPE-2-5.
           MOVE SPA-EDT-AMT(2 5) TO QSPA-EDT-AMT-2-5.
           MOVE SPA-EDT-AMTYPE(2 5) TO QSPA-EDT-AMTYPE-2-5.
           MOVE SPA-EDT-TERM(2 5) TO QSPA-EDT-TERM-2-5.
           MOVE SPA-EDT-TYPE(2 6) TO QSPA-EDT-TYPE-2-6.
           MOVE SPA-EDT-HL(2 6) TO QSPA-EDT-HL-2-6.
           MOVE SPA-EDT-COLL(2 6) TO QSPA-EDT-COLL-2-6.
           MOVE SPA-EDT-CD(2 6) TO QSPA-EDT-CD-2-6.
           MOVE SPA-EDT-RATE(2 6) TO QSPA-EDT-RATE-2-6.
           MOVE SPA-EDT-RATETYPE(2 6) TO QSPA-EDT-RATETYPE-2-6.
           MOVE SPA-EDT-AMT(2 6) TO QSPA-EDT-AMT-2-6.
           MOVE SPA-EDT-AMTYPE(2 6) TO QSPA-EDT-AMTYPE-2-6.
           MOVE SPA-EDT-TERM(2 6) TO QSPA-EDT-TERM-2-6.

      *-----------------------------------------------------------------
      *    LOAD SPB-REC FROM QSPB-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-SPB-FIELDS.
           MOVE QSPB-ORGST TO SPB-ORGST.
           MOVE QSPB-SPRCLASS TO SPB-SPRCLASS.
           MOVE QSPB-SUBCLASS TO SPB-SUBCLASS.
           MOVE QSPB-LAWCODE TO SPB-LAWCODE.
           MOVE QSPB-INSCOM-1 TO SPB-INSCOM(1).
           MOVE QSPB-INSRATECD-1 TO SPB-INSRATECD(1).
           MOVE QSPB-INSJFAC-1 TO SPB-INSJFAC(1).
           MOVE QSPB-INSDISCNT-1 TO SPB-INSDISCNT(1).
           MOVE QSPB-INSCHGABL-1 TO SPB-INSCHGABL(1).
           MOVE QSPB-INSTERM-1-1 TO SPB-INSTERM(1 1).
           MOVE QSPB-INSRATE-1-1 TO SPB-INSRATE(1 1).
           MOVE QSPB-INSTERM-1-2 TO SPB-INSTERM(1 2).
           MOVE QSPB-INSRATE-1-2 TO SPB-INSRATE(1 2).
           MOVE QSPB-INSTERM-1-3 TO SPB-INSTERM(1 3).
           MOVE QSPB-INSRATE-1-3 TO SPB-INSRATE(1 3).
           MOVE QSPB-INSTERM-1-4 TO SPB-INSTERM(1 4).
           MOVE QSPB-INSRATE-1-4 TO SPB-INSRATE(1 4).
           MOVE QSPB-INSTERM-1-5 TO SPB-INSTERM(1 5).
           MOVE QSPB-INSRATE-1-5 TO SPB-INSRATE(1 5).
           MOVE QSPB-INSTERM-1-6 TO SPB-INSTERM(1 6).
           MOVE QSPB-INSRATE-1-6 TO SPB-INSRATE(1 6).
           MOVE QSPB-INSTERM-1-7 TO SPB-INSTERM(1 7).
           MOVE QSPB-INSRATE-1-7 TO SPB-INSRATE(1 7).
           MOVE QSPB-INSTERM-1-8 TO SPB-INSTERM(1 8).
           MOVE QSPB-INSRATE-1-8 TO SPB-INSRATE(1 8).
           MOVE QSPB-INSTERM-1-9 TO SPB-INSTERM(1 9).
           MOVE QSPB-INSRATE-1-9 TO SPB-INSRATE(1 9).
           MOVE QSPB-INSTERM-1-10 TO SPB-INSTERM(1 10).
           MOVE QSPB-INSRATE-1-10 TO SPB-INSRATE(1 10).
           MOVE QSPB-INSTERM-1-11 TO SPB-INSTERM(1 11).
           MOVE QSPB-INSRATE-1-11 TO SPB-INSRATE(1 11).
           MOVE QSPB-INSTERM-1-12 TO SPB-INSTERM(1 12).
           MOVE QSPB-INSRATE-1-12 TO SPB-INSRATE(1 12).
           MOVE QSPB-INSTERM-1-13 TO SPB-INSTERM(1 13).
           MOVE QSPB-INSRATE-1-13 TO SPB-INSRATE(1 13).
           MOVE QSPB-INSTERM-1-14 TO SPB-INSTERM(1 14).
           MOVE QSPB-INSRATE-1-14 TO SPB-INSRATE(1 14).
           MOVE QSPB-INSTERM-1-15 TO SPB-INSTERM(1 15).
           MOVE QSPB-INSRATE-1-15 TO SPB-INSRATE(1 15).
           MOVE QSPB-INSTERM-1-16 TO SPB-INSTERM(1 16).
           MOVE QSPB-INSRATE-1-16 TO SPB-INSRATE(1 16).
           MOVE QSPB-INSTERM-1-17 TO SPB-INSTERM(1 17).
           MOVE QSPB-INSRATE-1-17 TO SPB-INSRATE(1 17).
           MOVE QSPB-INSTERM-1-18 TO SPB-INSTERM(1 18).
           MOVE QSPB-INSRATE-1-18 TO SPB-INSRATE(1 18).
           MOVE QSPB-INSTERM-1-19 TO SPB-INSTERM(1 19).
           MOVE QSPB-INSRATE-1-19 TO SPB-INSRATE(1 19).
           MOVE QSPB-INSTERM-1-20 TO SPB-INSTERM(1 20).
           MOVE QSPB-INSRATE-1-20 TO SPB-INSRATE(1 20).
           MOVE QSPB-INSMINPREM-1 TO SPB-INSMINPREM(1).
           MOVE QSPB-INSMINCHGABL-1 TO SPB-INSMINCHGABL(1).
           MOVE QSPB-INSMAXCHGABL-1 TO SPB-INSMAXCHGABL(1).
           MOVE QSPB-INSMAXBENIF-1 TO SPB-INSMAXBENIF(1).
           MOVE QSPB-INSMAXTERM-1 TO SPB-INSMAXTERM(1).
           MOVE QSPB-INSCHGBEN-1 TO SPB-INSCHGBEN(1).
           MOVE QSPB-INSEXCEPT-1 TO SPB-INSEXCEPT(1).
           MOVE QSPB-INSNOVER-1 TO SPB-INSNOVER(1).
           MOVE QSPB-INSMINAGE-1-1 TO SPB-INSMINAGE(1 1).
           MOVE QSPB-INSMAXAGE-1-1 TO SPB-INSMAXAGE(1 1).
           MOVE QSPB-INSMAXCOV-1-1 TO SPB-INSMAXCOV(1 1).
           MOVE QSPB-INSMAXTRM-1-1 TO SPB-INSMAXTRM(1 1).
           MOVE QSPB-INSMINAGE-1-2 TO SPB-INSMINAGE(1 2).
           MOVE QSPB-INSMAXAGE-1-2 TO SPB-INSMAXAGE(1 2).
           MOVE QSPB-INSMAXCOV-1-2 TO SPB-INSMAXCOV(1 2).
           MOVE QSPB-INSMAXTRM-1-2 TO SPB-INSMAXTRM(1 2).
           MOVE QSPB-INSMINAGE-1-3 TO SPB-INSMINAGE(1 3).
           MOVE QSPB-INSMAXAGE-1-3 TO SPB-INSMAXAGE(1 3).
           MOVE QSPB-INSMAXCOV-1-3 TO SPB-INSMAXCOV(1 3).
           MOVE QSPB-INSMAXTRM-1-3 TO SPB-INSMAXTRM(1 3).
           MOVE QSPB-INSMINAGE-1-4 TO SPB-INSMINAGE(1 4).
           MOVE QSPB-INSMAXAGE-1-4 TO SPB-INSMAXAGE(1 4).
           MOVE QSPB-INSMAXCOV-1-4 TO SPB-INSMAXCOV(1 4).
           MOVE QSPB-INSMAXTRM-1-4 TO SPB-INSMAXTRM(1 4).
           MOVE QSPB-INSMINAGE-1-5 TO SPB-INSMINAGE(1 5).
           MOVE QSPB-INSMAXAGE-1-5 TO SPB-INSMAXAGE(1 5).
           MOVE QSPB-INSMAXCOV-1-5 TO SPB-INSMAXCOV(1 5).
           MOVE QSPB-INSMAXTRM-1-5 TO SPB-INSMAXTRM(1 5).
           MOVE QSPB-INSFEECD-1 TO SPB-INSFEECD(1).
           MOVE QSPB-INSFEE-FRMLA-1 TO SPB-INSFEE-FRMLA(1).
           MOVE QSPB-INSCHGABL-FG-1 TO SPB-INSCHGABL-FG(1).
           MOVE QSPB-INSREJECT-FG-1 TO SPB-INSREJECT-FG(1).
           MOVE QSPB-INSFINCHG-1 TO SPB-INSFINCHG(1).
           MOVE QSPB-INSCOM-2 TO SPB-INSCOM(2).
           MOVE QSPB-INSRATECD-2 TO SPB-INSRATECD(2).
           MOVE QSPB-INSJFAC-2 TO SPB-INSJFAC(2).
           MOVE QSPB-INSDISCNT-2 TO SPB-INSDISCNT(2).
           MOVE QSPB-INSCHGABL-2 TO SPB-INSCHGABL(2).
           MOVE QSPB-INSTERM-2-1 TO SPB-INSTERM(2 1).
           MOVE QSPB-INSRATE-2-1 TO SPB-INSRATE(2 1).
           MOVE QSPB-INSTERM-2-2 TO SPB-INSTERM(2 2).
           MOVE QSPB-INSRATE-2-2 TO SPB-INSRATE(2 2).
           MOVE QSPB-INSTERM-2-3 TO SPB-INSTERM(2 3).
           MOVE QSPB-INSRATE-2-3 TO SPB-INSRATE(2 3).
           MOVE QSPB-INSTERM-2-4 TO SPB-INSTERM(2 4).
           MOVE QSPB-INSRATE-2-4 TO SPB-INSRATE(2 4).
           MOVE QSPB-INSTERM-2-5 TO SPB-INSTERM(2 5).
           MOVE QSPB-INSRATE-2-5 TO SPB-INSRATE(2 5).
           MOVE QSPB-INSTERM-2-6 TO SPB-INSTERM(2 6).
           MOVE QSPB-INSRATE-2-6 TO SPB-INSRATE(2 6).
           MOVE QSPB-INSTERM-2-7 TO SPB-INSTERM(2 7).
           MOVE QSPB-INSRATE-2-7 TO SPB-INSRATE(2 7).
           MOVE QSPB-INSTERM-2-8 TO SPB-INSTERM(2 8).
           MOVE QSPB-INSRATE-2-8 TO SPB-INSRATE(2 8).
           MOVE QSPB-INSTERM-2-9 TO SPB-INSTERM(2 9).
           MOVE QSPB-INSRATE-2-9 TO SPB-INSRATE(2 9).
           MOVE QSPB-INSTERM-2-10 TO SPB-INSTERM(2 10).
           MOVE QSPB-INSRATE-2-10 TO SPB-INSRATE(2 10).
           MOVE QSPB-INSTERM-2-11 TO SPB-INSTERM(2 11).
           MOVE QSPB-INSRATE-2-11 TO SPB-INSRATE(2 11).
           MOVE QSPB-INSTERM-2-12 TO SPB-INSTERM(2 12).
           MOVE QSPB-INSRATE-2-12 TO SPB-INSRATE(2 12).
           MOVE QSPB-INSTERM-2-13 TO SPB-INSTERM(2 13).
           MOVE QSPB-INSRATE-2-13 TO SPB-INSRATE(2 13).
           MOVE QSPB-INSTERM-2-14 TO SPB-INSTERM(2 14).
           MOVE QSPB-INSRATE-2-14 TO SPB-INSRATE(2 14).
           MOVE QSPB-INSTERM-2-15 TO SPB-INSTERM(2 15).
           MOVE QSPB-INSRATE-2-15 TO SPB-INSRATE(2 15).
           MOVE QSPB-INSTERM-2-16 TO SPB-INSTERM(2 16).
           MOVE QSPB-INSRATE-2-16 TO SPB-INSRATE(2 16).
           MOVE QSPB-INSTERM-2-17 TO SPB-INSTERM(2 17).
           MOVE QSPB-INSRATE-2-17 TO SPB-INSRATE(2 17).
           MOVE QSPB-INSTERM-2-18 TO SPB-INSTERM(2 18).
           MOVE QSPB-INSRATE-2-18 TO SPB-INSRATE(2 18).
           MOVE QSPB-INSTERM-2-19 TO SPB-INSTERM(2 19).
           MOVE QSPB-INSRATE-2-19 TO SPB-INSRATE(2 19).
           MOVE QSPB-INSTERM-2-20 TO SPB-INSTERM(2 20).
           MOVE QSPB-INSRATE-2-20 TO SPB-INSRATE(2 20).
           MOVE QSPB-INSMINPREM-2 TO SPB-INSMINPREM(2).
           MOVE QSPB-INSMINCHGABL-2 TO SPB-INSMINCHGABL(2).
           MOVE QSPB-INSMAXCHGABL-2 TO SPB-INSMAXCHGABL(2).
           MOVE QSPB-INSMAXBENIF-2 TO SPB-INSMAXBENIF(2).
           MOVE QSPB-INSMAXTERM-2 TO SPB-INSMAXTERM(2).
           MOVE QSPB-INSCHGBEN-2 TO SPB-INSCHGBEN(2).
           MOVE QSPB-INSEXCEPT-2 TO SPB-INSEXCEPT(2).
           MOVE QSPB-INSNOVER-2 TO SPB-INSNOVER(2).
           MOVE QSPB-INSMINAGE-2-1 TO SPB-INSMINAGE(2 1).
           MOVE QSPB-INSMAXAGE-2-1 TO SPB-INSMAXAGE(2 1).
           MOVE QSPB-INSMAXCOV-2-1 TO SPB-INSMAXCOV(2 1).
           MOVE QSPB-INSMAXTRM-2-1 TO SPB-INSMAXTRM(2 1).
           MOVE QSPB-INSMINAGE-2-2 TO SPB-INSMINAGE(2 2).
           MOVE QSPB-INSMAXAGE-2-2 TO SPB-INSMAXAGE(2 2).
           MOVE QSPB-INSMAXCOV-2-2 TO SPB-INSMAXCOV(2 2).
           MOVE QSPB-INSMAXTRM-2-2 TO SPB-INSMAXTRM(2 2).
           MOVE QSPB-INSMINAGE-2-3 TO SPB-INSMINAGE(2 3).
           MOVE QSPB-INSMAXAGE-2-3 TO SPB-INSMAXAGE(2 3).
           MOVE QSPB-INSMAXCOV-2-3 TO SPB-INSMAXCOV(2 3).
           MOVE QSPB-INSMAXTRM-2-3 TO SPB-INSMAXTRM(2 3).
           MOVE QSPB-INSMINAGE-2-4 TO SPB-INSMINAGE(2 4).
           MOVE QSPB-INSMAXAGE-2-4 TO SPB-INSMAXAGE(2 4).
           MOVE QSPB-INSMAXCOV-2-4 TO SPB-INSMAXCOV(2 4).
           MOVE QSPB-INSMAXTRM-2-4 TO SPB-INSMAXTRM(2 4).
           MOVE QSPB-INSMINAGE-2-5 TO SPB-INSMINAGE(2 5).
           MOVE QSPB-INSMAXAGE-2-5 TO SPB-INSMAXAGE(2 5).
           MOVE QSPB-INSMAXCOV-2-5 TO SPB-INSMAXCOV(2 5).
           MOVE QSPB-INSMAXTRM-2-5 TO SPB-INSMAXTRM(2 5).
           MOVE QSPB-INSFEECD-2 TO SPB-INSFEECD(2).
           MOVE QSPB-INSFEE-FRMLA-2 TO SPB-INSFEE-FRMLA(2).
           MOVE QSPB-INSCHGABL-FG-2 TO SPB-INSCHGABL-FG(2).
           MOVE QSPB-INSREJECT-FG-2 TO SPB-INSREJECT-FG(2).
           MOVE QSPB-INSFINCHG-2 TO SPB-INSFINCHG(2).
           MOVE QSPB-INSCOM-3 TO SPB-INSCOM(3).
           MOVE QSPB-INSRATECD-3 TO SPB-INSRATECD(3).
           MOVE QSPB-INSJFAC-3 TO SPB-INSJFAC(3).
           MOVE QSPB-INSDISCNT-3 TO SPB-INSDISCNT(3).
           MOVE QSPB-INSCHGABL-3 TO SPB-INSCHGABL(3).
           MOVE QSPB-INSTERM-3-1 TO SPB-INSTERM(3 1).
           MOVE QSPB-INSRATE-3-1 TO SPB-INSRATE(3 1).
           MOVE QSPB-INSTERM-3-2 TO SPB-INSTERM(3 2).
           MOVE QSPB-INSRATE-3-2 TO SPB-INSRATE(3 2).
           MOVE QSPB-INSTERM-3-3 TO SPB-INSTERM(3 3).
           MOVE QSPB-INSRATE-3-3 TO SPB-INSRATE(3 3).
           MOVE QSPB-INSTERM-3-4 TO SPB-INSTERM(3 4).
           MOVE QSPB-INSRATE-3-4 TO SPB-INSRATE(3 4).
           MOVE QSPB-INSTERM-3-5 TO SPB-INSTERM(3 5).
           MOVE QSPB-INSRATE-3-5 TO SPB-INSRATE(3 5).
           MOVE QSPB-INSTERM-3-6 TO SPB-INSTERM(3 6).
           MOVE QSPB-INSRATE-3-6 TO SPB-INSRATE(3 6).
           MOVE QSPB-INSTERM-3-7 TO SPB-INSTERM(3 7).
           MOVE QSPB-INSRATE-3-7 TO SPB-INSRATE(3 7).
           MOVE QSPB-INSTERM-3-8 TO SPB-INSTERM(3 8).
           MOVE QSPB-INSRATE-3-8 TO SPB-INSRATE(3 8).
           MOVE QSPB-INSTERM-3-9 TO SPB-INSTERM(3 9).
           MOVE QSPB-INSRATE-3-9 TO SPB-INSRATE(3 9).
           MOVE QSPB-INSTERM-3-10 TO SPB-INSTERM(3 10).
           MOVE QSPB-INSRATE-3-10 TO SPB-INSRATE(3 10).
           MOVE QSPB-INSTERM-3-11 TO SPB-INSTERM(3 11).
           MOVE QSPB-INSRATE-3-11 TO SPB-INSRATE(3 11).
           MOVE QSPB-INSTERM-3-12 TO SPB-INSTERM(3 12).
           MOVE QSPB-INSRATE-3-12 TO SPB-INSRATE(3 12).
           MOVE QSPB-INSTERM-3-13 TO SPB-INSTERM(3 13).
           MOVE QSPB-INSRATE-3-13 TO SPB-INSRATE(3 13).
           MOVE QSPB-INSTERM-3-14 TO SPB-INSTERM(3 14).
           MOVE QSPB-INSRATE-3-14 TO SPB-INSRATE(3 14).
           MOVE QSPB-INSTERM-3-15 TO SPB-INSTERM(3 15).
           MOVE QSPB-INSRATE-3-15 TO SPB-INSRATE(3 15).
           MOVE QSPB-INSTERM-3-16 TO SPB-INSTERM(3 16).
           MOVE QSPB-INSRATE-3-16 TO SPB-INSRATE(3 16).
           MOVE QSPB-INSTERM-3-17 TO SPB-INSTERM(3 17).
           MOVE QSPB-INSRATE-3-17 TO SPB-INSRATE(3 17).
           MOVE QSPB-INSTERM-3-18 TO SPB-INSTERM(3 18).
           MOVE QSPB-INSRATE-3-18 TO SPB-INSRATE(3 18).
           MOVE QSPB-INSTERM-3-19 TO SPB-INSTERM(3 19).
           MOVE QSPB-INSRATE-3-19 TO SPB-INSRATE(3 19).
           MOVE QSPB-INSTERM-3-20 TO SPB-INSTERM(3 20).
           MOVE QSPB-INSRATE-3-20 TO SPB-INSRATE(3 20).
           MOVE QSPB-INSMINPREM-3 TO SPB-INSMINPREM(3).
           MOVE QSPB-INSMINCHGABL-3 TO SPB-INSMINCHGABL(3).
           MOVE QSPB-INSMAXCHGABL-3 TO SPB-INSMAXCHGABL(3).
           MOVE QSPB-INSMAXBENIF-3 TO SPB-INSMAXBENIF(3).
           MOVE QSPB-INSMAXTERM-3 TO SPB-INSMAXTERM(3).
           MOVE QSPB-INSCHGBEN-3 TO SPB-INSCHGBEN(3).
           MOVE QSPB-INSEXCEPT-3 TO SPB-INSEXCEPT(3).
           MOVE QSPB-INSNOVER-3 TO SPB-INSNOVER(3).
           MOVE QSPB-INSMINAGE-3-1 TO SPB-INSMINAGE(3 1).
           MOVE QSPB-INSMAXAGE-3-1 TO SPB-INSMAXAGE(3 1).
           MOVE QSPB-INSMAXCOV-3-1 TO SPB-INSMAXCOV(3 1).
           MOVE QSPB-INSMAXTRM-3-1 TO SPB-INSMAXTRM(3 1).
           MOVE QSPB-INSMINAGE-3-2 TO SPB-INSMINAGE(3 2).
           MOVE QSPB-INSMAXAGE-3-2 TO SPB-INSMAXAGE(3 2).
           MOVE QSPB-INSMAXCOV-3-2 TO SPB-INSMAXCOV(3 2).
           MOVE QSPB-INSMAXTRM-3-2 TO SPB-INSMAXTRM(3 2).
           MOVE QSPB-INSMINAGE-3-3 TO SPB-INSMINAGE(3 3).
           MOVE QSPB-INSMAXAGE-3-3 TO SPB-INSMAXAGE(3 3).
           MOVE QSPB-INSMAXCOV-3-3 TO SPB-INSMAXCOV(3 3).
           MOVE QSPB-INSMAXTRM-3-3 TO SPB-INSMAXTRM(3 3).
           MOVE QSPB-INSMINAGE-3-4 TO SPB-INSMINAGE(3 4).
           MOVE QSPB-INSMAXAGE-3-4 TO SPB-INSMAXAGE(3 4).
           MOVE QSPB-INSMAXCOV-3-4 TO SPB-INSMAXCOV(3 4).
           MOVE QSPB-INSMAXTRM-3-4 TO SPB-INSMAXTRM(3 4).
           MOVE QSPB-INSMINAGE-3-5 TO SPB-INSMINAGE(3 5).
           MOVE QSPB-INSMAXAGE-3-5 TO SPB-INSMAXAGE(3 5).
           MOVE QSPB-INSMAXCOV-3-5 TO SPB-INSMAXCOV(3 5).
           MOVE QSPB-INSMAXTRM-3-5 TO SPB-INSMAXTRM(3 5).
           MOVE QSPB-INSFEECD-3 TO SPB-INSFEECD(3).
           MOVE QSPB-INSFEE-FRMLA-3 TO SPB-INSFEE-FRMLA(3).
           MOVE QSPB-INSCHGABL-FG-3 TO SPB-INSCHGABL-FG(3).
           MOVE QSPB-INSREJECT-FG-3 TO SPB-INSREJECT-FG(3).
           MOVE QSPB-INSFINCHG-3 TO SPB-INSFINCHG(3).
           MOVE QSPB-INSCOM-4 TO SPB-INSCOM(4).
           MOVE QSPB-INSRATECD-4 TO SPB-INSRATECD(4).
           MOVE QSPB-INSJFAC-4 TO SPB-INSJFAC(4).
           MOVE QSPB-INSDISCNT-4 TO SPB-INSDISCNT(4).
           MOVE QSPB-INSCHGABL-4 TO SPB-INSCHGABL(4).
           MOVE QSPB-INSTERM-4-1 TO SPB-INSTERM(4 1).
           MOVE QSPB-INSRATE-4-1 TO SPB-INSRATE(4 1).
           MOVE QSPB-INSTERM-4-2 TO SPB-INSTERM(4 2).
           MOVE QSPB-INSRATE-4-2 TO SPB-INSRATE(4 2).
           MOVE QSPB-INSTERM-4-3 TO SPB-INSTERM(4 3).
           MOVE QSPB-INSRATE-4-3 TO SPB-INSRATE(4 3).
           MOVE QSPB-INSTERM-4-4 TO SPB-INSTERM(4 4).
           MOVE QSPB-INSRATE-4-4 TO SPB-INSRATE(4 4).
           MOVE QSPB-INSTERM-4-5 TO SPB-INSTERM(4 5).
           MOVE QSPB-INSRATE-4-5 TO SPB-INSRATE(4 5).
           MOVE QSPB-INSTERM-4-6 TO SPB-INSTERM(4 6).
           MOVE QSPB-INSRATE-4-6 TO SPB-INSRATE(4 6).
           MOVE QSPB-INSTERM-4-7 TO SPB-INSTERM(4 7).
           MOVE QSPB-INSRATE-4-7 TO SPB-INSRATE(4 7).
           MOVE QSPB-INSTERM-4-8 TO SPB-INSTERM(4 8).
           MOVE QSPB-INSRATE-4-8 TO SPB-INSRATE(4 8).
           MOVE QSPB-INSTERM-4-9 TO SPB-INSTERM(4 9).
           MOVE QSPB-INSRATE-4-9 TO SPB-INSRATE(4 9).
           MOVE QSPB-INSTERM-4-10 TO SPB-INSTERM(4 10).
           MOVE QSPB-INSRATE-4-10 TO SPB-INSRATE(4 10).
           MOVE QSPB-INSTERM-4-11 TO SPB-INSTERM(4 11).
           MOVE QSPB-INSRATE-4-11 TO SPB-INSRATE(4 11).
           MOVE QSPB-INSTERM-4-12 TO SPB-INSTERM(4 12).
           MOVE QSPB-INSRATE-4-12 TO SPB-INSRATE(4 12).
           MOVE QSPB-INSTERM-4-13 TO SPB-INSTERM(4 13).
           MOVE QSPB-INSRATE-4-13 TO SPB-INSRATE(4 13).
           MOVE QSPB-INSTERM-4-14 TO SPB-INSTERM(4 14).
           MOVE QSPB-INSRATE-4-14 TO SPB-INSRATE(4 14).
           MOVE QSPB-INSTERM-4-15 TO SPB-INSTERM(4 15).
           MOVE QSPB-INSRATE-4-15 TO SPB-INSRATE(4 15).
           MOVE QSPB-INSTERM-4-16 TO SPB-INSTERM(4 16).
           MOVE QSPB-INSRATE-4-16 TO SPB-INSRATE(4 16).
           MOVE QSPB-INSTERM-4-17 TO SPB-INSTERM(4 17).
           MOVE QSPB-INSRATE-4-17 TO SPB-INSRATE(4 17).
           MOVE QSPB-INSTERM-4-18 TO SPB-INSTERM(4 18).
           MOVE QSPB-INSRATE-4-18 TO SPB-INSRATE(4 18).
           MOVE QSPB-INSTERM-4-19 TO SPB-INSTERM(4 19).
           MOVE QSPB-INSRATE-4-19 TO SPB-INSRATE(4 19).
           MOVE QSPB-INSTERM-4-20 TO SPB-INSTERM(4 20).
           MOVE QSPB-INSRATE-4-20 TO SPB-INSRATE(4 20).
           MOVE QSPB-INSMINPREM-4 TO SPB-INSMINPREM(4).
           MOVE QSPB-INSMINCHGABL-4 TO SPB-INSMINCHGABL(4).
           MOVE QSPB-INSMAXCHGABL-4 TO SPB-INSMAXCHGABL(4).
           MOVE QSPB-INSMAXBENIF-4 TO SPB-INSMAXBENIF(4).
           MOVE QSPB-INSMAXTERM-4 TO SPB-INSMAXTERM(4).
           MOVE QSPB-INSCHGBEN-4 TO SPB-INSCHGBEN(4).
           MOVE QSPB-INSEXCEPT-4 TO SPB-INSEXCEPT(4).
           MOVE QSPB-INSNOVER-4 TO SPB-INSNOVER(4).
           MOVE QSPB-INSMINAGE-4-1 TO SPB-INSMINAGE(4 1).
           MOVE QSPB-INSMAXAGE-4-1 TO SPB-INSMAXAGE(4 1).
           MOVE QSPB-INSMAXCOV-4-1 TO SPB-INSMAXCOV(4 1).
           MOVE QSPB-INSMAXTRM-4-1 TO SPB-INSMAXTRM(4 1).
           MOVE QSPB-INSMINAGE-4-2 TO SPB-INSMINAGE(4 2).
           MOVE QSPB-INSMAXAGE-4-2 TO SPB-INSMAXAGE(4 2).
           MOVE QSPB-INSMAXCOV-4-2 TO SPB-INSMAXCOV(4 2).
           MOVE QSPB-INSMAXTRM-4-2 TO SPB-INSMAXTRM(4 2).
           MOVE QSPB-INSMINAGE-4-3 TO SPB-INSMINAGE(4 3).
           MOVE QSPB-INSMAXAGE-4-3 TO SPB-INSMAXAGE(4 3).
           MOVE QSPB-INSMAXCOV-4-3 TO SPB-INSMAXCOV(4 3).
           MOVE QSPB-INSMAXTRM-4-3 TO SPB-INSMAXTRM(4 3).
           MOVE QSPB-INSMINAGE-4-4 TO SPB-INSMINAGE(4 4).
           MOVE QSPB-INSMAXAGE-4-4 TO SPB-INSMAXAGE(4 4).
           MOVE QSPB-INSMAXCOV-4-4 TO SPB-INSMAXCOV(4 4).
           MOVE QSPB-INSMAXTRM-4-4 TO SPB-INSMAXTRM(4 4).
           MOVE QSPB-INSMINAGE-4-5 TO SPB-INSMINAGE(4 5).
           MOVE QSPB-INSMAXAGE-4-5 TO SPB-INSMAXAGE(4 5).
           MOVE QSPB-INSMAXCOV-4-5 TO SPB-INSMAXCOV(4 5).
           MOVE QSPB-INSMAXTRM-4-5 TO SPB-INSMAXTRM(4 5).
           MOVE QSPB-INSFEECD-4 TO SPB-INSFEECD(4).
           MOVE QSPB-INSFEE-FRMLA-4 TO SPB-INSFEE-FRMLA(4).
           MOVE QSPB-INSCHGABL-FG-4 TO SPB-INSCHGABL-FG(4).
           MOVE QSPB-INSREJECT-FG-4 TO SPB-INSREJECT-FG(4).
           MOVE QSPB-INSFINCHG-4 TO SPB-INSFINCHG(4).
           MOVE QSPB-INSCOM-5 TO SPB-INSCOM(5).
           MOVE QSPB-INSRATECD-5 TO SPB-INSRATECD(5).
           MOVE QSPB-INSJFAC-5 TO SPB-INSJFAC(5).
           MOVE QSPB-INSDISCNT-5 TO SPB-INSDISCNT(5).
           MOVE QSPB-INSCHGABL-5 TO SPB-INSCHGABL(5).
           MOVE QSPB-INSTERM-5-1 TO SPB-INSTERM(5 1).
           MOVE QSPB-INSRATE-5-1 TO SPB-INSRATE(5 1).
           MOVE QSPB-INSTERM-5-2 TO SPB-INSTERM(5 2).
           MOVE QSPB-INSRATE-5-2 TO SPB-INSRATE(5 2).
           MOVE QSPB-INSTERM-5-3 TO SPB-INSTERM(5 3).
           MOVE QSPB-INSRATE-5-3 TO SPB-INSRATE(5 3).
           MOVE QSPB-INSTERM-5-4 TO SPB-INSTERM(5 4).
           MOVE QSPB-INSRATE-5-4 TO SPB-INSRATE(5 4).
           MOVE QSPB-INSTERM-5-5 TO SPB-INSTERM(5 5).
           MOVE QSPB-INSRATE-5-5 TO SPB-INSRATE(5 5).
           MOVE QSPB-INSTERM-5-6 TO SPB-INSTERM(5 6).
           MOVE QSPB-INSRATE-5-6 TO SPB-INSRATE(5 6).
           MOVE QSPB-INSTERM-5-7 TO SPB-INSTERM(5 7).
           MOVE QSPB-INSRATE-5-7 TO SPB-INSRATE(5 7).
           MOVE QSPB-INSTERM-5-8 TO SPB-INSTERM(5 8).
           MOVE QSPB-INSRATE-5-8 TO SPB-INSRATE(5 8).
           MOVE QSPB-INSTERM-5-9 TO SPB-INSTERM(5 9).
           MOVE QSPB-INSRATE-5-9 TO SPB-INSRATE(5 9).
           MOVE QSPB-INSTERM-5-10 TO SPB-INSTERM(5 10).
           MOVE QSPB-INSRATE-5-10 TO SPB-INSRATE(5 10).
           MOVE QSPB-INSTERM-5-11 TO SPB-INSTERM(5 11).
           MOVE QSPB-INSRATE-5-11 TO SPB-INSRATE(5 11).
           MOVE QSPB-INSTERM-5-12 TO SPB-INSTERM(5 12).
           MOVE QSPB-INSRATE-5-12 TO SPB-INSRATE(5 12).
           MOVE QSPB-INSTERM-5-13 TO SPB-INSTERM(5 13).
           MOVE QSPB-INSRATE-5-13 TO SPB-INSRATE(5 13).
           MOVE QSPB-INSTERM-5-14 TO SPB-INSTERM(5 14).
           MOVE QSPB-INSRATE-5-14 TO SPB-INSRATE(5 14).
           MOVE QSPB-INSTERM-5-15 TO SPB-INSTERM(5 15).
           MOVE QSPB-INSRATE-5-15 TO SPB-INSRATE(5 15).
           MOVE QSPB-INSTERM-5-16 TO SPB-INSTERM(5 16).
           MOVE QSPB-INSRATE-5-16 TO SPB-INSRATE(5 16).
           MOVE QSPB-INSTERM-5-17 TO SPB-INSTERM(5 17).
           MOVE QSPB-INSRATE-5-17 TO SPB-INSRATE(5 17).
           MOVE QSPB-INSTERM-5-18 TO SPB-INSTERM(5 18).
           MOVE QSPB-INSRATE-5-18 TO SPB-INSRATE(5 18).
           MOVE QSPB-INSTERM-5-19 TO SPB-INSTERM(5 19).
           MOVE QSPB-INSRATE-5-19 TO SPB-INSRATE(5 19).
           MOVE QSPB-INSTERM-5-20 TO SPB-INSTERM(5 20).
           MOVE QSPB-INSRATE-5-20 TO SPB-INSRATE(5 20).
           MOVE QSPB-INSMINPREM-5 TO SPB-INSMINPREM(5).
           MOVE QSPB-INSMINCHGABL-5 TO SPB-INSMINCHGABL(5).
           MOVE QSPB-INSMAXCHGABL-5 TO SPB-INSMAXCHGABL(5).
           MOVE QSPB-INSMAXBENIF-5 TO SPB-INSMAXBENIF(5).
           MOVE QSPB-INSMAXTERM-5 TO SPB-INSMAXTERM(5).
           MOVE QSPB-INSCHGBEN-5 TO SPB-INSCHGBEN(5).
           MOVE QSPB-INSEXCEPT-5 TO SPB-INSEXCEPT(5).
           MOVE QSPB-INSNOVER-5 TO SPB-INSNOVER(5).
           MOVE QSPB-INSMINAGE-5-1 TO SPB-INSMINAGE(5 1).
           MOVE QSPB-INSMAXAGE-5-1 TO SPB-INSMAXAGE(5 1).
           MOVE QSPB-INSMAXCOV-5-1 TO SPB-INSMAXCOV(5 1).
           MOVE QSPB-INSMAXTRM-5-1 TO SPB-INSMAXTRM(5 1).
           MOVE QSPB-INSMINAGE-5-2 TO SPB-INSMINAGE(5 2).
           MOVE QSPB-INSMAXAGE-5-2 TO SPB-INSMAXAGE(5 2).
           MOVE QSPB-INSMAXCOV-5-2 TO SPB-INSMAXCOV(5 2).
           MOVE QSPB-INSMAXTRM-5-2 TO SPB-INSMAXTRM(5 2).
           MOVE QSPB-INSMINAGE-5-3 TO SPB-INSMINAGE(5 3).
           MOVE QSPB-INSMAXAGE-5-3 TO SPB-INSMAXAGE(5 3).
           MOVE QSPB-INSMAXCOV-5-3 TO SPB-INSMAXCOV(5 3).
           MOVE QSPB-INSMAXTRM-5-3 TO SPB-INSMAXTRM(5 3).
           MOVE QSPB-INSMINAGE-5-4 TO SPB-INSMINAGE(5 4).
           MOVE QSPB-INSMAXAGE-5-4 TO SPB-INSMAXAGE(5 4).
           MOVE QSPB-INSMAXCOV-5-4 TO SPB-INSMAXCOV(5 4).
           MOVE QSPB-INSMAXTRM-5-4 TO SPB-INSMAXTRM(5 4).
           MOVE QSPB-INSMINAGE-5-5 TO SPB-INSMINAGE(5 5).
           MOVE QSPB-INSMAXAGE-5-5 TO SPB-INSMAXAGE(5 5).
           MOVE QSPB-INSMAXCOV-5-5 TO SPB-INSMAXCOV(5 5).
           MOVE QSPB-INSMAXTRM-5-5 TO SPB-INSMAXTRM(5 5).
           MOVE QSPB-INSFEECD-5 TO SPB-INSFEECD(5).
           MOVE QSPB-INSFEE-FRMLA-5 TO SPB-INSFEE-FRMLA(5).
           MOVE QSPB-INSCHGABL-FG-5 TO SPB-INSCHGABL-FG(5).
           MOVE QSPB-INSREJECT-FG-5 TO SPB-INSREJECT-FG(5).
           MOVE QSPB-INSFINCHG-5 TO SPB-INSFINCHG(5).
           MOVE QSPB-INSCOM-6 TO SPB-INSCOM(6).
           MOVE QSPB-INSRATECD-6 TO SPB-INSRATECD(6).
           MOVE QSPB-INSJFAC-6 TO SPB-INSJFAC(6).
           MOVE QSPB-INSDISCNT-6 TO SPB-INSDISCNT(6).
           MOVE QSPB-INSCHGABL-6 TO SPB-INSCHGABL(6).
           MOVE QSPB-INSTERM-6-1 TO SPB-INSTERM(6 1).
           MOVE QSPB-INSRATE-6-1 TO SPB-INSRATE(6 1).
           MOVE QSPB-INSTERM-6-2 TO SPB-INSTERM(6 2).
           MOVE QSPB-INSRATE-6-2 TO SPB-INSRATE(6 2).
           MOVE QSPB-INSTERM-6-3 TO SPB-INSTERM(6 3).
           MOVE QSPB-INSRATE-6-3 TO SPB-INSRATE(6 3).
           MOVE QSPB-INSTERM-6-4 TO SPB-INSTERM(6 4).
           MOVE QSPB-INSRATE-6-4 TO SPB-INSRATE(6 4).
           MOVE QSPB-INSTERM-6-5 TO SPB-INSTERM(6 5).
           MOVE QSPB-INSRATE-6-5 TO SPB-INSRATE(6 5).
           MOVE QSPB-INSTERM-6-6 TO SPB-INSTERM(6 6).
           MOVE QSPB-INSRATE-6-6 TO SPB-INSRATE(6 6).
           MOVE QSPB-INSTERM-6-7 TO SPB-INSTERM(6 7).
           MOVE QSPB-INSRATE-6-7 TO SPB-INSRATE(6 7).
           MOVE QSPB-INSTERM-6-8 TO SPB-INSTERM(6 8).
           MOVE QSPB-INSRATE-6-8 TO SPB-INSRATE(6 8).
           MOVE QSPB-INSTERM-6-9 TO SPB-INSTERM(6 9).
           MOVE QSPB-INSRATE-6-9 TO SPB-INSRATE(6 9).
           MOVE QSPB-INSTERM-6-10 TO SPB-INSTERM(6 10).
           MOVE QSPB-INSRATE-6-10 TO SPB-INSRATE(6 10).
           MOVE QSPB-INSTERM-6-11 TO SPB-INSTERM(6 11).
           MOVE QSPB-INSRATE-6-11 TO SPB-INSRATE(6 11).
           MOVE QSPB-INSTERM-6-12 TO SPB-INSTERM(6 12).
           MOVE QSPB-INSRATE-6-12 TO SPB-INSRATE(6 12).
           MOVE QSPB-INSTERM-6-13 TO SPB-INSTERM(6 13).
           MOVE QSPB-INSRATE-6-13 TO SPB-INSRATE(6 13).
           MOVE QSPB-INSTERM-6-14 TO SPB-INSTERM(6 14).
           MOVE QSPB-INSRATE-6-14 TO SPB-INSRATE(6 14).
           MOVE QSPB-INSTERM-6-15 TO SPB-INSTERM(6 15).
           MOVE QSPB-INSRATE-6-15 TO SPB-INSRATE(6 15).
           MOVE QSPB-INSTERM-6-16 TO SPB-INSTERM(6 16).
           MOVE QSPB-INSRATE-6-16 TO SPB-INSRATE(6 16).
           MOVE QSPB-INSTERM-6-17 TO SPB-INSTERM(6 17).
           MOVE QSPB-INSRATE-6-17 TO SPB-INSRATE(6 17).
           MOVE QSPB-INSTERM-6-18 TO SPB-INSTERM(6 18).
           MOVE QSPB-INSRATE-6-18 TO SPB-INSRATE(6 18).
           MOVE QSPB-INSTERM-6-19 TO SPB-INSTERM(6 19).
           MOVE QSPB-INSRATE-6-19 TO SPB-INSRATE(6 19).
           MOVE QSPB-INSTERM-6-20 TO SPB-INSTERM(6 20).
           MOVE QSPB-INSRATE-6-20 TO SPB-INSRATE(6 20).
           MOVE QSPB-INSMINPREM-6 TO SPB-INSMINPREM(6).
           MOVE QSPB-INSMINCHGABL-6 TO SPB-INSMINCHGABL(6).
           MOVE QSPB-INSMAXCHGABL-6 TO SPB-INSMAXCHGABL(6).
           MOVE QSPB-INSMAXBENIF-6 TO SPB-INSMAXBENIF(6).
           MOVE QSPB-INSMAXTERM-6 TO SPB-INSMAXTERM(6).
           MOVE QSPB-INSCHGBEN-6 TO SPB-INSCHGBEN(6).
           MOVE QSPB-INSEXCEPT-6 TO SPB-INSEXCEPT(6).
           MOVE QSPB-INSNOVER-6 TO SPB-INSNOVER(6).
           MOVE QSPB-INSMINAGE-6-1 TO SPB-INSMINAGE(6 1).
           MOVE QSPB-INSMAXAGE-6-1 TO SPB-INSMAXAGE(6 1).
           MOVE QSPB-INSMAXCOV-6-1 TO SPB-INSMAXCOV(6 1).
           MOVE QSPB-INSMAXTRM-6-1 TO SPB-INSMAXTRM(6 1).
           MOVE QSPB-INSMINAGE-6-2 TO SPB-INSMINAGE(6 2).
           MOVE QSPB-INSMAXAGE-6-2 TO SPB-INSMAXAGE(6 2).
           MOVE QSPB-INSMAXCOV-6-2 TO SPB-INSMAXCOV(6 2).
           MOVE QSPB-INSMAXTRM-6-2 TO SPB-INSMAXTRM(6 2).
           MOVE QSPB-INSMINAGE-6-3 TO SPB-INSMINAGE(6 3).
           MOVE QSPB-INSMAXAGE-6-3 TO SPB-INSMAXAGE(6 3).
           MOVE QSPB-INSMAXCOV-6-3 TO SPB-INSMAXCOV(6 3).
           MOVE QSPB-INSMAXTRM-6-3 TO SPB-INSMAXTRM(6 3).
           MOVE QSPB-INSMINAGE-6-4 TO SPB-INSMINAGE(6 4).
           MOVE QSPB-INSMAXAGE-6-4 TO SPB-INSMAXAGE(6 4).
           MOVE QSPB-INSMAXCOV-6-4 TO SPB-INSMAXCOV(6 4).
           MOVE QSPB-INSMAXTRM-6-4 TO SPB-INSMAXTRM(6 4).
           MOVE QSPB-INSMINAGE-6-5 TO SPB-INSMINAGE(6 5).
           MOVE QSPB-INSMAXAGE-6-5 TO SPB-INSMAXAGE(6 5).
           MOVE QSPB-INSMAXCOV-6-5 TO SPB-INSMAXCOV(6 5).
           MOVE QSPB-INSMAXTRM-6-5 TO SPB-INSMAXTRM(6 5).
           MOVE QSPB-INSFEECD-6 TO SPB-INSFEECD(6).
           MOVE QSPB-INSFEE-FRMLA-6 TO SPB-INSFEE-FRMLA(6).
           MOVE QSPB-INSCHGABL-FG-6 TO SPB-INSCHGABL-FG(6).
           MOVE QSPB-INSREJECT-FG-6 TO SPB-INSREJECT-FG(6).
           MOVE QSPB-INSFINCHG-6 TO SPB-INSFINCHG(6).
           MOVE QSPB-INSCOM-7 TO SPB-INSCOM(7).
           MOVE QSPB-INSRATECD-7 TO SPB-INSRATECD(7).
           MOVE QSPB-INSJFAC-7 TO SPB-INSJFAC(7).
           MOVE QSPB-INSDISCNT-7 TO SPB-INSDISCNT(7).
           MOVE QSPB-INSCHGABL-7 TO SPB-INSCHGABL(7).
           MOVE QSPB-INSTERM-7-1 TO SPB-INSTERM(7 1).
           MOVE QSPB-INSRATE-7-1 TO SPB-INSRATE(7 1).
           MOVE QSPB-INSTERM-7-2 TO SPB-INSTERM(7 2).
           MOVE QSPB-INSRATE-7-2 TO SPB-INSRATE(7 2).
           MOVE QSPB-INSTERM-7-3 TO SPB-INSTERM(7 3).
           MOVE QSPB-INSRATE-7-3 TO SPB-INSRATE(7 3).
           MOVE QSPB-INSTERM-7-4 TO SPB-INSTERM(7 4).
           MOVE QSPB-INSRATE-7-4 TO SPB-INSRATE(7 4).
           MOVE QSPB-INSTERM-7-5 TO SPB-INSTERM(7 5).
           MOVE QSPB-INSRATE-7-5 TO SPB-INSRATE(7 5).
           MOVE QSPB-INSTERM-7-6 TO SPB-INSTERM(7 6).
           MOVE QSPB-INSRATE-7-6 TO SPB-INSRATE(7 6).
           MOVE QSPB-INSTERM-7-7 TO SPB-INSTERM(7 7).
           MOVE QSPB-INSRATE-7-7 TO SPB-INSRATE(7 7).
           MOVE QSPB-INSTERM-7-8 TO SPB-INSTERM(7 8).
           MOVE QSPB-INSRATE-7-8 TO SPB-INSRATE(7 8).
           MOVE QSPB-INSTERM-7-9 TO SPB-INSTERM(7 9).
           MOVE QSPB-INSRATE-7-9 TO SPB-INSRATE(7 9).
           MOVE QSPB-INSTERM-7-10 TO SPB-INSTERM(7 10).
           MOVE QSPB-INSRATE-7-10 TO SPB-INSRATE(7 10).
           MOVE QSPB-INSTERM-7-11 TO SPB-INSTERM(7 11).
           MOVE QSPB-INSRATE-7-11 TO SPB-INSRATE(7 11).
           MOVE QSPB-INSTERM-7-12 TO SPB-INSTERM(7 12).
           MOVE QSPB-INSRATE-7-12 TO SPB-INSRATE(7 12).
           MOVE QSPB-INSTERM-7-13 TO SPB-INSTERM(7 13).
           MOVE QSPB-INSRATE-7-13 TO SPB-INSRATE(7 13).
           MOVE QSPB-INSTERM-7-14 TO SPB-INSTERM(7 14).
           MOVE QSPB-INSRATE-7-14 TO SPB-INSRATE(7 14).
           MOVE QSPB-INSTERM-7-15 TO SPB-INSTERM(7 15).
           MOVE QSPB-INSRATE-7-15 TO SPB-INSRATE(7 15).
           MOVE QSPB-INSTERM-7-16 TO SPB-INSTERM(7 16).
           MOVE QSPB-INSRATE-7-16 TO SPB-INSRATE(7 16).
           MOVE QSPB-INSTERM-7-17 TO SPB-INSTERM(7 17).
           MOVE QSPB-INSRATE-7-17 TO SPB-INSRATE(7 17).
           MOVE QSPB-INSTERM-7-18 TO SPB-INSTERM(7 18).
           MOVE QSPB-INSRATE-7-18 TO SPB-INSRATE(7 18).
           MOVE QSPB-INSTERM-7-19 TO SPB-INSTERM(7 19).
           MOVE QSPB-INSRATE-7-19 TO SPB-INSRATE(7 19).
           MOVE QSPB-INSTERM-7-20 TO SPB-INSTERM(7 20).
           MOVE QSPB-INSRATE-7-20 TO SPB-INSRATE(7 20).
           MOVE QSPB-INSMINPREM-7 TO SPB-INSMINPREM(7).
           MOVE QSPB-INSMINCHGABL-7 TO SPB-INSMINCHGABL(7).
           MOVE QSPB-INSMAXCHGABL-7 TO SPB-INSMAXCHGABL(7).
           MOVE QSPB-INSMAXBENIF-7 TO SPB-INSMAXBENIF(7).
           MOVE QSPB-INSMAXTERM-7 TO SPB-INSMAXTERM(7).
           MOVE QSPB-INSCHGBEN-7 TO SPB-INSCHGBEN(7).
           MOVE QSPB-INSEXCEPT-7 TO SPB-INSEXCEPT(7).
           MOVE QSPB-INSNOVER-7 TO SPB-INSNOVER(7).
           MOVE QSPB-INSMINAGE-7-1 TO SPB-INSMINAGE(7 1).
           MOVE QSPB-INSMAXAGE-7-1 TO SPB-INSMAXAGE(7 1).
           MOVE QSPB-INSMAXCOV-7-1 TO SPB-INSMAXCOV(7 1).
           MOVE QSPB-INSMAXTRM-7-1 TO SPB-INSMAXTRM(7 1).
           MOVE QSPB-INSMINAGE-7-2 TO SPB-INSMINAGE(7 2).
           MOVE QSPB-INSMAXAGE-7-2 TO SPB-INSMAXAGE(7 2).
           MOVE QSPB-INSMAXCOV-7-2 TO SPB-INSMAXCOV(7 2).
           MOVE QSPB-INSMAXTRM-7-2 TO SPB-INSMAXTRM(7 2).
           MOVE QSPB-INSMINAGE-7-3 TO SPB-INSMINAGE(7 3).
           MOVE QSPB-INSMAXAGE-7-3 TO SPB-INSMAXAGE(7 3).
           MOVE QSPB-INSMAXCOV-7-3 TO SPB-INSMAXCOV(7 3).
           MOVE QSPB-INSMAXTRM-7-3 TO SPB-INSMAXTRM(7 3).
           MOVE QSPB-INSMINAGE-7-4 TO SPB-INSMINAGE(7 4).
           MOVE QSPB-INSMAXAGE-7-4 TO SPB-INSMAXAGE(7 4).
           MOVE QSPB-INSMAXCOV-7-4 TO SPB-INSMAXCOV(7 4).
           MOVE QSPB-INSMAXTRM-7-4 TO SPB-INSMAXTRM(7 4).
           MOVE QSPB-INSMINAGE-7-5 TO SPB-INSMINAGE(7 5).
           MOVE QSPB-INSMAXAGE-7-5 TO SPB-INSMAXAGE(7 5).
           MOVE QSPB-INSMAXCOV-7-5 TO SPB-INSMAXCOV(7 5).
           MOVE QSPB-INSMAXTRM-7-5 TO SPB-INSMAXTRM(7 5).
           MOVE QSPB-INSFEECD-7 TO SPB-INSFEECD(7).
           MOVE QSPB-INSFEE-FRMLA-7 TO SPB-INSFEE-FRMLA(7).
           MOVE QSPB-INSCHGABL-FG-7 TO SPB-INSCHGABL-FG(7).
           MOVE QSPB-INSREJECT-FG-7 TO SPB-INSREJECT-FG(7).
           MOVE QSPB-INSFINCHG-7 TO SPB-INSFINCHG(7).
           MOVE QSPB-INSCOM-8 TO SPB-INSCOM(8).
           MOVE QSPB-INSRATECD-8 TO SPB-INSRATECD(8).
           MOVE QSPB-INSJFAC-8 TO SPB-INSJFAC(8).
           MOVE QSPB-INSDISCNT-8 TO SPB-INSDISCNT(8).
           MOVE QSPB-INSCHGABL-8 TO SPB-INSCHGABL(8).
           MOVE QSPB-INSTERM-8-1 TO SPB-INSTERM(8 1).
           MOVE QSPB-INSRATE-8-1 TO SPB-INSRATE(8 1).
           MOVE QSPB-INSTERM-8-2 TO SPB-INSTERM(8 2).
           MOVE QSPB-INSRATE-8-2 TO SPB-INSRATE(8 2).
           MOVE QSPB-INSTERM-8-3 TO SPB-INSTERM(8 3).
           MOVE QSPB-INSRATE-8-3 TO SPB-INSRATE(8 3).
           MOVE QSPB-INSTERM-8-4 TO SPB-INSTERM(8 4).
           MOVE QSPB-INSRATE-8-4 TO SPB-INSRATE(8 4).
           MOVE QSPB-INSTERM-8-5 TO SPB-INSTERM(8 5).
           MOVE QSPB-INSRATE-8-5 TO SPB-INSRATE(8 5).
           MOVE QSPB-INSTERM-8-6 TO SPB-INSTERM(8 6).
           MOVE QSPB-INSRATE-8-6 TO SPB-INSRATE(8 6).
           MOVE QSPB-INSTERM-8-7 TO SPB-INSTERM(8 7).
           MOVE QSPB-INSRATE-8-7 TO SPB-INSRATE(8 7).
           MOVE QSPB-INSTERM-8-8 TO SPB-INSTERM(8 8).
           MOVE QSPB-INSRATE-8-8 TO SPB-INSRATE(8 8).
           MOVE QSPB-INSTERM-8-9 TO SPB-INSTERM(8 9).
           MOVE QSPB-INSRATE-8-9 TO SPB-INSRATE(8 9).
           MOVE QSPB-INSTERM-8-10 TO SPB-INSTERM(8 10).
           MOVE QSPB-INSRATE-8-10 TO SPB-INSRATE(8 10).
           MOVE QSPB-INSTERM-8-11 TO SPB-INSTERM(8 11).
           MOVE QSPB-INSRATE-8-11 TO SPB-INSRATE(8 11).
           MOVE QSPB-INSTERM-8-12 TO SPB-INSTERM(8 12).
           MOVE QSPB-INSRATE-8-12 TO SPB-INSRATE(8 12).
           MOVE QSPB-INSTERM-8-13 TO SPB-INSTERM(8 13).
           MOVE QSPB-INSRATE-8-13 TO SPB-INSRATE(8 13).
           MOVE QSPB-INSTERM-8-14 TO SPB-INSTERM(8 14).
           MOVE QSPB-INSRATE-8-14 TO SPB-INSRATE(8 14).
           MOVE QSPB-INSTERM-8-15 TO SPB-INSTERM(8 15).
           MOVE QSPB-INSRATE-8-15 TO SPB-INSRATE(8 15).
           MOVE QSPB-INSTERM-8-16 TO SPB-INSTERM(8 16).
           MOVE QSPB-INSRATE-8-16 TO SPB-INSRATE(8 16).
           MOVE QSPB-INSTERM-8-17 TO SPB-INSTERM(8 17).
           MOVE QSPB-INSRATE-8-17 TO SPB-INSRATE(8 17).
           MOVE QSPB-INSTERM-8-18 TO SPB-INSTERM(8 18).
           MOVE QSPB-INSRATE-8-18 TO SPB-INSRATE(8 18).
           MOVE QSPB-INSTERM-8-19 TO SPB-INSTERM(8 19).
           MOVE QSPB-INSRATE-8-19 TO SPB-INSRATE(8 19).
           MOVE QSPB-INSTERM-8-20 TO SPB-INSTERM(8 20).
           MOVE QSPB-INSRATE-8-20 TO SPB-INSRATE(8 20).
           MOVE QSPB-INSMINPREM-8 TO SPB-INSMINPREM(8).
           MOVE QSPB-INSMINCHGABL-8 TO SPB-INSMINCHGABL(8).
           MOVE QSPB-INSMAXCHGABL-8 TO SPB-INSMAXCHGABL(8).
           MOVE QSPB-INSMAXBENIF-8 TO SPB-INSMAXBENIF(8).
           MOVE QSPB-INSMAXTERM-8 TO SPB-INSMAXTERM(8).
           MOVE QSPB-INSCHGBEN-8 TO SPB-INSCHGBEN(8).
           MOVE QSPB-INSEXCEPT-8 TO SPB-INSEXCEPT(8).
           MOVE QSPB-INSNOVER-8 TO SPB-INSNOVER(8).
           MOVE QSPB-INSMINAGE-8-1 TO SPB-INSMINAGE(8 1).
           MOVE QSPB-INSMAXAGE-8-1 TO SPB-INSMAXAGE(8 1).
           MOVE QSPB-INSMAXCOV-8-1 TO SPB-INSMAXCOV(8 1).
           MOVE QSPB-INSMAXTRM-8-1 TO SPB-INSMAXTRM(8 1).
           MOVE QSPB-INSMINAGE-8-2 TO SPB-INSMINAGE(8 2).
           MOVE QSPB-INSMAXAGE-8-2 TO SPB-INSMAXAGE(8 2).
           MOVE QSPB-INSMAXCOV-8-2 TO SPB-INSMAXCOV(8 2).
           MOVE QSPB-INSMAXTRM-8-2 TO SPB-INSMAXTRM(8 2).
           MOVE QSPB-INSMINAGE-8-3 TO SPB-INSMINAGE(8 3).
           MOVE QSPB-INSMAXAGE-8-3 TO SPB-INSMAXAGE(8 3).
           MOVE QSPB-INSMAXCOV-8-3 TO SPB-INSMAXCOV(8 3).
           MOVE QSPB-INSMAXTRM-8-3 TO SPB-INSMAXTRM(8 3).
           MOVE QSPB-INSMINAGE-8-4 TO SPB-INSMINAGE(8 4).
           MOVE QSPB-INSMAXAGE-8-4 TO SPB-INSMAXAGE(8 4).
           MOVE QSPB-INSMAXCOV-8-4 TO SPB-INSMAXCOV(8 4).
           MOVE QSPB-INSMAXTRM-8-4 TO SPB-INSMAXTRM(8 4).
           MOVE QSPB-INSMINAGE-8-5 TO SPB-INSMINAGE(8 5).
           MOVE QSPB-INSMAXAGE-8-5 TO SPB-INSMAXAGE(8 5).
           MOVE QSPB-INSMAXCOV-8-5 TO SPB-INSMAXCOV(8 5).
           MOVE QSPB-INSMAXTRM-8-5 TO SPB-INSMAXTRM(8 5).
           MOVE QSPB-INSFEECD-8 TO SPB-INSFEECD(8).
           MOVE QSPB-INSFEE-FRMLA-8 TO SPB-INSFEE-FRMLA(8).
           MOVE QSPB-INSCHGABL-FG-8 TO SPB-INSCHGABL-FG(8).
           MOVE QSPB-INSREJECT-FG-8 TO SPB-INSREJECT-FG(8).
           MOVE QSPB-INSFINCHG-8 TO SPB-INSFINCHG(8).
           MOVE QSPB-INSCOM-9 TO SPB-INSCOM(9).
           MOVE QSPB-INSRATECD-9 TO SPB-INSRATECD(9).
           MOVE QSPB-INSJFAC-9 TO SPB-INSJFAC(9).
           MOVE QSPB-INSDISCNT-9 TO SPB-INSDISCNT(9).
           MOVE QSPB-INSCHGABL-9 TO SPB-INSCHGABL(9).
           MOVE QSPB-INSTERM-9-1 TO SPB-INSTERM(9 1).
           MOVE QSPB-INSRATE-9-1 TO SPB-INSRATE(9 1).
           MOVE QSPB-INSTERM-9-2 TO SPB-INSTERM(9 2).
           MOVE QSPB-INSRATE-9-2 TO SPB-INSRATE(9 2).
           MOVE QSPB-INSTERM-9-3 TO SPB-INSTERM(9 3).
           MOVE QSPB-INSRATE-9-3 TO SPB-INSRATE(9 3).
           MOVE QSPB-INSTERM-9-4 TO SPB-INSTERM(9 4).
           MOVE QSPB-INSRATE-9-4 TO SPB-INSRATE(9 4).
           MOVE QSPB-INSTERM-9-5 TO SPB-INSTERM(9 5).
           MOVE QSPB-INSRATE-9-5 TO SPB-INSRATE(9 5).
           MOVE QSPB-INSTERM-9-6 TO SPB-INSTERM(9 6).
           MOVE QSPB-INSRATE-9-6 TO SPB-INSRATE(9 6).
           MOVE QSPB-INSTERM-9-7 TO SPB-INSTERM(9 7).
           MOVE QSPB-INSRATE-9-7 TO SPB-INSRATE(9 7).
           MOVE QSPB-INSTERM-9-8 TO SPB-INSTERM(9 8).
           MOVE QSPB-INSRATE-9-8 TO SPB-INSRATE(9 8).
           MOVE QSPB-INSTERM-9-9 TO SPB-INSTERM(9 9).
           MOVE QSPB-INSRATE-9-9 TO SPB-INSRATE(9 9).
           MOVE QSPB-INSTERM-9-10 TO SPB-INSTERM(9 10).
           MOVE QSPB-INSRATE-9-10 TO SPB-INSRATE(9 10).
           MOVE QSPB-INSTERM-9-11 TO SPB-INSTERM(9 11).
           MOVE QSPB-INSRATE-9-11 TO SPB-INSRATE(9 11).
           MOVE QSPB-INSTERM-9-12 TO SPB-INSTERM(9 12).
           MOVE QSPB-INSRATE-9-12 TO SPB-INSRATE(9 12).
           MOVE QSPB-INSTERM-9-13 TO SPB-INSTERM(9 13).
           MOVE QSPB-INSRATE-9-13 TO SPB-INSRATE(9 13).
           MOVE QSPB-INSTERM-9-14 TO SPB-INSTERM(9 14).
           MOVE QSPB-INSRATE-9-14 TO SPB-INSRATE(9 14).
           MOVE QSPB-INSTERM-9-15 TO SPB-INSTERM(9 15).
           MOVE QSPB-INSRATE-9-15 TO SPB-INSRATE(9 15).
           MOVE QSPB-INSTERM-9-16 TO SPB-INSTERM(9 16).
           MOVE QSPB-INSRATE-9-16 TO SPB-INSRATE(9 16).
           MOVE QSPB-INSTERM-9-17 TO SPB-INSTERM(9 17).
           MOVE QSPB-INSRATE-9-17 TO SPB-INSRATE(9 17).
           MOVE QSPB-INSTERM-9-18 TO SPB-INSTERM(9 18).
           MOVE QSPB-INSRATE-9-18 TO SPB-INSRATE(9 18).
           MOVE QSPB-INSTERM-9-19 TO SPB-INSTERM(9 19).
           MOVE QSPB-INSRATE-9-19 TO SPB-INSRATE(9 19).
           MOVE QSPB-INSTERM-9-20 TO SPB-INSTERM(9 20).
           MOVE QSPB-INSRATE-9-20 TO SPB-INSRATE(9 20).
           MOVE QSPB-INSMINPREM-9 TO SPB-INSMINPREM(9).
           MOVE QSPB-INSMINCHGABL-9 TO SPB-INSMINCHGABL(9).
           MOVE QSPB-INSMAXCHGABL-9 TO SPB-INSMAXCHGABL(9).
           MOVE QSPB-INSMAXBENIF-9 TO SPB-INSMAXBENIF(9).
           MOVE QSPB-INSMAXTERM-9 TO SPB-INSMAXTERM(9).
           MOVE QSPB-INSCHGBEN-9 TO SPB-INSCHGBEN(9).
           MOVE QSPB-INSEXCEPT-9 TO SPB-INSEXCEPT(9).
           MOVE QSPB-INSNOVER-9 TO SPB-INSNOVER(9).
           MOVE QSPB-INSMINAGE-9-1 TO SPB-INSMINAGE(9 1).
           MOVE QSPB-INSMAXAGE-9-1 TO SPB-INSMAXAGE(9 1).
           MOVE QSPB-INSMAXCOV-9-1 TO SPB-INSMAXCOV(9 1).
           MOVE QSPB-INSMAXTRM-9-1 TO SPB-INSMAXTRM(9 1).
           MOVE QSPB-INSMINAGE-9-2 TO SPB-INSMINAGE(9 2).
           MOVE QSPB-INSMAXAGE-9-2 TO SPB-INSMAXAGE(9 2).
           MOVE QSPB-INSMAXCOV-9-2 TO SPB-INSMAXCOV(9 2).
           MOVE QSPB-INSMAXTRM-9-2 TO SPB-INSMAXTRM(9 2).
           MOVE QSPB-INSMINAGE-9-3 TO SPB-INSMINAGE(9 3).
           MOVE QSPB-INSMAXAGE-9-3 TO SPB-INSMAXAGE(9 3).
           MOVE QSPB-INSMAXCOV-9-3 TO SPB-INSMAXCOV(9 3).
           MOVE QSPB-INSMAXTRM-9-3 TO SPB-INSMAXTRM(9 3).
           MOVE QSPB-INSMINAGE-9-4 TO SPB-INSMINAGE(9 4).
           MOVE QSPB-INSMAXAGE-9-4 TO SPB-INSMAXAGE(9 4).
           MOVE QSPB-INSMAXCOV-9-4 TO SPB-INSMAXCOV(9 4).
           MOVE QSPB-INSMAXTRM-9-4 TO SPB-INSMAXTRM(9 4).
           MOVE QSPB-INSMINAGE-9-5 TO SPB-INSMINAGE(9 5).
           MOVE QSPB-INSMAXAGE-9-5 TO SPB-INSMAXAGE(9 5).
           MOVE QSPB-INSMAXCOV-9-5 TO SPB-INSMAXCOV(9 5).
           MOVE QSPB-INSMAXTRM-9-5 TO SPB-INSMAXTRM(9 5).
           MOVE QSPB-INSFEECD-9 TO SPB-INSFEECD(9).
           MOVE QSPB-INSFEE-FRMLA-9 TO SPB-INSFEE-FRMLA(9).
           MOVE QSPB-INSCHGABL-FG-9 TO SPB-INSCHGABL-FG(9).
           MOVE QSPB-INSREJECT-FG-9 TO SPB-INSREJECT-FG(9).
           MOVE QSPB-INSFINCHG-9 TO SPB-INSFINCHG(9).
           MOVE QSPB-INSCOM-10 TO SPB-INSCOM(10).
           MOVE QSPB-INSRATECD-10 TO SPB-INSRATECD(10).
           MOVE QSPB-INSJFAC-10 TO SPB-INSJFAC(10).
           MOVE QSPB-INSDISCNT-10 TO SPB-INSDISCNT(10).
           MOVE QSPB-INSCHGABL-10 TO SPB-INSCHGABL(10).
           MOVE QSPB-INSTERM-10-1 TO SPB-INSTERM(10 1).
           MOVE QSPB-INSRATE-10-1 TO SPB-INSRATE(10 1).
           MOVE QSPB-INSTERM-10-2 TO SPB-INSTERM(10 2).
           MOVE QSPB-INSRATE-10-2 TO SPB-INSRATE(10 2).
           MOVE QSPB-INSTERM-10-3 TO SPB-INSTERM(10 3).
           MOVE QSPB-INSRATE-10-3 TO SPB-INSRATE(10 3).
           MOVE QSPB-INSTERM-10-4 TO SPB-INSTERM(10 4).
           MOVE QSPB-INSRATE-10-4 TO SPB-INSRATE(10 4).
           MOVE QSPB-INSTERM-10-5 TO SPB-INSTERM(10 5).
           MOVE QSPB-INSRATE-10-5 TO SPB-INSRATE(10 5).
           MOVE QSPB-INSTERM-10-6 TO SPB-INSTERM(10 6).
           MOVE QSPB-INSRATE-10-6 TO SPB-INSRATE(10 6).
           MOVE QSPB-INSTERM-10-7 TO SPB-INSTERM(10 7).
           MOVE QSPB-INSRATE-10-7 TO SPB-INSRATE(10 7).
           MOVE QSPB-INSTERM-10-8 TO SPB-INSTERM(10 8).
           MOVE QSPB-INSRATE-10-8 TO SPB-INSRATE(10 8).
           MOVE QSPB-INSTERM-10-9 TO SPB-INSTERM(10 9).
           MOVE QSPB-INSRATE-10-9 TO SPB-INSRATE(10 9).
           MOVE QSPB-INSTERM-10-10 TO SPB-INSTERM(10 10).
           MOVE QSPB-INSRATE-10-10 TO SPB-INSRATE(10 10).
           MOVE QSPB-INSTERM-10-11 TO SPB-INSTERM(10 11).
           MOVE QSPB-INSRATE-10-11 TO SPB-INSRATE(10 11).
           MOVE QSPB-INSTERM-10-12 TO SPB-INSTERM(10 12).
           MOVE QSPB-INSRATE-10-12 TO SPB-INSRATE(10 12).
           MOVE QSPB-INSTERM-10-13 TO SPB-INSTERM(10 13).
           MOVE QSPB-INSRATE-10-13 TO SPB-INSRATE(10 13).
           MOVE QSPB-INSTERM-10-14 TO SPB-INSTERM(10 14).
           MOVE QSPB-INSRATE-10-14 TO SPB-INSRATE(10 14).
           MOVE QSPB-INSTERM-10-15 TO SPB-INSTERM(10 15).
           MOVE QSPB-INSRATE-10-15 TO SPB-INSRATE(10 15).
           MOVE QSPB-INSTERM-10-16 TO SPB-INSTERM(10 16).
           MOVE QSPB-INSRATE-10-16 TO SPB-INSRATE(10 16).
           MOVE QSPB-INSTERM-10-17 TO SPB-INSTERM(10 17).
           MOVE QSPB-INSRATE-10-17 TO SPB-INSRATE(10 17).
           MOVE QSPB-INSTERM-10-18 TO SPB-INSTERM(10 18).
           MOVE QSPB-INSRATE-10-18 TO SPB-INSRATE(10 18).
           MOVE QSPB-INSTERM-10-19 TO SPB-INSTERM(10 19).
           MOVE QSPB-INSRATE-10-19 TO SPB-INSRATE(10 19).
           MOVE QSPB-INSTERM-10-20 TO SPB-INSTERM(10 20).
           MOVE QSPB-INSRATE-10-20 TO SPB-INSRATE(10 20).
           MOVE QSPB-INSMINPREM-10 TO SPB-INSMINPREM(10).
           MOVE QSPB-INSMINCHGABL-10 TO SPB-INSMINCHGABL(10).
           MOVE QSPB-INSMAXCHGABL-10 TO SPB-INSMAXCHGABL(10).
           MOVE QSPB-INSMAXBENIF-10 TO SPB-INSMAXBENIF(10).
           MOVE QSPB-INSMAXTERM-10 TO SPB-INSMAXTERM(10).
           MOVE QSPB-INSCHGBEN-10 TO SPB-INSCHGBEN(10).
           MOVE QSPB-INSEXCEPT-10 TO SPB-INSEXCEPT(10).
           MOVE QSPB-INSNOVER-10 TO SPB-INSNOVER(10).
           MOVE QSPB-INSMINAGE-10-1 TO SPB-INSMINAGE(10 1).
           MOVE QSPB-INSMAXAGE-10-1 TO SPB-INSMAXAGE(10 1).
           MOVE QSPB-INSMAXCOV-10-1 TO SPB-INSMAXCOV(10 1).
           MOVE QSPB-INSMAXTRM-10-1 TO SPB-INSMAXTRM(10 1).
           MOVE QSPB-INSMINAGE-10-2 TO SPB-INSMINAGE(10 2).
           MOVE QSPB-INSMAXAGE-10-2 TO SPB-INSMAXAGE(10 2).
           MOVE QSPB-INSMAXCOV-10-2 TO SPB-INSMAXCOV(10 2).
           MOVE QSPB-INSMAXTRM-10-2 TO SPB-INSMAXTRM(10 2).
           MOVE QSPB-INSMINAGE-10-3 TO SPB-INSMINAGE(10 3).
           MOVE QSPB-INSMAXAGE-10-3 TO SPB-INSMAXAGE(10 3).
           MOVE QSPB-INSMAXCOV-10-3 TO SPB-INSMAXCOV(10 3).
           MOVE QSPB-INSMAXTRM-10-3 TO SPB-INSMAXTRM(10 3).
           MOVE QSPB-INSMINAGE-10-4 TO SPB-INSMINAGE(10 4).
           MOVE QSPB-INSMAXAGE-10-4 TO SPB-INSMAXAGE(10 4).
           MOVE QSPB-INSMAXCOV-10-4 TO SPB-INSMAXCOV(10 4).
           MOVE QSPB-INSMAXTRM-10-4 TO SPB-INSMAXTRM(10 4).
           MOVE QSPB-INSMINAGE-10-5 TO SPB-INSMINAGE(10 5).
           MOVE QSPB-INSMAXAGE-10-5 TO SPB-INSMAXAGE(10 5).
           MOVE QSPB-INSMAXCOV-10-5 TO SPB-INSMAXCOV(10 5).
           MOVE QSPB-INSMAXTRM-10-5 TO SPB-INSMAXTRM(10 5).
           MOVE QSPB-INSFEECD-10 TO SPB-INSFEECD(10).
           MOVE QSPB-INSFEE-FRMLA-10 TO SPB-INSFEE-FRMLA(10).
           MOVE QSPB-INSCHGABL-FG-10 TO SPB-INSCHGABL-FG(10).
           MOVE QSPB-INSREJECT-FG-10 TO SPB-INSREJECT-FG(10).
           MOVE QSPB-INSFINCHG-10 TO SPB-INSFINCHG(10).
           MOVE QSPB-INSCOM-11 TO SPB-INSCOM(11).
           MOVE QSPB-INSRATECD-11 TO SPB-INSRATECD(11).
           MOVE QSPB-INSJFAC-11 TO SPB-INSJFAC(11).
           MOVE QSPB-INSDISCNT-11 TO SPB-INSDISCNT(11).
           MOVE QSPB-INSCHGABL-11 TO SPB-INSCHGABL(11).
           MOVE QSPB-INSTERM-11-1 TO SPB-INSTERM(11 1).
           MOVE QSPB-INSRATE-11-1 TO SPB-INSRATE(11 1).
           MOVE QSPB-INSTERM-11-2 TO SPB-INSTERM(11 2).
           MOVE QSPB-INSRATE-11-2 TO SPB-INSRATE(11 2).
           MOVE QSPB-INSTERM-11-3 TO SPB-INSTERM(11 3).
           MOVE QSPB-INSRATE-11-3 TO SPB-INSRATE(11 3).
           MOVE QSPB-INSTERM-11-4 TO SPB-INSTERM(11 4).
           MOVE QSPB-INSRATE-11-4 TO SPB-INSRATE(11 4).
           MOVE QSPB-INSTERM-11-5 TO SPB-INSTERM(11 5).
           MOVE QSPB-INSRATE-11-5 TO SPB-INSRATE(11 5).
           MOVE QSPB-INSTERM-11-6 TO SPB-INSTERM(11 6).
           MOVE QSPB-INSRATE-11-6 TO SPB-INSRATE(11 6).
           MOVE QSPB-INSTERM-11-7 TO SPB-INSTERM(11 7).
           MOVE QSPB-INSRATE-11-7 TO SPB-INSRATE(11 7).
           MOVE QSPB-INSTERM-11-8 TO SPB-INSTERM(11 8).
           MOVE QSPB-INSRATE-11-8 TO SPB-INSRATE(11 8).
           MOVE QSPB-INSTERM-11-9 TO SPB-INSTERM(11 9).
           MOVE QSPB-INSRATE-11-9 TO SPB-INSRATE(11 9).
           MOVE QSPB-INSTERM-11-10 TO SPB-INSTERM(11 10).
           MOVE QSPB-INSRATE-11-10 TO SPB-INSRATE(11 10).
           MOVE QSPB-INSTERM-11-11 TO SPB-INSTERM(11 11).
           MOVE QSPB-INSRATE-11-11 TO SPB-INSRATE(11 11).
           MOVE QSPB-INSTERM-11-12 TO SPB-INSTERM(11 12).
           MOVE QSPB-INSRATE-11-12 TO SPB-INSRATE(11 12).
           MOVE QSPB-INSTERM-11-13 TO SPB-INSTERM(11 13).
           MOVE QSPB-INSRATE-11-13 TO SPB-INSRATE(11 13).
           MOVE QSPB-INSTERM-11-14 TO SPB-INSTERM(11 14).
           MOVE QSPB-INSRATE-11-14 TO SPB-INSRATE(11 14).
           MOVE QSPB-INSTERM-11-15 TO SPB-INSTERM(11 15).
           MOVE QSPB-INSRATE-11-15 TO SPB-INSRATE(11 15).
           MOVE QSPB-INSTERM-11-16 TO SPB-INSTERM(11 16).
           MOVE QSPB-INSRATE-11-16 TO SPB-INSRATE(11 16).
           MOVE QSPB-INSTERM-11-17 TO SPB-INSTERM(11 17).
           MOVE QSPB-INSRATE-11-17 TO SPB-INSRATE(11 17).
           MOVE QSPB-INSTERM-11-18 TO SPB-INSTERM(11 18).
           MOVE QSPB-INSRATE-11-18 TO SPB-INSRATE(11 18).
           MOVE QSPB-INSTERM-11-19 TO SPB-INSTERM(11 19).
           MOVE QSPB-INSRATE-11-19 TO SPB-INSRATE(11 19).
           MOVE QSPB-INSTERM-11-20 TO SPB-INSTERM(11 20).
           MOVE QSPB-INSRATE-11-20 TO SPB-INSRATE(11 20).
           MOVE QSPB-INSMINPREM-11 TO SPB-INSMINPREM(11).
           MOVE QSPB-INSMINCHGABL-11 TO SPB-INSMINCHGABL(11).
           MOVE QSPB-INSMAXCHGABL-11 TO SPB-INSMAXCHGABL(11).
           MOVE QSPB-INSMAXBENIF-11 TO SPB-INSMAXBENIF(11).
           MOVE QSPB-INSMAXTERM-11 TO SPB-INSMAXTERM(11).
           MOVE QSPB-INSCHGBEN-11 TO SPB-INSCHGBEN(11).
           MOVE QSPB-INSEXCEPT-11 TO SPB-INSEXCEPT(11).
           MOVE QSPB-INSNOVER-11 TO SPB-INSNOVER(11).
           MOVE QSPB-INSMINAGE-11-1 TO SPB-INSMINAGE(11 1).
           MOVE QSPB-INSMAXAGE-11-1 TO SPB-INSMAXAGE(11 1).
           MOVE QSPB-INSMAXCOV-11-1 TO SPB-INSMAXCOV(11 1).
           MOVE QSPB-INSMAXTRM-11-1 TO SPB-INSMAXTRM(11 1).
           MOVE QSPB-INSMINAGE-11-2 TO SPB-INSMINAGE(11 2).
           MOVE QSPB-INSMAXAGE-11-2 TO SPB-INSMAXAGE(11 2).
           MOVE QSPB-INSMAXCOV-11-2 TO SPB-INSMAXCOV(11 2).
           MOVE QSPB-INSMAXTRM-11-2 TO SPB-INSMAXTRM(11 2).
           MOVE QSPB-INSMINAGE-11-3 TO SPB-INSMINAGE(11 3).
           MOVE QSPB-INSMAXAGE-11-3 TO SPB-INSMAXAGE(11 3).
           MOVE QSPB-INSMAXCOV-11-3 TO SPB-INSMAXCOV(11 3).
           MOVE QSPB-INSMAXTRM-11-3 TO SPB-INSMAXTRM(11 3).
           MOVE QSPB-INSMINAGE-11-4 TO SPB-INSMINAGE(11 4).
           MOVE QSPB-INSMAXAGE-11-4 TO SPB-INSMAXAGE(11 4).
           MOVE QSPB-INSMAXCOV-11-4 TO SPB-INSMAXCOV(11 4).
           MOVE QSPB-INSMAXTRM-11-4 TO SPB-INSMAXTRM(11 4).
           MOVE QSPB-INSMINAGE-11-5 TO SPB-INSMINAGE(11 5).
           MOVE QSPB-INSMAXAGE-11-5 TO SPB-INSMAXAGE(11 5).
           MOVE QSPB-INSMAXCOV-11-5 TO SPB-INSMAXCOV(11 5).
           MOVE QSPB-INSMAXTRM-11-5 TO SPB-INSMAXTRM(11 5).
           MOVE QSPB-INSFEECD-11 TO SPB-INSFEECD(11).
           MOVE QSPB-INSFEE-FRMLA-11 TO SPB-INSFEE-FRMLA(11).
           MOVE QSPB-INSCHGABL-FG-11 TO SPB-INSCHGABL-FG(11).
           MOVE QSPB-INSREJECT-FG-11 TO SPB-INSREJECT-FG(11).
           MOVE QSPB-INSFINCHG-11 TO SPB-INSFINCHG(11).
           MOVE QSPB-INSCOM-12 TO SPB-INSCOM(12).
           MOVE QSPB-INSRATECD-12 TO SPB-INSRATECD(12).
           MOVE QSPB-INSJFAC-12 TO SPB-INSJFAC(12).
           MOVE QSPB-INSDISCNT-12 TO SPB-INSDISCNT(12).
           MOVE QSPB-INSCHGABL-12 TO SPB-INSCHGABL(12).
           MOVE QSPB-INSTERM-12-1 TO SPB-INSTERM(12 1).
           MOVE QSPB-INSRATE-12-1 TO SPB-INSRATE(12 1).
           MOVE QSPB-INSTERM-12-2 TO SPB-INSTERM(12 2).
           MOVE QSPB-INSRATE-12-2 TO SPB-INSRATE(12 2).
           MOVE QSPB-INSTERM-12-3 TO SPB-INSTERM(12 3).
           MOVE QSPB-INSRATE-12-3 TO SPB-INSRATE(12 3).
           MOVE QSPB-INSTERM-12-4 TO SPB-INSTERM(12 4).
           MOVE QSPB-INSRATE-12-4 TO SPB-INSRATE(12 4).
           MOVE QSPB-INSTERM-12-5 TO SPB-INSTERM(12 5).
           MOVE QSPB-INSRATE-12-5 TO SPB-INSRATE(12 5).
           MOVE QSPB-INSTERM-12-6 TO SPB-INSTERM(12 6).
           MOVE QSPB-INSRATE-12-6 TO SPB-INSRATE(12 6).
           MOVE QSPB-INSTERM-12-7 TO SPB-INSTERM(12 7).
           MOVE QSPB-INSRATE-12-7 TO SPB-INSRATE(12 7).
           MOVE QSPB-INSTERM-12-8 TO SPB-INSTERM(12 8).
           MOVE QSPB-INSRATE-12-8 TO SPB-INSRATE(12 8).
           MOVE QSPB-INSTERM-12-9 TO SPB-INSTERM(12 9).
           MOVE QSPB-INSRATE-12-9 TO SPB-INSRATE(12 9).
           MOVE QSPB-INSTERM-12-10 TO SPB-INSTERM(12 10).
           MOVE QSPB-INSRATE-12-10 TO SPB-INSRATE(12 10).
           MOVE QSPB-INSTERM-12-11 TO SPB-INSTERM(12 11).
           MOVE QSPB-INSRATE-12-11 TO SPB-INSRATE(12 11).
           MOVE QSPB-INSTERM-12-12 TO SPB-INSTERM(12 12).
           MOVE QSPB-INSRATE-12-12 TO SPB-INSRATE(12 12).
           MOVE QSPB-INSTERM-12-13 TO SPB-INSTERM(12 13).
           MOVE QSPB-INSRATE-12-13 TO SPB-INSRATE(12 13).
           MOVE QSPB-INSTERM-12-14 TO SPB-INSTERM(12 14).
           MOVE QSPB-INSRATE-12-14 TO SPB-INSRATE(12 14).
           MOVE QSPB-INSTERM-12-15 TO SPB-INSTERM(12 15).
           MOVE QSPB-INSRATE-12-15 TO SPB-INSRATE(12 15).
           MOVE QSPB-INSTERM-12-16 TO SPB-INSTERM(12 16).
           MOVE QSPB-INSRATE-12-16 TO SPB-INSRATE(12 16).
           MOVE QSPB-INSTERM-12-17 TO SPB-INSTERM(12 17).
           MOVE QSPB-INSRATE-12-17 TO SPB-INSRATE(12 17).
           MOVE QSPB-INSTERM-12-18 TO SPB-INSTERM(12 18).
           MOVE QSPB-INSRATE-12-18 TO SPB-INSRATE(12 18).
           MOVE QSPB-INSTERM-12-19 TO SPB-INSTERM(12 19).
           MOVE QSPB-INSRATE-12-19 TO SPB-INSRATE(12 19).
           MOVE QSPB-INSTERM-12-20 TO SPB-INSTERM(12 20).
           MOVE QSPB-INSRATE-12-20 TO SPB-INSRATE(12 20).
           MOVE QSPB-INSMINPREM-12 TO SPB-INSMINPREM(12).
           MOVE QSPB-INSMINCHGABL-12 TO SPB-INSMINCHGABL(12).
           MOVE QSPB-INSMAXCHGABL-12 TO SPB-INSMAXCHGABL(12).
           MOVE QSPB-INSMAXBENIF-12 TO SPB-INSMAXBENIF(12).
           MOVE QSPB-INSMAXTERM-12 TO SPB-INSMAXTERM(12).
           MOVE QSPB-INSCHGBEN-12 TO SPB-INSCHGBEN(12).
           MOVE QSPB-INSEXCEPT-12 TO SPB-INSEXCEPT(12).
           MOVE QSPB-INSNOVER-12 TO SPB-INSNOVER(12).
           MOVE QSPB-INSMINAGE-12-1 TO SPB-INSMINAGE(12 1).
           MOVE QSPB-INSMAXAGE-12-1 TO SPB-INSMAXAGE(12 1).
           MOVE QSPB-INSMAXCOV-12-1 TO SPB-INSMAXCOV(12 1).
           MOVE QSPB-INSMAXTRM-12-1 TO SPB-INSMAXTRM(12 1).
           MOVE QSPB-INSMINAGE-12-2 TO SPB-INSMINAGE(12 2).
           MOVE QSPB-INSMAXAGE-12-2 TO SPB-INSMAXAGE(12 2).
           MOVE QSPB-INSMAXCOV-12-2 TO SPB-INSMAXCOV(12 2).
           MOVE QSPB-INSMAXTRM-12-2 TO SPB-INSMAXTRM(12 2).
           MOVE QSPB-INSMINAGE-12-3 TO SPB-INSMINAGE(12 3).
           MOVE QSPB-INSMAXAGE-12-3 TO SPB-INSMAXAGE(12 3).
           MOVE QSPB-INSMAXCOV-12-3 TO SPB-INSMAXCOV(12 3).
           MOVE QSPB-INSMAXTRM-12-3 TO SPB-INSMAXTRM(12 3).
           MOVE QSPB-INSMINAGE-12-4 TO SPB-INSMINAGE(12 4).
           MOVE QSPB-INSMAXAGE-12-4 TO SPB-INSMAXAGE(12 4).
           MOVE QSPB-INSMAXCOV-12-4 TO SPB-INSMAXCOV(12 4).
           MOVE QSPB-INSMAXTRM-12-4 TO SPB-INSMAXTRM(12 4).
           MOVE QSPB-INSMINAGE-12-5 TO SPB-INSMINAGE(12 5).
           MOVE QSPB-INSMAXAGE-12-5 TO SPB-INSMAXAGE(12 5).
           MOVE QSPB-INSMAXCOV-12-5 TO SPB-INSMAXCOV(12 5).
           MOVE QSPB-INSMAXTRM-12-5 TO SPB-INSMAXTRM(12 5).
           MOVE QSPB-INSFEECD-12 TO SPB-INSFEECD(12).
           MOVE QSPB-INSFEE-FRMLA-12 TO SPB-INSFEE-FRMLA(12).
           MOVE QSPB-INSCHGABL-FG-12 TO SPB-INSCHGABL-FG(12).
           MOVE QSPB-INSREJECT-FG-12 TO SPB-INSREJECT-FG(12).
           MOVE QSPB-INSFINCHG-12 TO SPB-INSFINCHG(12).
 
      *-----------------------------------------------------------------
      *    LOAD QSPB-REC FROM SPB-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-SPB-FIELDS.
           MOVE SPB-ORGST TO QSPB-ORGST.
           MOVE SPB-SPRCLASS TO QSPB-SPRCLASS.
           MOVE SPB-SUBCLASS TO QSPB-SUBCLASS.
           MOVE SPB-LAWCODE TO QSPB-LAWCODE.
           MOVE SPB-INSCOM(1) TO QSPB-INSCOM-1.
           MOVE SPB-INSRATECD(1) TO QSPB-INSRATECD-1.
           MOVE SPB-INSJFAC(1) TO QSPB-INSJFAC-1.
           MOVE SPB-INSDISCNT(1) TO QSPB-INSDISCNT-1.
           MOVE SPB-INSCHGABL(1) TO QSPB-INSCHGABL-1.
           MOVE SPB-INSTERM(1 1) TO QSPB-INSTERM-1-1.
           MOVE SPB-INSRATE(1 1) TO QSPB-INSRATE-1-1.
           MOVE SPB-INSTERM(1 2) TO QSPB-INSTERM-1-2.
           MOVE SPB-INSRATE(1 2) TO QSPB-INSRATE-1-2.
           MOVE SPB-INSTERM(1 3) TO QSPB-INSTERM-1-3.
           MOVE SPB-INSRATE(1 3) TO QSPB-INSRATE-1-3.
           MOVE SPB-INSTERM(1 4) TO QSPB-INSTERM-1-4.
           MOVE SPB-INSRATE(1 4) TO QSPB-INSRATE-1-4.
           MOVE SPB-INSTERM(1 5) TO QSPB-INSTERM-1-5.
           MOVE SPB-INSRATE(1 5) TO QSPB-INSRATE-1-5.
           MOVE SPB-INSTERM(1 6) TO QSPB-INSTERM-1-6.
           MOVE SPB-INSRATE(1 6) TO QSPB-INSRATE-1-6.
           MOVE SPB-INSTERM(1 7) TO QSPB-INSTERM-1-7.
           MOVE SPB-INSRATE(1 7) TO QSPB-INSRATE-1-7.
           MOVE SPB-INSTERM(1 8) TO QSPB-INSTERM-1-8.
           MOVE SPB-INSRATE(1 8) TO QSPB-INSRATE-1-8.
           MOVE SPB-INSTERM(1 9) TO QSPB-INSTERM-1-9.
           MOVE SPB-INSRATE(1 9) TO QSPB-INSRATE-1-9.
           MOVE SPB-INSTERM(1 10) TO QSPB-INSTERM-1-10.
           MOVE SPB-INSRATE(1 10) TO QSPB-INSRATE-1-10.
           MOVE SPB-INSTERM(1 11) TO QSPB-INSTERM-1-11.
           MOVE SPB-INSRATE(1 11) TO QSPB-INSRATE-1-11.
           MOVE SPB-INSTERM(1 12) TO QSPB-INSTERM-1-12.
           MOVE SPB-INSRATE(1 12) TO QSPB-INSRATE-1-12.
           MOVE SPB-INSTERM(1 13) TO QSPB-INSTERM-1-13.
           MOVE SPB-INSRATE(1 13) TO QSPB-INSRATE-1-13.
           MOVE SPB-INSTERM(1 14) TO QSPB-INSTERM-1-14.
           MOVE SPB-INSRATE(1 14) TO QSPB-INSRATE-1-14.
           MOVE SPB-INSTERM(1 15) TO QSPB-INSTERM-1-15.
           MOVE SPB-INSRATE(1 15) TO QSPB-INSRATE-1-15.
           MOVE SPB-INSTERM(1 16) TO QSPB-INSTERM-1-16.
           MOVE SPB-INSRATE(1 16) TO QSPB-INSRATE-1-16.
           MOVE SPB-INSTERM(1 17) TO QSPB-INSTERM-1-17.
           MOVE SPB-INSRATE(1 17) TO QSPB-INSRATE-1-17.
           MOVE SPB-INSTERM(1 18) TO QSPB-INSTERM-1-18.
           MOVE SPB-INSRATE(1 18) TO QSPB-INSRATE-1-18.
           MOVE SPB-INSTERM(1 19) TO QSPB-INSTERM-1-19.
           MOVE SPB-INSRATE(1 19) TO QSPB-INSRATE-1-19.
           MOVE SPB-INSTERM(1 20) TO QSPB-INSTERM-1-20.
           MOVE SPB-INSRATE(1 20) TO QSPB-INSRATE-1-20.
           MOVE SPB-INSMINPREM(1) TO QSPB-INSMINPREM-1.
           MOVE SPB-INSMINCHGABL(1) TO QSPB-INSMINCHGABL-1.
           MOVE SPB-INSMAXCHGABL(1) TO QSPB-INSMAXCHGABL-1.
           MOVE SPB-INSMAXBENIF(1) TO QSPB-INSMAXBENIF-1.
           MOVE SPB-INSMAXTERM(1) TO QSPB-INSMAXTERM-1.
           MOVE SPB-INSCHGBEN(1) TO QSPB-INSCHGBEN-1.
           MOVE SPB-INSEXCEPT(1) TO QSPB-INSEXCEPT-1.
           MOVE SPB-INSNOVER(1) TO QSPB-INSNOVER-1.
           MOVE SPB-INSMINAGE(1 1) TO QSPB-INSMINAGE-1-1.
           MOVE SPB-INSMAXAGE(1 1) TO QSPB-INSMAXAGE-1-1.
           MOVE SPB-INSMAXCOV(1 1) TO QSPB-INSMAXCOV-1-1.
           MOVE SPB-INSMAXTRM(1 1) TO QSPB-INSMAXTRM-1-1.
           MOVE SPB-INSMINAGE(1 2) TO QSPB-INSMINAGE-1-2.
           MOVE SPB-INSMAXAGE(1 2) TO QSPB-INSMAXAGE-1-2.
           MOVE SPB-INSMAXCOV(1 2) TO QSPB-INSMAXCOV-1-2.
           MOVE SPB-INSMAXTRM(1 2) TO QSPB-INSMAXTRM-1-2.
           MOVE SPB-INSMINAGE(1 3) TO QSPB-INSMINAGE-1-3.
           MOVE SPB-INSMAXAGE(1 3) TO QSPB-INSMAXAGE-1-3.
           MOVE SPB-INSMAXCOV(1 3) TO QSPB-INSMAXCOV-1-3.
           MOVE SPB-INSMAXTRM(1 3) TO QSPB-INSMAXTRM-1-3.
           MOVE SPB-INSMINAGE(1 4) TO QSPB-INSMINAGE-1-4.
           MOVE SPB-INSMAXAGE(1 4) TO QSPB-INSMAXAGE-1-4.
           MOVE SPB-INSMAXCOV(1 4) TO QSPB-INSMAXCOV-1-4.
           MOVE SPB-INSMAXTRM(1 4) TO QSPB-INSMAXTRM-1-4.
           MOVE SPB-INSMINAGE(1 5) TO QSPB-INSMINAGE-1-5.
           MOVE SPB-INSMAXAGE(1 5) TO QSPB-INSMAXAGE-1-5.
           MOVE SPB-INSMAXCOV(1 5) TO QSPB-INSMAXCOV-1-5.
           MOVE SPB-INSMAXTRM(1 5) TO QSPB-INSMAXTRM-1-5.
           MOVE SPB-INSFEECD(1) TO QSPB-INSFEECD-1.
           MOVE SPB-INSFEE-FRMLA(1) TO QSPB-INSFEE-FRMLA-1.
           MOVE SPB-INSCHGABL-FG(1) TO QSPB-INSCHGABL-FG-1.
           MOVE SPB-INSREJECT-FG(1) TO QSPB-INSREJECT-FG-1.
           MOVE SPB-INSFINCHG(1) TO QSPB-INSFINCHG-1.
           MOVE SPB-INSCOM(2) TO QSPB-INSCOM-2.
           MOVE SPB-INSRATECD(2) TO QSPB-INSRATECD-2.
           MOVE SPB-INSJFAC(2) TO QSPB-INSJFAC-2.
           MOVE SPB-INSDISCNT(2) TO QSPB-INSDISCNT-2.
           MOVE SPB-INSCHGABL(2) TO QSPB-INSCHGABL-2.
           MOVE SPB-INSTERM(2 1) TO QSPB-INSTERM-2-1.
           MOVE SPB-INSRATE(2 1) TO QSPB-INSRATE-2-1.
           MOVE SPB-INSTERM(2 2) TO QSPB-INSTERM-2-2.
           MOVE SPB-INSRATE(2 2) TO QSPB-INSRATE-2-2.
           MOVE SPB-INSTERM(2 3) TO QSPB-INSTERM-2-3.
           MOVE SPB-INSRATE(2 3) TO QSPB-INSRATE-2-3.
           MOVE SPB-INSTERM(2 4) TO QSPB-INSTERM-2-4.
           MOVE SPB-INSRATE(2 4) TO QSPB-INSRATE-2-4.
           MOVE SPB-INSTERM(2 5) TO QSPB-INSTERM-2-5.
           MOVE SPB-INSRATE(2 5) TO QSPB-INSRATE-2-5.
           MOVE SPB-INSTERM(2 6) TO QSPB-INSTERM-2-6.
           MOVE SPB-INSRATE(2 6) TO QSPB-INSRATE-2-6.
           MOVE SPB-INSTERM(2 7) TO QSPB-INSTERM-2-7.
           MOVE SPB-INSRATE(2 7) TO QSPB-INSRATE-2-7.
           MOVE SPB-INSTERM(2 8) TO QSPB-INSTERM-2-8.
           MOVE SPB-INSRATE(2 8) TO QSPB-INSRATE-2-8.
           MOVE SPB-INSTERM(2 9) TO QSPB-INSTERM-2-9.
           MOVE SPB-INSRATE(2 9) TO QSPB-INSRATE-2-9.
           MOVE SPB-INSTERM(2 10) TO QSPB-INSTERM-2-10.
           MOVE SPB-INSRATE(2 10) TO QSPB-INSRATE-2-10.
           MOVE SPB-INSTERM(2 11) TO QSPB-INSTERM-2-11.
           MOVE SPB-INSRATE(2 11) TO QSPB-INSRATE-2-11.
           MOVE SPB-INSTERM(2 12) TO QSPB-INSTERM-2-12.
           MOVE SPB-INSRATE(2 12) TO QSPB-INSRATE-2-12.
           MOVE SPB-INSTERM(2 13) TO QSPB-INSTERM-2-13.
           MOVE SPB-INSRATE(2 13) TO QSPB-INSRATE-2-13.
           MOVE SPB-INSTERM(2 14) TO QSPB-INSTERM-2-14.
           MOVE SPB-INSRATE(2 14) TO QSPB-INSRATE-2-14.
           MOVE SPB-INSTERM(2 15) TO QSPB-INSTERM-2-15.
           MOVE SPB-INSRATE(2 15) TO QSPB-INSRATE-2-15.
           MOVE SPB-INSTERM(2 16) TO QSPB-INSTERM-2-16.
           MOVE SPB-INSRATE(2 16) TO QSPB-INSRATE-2-16.
           MOVE SPB-INSTERM(2 17) TO QSPB-INSTERM-2-17.
           MOVE SPB-INSRATE(2 17) TO QSPB-INSRATE-2-17.
           MOVE SPB-INSTERM(2 18) TO QSPB-INSTERM-2-18.
           MOVE SPB-INSRATE(2 18) TO QSPB-INSRATE-2-18.
           MOVE SPB-INSTERM(2 19) TO QSPB-INSTERM-2-19.
           MOVE SPB-INSRATE(2 19) TO QSPB-INSRATE-2-19.
           MOVE SPB-INSTERM(2 20) TO QSPB-INSTERM-2-20.
           MOVE SPB-INSRATE(2 20) TO QSPB-INSRATE-2-20.
           MOVE SPB-INSMINPREM(2) TO QSPB-INSMINPREM-2.
           MOVE SPB-INSMINCHGABL(2) TO QSPB-INSMINCHGABL-2.
           MOVE SPB-INSMAXCHGABL(2) TO QSPB-INSMAXCHGABL-2.
           MOVE SPB-INSMAXBENIF(2) TO QSPB-INSMAXBENIF-2.
           MOVE SPB-INSMAXTERM(2) TO QSPB-INSMAXTERM-2.
           MOVE SPB-INSCHGBEN(2) TO QSPB-INSCHGBEN-2.
           MOVE SPB-INSEXCEPT(2) TO QSPB-INSEXCEPT-2.
           MOVE SPB-INSNOVER(2) TO QSPB-INSNOVER-2.
           MOVE SPB-INSMINAGE(2 1) TO QSPB-INSMINAGE-2-1.
           MOVE SPB-INSMAXAGE(2 1) TO QSPB-INSMAXAGE-2-1.
           MOVE SPB-INSMAXCOV(2 1) TO QSPB-INSMAXCOV-2-1.
           MOVE SPB-INSMAXTRM(2 1) TO QSPB-INSMAXTRM-2-1.
           MOVE SPB-INSMINAGE(2 2) TO QSPB-INSMINAGE-2-2.
           MOVE SPB-INSMAXAGE(2 2) TO QSPB-INSMAXAGE-2-2.
           MOVE SPB-INSMAXCOV(2 2) TO QSPB-INSMAXCOV-2-2.
           MOVE SPB-INSMAXTRM(2 2) TO QSPB-INSMAXTRM-2-2.
           MOVE SPB-INSMINAGE(2 3) TO QSPB-INSMINAGE-2-3.
           MOVE SPB-INSMAXAGE(2 3) TO QSPB-INSMAXAGE-2-3.
           MOVE SPB-INSMAXCOV(2 3) TO QSPB-INSMAXCOV-2-3.
           MOVE SPB-INSMAXTRM(2 3) TO QSPB-INSMAXTRM-2-3.
           MOVE SPB-INSMINAGE(2 4) TO QSPB-INSMINAGE-2-4.
           MOVE SPB-INSMAXAGE(2 4) TO QSPB-INSMAXAGE-2-4.
           MOVE SPB-INSMAXCOV(2 4) TO QSPB-INSMAXCOV-2-4.
           MOVE SPB-INSMAXTRM(2 4) TO QSPB-INSMAXTRM-2-4.
           MOVE SPB-INSMINAGE(2 5) TO QSPB-INSMINAGE-2-5.
           MOVE SPB-INSMAXAGE(2 5) TO QSPB-INSMAXAGE-2-5.
           MOVE SPB-INSMAXCOV(2 5) TO QSPB-INSMAXCOV-2-5.
           MOVE SPB-INSMAXTRM(2 5) TO QSPB-INSMAXTRM-2-5.
           MOVE SPB-INSFEECD(2) TO QSPB-INSFEECD-2.
           MOVE SPB-INSFEE-FRMLA(2) TO QSPB-INSFEE-FRMLA-2.
           MOVE SPB-INSCHGABL-FG(2) TO QSPB-INSCHGABL-FG-2.
           MOVE SPB-INSREJECT-FG(2) TO QSPB-INSREJECT-FG-2.
           MOVE SPB-INSFINCHG(2) TO QSPB-INSFINCHG-2.
           MOVE SPB-INSCOM(3) TO QSPB-INSCOM-3.
           MOVE SPB-INSRATECD(3) TO QSPB-INSRATECD-3.
           MOVE SPB-INSJFAC(3) TO QSPB-INSJFAC-3.
           MOVE SPB-INSDISCNT(3) TO QSPB-INSDISCNT-3.
           MOVE SPB-INSCHGABL(3) TO QSPB-INSCHGABL-3.
           MOVE SPB-INSTERM(3 1) TO QSPB-INSTERM-3-1.
           MOVE SPB-INSRATE(3 1) TO QSPB-INSRATE-3-1.
           MOVE SPB-INSTERM(3 2) TO QSPB-INSTERM-3-2.
           MOVE SPB-INSRATE(3 2) TO QSPB-INSRATE-3-2.
           MOVE SPB-INSTERM(3 3) TO QSPB-INSTERM-3-3.
           MOVE SPB-INSRATE(3 3) TO QSPB-INSRATE-3-3.
           MOVE SPB-INSTERM(3 4) TO QSPB-INSTERM-3-4.
           MOVE SPB-INSRATE(3 4) TO QSPB-INSRATE-3-4.
           MOVE SPB-INSTERM(3 5) TO QSPB-INSTERM-3-5.
           MOVE SPB-INSRATE(3 5) TO QSPB-INSRATE-3-5.
           MOVE SPB-INSTERM(3 6) TO QSPB-INSTERM-3-6.
           MOVE SPB-INSRATE(3 6) TO QSPB-INSRATE-3-6.
           MOVE SPB-INSTERM(3 7) TO QSPB-INSTERM-3-7.
           MOVE SPB-INSRATE(3 7) TO QSPB-INSRATE-3-7.
           MOVE SPB-INSTERM(3 8) TO QSPB-INSTERM-3-8.
           MOVE SPB-INSRATE(3 8) TO QSPB-INSRATE-3-8.
           MOVE SPB-INSTERM(3 9) TO QSPB-INSTERM-3-9.
           MOVE SPB-INSRATE(3 9) TO QSPB-INSRATE-3-9.
           MOVE SPB-INSTERM(3 10) TO QSPB-INSTERM-3-10.
           MOVE SPB-INSRATE(3 10) TO QSPB-INSRATE-3-10.
           MOVE SPB-INSTERM(3 11) TO QSPB-INSTERM-3-11.
           MOVE SPB-INSRATE(3 11) TO QSPB-INSRATE-3-11.
           MOVE SPB-INSTERM(3 12) TO QSPB-INSTERM-3-12.
           MOVE SPB-INSRATE(3 12) TO QSPB-INSRATE-3-12.
           MOVE SPB-INSTERM(3 13) TO QSPB-INSTERM-3-13.
           MOVE SPB-INSRATE(3 13) TO QSPB-INSRATE-3-13.
           MOVE SPB-INSTERM(3 14) TO QSPB-INSTERM-3-14.
           MOVE SPB-INSRATE(3 14) TO QSPB-INSRATE-3-14.
           MOVE SPB-INSTERM(3 15) TO QSPB-INSTERM-3-15.
           MOVE SPB-INSRATE(3 15) TO QSPB-INSRATE-3-15.
           MOVE SPB-INSTERM(3 16) TO QSPB-INSTERM-3-16.
           MOVE SPB-INSRATE(3 16) TO QSPB-INSRATE-3-16.
           MOVE SPB-INSTERM(3 17) TO QSPB-INSTERM-3-17.
           MOVE SPB-INSRATE(3 17) TO QSPB-INSRATE-3-17.
           MOVE SPB-INSTERM(3 18) TO QSPB-INSTERM-3-18.
           MOVE SPB-INSRATE(3 18) TO QSPB-INSRATE-3-18.
           MOVE SPB-INSTERM(3 19) TO QSPB-INSTERM-3-19.
           MOVE SPB-INSRATE(3 19) TO QSPB-INSRATE-3-19.
           MOVE SPB-INSTERM(3 20) TO QSPB-INSTERM-3-20.
           MOVE SPB-INSRATE(3 20) TO QSPB-INSRATE-3-20.
           MOVE SPB-INSMINPREM(3) TO QSPB-INSMINPREM-3.
           MOVE SPB-INSMINCHGABL(3) TO QSPB-INSMINCHGABL-3.
           MOVE SPB-INSMAXCHGABL(3) TO QSPB-INSMAXCHGABL-3.
           MOVE SPB-INSMAXBENIF(3) TO QSPB-INSMAXBENIF-3.
           MOVE SPB-INSMAXTERM(3) TO QSPB-INSMAXTERM-3.
           MOVE SPB-INSCHGBEN(3) TO QSPB-INSCHGBEN-3.
           MOVE SPB-INSEXCEPT(3) TO QSPB-INSEXCEPT-3.
           MOVE SPB-INSNOVER(3) TO QSPB-INSNOVER-3.
           MOVE SPB-INSMINAGE(3 1) TO QSPB-INSMINAGE-3-1.
           MOVE SPB-INSMAXAGE(3 1) TO QSPB-INSMAXAGE-3-1.
           MOVE SPB-INSMAXCOV(3 1) TO QSPB-INSMAXCOV-3-1.
           MOVE SPB-INSMAXTRM(3 1) TO QSPB-INSMAXTRM-3-1.
           MOVE SPB-INSMINAGE(3 2) TO QSPB-INSMINAGE-3-2.
           MOVE SPB-INSMAXAGE(3 2) TO QSPB-INSMAXAGE-3-2.
           MOVE SPB-INSMAXCOV(3 2) TO QSPB-INSMAXCOV-3-2.
           MOVE SPB-INSMAXTRM(3 2) TO QSPB-INSMAXTRM-3-2.
           MOVE SPB-INSMINAGE(3 3) TO QSPB-INSMINAGE-3-3.
           MOVE SPB-INSMAXAGE(3 3) TO QSPB-INSMAXAGE-3-3.
           MOVE SPB-INSMAXCOV(3 3) TO QSPB-INSMAXCOV-3-3.
           MOVE SPB-INSMAXTRM(3 3) TO QSPB-INSMAXTRM-3-3.
           MOVE SPB-INSMINAGE(3 4) TO QSPB-INSMINAGE-3-4.
           MOVE SPB-INSMAXAGE(3 4) TO QSPB-INSMAXAGE-3-4.
           MOVE SPB-INSMAXCOV(3 4) TO QSPB-INSMAXCOV-3-4.
           MOVE SPB-INSMAXTRM(3 4) TO QSPB-INSMAXTRM-3-4.
           MOVE SPB-INSMINAGE(3 5) TO QSPB-INSMINAGE-3-5.
           MOVE SPB-INSMAXAGE(3 5) TO QSPB-INSMAXAGE-3-5.
           MOVE SPB-INSMAXCOV(3 5) TO QSPB-INSMAXCOV-3-5.
           MOVE SPB-INSMAXTRM(3 5) TO QSPB-INSMAXTRM-3-5.
           MOVE SPB-INSFEECD(3) TO QSPB-INSFEECD-3.
           MOVE SPB-INSFEE-FRMLA(3) TO QSPB-INSFEE-FRMLA-3.
           MOVE SPB-INSCHGABL-FG(3) TO QSPB-INSCHGABL-FG-3.
           MOVE SPB-INSREJECT-FG(3) TO QSPB-INSREJECT-FG-3.
           MOVE SPB-INSFINCHG(3) TO QSPB-INSFINCHG-3.
           MOVE SPB-INSCOM(4) TO QSPB-INSCOM-4.
           MOVE SPB-INSRATECD(4) TO QSPB-INSRATECD-4.
           MOVE SPB-INSJFAC(4) TO QSPB-INSJFAC-4.
           MOVE SPB-INSDISCNT(4) TO QSPB-INSDISCNT-4.
           MOVE SPB-INSCHGABL(4) TO QSPB-INSCHGABL-4.
           MOVE SPB-INSTERM(4 1) TO QSPB-INSTERM-4-1.
           MOVE SPB-INSRATE(4 1) TO QSPB-INSRATE-4-1.
           MOVE SPB-INSTERM(4 2) TO QSPB-INSTERM-4-2.
           MOVE SPB-INSRATE(4 2) TO QSPB-INSRATE-4-2.
           MOVE SPB-INSTERM(4 3) TO QSPB-INSTERM-4-3.
           MOVE SPB-INSRATE(4 3) TO QSPB-INSRATE-4-3.
           MOVE SPB-INSTERM(4 4) TO QSPB-INSTERM-4-4.
           MOVE SPB-INSRATE(4 4) TO QSPB-INSRATE-4-4.
           MOVE SPB-INSTERM(4 5) TO QSPB-INSTERM-4-5.
           MOVE SPB-INSRATE(4 5) TO QSPB-INSRATE-4-5.
           MOVE SPB-INSTERM(4 6) TO QSPB-INSTERM-4-6.
           MOVE SPB-INSRATE(4 6) TO QSPB-INSRATE-4-6.
           MOVE SPB-INSTERM(4 7) TO QSPB-INSTERM-4-7.
           MOVE SPB-INSRATE(4 7) TO QSPB-INSRATE-4-7.
           MOVE SPB-INSTERM(4 8) TO QSPB-INSTERM-4-8.
           MOVE SPB-INSRATE(4 8) TO QSPB-INSRATE-4-8.
           MOVE SPB-INSTERM(4 9) TO QSPB-INSTERM-4-9.
           MOVE SPB-INSRATE(4 9) TO QSPB-INSRATE-4-9.
           MOVE SPB-INSTERM(4 10) TO QSPB-INSTERM-4-10.
           MOVE SPB-INSRATE(4 10) TO QSPB-INSRATE-4-10.
           MOVE SPB-INSTERM(4 11) TO QSPB-INSTERM-4-11.
           MOVE SPB-INSRATE(4 11) TO QSPB-INSRATE-4-11.
           MOVE SPB-INSTERM(4 12) TO QSPB-INSTERM-4-12.
           MOVE SPB-INSRATE(4 12) TO QSPB-INSRATE-4-12.
           MOVE SPB-INSTERM(4 13) TO QSPB-INSTERM-4-13.
           MOVE SPB-INSRATE(4 13) TO QSPB-INSRATE-4-13.
           MOVE SPB-INSTERM(4 14) TO QSPB-INSTERM-4-14.
           MOVE SPB-INSRATE(4 14) TO QSPB-INSRATE-4-14.
           MOVE SPB-INSTERM(4 15) TO QSPB-INSTERM-4-15.
           MOVE SPB-INSRATE(4 15) TO QSPB-INSRATE-4-15.
           MOVE SPB-INSTERM(4 16) TO QSPB-INSTERM-4-16.
           MOVE SPB-INSRATE(4 16) TO QSPB-INSRATE-4-16.
           MOVE SPB-INSTERM(4 17) TO QSPB-INSTERM-4-17.
           MOVE SPB-INSRATE(4 17) TO QSPB-INSRATE-4-17.
           MOVE SPB-INSTERM(4 18) TO QSPB-INSTERM-4-18.
           MOVE SPB-INSRATE(4 18) TO QSPB-INSRATE-4-18.
           MOVE SPB-INSTERM(4 19) TO QSPB-INSTERM-4-19.
           MOVE SPB-INSRATE(4 19) TO QSPB-INSRATE-4-19.
           MOVE SPB-INSTERM(4 20) TO QSPB-INSTERM-4-20.
           MOVE SPB-INSRATE(4 20) TO QSPB-INSRATE-4-20.
           MOVE SPB-INSMINPREM(4) TO QSPB-INSMINPREM-4.
           MOVE SPB-INSMINCHGABL(4) TO QSPB-INSMINCHGABL-4.
           MOVE SPB-INSMAXCHGABL(4) TO QSPB-INSMAXCHGABL-4.
           MOVE SPB-INSMAXBENIF(4) TO QSPB-INSMAXBENIF-4.
           MOVE SPB-INSMAXTERM(4) TO QSPB-INSMAXTERM-4.
           MOVE SPB-INSCHGBEN(4) TO QSPB-INSCHGBEN-4.
           MOVE SPB-INSEXCEPT(4) TO QSPB-INSEXCEPT-4.
           MOVE SPB-INSNOVER(4) TO QSPB-INSNOVER-4.
           MOVE SPB-INSMINAGE(4 1) TO QSPB-INSMINAGE-4-1.
           MOVE SPB-INSMAXAGE(4 1) TO QSPB-INSMAXAGE-4-1.
           MOVE SPB-INSMAXCOV(4 1) TO QSPB-INSMAXCOV-4-1.
           MOVE SPB-INSMAXTRM(4 1) TO QSPB-INSMAXTRM-4-1.
           MOVE SPB-INSMINAGE(4 2) TO QSPB-INSMINAGE-4-2.
           MOVE SPB-INSMAXAGE(4 2) TO QSPB-INSMAXAGE-4-2.
           MOVE SPB-INSMAXCOV(4 2) TO QSPB-INSMAXCOV-4-2.
           MOVE SPB-INSMAXTRM(4 2) TO QSPB-INSMAXTRM-4-2.
           MOVE SPB-INSMINAGE(4 3) TO QSPB-INSMINAGE-4-3.
           MOVE SPB-INSMAXAGE(4 3) TO QSPB-INSMAXAGE-4-3.
           MOVE SPB-INSMAXCOV(4 3) TO QSPB-INSMAXCOV-4-3.
           MOVE SPB-INSMAXTRM(4 3) TO QSPB-INSMAXTRM-4-3.
           MOVE SPB-INSMINAGE(4 4) TO QSPB-INSMINAGE-4-4.
           MOVE SPB-INSMAXAGE(4 4) TO QSPB-INSMAXAGE-4-4.
           MOVE SPB-INSMAXCOV(4 4) TO QSPB-INSMAXCOV-4-4.
           MOVE SPB-INSMAXTRM(4 4) TO QSPB-INSMAXTRM-4-4.
           MOVE SPB-INSMINAGE(4 5) TO QSPB-INSMINAGE-4-5.
           MOVE SPB-INSMAXAGE(4 5) TO QSPB-INSMAXAGE-4-5.
           MOVE SPB-INSMAXCOV(4 5) TO QSPB-INSMAXCOV-4-5.
           MOVE SPB-INSMAXTRM(4 5) TO QSPB-INSMAXTRM-4-5.
           MOVE SPB-INSFEECD(4) TO QSPB-INSFEECD-4.
           MOVE SPB-INSFEE-FRMLA(4) TO QSPB-INSFEE-FRMLA-4.
           MOVE SPB-INSCHGABL-FG(4) TO QSPB-INSCHGABL-FG-4.
           MOVE SPB-INSREJECT-FG(4) TO QSPB-INSREJECT-FG-4.
           MOVE SPB-INSFINCHG(4) TO QSPB-INSFINCHG-4.
           MOVE SPB-INSCOM(5) TO QSPB-INSCOM-5.
           MOVE SPB-INSRATECD(5) TO QSPB-INSRATECD-5.
           MOVE SPB-INSJFAC(5) TO QSPB-INSJFAC-5.
           MOVE SPB-INSDISCNT(5) TO QSPB-INSDISCNT-5.
           MOVE SPB-INSCHGABL(5) TO QSPB-INSCHGABL-5.
           MOVE SPB-INSTERM(5 1) TO QSPB-INSTERM-5-1.
           MOVE SPB-INSRATE(5 1) TO QSPB-INSRATE-5-1.
           MOVE SPB-INSTERM(5 2) TO QSPB-INSTERM-5-2.
           MOVE SPB-INSRATE(5 2) TO QSPB-INSRATE-5-2.
           MOVE SPB-INSTERM(5 3) TO QSPB-INSTERM-5-3.
           MOVE SPB-INSRATE(5 3) TO QSPB-INSRATE-5-3.
           MOVE SPB-INSTERM(5 4) TO QSPB-INSTERM-5-4.
           MOVE SPB-INSRATE(5 4) TO QSPB-INSRATE-5-4.
           MOVE SPB-INSTERM(5 5) TO QSPB-INSTERM-5-5.
           MOVE SPB-INSRATE(5 5) TO QSPB-INSRATE-5-5.
           MOVE SPB-INSTERM(5 6) TO QSPB-INSTERM-5-6.
           MOVE SPB-INSRATE(5 6) TO QSPB-INSRATE-5-6.
           MOVE SPB-INSTERM(5 7) TO QSPB-INSTERM-5-7.
           MOVE SPB-INSRATE(5 7) TO QSPB-INSRATE-5-7.
           MOVE SPB-INSTERM(5 8) TO QSPB-INSTERM-5-8.
           MOVE SPB-INSRATE(5 8) TO QSPB-INSRATE-5-8.
           MOVE SPB-INSTERM(5 9) TO QSPB-INSTERM-5-9.
           MOVE SPB-INSRATE(5 9) TO QSPB-INSRATE-5-9.
           MOVE SPB-INSTERM(5 10) TO QSPB-INSTERM-5-10.
           MOVE SPB-INSRATE(5 10) TO QSPB-INSRATE-5-10.
           MOVE SPB-INSTERM(5 11) TO QSPB-INSTERM-5-11.
           MOVE SPB-INSRATE(5 11) TO QSPB-INSRATE-5-11.
           MOVE SPB-INSTERM(5 12) TO QSPB-INSTERM-5-12.
           MOVE SPB-INSRATE(5 12) TO QSPB-INSRATE-5-12.
           MOVE SPB-INSTERM(5 13) TO QSPB-INSTERM-5-13.
           MOVE SPB-INSRATE(5 13) TO QSPB-INSRATE-5-13.
           MOVE SPB-INSTERM(5 14) TO QSPB-INSTERM-5-14.
           MOVE SPB-INSRATE(5 14) TO QSPB-INSRATE-5-14.
           MOVE SPB-INSTERM(5 15) TO QSPB-INSTERM-5-15.
           MOVE SPB-INSRATE(5 15) TO QSPB-INSRATE-5-15.
           MOVE SPB-INSTERM(5 16) TO QSPB-INSTERM-5-16.
           MOVE SPB-INSRATE(5 16) TO QSPB-INSRATE-5-16.
           MOVE SPB-INSTERM(5 17) TO QSPB-INSTERM-5-17.
           MOVE SPB-INSRATE(5 17) TO QSPB-INSRATE-5-17.
           MOVE SPB-INSTERM(5 18) TO QSPB-INSTERM-5-18.
           MOVE SPB-INSRATE(5 18) TO QSPB-INSRATE-5-18.
           MOVE SPB-INSTERM(5 19) TO QSPB-INSTERM-5-19.
           MOVE SPB-INSRATE(5 19) TO QSPB-INSRATE-5-19.
           MOVE SPB-INSTERM(5 20) TO QSPB-INSTERM-5-20.
           MOVE SPB-INSRATE(5 20) TO QSPB-INSRATE-5-20.
           MOVE SPB-INSMINPREM(5) TO QSPB-INSMINPREM-5.
           MOVE SPB-INSMINCHGABL(5) TO QSPB-INSMINCHGABL-5.
           MOVE SPB-INSMAXCHGABL(5) TO QSPB-INSMAXCHGABL-5.
           MOVE SPB-INSMAXBENIF(5) TO QSPB-INSMAXBENIF-5.
           MOVE SPB-INSMAXTERM(5) TO QSPB-INSMAXTERM-5.
           MOVE SPB-INSCHGBEN(5) TO QSPB-INSCHGBEN-5.
           MOVE SPB-INSEXCEPT(5) TO QSPB-INSEXCEPT-5.
           MOVE SPB-INSNOVER(5) TO QSPB-INSNOVER-5.
           MOVE SPB-INSMINAGE(5 1) TO QSPB-INSMINAGE-5-1.
           MOVE SPB-INSMAXAGE(5 1) TO QSPB-INSMAXAGE-5-1.
           MOVE SPB-INSMAXCOV(5 1) TO QSPB-INSMAXCOV-5-1.
           MOVE SPB-INSMAXTRM(5 1) TO QSPB-INSMAXTRM-5-1.
           MOVE SPB-INSMINAGE(5 2) TO QSPB-INSMINAGE-5-2.
           MOVE SPB-INSMAXAGE(5 2) TO QSPB-INSMAXAGE-5-2.
           MOVE SPB-INSMAXCOV(5 2) TO QSPB-INSMAXCOV-5-2.
           MOVE SPB-INSMAXTRM(5 2) TO QSPB-INSMAXTRM-5-2.
           MOVE SPB-INSMINAGE(5 3) TO QSPB-INSMINAGE-5-3.
           MOVE SPB-INSMAXAGE(5 3) TO QSPB-INSMAXAGE-5-3.
           MOVE SPB-INSMAXCOV(5 3) TO QSPB-INSMAXCOV-5-3.
           MOVE SPB-INSMAXTRM(5 3) TO QSPB-INSMAXTRM-5-3.
           MOVE SPB-INSMINAGE(5 4) TO QSPB-INSMINAGE-5-4.
           MOVE SPB-INSMAXAGE(5 4) TO QSPB-INSMAXAGE-5-4.
           MOVE SPB-INSMAXCOV(5 4) TO QSPB-INSMAXCOV-5-4.
           MOVE SPB-INSMAXTRM(5 4) TO QSPB-INSMAXTRM-5-4.
           MOVE SPB-INSMINAGE(5 5) TO QSPB-INSMINAGE-5-5.
           MOVE SPB-INSMAXAGE(5 5) TO QSPB-INSMAXAGE-5-5.
           MOVE SPB-INSMAXCOV(5 5) TO QSPB-INSMAXCOV-5-5.
           MOVE SPB-INSMAXTRM(5 5) TO QSPB-INSMAXTRM-5-5.
           MOVE SPB-INSFEECD(5) TO QSPB-INSFEECD-5.
           MOVE SPB-INSFEE-FRMLA(5) TO QSPB-INSFEE-FRMLA-5.
           MOVE SPB-INSCHGABL-FG(5) TO QSPB-INSCHGABL-FG-5.
           MOVE SPB-INSREJECT-FG(5) TO QSPB-INSREJECT-FG-5.
           MOVE SPB-INSFINCHG(5) TO QSPB-INSFINCHG-5.
           MOVE SPB-INSCOM(6) TO QSPB-INSCOM-6.
           MOVE SPB-INSRATECD(6) TO QSPB-INSRATECD-6.
           MOVE SPB-INSJFAC(6) TO QSPB-INSJFAC-6.
           MOVE SPB-INSDISCNT(6) TO QSPB-INSDISCNT-6.
           MOVE SPB-INSCHGABL(6) TO QSPB-INSCHGABL-6.
           MOVE SPB-INSTERM(6 1) TO QSPB-INSTERM-6-1.
           MOVE SPB-INSRATE(6 1) TO QSPB-INSRATE-6-1.
           MOVE SPB-INSTERM(6 2) TO QSPB-INSTERM-6-2.
           MOVE SPB-INSRATE(6 2) TO QSPB-INSRATE-6-2.
           MOVE SPB-INSTERM(6 3) TO QSPB-INSTERM-6-3.
           MOVE SPB-INSRATE(6 3) TO QSPB-INSRATE-6-3.
           MOVE SPB-INSTERM(6 4) TO QSPB-INSTERM-6-4.
           MOVE SPB-INSRATE(6 4) TO QSPB-INSRATE-6-4.
           MOVE SPB-INSTERM(6 5) TO QSPB-INSTERM-6-5.
           MOVE SPB-INSRATE(6 5) TO QSPB-INSRATE-6-5.
           MOVE SPB-INSTERM(6 6) TO QSPB-INSTERM-6-6.
           MOVE SPB-INSRATE(6 6) TO QSPB-INSRATE-6-6.
           MOVE SPB-INSTERM(6 7) TO QSPB-INSTERM-6-7.
           MOVE SPB-INSRATE(6 7) TO QSPB-INSRATE-6-7.
           MOVE SPB-INSTERM(6 8) TO QSPB-INSTERM-6-8.
           MOVE SPB-INSRATE(6 8) TO QSPB-INSRATE-6-8.
           MOVE SPB-INSTERM(6 9) TO QSPB-INSTERM-6-9.
           MOVE SPB-INSRATE(6 9) TO QSPB-INSRATE-6-9.
           MOVE SPB-INSTERM(6 10) TO QSPB-INSTERM-6-10.
           MOVE SPB-INSRATE(6 10) TO QSPB-INSRATE-6-10.
           MOVE SPB-INSTERM(6 11) TO QSPB-INSTERM-6-11.
           MOVE SPB-INSRATE(6 11) TO QSPB-INSRATE-6-11.
           MOVE SPB-INSTERM(6 12) TO QSPB-INSTERM-6-12.
           MOVE SPB-INSRATE(6 12) TO QSPB-INSRATE-6-12.
           MOVE SPB-INSTERM(6 13) TO QSPB-INSTERM-6-13.
           MOVE SPB-INSRATE(6 13) TO QSPB-INSRATE-6-13.
           MOVE SPB-INSTERM(6 14) TO QSPB-INSTERM-6-14.
           MOVE SPB-INSRATE(6 14) TO QSPB-INSRATE-6-14.
           MOVE SPB-INSTERM(6 15) TO QSPB-INSTERM-6-15.
           MOVE SPB-INSRATE(6 15) TO QSPB-INSRATE-6-15.
           MOVE SPB-INSTERM(6 16) TO QSPB-INSTERM-6-16.
           MOVE SPB-INSRATE(6 16) TO QSPB-INSRATE-6-16.
           MOVE SPB-INSTERM(6 17) TO QSPB-INSTERM-6-17.
           MOVE SPB-INSRATE(6 17) TO QSPB-INSRATE-6-17.
           MOVE SPB-INSTERM(6 18) TO QSPB-INSTERM-6-18.
           MOVE SPB-INSRATE(6 18) TO QSPB-INSRATE-6-18.
           MOVE SPB-INSTERM(6 19) TO QSPB-INSTERM-6-19.
           MOVE SPB-INSRATE(6 19) TO QSPB-INSRATE-6-19.
           MOVE SPB-INSTERM(6 20) TO QSPB-INSTERM-6-20.
           MOVE SPB-INSRATE(6 20) TO QSPB-INSRATE-6-20.
           MOVE SPB-INSMINPREM(6) TO QSPB-INSMINPREM-6.
           MOVE SPB-INSMINCHGABL(6) TO QSPB-INSMINCHGABL-6.
           MOVE SPB-INSMAXCHGABL(6) TO QSPB-INSMAXCHGABL-6.
           MOVE SPB-INSMAXBENIF(6) TO QSPB-INSMAXBENIF-6.
           MOVE SPB-INSMAXTERM(6) TO QSPB-INSMAXTERM-6.
           MOVE SPB-INSCHGBEN(6) TO QSPB-INSCHGBEN-6.
           MOVE SPB-INSEXCEPT(6) TO QSPB-INSEXCEPT-6.
           MOVE SPB-INSNOVER(6) TO QSPB-INSNOVER-6.
           MOVE SPB-INSMINAGE(6 1) TO QSPB-INSMINAGE-6-1.
           MOVE SPB-INSMAXAGE(6 1) TO QSPB-INSMAXAGE-6-1.
           MOVE SPB-INSMAXCOV(6 1) TO QSPB-INSMAXCOV-6-1.
           MOVE SPB-INSMAXTRM(6 1) TO QSPB-INSMAXTRM-6-1.
           MOVE SPB-INSMINAGE(6 2) TO QSPB-INSMINAGE-6-2.
           MOVE SPB-INSMAXAGE(6 2) TO QSPB-INSMAXAGE-6-2.
           MOVE SPB-INSMAXCOV(6 2) TO QSPB-INSMAXCOV-6-2.
           MOVE SPB-INSMAXTRM(6 2) TO QSPB-INSMAXTRM-6-2.
           MOVE SPB-INSMINAGE(6 3) TO QSPB-INSMINAGE-6-3.
           MOVE SPB-INSMAXAGE(6 3) TO QSPB-INSMAXAGE-6-3.
           MOVE SPB-INSMAXCOV(6 3) TO QSPB-INSMAXCOV-6-3.
           MOVE SPB-INSMAXTRM(6 3) TO QSPB-INSMAXTRM-6-3.
           MOVE SPB-INSMINAGE(6 4) TO QSPB-INSMINAGE-6-4.
           MOVE SPB-INSMAXAGE(6 4) TO QSPB-INSMAXAGE-6-4.
           MOVE SPB-INSMAXCOV(6 4) TO QSPB-INSMAXCOV-6-4.
           MOVE SPB-INSMAXTRM(6 4) TO QSPB-INSMAXTRM-6-4.
           MOVE SPB-INSMINAGE(6 5) TO QSPB-INSMINAGE-6-5.
           MOVE SPB-INSMAXAGE(6 5) TO QSPB-INSMAXAGE-6-5.
           MOVE SPB-INSMAXCOV(6 5) TO QSPB-INSMAXCOV-6-5.
           MOVE SPB-INSMAXTRM(6 5) TO QSPB-INSMAXTRM-6-5.
           MOVE SPB-INSFEECD(6) TO QSPB-INSFEECD-6.
           MOVE SPB-INSFEE-FRMLA(6) TO QSPB-INSFEE-FRMLA-6.
           MOVE SPB-INSCHGABL-FG(6) TO QSPB-INSCHGABL-FG-6.
           MOVE SPB-INSREJECT-FG(6) TO QSPB-INSREJECT-FG-6.
           MOVE SPB-INSFINCHG(6) TO QSPB-INSFINCHG-6.
           MOVE SPB-INSCOM(7) TO QSPB-INSCOM-7.
           MOVE SPB-INSRATECD(7) TO QSPB-INSRATECD-7.
           MOVE SPB-INSJFAC(7) TO QSPB-INSJFAC-7.
           MOVE SPB-INSDISCNT(7) TO QSPB-INSDISCNT-7.
           MOVE SPB-INSCHGABL(7) TO QSPB-INSCHGABL-7.
           MOVE SPB-INSTERM(7 1) TO QSPB-INSTERM-7-1.
           MOVE SPB-INSRATE(7 1) TO QSPB-INSRATE-7-1.
           MOVE SPB-INSTERM(7 2) TO QSPB-INSTERM-7-2.
           MOVE SPB-INSRATE(7 2) TO QSPB-INSRATE-7-2.
           MOVE SPB-INSTERM(7 3) TO QSPB-INSTERM-7-3.
           MOVE SPB-INSRATE(7 3) TO QSPB-INSRATE-7-3.
           MOVE SPB-INSTERM(7 4) TO QSPB-INSTERM-7-4.
           MOVE SPB-INSRATE(7 4) TO QSPB-INSRATE-7-4.
           MOVE SPB-INSTERM(7 5) TO QSPB-INSTERM-7-5.
           MOVE SPB-INSRATE(7 5) TO QSPB-INSRATE-7-5.
           MOVE SPB-INSTERM(7 6) TO QSPB-INSTERM-7-6.
           MOVE SPB-INSRATE(7 6) TO QSPB-INSRATE-7-6.
           MOVE SPB-INSTERM(7 7) TO QSPB-INSTERM-7-7.
           MOVE SPB-INSRATE(7 7) TO QSPB-INSRATE-7-7.
           MOVE SPB-INSTERM(7 8) TO QSPB-INSTERM-7-8.
           MOVE SPB-INSRATE(7 8) TO QSPB-INSRATE-7-8.
           MOVE SPB-INSTERM(7 9) TO QSPB-INSTERM-7-9.
           MOVE SPB-INSRATE(7 9) TO QSPB-INSRATE-7-9.
           MOVE SPB-INSTERM(7 10) TO QSPB-INSTERM-7-10.
           MOVE SPB-INSRATE(7 10) TO QSPB-INSRATE-7-10.
           MOVE SPB-INSTERM(7 11) TO QSPB-INSTERM-7-11.
           MOVE SPB-INSRATE(7 11) TO QSPB-INSRATE-7-11.
           MOVE SPB-INSTERM(7 12) TO QSPB-INSTERM-7-12.
           MOVE SPB-INSRATE(7 12) TO QSPB-INSRATE-7-12.
           MOVE SPB-INSTERM(7 13) TO QSPB-INSTERM-7-13.
           MOVE SPB-INSRATE(7 13) TO QSPB-INSRATE-7-13.
           MOVE SPB-INSTERM(7 14) TO QSPB-INSTERM-7-14.
           MOVE SPB-INSRATE(7 14) TO QSPB-INSRATE-7-14.
           MOVE SPB-INSTERM(7 15) TO QSPB-INSTERM-7-15.
           MOVE SPB-INSRATE(7 15) TO QSPB-INSRATE-7-15.
           MOVE SPB-INSTERM(7 16) TO QSPB-INSTERM-7-16.
           MOVE SPB-INSRATE(7 16) TO QSPB-INSRATE-7-16.
           MOVE SPB-INSTERM(7 17) TO QSPB-INSTERM-7-17.
           MOVE SPB-INSRATE(7 17) TO QSPB-INSRATE-7-17.
           MOVE SPB-INSTERM(7 18) TO QSPB-INSTERM-7-18.
           MOVE SPB-INSRATE(7 18) TO QSPB-INSRATE-7-18.
           MOVE SPB-INSTERM(7 19) TO QSPB-INSTERM-7-19.
           MOVE SPB-INSRATE(7 19) TO QSPB-INSRATE-7-19.
           MOVE SPB-INSTERM(7 20) TO QSPB-INSTERM-7-20.
           MOVE SPB-INSRATE(7 20) TO QSPB-INSRATE-7-20.
           MOVE SPB-INSMINPREM(7) TO QSPB-INSMINPREM-7.
           MOVE SPB-INSMINCHGABL(7) TO QSPB-INSMINCHGABL-7.
           MOVE SPB-INSMAXCHGABL(7) TO QSPB-INSMAXCHGABL-7.
           MOVE SPB-INSMAXBENIF(7) TO QSPB-INSMAXBENIF-7.
           MOVE SPB-INSMAXTERM(7) TO QSPB-INSMAXTERM-7.
           MOVE SPB-INSCHGBEN(7) TO QSPB-INSCHGBEN-7.
           MOVE SPB-INSEXCEPT(7) TO QSPB-INSEXCEPT-7.
           MOVE SPB-INSNOVER(7) TO QSPB-INSNOVER-7.
           MOVE SPB-INSMINAGE(7 1) TO QSPB-INSMINAGE-7-1.
           MOVE SPB-INSMAXAGE(7 1) TO QSPB-INSMAXAGE-7-1.
           MOVE SPB-INSMAXCOV(7 1) TO QSPB-INSMAXCOV-7-1.
           MOVE SPB-INSMAXTRM(7 1) TO QSPB-INSMAXTRM-7-1.
           MOVE SPB-INSMINAGE(7 2) TO QSPB-INSMINAGE-7-2.
           MOVE SPB-INSMAXAGE(7 2) TO QSPB-INSMAXAGE-7-2.
           MOVE SPB-INSMAXCOV(7 2) TO QSPB-INSMAXCOV-7-2.
           MOVE SPB-INSMAXTRM(7 2) TO QSPB-INSMAXTRM-7-2.
           MOVE SPB-INSMINAGE(7 3) TO QSPB-INSMINAGE-7-3.
           MOVE SPB-INSMAXAGE(7 3) TO QSPB-INSMAXAGE-7-3.
           MOVE SPB-INSMAXCOV(7 3) TO QSPB-INSMAXCOV-7-3.
           MOVE SPB-INSMAXTRM(7 3) TO QSPB-INSMAXTRM-7-3.
           MOVE SPB-INSMINAGE(7 4) TO QSPB-INSMINAGE-7-4.
           MOVE SPB-INSMAXAGE(7 4) TO QSPB-INSMAXAGE-7-4.
           MOVE SPB-INSMAXCOV(7 4) TO QSPB-INSMAXCOV-7-4.
           MOVE SPB-INSMAXTRM(7 4) TO QSPB-INSMAXTRM-7-4.
           MOVE SPB-INSMINAGE(7 5) TO QSPB-INSMINAGE-7-5.
           MOVE SPB-INSMAXAGE(7 5) TO QSPB-INSMAXAGE-7-5.
           MOVE SPB-INSMAXCOV(7 5) TO QSPB-INSMAXCOV-7-5.
           MOVE SPB-INSMAXTRM(7 5) TO QSPB-INSMAXTRM-7-5.
           MOVE SPB-INSFEECD(7) TO QSPB-INSFEECD-7.
           MOVE SPB-INSFEE-FRMLA(7) TO QSPB-INSFEE-FRMLA-7.
           MOVE SPB-INSCHGABL-FG(7) TO QSPB-INSCHGABL-FG-7.
           MOVE SPB-INSREJECT-FG(7) TO QSPB-INSREJECT-FG-7.
           MOVE SPB-INSFINCHG(7) TO QSPB-INSFINCHG-7.
           MOVE SPB-INSCOM(8) TO QSPB-INSCOM-8.
           MOVE SPB-INSRATECD(8) TO QSPB-INSRATECD-8.
           MOVE SPB-INSJFAC(8) TO QSPB-INSJFAC-8.
           MOVE SPB-INSDISCNT(8) TO QSPB-INSDISCNT-8.
           MOVE SPB-INSCHGABL(8) TO QSPB-INSCHGABL-8.
           MOVE SPB-INSTERM(8 1) TO QSPB-INSTERM-8-1.
           MOVE SPB-INSRATE(8 1) TO QSPB-INSRATE-8-1.
           MOVE SPB-INSTERM(8 2) TO QSPB-INSTERM-8-2.
           MOVE SPB-INSRATE(8 2) TO QSPB-INSRATE-8-2.
           MOVE SPB-INSTERM(8 3) TO QSPB-INSTERM-8-3.
           MOVE SPB-INSRATE(8 3) TO QSPB-INSRATE-8-3.
           MOVE SPB-INSTERM(8 4) TO QSPB-INSTERM-8-4.
           MOVE SPB-INSRATE(8 4) TO QSPB-INSRATE-8-4.
           MOVE SPB-INSTERM(8 5) TO QSPB-INSTERM-8-5.
           MOVE SPB-INSRATE(8 5) TO QSPB-INSRATE-8-5.
           MOVE SPB-INSTERM(8 6) TO QSPB-INSTERM-8-6.
           MOVE SPB-INSRATE(8 6) TO QSPB-INSRATE-8-6.
           MOVE SPB-INSTERM(8 7) TO QSPB-INSTERM-8-7.
           MOVE SPB-INSRATE(8 7) TO QSPB-INSRATE-8-7.
           MOVE SPB-INSTERM(8 8) TO QSPB-INSTERM-8-8.
           MOVE SPB-INSRATE(8 8) TO QSPB-INSRATE-8-8.
           MOVE SPB-INSTERM(8 9) TO QSPB-INSTERM-8-9.
           MOVE SPB-INSRATE(8 9) TO QSPB-INSRATE-8-9.
           MOVE SPB-INSTERM(8 10) TO QSPB-INSTERM-8-10.
           MOVE SPB-INSRATE(8 10) TO QSPB-INSRATE-8-10.
           MOVE SPB-INSTERM(8 11) TO QSPB-INSTERM-8-11.
           MOVE SPB-INSRATE(8 11) TO QSPB-INSRATE-8-11.
           MOVE SPB-INSTERM(8 12) TO QSPB-INSTERM-8-12.
           MOVE SPB-INSRATE(8 12) TO QSPB-INSRATE-8-12.
           MOVE SPB-INSTERM(8 13) TO QSPB-INSTERM-8-13.
           MOVE SPB-INSRATE(8 13) TO QSPB-INSRATE-8-13.
           MOVE SPB-INSTERM(8 14) TO QSPB-INSTERM-8-14.
           MOVE SPB-INSRATE(8 14) TO QSPB-INSRATE-8-14.
           MOVE SPB-INSTERM(8 15) TO QSPB-INSTERM-8-15.
           MOVE SPB-INSRATE(8 15) TO QSPB-INSRATE-8-15.
           MOVE SPB-INSTERM(8 16) TO QSPB-INSTERM-8-16.
           MOVE SPB-INSRATE(8 16) TO QSPB-INSRATE-8-16.
           MOVE SPB-INSTERM(8 17) TO QSPB-INSTERM-8-17.
           MOVE SPB-INSRATE(8 17) TO QSPB-INSRATE-8-17.
           MOVE SPB-INSTERM(8 18) TO QSPB-INSTERM-8-18.
           MOVE SPB-INSRATE(8 18) TO QSPB-INSRATE-8-18.
           MOVE SPB-INSTERM(8 19) TO QSPB-INSTERM-8-19.
           MOVE SPB-INSRATE(8 19) TO QSPB-INSRATE-8-19.
           MOVE SPB-INSTERM(8 20) TO QSPB-INSTERM-8-20.
           MOVE SPB-INSRATE(8 20) TO QSPB-INSRATE-8-20.
           MOVE SPB-INSMINPREM(8) TO QSPB-INSMINPREM-8.
           MOVE SPB-INSMINCHGABL(8) TO QSPB-INSMINCHGABL-8.
           MOVE SPB-INSMAXCHGABL(8) TO QSPB-INSMAXCHGABL-8.
           MOVE SPB-INSMAXBENIF(8) TO QSPB-INSMAXBENIF-8.
           MOVE SPB-INSMAXTERM(8) TO QSPB-INSMAXTERM-8.
           MOVE SPB-INSCHGBEN(8) TO QSPB-INSCHGBEN-8.
           MOVE SPB-INSEXCEPT(8) TO QSPB-INSEXCEPT-8.
           MOVE SPB-INSNOVER(8) TO QSPB-INSNOVER-8.
           MOVE SPB-INSMINAGE(8 1) TO QSPB-INSMINAGE-8-1.
           MOVE SPB-INSMAXAGE(8 1) TO QSPB-INSMAXAGE-8-1.
           MOVE SPB-INSMAXCOV(8 1) TO QSPB-INSMAXCOV-8-1.
           MOVE SPB-INSMAXTRM(8 1) TO QSPB-INSMAXTRM-8-1.
           MOVE SPB-INSMINAGE(8 2) TO QSPB-INSMINAGE-8-2.
           MOVE SPB-INSMAXAGE(8 2) TO QSPB-INSMAXAGE-8-2.
           MOVE SPB-INSMAXCOV(8 2) TO QSPB-INSMAXCOV-8-2.
           MOVE SPB-INSMAXTRM(8 2) TO QSPB-INSMAXTRM-8-2.
           MOVE SPB-INSMINAGE(8 3) TO QSPB-INSMINAGE-8-3.
           MOVE SPB-INSMAXAGE(8 3) TO QSPB-INSMAXAGE-8-3.
           MOVE SPB-INSMAXCOV(8 3) TO QSPB-INSMAXCOV-8-3.
           MOVE SPB-INSMAXTRM(8 3) TO QSPB-INSMAXTRM-8-3.
           MOVE SPB-INSMINAGE(8 4) TO QSPB-INSMINAGE-8-4.
           MOVE SPB-INSMAXAGE(8 4) TO QSPB-INSMAXAGE-8-4.
           MOVE SPB-INSMAXCOV(8 4) TO QSPB-INSMAXCOV-8-4.
           MOVE SPB-INSMAXTRM(8 4) TO QSPB-INSMAXTRM-8-4.
           MOVE SPB-INSMINAGE(8 5) TO QSPB-INSMINAGE-8-5.
           MOVE SPB-INSMAXAGE(8 5) TO QSPB-INSMAXAGE-8-5.
           MOVE SPB-INSMAXCOV(8 5) TO QSPB-INSMAXCOV-8-5.
           MOVE SPB-INSMAXTRM(8 5) TO QSPB-INSMAXTRM-8-5.
           MOVE SPB-INSFEECD(8) TO QSPB-INSFEECD-8.
           MOVE SPB-INSFEE-FRMLA(8) TO QSPB-INSFEE-FRMLA-8.
           MOVE SPB-INSCHGABL-FG(8) TO QSPB-INSCHGABL-FG-8.
           MOVE SPB-INSREJECT-FG(8) TO QSPB-INSREJECT-FG-8.
           MOVE SPB-INSFINCHG(8) TO QSPB-INSFINCHG-8.
           MOVE SPB-INSCOM(9) TO QSPB-INSCOM-9.
           MOVE SPB-INSRATECD(9) TO QSPB-INSRATECD-9.
           MOVE SPB-INSJFAC(9) TO QSPB-INSJFAC-9.
           MOVE SPB-INSDISCNT(9) TO QSPB-INSDISCNT-9.
           MOVE SPB-INSCHGABL(9) TO QSPB-INSCHGABL-9.
           MOVE SPB-INSTERM(9 1) TO QSPB-INSTERM-9-1.
           MOVE SPB-INSRATE(9 1) TO QSPB-INSRATE-9-1.
           MOVE SPB-INSTERM(9 2) TO QSPB-INSTERM-9-2.
           MOVE SPB-INSRATE(9 2) TO QSPB-INSRATE-9-2.
           MOVE SPB-INSTERM(9 3) TO QSPB-INSTERM-9-3.
           MOVE SPB-INSRATE(9 3) TO QSPB-INSRATE-9-3.
           MOVE SPB-INSTERM(9 4) TO QSPB-INSTERM-9-4.
           MOVE SPB-INSRATE(9 4) TO QSPB-INSRATE-9-4.
           MOVE SPB-INSTERM(9 5) TO QSPB-INSTERM-9-5.
           MOVE SPB-INSRATE(9 5) TO QSPB-INSRATE-9-5.
           MOVE SPB-INSTERM(9 6) TO QSPB-INSTERM-9-6.
           MOVE SPB-INSRATE(9 6) TO QSPB-INSRATE-9-6.
           MOVE SPB-INSTERM(9 7) TO QSPB-INSTERM-9-7.
           MOVE SPB-INSRATE(9 7) TO QSPB-INSRATE-9-7.
           MOVE SPB-INSTERM(9 8) TO QSPB-INSTERM-9-8.
           MOVE SPB-INSRATE(9 8) TO QSPB-INSRATE-9-8.
           MOVE SPB-INSTERM(9 9) TO QSPB-INSTERM-9-9.
           MOVE SPB-INSRATE(9 9) TO QSPB-INSRATE-9-9.
           MOVE SPB-INSTERM(9 10) TO QSPB-INSTERM-9-10.
           MOVE SPB-INSRATE(9 10) TO QSPB-INSRATE-9-10.
           MOVE SPB-INSTERM(9 11) TO QSPB-INSTERM-9-11.
           MOVE SPB-INSRATE(9 11) TO QSPB-INSRATE-9-11.
           MOVE SPB-INSTERM(9 12) TO QSPB-INSTERM-9-12.
           MOVE SPB-INSRATE(9 12) TO QSPB-INSRATE-9-12.
           MOVE SPB-INSTERM(9 13) TO QSPB-INSTERM-9-13.
           MOVE SPB-INSRATE(9 13) TO QSPB-INSRATE-9-13.
           MOVE SPB-INSTERM(9 14) TO QSPB-INSTERM-9-14.
           MOVE SPB-INSRATE(9 14) TO QSPB-INSRATE-9-14.
           MOVE SPB-INSTERM(9 15) TO QSPB-INSTERM-9-15.
           MOVE SPB-INSRATE(9 15) TO QSPB-INSRATE-9-15.
           MOVE SPB-INSTERM(9 16) TO QSPB-INSTERM-9-16.
           MOVE SPB-INSRATE(9 16) TO QSPB-INSRATE-9-16.
           MOVE SPB-INSTERM(9 17) TO QSPB-INSTERM-9-17.
           MOVE SPB-INSRATE(9 17) TO QSPB-INSRATE-9-17.
           MOVE SPB-INSTERM(9 18) TO QSPB-INSTERM-9-18.
           MOVE SPB-INSRATE(9 18) TO QSPB-INSRATE-9-18.
           MOVE SPB-INSTERM(9 19) TO QSPB-INSTERM-9-19.
           MOVE SPB-INSRATE(9 19) TO QSPB-INSRATE-9-19.
           MOVE SPB-INSTERM(9 20) TO QSPB-INSTERM-9-20.
           MOVE SPB-INSRATE(9 20) TO QSPB-INSRATE-9-20.
           MOVE SPB-INSMINPREM(9) TO QSPB-INSMINPREM-9.
           MOVE SPB-INSMINCHGABL(9) TO QSPB-INSMINCHGABL-9.
           MOVE SPB-INSMAXCHGABL(9) TO QSPB-INSMAXCHGABL-9.
           MOVE SPB-INSMAXBENIF(9) TO QSPB-INSMAXBENIF-9.
           MOVE SPB-INSMAXTERM(9) TO QSPB-INSMAXTERM-9.
           MOVE SPB-INSCHGBEN(9) TO QSPB-INSCHGBEN-9.
           MOVE SPB-INSEXCEPT(9) TO QSPB-INSEXCEPT-9.
           MOVE SPB-INSNOVER(9) TO QSPB-INSNOVER-9.
           MOVE SPB-INSMINAGE(9 1) TO QSPB-INSMINAGE-9-1.
           MOVE SPB-INSMAXAGE(9 1) TO QSPB-INSMAXAGE-9-1.
           MOVE SPB-INSMAXCOV(9 1) TO QSPB-INSMAXCOV-9-1.
           MOVE SPB-INSMAXTRM(9 1) TO QSPB-INSMAXTRM-9-1.
           MOVE SPB-INSMINAGE(9 2) TO QSPB-INSMINAGE-9-2.
           MOVE SPB-INSMAXAGE(9 2) TO QSPB-INSMAXAGE-9-2.
           MOVE SPB-INSMAXCOV(9 2) TO QSPB-INSMAXCOV-9-2.
           MOVE SPB-INSMAXTRM(9 2) TO QSPB-INSMAXTRM-9-2.
           MOVE SPB-INSMINAGE(9 3) TO QSPB-INSMINAGE-9-3.
           MOVE SPB-INSMAXAGE(9 3) TO QSPB-INSMAXAGE-9-3.
           MOVE SPB-INSMAXCOV(9 3) TO QSPB-INSMAXCOV-9-3.
           MOVE SPB-INSMAXTRM(9 3) TO QSPB-INSMAXTRM-9-3.
           MOVE SPB-INSMINAGE(9 4) TO QSPB-INSMINAGE-9-4.
           MOVE SPB-INSMAXAGE(9 4) TO QSPB-INSMAXAGE-9-4.
           MOVE SPB-INSMAXCOV(9 4) TO QSPB-INSMAXCOV-9-4.
           MOVE SPB-INSMAXTRM(9 4) TO QSPB-INSMAXTRM-9-4.
           MOVE SPB-INSMINAGE(9 5) TO QSPB-INSMINAGE-9-5.
           MOVE SPB-INSMAXAGE(9 5) TO QSPB-INSMAXAGE-9-5.
           MOVE SPB-INSMAXCOV(9 5) TO QSPB-INSMAXCOV-9-5.
           MOVE SPB-INSMAXTRM(9 5) TO QSPB-INSMAXTRM-9-5.
           MOVE SPB-INSFEECD(9) TO QSPB-INSFEECD-9.
           MOVE SPB-INSFEE-FRMLA(9) TO QSPB-INSFEE-FRMLA-9.
           MOVE SPB-INSCHGABL-FG(9) TO QSPB-INSCHGABL-FG-9.
           MOVE SPB-INSREJECT-FG(9) TO QSPB-INSREJECT-FG-9.
           MOVE SPB-INSFINCHG(9) TO QSPB-INSFINCHG-9.
           MOVE SPB-INSCOM(10) TO QSPB-INSCOM-10.
           MOVE SPB-INSRATECD(10) TO QSPB-INSRATECD-10.
           MOVE SPB-INSJFAC(10) TO QSPB-INSJFAC-10.
           MOVE SPB-INSDISCNT(10) TO QSPB-INSDISCNT-10.
           MOVE SPB-INSCHGABL(10) TO QSPB-INSCHGABL-10.
           MOVE SPB-INSTERM(10 1) TO QSPB-INSTERM-10-1.
           MOVE SPB-INSRATE(10 1) TO QSPB-INSRATE-10-1.
           MOVE SPB-INSTERM(10 2) TO QSPB-INSTERM-10-2.
           MOVE SPB-INSRATE(10 2) TO QSPB-INSRATE-10-2.
           MOVE SPB-INSTERM(10 3) TO QSPB-INSTERM-10-3.
           MOVE SPB-INSRATE(10 3) TO QSPB-INSRATE-10-3.
           MOVE SPB-INSTERM(10 4) TO QSPB-INSTERM-10-4.
           MOVE SPB-INSRATE(10 4) TO QSPB-INSRATE-10-4.
           MOVE SPB-INSTERM(10 5) TO QSPB-INSTERM-10-5.
           MOVE SPB-INSRATE(10 5) TO QSPB-INSRATE-10-5.
           MOVE SPB-INSTERM(10 6) TO QSPB-INSTERM-10-6.
           MOVE SPB-INSRATE(10 6) TO QSPB-INSRATE-10-6.
           MOVE SPB-INSTERM(10 7) TO QSPB-INSTERM-10-7.
           MOVE SPB-INSRATE(10 7) TO QSPB-INSRATE-10-7.
           MOVE SPB-INSTERM(10 8) TO QSPB-INSTERM-10-8.
           MOVE SPB-INSRATE(10 8) TO QSPB-INSRATE-10-8.
           MOVE SPB-INSTERM(10 9) TO QSPB-INSTERM-10-9.
           MOVE SPB-INSRATE(10 9) TO QSPB-INSRATE-10-9.
           MOVE SPB-INSTERM(10 10) TO QSPB-INSTERM-10-10.
           MOVE SPB-INSRATE(10 10) TO QSPB-INSRATE-10-10.
           MOVE SPB-INSTERM(10 11) TO QSPB-INSTERM-10-11.
           MOVE SPB-INSRATE(10 11) TO QSPB-INSRATE-10-11.
           MOVE SPB-INSTERM(10 12) TO QSPB-INSTERM-10-12.
           MOVE SPB-INSRATE(10 12) TO QSPB-INSRATE-10-12.
           MOVE SPB-INSTERM(10 13) TO QSPB-INSTERM-10-13.
           MOVE SPB-INSRATE(10 13) TO QSPB-INSRATE-10-13.
           MOVE SPB-INSTERM(10 14) TO QSPB-INSTERM-10-14.
           MOVE SPB-INSRATE(10 14) TO QSPB-INSRATE-10-14.
           MOVE SPB-INSTERM(10 15) TO QSPB-INSTERM-10-15.
           MOVE SPB-INSRATE(10 15) TO QSPB-INSRATE-10-15.
           MOVE SPB-INSTERM(10 16) TO QSPB-INSTERM-10-16.
           MOVE SPB-INSRATE(10 16) TO QSPB-INSRATE-10-16.
           MOVE SPB-INSTERM(10 17) TO QSPB-INSTERM-10-17.
           MOVE SPB-INSRATE(10 17) TO QSPB-INSRATE-10-17.
           MOVE SPB-INSTERM(10 18) TO QSPB-INSTERM-10-18.
           MOVE SPB-INSRATE(10 18) TO QSPB-INSRATE-10-18.
           MOVE SPB-INSTERM(10 19) TO QSPB-INSTERM-10-19.
           MOVE SPB-INSRATE(10 19) TO QSPB-INSRATE-10-19.
           MOVE SPB-INSTERM(10 20) TO QSPB-INSTERM-10-20.
           MOVE SPB-INSRATE(10 20) TO QSPB-INSRATE-10-20.
           MOVE SPB-INSMINPREM(10) TO QSPB-INSMINPREM-10.
           MOVE SPB-INSMINCHGABL(10) TO QSPB-INSMINCHGABL-10.
           MOVE SPB-INSMAXCHGABL(10) TO QSPB-INSMAXCHGABL-10.
           MOVE SPB-INSMAXBENIF(10) TO QSPB-INSMAXBENIF-10.
           MOVE SPB-INSMAXTERM(10) TO QSPB-INSMAXTERM-10.
           MOVE SPB-INSCHGBEN(10) TO QSPB-INSCHGBEN-10.
           MOVE SPB-INSEXCEPT(10) TO QSPB-INSEXCEPT-10.
           MOVE SPB-INSNOVER(10) TO QSPB-INSNOVER-10.
           MOVE SPB-INSMINAGE(10 1) TO QSPB-INSMINAGE-10-1.
           MOVE SPB-INSMAXAGE(10 1) TO QSPB-INSMAXAGE-10-1.
           MOVE SPB-INSMAXCOV(10 1) TO QSPB-INSMAXCOV-10-1.
           MOVE SPB-INSMAXTRM(10 1) TO QSPB-INSMAXTRM-10-1.
           MOVE SPB-INSMINAGE(10 2) TO QSPB-INSMINAGE-10-2.
           MOVE SPB-INSMAXAGE(10 2) TO QSPB-INSMAXAGE-10-2.
           MOVE SPB-INSMAXCOV(10 2) TO QSPB-INSMAXCOV-10-2.
           MOVE SPB-INSMAXTRM(10 2) TO QSPB-INSMAXTRM-10-2.
           MOVE SPB-INSMINAGE(10 3) TO QSPB-INSMINAGE-10-3.
           MOVE SPB-INSMAXAGE(10 3) TO QSPB-INSMAXAGE-10-3.
           MOVE SPB-INSMAXCOV(10 3) TO QSPB-INSMAXCOV-10-3.
           MOVE SPB-INSMAXTRM(10 3) TO QSPB-INSMAXTRM-10-3.
           MOVE SPB-INSMINAGE(10 4) TO QSPB-INSMINAGE-10-4.
           MOVE SPB-INSMAXAGE(10 4) TO QSPB-INSMAXAGE-10-4.
           MOVE SPB-INSMAXCOV(10 4) TO QSPB-INSMAXCOV-10-4.
           MOVE SPB-INSMAXTRM(10 4) TO QSPB-INSMAXTRM-10-4.
           MOVE SPB-INSMINAGE(10 5) TO QSPB-INSMINAGE-10-5.
           MOVE SPB-INSMAXAGE(10 5) TO QSPB-INSMAXAGE-10-5.
           MOVE SPB-INSMAXCOV(10 5) TO QSPB-INSMAXCOV-10-5.
           MOVE SPB-INSMAXTRM(10 5) TO QSPB-INSMAXTRM-10-5.
           MOVE SPB-INSFEECD(10) TO QSPB-INSFEECD-10.
           MOVE SPB-INSFEE-FRMLA(10) TO QSPB-INSFEE-FRMLA-10.
           MOVE SPB-INSCHGABL-FG(10) TO QSPB-INSCHGABL-FG-10.
           MOVE SPB-INSREJECT-FG(10) TO QSPB-INSREJECT-FG-10.
           MOVE SPB-INSFINCHG(10) TO QSPB-INSFINCHG-10.
           MOVE SPB-INSCOM(11) TO QSPB-INSCOM-11.
           MOVE SPB-INSRATECD(11) TO QSPB-INSRATECD-11.
           MOVE SPB-INSJFAC(11) TO QSPB-INSJFAC-11.
           MOVE SPB-INSDISCNT(11) TO QSPB-INSDISCNT-11.
           MOVE SPB-INSCHGABL(11) TO QSPB-INSCHGABL-11.
           MOVE SPB-INSTERM(11 1) TO QSPB-INSTERM-11-1.
           MOVE SPB-INSRATE(11 1) TO QSPB-INSRATE-11-1.
           MOVE SPB-INSTERM(11 2) TO QSPB-INSTERM-11-2.
           MOVE SPB-INSRATE(11 2) TO QSPB-INSRATE-11-2.
           MOVE SPB-INSTERM(11 3) TO QSPB-INSTERM-11-3.
           MOVE SPB-INSRATE(11 3) TO QSPB-INSRATE-11-3.
           MOVE SPB-INSTERM(11 4) TO QSPB-INSTERM-11-4.
           MOVE SPB-INSRATE(11 4) TO QSPB-INSRATE-11-4.
           MOVE SPB-INSTERM(11 5) TO QSPB-INSTERM-11-5.
           MOVE SPB-INSRATE(11 5) TO QSPB-INSRATE-11-5.
           MOVE SPB-INSTERM(11 6) TO QSPB-INSTERM-11-6.
           MOVE SPB-INSRATE(11 6) TO QSPB-INSRATE-11-6.
           MOVE SPB-INSTERM(11 7) TO QSPB-INSTERM-11-7.
           MOVE SPB-INSRATE(11 7) TO QSPB-INSRATE-11-7.
           MOVE SPB-INSTERM(11 8) TO QSPB-INSTERM-11-8.
           MOVE SPB-INSRATE(11 8) TO QSPB-INSRATE-11-8.
           MOVE SPB-INSTERM(11 9) TO QSPB-INSTERM-11-9.
           MOVE SPB-INSRATE(11 9) TO QSPB-INSRATE-11-9.
           MOVE SPB-INSTERM(11 10) TO QSPB-INSTERM-11-10.
           MOVE SPB-INSRATE(11 10) TO QSPB-INSRATE-11-10.
           MOVE SPB-INSTERM(11 11) TO QSPB-INSTERM-11-11.
           MOVE SPB-INSRATE(11 11) TO QSPB-INSRATE-11-11.
           MOVE SPB-INSTERM(11 12) TO QSPB-INSTERM-11-12.
           MOVE SPB-INSRATE(11 12) TO QSPB-INSRATE-11-12.
           MOVE SPB-INSTERM(11 13) TO QSPB-INSTERM-11-13.
           MOVE SPB-INSRATE(11 13) TO QSPB-INSRATE-11-13.
           MOVE SPB-INSTERM(11 14) TO QSPB-INSTERM-11-14.
           MOVE SPB-INSRATE(11 14) TO QSPB-INSRATE-11-14.
           MOVE SPB-INSTERM(11 15) TO QSPB-INSTERM-11-15.
           MOVE SPB-INSRATE(11 15) TO QSPB-INSRATE-11-15.
           MOVE SPB-INSTERM(11 16) TO QSPB-INSTERM-11-16.
           MOVE SPB-INSRATE(11 16) TO QSPB-INSRATE-11-16.
           MOVE SPB-INSTERM(11 17) TO QSPB-INSTERM-11-17.
           MOVE SPB-INSRATE(11 17) TO QSPB-INSRATE-11-17.
           MOVE SPB-INSTERM(11 18) TO QSPB-INSTERM-11-18.
           MOVE SPB-INSRATE(11 18) TO QSPB-INSRATE-11-18.
           MOVE SPB-INSTERM(11 19) TO QSPB-INSTERM-11-19.
           MOVE SPB-INSRATE(11 19) TO QSPB-INSRATE-11-19.
           MOVE SPB-INSTERM(11 20) TO QSPB-INSTERM-11-20.
           MOVE SPB-INSRATE(11 20) TO QSPB-INSRATE-11-20.
           MOVE SPB-INSMINPREM(11) TO QSPB-INSMINPREM-11.
           MOVE SPB-INSMINCHGABL(11) TO QSPB-INSMINCHGABL-11.
           MOVE SPB-INSMAXCHGABL(11) TO QSPB-INSMAXCHGABL-11.
           MOVE SPB-INSMAXBENIF(11) TO QSPB-INSMAXBENIF-11.
           MOVE SPB-INSMAXTERM(11) TO QSPB-INSMAXTERM-11.
           MOVE SPB-INSCHGBEN(11) TO QSPB-INSCHGBEN-11.
           MOVE SPB-INSEXCEPT(11) TO QSPB-INSEXCEPT-11.
           MOVE SPB-INSNOVER(11) TO QSPB-INSNOVER-11.
           MOVE SPB-INSMINAGE(11 1) TO QSPB-INSMINAGE-11-1.
           MOVE SPB-INSMAXAGE(11 1) TO QSPB-INSMAXAGE-11-1.
           MOVE SPB-INSMAXCOV(11 1) TO QSPB-INSMAXCOV-11-1.
           MOVE SPB-INSMAXTRM(11 1) TO QSPB-INSMAXTRM-11-1.
           MOVE SPB-INSMINAGE(11 2) TO QSPB-INSMINAGE-11-2.
           MOVE SPB-INSMAXAGE(11 2) TO QSPB-INSMAXAGE-11-2.
           MOVE SPB-INSMAXCOV(11 2) TO QSPB-INSMAXCOV-11-2.
           MOVE SPB-INSMAXTRM(11 2) TO QSPB-INSMAXTRM-11-2.
           MOVE SPB-INSMINAGE(11 3) TO QSPB-INSMINAGE-11-3.
           MOVE SPB-INSMAXAGE(11 3) TO QSPB-INSMAXAGE-11-3.
           MOVE SPB-INSMAXCOV(11 3) TO QSPB-INSMAXCOV-11-3.
           MOVE SPB-INSMAXTRM(11 3) TO QSPB-INSMAXTRM-11-3.
           MOVE SPB-INSMINAGE(11 4) TO QSPB-INSMINAGE-11-4.
           MOVE SPB-INSMAXAGE(11 4) TO QSPB-INSMAXAGE-11-4.
           MOVE SPB-INSMAXCOV(11 4) TO QSPB-INSMAXCOV-11-4.
           MOVE SPB-INSMAXTRM(11 4) TO QSPB-INSMAXTRM-11-4.
           MOVE SPB-INSMINAGE(11 5) TO QSPB-INSMINAGE-11-5.
           MOVE SPB-INSMAXAGE(11 5) TO QSPB-INSMAXAGE-11-5.
           MOVE SPB-INSMAXCOV(11 5) TO QSPB-INSMAXCOV-11-5.
           MOVE SPB-INSMAXTRM(11 5) TO QSPB-INSMAXTRM-11-5.
           MOVE SPB-INSFEECD(11) TO QSPB-INSFEECD-11.
           MOVE SPB-INSFEE-FRMLA(11) TO QSPB-INSFEE-FRMLA-11.
           MOVE SPB-INSCHGABL-FG(11) TO QSPB-INSCHGABL-FG-11.
           MOVE SPB-INSREJECT-FG(11) TO QSPB-INSREJECT-FG-11.
           MOVE SPB-INSFINCHG(11) TO QSPB-INSFINCHG-11.
           MOVE SPB-INSCOM(12) TO QSPB-INSCOM-12.
           MOVE SPB-INSRATECD(12) TO QSPB-INSRATECD-12.
           MOVE SPB-INSJFAC(12) TO QSPB-INSJFAC-12.
           MOVE SPB-INSDISCNT(12) TO QSPB-INSDISCNT-12.
           MOVE SPB-INSCHGABL(12) TO QSPB-INSCHGABL-12.
           MOVE SPB-INSTERM(12 1) TO QSPB-INSTERM-12-1.
           MOVE SPB-INSRATE(12 1) TO QSPB-INSRATE-12-1.
           MOVE SPB-INSTERM(12 2) TO QSPB-INSTERM-12-2.
           MOVE SPB-INSRATE(12 2) TO QSPB-INSRATE-12-2.
           MOVE SPB-INSTERM(12 3) TO QSPB-INSTERM-12-3.
           MOVE SPB-INSRATE(12 3) TO QSPB-INSRATE-12-3.
           MOVE SPB-INSTERM(12 4) TO QSPB-INSTERM-12-4.
           MOVE SPB-INSRATE(12 4) TO QSPB-INSRATE-12-4.
           MOVE SPB-INSTERM(12 5) TO QSPB-INSTERM-12-5.
           MOVE SPB-INSRATE(12 5) TO QSPB-INSRATE-12-5.
           MOVE SPB-INSTERM(12 6) TO QSPB-INSTERM-12-6.
           MOVE SPB-INSRATE(12 6) TO QSPB-INSRATE-12-6.
           MOVE SPB-INSTERM(12 7) TO QSPB-INSTERM-12-7.
           MOVE SPB-INSRATE(12 7) TO QSPB-INSRATE-12-7.
           MOVE SPB-INSTERM(12 8) TO QSPB-INSTERM-12-8.
           MOVE SPB-INSRATE(12 8) TO QSPB-INSRATE-12-8.
           MOVE SPB-INSTERM(12 9) TO QSPB-INSTERM-12-9.
           MOVE SPB-INSRATE(12 9) TO QSPB-INSRATE-12-9.
           MOVE SPB-INSTERM(12 10) TO QSPB-INSTERM-12-10.
           MOVE SPB-INSRATE(12 10) TO QSPB-INSRATE-12-10.
           MOVE SPB-INSTERM(12 11) TO QSPB-INSTERM-12-11.
           MOVE SPB-INSRATE(12 11) TO QSPB-INSRATE-12-11.
           MOVE SPB-INSTERM(12 12) TO QSPB-INSTERM-12-12.
           MOVE SPB-INSRATE(12 12) TO QSPB-INSRATE-12-12.
           MOVE SPB-INSTERM(12 13) TO QSPB-INSTERM-12-13.
           MOVE SPB-INSRATE(12 13) TO QSPB-INSRATE-12-13.
           MOVE SPB-INSTERM(12 14) TO QSPB-INSTERM-12-14.
           MOVE SPB-INSRATE(12 14) TO QSPB-INSRATE-12-14.
           MOVE SPB-INSTERM(12 15) TO QSPB-INSTERM-12-15.
           MOVE SPB-INSRATE(12 15) TO QSPB-INSRATE-12-15.
           MOVE SPB-INSTERM(12 16) TO QSPB-INSTERM-12-16.
           MOVE SPB-INSRATE(12 16) TO QSPB-INSRATE-12-16.
           MOVE SPB-INSTERM(12 17) TO QSPB-INSTERM-12-17.
           MOVE SPB-INSRATE(12 17) TO QSPB-INSRATE-12-17.
           MOVE SPB-INSTERM(12 18) TO QSPB-INSTERM-12-18.
           MOVE SPB-INSRATE(12 18) TO QSPB-INSRATE-12-18.
           MOVE SPB-INSTERM(12 19) TO QSPB-INSTERM-12-19.
           MOVE SPB-INSRATE(12 19) TO QSPB-INSRATE-12-19.
           MOVE SPB-INSTERM(12 20) TO QSPB-INSTERM-12-20.
           MOVE SPB-INSRATE(12 20) TO QSPB-INSRATE-12-20.
           MOVE SPB-INSMINPREM(12) TO QSPB-INSMINPREM-12.
           MOVE SPB-INSMINCHGABL(12) TO QSPB-INSMINCHGABL-12.
           MOVE SPB-INSMAXCHGABL(12) TO QSPB-INSMAXCHGABL-12.
           MOVE SPB-INSMAXBENIF(12) TO QSPB-INSMAXBENIF-12.
           MOVE SPB-INSMAXTERM(12) TO QSPB-INSMAXTERM-12.
           MOVE SPB-INSCHGBEN(12) TO QSPB-INSCHGBEN-12.
           MOVE SPB-INSEXCEPT(12) TO QSPB-INSEXCEPT-12.
           MOVE SPB-INSNOVER(12) TO QSPB-INSNOVER-12.
           MOVE SPB-INSMINAGE(12 1) TO QSPB-INSMINAGE-12-1.
           MOVE SPB-INSMAXAGE(12 1) TO QSPB-INSMAXAGE-12-1.
           MOVE SPB-INSMAXCOV(12 1) TO QSPB-INSMAXCOV-12-1.
           MOVE SPB-INSMAXTRM(12 1) TO QSPB-INSMAXTRM-12-1.
           MOVE SPB-INSMINAGE(12 2) TO QSPB-INSMINAGE-12-2.
           MOVE SPB-INSMAXAGE(12 2) TO QSPB-INSMAXAGE-12-2.
           MOVE SPB-INSMAXCOV(12 2) TO QSPB-INSMAXCOV-12-2.
           MOVE SPB-INSMAXTRM(12 2) TO QSPB-INSMAXTRM-12-2.
           MOVE SPB-INSMINAGE(12 3) TO QSPB-INSMINAGE-12-3.
           MOVE SPB-INSMAXAGE(12 3) TO QSPB-INSMAXAGE-12-3.
           MOVE SPB-INSMAXCOV(12 3) TO QSPB-INSMAXCOV-12-3.
           MOVE SPB-INSMAXTRM(12 3) TO QSPB-INSMAXTRM-12-3.
           MOVE SPB-INSMINAGE(12 4) TO QSPB-INSMINAGE-12-4.
           MOVE SPB-INSMAXAGE(12 4) TO QSPB-INSMAXAGE-12-4.
           MOVE SPB-INSMAXCOV(12 4) TO QSPB-INSMAXCOV-12-4.
           MOVE SPB-INSMAXTRM(12 4) TO QSPB-INSMAXTRM-12-4.
           MOVE SPB-INSMINAGE(12 5) TO QSPB-INSMINAGE-12-5.
           MOVE SPB-INSMAXAGE(12 5) TO QSPB-INSMAXAGE-12-5.
           MOVE SPB-INSMAXCOV(12 5) TO QSPB-INSMAXCOV-12-5.
           MOVE SPB-INSMAXTRM(12 5) TO QSPB-INSMAXTRM-12-5.
           MOVE SPB-INSFEECD(12) TO QSPB-INSFEECD-12.
           MOVE SPB-INSFEE-FRMLA(12) TO QSPB-INSFEE-FRMLA-12.
           MOVE SPB-INSCHGABL-FG(12) TO QSPB-INSCHGABL-FG-12.
           MOVE SPB-INSREJECT-FG(12) TO QSPB-INSREJECT-FG-12.
           MOVE SPB-INSFINCHG(12) TO QSPB-INSFINCHG-12.
 
      *-----------------------------------------------------------------
      *    LOAD SPC-REC FROM QSPC-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-SPC-FIELDS.
           MOVE QSPC-ORGST TO SPC-ORGST.
           MOVE QSPC-SPRCLASS TO SPC-SPRCLASS.
           MOVE QSPC-SUBCLASS TO SPC-SUBCLASS.
           MOVE QSPC-LAWCODE TO SPC-LAWCODE.
           MOVE QSPC-INSEARN-ORDER-1 TO SPC-INSEARN-ORDER(1).
           MOVE QSPC-INSEARN-ORDER-2 TO SPC-INSEARN-ORDER(2).
           MOVE QSPC-INSEARN-ORDER-3 TO SPC-INSEARN-ORDER(3).
           MOVE QSPC-INSEARN-ORDER-4 TO SPC-INSEARN-ORDER(4).
           MOVE QSPC-INSEARN-ORDER-5 TO SPC-INSEARN-ORDER(5).
           MOVE QSPC-INSEARN-ORDER-6 TO SPC-INSEARN-ORDER(6).
           MOVE QSPC-INSEARN-ORDER-7 TO SPC-INSEARN-ORDER(7).
           MOVE QSPC-INSEARN-ORDER-8 TO SPC-INSEARN-ORDER(8).
           MOVE QSPC-INSEARN-ORDER-9 TO SPC-INSEARN-ORDER(9).
           MOVE QSPC-INSEARN-ORDER-10 TO SPC-INSEARN-ORDER(10).
           MOVE QSPC-INSEARN-ORDER-11 TO SPC-INSEARN-ORDER(11).
           MOVE QSPC-INSEARN-ORDER-12 TO SPC-INSEARN-ORDER(12).
           MOVE QSPC-INSAH-JOINT-FG TO SPC-INSAH-JOINT-FG.
           MOVE QSPC-INS-O1-EDIT TO SPC-INS-O1-EDIT.
           MOVE QSPC-INS-O2-EDIT TO SPC-INS-O2-EDIT.
           MOVE QSPC-RBFRMLA-1 TO SPC-RBFRMLA(1).
           MOVE QSPC-RBDTE-1-1 TO SPC-RBDTE(1 1).
           MOVE QSPC-RBDAYS-1-1 TO SPC-RBDAYS(1 1).
           MOVE QSPC-RBYRTYPE-1-1 TO SPC-RBYRTYPE(1 1).
           MOVE QSPC-RBDTE-1-2 TO SPC-RBDTE(1 2).
           MOVE QSPC-RBDAYS-1-2 TO SPC-RBDAYS(1 2).
           MOVE QSPC-RBYRTYPE-1-2 TO SPC-RBYRTYPE(1 2).
           MOVE QSPC-RBMINRET-1 TO SPC-RBMINRET(1).
           MOVE QSPC-RBNOMON-1 TO SPC-RBNOMON(1).
           MOVE QSPC-RBDEDCHG-1 TO SPC-RBDEDCHG(1).
           MOVE QSPC-RBRECAST-1 TO SPC-RBRECAST(1).
           MOVE QSPC-RBEARLY-1 TO SPC-RBEARLY(1).
           MOVE QSPC-RBDEFER-1 TO SPC-RBDEFER(1).
           MOVE QSPC-RBPCT-1 TO SPC-RBPCT(1).
           MOVE QSPC-RBREDUC-1 TO SPC-RBREDUC(1).
           MOVE QSPC-RBMIN-1 TO SPC-RBMIN(1).
           MOVE QSPC-RBSPOPT1-1 TO SPC-RBSPOPT1(1).
           MOVE QSPC-RBSPOPT2-1 TO SPC-RBSPOPT2(1).
           MOVE QSPC-RBMANAGER-1 TO SPC-RBMANAGER(1).
           MOVE QSPC-RBUNITPER-CD-1 TO SPC-RBUNITPER-CD(1).
           MOVE QSPC-RBFRMLA2-1 TO SPC-RBFRMLA2(1).
           MOVE QSPC-RBFRMLA-2 TO SPC-RBFRMLA(2).
           MOVE QSPC-RBDTE-2-1 TO SPC-RBDTE(2 1).
           MOVE QSPC-RBDAYS-2-1 TO SPC-RBDAYS(2 1).
           MOVE QSPC-RBYRTYPE-2-1 TO SPC-RBYRTYPE(2 1).
           MOVE QSPC-RBDTE-2-2 TO SPC-RBDTE(2 2).
           MOVE QSPC-RBDAYS-2-2 TO SPC-RBDAYS(2 2).
           MOVE QSPC-RBYRTYPE-2-2 TO SPC-RBYRTYPE(2 2).
           MOVE QSPC-RBMINRET-2 TO SPC-RBMINRET(2).
           MOVE QSPC-RBNOMON-2 TO SPC-RBNOMON(2).
           MOVE QSPC-RBDEDCHG-2 TO SPC-RBDEDCHG(2).
           MOVE QSPC-RBRECAST-2 TO SPC-RBRECAST(2).
           MOVE QSPC-RBEARLY-2 TO SPC-RBEARLY(2).
           MOVE QSPC-RBDEFER-2 TO SPC-RBDEFER(2).
           MOVE QSPC-RBPCT-2 TO SPC-RBPCT(2).
           MOVE QSPC-RBREDUC-2 TO SPC-RBREDUC(2).
           MOVE QSPC-RBMIN-2 TO SPC-RBMIN(2).
           MOVE QSPC-RBSPOPT1-2 TO SPC-RBSPOPT1(2).
           MOVE QSPC-RBSPOPT2-2 TO SPC-RBSPOPT2(2).
           MOVE QSPC-RBMANAGER-2 TO SPC-RBMANAGER(2).
           MOVE QSPC-RBUNITPER-CD-2 TO SPC-RBUNITPER-CD(2).
           MOVE QSPC-RBFRMLA2-2 TO SPC-RBFRMLA2(2).
           MOVE QSPC-RBFRMLA-3 TO SPC-RBFRMLA(3).
           MOVE QSPC-RBDTE-3-1 TO SPC-RBDTE(3 1).
           MOVE QSPC-RBDAYS-3-1 TO SPC-RBDAYS(3 1).
           MOVE QSPC-RBYRTYPE-3-1 TO SPC-RBYRTYPE(3 1).
           MOVE QSPC-RBDTE-3-2 TO SPC-RBDTE(3 2).
           MOVE QSPC-RBDAYS-3-2 TO SPC-RBDAYS(3 2).
           MOVE QSPC-RBYRTYPE-3-2 TO SPC-RBYRTYPE(3 2).
           MOVE QSPC-RBMINRET-3 TO SPC-RBMINRET(3).
           MOVE QSPC-RBNOMON-3 TO SPC-RBNOMON(3).
           MOVE QSPC-RBDEDCHG-3 TO SPC-RBDEDCHG(3).
           MOVE QSPC-RBRECAST-3 TO SPC-RBRECAST(3).
           MOVE QSPC-RBEARLY-3 TO SPC-RBEARLY(3).
           MOVE QSPC-RBDEFER-3 TO SPC-RBDEFER(3).
           MOVE QSPC-RBPCT-3 TO SPC-RBPCT(3).
           MOVE QSPC-RBREDUC-3 TO SPC-RBREDUC(3).
           MOVE QSPC-RBMIN-3 TO SPC-RBMIN(3).
           MOVE QSPC-RBSPOPT1-3 TO SPC-RBSPOPT1(3).
           MOVE QSPC-RBSPOPT2-3 TO SPC-RBSPOPT2(3).
           MOVE QSPC-RBMANAGER-3 TO SPC-RBMANAGER(3).
           MOVE QSPC-RBUNITPER-CD-3 TO SPC-RBUNITPER-CD(3).
           MOVE QSPC-RBFRMLA2-3 TO SPC-RBFRMLA2(3).
           MOVE QSPC-RBFRMLA-4 TO SPC-RBFRMLA(4).
           MOVE QSPC-RBDTE-4-1 TO SPC-RBDTE(4 1).
           MOVE QSPC-RBDAYS-4-1 TO SPC-RBDAYS(4 1).
           MOVE QSPC-RBYRTYPE-4-1 TO SPC-RBYRTYPE(4 1).
           MOVE QSPC-RBDTE-4-2 TO SPC-RBDTE(4 2).
           MOVE QSPC-RBDAYS-4-2 TO SPC-RBDAYS(4 2).
           MOVE QSPC-RBYRTYPE-4-2 TO SPC-RBYRTYPE(4 2).
           MOVE QSPC-RBMINRET-4 TO SPC-RBMINRET(4).
           MOVE QSPC-RBNOMON-4 TO SPC-RBNOMON(4).
           MOVE QSPC-RBDEDCHG-4 TO SPC-RBDEDCHG(4).
           MOVE QSPC-RBRECAST-4 TO SPC-RBRECAST(4).
           MOVE QSPC-RBEARLY-4 TO SPC-RBEARLY(4).
           MOVE QSPC-RBDEFER-4 TO SPC-RBDEFER(4).
           MOVE QSPC-RBPCT-4 TO SPC-RBPCT(4).
           MOVE QSPC-RBREDUC-4 TO SPC-RBREDUC(4).
           MOVE QSPC-RBMIN-4 TO SPC-RBMIN(4).
           MOVE QSPC-RBSPOPT1-4 TO SPC-RBSPOPT1(4).
           MOVE QSPC-RBSPOPT2-4 TO SPC-RBSPOPT2(4).
           MOVE QSPC-RBMANAGER-4 TO SPC-RBMANAGER(4).
           MOVE QSPC-RBUNITPER-CD-4 TO SPC-RBUNITPER-CD(4).
           MOVE QSPC-RBFRMLA2-4 TO SPC-RBFRMLA2(4).
           MOVE QSPC-RBFRMLA-5 TO SPC-RBFRMLA(5).
           MOVE QSPC-RBDTE-5-1 TO SPC-RBDTE(5 1).
           MOVE QSPC-RBDAYS-5-1 TO SPC-RBDAYS(5 1).
           MOVE QSPC-RBYRTYPE-5-1 TO SPC-RBYRTYPE(5 1).
           MOVE QSPC-RBDTE-5-2 TO SPC-RBDTE(5 2).
           MOVE QSPC-RBDAYS-5-2 TO SPC-RBDAYS(5 2).
           MOVE QSPC-RBYRTYPE-5-2 TO SPC-RBYRTYPE(5 2).
           MOVE QSPC-RBMINRET-5 TO SPC-RBMINRET(5).
           MOVE QSPC-RBNOMON-5 TO SPC-RBNOMON(5).
           MOVE QSPC-RBDEDCHG-5 TO SPC-RBDEDCHG(5).
           MOVE QSPC-RBRECAST-5 TO SPC-RBRECAST(5).
           MOVE QSPC-RBEARLY-5 TO SPC-RBEARLY(5).
           MOVE QSPC-RBDEFER-5 TO SPC-RBDEFER(5).
           MOVE QSPC-RBPCT-5 TO SPC-RBPCT(5).
           MOVE QSPC-RBREDUC-5 TO SPC-RBREDUC(5).
           MOVE QSPC-RBMIN-5 TO SPC-RBMIN(5).
           MOVE QSPC-RBSPOPT1-5 TO SPC-RBSPOPT1(5).
           MOVE QSPC-RBSPOPT2-5 TO SPC-RBSPOPT2(5).
           MOVE QSPC-RBMANAGER-5 TO SPC-RBMANAGER(5).
           MOVE QSPC-RBUNITPER-CD-5 TO SPC-RBUNITPER-CD(5).
           MOVE QSPC-RBFRMLA2-5 TO SPC-RBFRMLA2(5).
           MOVE QSPC-RBFRMLA-6 TO SPC-RBFRMLA(6).
           MOVE QSPC-RBDTE-6-1 TO SPC-RBDTE(6 1).
           MOVE QSPC-RBDAYS-6-1 TO SPC-RBDAYS(6 1).
           MOVE QSPC-RBYRTYPE-6-1 TO SPC-RBYRTYPE(6 1).
           MOVE QSPC-RBDTE-6-2 TO SPC-RBDTE(6 2).
           MOVE QSPC-RBDAYS-6-2 TO SPC-RBDAYS(6 2).
           MOVE QSPC-RBYRTYPE-6-2 TO SPC-RBYRTYPE(6 2).
           MOVE QSPC-RBMINRET-6 TO SPC-RBMINRET(6).
           MOVE QSPC-RBNOMON-6 TO SPC-RBNOMON(6).
           MOVE QSPC-RBDEDCHG-6 TO SPC-RBDEDCHG(6).
           MOVE QSPC-RBRECAST-6 TO SPC-RBRECAST(6).
           MOVE QSPC-RBEARLY-6 TO SPC-RBEARLY(6).
           MOVE QSPC-RBDEFER-6 TO SPC-RBDEFER(6).
           MOVE QSPC-RBPCT-6 TO SPC-RBPCT(6).
           MOVE QSPC-RBREDUC-6 TO SPC-RBREDUC(6).
           MOVE QSPC-RBMIN-6 TO SPC-RBMIN(6).
           MOVE QSPC-RBSPOPT1-6 TO SPC-RBSPOPT1(6).
           MOVE QSPC-RBSPOPT2-6 TO SPC-RBSPOPT2(6).
           MOVE QSPC-RBMANAGER-6 TO SPC-RBMANAGER(6).
           MOVE QSPC-RBUNITPER-CD-6 TO SPC-RBUNITPER-CD(6).
           MOVE QSPC-RBFRMLA2-6 TO SPC-RBFRMLA2(6).
           MOVE QSPC-RBFRMLA-7 TO SPC-RBFRMLA(7).
           MOVE QSPC-RBDTE-7-1 TO SPC-RBDTE(7 1).
           MOVE QSPC-RBDAYS-7-1 TO SPC-RBDAYS(7 1).
           MOVE QSPC-RBYRTYPE-7-1 TO SPC-RBYRTYPE(7 1).
           MOVE QSPC-RBDTE-7-2 TO SPC-RBDTE(7 2).
           MOVE QSPC-RBDAYS-7-2 TO SPC-RBDAYS(7 2).
           MOVE QSPC-RBYRTYPE-7-2 TO SPC-RBYRTYPE(7 2).
           MOVE QSPC-RBMINRET-7 TO SPC-RBMINRET(7).
           MOVE QSPC-RBNOMON-7 TO SPC-RBNOMON(7).
           MOVE QSPC-RBDEDCHG-7 TO SPC-RBDEDCHG(7).
           MOVE QSPC-RBRECAST-7 TO SPC-RBRECAST(7).
           MOVE QSPC-RBEARLY-7 TO SPC-RBEARLY(7).
           MOVE QSPC-RBDEFER-7 TO SPC-RBDEFER(7).
           MOVE QSPC-RBPCT-7 TO SPC-RBPCT(7).
           MOVE QSPC-RBREDUC-7 TO SPC-RBREDUC(7).
           MOVE QSPC-RBMIN-7 TO SPC-RBMIN(7).
           MOVE QSPC-RBSPOPT1-7 TO SPC-RBSPOPT1(7).
           MOVE QSPC-RBSPOPT2-7 TO SPC-RBSPOPT2(7).
           MOVE QSPC-RBMANAGER-7 TO SPC-RBMANAGER(7).
           MOVE QSPC-RBUNITPER-CD-7 TO SPC-RBUNITPER-CD(7).
           MOVE QSPC-RBFRMLA2-7 TO SPC-RBFRMLA2(7).
           MOVE QSPC-RBFRMLA-8 TO SPC-RBFRMLA(8).
           MOVE QSPC-RBDTE-8-1 TO SPC-RBDTE(8 1).
           MOVE QSPC-RBDAYS-8-1 TO SPC-RBDAYS(8 1).
           MOVE QSPC-RBYRTYPE-8-1 TO SPC-RBYRTYPE(8 1).
           MOVE QSPC-RBDTE-8-2 TO SPC-RBDTE(8 2).
           MOVE QSPC-RBDAYS-8-2 TO SPC-RBDAYS(8 2).
           MOVE QSPC-RBYRTYPE-8-2 TO SPC-RBYRTYPE(8 2).
           MOVE QSPC-RBMINRET-8 TO SPC-RBMINRET(8).
           MOVE QSPC-RBNOMON-8 TO SPC-RBNOMON(8).
           MOVE QSPC-RBDEDCHG-8 TO SPC-RBDEDCHG(8).
           MOVE QSPC-RBRECAST-8 TO SPC-RBRECAST(8).
           MOVE QSPC-RBEARLY-8 TO SPC-RBEARLY(8).
           MOVE QSPC-RBDEFER-8 TO SPC-RBDEFER(8).
           MOVE QSPC-RBPCT-8 TO SPC-RBPCT(8).
           MOVE QSPC-RBREDUC-8 TO SPC-RBREDUC(8).
           MOVE QSPC-RBMIN-8 TO SPC-RBMIN(8).
           MOVE QSPC-RBSPOPT1-8 TO SPC-RBSPOPT1(8).
           MOVE QSPC-RBSPOPT2-8 TO SPC-RBSPOPT2(8).
           MOVE QSPC-RBMANAGER-8 TO SPC-RBMANAGER(8).
           MOVE QSPC-RBUNITPER-CD-8 TO SPC-RBUNITPER-CD(8).
           MOVE QSPC-RBFRMLA2-8 TO SPC-RBFRMLA2(8).
           MOVE QSPC-RBFRMLA-9 TO SPC-RBFRMLA(9).
           MOVE QSPC-RBDTE-9-1 TO SPC-RBDTE(9 1).
           MOVE QSPC-RBDAYS-9-1 TO SPC-RBDAYS(9 1).
           MOVE QSPC-RBYRTYPE-9-1 TO SPC-RBYRTYPE(9 1).
           MOVE QSPC-RBDTE-9-2 TO SPC-RBDTE(9 2).
           MOVE QSPC-RBDAYS-9-2 TO SPC-RBDAYS(9 2).
           MOVE QSPC-RBYRTYPE-9-2 TO SPC-RBYRTYPE(9 2).
           MOVE QSPC-RBMINRET-9 TO SPC-RBMINRET(9).
           MOVE QSPC-RBNOMON-9 TO SPC-RBNOMON(9).
           MOVE QSPC-RBDEDCHG-9 TO SPC-RBDEDCHG(9).
           MOVE QSPC-RBRECAST-9 TO SPC-RBRECAST(9).
           MOVE QSPC-RBEARLY-9 TO SPC-RBEARLY(9).
           MOVE QSPC-RBDEFER-9 TO SPC-RBDEFER(9).
           MOVE QSPC-RBPCT-9 TO SPC-RBPCT(9).
           MOVE QSPC-RBREDUC-9 TO SPC-RBREDUC(9).
           MOVE QSPC-RBMIN-9 TO SPC-RBMIN(9).
           MOVE QSPC-RBSPOPT1-9 TO SPC-RBSPOPT1(9).
           MOVE QSPC-RBSPOPT2-9 TO SPC-RBSPOPT2(9).
           MOVE QSPC-RBMANAGER-9 TO SPC-RBMANAGER(9).
           MOVE QSPC-RBUNITPER-CD-9 TO SPC-RBUNITPER-CD(9).
           MOVE QSPC-RBFRMLA2-9 TO SPC-RBFRMLA2(9).
           MOVE QSPC-RBFRMLA-10 TO SPC-RBFRMLA(10).
           MOVE QSPC-RBDTE-10-1 TO SPC-RBDTE(10 1).
           MOVE QSPC-RBDAYS-10-1 TO SPC-RBDAYS(10 1).
           MOVE QSPC-RBYRTYPE-10-1 TO SPC-RBYRTYPE(10 1).
           MOVE QSPC-RBDTE-10-2 TO SPC-RBDTE(10 2).
           MOVE QSPC-RBDAYS-10-2 TO SPC-RBDAYS(10 2).
           MOVE QSPC-RBYRTYPE-10-2 TO SPC-RBYRTYPE(10 2).
           MOVE QSPC-RBMINRET-10 TO SPC-RBMINRET(10).
           MOVE QSPC-RBNOMON-10 TO SPC-RBNOMON(10).
           MOVE QSPC-RBDEDCHG-10 TO SPC-RBDEDCHG(10).
           MOVE QSPC-RBRECAST-10 TO SPC-RBRECAST(10).
           MOVE QSPC-RBEARLY-10 TO SPC-RBEARLY(10).
           MOVE QSPC-RBDEFER-10 TO SPC-RBDEFER(10).
           MOVE QSPC-RBPCT-10 TO SPC-RBPCT(10).
           MOVE QSPC-RBREDUC-10 TO SPC-RBREDUC(10).
           MOVE QSPC-RBMIN-10 TO SPC-RBMIN(10).
           MOVE QSPC-RBSPOPT1-10 TO SPC-RBSPOPT1(10).
           MOVE QSPC-RBSPOPT2-10 TO SPC-RBSPOPT2(10).
           MOVE QSPC-RBMANAGER-10 TO SPC-RBMANAGER(10).
           MOVE QSPC-RBUNITPER-CD-10 TO SPC-RBUNITPER-CD(10).
           MOVE QSPC-RBFRMLA2-10 TO SPC-RBFRMLA2(10).
           MOVE QSPC-RBFRMLA-11 TO SPC-RBFRMLA(11).
           MOVE QSPC-RBDTE-11-1 TO SPC-RBDTE(11 1).
           MOVE QSPC-RBDAYS-11-1 TO SPC-RBDAYS(11 1).
           MOVE QSPC-RBYRTYPE-11-1 TO SPC-RBYRTYPE(11 1).
           MOVE QSPC-RBDTE-11-2 TO SPC-RBDTE(11 2).
           MOVE QSPC-RBDAYS-11-2 TO SPC-RBDAYS(11 2).
           MOVE QSPC-RBYRTYPE-11-2 TO SPC-RBYRTYPE(11 2).
           MOVE QSPC-RBMINRET-11 TO SPC-RBMINRET(11).
           MOVE QSPC-RBNOMON-11 TO SPC-RBNOMON(11).
           MOVE QSPC-RBDEDCHG-11 TO SPC-RBDEDCHG(11).
           MOVE QSPC-RBRECAST-11 TO SPC-RBRECAST(11).
           MOVE QSPC-RBEARLY-11 TO SPC-RBEARLY(11).
           MOVE QSPC-RBDEFER-11 TO SPC-RBDEFER(11).
           MOVE QSPC-RBPCT-11 TO SPC-RBPCT(11).
           MOVE QSPC-RBREDUC-11 TO SPC-RBREDUC(11).
           MOVE QSPC-RBMIN-11 TO SPC-RBMIN(11).
           MOVE QSPC-RBSPOPT1-11 TO SPC-RBSPOPT1(11).
           MOVE QSPC-RBSPOPT2-11 TO SPC-RBSPOPT2(11).
           MOVE QSPC-RBMANAGER-11 TO SPC-RBMANAGER(11).
           MOVE QSPC-RBUNITPER-CD-11 TO SPC-RBUNITPER-CD(11).
           MOVE QSPC-RBFRMLA2-11 TO SPC-RBFRMLA2(11).
           MOVE QSPC-RBFRMLA-12 TO SPC-RBFRMLA(12).
           MOVE QSPC-RBDTE-12-1 TO SPC-RBDTE(12 1).
           MOVE QSPC-RBDAYS-12-1 TO SPC-RBDAYS(12 1).
           MOVE QSPC-RBYRTYPE-12-1 TO SPC-RBYRTYPE(12 1).
           MOVE QSPC-RBDTE-12-2 TO SPC-RBDTE(12 2).
           MOVE QSPC-RBDAYS-12-2 TO SPC-RBDAYS(12 2).
           MOVE QSPC-RBYRTYPE-12-2 TO SPC-RBYRTYPE(12 2).
           MOVE QSPC-RBMINRET-12 TO SPC-RBMINRET(12).
           MOVE QSPC-RBNOMON-12 TO SPC-RBNOMON(12).
           MOVE QSPC-RBDEDCHG-12 TO SPC-RBDEDCHG(12).
           MOVE QSPC-RBRECAST-12 TO SPC-RBRECAST(12).
           MOVE QSPC-RBEARLY-12 TO SPC-RBEARLY(12).
           MOVE QSPC-RBDEFER-12 TO SPC-RBDEFER(12).
           MOVE QSPC-RBPCT-12 TO SPC-RBPCT(12).
           MOVE QSPC-RBREDUC-12 TO SPC-RBREDUC(12).
           MOVE QSPC-RBMIN-12 TO SPC-RBMIN(12).
           MOVE QSPC-RBSPOPT1-12 TO SPC-RBSPOPT1(12).
           MOVE QSPC-RBSPOPT2-12 TO SPC-RBSPOPT2(12).
           MOVE QSPC-RBMANAGER-12 TO SPC-RBMANAGER(12).
           MOVE QSPC-RBUNITPER-CD-12 TO SPC-RBUNITPER-CD(12).
           MOVE QSPC-RBFRMLA2-12 TO SPC-RBFRMLA2(12).
           MOVE QSPC-RBFRMLA-13 TO SPC-RBFRMLA(13).
           MOVE QSPC-RBDTE-13-1 TO SPC-RBDTE(13 1).
           MOVE QSPC-RBDAYS-13-1 TO SPC-RBDAYS(13 1).
           MOVE QSPC-RBYRTYPE-13-1 TO SPC-RBYRTYPE(13 1).
           MOVE QSPC-RBDTE-13-2 TO SPC-RBDTE(13 2).
           MOVE QSPC-RBDAYS-13-2 TO SPC-RBDAYS(13 2).
           MOVE QSPC-RBYRTYPE-13-2 TO SPC-RBYRTYPE(13 2).
           MOVE QSPC-RBMINRET-13 TO SPC-RBMINRET(13).
           MOVE QSPC-RBNOMON-13 TO SPC-RBNOMON(13).
           MOVE QSPC-RBDEDCHG-13 TO SPC-RBDEDCHG(13).
           MOVE QSPC-RBRECAST-13 TO SPC-RBRECAST(13).
           MOVE QSPC-RBEARLY-13 TO SPC-RBEARLY(13).
           MOVE QSPC-RBDEFER-13 TO SPC-RBDEFER(13).
           MOVE QSPC-RBPCT-13 TO SPC-RBPCT(13).
           MOVE QSPC-RBREDUC-13 TO SPC-RBREDUC(13).
           MOVE QSPC-RBMIN-13 TO SPC-RBMIN(13).
           MOVE QSPC-RBSPOPT1-13 TO SPC-RBSPOPT1(13).
           MOVE QSPC-RBSPOPT2-13 TO SPC-RBSPOPT2(13).
           MOVE QSPC-RBMANAGER-13 TO SPC-RBMANAGER(13).
           MOVE QSPC-RBUNITPER-CD-13 TO SPC-RBUNITPER-CD(13).
           MOVE QSPC-RBFRMLA2-13 TO SPC-RBFRMLA2(13).
           MOVE QSPC-RBFRMLA-14 TO SPC-RBFRMLA(14).
           MOVE QSPC-RBDTE-14-1 TO SPC-RBDTE(14 1).
           MOVE QSPC-RBDAYS-14-1 TO SPC-RBDAYS(14 1).
           MOVE QSPC-RBYRTYPE-14-1 TO SPC-RBYRTYPE(14 1).
           MOVE QSPC-RBDTE-14-2 TO SPC-RBDTE(14 2).
           MOVE QSPC-RBDAYS-14-2 TO SPC-RBDAYS(14 2).
           MOVE QSPC-RBYRTYPE-14-2 TO SPC-RBYRTYPE(14 2).
           MOVE QSPC-RBMINRET-14 TO SPC-RBMINRET(14).
           MOVE QSPC-RBNOMON-14 TO SPC-RBNOMON(14).
           MOVE QSPC-RBDEDCHG-14 TO SPC-RBDEDCHG(14).
           MOVE QSPC-RBRECAST-14 TO SPC-RBRECAST(14).
           MOVE QSPC-RBEARLY-14 TO SPC-RBEARLY(14).
           MOVE QSPC-RBDEFER-14 TO SPC-RBDEFER(14).
           MOVE QSPC-RBPCT-14 TO SPC-RBPCT(14).
           MOVE QSPC-RBREDUC-14 TO SPC-RBREDUC(14).
           MOVE QSPC-RBMIN-14 TO SPC-RBMIN(14).
           MOVE QSPC-RBSPOPT1-14 TO SPC-RBSPOPT1(14).
           MOVE QSPC-RBSPOPT2-14 TO SPC-RBSPOPT2(14).
           MOVE QSPC-RBMANAGER-14 TO SPC-RBMANAGER(14).
           MOVE QSPC-RBUNITPER-CD-14 TO SPC-RBUNITPER-CD(14).
           MOVE QSPC-RBFRMLA2-14 TO SPC-RBFRMLA2(14).
           MOVE QSPC-RBFRMLA-15 TO SPC-RBFRMLA(15).
           MOVE QSPC-RBDTE-15-1 TO SPC-RBDTE(15 1).
           MOVE QSPC-RBDAYS-15-1 TO SPC-RBDAYS(15 1).
           MOVE QSPC-RBYRTYPE-15-1 TO SPC-RBYRTYPE(15 1).
           MOVE QSPC-RBDTE-15-2 TO SPC-RBDTE(15 2).
           MOVE QSPC-RBDAYS-15-2 TO SPC-RBDAYS(15 2).
           MOVE QSPC-RBYRTYPE-15-2 TO SPC-RBYRTYPE(15 2).
           MOVE QSPC-RBMINRET-15 TO SPC-RBMINRET(15).
           MOVE QSPC-RBNOMON-15 TO SPC-RBNOMON(15).
           MOVE QSPC-RBDEDCHG-15 TO SPC-RBDEDCHG(15).
           MOVE QSPC-RBRECAST-15 TO SPC-RBRECAST(15).
           MOVE QSPC-RBEARLY-15 TO SPC-RBEARLY(15).
           MOVE QSPC-RBDEFER-15 TO SPC-RBDEFER(15).
           MOVE QSPC-RBPCT-15 TO SPC-RBPCT(15).
           MOVE QSPC-RBREDUC-15 TO SPC-RBREDUC(15).
           MOVE QSPC-RBMIN-15 TO SPC-RBMIN(15).
           MOVE QSPC-RBSPOPT1-15 TO SPC-RBSPOPT1(15).
           MOVE QSPC-RBSPOPT2-15 TO SPC-RBSPOPT2(15).
           MOVE QSPC-RBMANAGER-15 TO SPC-RBMANAGER(15).
           MOVE QSPC-RBUNITPER-CD-15 TO SPC-RBUNITPER-CD(15).
           MOVE QSPC-RBFRMLA2-15 TO SPC-RBFRMLA2(15).
           MOVE QSPC-RBFRMLA-16 TO SPC-RBFRMLA(16).
           MOVE QSPC-RBDTE-16-1 TO SPC-RBDTE(16 1).
           MOVE QSPC-RBDAYS-16-1 TO SPC-RBDAYS(16 1).
           MOVE QSPC-RBYRTYPE-16-1 TO SPC-RBYRTYPE(16 1).
           MOVE QSPC-RBDTE-16-2 TO SPC-RBDTE(16 2).
           MOVE QSPC-RBDAYS-16-2 TO SPC-RBDAYS(16 2).
           MOVE QSPC-RBYRTYPE-16-2 TO SPC-RBYRTYPE(16 2).
           MOVE QSPC-RBMINRET-16 TO SPC-RBMINRET(16).
           MOVE QSPC-RBNOMON-16 TO SPC-RBNOMON(16).
           MOVE QSPC-RBDEDCHG-16 TO SPC-RBDEDCHG(16).
           MOVE QSPC-RBRECAST-16 TO SPC-RBRECAST(16).
           MOVE QSPC-RBEARLY-16 TO SPC-RBEARLY(16).
           MOVE QSPC-RBDEFER-16 TO SPC-RBDEFER(16).
           MOVE QSPC-RBPCT-16 TO SPC-RBPCT(16).
           MOVE QSPC-RBREDUC-16 TO SPC-RBREDUC(16).
           MOVE QSPC-RBMIN-16 TO SPC-RBMIN(16).
           MOVE QSPC-RBSPOPT1-16 TO SPC-RBSPOPT1(16).
           MOVE QSPC-RBSPOPT2-16 TO SPC-RBSPOPT2(16).
           MOVE QSPC-RBMANAGER-16 TO SPC-RBMANAGER(16).
           MOVE QSPC-RBUNITPER-CD-16 TO SPC-RBUNITPER-CD(16).
           MOVE QSPC-RBFRMLA2-16 TO SPC-RBFRMLA2(16).
           MOVE QSPC-RBFRMLA-17 TO SPC-RBFRMLA(17).
           MOVE QSPC-RBDTE-17-1 TO SPC-RBDTE(17 1).
           MOVE QSPC-RBDAYS-17-1 TO SPC-RBDAYS(17 1).
           MOVE QSPC-RBYRTYPE-17-1 TO SPC-RBYRTYPE(17 1).
           MOVE QSPC-RBDTE-17-2 TO SPC-RBDTE(17 2).
           MOVE QSPC-RBDAYS-17-2 TO SPC-RBDAYS(17 2).
           MOVE QSPC-RBYRTYPE-17-2 TO SPC-RBYRTYPE(17 2).
           MOVE QSPC-RBMINRET-17 TO SPC-RBMINRET(17).
           MOVE QSPC-RBNOMON-17 TO SPC-RBNOMON(17).
           MOVE QSPC-RBDEDCHG-17 TO SPC-RBDEDCHG(17).
           MOVE QSPC-RBRECAST-17 TO SPC-RBRECAST(17).
           MOVE QSPC-RBEARLY-17 TO SPC-RBEARLY(17).
           MOVE QSPC-RBDEFER-17 TO SPC-RBDEFER(17).
           MOVE QSPC-RBPCT-17 TO SPC-RBPCT(17).
           MOVE QSPC-RBREDUC-17 TO SPC-RBREDUC(17).
           MOVE QSPC-RBMIN-17 TO SPC-RBMIN(17).
           MOVE QSPC-RBSPOPT1-17 TO SPC-RBSPOPT1(17).
           MOVE QSPC-RBSPOPT2-17 TO SPC-RBSPOPT2(17).
           MOVE QSPC-RBMANAGER-17 TO SPC-RBMANAGER(17).
           MOVE QSPC-RBUNITPER-CD-17 TO SPC-RBUNITPER-CD(17).
           MOVE QSPC-RBFRMLA2-17 TO SPC-RBFRMLA2(17).
           MOVE QSPC-RBFRMLA-18 TO SPC-RBFRMLA(18).
           MOVE QSPC-RBDTE-18-1 TO SPC-RBDTE(18 1).
           MOVE QSPC-RBDAYS-18-1 TO SPC-RBDAYS(18 1).
           MOVE QSPC-RBYRTYPE-18-1 TO SPC-RBYRTYPE(18 1).
           MOVE QSPC-RBDTE-18-2 TO SPC-RBDTE(18 2).
           MOVE QSPC-RBDAYS-18-2 TO SPC-RBDAYS(18 2).
           MOVE QSPC-RBYRTYPE-18-2 TO SPC-RBYRTYPE(18 2).
           MOVE QSPC-RBMINRET-18 TO SPC-RBMINRET(18).
           MOVE QSPC-RBNOMON-18 TO SPC-RBNOMON(18).
           MOVE QSPC-RBDEDCHG-18 TO SPC-RBDEDCHG(18).
           MOVE QSPC-RBRECAST-18 TO SPC-RBRECAST(18).
           MOVE QSPC-RBEARLY-18 TO SPC-RBEARLY(18).
           MOVE QSPC-RBDEFER-18 TO SPC-RBDEFER(18).
           MOVE QSPC-RBPCT-18 TO SPC-RBPCT(18).
           MOVE QSPC-RBREDUC-18 TO SPC-RBREDUC(18).
           MOVE QSPC-RBMIN-18 TO SPC-RBMIN(18).
           MOVE QSPC-RBSPOPT1-18 TO SPC-RBSPOPT1(18).
           MOVE QSPC-RBSPOPT2-18 TO SPC-RBSPOPT2(18).
           MOVE QSPC-RBMANAGER-18 TO SPC-RBMANAGER(18).
           MOVE QSPC-RBUNITPER-CD-18 TO SPC-RBUNITPER-CD(18).
           MOVE QSPC-RBFRMLA2-18 TO SPC-RBFRMLA2(18).
           MOVE QSPC-RBFRMLA-19 TO SPC-RBFRMLA(19).
           MOVE QSPC-RBDTE-19-1 TO SPC-RBDTE(19 1).
           MOVE QSPC-RBDAYS-19-1 TO SPC-RBDAYS(19 1).
           MOVE QSPC-RBYRTYPE-19-1 TO SPC-RBYRTYPE(19 1).
           MOVE QSPC-RBDTE-19-2 TO SPC-RBDTE(19 2).
           MOVE QSPC-RBDAYS-19-2 TO SPC-RBDAYS(19 2).
           MOVE QSPC-RBYRTYPE-19-2 TO SPC-RBYRTYPE(19 2).
           MOVE QSPC-RBMINRET-19 TO SPC-RBMINRET(19).
           MOVE QSPC-RBNOMON-19 TO SPC-RBNOMON(19).
           MOVE QSPC-RBDEDCHG-19 TO SPC-RBDEDCHG(19).
           MOVE QSPC-RBRECAST-19 TO SPC-RBRECAST(19).
           MOVE QSPC-RBEARLY-19 TO SPC-RBEARLY(19).
           MOVE QSPC-RBDEFER-19 TO SPC-RBDEFER(19).
           MOVE QSPC-RBPCT-19 TO SPC-RBPCT(19).
           MOVE QSPC-RBREDUC-19 TO SPC-RBREDUC(19).
           MOVE QSPC-RBMIN-19 TO SPC-RBMIN(19).
           MOVE QSPC-RBSPOPT1-19 TO SPC-RBSPOPT1(19).
           MOVE QSPC-RBSPOPT2-19 TO SPC-RBSPOPT2(19).
           MOVE QSPC-RBMANAGER-19 TO SPC-RBMANAGER(19).
           MOVE QSPC-RBUNITPER-CD-19 TO SPC-RBUNITPER-CD(19).
           MOVE QSPC-RBFRMLA2-19 TO SPC-RBFRMLA2(19).
           MOVE QSPC-RBFRMLA-20 TO SPC-RBFRMLA(20).
           MOVE QSPC-RBDTE-20-1 TO SPC-RBDTE(20 1).
           MOVE QSPC-RBDAYS-20-1 TO SPC-RBDAYS(20 1).
           MOVE QSPC-RBYRTYPE-20-1 TO SPC-RBYRTYPE(20 1).
           MOVE QSPC-RBDTE-20-2 TO SPC-RBDTE(20 2).
           MOVE QSPC-RBDAYS-20-2 TO SPC-RBDAYS(20 2).
           MOVE QSPC-RBYRTYPE-20-2 TO SPC-RBYRTYPE(20 2).
           MOVE QSPC-RBMINRET-20 TO SPC-RBMINRET(20).
           MOVE QSPC-RBNOMON-20 TO SPC-RBNOMON(20).
           MOVE QSPC-RBDEDCHG-20 TO SPC-RBDEDCHG(20).
           MOVE QSPC-RBRECAST-20 TO SPC-RBRECAST(20).
           MOVE QSPC-RBEARLY-20 TO SPC-RBEARLY(20).
           MOVE QSPC-RBDEFER-20 TO SPC-RBDEFER(20).
           MOVE QSPC-RBPCT-20 TO SPC-RBPCT(20).
           MOVE QSPC-RBREDUC-20 TO SPC-RBREDUC(20).
           MOVE QSPC-RBMIN-20 TO SPC-RBMIN(20).
           MOVE QSPC-RBSPOPT1-20 TO SPC-RBSPOPT1(20).
           MOVE QSPC-RBSPOPT2-20 TO SPC-RBSPOPT2(20).
           MOVE QSPC-RBMANAGER-20 TO SPC-RBMANAGER(20).
           MOVE QSPC-RBUNITPER-CD-20 TO SPC-RBUNITPER-CD(20).
           MOVE QSPC-RBFRMLA2-20 TO SPC-RBFRMLA2(20).
           MOVE QSPC-RBFRMLA-21 TO SPC-RBFRMLA(21).
           MOVE QSPC-RBDTE-21-1 TO SPC-RBDTE(21 1).
           MOVE QSPC-RBDAYS-21-1 TO SPC-RBDAYS(21 1).
           MOVE QSPC-RBYRTYPE-21-1 TO SPC-RBYRTYPE(21 1).
           MOVE QSPC-RBDTE-21-2 TO SPC-RBDTE(21 2).
           MOVE QSPC-RBDAYS-21-2 TO SPC-RBDAYS(21 2).
           MOVE QSPC-RBYRTYPE-21-2 TO SPC-RBYRTYPE(21 2).
           MOVE QSPC-RBMINRET-21 TO SPC-RBMINRET(21).
           MOVE QSPC-RBNOMON-21 TO SPC-RBNOMON(21).
           MOVE QSPC-RBDEDCHG-21 TO SPC-RBDEDCHG(21).
           MOVE QSPC-RBRECAST-21 TO SPC-RBRECAST(21).
           MOVE QSPC-RBEARLY-21 TO SPC-RBEARLY(21).
           MOVE QSPC-RBDEFER-21 TO SPC-RBDEFER(21).
           MOVE QSPC-RBPCT-21 TO SPC-RBPCT(21).
           MOVE QSPC-RBREDUC-21 TO SPC-RBREDUC(21).
           MOVE QSPC-RBMIN-21 TO SPC-RBMIN(21).
           MOVE QSPC-RBSPOPT1-21 TO SPC-RBSPOPT1(21).
           MOVE QSPC-RBSPOPT2-21 TO SPC-RBSPOPT2(21).
           MOVE QSPC-RBMANAGER-21 TO SPC-RBMANAGER(21).
           MOVE QSPC-RBUNITPER-CD-21 TO SPC-RBUNITPER-CD(21).
           MOVE QSPC-RBFRMLA2-21 TO SPC-RBFRMLA2(21).
           MOVE QSPC-RBFRMLA-22 TO SPC-RBFRMLA(22).
           MOVE QSPC-RBDTE-22-1 TO SPC-RBDTE(22 1).
           MOVE QSPC-RBDAYS-22-1 TO SPC-RBDAYS(22 1).
           MOVE QSPC-RBYRTYPE-22-1 TO SPC-RBYRTYPE(22 1).
           MOVE QSPC-RBDTE-22-2 TO SPC-RBDTE(22 2).
           MOVE QSPC-RBDAYS-22-2 TO SPC-RBDAYS(22 2).
           MOVE QSPC-RBYRTYPE-22-2 TO SPC-RBYRTYPE(22 2).
           MOVE QSPC-RBMINRET-22 TO SPC-RBMINRET(22).
           MOVE QSPC-RBNOMON-22 TO SPC-RBNOMON(22).
           MOVE QSPC-RBDEDCHG-22 TO SPC-RBDEDCHG(22).
           MOVE QSPC-RBRECAST-22 TO SPC-RBRECAST(22).
           MOVE QSPC-RBEARLY-22 TO SPC-RBEARLY(22).
           MOVE QSPC-RBDEFER-22 TO SPC-RBDEFER(22).
           MOVE QSPC-RBPCT-22 TO SPC-RBPCT(22).
           MOVE QSPC-RBREDUC-22 TO SPC-RBREDUC(22).
           MOVE QSPC-RBMIN-22 TO SPC-RBMIN(22).
           MOVE QSPC-RBSPOPT1-22 TO SPC-RBSPOPT1(22).
           MOVE QSPC-RBSPOPT2-22 TO SPC-RBSPOPT2(22).
           MOVE QSPC-RBMANAGER-22 TO SPC-RBMANAGER(22).
           MOVE QSPC-RBUNITPER-CD-22 TO SPC-RBUNITPER-CD(22).
           MOVE QSPC-RBFRMLA2-22 TO SPC-RBFRMLA2(22).
           MOVE QSPC-RBCOMCD TO SPC-RBCOMCD.
           MOVE QSPC-ADDON-DISFRMLA TO SPC-ADDON-DISFRMLA.
           MOVE QSPC-ADDON-WHOLE-FG TO SPC-ADDON-WHOLE-FG.
           MOVE QSPC-ADDON-ORG-MATURITY TO SPC-ADDON-ORG-MATURITY.
           MOVE QSPC-CONSOLIDATE-RT-FG TO SPC-CONSOLIDATE-RT-FG.
           MOVE QSPC-1STPY-ADDON-DAYS TO SPC-1STPY-ADDON-DAYS.
           MOVE QSPC-1STPY-ADDON-YRTYPE TO SPC-1STPY-ADDON-YRTYPE.
           MOVE QSPC-ADDON-RATE-FG TO SPC-ADDON-RATE-FG.
           MOVE QSPC-ADDON-RATE TO SPC-ADDON-RATE.
           MOVE QSPC-FEEFRMLA-1 TO SPC-FEEFRMLA(1).
           MOVE QSPC-FEECD-1 TO SPC-FEECD(1).
           MOVE QSPC-FEEID-1 TO SPC-FEEID(1).
           MOVE QSPC-FEEFRMLA-2 TO SPC-FEEFRMLA(2).
           MOVE QSPC-FEECD-2 TO SPC-FEECD(2).
           MOVE QSPC-FEEID-2 TO SPC-FEEID(2).
           MOVE QSPC-ROUND-PAY-FG TO SPC-ROUND-PAY-FG.
           MOVE QSPC-IRREGULAR-PAY-FG TO SPC-IRREGULAR-PAY-FG.
           MOVE QSPC-DLRVCHGS-RB-CD-1 TO SPC-DLRVCHGS-RB-CD(1).
           MOVE QSPC-DLRVCHGS-RB-CD-2 TO SPC-DLRVCHGS-RB-CD(2).
           MOVE QSPC-DLRVCHGS-RB-CD-3 TO SPC-DLRVCHGS-RB-CD(3).
           MOVE QSPC-PARVHELD-RB-CD TO SPC-PARVHELD-RB-CD.
           MOVE QSPC-PARVPAID-RB-CD TO SPC-PARVPAID-RB-CD.
           MOVE QSPC-CONTR-CREDIT TO SPC-CONTR-CREDIT.
           MOVE QSPC-CONTR-CREDIT-CD TO SPC-CONTR-CREDIT-CD.
           MOVE QSPC-CONTR-CREDIT-MIN-DEL-PER
              TO SPC-CONTR-CREDIT-MIN-DEL-PER.
           MOVE QSPC-OPT-CAL-CHGABLE TO SPC-OPT-CAL-CHGABLE.
           MOVE QSPC-PBDEL-DAYS TO SPC-PBDEL-DAYS.
           MOVE QSPC-PBINCR-PERCENT TO SPC-PBINCR-PERCENT.
           MOVE QSPC-CPOT30-CD TO SPC-CPOT30-CD.
           MOVE QSPC-CPOTCHGOFF-CD TO SPC-CPOTCHGOFF-CD.
           MOVE QSPC-RDPOT30-CD TO SPC-RDPOT30-CD.
           MOVE QSPC-RDPOTCHGOFF-CD TO SPC-RDPOTCHGOFF-CD.
           MOVE QSPC-MAX-ALLOW-DELINQ TO SPC-MAX-ALLOW-DELINQ.
           MOVE QSPC-RESCIND-BUS-DAYS TO SPC-RESCIND-BUS-DAYS.
           MOVE QSPC-RESCIND-SATURDAY TO SPC-RESCIND-SATURDAY.
           MOVE QSPC-MIN-CREDIT-SCORE TO SPC-MIN-CREDIT-SCORE.
           MOVE QSPC-DUAL-IN-BRANCH TO SPC-DUAL-IN-BRANCH.
           MOVE QSPC-DUAL-IN-STATE TO SPC-DUAL-IN-STATE.
           MOVE QSPC-PACESETTER-FG TO SPC-PACESETTER-FG.
           MOVE QSPC-WAGE-ASSIGN-DAYS TO SPC-WAGE-ASSIGN-DAYS.
           MOVE QSPC-CAL-PAYMENT-ROUND-FG TO SPC-CAL-PAYMENT-ROUND-FG.
           MOVE QSPC-CAN-LOANS-FG TO SPC-CAN-LOANS-FG.
           MOVE QSPC-ALLOW-DELINQ-POLCD TO SPC-ALLOW-DELINQ-POLCD.
           MOVE QSPC-YTD-MAX-ALLOW-DELINQ TO SPC-YTD-MAX-ALLOW-DELINQ.
           MOVE QSPC-LNCOST-FRMLA TO SPC-LNCOST-FRMLA.
           MOVE QSPC-LNCOST-AMT TO SPC-LNCOST-AMT.
           MOVE QSPC-DLDISC-RVPRIN1-PCNT TO SPC-DLDISC-RVPRIN1-PCNT.
           MOVE QSPC-RENEWABLE-PAYS TO SPC-RENEWABLE-PAYS.
           MOVE QSPC-RENEWABLE-DAYS TO SPC-RENEWABLE-DAYS.
           MOVE QSPC-ALLOW-LN-BACK-DATE TO SPC-ALLOW-LN-BACK-DATE.
           MOVE QSPC-DLDISC-RVPRIN1-AF-PCNT
              TO SPC-DLDISC-RVPRIN1-AF-PCNT.
           MOVE QSPC-CAL-MAX-AT-ESTIMATE TO SPC-CAL-MAX-AT-ESTIMATE.
           MOVE QSPC-INSAH-RETRO TO SPC-INSAH-RETRO.
           MOVE QSPC-INSAH-DAYS-WAIT TO SPC-INSAH-DAYS-WAIT.
           MOVE QSPC-ADDON-EXCEPT-FG TO SPC-ADDON-EXCEPT-FG.
           MOVE QSPC-REPO-TRCD-1 TO SPC-REPO-TRCD(1).
           MOVE QSPC-REPO-FMNO-1-1 TO SPC-REPO-FMNO(1 1).
           MOVE QSPC-REPO-FMNO-1-2 TO SPC-REPO-FMNO(1 2).
           MOVE QSPC-REPO-FMNO-1-3 TO SPC-REPO-FMNO(1 3).
           MOVE QSPC-REPO-FMNO-1-4 TO SPC-REPO-FMNO(1 4).
           MOVE QSPC-REPO-TRCD-2 TO SPC-REPO-TRCD(2).
           MOVE QSPC-REPO-FMNO-2-1 TO SPC-REPO-FMNO(2 1).
           MOVE QSPC-REPO-FMNO-2-2 TO SPC-REPO-FMNO(2 2).
           MOVE QSPC-REPO-FMNO-2-3 TO SPC-REPO-FMNO(2 3).
           MOVE QSPC-REPO-FMNO-2-4 TO SPC-REPO-FMNO(2 4).
           MOVE QSPC-REPO-TRCD-3 TO SPC-REPO-TRCD(3).
           MOVE QSPC-REPO-FMNO-3-1 TO SPC-REPO-FMNO(3 1).
           MOVE QSPC-REPO-FMNO-3-2 TO SPC-REPO-FMNO(3 2).
           MOVE QSPC-REPO-FMNO-3-3 TO SPC-REPO-FMNO(3 3).
           MOVE QSPC-REPO-FMNO-3-4 TO SPC-REPO-FMNO(3 4).
           MOVE QSPC-CANCEL-ADDON-FG TO SPC-CANCEL-ADDON-FG.
           MOVE QSPC-DEFAULT-O2-INS-COMP TO SPC-DEFAULT-O2-INS-COMP.
           MOVE QSPC-LC-ON-JUDGEMENTS TO SPC-LC-ON-JUDGEMENTS.
           MOVE QSPC-DELQ-IB-AGE TO SPC-DELQ-IB-AGE.
           MOVE QSPC-DELQ-IB-MAX-DAYS TO SPC-DELQ-IB-MAX-DAYS.
           MOVE QSPC-INSURANCE-WRITTEN TO SPC-INSURANCE-WRITTEN.
           MOVE QSPC-INTERIM-COUPON TO SPC-INTERIM-COUPON.
           MOVE QSPC-AUTO-ASSESS-LCHG TO SPC-AUTO-ASSESS-LCHG.
           MOVE QSPC-MAX-CASH-PROCEEDS TO SPC-MAX-CASH-PROCEEDS.
           MOVE QSPC-ALLOW-ZERO-PROC TO SPC-ALLOW-ZERO-PROC.
           MOVE QSPC-BANKRULE-RATE TO SPC-BANKRULE-RATE.
           MOVE QSPC-AUTO-SEND-TRCD-1 TO SPC-AUTO-SEND-TRCD(1).
           MOVE QSPC-AUTO-SEND-FMNO-1-1 TO SPC-AUTO-SEND-FMNO(1 1).
           MOVE QSPC-AUTO-SEND-FMNO-1-2 TO SPC-AUTO-SEND-FMNO(1 2).
           MOVE QSPC-AUTO-SEND-FMNO-1-3 TO SPC-AUTO-SEND-FMNO(1 3).
           MOVE QSPC-AUTO-SEND-FMNO-1-4 TO SPC-AUTO-SEND-FMNO(1 4).
           MOVE QSPC-AUTO-SEND-TRCD-2 TO SPC-AUTO-SEND-TRCD(2).
           MOVE QSPC-AUTO-SEND-FMNO-2-1 TO SPC-AUTO-SEND-FMNO(2 1).
           MOVE QSPC-AUTO-SEND-FMNO-2-2 TO SPC-AUTO-SEND-FMNO(2 2).
           MOVE QSPC-AUTO-SEND-FMNO-2-3 TO SPC-AUTO-SEND-FMNO(2 3).
           MOVE QSPC-AUTO-SEND-FMNO-2-4 TO SPC-AUTO-SEND-FMNO(2 4).
           MOVE QSPC-DLDISC-CONV-BF-PCNT TO SPC-DLDISC-CONV-BF-PCNT.
           MOVE QSPC-UNITPER-CD TO SPC-UNITPER-CD.
           MOVE QSPC-RENEWAL-EDIT TO SPC-RENEWAL-EDIT.
           MOVE QSPC-ROUND-MFEE TO SPC-ROUND-MFEE.
           MOVE QSPC-ADV-TO-1ST-NO-CORR TO SPC-ADV-TO-1ST-NO-CORR.
           MOVE QSPC-LC-INT-ON-REPO-NON-PL
              TO SPC-LC-INT-ON-REPO-NON-PL.
           MOVE QSPC-CONTRACTUAL-SET-NONACCRUAL
              TO SPC-CONTRACTUAL-SET-NONACCRUAL.
           MOVE QSPC-CONTRACTUAL-RESET-ACCRUAL
              TO SPC-CONTRACTUAL-RESET-ACCRUAL.
           MOVE QSPC-MONTHS-PAY-NAPP-COUNTER
              TO SPC-MONTHS-PAY-NAPP-COUNTER.
           MOVE QSPC-SHORT-PAY-THRESHOLD TO SPC-SHORT-PAY-THRESHOLD.
           MOVE QSPC-CASH-AVAIL-PERCENT TO SPC-CASH-AVAIL-PERCENT.
           MOVE QSPC-O1-JOINT-FG TO SPC-O1-JOINT-FG.
           MOVE QSPC-NET-BAL-REQ TO SPC-NET-BAL-REQ.
           MOVE QSPC-AMT-PROCEEDS-REQ TO SPC-AMT-PROCEEDS-REQ.
           MOVE QSPC-ALT-PREPAY-MONTHS TO SPC-ALT-PREPAY-MONTHS.
           MOVE QSPC-PERCENT-REGPAY-AVAIL TO SPC-PERCENT-REGPAY-AVAIL.
           MOVE QSPC-ALT-DEFFRMLA TO SPC-ALT-DEFFRMLA.
           MOVE QSPC-ALT-DEFMIN TO SPC-ALT-DEFMIN.
           MOVE QSPC-ALT-DEFMAX TO SPC-ALT-DEFMAX.
           MOVE QSPC-NON-LOAN-FLAG TO SPC-NON-LOAN-FLAG.
           MOVE QSPC-SKIP-TRW TO SPC-SKIP-TRW.
 
      *-----------------------------------------------------------------
      *    LOAD QSPC-REC FROM SPC-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-SPC-FIELDS.
           MOVE SPC-ORGST TO QSPC-ORGST.
           MOVE SPC-SPRCLASS TO QSPC-SPRCLASS.
           MOVE SPC-SUBCLASS TO QSPC-SUBCLASS.
           MOVE SPC-LAWCODE TO QSPC-LAWCODE.
           MOVE SPC-INSEARN-ORDER(1) TO QSPC-INSEARN-ORDER-1.
           MOVE SPC-INSEARN-ORDER(2) TO QSPC-INSEARN-ORDER-2.
           MOVE SPC-INSEARN-ORDER(3) TO QSPC-INSEARN-ORDER-3.
           MOVE SPC-INSEARN-ORDER(4) TO QSPC-INSEARN-ORDER-4.
           MOVE SPC-INSEARN-ORDER(5) TO QSPC-INSEARN-ORDER-5.
           MOVE SPC-INSEARN-ORDER(6) TO QSPC-INSEARN-ORDER-6.
           MOVE SPC-INSEARN-ORDER(7) TO QSPC-INSEARN-ORDER-7.
           MOVE SPC-INSEARN-ORDER(8) TO QSPC-INSEARN-ORDER-8.
           MOVE SPC-INSEARN-ORDER(9) TO QSPC-INSEARN-ORDER-9.
           MOVE SPC-INSEARN-ORDER(10) TO QSPC-INSEARN-ORDER-10.
           MOVE SPC-INSEARN-ORDER(11) TO QSPC-INSEARN-ORDER-11.
           MOVE SPC-INSEARN-ORDER(12) TO QSPC-INSEARN-ORDER-12.
           MOVE SPC-INSAH-JOINT-FG TO QSPC-INSAH-JOINT-FG.
           MOVE SPC-INS-O1-EDIT TO QSPC-INS-O1-EDIT.
           MOVE SPC-INS-O2-EDIT TO QSPC-INS-O2-EDIT.
           MOVE SPC-RBFRMLA(1) TO QSPC-RBFRMLA-1.
           MOVE SPC-RBDTE(1 1) TO QSPC-RBDTE-1-1.
           MOVE SPC-RBDAYS(1 1) TO QSPC-RBDAYS-1-1.
           MOVE SPC-RBYRTYPE(1 1) TO QSPC-RBYRTYPE-1-1.
           MOVE SPC-RBDTE(1 2) TO QSPC-RBDTE-1-2.
           MOVE SPC-RBDAYS(1 2) TO QSPC-RBDAYS-1-2.
           MOVE SPC-RBYRTYPE(1 2) TO QSPC-RBYRTYPE-1-2.
           MOVE SPC-RBMINRET(1) TO QSPC-RBMINRET-1.
           MOVE SPC-RBNOMON(1) TO QSPC-RBNOMON-1.
           MOVE SPC-RBDEDCHG(1) TO QSPC-RBDEDCHG-1.
           MOVE SPC-RBRECAST(1) TO QSPC-RBRECAST-1.
           MOVE SPC-RBEARLY(1) TO QSPC-RBEARLY-1.
           MOVE SPC-RBDEFER(1) TO QSPC-RBDEFER-1.
           MOVE SPC-RBPCT(1) TO QSPC-RBPCT-1.
           MOVE SPC-RBREDUC(1) TO QSPC-RBREDUC-1.
           MOVE SPC-RBMIN(1) TO QSPC-RBMIN-1.
           MOVE SPC-RBSPOPT1(1) TO QSPC-RBSPOPT1-1.
           MOVE SPC-RBSPOPT2(1) TO QSPC-RBSPOPT2-1.
           MOVE SPC-RBMANAGER(1) TO QSPC-RBMANAGER-1.
           MOVE SPC-RBUNITPER-CD(1) TO QSPC-RBUNITPER-CD-1.
           MOVE SPC-RBFRMLA2(1) TO QSPC-RBFRMLA2-1.
           MOVE SPC-RBFRMLA(2) TO QSPC-RBFRMLA-2.
           MOVE SPC-RBDTE(2 1) TO QSPC-RBDTE-2-1.
           MOVE SPC-RBDAYS(2 1) TO QSPC-RBDAYS-2-1.
           MOVE SPC-RBYRTYPE(2 1) TO QSPC-RBYRTYPE-2-1.
           MOVE SPC-RBDTE(2 2) TO QSPC-RBDTE-2-2.
           MOVE SPC-RBDAYS(2 2) TO QSPC-RBDAYS-2-2.
           MOVE SPC-RBYRTYPE(2 2) TO QSPC-RBYRTYPE-2-2.
           MOVE SPC-RBMINRET(2) TO QSPC-RBMINRET-2.
           MOVE SPC-RBNOMON(2) TO QSPC-RBNOMON-2.
           MOVE SPC-RBDEDCHG(2) TO QSPC-RBDEDCHG-2.
           MOVE SPC-RBRECAST(2) TO QSPC-RBRECAST-2.
           MOVE SPC-RBEARLY(2) TO QSPC-RBEARLY-2.
           MOVE SPC-RBDEFER(2) TO QSPC-RBDEFER-2.
           MOVE SPC-RBPCT(2) TO QSPC-RBPCT-2.
           MOVE SPC-RBREDUC(2) TO QSPC-RBREDUC-2.
           MOVE SPC-RBMIN(2) TO QSPC-RBMIN-2.
           MOVE SPC-RBSPOPT1(2) TO QSPC-RBSPOPT1-2.
           MOVE SPC-RBSPOPT2(2) TO QSPC-RBSPOPT2-2.
           MOVE SPC-RBMANAGER(2) TO QSPC-RBMANAGER-2.
           MOVE SPC-RBUNITPER-CD(2) TO QSPC-RBUNITPER-CD-2.
           MOVE SPC-RBFRMLA2(2) TO QSPC-RBFRMLA2-2.
           MOVE SPC-RBFRMLA(3) TO QSPC-RBFRMLA-3.
           MOVE SPC-RBDTE(3 1) TO QSPC-RBDTE-3-1.
           MOVE SPC-RBDAYS(3 1) TO QSPC-RBDAYS-3-1.
           MOVE SPC-RBYRTYPE(3 1) TO QSPC-RBYRTYPE-3-1.
           MOVE SPC-RBDTE(3 2) TO QSPC-RBDTE-3-2.
           MOVE SPC-RBDAYS(3 2) TO QSPC-RBDAYS-3-2.
           MOVE SPC-RBYRTYPE(3 2) TO QSPC-RBYRTYPE-3-2.
           MOVE SPC-RBMINRET(3) TO QSPC-RBMINRET-3.
           MOVE SPC-RBNOMON(3) TO QSPC-RBNOMON-3.
           MOVE SPC-RBDEDCHG(3) TO QSPC-RBDEDCHG-3.
           MOVE SPC-RBRECAST(3) TO QSPC-RBRECAST-3.
           MOVE SPC-RBEARLY(3) TO QSPC-RBEARLY-3.
           MOVE SPC-RBDEFER(3) TO QSPC-RBDEFER-3.
           MOVE SPC-RBPCT(3) TO QSPC-RBPCT-3.
           MOVE SPC-RBREDUC(3) TO QSPC-RBREDUC-3.
           MOVE SPC-RBMIN(3) TO QSPC-RBMIN-3.
           MOVE SPC-RBSPOPT1(3) TO QSPC-RBSPOPT1-3.
           MOVE SPC-RBSPOPT2(3) TO QSPC-RBSPOPT2-3.
           MOVE SPC-RBMANAGER(3) TO QSPC-RBMANAGER-3.
           MOVE SPC-RBUNITPER-CD(3) TO QSPC-RBUNITPER-CD-3.
           MOVE SPC-RBFRMLA2(3) TO QSPC-RBFRMLA2-3.
           MOVE SPC-RBFRMLA(4) TO QSPC-RBFRMLA-4.
           MOVE SPC-RBDTE(4 1) TO QSPC-RBDTE-4-1.
           MOVE SPC-RBDAYS(4 1) TO QSPC-RBDAYS-4-1.
           MOVE SPC-RBYRTYPE(4 1) TO QSPC-RBYRTYPE-4-1.
           MOVE SPC-RBDTE(4 2) TO QSPC-RBDTE-4-2.
           MOVE SPC-RBDAYS(4 2) TO QSPC-RBDAYS-4-2.
           MOVE SPC-RBYRTYPE(4 2) TO QSPC-RBYRTYPE-4-2.
           MOVE SPC-RBMINRET(4) TO QSPC-RBMINRET-4.
           MOVE SPC-RBNOMON(4) TO QSPC-RBNOMON-4.
           MOVE SPC-RBDEDCHG(4) TO QSPC-RBDEDCHG-4.
           MOVE SPC-RBRECAST(4) TO QSPC-RBRECAST-4.
           MOVE SPC-RBEARLY(4) TO QSPC-RBEARLY-4.
           MOVE SPC-RBDEFER(4) TO QSPC-RBDEFER-4.
           MOVE SPC-RBPCT(4) TO QSPC-RBPCT-4.
           MOVE SPC-RBREDUC(4) TO QSPC-RBREDUC-4.
           MOVE SPC-RBMIN(4) TO QSPC-RBMIN-4.
           MOVE SPC-RBSPOPT1(4) TO QSPC-RBSPOPT1-4.
           MOVE SPC-RBSPOPT2(4) TO QSPC-RBSPOPT2-4.
           MOVE SPC-RBMANAGER(4) TO QSPC-RBMANAGER-4.
           MOVE SPC-RBUNITPER-CD(4) TO QSPC-RBUNITPER-CD-4.
           MOVE SPC-RBFRMLA2(4) TO QSPC-RBFRMLA2-4.
           MOVE SPC-RBFRMLA(5) TO QSPC-RBFRMLA-5.
           MOVE SPC-RBDTE(5 1) TO QSPC-RBDTE-5-1.
           MOVE SPC-RBDAYS(5 1) TO QSPC-RBDAYS-5-1.
           MOVE SPC-RBYRTYPE(5 1) TO QSPC-RBYRTYPE-5-1.
           MOVE SPC-RBDTE(5 2) TO QSPC-RBDTE-5-2.
           MOVE SPC-RBDAYS(5 2) TO QSPC-RBDAYS-5-2.
           MOVE SPC-RBYRTYPE(5 2) TO QSPC-RBYRTYPE-5-2.
           MOVE SPC-RBMINRET(5) TO QSPC-RBMINRET-5.
           MOVE SPC-RBNOMON(5) TO QSPC-RBNOMON-5.
           MOVE SPC-RBDEDCHG(5) TO QSPC-RBDEDCHG-5.
           MOVE SPC-RBRECAST(5) TO QSPC-RBRECAST-5.
           MOVE SPC-RBEARLY(5) TO QSPC-RBEARLY-5.
           MOVE SPC-RBDEFER(5) TO QSPC-RBDEFER-5.
           MOVE SPC-RBPCT(5) TO QSPC-RBPCT-5.
           MOVE SPC-RBREDUC(5) TO QSPC-RBREDUC-5.
           MOVE SPC-RBMIN(5) TO QSPC-RBMIN-5.
           MOVE SPC-RBSPOPT1(5) TO QSPC-RBSPOPT1-5.
           MOVE SPC-RBSPOPT2(5) TO QSPC-RBSPOPT2-5.
           MOVE SPC-RBMANAGER(5) TO QSPC-RBMANAGER-5.
           MOVE SPC-RBUNITPER-CD(5) TO QSPC-RBUNITPER-CD-5.
           MOVE SPC-RBFRMLA2(5) TO QSPC-RBFRMLA2-5.
           MOVE SPC-RBFRMLA(6) TO QSPC-RBFRMLA-6.
           MOVE SPC-RBDTE(6 1) TO QSPC-RBDTE-6-1.
           MOVE SPC-RBDAYS(6 1) TO QSPC-RBDAYS-6-1.
           MOVE SPC-RBYRTYPE(6 1) TO QSPC-RBYRTYPE-6-1.
           MOVE SPC-RBDTE(6 2) TO QSPC-RBDTE-6-2.
           MOVE SPC-RBDAYS(6 2) TO QSPC-RBDAYS-6-2.
           MOVE SPC-RBYRTYPE(6 2) TO QSPC-RBYRTYPE-6-2.
           MOVE SPC-RBMINRET(6) TO QSPC-RBMINRET-6.
           MOVE SPC-RBNOMON(6) TO QSPC-RBNOMON-6.
           MOVE SPC-RBDEDCHG(6) TO QSPC-RBDEDCHG-6.
           MOVE SPC-RBRECAST(6) TO QSPC-RBRECAST-6.
           MOVE SPC-RBEARLY(6) TO QSPC-RBEARLY-6.
           MOVE SPC-RBDEFER(6) TO QSPC-RBDEFER-6.
           MOVE SPC-RBPCT(6) TO QSPC-RBPCT-6.
           MOVE SPC-RBREDUC(6) TO QSPC-RBREDUC-6.
           MOVE SPC-RBMIN(6) TO QSPC-RBMIN-6.
           MOVE SPC-RBSPOPT1(6) TO QSPC-RBSPOPT1-6.
           MOVE SPC-RBSPOPT2(6) TO QSPC-RBSPOPT2-6.
           MOVE SPC-RBMANAGER(6) TO QSPC-RBMANAGER-6.
           MOVE SPC-RBUNITPER-CD(6) TO QSPC-RBUNITPER-CD-6.
           MOVE SPC-RBFRMLA2(6) TO QSPC-RBFRMLA2-6.
           MOVE SPC-RBFRMLA(7) TO QSPC-RBFRMLA-7.
           MOVE SPC-RBDTE(7 1) TO QSPC-RBDTE-7-1.
           MOVE SPC-RBDAYS(7 1) TO QSPC-RBDAYS-7-1.
           MOVE SPC-RBYRTYPE(7 1) TO QSPC-RBYRTYPE-7-1.
           MOVE SPC-RBDTE(7 2) TO QSPC-RBDTE-7-2.
           MOVE SPC-RBDAYS(7 2) TO QSPC-RBDAYS-7-2.
           MOVE SPC-RBYRTYPE(7 2) TO QSPC-RBYRTYPE-7-2.
           MOVE SPC-RBMINRET(7) TO QSPC-RBMINRET-7.
           MOVE SPC-RBNOMON(7) TO QSPC-RBNOMON-7.
           MOVE SPC-RBDEDCHG(7) TO QSPC-RBDEDCHG-7.
           MOVE SPC-RBRECAST(7) TO QSPC-RBRECAST-7.
           MOVE SPC-RBEARLY(7) TO QSPC-RBEARLY-7.
           MOVE SPC-RBDEFER(7) TO QSPC-RBDEFER-7.
           MOVE SPC-RBPCT(7) TO QSPC-RBPCT-7.
           MOVE SPC-RBREDUC(7) TO QSPC-RBREDUC-7.
           MOVE SPC-RBMIN(7) TO QSPC-RBMIN-7.
           MOVE SPC-RBSPOPT1(7) TO QSPC-RBSPOPT1-7.
           MOVE SPC-RBSPOPT2(7) TO QSPC-RBSPOPT2-7.
           MOVE SPC-RBMANAGER(7) TO QSPC-RBMANAGER-7.
           MOVE SPC-RBUNITPER-CD(7) TO QSPC-RBUNITPER-CD-7.
           MOVE SPC-RBFRMLA2(7) TO QSPC-RBFRMLA2-7.
           MOVE SPC-RBFRMLA(8) TO QSPC-RBFRMLA-8.
           MOVE SPC-RBDTE(8 1) TO QSPC-RBDTE-8-1.
           MOVE SPC-RBDAYS(8 1) TO QSPC-RBDAYS-8-1.
           MOVE SPC-RBYRTYPE(8 1) TO QSPC-RBYRTYPE-8-1.
           MOVE SPC-RBDTE(8 2) TO QSPC-RBDTE-8-2.
           MOVE SPC-RBDAYS(8 2) TO QSPC-RBDAYS-8-2.
           MOVE SPC-RBYRTYPE(8 2) TO QSPC-RBYRTYPE-8-2.
           MOVE SPC-RBMINRET(8) TO QSPC-RBMINRET-8.
           MOVE SPC-RBNOMON(8) TO QSPC-RBNOMON-8.
           MOVE SPC-RBDEDCHG(8) TO QSPC-RBDEDCHG-8.
           MOVE SPC-RBRECAST(8) TO QSPC-RBRECAST-8.
           MOVE SPC-RBEARLY(8) TO QSPC-RBEARLY-8.
           MOVE SPC-RBDEFER(8) TO QSPC-RBDEFER-8.
           MOVE SPC-RBPCT(8) TO QSPC-RBPCT-8.
           MOVE SPC-RBREDUC(8) TO QSPC-RBREDUC-8.
           MOVE SPC-RBMIN(8) TO QSPC-RBMIN-8.
           MOVE SPC-RBSPOPT1(8) TO QSPC-RBSPOPT1-8.
           MOVE SPC-RBSPOPT2(8) TO QSPC-RBSPOPT2-8.
           MOVE SPC-RBMANAGER(8) TO QSPC-RBMANAGER-8.
           MOVE SPC-RBUNITPER-CD(8) TO QSPC-RBUNITPER-CD-8.
           MOVE SPC-RBFRMLA2(8) TO QSPC-RBFRMLA2-8.
           MOVE SPC-RBFRMLA(9) TO QSPC-RBFRMLA-9.
           MOVE SPC-RBDTE(9 1) TO QSPC-RBDTE-9-1.
           MOVE SPC-RBDAYS(9 1) TO QSPC-RBDAYS-9-1.
           MOVE SPC-RBYRTYPE(9 1) TO QSPC-RBYRTYPE-9-1.
           MOVE SPC-RBDTE(9 2) TO QSPC-RBDTE-9-2.
           MOVE SPC-RBDAYS(9 2) TO QSPC-RBDAYS-9-2.
           MOVE SPC-RBYRTYPE(9 2) TO QSPC-RBYRTYPE-9-2.
           MOVE SPC-RBMINRET(9) TO QSPC-RBMINRET-9.
           MOVE SPC-RBNOMON(9) TO QSPC-RBNOMON-9.
           MOVE SPC-RBDEDCHG(9) TO QSPC-RBDEDCHG-9.
           MOVE SPC-RBRECAST(9) TO QSPC-RBRECAST-9.
           MOVE SPC-RBEARLY(9) TO QSPC-RBEARLY-9.
           MOVE SPC-RBDEFER(9) TO QSPC-RBDEFER-9.
           MOVE SPC-RBPCT(9) TO QSPC-RBPCT-9.
           MOVE SPC-RBREDUC(9) TO QSPC-RBREDUC-9.
           MOVE SPC-RBMIN(9) TO QSPC-RBMIN-9.
           MOVE SPC-RBSPOPT1(9) TO QSPC-RBSPOPT1-9.
           MOVE SPC-RBSPOPT2(9) TO QSPC-RBSPOPT2-9.
           MOVE SPC-RBMANAGER(9) TO QSPC-RBMANAGER-9.
           MOVE SPC-RBUNITPER-CD(9) TO QSPC-RBUNITPER-CD-9.
           MOVE SPC-RBFRMLA2(9) TO QSPC-RBFRMLA2-9.
           MOVE SPC-RBFRMLA(10) TO QSPC-RBFRMLA-10.
           MOVE SPC-RBDTE(10 1) TO QSPC-RBDTE-10-1.
           MOVE SPC-RBDAYS(10 1) TO QSPC-RBDAYS-10-1.
           MOVE SPC-RBYRTYPE(10 1) TO QSPC-RBYRTYPE-10-1.
           MOVE SPC-RBDTE(10 2) TO QSPC-RBDTE-10-2.
           MOVE SPC-RBDAYS(10 2) TO QSPC-RBDAYS-10-2.
           MOVE SPC-RBYRTYPE(10 2) TO QSPC-RBYRTYPE-10-2.
           MOVE SPC-RBMINRET(10) TO QSPC-RBMINRET-10.
           MOVE SPC-RBNOMON(10) TO QSPC-RBNOMON-10.
           MOVE SPC-RBDEDCHG(10) TO QSPC-RBDEDCHG-10.
           MOVE SPC-RBRECAST(10) TO QSPC-RBRECAST-10.
           MOVE SPC-RBEARLY(10) TO QSPC-RBEARLY-10.
           MOVE SPC-RBDEFER(10) TO QSPC-RBDEFER-10.
           MOVE SPC-RBPCT(10) TO QSPC-RBPCT-10.
           MOVE SPC-RBREDUC(10) TO QSPC-RBREDUC-10.
           MOVE SPC-RBMIN(10) TO QSPC-RBMIN-10.
           MOVE SPC-RBSPOPT1(10) TO QSPC-RBSPOPT1-10.
           MOVE SPC-RBSPOPT2(10) TO QSPC-RBSPOPT2-10.
           MOVE SPC-RBMANAGER(10) TO QSPC-RBMANAGER-10.
           MOVE SPC-RBUNITPER-CD(10) TO QSPC-RBUNITPER-CD-10.
           MOVE SPC-RBFRMLA2(10) TO QSPC-RBFRMLA2-10.
           MOVE SPC-RBFRMLA(11) TO QSPC-RBFRMLA-11.
           MOVE SPC-RBDTE(11 1) TO QSPC-RBDTE-11-1.
           MOVE SPC-RBDAYS(11 1) TO QSPC-RBDAYS-11-1.
           MOVE SPC-RBYRTYPE(11 1) TO QSPC-RBYRTYPE-11-1.
           MOVE SPC-RBDTE(11 2) TO QSPC-RBDTE-11-2.
           MOVE SPC-RBDAYS(11 2) TO QSPC-RBDAYS-11-2.
           MOVE SPC-RBYRTYPE(11 2) TO QSPC-RBYRTYPE-11-2.
           MOVE SPC-RBMINRET(11) TO QSPC-RBMINRET-11.
           MOVE SPC-RBNOMON(11) TO QSPC-RBNOMON-11.
           MOVE SPC-RBDEDCHG(11) TO QSPC-RBDEDCHG-11.
           MOVE SPC-RBRECAST(11) TO QSPC-RBRECAST-11.
           MOVE SPC-RBEARLY(11) TO QSPC-RBEARLY-11.
           MOVE SPC-RBDEFER(11) TO QSPC-RBDEFER-11.
           MOVE SPC-RBPCT(11) TO QSPC-RBPCT-11.
           MOVE SPC-RBREDUC(11) TO QSPC-RBREDUC-11.
           MOVE SPC-RBMIN(11) TO QSPC-RBMIN-11.
           MOVE SPC-RBSPOPT1(11) TO QSPC-RBSPOPT1-11.
           MOVE SPC-RBSPOPT2(11) TO QSPC-RBSPOPT2-11.
           MOVE SPC-RBMANAGER(11) TO QSPC-RBMANAGER-11.
           MOVE SPC-RBUNITPER-CD(11) TO QSPC-RBUNITPER-CD-11.
           MOVE SPC-RBFRMLA2(11) TO QSPC-RBFRMLA2-11.
           MOVE SPC-RBFRMLA(12) TO QSPC-RBFRMLA-12.
           MOVE SPC-RBDTE(12 1) TO QSPC-RBDTE-12-1.
           MOVE SPC-RBDAYS(12 1) TO QSPC-RBDAYS-12-1.
           MOVE SPC-RBYRTYPE(12 1) TO QSPC-RBYRTYPE-12-1.
           MOVE SPC-RBDTE(12 2) TO QSPC-RBDTE-12-2.
           MOVE SPC-RBDAYS(12 2) TO QSPC-RBDAYS-12-2.
           MOVE SPC-RBYRTYPE(12 2) TO QSPC-RBYRTYPE-12-2.
           MOVE SPC-RBMINRET(12) TO QSPC-RBMINRET-12.
           MOVE SPC-RBNOMON(12) TO QSPC-RBNOMON-12.
           MOVE SPC-RBDEDCHG(12) TO QSPC-RBDEDCHG-12.
           MOVE SPC-RBRECAST(12) TO QSPC-RBRECAST-12.
           MOVE SPC-RBEARLY(12) TO QSPC-RBEARLY-12.
           MOVE SPC-RBDEFER(12) TO QSPC-RBDEFER-12.
           MOVE SPC-RBPCT(12) TO QSPC-RBPCT-12.
           MOVE SPC-RBREDUC(12) TO QSPC-RBREDUC-12.
           MOVE SPC-RBMIN(12) TO QSPC-RBMIN-12.
           MOVE SPC-RBSPOPT1(12) TO QSPC-RBSPOPT1-12.
           MOVE SPC-RBSPOPT2(12) TO QSPC-RBSPOPT2-12.
           MOVE SPC-RBMANAGER(12) TO QSPC-RBMANAGER-12.
           MOVE SPC-RBUNITPER-CD(12) TO QSPC-RBUNITPER-CD-12.
           MOVE SPC-RBFRMLA2(12) TO QSPC-RBFRMLA2-12.
           MOVE SPC-RBFRMLA(13) TO QSPC-RBFRMLA-13.
           MOVE SPC-RBDTE(13 1) TO QSPC-RBDTE-13-1.
           MOVE SPC-RBDAYS(13 1) TO QSPC-RBDAYS-13-1.
           MOVE SPC-RBYRTYPE(13 1) TO QSPC-RBYRTYPE-13-1.
           MOVE SPC-RBDTE(13 2) TO QSPC-RBDTE-13-2.
           MOVE SPC-RBDAYS(13 2) TO QSPC-RBDAYS-13-2.
           MOVE SPC-RBYRTYPE(13 2) TO QSPC-RBYRTYPE-13-2.
           MOVE SPC-RBMINRET(13) TO QSPC-RBMINRET-13.
           MOVE SPC-RBNOMON(13) TO QSPC-RBNOMON-13.
           MOVE SPC-RBDEDCHG(13) TO QSPC-RBDEDCHG-13.
           MOVE SPC-RBRECAST(13) TO QSPC-RBRECAST-13.
           MOVE SPC-RBEARLY(13) TO QSPC-RBEARLY-13.
           MOVE SPC-RBDEFER(13) TO QSPC-RBDEFER-13.
           MOVE SPC-RBPCT(13) TO QSPC-RBPCT-13.
           MOVE SPC-RBREDUC(13) TO QSPC-RBREDUC-13.
           MOVE SPC-RBMIN(13) TO QSPC-RBMIN-13.
           MOVE SPC-RBSPOPT1(13) TO QSPC-RBSPOPT1-13.
           MOVE SPC-RBSPOPT2(13) TO QSPC-RBSPOPT2-13.
           MOVE SPC-RBMANAGER(13) TO QSPC-RBMANAGER-13.
           MOVE SPC-RBUNITPER-CD(13) TO QSPC-RBUNITPER-CD-13.
           MOVE SPC-RBFRMLA2(13) TO QSPC-RBFRMLA2-13.
           MOVE SPC-RBFRMLA(14) TO QSPC-RBFRMLA-14.
           MOVE SPC-RBDTE(14 1) TO QSPC-RBDTE-14-1.
           MOVE SPC-RBDAYS(14 1) TO QSPC-RBDAYS-14-1.
           MOVE SPC-RBYRTYPE(14 1) TO QSPC-RBYRTYPE-14-1.
           MOVE SPC-RBDTE(14 2) TO QSPC-RBDTE-14-2.
           MOVE SPC-RBDAYS(14 2) TO QSPC-RBDAYS-14-2.
           MOVE SPC-RBYRTYPE(14 2) TO QSPC-RBYRTYPE-14-2.
           MOVE SPC-RBMINRET(14) TO QSPC-RBMINRET-14.
           MOVE SPC-RBNOMON(14) TO QSPC-RBNOMON-14.
           MOVE SPC-RBDEDCHG(14) TO QSPC-RBDEDCHG-14.
           MOVE SPC-RBRECAST(14) TO QSPC-RBRECAST-14.
           MOVE SPC-RBEARLY(14) TO QSPC-RBEARLY-14.
           MOVE SPC-RBDEFER(14) TO QSPC-RBDEFER-14.
           MOVE SPC-RBPCT(14) TO QSPC-RBPCT-14.
           MOVE SPC-RBREDUC(14) TO QSPC-RBREDUC-14.
           MOVE SPC-RBMIN(14) TO QSPC-RBMIN-14.
           MOVE SPC-RBSPOPT1(14) TO QSPC-RBSPOPT1-14.
           MOVE SPC-RBSPOPT2(14) TO QSPC-RBSPOPT2-14.
           MOVE SPC-RBMANAGER(14) TO QSPC-RBMANAGER-14.
           MOVE SPC-RBUNITPER-CD(14) TO QSPC-RBUNITPER-CD-14.
           MOVE SPC-RBFRMLA2(14) TO QSPC-RBFRMLA2-14.
           MOVE SPC-RBFRMLA(15) TO QSPC-RBFRMLA-15.
           MOVE SPC-RBDTE(15 1) TO QSPC-RBDTE-15-1.
           MOVE SPC-RBDAYS(15 1) TO QSPC-RBDAYS-15-1.
           MOVE SPC-RBYRTYPE(15 1) TO QSPC-RBYRTYPE-15-1.
           MOVE SPC-RBDTE(15 2) TO QSPC-RBDTE-15-2.
           MOVE SPC-RBDAYS(15 2) TO QSPC-RBDAYS-15-2.
           MOVE SPC-RBYRTYPE(15 2) TO QSPC-RBYRTYPE-15-2.
           MOVE SPC-RBMINRET(15) TO QSPC-RBMINRET-15.
           MOVE SPC-RBNOMON(15) TO QSPC-RBNOMON-15.
           MOVE SPC-RBDEDCHG(15) TO QSPC-RBDEDCHG-15.
           MOVE SPC-RBRECAST(15) TO QSPC-RBRECAST-15.
           MOVE SPC-RBEARLY(15) TO QSPC-RBEARLY-15.
           MOVE SPC-RBDEFER(15) TO QSPC-RBDEFER-15.
           MOVE SPC-RBPCT(15) TO QSPC-RBPCT-15.
           MOVE SPC-RBREDUC(15) TO QSPC-RBREDUC-15.
           MOVE SPC-RBMIN(15) TO QSPC-RBMIN-15.
           MOVE SPC-RBSPOPT1(15) TO QSPC-RBSPOPT1-15.
           MOVE SPC-RBSPOPT2(15) TO QSPC-RBSPOPT2-15.
           MOVE SPC-RBMANAGER(15) TO QSPC-RBMANAGER-15.
           MOVE SPC-RBUNITPER-CD(15) TO QSPC-RBUNITPER-CD-15.
           MOVE SPC-RBFRMLA2(15) TO QSPC-RBFRMLA2-15.
           MOVE SPC-RBFRMLA(16) TO QSPC-RBFRMLA-16.
           MOVE SPC-RBDTE(16 1) TO QSPC-RBDTE-16-1.
           MOVE SPC-RBDAYS(16 1) TO QSPC-RBDAYS-16-1.
           MOVE SPC-RBYRTYPE(16 1) TO QSPC-RBYRTYPE-16-1.
           MOVE SPC-RBDTE(16 2) TO QSPC-RBDTE-16-2.
           MOVE SPC-RBDAYS(16 2) TO QSPC-RBDAYS-16-2.
           MOVE SPC-RBYRTYPE(16 2) TO QSPC-RBYRTYPE-16-2.
           MOVE SPC-RBMINRET(16) TO QSPC-RBMINRET-16.
           MOVE SPC-RBNOMON(16) TO QSPC-RBNOMON-16.
           MOVE SPC-RBDEDCHG(16) TO QSPC-RBDEDCHG-16.
           MOVE SPC-RBRECAST(16) TO QSPC-RBRECAST-16.
           MOVE SPC-RBEARLY(16) TO QSPC-RBEARLY-16.
           MOVE SPC-RBDEFER(16) TO QSPC-RBDEFER-16.
           MOVE SPC-RBPCT(16) TO QSPC-RBPCT-16.
           MOVE SPC-RBREDUC(16) TO QSPC-RBREDUC-16.
           MOVE SPC-RBMIN(16) TO QSPC-RBMIN-16.
           MOVE SPC-RBSPOPT1(16) TO QSPC-RBSPOPT1-16.
           MOVE SPC-RBSPOPT2(16) TO QSPC-RBSPOPT2-16.
           MOVE SPC-RBMANAGER(16) TO QSPC-RBMANAGER-16.
           MOVE SPC-RBUNITPER-CD(16) TO QSPC-RBUNITPER-CD-16.
           MOVE SPC-RBFRMLA2(16) TO QSPC-RBFRMLA2-16.
           MOVE SPC-RBFRMLA(17) TO QSPC-RBFRMLA-17.
           MOVE SPC-RBDTE(17 1) TO QSPC-RBDTE-17-1.
           MOVE SPC-RBDAYS(17 1) TO QSPC-RBDAYS-17-1.
           MOVE SPC-RBYRTYPE(17 1) TO QSPC-RBYRTYPE-17-1.
           MOVE SPC-RBDTE(17 2) TO QSPC-RBDTE-17-2.
           MOVE SPC-RBDAYS(17 2) TO QSPC-RBDAYS-17-2.
           MOVE SPC-RBYRTYPE(17 2) TO QSPC-RBYRTYPE-17-2.
           MOVE SPC-RBMINRET(17) TO QSPC-RBMINRET-17.
           MOVE SPC-RBNOMON(17) TO QSPC-RBNOMON-17.
           MOVE SPC-RBDEDCHG(17) TO QSPC-RBDEDCHG-17.
           MOVE SPC-RBRECAST(17) TO QSPC-RBRECAST-17.
           MOVE SPC-RBEARLY(17) TO QSPC-RBEARLY-17.
           MOVE SPC-RBDEFER(17) TO QSPC-RBDEFER-17.
           MOVE SPC-RBPCT(17) TO QSPC-RBPCT-17.
           MOVE SPC-RBREDUC(17) TO QSPC-RBREDUC-17.
           MOVE SPC-RBMIN(17) TO QSPC-RBMIN-17.
           MOVE SPC-RBSPOPT1(17) TO QSPC-RBSPOPT1-17.
           MOVE SPC-RBSPOPT2(17) TO QSPC-RBSPOPT2-17.
           MOVE SPC-RBMANAGER(17) TO QSPC-RBMANAGER-17.
           MOVE SPC-RBUNITPER-CD(17) TO QSPC-RBUNITPER-CD-17.
           MOVE SPC-RBFRMLA2(17) TO QSPC-RBFRMLA2-17.
           MOVE SPC-RBFRMLA(18) TO QSPC-RBFRMLA-18.
           MOVE SPC-RBDTE(18 1) TO QSPC-RBDTE-18-1.
           MOVE SPC-RBDAYS(18 1) TO QSPC-RBDAYS-18-1.
           MOVE SPC-RBYRTYPE(18 1) TO QSPC-RBYRTYPE-18-1.
           MOVE SPC-RBDTE(18 2) TO QSPC-RBDTE-18-2.
           MOVE SPC-RBDAYS(18 2) TO QSPC-RBDAYS-18-2.
           MOVE SPC-RBYRTYPE(18 2) TO QSPC-RBYRTYPE-18-2.
           MOVE SPC-RBMINRET(18) TO QSPC-RBMINRET-18.
           MOVE SPC-RBNOMON(18) TO QSPC-RBNOMON-18.
           MOVE SPC-RBDEDCHG(18) TO QSPC-RBDEDCHG-18.
           MOVE SPC-RBRECAST(18) TO QSPC-RBRECAST-18.
           MOVE SPC-RBEARLY(18) TO QSPC-RBEARLY-18.
           MOVE SPC-RBDEFER(18) TO QSPC-RBDEFER-18.
           MOVE SPC-RBPCT(18) TO QSPC-RBPCT-18.
           MOVE SPC-RBREDUC(18) TO QSPC-RBREDUC-18.
           MOVE SPC-RBMIN(18) TO QSPC-RBMIN-18.
           MOVE SPC-RBSPOPT1(18) TO QSPC-RBSPOPT1-18.
           MOVE SPC-RBSPOPT2(18) TO QSPC-RBSPOPT2-18.
           MOVE SPC-RBMANAGER(18) TO QSPC-RBMANAGER-18.
           MOVE SPC-RBUNITPER-CD(18) TO QSPC-RBUNITPER-CD-18.
           MOVE SPC-RBFRMLA2(18) TO QSPC-RBFRMLA2-18.
           MOVE SPC-RBFRMLA(19) TO QSPC-RBFRMLA-19.
           MOVE SPC-RBDTE(19 1) TO QSPC-RBDTE-19-1.
           MOVE SPC-RBDAYS(19 1) TO QSPC-RBDAYS-19-1.
           MOVE SPC-RBYRTYPE(19 1) TO QSPC-RBYRTYPE-19-1.
           MOVE SPC-RBDTE(19 2) TO QSPC-RBDTE-19-2.
           MOVE SPC-RBDAYS(19 2) TO QSPC-RBDAYS-19-2.
           MOVE SPC-RBYRTYPE(19 2) TO QSPC-RBYRTYPE-19-2.
           MOVE SPC-RBMINRET(19) TO QSPC-RBMINRET-19.
           MOVE SPC-RBNOMON(19) TO QSPC-RBNOMON-19.
           MOVE SPC-RBDEDCHG(19) TO QSPC-RBDEDCHG-19.
           MOVE SPC-RBRECAST(19) TO QSPC-RBRECAST-19.
           MOVE SPC-RBEARLY(19) TO QSPC-RBEARLY-19.
           MOVE SPC-RBDEFER(19) TO QSPC-RBDEFER-19.
           MOVE SPC-RBPCT(19) TO QSPC-RBPCT-19.
           MOVE SPC-RBREDUC(19) TO QSPC-RBREDUC-19.
           MOVE SPC-RBMIN(19) TO QSPC-RBMIN-19.
           MOVE SPC-RBSPOPT1(19) TO QSPC-RBSPOPT1-19.
           MOVE SPC-RBSPOPT2(19) TO QSPC-RBSPOPT2-19.
           MOVE SPC-RBMANAGER(19) TO QSPC-RBMANAGER-19.
           MOVE SPC-RBUNITPER-CD(19) TO QSPC-RBUNITPER-CD-19.
           MOVE SPC-RBFRMLA2(19) TO QSPC-RBFRMLA2-19.
           MOVE SPC-RBFRMLA(20) TO QSPC-RBFRMLA-20.
           MOVE SPC-RBDTE(20 1) TO QSPC-RBDTE-20-1.
           MOVE SPC-RBDAYS(20 1) TO QSPC-RBDAYS-20-1.
           MOVE SPC-RBYRTYPE(20 1) TO QSPC-RBYRTYPE-20-1.
           MOVE SPC-RBDTE(20 2) TO QSPC-RBDTE-20-2.
           MOVE SPC-RBDAYS(20 2) TO QSPC-RBDAYS-20-2.
           MOVE SPC-RBYRTYPE(20 2) TO QSPC-RBYRTYPE-20-2.
           MOVE SPC-RBMINRET(20) TO QSPC-RBMINRET-20.
           MOVE SPC-RBNOMON(20) TO QSPC-RBNOMON-20.
           MOVE SPC-RBDEDCHG(20) TO QSPC-RBDEDCHG-20.
           MOVE SPC-RBRECAST(20) TO QSPC-RBRECAST-20.
           MOVE SPC-RBEARLY(20) TO QSPC-RBEARLY-20.
           MOVE SPC-RBDEFER(20) TO QSPC-RBDEFER-20.
           MOVE SPC-RBPCT(20) TO QSPC-RBPCT-20.
           MOVE SPC-RBREDUC(20) TO QSPC-RBREDUC-20.
           MOVE SPC-RBMIN(20) TO QSPC-RBMIN-20.
           MOVE SPC-RBSPOPT1(20) TO QSPC-RBSPOPT1-20.
           MOVE SPC-RBSPOPT2(20) TO QSPC-RBSPOPT2-20.
           MOVE SPC-RBMANAGER(20) TO QSPC-RBMANAGER-20.
           MOVE SPC-RBUNITPER-CD(20) TO QSPC-RBUNITPER-CD-20.
           MOVE SPC-RBFRMLA2(20) TO QSPC-RBFRMLA2-20.
           MOVE SPC-RBFRMLA(21) TO QSPC-RBFRMLA-21.
           MOVE SPC-RBDTE(21 1) TO QSPC-RBDTE-21-1.
           MOVE SPC-RBDAYS(21 1) TO QSPC-RBDAYS-21-1.
           MOVE SPC-RBYRTYPE(21 1) TO QSPC-RBYRTYPE-21-1.
           MOVE SPC-RBDTE(21 2) TO QSPC-RBDTE-21-2.
           MOVE SPC-RBDAYS(21 2) TO QSPC-RBDAYS-21-2.
           MOVE SPC-RBYRTYPE(21 2) TO QSPC-RBYRTYPE-21-2.
           MOVE SPC-RBMINRET(21) TO QSPC-RBMINRET-21.
           MOVE SPC-RBNOMON(21) TO QSPC-RBNOMON-21.
           MOVE SPC-RBDEDCHG(21) TO QSPC-RBDEDCHG-21.
           MOVE SPC-RBRECAST(21) TO QSPC-RBRECAST-21.
           MOVE SPC-RBEARLY(21) TO QSPC-RBEARLY-21.
           MOVE SPC-RBDEFER(21) TO QSPC-RBDEFER-21.
           MOVE SPC-RBPCT(21) TO QSPC-RBPCT-21.
           MOVE SPC-RBREDUC(21) TO QSPC-RBREDUC-21.
           MOVE SPC-RBMIN(21) TO QSPC-RBMIN-21.
           MOVE SPC-RBSPOPT1(21) TO QSPC-RBSPOPT1-21.
           MOVE SPC-RBSPOPT2(21) TO QSPC-RBSPOPT2-21.
           MOVE SPC-RBMANAGER(21) TO QSPC-RBMANAGER-21.
           MOVE SPC-RBUNITPER-CD(21) TO QSPC-RBUNITPER-CD-21.
           MOVE SPC-RBFRMLA2(21) TO QSPC-RBFRMLA2-21.
           MOVE SPC-RBFRMLA(22) TO QSPC-RBFRMLA-22.
           MOVE SPC-RBDTE(22 1) TO QSPC-RBDTE-22-1.
           MOVE SPC-RBDAYS(22 1) TO QSPC-RBDAYS-22-1.
           MOVE SPC-RBYRTYPE(22 1) TO QSPC-RBYRTYPE-22-1.
           MOVE SPC-RBDTE(22 2) TO QSPC-RBDTE-22-2.
           MOVE SPC-RBDAYS(22 2) TO QSPC-RBDAYS-22-2.
           MOVE SPC-RBYRTYPE(22 2) TO QSPC-RBYRTYPE-22-2.
           MOVE SPC-RBMINRET(22) TO QSPC-RBMINRET-22.
           MOVE SPC-RBNOMON(22) TO QSPC-RBNOMON-22.
           MOVE SPC-RBDEDCHG(22) TO QSPC-RBDEDCHG-22.
           MOVE SPC-RBRECAST(22) TO QSPC-RBRECAST-22.
           MOVE SPC-RBEARLY(22) TO QSPC-RBEARLY-22.
           MOVE SPC-RBDEFER(22) TO QSPC-RBDEFER-22.
           MOVE SPC-RBPCT(22) TO QSPC-RBPCT-22.
           MOVE SPC-RBREDUC(22) TO QSPC-RBREDUC-22.
           MOVE SPC-RBMIN(22) TO QSPC-RBMIN-22.
           MOVE SPC-RBSPOPT1(22) TO QSPC-RBSPOPT1-22.
           MOVE SPC-RBSPOPT2(22) TO QSPC-RBSPOPT2-22.
           MOVE SPC-RBMANAGER(22) TO QSPC-RBMANAGER-22.
           MOVE SPC-RBUNITPER-CD(22) TO QSPC-RBUNITPER-CD-22.
           MOVE SPC-RBFRMLA2(22) TO QSPC-RBFRMLA2-22.
           MOVE SPC-RBCOMCD TO QSPC-RBCOMCD.
           MOVE SPC-ADDON-DISFRMLA TO QSPC-ADDON-DISFRMLA.
           MOVE SPC-ADDON-WHOLE-FG TO QSPC-ADDON-WHOLE-FG.
           MOVE SPC-ADDON-ORG-MATURITY TO QSPC-ADDON-ORG-MATURITY.
           MOVE SPC-CONSOLIDATE-RT-FG TO QSPC-CONSOLIDATE-RT-FG.
           MOVE SPC-1STPY-ADDON-DAYS TO QSPC-1STPY-ADDON-DAYS.
           MOVE SPC-1STPY-ADDON-YRTYPE TO QSPC-1STPY-ADDON-YRTYPE.
           MOVE SPC-ADDON-RATE-FG TO QSPC-ADDON-RATE-FG.
           MOVE SPC-ADDON-RATE TO QSPC-ADDON-RATE.
           MOVE SPC-FEEFRMLA(1) TO QSPC-FEEFRMLA-1.
           MOVE SPC-FEECD(1) TO QSPC-FEECD-1.
           MOVE SPC-FEEID(1) TO QSPC-FEEID-1.
           MOVE SPC-FEEFRMLA(2) TO QSPC-FEEFRMLA-2.
           MOVE SPC-FEECD(2) TO QSPC-FEECD-2.
           MOVE SPC-FEEID(2) TO QSPC-FEEID-2.
           MOVE SPC-ROUND-PAY-FG TO QSPC-ROUND-PAY-FG.
           MOVE SPC-IRREGULAR-PAY-FG TO QSPC-IRREGULAR-PAY-FG.
           MOVE SPC-DLRVCHGS-RB-CD(1) TO QSPC-DLRVCHGS-RB-CD-1.
           MOVE SPC-DLRVCHGS-RB-CD(2) TO QSPC-DLRVCHGS-RB-CD-2.
           MOVE SPC-DLRVCHGS-RB-CD(3) TO QSPC-DLRVCHGS-RB-CD-3.
           MOVE SPC-PARVHELD-RB-CD TO QSPC-PARVHELD-RB-CD.
           MOVE SPC-PARVPAID-RB-CD TO QSPC-PARVPAID-RB-CD.
           MOVE SPC-CONTR-CREDIT TO QSPC-CONTR-CREDIT.
           MOVE SPC-CONTR-CREDIT-CD TO QSPC-CONTR-CREDIT-CD.
           MOVE SPC-CONTR-CREDIT-MIN-DEL-PER
            TO QSPC-CONTR-CREDIT-MIN-DEL-PER.
           MOVE SPC-OPT-CAL-CHGABLE TO QSPC-OPT-CAL-CHGABLE.
           MOVE SPC-PBDEL-DAYS TO QSPC-PBDEL-DAYS.
           MOVE SPC-PBINCR-PERCENT TO QSPC-PBINCR-PERCENT.
           MOVE SPC-CPOT30-CD TO QSPC-CPOT30-CD.
           MOVE SPC-CPOTCHGOFF-CD TO QSPC-CPOTCHGOFF-CD.
           MOVE SPC-RDPOT30-CD TO QSPC-RDPOT30-CD.
           MOVE SPC-RDPOTCHGOFF-CD TO QSPC-RDPOTCHGOFF-CD.
           MOVE SPC-MAX-ALLOW-DELINQ TO QSPC-MAX-ALLOW-DELINQ.
           MOVE SPC-RESCIND-BUS-DAYS TO QSPC-RESCIND-BUS-DAYS.
           MOVE SPC-RESCIND-SATURDAY TO QSPC-RESCIND-SATURDAY.
           MOVE SPC-MIN-CREDIT-SCORE TO QSPC-MIN-CREDIT-SCORE.
           MOVE SPC-DUAL-IN-BRANCH TO QSPC-DUAL-IN-BRANCH.
           MOVE SPC-DUAL-IN-STATE TO QSPC-DUAL-IN-STATE.
           MOVE SPC-PACESETTER-FG TO QSPC-PACESETTER-FG.
           MOVE SPC-WAGE-ASSIGN-DAYS TO QSPC-WAGE-ASSIGN-DAYS.
           MOVE SPC-CAL-PAYMENT-ROUND-FG TO QSPC-CAL-PAYMENT-ROUND-FG.
           MOVE SPC-CAN-LOANS-FG TO QSPC-CAN-LOANS-FG.
           MOVE SPC-ALLOW-DELINQ-POLCD TO QSPC-ALLOW-DELINQ-POLCD.
           MOVE SPC-YTD-MAX-ALLOW-DELINQ TO QSPC-YTD-MAX-ALLOW-DELINQ.
           MOVE SPC-LNCOST-FRMLA TO QSPC-LNCOST-FRMLA.
           MOVE SPC-LNCOST-AMT TO QSPC-LNCOST-AMT.
           MOVE SPC-DLDISC-RVPRIN1-PCNT TO QSPC-DLDISC-RVPRIN1-PCNT.
           MOVE SPC-RENEWABLE-PAYS TO QSPC-RENEWABLE-PAYS.
           MOVE SPC-RENEWABLE-DAYS TO QSPC-RENEWABLE-DAYS.
           MOVE SPC-ALLOW-LN-BACK-DATE TO QSPC-ALLOW-LN-BACK-DATE.
           MOVE SPC-DLDISC-RVPRIN1-AF-PCNT
            TO QSPC-DLDISC-RVPRIN1-AF-PCNT.
           MOVE SPC-CAL-MAX-AT-ESTIMATE TO QSPC-CAL-MAX-AT-ESTIMATE.
           MOVE SPC-INSAH-RETRO TO QSPC-INSAH-RETRO.
           MOVE SPC-INSAH-DAYS-WAIT TO QSPC-INSAH-DAYS-WAIT.
           MOVE SPC-ADDON-EXCEPT-FG TO QSPC-ADDON-EXCEPT-FG.
           MOVE SPC-REPO-TRCD(1) TO QSPC-REPO-TRCD-1.
           MOVE SPC-REPO-FMNO(1 1) TO QSPC-REPO-FMNO-1-1.
           MOVE SPC-REPO-FMNO(1 2) TO QSPC-REPO-FMNO-1-2.
           MOVE SPC-REPO-FMNO(1 3) TO QSPC-REPO-FMNO-1-3.
           MOVE SPC-REPO-FMNO(1 4) TO QSPC-REPO-FMNO-1-4.
           MOVE SPC-REPO-TRCD(2) TO QSPC-REPO-TRCD-2.
           MOVE SPC-REPO-FMNO(2 1) TO QSPC-REPO-FMNO-2-1.
           MOVE SPC-REPO-FMNO(2 2) TO QSPC-REPO-FMNO-2-2.
           MOVE SPC-REPO-FMNO(2 3) TO QSPC-REPO-FMNO-2-3.
           MOVE SPC-REPO-FMNO(2 4) TO QSPC-REPO-FMNO-2-4.
           MOVE SPC-REPO-TRCD(3) TO QSPC-REPO-TRCD-3.
           MOVE SPC-REPO-FMNO(3 1) TO QSPC-REPO-FMNO-3-1.
           MOVE SPC-REPO-FMNO(3 2) TO QSPC-REPO-FMNO-3-2.
           MOVE SPC-REPO-FMNO(3 3) TO QSPC-REPO-FMNO-3-3.
           MOVE SPC-REPO-FMNO(3 4) TO QSPC-REPO-FMNO-3-4.
           MOVE SPC-CANCEL-ADDON-FG TO QSPC-CANCEL-ADDON-FG.
           MOVE SPC-DEFAULT-O2-INS-COMP TO QSPC-DEFAULT-O2-INS-COMP.
           MOVE SPC-LC-ON-JUDGEMENTS TO QSPC-LC-ON-JUDGEMENTS.
           MOVE SPC-DELQ-IB-AGE TO QSPC-DELQ-IB-AGE.
           MOVE SPC-DELQ-IB-MAX-DAYS TO QSPC-DELQ-IB-MAX-DAYS.
           MOVE SPC-INSURANCE-WRITTEN TO QSPC-INSURANCE-WRITTEN.
           MOVE SPC-INTERIM-COUPON TO QSPC-INTERIM-COUPON.
           MOVE SPC-AUTO-ASSESS-LCHG TO QSPC-AUTO-ASSESS-LCHG.
           MOVE SPC-MAX-CASH-PROCEEDS TO QSPC-MAX-CASH-PROCEEDS.
           MOVE SPC-ALLOW-ZERO-PROC TO QSPC-ALLOW-ZERO-PROC.
           MOVE SPC-BANKRULE-RATE TO QSPC-BANKRULE-RATE.
           MOVE SPC-AUTO-SEND-TRCD(1) TO QSPC-AUTO-SEND-TRCD-1.
           MOVE SPC-AUTO-SEND-FMNO(1 1) TO QSPC-AUTO-SEND-FMNO-1-1.
           MOVE SPC-AUTO-SEND-FMNO(1 2) TO QSPC-AUTO-SEND-FMNO-1-2.
           MOVE SPC-AUTO-SEND-FMNO(1 3) TO QSPC-AUTO-SEND-FMNO-1-3.
           MOVE SPC-AUTO-SEND-FMNO(1 4) TO QSPC-AUTO-SEND-FMNO-1-4.
           MOVE SPC-AUTO-SEND-TRCD(2) TO QSPC-AUTO-SEND-TRCD-2.
           MOVE SPC-AUTO-SEND-FMNO(2 1) TO QSPC-AUTO-SEND-FMNO-2-1.
           MOVE SPC-AUTO-SEND-FMNO(2 2) TO QSPC-AUTO-SEND-FMNO-2-2.
           MOVE SPC-AUTO-SEND-FMNO(2 3) TO QSPC-AUTO-SEND-FMNO-2-3.
           MOVE SPC-AUTO-SEND-FMNO(2 4) TO QSPC-AUTO-SEND-FMNO-2-4.
           MOVE SPC-DLDISC-CONV-BF-PCNT TO QSPC-DLDISC-CONV-BF-PCNT.
           MOVE SPC-UNITPER-CD TO QSPC-UNITPER-CD.
           MOVE SPC-RENEWAL-EDIT TO QSPC-RENEWAL-EDIT.
           MOVE SPC-ROUND-MFEE TO QSPC-ROUND-MFEE.
           MOVE SPC-ADV-TO-1ST-NO-CORR TO QSPC-ADV-TO-1ST-NO-CORR.
           MOVE SPC-LC-INT-ON-REPO-NON-PL
            TO QSPC-LC-INT-ON-REPO-NON-PL.
           MOVE SPC-CONTRACTUAL-SET-NONACCRUAL
            TO QSPC-CONTRACTUAL-SET-NONACCRUAL.
           MOVE SPC-CONTRACTUAL-RESET-ACCRUAL
            TO QSPC-CONTRACTUAL-RESET-ACCRUAL.
           MOVE SPC-MONTHS-PAY-NAPP-COUNTER
            TO QSPC-MONTHS-PAY-NAPP-COUNTER.
           MOVE SPC-SHORT-PAY-THRESHOLD TO QSPC-SHORT-PAY-THRESHOLD.
           MOVE SPC-CASH-AVAIL-PERCENT TO QSPC-CASH-AVAIL-PERCENT.
           MOVE SPC-O1-JOINT-FG TO QSPC-O1-JOINT-FG.
           MOVE SPC-NET-BAL-REQ TO QSPC-NET-BAL-REQ.
           MOVE SPC-AMT-PROCEEDS-REQ TO QSPC-AMT-PROCEEDS-REQ.
           MOVE SPC-ALT-PREPAY-MONTHS TO QSPC-ALT-PREPAY-MONTHS.
           MOVE SPC-PERCENT-REGPAY-AVAIL TO QSPC-PERCENT-REGPAY-AVAIL.
           MOVE SPC-ALT-DEFFRMLA TO QSPC-ALT-DEFFRMLA.
           MOVE SPC-ALT-DEFMIN TO QSPC-ALT-DEFMIN.
           MOVE SPC-ALT-DEFMAX TO QSPC-ALT-DEFMAX.
           MOVE SPC-NON-LOAN-FLAG TO QSPC-NON-LOAN-FLAG.
           MOVE SPC-SKIP-TRW TO QSPC-SKIP-TRW.
 
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBLP/LPSP1RN
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
       START-SP1-FILE-NOT-GREATER.
           PERFORM SET-SP1-TO-ALL.
           PERFORM START-SPA1-FILE-NOT-GREATER.
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
       READ-SP1-FILE-PREVIOUS.
           PERFORM SET-SP1-TO-ALL.
           PERFORM READ-SPA1-FILE-PREVIOUS.
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
       WRITE-SP1-FILE.

           PERFORM COMPUTE-SP-TABLE-NUMS.
           PERFORM SET-SP1-TO-ALL.
           PERFORM WRITE-SPA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM WRITE-SPB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM WRITE-SPC1-FILE
                 IF ( IO-FG = 0 )
                    PERFORM SET-ALL-TO-SP1
                 ELSE
                    MOVE "C" TO IO-MULTI-FG
              ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       REWRITE-SP1-FILE.

           PERFORM COMPUTE-SP-TABLE-NUMS.
           PERFORM SET-SP1-TO-ALL.
           PERFORM REWRITE-SPA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM REWRITE-SPB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM REWRITE-SPC1-FILE
                 IF ( IO-FG = 0 )
                    PERFORM SET-ALL-TO-SP1
                 ELSE
                    MOVE "C" TO IO-MULTI-FG
              ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       DELETE-SP1-FILE.

           PERFORM SET-SP1-TO-ALL.
           PERFORM DELETE-SPA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM DELETE-SPB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM DELETE-SPC1-FILE
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

*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*G2G  *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-SPAFILE NOT = "S" )
*G2G          MOVE SPA-PATH-BEG          TO SPA-SELECT-PATH-BEG
*G2G          MOVE SPA-PATH-FILE         TO SPA-SELECT-PATH-FILE
*G2G       ELSE
*G2G          MOVE SPA-PATH-SQL          TO SPA-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-SPA1-FILE.
           PERFORM LOAD-SPA1-FILE.
           PERFORM OPEN-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SPA1-FILE.
           PERFORM START-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.

           IF ( SPA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SPA1-FILE.

           IF ( QSP1-WEND-LAWCODE = 0  ) OR
              ( QSP1-WEND-LAWCODE NOT NUMERIC)
              MOVE SPA-ORGST            TO QSP1-WBEG-ORGST 
              MOVE ALL "Z"              TO QSP1-WEND-ORGST 
              MOVE SPA-SPRCLASS         TO QSP1-WBEG-SPRCLASS
              MOVE ALL "9"              TO QSP1-WEND-SPRCLASS
              MOVE SPA-SUBCLASS         TO QSP1-WBEG-SUBCLASS
              MOVE ALL "9"              TO QSP1-WEND-SUBCLASS
              MOVE SPA-LAWCODE          TO QSP1-WBEG-LAWCODE
              MOVE ALL "9"              TO QSP1-WEND-LAWCODE.

           EXEC SQL
            DECLARE SPA1_CURSOR CURSOR FOR
            SELECT ISNULL(SPAFILE.SPA_ORGST,''),
                   SPAFILE.SPA_SPRCLASS,
                   SPAFILE.SPA_SUBCLASS,
                   SPAFILE.SPA_LAWCODE,
                   ISNULL(CAST(SPAFILE.SPA_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(SPAFILE.SPA_DESC,''),
                   ISNULL(SPAFILE.SPA_NOTES,''),
                   ISNULL(CAST(SPAFILE.SPA_EFFDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(SPAFILE.SPA_EXPDATE
                                                  AS VARCHAR(10)),'0'),
                   SPAFILE.SPA_EXPD_MONTHS,
                   SPAFILE.SPA_EXPD_DAYS,
                   ISNULL(SPAFILE.SPA_EXPD_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPD_EXCEPT,''),
                   ISNULL(SPAFILE.SPA_EXPR_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPR_REDU_FRMLA,''),
                   SPAFILE.SPA_EXPR_RATE_1,
                   SPAFILE.SPA_EXPR_BREAK_1,
                   SPAFILE.SPA_EXPR_YRTYPE_1,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_1,''),
                   SPAFILE.SPA_EXPR_RATE_2,
                   SPAFILE.SPA_EXPR_BREAK_2,
                   SPAFILE.SPA_EXPR_YRTYPE_2,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_2,''),
                   SPAFILE.SPA_EXPR_RATE_3,
                   SPAFILE.SPA_EXPR_BREAK_3,
                   SPAFILE.SPA_EXPR_YRTYPE_3,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_3,''),
                   SPAFILE.SPA_EXPR_RATE_4,
                   SPAFILE.SPA_EXPR_BREAK_4,
                   SPAFILE.SPA_EXPR_YRTYPE_4,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_4,''),
                   SPAFILE.SPA_EXPR_RATE_5,
                   SPAFILE.SPA_EXPR_BREAK_5,
                   SPAFILE.SPA_EXPR_YRTYPE_5,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_5,''),
                   ISNULL(SPAFILE.SPA_EXPR_LCHG,''),
                   ISNULL(SPAFILE.SPA_JUDGFRMLA,''),
                   SPAFILE.SPA_JUDGRATE,
                   SPAFILE.SPA_JUDGYRTYPE,
                   ISNULL(SPAFILE.SPA_JUDGINTMETH,''),
                   ISNULL(SPAFILE.SPA_ACC_FRMLA,''),
                   SPAFILE.SPA_ACC_YRTYPE,
                   ISNULL(SPAFILE.SPA_ACC_INTMETH,''),
                   SPAFILE.SPA_ACC_RATE_1,
                   SPAFILE.SPA_ACC_BREAK_1,
                   SPAFILE.SPA_ACC_RATE_2,
                   SPAFILE.SPA_ACC_BREAK_2,
                   SPAFILE.SPA_ACC_RATE_3,
                   SPAFILE.SPA_ACC_BREAK_3,
                   SPAFILE.SPA_ACC_RATE_4,
                   SPAFILE.SPA_ACC_BREAK_4,
                   SPAFILE.SPA_ACC_RATE_5,
                   SPAFILE.SPA_ACC_BREAK_5,
                   ISNULL(SPAFILE.SPA_1098FRMLA,''),
                   ISNULL(SPAFILE.SPA_DISFRMLA,''),
                   ISNULL(SPAFILE.SPA_CONTRFRMLA,''),
                   ISNULL(SPAFILE.SPA_BANKRULE,''),
                   SPAFILE.SPA_CONTPDTH,
                   SPAFILE.SPA_FINCHG,
                   ISNULL(SPAFILE.SPA_BANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_TRWBANKRULE,''),
                   SPAFILE.SPA_TRWCONTPDTH,
                   ISNULL(SPAFILE.SPA_TRWBANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_RECFRMLA,''),
                   ISNULL(SPAFILE.SPA_RECPAYCD,''),
                   SPAFILE.SPA_RECPDTH,
                   ISNULL(SPAFILE.SPA_DISFRMLA_SPAY_FG,''),
                   SPAFILE.SPA_ERNFRMLA_1,
                   SPAFILE.SPA_ERNAMT_1,
                   SPAFILE.SPA_ERNPERCT_1,
                   ISNULL(SPAFILE.SPA_ERNPERCD_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_1,''),
                   SPAFILE.SPA_ERN_DAYS_1,
                   SPAFILE.SPA_ERN_TERM_1,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_1,''),
                   SPAFILE.SPA_ERNFRMLA_2,
                   SPAFILE.SPA_ERNAMT_2,
                   SPAFILE.SPA_ERNPERCT_2,
                   ISNULL(SPAFILE.SPA_ERNPERCD_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_2,''),
                   SPAFILE.SPA_ERN_DAYS_2,
                   SPAFILE.SPA_ERN_TERM_2,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_2,''),
                   SPAFILE.SPA_ERNFRMLA_3,
                   SPAFILE.SPA_ERNAMT_3,
                   SPAFILE.SPA_ERNPERCT_3,
                   ISNULL(SPAFILE.SPA_ERNPERCD_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_3,''),
                   SPAFILE.SPA_ERN_DAYS_3,
                   SPAFILE.SPA_ERN_TERM_3,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_3,''),
                   SPAFILE.SPA_ERNFRMLA_4,
                   SPAFILE.SPA_ERNAMT_4,
                   SPAFILE.SPA_ERNPERCT_4,
                   ISNULL(SPAFILE.SPA_ERNPERCD_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_4,''),
                   SPAFILE.SPA_ERN_DAYS_4,
                   SPAFILE.SPA_ERN_TERM_4,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_4,''),
                   SPAFILE.SPA_ERNFRMLA_5,
                   SPAFILE.SPA_ERNAMT_5,
                   SPAFILE.SPA_ERNPERCT_5,
                   ISNULL(SPAFILE.SPA_ERNPERCD_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_5,''),
                   SPAFILE.SPA_ERN_DAYS_5,
                   SPAFILE.SPA_ERN_TERM_5,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_5,''),
                   SPAFILE.SPA_ERNFRMLA_6,
                   SPAFILE.SPA_ERNAMT_6,
                   SPAFILE.SPA_ERNPERCT_6,
                   ISNULL(SPAFILE.SPA_ERNPERCD_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_6,''),
                   SPAFILE.SPA_ERN_DAYS_6,
                   SPAFILE.SPA_ERN_TERM_6,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_6,''),
                   SPAFILE.SPA_ERNFRMLA_7,
                   SPAFILE.SPA_ERNAMT_7,
                   SPAFILE.SPA_ERNPERCT_7,
                   ISNULL(SPAFILE.SPA_ERNPERCD_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_7,''),
                   SPAFILE.SPA_ERN_DAYS_7,
                   SPAFILE.SPA_ERN_TERM_7,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_7,''),
                   SPAFILE.SPA_ERNFRMLA_8,
                   SPAFILE.SPA_ERNAMT_8,
                   SPAFILE.SPA_ERNPERCT_8,
                   ISNULL(SPAFILE.SPA_ERNPERCD_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_8,''),
                   SPAFILE.SPA_ERN_DAYS_8,
                   SPAFILE.SPA_ERN_TERM_8,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_8,''),
                   SPAFILE.SPA_ERNFRMLA_9,
                   SPAFILE.SPA_ERNAMT_9,
                   SPAFILE.SPA_ERNPERCT_9,
                   ISNULL(SPAFILE.SPA_ERNPERCD_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_9,''),
                   SPAFILE.SPA_ERN_DAYS_9,
                   SPAFILE.SPA_ERN_TERM_9,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_9,''),
                   SPAFILE.SPA_ERNFRMLA_10,
                   SPAFILE.SPA_ERNAMT_10,
                   SPAFILE.SPA_ERNPERCT_10,
                   ISNULL(SPAFILE.SPA_ERNPERCD_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_10,''),
                   SPAFILE.SPA_ERN_DAYS_10,
                   SPAFILE.SPA_ERN_TERM_10,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCEL_CD,''),
                   ISNULL(SPAFILE.SPA_ERNJD_CD,''),
                   ISNULL(SPAFILE.SPA_MIDSTRERNCD,''),
                   SPAFILE.SPA_EARN_ORDER_1,
                   SPAFILE.SPA_EARN_ORDER_2,
                   SPAFILE.SPA_EARN_ORDER_3,
                   SPAFILE.SPA_EARN_ORDER_4,
                   SPAFILE.SPA_EARN_ORDER_5,
                   SPAFILE.SPA_EARN_ORDER_6,
                   SPAFILE.SPA_EARN_ORDER_7,
                   SPAFILE.SPA_EARN_ORDER_8,
                   SPAFILE.SPA_EARN_ORDER_9,
                   SPAFILE.SPA_EARN_ORDER_10,
                   ISNULL(SPAFILE.SPA_ERNDELQFRMLA,''),
                   SPAFILE.SPA_ERNDELQ_START,
                   SPAFILE.SPA_ERNDELQ_STOP,
                   ISNULL(SPAFILE.SPA_1098_INC_LC,''),
                   SPAFILE.SPA_MAX_IB_ACR_DAYS,
                   SPAFILE.SPA_LCGRACE,
                   ISNULL(SPAFILE.SPA_LCGRACECD,''),
                   ISNULL(SPAFILE.SPA_LCTYPE,''),
                   SPAFILE.SPA_LCRATE,
                   ISNULL(SPAFILE.SPA_LCFRMLA,''),
                   SPAFILE.SPA_LCMINCHG,
                   SPAFILE.SPA_LCMAXAMT,
                   SPAFILE.SPA_LCDELFAC,
                   SPAFILE.SPA_LCYRTYPE,
                   SPAFILE.SPA_LC_CODE,
                   ISNULL(SPAFILE.SPA_LCUNITPER_CD,''),
                   SPAFILE.SPA_DEFFRMLA,
                   ISNULL(SPAFILE.SPA_DEFEXCEPT,''),
                   SPAFILE.SPA_DEFWORKER,
                   SPAFILE.SPA_DEFPERCD,
                   SPAFILE.SPA_DEFBASE,
                   ISNULL(SPAFILE.SPA_DEFPOLCD,''),
                   SPAFILE.SPA_DEFPOLMX_1,
                   SPAFILE.SPA_DEFPOLMX_2,
                   SPAFILE.SPA_DEFPOLMX_3,
                   SPAFILE.SPA_DEFMIN,
                   SPAFILE.SPA_DEFMAX,
                   ISNULL(SPAFILE.SPA_DEFUNITPER_CD,''),
                   SPAFILE.SPA_PRFORM_1,
                   ISNULL(SPAFILE.SPA_PRFLAG_1,''),
                   SPAFILE.SPA_PRFORM_2,
                   ISNULL(SPAFILE.SPA_PRFLAG_2,''),
                   SPAFILE.SPA_PRFORM_3,
                   ISNULL(SPAFILE.SPA_PRFLAG_3,''),
                   SPAFILE.SPA_PRFORM_4,
                   ISNULL(SPAFILE.SPA_PRFLAG_4,''),
                   SPAFILE.SPA_PRFORM_5,
                   ISNULL(SPAFILE.SPA_PRFLAG_5,''),
                   SPAFILE.SPA_PRFORM_6,
                   ISNULL(SPAFILE.SPA_PRFLAG_6,''),
                   SPAFILE.SPA_PRFORM_7,
                   ISNULL(SPAFILE.SPA_PRFLAG_7,''),
                   SPAFILE.SPA_PRFORM_8,
                   ISNULL(SPAFILE.SPA_PRFLAG_8,''),
                   SPAFILE.SPA_PRFORM_9,
                   ISNULL(SPAFILE.SPA_PRFLAG_9,''),
                   SPAFILE.SPA_PRFORM_10,
                   ISNULL(SPAFILE.SPA_PRFLAG_10,''),
                   SPAFILE.SPA_PRFORM_11,
                   ISNULL(SPAFILE.SPA_PRFLAG_11,''),
                   SPAFILE.SPA_PRFORM_12,
                   ISNULL(SPAFILE.SPA_PRFLAG_12,''),
                   SPAFILE.SPA_PRFORM_13,
                   ISNULL(SPAFILE.SPA_PRFLAG_13,''),
                   SPAFILE.SPA_PRFORM_14,
                   ISNULL(SPAFILE.SPA_PRFLAG_14,''),
                   SPAFILE.SPA_PRFORM_15,
                   ISNULL(SPAFILE.SPA_PRFLAG_15,''),
                   SPAFILE.SPA_PRFORM_16,
                   ISNULL(SPAFILE.SPA_PRFLAG_16,''),
                   SPAFILE.SPA_PRFORM_17,
                   ISNULL(SPAFILE.SPA_PRFLAG_17,''),
                   SPAFILE.SPA_PRFORM_18,
                   ISNULL(SPAFILE.SPA_PRFLAG_18,''),
                   SPAFILE.SPA_PRFORM_19,
                   ISNULL(SPAFILE.SPA_PRFLAG_19,''),
                   SPAFILE.SPA_PRFORM_20,
                   ISNULL(SPAFILE.SPA_PRFLAG_20,''),
                   ISNULL(SPAFILE.SPA_IMED_REC,''),
                   SPAFILE.SPA_SCFRMLA,
                   SPAFILE.SPA_SCRATE,
                   SPAFILE.SPA_SCMIN,
                   SPAFILE.SPA_SCMAX,
                   SPAFILE.SPA_MFFRMLA,
                   SPAFILE.SPA_MFRATE,
                   SPAFILE.SPA_MFMIN,
                   SPAFILE.SPA_MFMAX,
                   ISNULL(SPAFILE.SPA_INTMETHOD,''),
                   SPAFILE.SPA_YEARTYPE,
                   SPAFILE.SPA_PYEXFRMLA,
                   SPAFILE.SPA_PYEXSPCODE,
                   SPAFILE.SPA_PYEXYEARTP,
                   ISNULL(SPAFILE.SPA_PYEXFMDATE,''),
                   ISNULL(SPAFILE.SPA_PYEXBASECD,''),
                   SPAFILE.SPA_PYEXBASE,
                   SPAFILE.SPA_PYEXDAYS,
                   SPAFILE.SPA_CAL_SPFRMLA,
                   ISNULL(SPAFILE.SPA_CAL_RATETYPE_1,''),
                   ISNULL(SPAFILE.SPA_CAL_BREAKTYPE_1,''),
                   ISNULL(SPAFILE.SPA_CAL_CHGABLE_1,''),
                   SPAFILE.SPA_CAL_NORATES_1,
                   SPAFILE.SPA_CAL_TBFRMLA_1,
                   SPAFILE.SPA_CAL_RATE_1_1,
                   SPAFILE.SPA_CAL_BREAK_1_1,
                   SPAFILE.SPA_CAL_RATE_1_2,
                   SPAFILE.SPA_CAL_BREAK_1_2,
                   SPAFILE.SPA_CAL_RATE_1_3,
                   SPAFILE.SPA_CAL_BREAK_1_3,
                   SPAFILE.SPA_CAL_RATE_1_4,
                   SPAFILE.SPA_CAL_BREAK_1_4,
                   SPAFILE.SPA_CAL_RATE_1_5,
                   SPAFILE.SPA_CAL_BREAK_1_5,
                   ISNULL(SPAFILE.SPA_CAL_RATETYPE_2,''),
                   ISNULL(SPAFILE.SPA_CAL_BREAKTYPE_2,''),
                   ISNULL(SPAFILE.SPA_CAL_CHGABLE_2,''),
                   SPAFILE.SPA_CAL_NORATES_2,
                   SPAFILE.SPA_CAL_TBFRMLA_2,
                   SPAFILE.SPA_CAL_RATE_2_1,
                   SPAFILE.SPA_CAL_BREAK_2_1,
                   SPAFILE.SPA_CAL_RATE_2_2,
                   SPAFILE.SPA_CAL_BREAK_2_2,
                   SPAFILE.SPA_CAL_RATE_2_3,
                   SPAFILE.SPA_CAL_BREAK_2_3,
                   SPAFILE.SPA_CAL_RATE_2_4,
                   SPAFILE.SPA_CAL_BREAK_2_4,
                   SPAFILE.SPA_CAL_RATE_2_5,
                   SPAFILE.SPA_CAL_BREAK_2_5,
                   SPAFILE.SPA_MINSF_FRMLA,
                   SPAFILE.SPA_MINSF_CHG,
                   SPAFILE.SPA_MAXAPR_FRMLA,
                   SPAFILE.SPA_EDT_NOEDITS_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_1,''),
                   SPAFILE.SPA_EDT_RATE_1_1,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_1,''),
                   SPAFILE.SPA_EDT_AMT_1_1,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_1,''),
                   SPAFILE.SPA_EDT_TERM_1_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_2,''),
                   SPAFILE.SPA_EDT_RATE_1_2,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_2,''),
                   SPAFILE.SPA_EDT_AMT_1_2,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_2,''),
                   SPAFILE.SPA_EDT_TERM_1_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_3,''),
                   SPAFILE.SPA_EDT_RATE_1_3,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_3,''),
                   SPAFILE.SPA_EDT_AMT_1_3,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_3,''),
                   SPAFILE.SPA_EDT_TERM_1_3,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_4,''),
                   SPAFILE.SPA_EDT_RATE_1_4,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_4,''),
                   SPAFILE.SPA_EDT_AMT_1_4,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_4,''),
                   SPAFILE.SPA_EDT_TERM_1_4,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_5,''),
                   SPAFILE.SPA_EDT_RATE_1_5,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_5,''),
                   SPAFILE.SPA_EDT_AMT_1_5,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_5,''),
                   SPAFILE.SPA_EDT_TERM_1_5,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_6,''),
                   SPAFILE.SPA_EDT_RATE_1_6,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_6,''),
                   SPAFILE.SPA_EDT_AMT_1_6,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_6,''),
                   SPAFILE.SPA_EDT_TERM_1_6,
                   SPAFILE.SPA_EDT_NOEDITS_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_1,''),
                   SPAFILE.SPA_EDT_RATE_2_1,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_1,''),
                   SPAFILE.SPA_EDT_AMT_2_1,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_1,''),
                   SPAFILE.SPA_EDT_TERM_2_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_2,''),
                   SPAFILE.SPA_EDT_RATE_2_2,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_2,''),
                   SPAFILE.SPA_EDT_AMT_2_2,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_2,''),
                   SPAFILE.SPA_EDT_TERM_2_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_3,''),
                   SPAFILE.SPA_EDT_RATE_2_3,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_3,''),
                   SPAFILE.SPA_EDT_AMT_2_3,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_3,''),
                   SPAFILE.SPA_EDT_TERM_2_3,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_4,''),
                   SPAFILE.SPA_EDT_RATE_2_4,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_4,''),
                   SPAFILE.SPA_EDT_AMT_2_4,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_4,''),
                   SPAFILE.SPA_EDT_TERM_2_4,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_5,''),
                   SPAFILE.SPA_EDT_RATE_2_5,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_5,''),
                   SPAFILE.SPA_EDT_AMT_2_5,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_5,''),
                   SPAFILE.SPA_EDT_TERM_2_5,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_6,''),
                   SPAFILE.SPA_EDT_RATE_2_6,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_6,''),
                   SPAFILE.SPA_EDT_AMT_2_6,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_6,''),
                   SPAFILE.SPA_EDT_TERM_2_6
            FROM DBO.SPAFILE
            WHERE SPAFILE.SPA_ORGST >= :QSP1-WBEG-ORGST
              AND SPAFILE.SPA_ORGST <= :QSP1-WEND-ORGST
              AND SPAFILE.SPA_SPRCLASS >= :QSP1-WBEG-SPRCLASS
              AND SPAFILE.SPA_SPRCLASS <= :QSP1-WEND-SPRCLASS
              AND SPAFILE.SPA_SUBCLASS >= :QSP1-WBEG-SUBCLASS
              AND SPAFILE.SPA_SUBCLASS <= :QSP1-WEND-SUBCLASS
              AND SPAFILE.SPA_LAWCODE >= :QSP1-WBEG-LAWCODE 
              AND SPAFILE.SPA_LAWCODE <= :QSP1-WEND-LAWCODE 
            ORDER BY SPAFILE.SPA_ORGST,
                     SPAFILE.SPA_SPRCLASS,
                     SPAFILE.SPA_SUBCLASS,
                     SPAFILE.SPA_LAWCODE
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
       START-SPA1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.

           IF ( SPA1-NG-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SPA1-FILE.

           IF ( QSP1-WEND-LAWCODE = 0  ) OR
              ( QSP1-WEND-LAWCODE NOT NUMERIC)
              MOVE SPACES               TO QSP1-WBEG-ORGST 
              MOVE SPA-ORGST            TO QSP1-WEND-ORGST 
              MOVE 0                    TO QSP1-WBEG-SPRCLASS
              MOVE SPA-SPRCLASS         TO QSP1-WEND-SPRCLASS
              MOVE 0                    TO QSP1-WBEG-SUBCLASS
              MOVE SPA-SUBCLASS         TO QSP1-WEND-SUBCLASS
              MOVE 0                    TO QSP1-WBEG-LAWCODE
              MOVE SPA-LAWCODE          TO QSP1-WEND-LAWCODE.

           EXEC SQL
            DECLARE SPA1_NG_CURSOR CURSOR FOR
            SELECT ISNULL(SPAFILE.SPA_ORGST,''),
                   SPAFILE.SPA_SPRCLASS,
                   SPAFILE.SPA_SUBCLASS,
                   SPAFILE.SPA_LAWCODE,
                   ISNULL(CAST(SPAFILE.SPA_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(SPAFILE.SPA_DESC,''),
                   ISNULL(SPAFILE.SPA_NOTES,''),
                   ISNULL(CAST(SPAFILE.SPA_EFFDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(SPAFILE.SPA_EXPDATE
                                                  AS VARCHAR(10)),'0'),
                   SPAFILE.SPA_EXPD_MONTHS,
                   SPAFILE.SPA_EXPD_DAYS,
                   ISNULL(SPAFILE.SPA_EXPD_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPD_EXCEPT,''),
                   ISNULL(SPAFILE.SPA_EXPR_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPR_REDU_FRMLA,''),
                   SPAFILE.SPA_EXPR_RATE_1,
                   SPAFILE.SPA_EXPR_BREAK_1,
                   SPAFILE.SPA_EXPR_YRTYPE_1,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_1,''),
                   SPAFILE.SPA_EXPR_RATE_2,
                   SPAFILE.SPA_EXPR_BREAK_2,
                   SPAFILE.SPA_EXPR_YRTYPE_2,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_2,''),
                   SPAFILE.SPA_EXPR_RATE_3,
                   SPAFILE.SPA_EXPR_BREAK_3,
                   SPAFILE.SPA_EXPR_YRTYPE_3,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_3,''),
                   SPAFILE.SPA_EXPR_RATE_4,
                   SPAFILE.SPA_EXPR_BREAK_4,
                   SPAFILE.SPA_EXPR_YRTYPE_4,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_4,''),
                   SPAFILE.SPA_EXPR_RATE_5,
                   SPAFILE.SPA_EXPR_BREAK_5,
                   SPAFILE.SPA_EXPR_YRTYPE_5,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_5,''),
                   ISNULL(SPAFILE.SPA_EXPR_LCHG,''),
                   ISNULL(SPAFILE.SPA_JUDGFRMLA,''),
                   SPAFILE.SPA_JUDGRATE,
                   SPAFILE.SPA_JUDGYRTYPE,
                   ISNULL(SPAFILE.SPA_JUDGINTMETH,''),
                   ISNULL(SPAFILE.SPA_ACC_FRMLA,''),
                   SPAFILE.SPA_ACC_YRTYPE,
                   ISNULL(SPAFILE.SPA_ACC_INTMETH,''),
                   SPAFILE.SPA_ACC_RATE_1,
                   SPAFILE.SPA_ACC_BREAK_1,
                   SPAFILE.SPA_ACC_RATE_2,
                   SPAFILE.SPA_ACC_BREAK_2,
                   SPAFILE.SPA_ACC_RATE_3,
                   SPAFILE.SPA_ACC_BREAK_3,
                   SPAFILE.SPA_ACC_RATE_4,
                   SPAFILE.SPA_ACC_BREAK_4,
                   SPAFILE.SPA_ACC_RATE_5,
                   SPAFILE.SPA_ACC_BREAK_5,
                   ISNULL(SPAFILE.SPA_1098FRMLA,''),
                   ISNULL(SPAFILE.SPA_DISFRMLA,''),
                   ISNULL(SPAFILE.SPA_CONTRFRMLA,''),
                   ISNULL(SPAFILE.SPA_BANKRULE,''),
                   SPAFILE.SPA_CONTPDTH,
                   SPAFILE.SPA_FINCHG,
                   ISNULL(SPAFILE.SPA_BANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_TRWBANKRULE,''),
                   SPAFILE.SPA_TRWCONTPDTH,
                   ISNULL(SPAFILE.SPA_TRWBANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_RECFRMLA,''),
                   ISNULL(SPAFILE.SPA_RECPAYCD,''),
                   SPAFILE.SPA_RECPDTH,
                   ISNULL(SPAFILE.SPA_DISFRMLA_SPAY_FG,''),
                   SPAFILE.SPA_ERNFRMLA_1,
                   SPAFILE.SPA_ERNAMT_1,
                   SPAFILE.SPA_ERNPERCT_1,
                   ISNULL(SPAFILE.SPA_ERNPERCD_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_1,''),
                   SPAFILE.SPA_ERN_DAYS_1,
                   SPAFILE.SPA_ERN_TERM_1,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_1,''),
                   SPAFILE.SPA_ERNFRMLA_2,
                   SPAFILE.SPA_ERNAMT_2,
                   SPAFILE.SPA_ERNPERCT_2,
                   ISNULL(SPAFILE.SPA_ERNPERCD_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_2,''),
                   SPAFILE.SPA_ERN_DAYS_2,
                   SPAFILE.SPA_ERN_TERM_2,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_2,''),
                   SPAFILE.SPA_ERNFRMLA_3,
                   SPAFILE.SPA_ERNAMT_3,
                   SPAFILE.SPA_ERNPERCT_3,
                   ISNULL(SPAFILE.SPA_ERNPERCD_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_3,''),
                   SPAFILE.SPA_ERN_DAYS_3,
                   SPAFILE.SPA_ERN_TERM_3,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_3,''),
                   SPAFILE.SPA_ERNFRMLA_4,
                   SPAFILE.SPA_ERNAMT_4,
                   SPAFILE.SPA_ERNPERCT_4,
                   ISNULL(SPAFILE.SPA_ERNPERCD_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_4,''),
                   SPAFILE.SPA_ERN_DAYS_4,
                   SPAFILE.SPA_ERN_TERM_4,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_4,''),
                   SPAFILE.SPA_ERNFRMLA_5,
                   SPAFILE.SPA_ERNAMT_5,
                   SPAFILE.SPA_ERNPERCT_5,
                   ISNULL(SPAFILE.SPA_ERNPERCD_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_5,''),
                   SPAFILE.SPA_ERN_DAYS_5,
                   SPAFILE.SPA_ERN_TERM_5,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_5,''),
                   SPAFILE.SPA_ERNFRMLA_6,
                   SPAFILE.SPA_ERNAMT_6,
                   SPAFILE.SPA_ERNPERCT_6,
                   ISNULL(SPAFILE.SPA_ERNPERCD_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_6,''),
                   SPAFILE.SPA_ERN_DAYS_6,
                   SPAFILE.SPA_ERN_TERM_6,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_6,''),
                   SPAFILE.SPA_ERNFRMLA_7,
                   SPAFILE.SPA_ERNAMT_7,
                   SPAFILE.SPA_ERNPERCT_7,
                   ISNULL(SPAFILE.SPA_ERNPERCD_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_7,''),
                   SPAFILE.SPA_ERN_DAYS_7,
                   SPAFILE.SPA_ERN_TERM_7,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_7,''),
                   SPAFILE.SPA_ERNFRMLA_8,
                   SPAFILE.SPA_ERNAMT_8,
                   SPAFILE.SPA_ERNPERCT_8,
                   ISNULL(SPAFILE.SPA_ERNPERCD_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_8,''),
                   SPAFILE.SPA_ERN_DAYS_8,
                   SPAFILE.SPA_ERN_TERM_8,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_8,''),
                   SPAFILE.SPA_ERNFRMLA_9,
                   SPAFILE.SPA_ERNAMT_9,
                   SPAFILE.SPA_ERNPERCT_9,
                   ISNULL(SPAFILE.SPA_ERNPERCD_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_9,''),
                   SPAFILE.SPA_ERN_DAYS_9,
                   SPAFILE.SPA_ERN_TERM_9,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_9,''),
                   SPAFILE.SPA_ERNFRMLA_10,
                   SPAFILE.SPA_ERNAMT_10,
                   SPAFILE.SPA_ERNPERCT_10,
                   ISNULL(SPAFILE.SPA_ERNPERCD_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_10,''),
                   SPAFILE.SPA_ERN_DAYS_10,
                   SPAFILE.SPA_ERN_TERM_10,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCEL_CD,''),
                   ISNULL(SPAFILE.SPA_ERNJD_CD,''),
                   ISNULL(SPAFILE.SPA_MIDSTRERNCD,''),
                   SPAFILE.SPA_EARN_ORDER_1,
                   SPAFILE.SPA_EARN_ORDER_2,
                   SPAFILE.SPA_EARN_ORDER_3,
                   SPAFILE.SPA_EARN_ORDER_4,
                   SPAFILE.SPA_EARN_ORDER_5,
                   SPAFILE.SPA_EARN_ORDER_6,
                   SPAFILE.SPA_EARN_ORDER_7,
                   SPAFILE.SPA_EARN_ORDER_8,
                   SPAFILE.SPA_EARN_ORDER_9,
                   SPAFILE.SPA_EARN_ORDER_10,
                   ISNULL(SPAFILE.SPA_ERNDELQFRMLA,''),
                   SPAFILE.SPA_ERNDELQ_START,
                   SPAFILE.SPA_ERNDELQ_STOP,
                   ISNULL(SPAFILE.SPA_1098_INC_LC,''),
                   SPAFILE.SPA_MAX_IB_ACR_DAYS,
                   SPAFILE.SPA_LCGRACE,
                   ISNULL(SPAFILE.SPA_LCGRACECD,''),
                   ISNULL(SPAFILE.SPA_LCTYPE,''),
                   SPAFILE.SPA_LCRATE,
                   ISNULL(SPAFILE.SPA_LCFRMLA,''),
                   SPAFILE.SPA_LCMINCHG,
                   SPAFILE.SPA_LCMAXAMT,
                   SPAFILE.SPA_LCDELFAC,
                   SPAFILE.SPA_LCYRTYPE,
                   SPAFILE.SPA_LC_CODE,
                   ISNULL(SPAFILE.SPA_LCUNITPER_CD,''),
                   SPAFILE.SPA_DEFFRMLA,
                   ISNULL(SPAFILE.SPA_DEFEXCEPT,''),
                   SPAFILE.SPA_DEFWORKER,
                   SPAFILE.SPA_DEFPERCD,
                   SPAFILE.SPA_DEFBASE,
                   ISNULL(SPAFILE.SPA_DEFPOLCD,''),
                   SPAFILE.SPA_DEFPOLMX_1,
                   SPAFILE.SPA_DEFPOLMX_2,
                   SPAFILE.SPA_DEFPOLMX_3,
                   SPAFILE.SPA_DEFMIN,
                   SPAFILE.SPA_DEFMAX,
                   ISNULL(SPAFILE.SPA_DEFUNITPER_CD,''),
                   SPAFILE.SPA_PRFORM_1,
                   ISNULL(SPAFILE.SPA_PRFLAG_1,''),
                   SPAFILE.SPA_PRFORM_2,
                   ISNULL(SPAFILE.SPA_PRFLAG_2,''),
                   SPAFILE.SPA_PRFORM_3,
                   ISNULL(SPAFILE.SPA_PRFLAG_3,''),
                   SPAFILE.SPA_PRFORM_4,
                   ISNULL(SPAFILE.SPA_PRFLAG_4,''),
                   SPAFILE.SPA_PRFORM_5,
                   ISNULL(SPAFILE.SPA_PRFLAG_5,''),
                   SPAFILE.SPA_PRFORM_6,
                   ISNULL(SPAFILE.SPA_PRFLAG_6,''),
                   SPAFILE.SPA_PRFORM_7,
                   ISNULL(SPAFILE.SPA_PRFLAG_7,''),
                   SPAFILE.SPA_PRFORM_8,
                   ISNULL(SPAFILE.SPA_PRFLAG_8,''),
                   SPAFILE.SPA_PRFORM_9,
                   ISNULL(SPAFILE.SPA_PRFLAG_9,''),
                   SPAFILE.SPA_PRFORM_10,
                   ISNULL(SPAFILE.SPA_PRFLAG_10,''),
                   SPAFILE.SPA_PRFORM_11,
                   ISNULL(SPAFILE.SPA_PRFLAG_11,''),
                   SPAFILE.SPA_PRFORM_12,
                   ISNULL(SPAFILE.SPA_PRFLAG_12,''),
                   SPAFILE.SPA_PRFORM_13,
                   ISNULL(SPAFILE.SPA_PRFLAG_13,''),
                   SPAFILE.SPA_PRFORM_14,
                   ISNULL(SPAFILE.SPA_PRFLAG_14,''),
                   SPAFILE.SPA_PRFORM_15,
                   ISNULL(SPAFILE.SPA_PRFLAG_15,''),
                   SPAFILE.SPA_PRFORM_16,
                   ISNULL(SPAFILE.SPA_PRFLAG_16,''),
                   SPAFILE.SPA_PRFORM_17,
                   ISNULL(SPAFILE.SPA_PRFLAG_17,''),
                   SPAFILE.SPA_PRFORM_18,
                   ISNULL(SPAFILE.SPA_PRFLAG_18,''),
                   SPAFILE.SPA_PRFORM_19,
                   ISNULL(SPAFILE.SPA_PRFLAG_19,''),
                   SPAFILE.SPA_PRFORM_20,
                   ISNULL(SPAFILE.SPA_PRFLAG_20,''),
                   ISNULL(SPAFILE.SPA_IMED_REC,''),
                   SPAFILE.SPA_SCFRMLA,
                   SPAFILE.SPA_SCRATE,
                   SPAFILE.SPA_SCMIN,
                   SPAFILE.SPA_SCMAX,
                   SPAFILE.SPA_MFFRMLA,
                   SPAFILE.SPA_MFRATE,
                   SPAFILE.SPA_MFMIN,
                   SPAFILE.SPA_MFMAX,
                   ISNULL(SPAFILE.SPA_INTMETHOD,''),
                   SPAFILE.SPA_YEARTYPE,
                   SPAFILE.SPA_PYEXFRMLA,
                   SPAFILE.SPA_PYEXSPCODE,
                   SPAFILE.SPA_PYEXYEARTP,
                   ISNULL(SPAFILE.SPA_PYEXFMDATE,''),
                   ISNULL(SPAFILE.SPA_PYEXBASECD,''),
                   SPAFILE.SPA_PYEXBASE,
                   SPAFILE.SPA_PYEXDAYS,
                   SPAFILE.SPA_CAL_SPFRMLA,
                   ISNULL(SPAFILE.SPA_CAL_RATETYPE_1,''),
                   ISNULL(SPAFILE.SPA_CAL_BREAKTYPE_1,''),
                   ISNULL(SPAFILE.SPA_CAL_CHGABLE_1,''),
                   SPAFILE.SPA_CAL_NORATES_1,
                   SPAFILE.SPA_CAL_TBFRMLA_1,
                   SPAFILE.SPA_CAL_RATE_1_1,
                   SPAFILE.SPA_CAL_BREAK_1_1,
                   SPAFILE.SPA_CAL_RATE_1_2,
                   SPAFILE.SPA_CAL_BREAK_1_2,
                   SPAFILE.SPA_CAL_RATE_1_3,
                   SPAFILE.SPA_CAL_BREAK_1_3,
                   SPAFILE.SPA_CAL_RATE_1_4,
                   SPAFILE.SPA_CAL_BREAK_1_4,
                   SPAFILE.SPA_CAL_RATE_1_5,
                   SPAFILE.SPA_CAL_BREAK_1_5,
                   ISNULL(SPAFILE.SPA_CAL_RATETYPE_2,''),
                   ISNULL(SPAFILE.SPA_CAL_BREAKTYPE_2,''),
                   ISNULL(SPAFILE.SPA_CAL_CHGABLE_2,''),
                   SPAFILE.SPA_CAL_NORATES_2,
                   SPAFILE.SPA_CAL_TBFRMLA_2,
                   SPAFILE.SPA_CAL_RATE_2_1,
                   SPAFILE.SPA_CAL_BREAK_2_1,
                   SPAFILE.SPA_CAL_RATE_2_2,
                   SPAFILE.SPA_CAL_BREAK_2_2,
                   SPAFILE.SPA_CAL_RATE_2_3,
                   SPAFILE.SPA_CAL_BREAK_2_3,
                   SPAFILE.SPA_CAL_RATE_2_4,
                   SPAFILE.SPA_CAL_BREAK_2_4,
                   SPAFILE.SPA_CAL_RATE_2_5,
                   SPAFILE.SPA_CAL_BREAK_2_5,
                   SPAFILE.SPA_MINSF_FRMLA,
                   SPAFILE.SPA_MINSF_CHG,
                   SPAFILE.SPA_MAXAPR_FRMLA,
                   SPAFILE.SPA_EDT_NOEDITS_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_1,''),
                   SPAFILE.SPA_EDT_RATE_1_1,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_1,''),
                   SPAFILE.SPA_EDT_AMT_1_1,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_1,''),
                   SPAFILE.SPA_EDT_TERM_1_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_2,''),
                   SPAFILE.SPA_EDT_RATE_1_2,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_2,''),
                   SPAFILE.SPA_EDT_AMT_1_2,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_2,''),
                   SPAFILE.SPA_EDT_TERM_1_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_3,''),
                   SPAFILE.SPA_EDT_RATE_1_3,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_3,''),
                   SPAFILE.SPA_EDT_AMT_1_3,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_3,''),
                   SPAFILE.SPA_EDT_TERM_1_3,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_4,''),
                   SPAFILE.SPA_EDT_RATE_1_4,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_4,''),
                   SPAFILE.SPA_EDT_AMT_1_4,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_4,''),
                   SPAFILE.SPA_EDT_TERM_1_4,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_5,''),
                   SPAFILE.SPA_EDT_RATE_1_5,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_5,''),
                   SPAFILE.SPA_EDT_AMT_1_5,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_5,''),
                   SPAFILE.SPA_EDT_TERM_1_5,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_6,''),
                   SPAFILE.SPA_EDT_RATE_1_6,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_6,''),
                   SPAFILE.SPA_EDT_AMT_1_6,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_6,''),
                   SPAFILE.SPA_EDT_TERM_1_6,
                   SPAFILE.SPA_EDT_NOEDITS_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_1,''),
                   SPAFILE.SPA_EDT_RATE_2_1,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_1,''),
                   SPAFILE.SPA_EDT_AMT_2_1,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_1,''),
                   SPAFILE.SPA_EDT_TERM_2_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_2,''),
                   SPAFILE.SPA_EDT_RATE_2_2,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_2,''),
                   SPAFILE.SPA_EDT_AMT_2_2,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_2,''),
                   SPAFILE.SPA_EDT_TERM_2_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_3,''),
                   SPAFILE.SPA_EDT_RATE_2_3,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_3,''),
                   SPAFILE.SPA_EDT_AMT_2_3,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_3,''),
                   SPAFILE.SPA_EDT_TERM_2_3,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_4,''),
                   SPAFILE.SPA_EDT_RATE_2_4,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_4,''),
                   SPAFILE.SPA_EDT_AMT_2_4,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_4,''),
                   SPAFILE.SPA_EDT_TERM_2_4,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_5,''),
                   SPAFILE.SPA_EDT_RATE_2_5,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_5,''),
                   SPAFILE.SPA_EDT_AMT_2_5,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_5,''),
                   SPAFILE.SPA_EDT_TERM_2_5,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_6,''),
                   SPAFILE.SPA_EDT_RATE_2_6,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_6,''),
                   SPAFILE.SPA_EDT_AMT_2_6,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_6,''),
                   SPAFILE.SPA_EDT_TERM_2_6
            FROM DBO.SPAFILE
            WHERE SPAFILE.SPA_ORGST >= :QSP1-WBEG-ORGST
              AND SPAFILE.SPA_ORGST <= :QSP1-WEND-ORGST
              AND SPAFILE.SPA_SPRCLASS >= :QSP1-WBEG-SPRCLASS
              AND SPAFILE.SPA_SPRCLASS <= :QSP1-WEND-SPRCLASS
              AND SPAFILE.SPA_SUBCLASS >= :QSP1-WBEG-SUBCLASS
              AND SPAFILE.SPA_SUBCLASS <= :QSP1-WEND-SUBCLASS
              AND SPAFILE.SPA_LAWCODE >= :QSP1-WBEG-LAWCODE 
              AND SPAFILE.SPA_LAWCODE <= :QSP1-WEND-LAWCODE 
            ORDER BY SPAFILE.SPA_ORGST DESC,
                     SPAFILE.SPA_SPRCLASS DESC,
                     SPAFILE.SPA_SUBCLASS DESC,
                     SPAFILE.SPA_LAWCODE DESC
           END-EXEC.

           EXEC SQL
               OPEN SPA1_NG_CURSOR
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

           MOVE STAT---GOOD TO SPA1-NG-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SPA1-FILE.
           PERFORM READ-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.

           MOVE SPA-ORGST            TO QSPA-ORGST.
           MOVE SPA-SPRCLASS         TO QSPA-SPRCLASS.
           MOVE SPA-SUBCLASS         TO QSPA-SUBCLASS.
           MOVE SPA-LAWCODE          TO QSPA-LAWCODE.

           EXEC SQL
            SELECT ISNULL(SPAFILE.SPA_ORGST,''),
                   SPAFILE.SPA_SPRCLASS,
                   SPAFILE.SPA_SUBCLASS,
                   SPAFILE.SPA_LAWCODE,
                   ISNULL(CAST(SPAFILE.SPA_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(SPAFILE.SPA_DESC,''),
                   ISNULL(SPAFILE.SPA_NOTES,''),
                   ISNULL(CAST(SPAFILE.SPA_EFFDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(SPAFILE.SPA_EXPDATE
                                                  AS VARCHAR(10)),'0'),
                   SPAFILE.SPA_EXPD_MONTHS,
                   SPAFILE.SPA_EXPD_DAYS,
                   ISNULL(SPAFILE.SPA_EXPD_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPD_EXCEPT,''),
                   ISNULL(SPAFILE.SPA_EXPR_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPR_REDU_FRMLA,''),
                   SPAFILE.SPA_EXPR_RATE_1,
                   SPAFILE.SPA_EXPR_BREAK_1,
                   SPAFILE.SPA_EXPR_YRTYPE_1,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_1,''),
                   SPAFILE.SPA_EXPR_RATE_2,
                   SPAFILE.SPA_EXPR_BREAK_2,
                   SPAFILE.SPA_EXPR_YRTYPE_2,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_2,''),
                   SPAFILE.SPA_EXPR_RATE_3,
                   SPAFILE.SPA_EXPR_BREAK_3,
                   SPAFILE.SPA_EXPR_YRTYPE_3,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_3,''),
                   SPAFILE.SPA_EXPR_RATE_4,
                   SPAFILE.SPA_EXPR_BREAK_4,
                   SPAFILE.SPA_EXPR_YRTYPE_4,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_4,''),
                   SPAFILE.SPA_EXPR_RATE_5,
                   SPAFILE.SPA_EXPR_BREAK_5,
                   SPAFILE.SPA_EXPR_YRTYPE_5,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_5,''),
                   ISNULL(SPAFILE.SPA_EXPR_LCHG,''),
                   ISNULL(SPAFILE.SPA_JUDGFRMLA,''),
                   SPAFILE.SPA_JUDGRATE,
                   SPAFILE.SPA_JUDGYRTYPE,
                   ISNULL(SPAFILE.SPA_JUDGINTMETH,''),
                   ISNULL(SPAFILE.SPA_ACC_FRMLA,''),
                   SPAFILE.SPA_ACC_YRTYPE,
                   ISNULL(SPAFILE.SPA_ACC_INTMETH,''),
                   SPAFILE.SPA_ACC_RATE_1,
                   SPAFILE.SPA_ACC_BREAK_1,
                   SPAFILE.SPA_ACC_RATE_2,
                   SPAFILE.SPA_ACC_BREAK_2,
                   SPAFILE.SPA_ACC_RATE_3,
                   SPAFILE.SPA_ACC_BREAK_3,
                   SPAFILE.SPA_ACC_RATE_4,
                   SPAFILE.SPA_ACC_BREAK_4,
                   SPAFILE.SPA_ACC_RATE_5,
                   SPAFILE.SPA_ACC_BREAK_5,
                   ISNULL(SPAFILE.SPA_1098FRMLA,''),
                   ISNULL(SPAFILE.SPA_DISFRMLA,''),
                   ISNULL(SPAFILE.SPA_CONTRFRMLA,''),
                   ISNULL(SPAFILE.SPA_BANKRULE,''),
                   SPAFILE.SPA_CONTPDTH,
                   SPAFILE.SPA_FINCHG,
                   ISNULL(SPAFILE.SPA_BANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_TRWBANKRULE,''),
                   SPAFILE.SPA_TRWCONTPDTH,
                   ISNULL(SPAFILE.SPA_TRWBANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_RECFRMLA,''),
                   ISNULL(SPAFILE.SPA_RECPAYCD,''),
                   SPAFILE.SPA_RECPDTH,
                   ISNULL(SPAFILE.SPA_DISFRMLA_SPAY_FG,''),
                   SPAFILE.SPA_ERNFRMLA_1,
                   SPAFILE.SPA_ERNAMT_1,
                   SPAFILE.SPA_ERNPERCT_1,
                   ISNULL(SPAFILE.SPA_ERNPERCD_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_1,''),
                   SPAFILE.SPA_ERN_DAYS_1,
                   SPAFILE.SPA_ERN_TERM_1,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_1,''),
                   SPAFILE.SPA_ERNFRMLA_2,
                   SPAFILE.SPA_ERNAMT_2,
                   SPAFILE.SPA_ERNPERCT_2,
                   ISNULL(SPAFILE.SPA_ERNPERCD_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_2,''),
                   SPAFILE.SPA_ERN_DAYS_2,
                   SPAFILE.SPA_ERN_TERM_2,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_2,''),
                   SPAFILE.SPA_ERNFRMLA_3,
                   SPAFILE.SPA_ERNAMT_3,
                   SPAFILE.SPA_ERNPERCT_3,
                   ISNULL(SPAFILE.SPA_ERNPERCD_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_3,''),
                   SPAFILE.SPA_ERN_DAYS_3,
                   SPAFILE.SPA_ERN_TERM_3,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_3,''),
                   SPAFILE.SPA_ERNFRMLA_4,
                   SPAFILE.SPA_ERNAMT_4,
                   SPAFILE.SPA_ERNPERCT_4,
                   ISNULL(SPAFILE.SPA_ERNPERCD_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_4,''),
                   SPAFILE.SPA_ERN_DAYS_4,
                   SPAFILE.SPA_ERN_TERM_4,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_4,''),
                   SPAFILE.SPA_ERNFRMLA_5,
                   SPAFILE.SPA_ERNAMT_5,
                   SPAFILE.SPA_ERNPERCT_5,
                   ISNULL(SPAFILE.SPA_ERNPERCD_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_5,''),
                   SPAFILE.SPA_ERN_DAYS_5,
                   SPAFILE.SPA_ERN_TERM_5,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_5,''),
                   SPAFILE.SPA_ERNFRMLA_6,
                   SPAFILE.SPA_ERNAMT_6,
                   SPAFILE.SPA_ERNPERCT_6,
                   ISNULL(SPAFILE.SPA_ERNPERCD_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_6,''),
                   SPAFILE.SPA_ERN_DAYS_6,
                   SPAFILE.SPA_ERN_TERM_6,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_6,''),
                   SPAFILE.SPA_ERNFRMLA_7,
                   SPAFILE.SPA_ERNAMT_7,
                   SPAFILE.SPA_ERNPERCT_7,
                   ISNULL(SPAFILE.SPA_ERNPERCD_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_7,''),
                   SPAFILE.SPA_ERN_DAYS_7,
                   SPAFILE.SPA_ERN_TERM_7,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_7,''),
                   SPAFILE.SPA_ERNFRMLA_8,
                   SPAFILE.SPA_ERNAMT_8,
                   SPAFILE.SPA_ERNPERCT_8,
                   ISNULL(SPAFILE.SPA_ERNPERCD_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_8,''),
                   SPAFILE.SPA_ERN_DAYS_8,
                   SPAFILE.SPA_ERN_TERM_8,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_8,''),
                   SPAFILE.SPA_ERNFRMLA_9,
                   SPAFILE.SPA_ERNAMT_9,
                   SPAFILE.SPA_ERNPERCT_9,
                   ISNULL(SPAFILE.SPA_ERNPERCD_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_9,''),
                   SPAFILE.SPA_ERN_DAYS_9,
                   SPAFILE.SPA_ERN_TERM_9,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_9,''),
                   SPAFILE.SPA_ERNFRMLA_10,
                   SPAFILE.SPA_ERNAMT_10,
                   SPAFILE.SPA_ERNPERCT_10,
                   ISNULL(SPAFILE.SPA_ERNPERCD_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_10,''),
                   SPAFILE.SPA_ERN_DAYS_10,
                   SPAFILE.SPA_ERN_TERM_10,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCEL_CD,''),
                   ISNULL(SPAFILE.SPA_ERNJD_CD,''),
                   ISNULL(SPAFILE.SPA_MIDSTRERNCD,''),
                   SPAFILE.SPA_EARN_ORDER_1,
                   SPAFILE.SPA_EARN_ORDER_2,
                   SPAFILE.SPA_EARN_ORDER_3,
                   SPAFILE.SPA_EARN_ORDER_4,
                   SPAFILE.SPA_EARN_ORDER_5,
                   SPAFILE.SPA_EARN_ORDER_6,
                   SPAFILE.SPA_EARN_ORDER_7,
                   SPAFILE.SPA_EARN_ORDER_8,
                   SPAFILE.SPA_EARN_ORDER_9,
                   SPAFILE.SPA_EARN_ORDER_10,
                   ISNULL(SPAFILE.SPA_ERNDELQFRMLA,''),
                   SPAFILE.SPA_ERNDELQ_START,
                   SPAFILE.SPA_ERNDELQ_STOP,
                   ISNULL(SPAFILE.SPA_1098_INC_LC,''),
                   SPAFILE.SPA_MAX_IB_ACR_DAYS,
                   SPAFILE.SPA_LCGRACE,
                   ISNULL(SPAFILE.SPA_LCGRACECD,''),
                   ISNULL(SPAFILE.SPA_LCTYPE,''),
                   SPAFILE.SPA_LCRATE,
                   ISNULL(SPAFILE.SPA_LCFRMLA,''),
                   SPAFILE.SPA_LCMINCHG,
                   SPAFILE.SPA_LCMAXAMT,
                   SPAFILE.SPA_LCDELFAC,
                   SPAFILE.SPA_LCYRTYPE,
                   SPAFILE.SPA_LC_CODE,
                   ISNULL(SPAFILE.SPA_LCUNITPER_CD,''),
                   SPAFILE.SPA_DEFFRMLA,
                   ISNULL(SPAFILE.SPA_DEFEXCEPT,''),
                   SPAFILE.SPA_DEFWORKER,
                   SPAFILE.SPA_DEFPERCD,
                   SPAFILE.SPA_DEFBASE,
                   ISNULL(SPAFILE.SPA_DEFPOLCD,''),
                   SPAFILE.SPA_DEFPOLMX_1,
                   SPAFILE.SPA_DEFPOLMX_2,
                   SPAFILE.SPA_DEFPOLMX_3,
                   SPAFILE.SPA_DEFMIN,
                   SPAFILE.SPA_DEFMAX,
                   ISNULL(SPAFILE.SPA_DEFUNITPER_CD,''),
                   SPAFILE.SPA_PRFORM_1,
                   ISNULL(SPAFILE.SPA_PRFLAG_1,''),
                   SPAFILE.SPA_PRFORM_2,
                   ISNULL(SPAFILE.SPA_PRFLAG_2,''),
                   SPAFILE.SPA_PRFORM_3,
                   ISNULL(SPAFILE.SPA_PRFLAG_3,''),
                   SPAFILE.SPA_PRFORM_4,
                   ISNULL(SPAFILE.SPA_PRFLAG_4,''),
                   SPAFILE.SPA_PRFORM_5,
                   ISNULL(SPAFILE.SPA_PRFLAG_5,''),
                   SPAFILE.SPA_PRFORM_6,
                   ISNULL(SPAFILE.SPA_PRFLAG_6,''),
                   SPAFILE.SPA_PRFORM_7,
                   ISNULL(SPAFILE.SPA_PRFLAG_7,''),
                   SPAFILE.SPA_PRFORM_8,
                   ISNULL(SPAFILE.SPA_PRFLAG_8,''),
                   SPAFILE.SPA_PRFORM_9,
                   ISNULL(SPAFILE.SPA_PRFLAG_9,''),
                   SPAFILE.SPA_PRFORM_10,
                   ISNULL(SPAFILE.SPA_PRFLAG_10,''),
                   SPAFILE.SPA_PRFORM_11,
                   ISNULL(SPAFILE.SPA_PRFLAG_11,''),
                   SPAFILE.SPA_PRFORM_12,
                   ISNULL(SPAFILE.SPA_PRFLAG_12,''),
                   SPAFILE.SPA_PRFORM_13,
                   ISNULL(SPAFILE.SPA_PRFLAG_13,''),
                   SPAFILE.SPA_PRFORM_14,
                   ISNULL(SPAFILE.SPA_PRFLAG_14,''),
                   SPAFILE.SPA_PRFORM_15,
                   ISNULL(SPAFILE.SPA_PRFLAG_15,''),
                   SPAFILE.SPA_PRFORM_16,
                   ISNULL(SPAFILE.SPA_PRFLAG_16,''),
                   SPAFILE.SPA_PRFORM_17,
                   ISNULL(SPAFILE.SPA_PRFLAG_17,''),
                   SPAFILE.SPA_PRFORM_18,
                   ISNULL(SPAFILE.SPA_PRFLAG_18,''),
                   SPAFILE.SPA_PRFORM_19,
                   ISNULL(SPAFILE.SPA_PRFLAG_19,''),
                   SPAFILE.SPA_PRFORM_20,
                   ISNULL(SPAFILE.SPA_PRFLAG_20,''),
                   ISNULL(SPAFILE.SPA_IMED_REC,''),
                   SPAFILE.SPA_SCFRMLA,
                   SPAFILE.SPA_SCRATE,
                   SPAFILE.SPA_SCMIN,
                   SPAFILE.SPA_SCMAX,
                   SPAFILE.SPA_MFFRMLA,
                   SPAFILE.SPA_MFRATE,
                   SPAFILE.SPA_MFMIN,
                   SPAFILE.SPA_MFMAX,
                   ISNULL(SPAFILE.SPA_INTMETHOD,''),
                   SPAFILE.SPA_YEARTYPE,
                   SPAFILE.SPA_PYEXFRMLA,
                   SPAFILE.SPA_PYEXSPCODE,
                   SPAFILE.SPA_PYEXYEARTP,
                   ISNULL(SPAFILE.SPA_PYEXFMDATE,''),
                   ISNULL(SPAFILE.SPA_PYEXBASECD,''),
                   SPAFILE.SPA_PYEXBASE,
                   SPAFILE.SPA_PYEXDAYS,
                   SPAFILE.SPA_CAL_SPFRMLA,
                   ISNULL(SPAFILE.SPA_CAL_RATETYPE_1,''),
                   ISNULL(SPAFILE.SPA_CAL_BREAKTYPE_1,''),
                   ISNULL(SPAFILE.SPA_CAL_CHGABLE_1,''),
                   SPAFILE.SPA_CAL_NORATES_1,
                   SPAFILE.SPA_CAL_TBFRMLA_1,
                   SPAFILE.SPA_CAL_RATE_1_1,
                   SPAFILE.SPA_CAL_BREAK_1_1,
                   SPAFILE.SPA_CAL_RATE_1_2,
                   SPAFILE.SPA_CAL_BREAK_1_2,
                   SPAFILE.SPA_CAL_RATE_1_3,
                   SPAFILE.SPA_CAL_BREAK_1_3,
                   SPAFILE.SPA_CAL_RATE_1_4,
                   SPAFILE.SPA_CAL_BREAK_1_4,
                   SPAFILE.SPA_CAL_RATE_1_5,
                   SPAFILE.SPA_CAL_BREAK_1_5,
                   ISNULL(SPAFILE.SPA_CAL_RATETYPE_2,''),
                   ISNULL(SPAFILE.SPA_CAL_BREAKTYPE_2,''),
                   ISNULL(SPAFILE.SPA_CAL_CHGABLE_2,''),
                   SPAFILE.SPA_CAL_NORATES_2,
                   SPAFILE.SPA_CAL_TBFRMLA_2,
                   SPAFILE.SPA_CAL_RATE_2_1,
                   SPAFILE.SPA_CAL_BREAK_2_1,
                   SPAFILE.SPA_CAL_RATE_2_2,
                   SPAFILE.SPA_CAL_BREAK_2_2,
                   SPAFILE.SPA_CAL_RATE_2_3,
                   SPAFILE.SPA_CAL_BREAK_2_3,
                   SPAFILE.SPA_CAL_RATE_2_4,
                   SPAFILE.SPA_CAL_BREAK_2_4,
                   SPAFILE.SPA_CAL_RATE_2_5,
                   SPAFILE.SPA_CAL_BREAK_2_5,
                   SPAFILE.SPA_MINSF_FRMLA,
                   SPAFILE.SPA_MINSF_CHG,
                   SPAFILE.SPA_MAXAPR_FRMLA,
                   SPAFILE.SPA_EDT_NOEDITS_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_1,''),
                   SPAFILE.SPA_EDT_RATE_1_1,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_1,''),
                   SPAFILE.SPA_EDT_AMT_1_1,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_1,''),
                   SPAFILE.SPA_EDT_TERM_1_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_2,''),
                   SPAFILE.SPA_EDT_RATE_1_2,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_2,''),
                   SPAFILE.SPA_EDT_AMT_1_2,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_2,''),
                   SPAFILE.SPA_EDT_TERM_1_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_3,''),
                   SPAFILE.SPA_EDT_RATE_1_3,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_3,''),
                   SPAFILE.SPA_EDT_AMT_1_3,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_3,''),
                   SPAFILE.SPA_EDT_TERM_1_3,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_4,''),
                   SPAFILE.SPA_EDT_RATE_1_4,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_4,''),
                   SPAFILE.SPA_EDT_AMT_1_4,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_4,''),
                   SPAFILE.SPA_EDT_TERM_1_4,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_5,''),
                   SPAFILE.SPA_EDT_RATE_1_5,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_5,''),
                   SPAFILE.SPA_EDT_AMT_1_5,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_5,''),
                   SPAFILE.SPA_EDT_TERM_1_5,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_6,''),
                   SPAFILE.SPA_EDT_RATE_1_6,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_6,''),
                   SPAFILE.SPA_EDT_AMT_1_6,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_6,''),
                   SPAFILE.SPA_EDT_TERM_1_6,
                   SPAFILE.SPA_EDT_NOEDITS_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_1,''),
                   SPAFILE.SPA_EDT_RATE_2_1,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_1,''),
                   SPAFILE.SPA_EDT_AMT_2_1,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_1,''),
                   SPAFILE.SPA_EDT_TERM_2_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_2,''),
                   SPAFILE.SPA_EDT_RATE_2_2,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_2,''),
                   SPAFILE.SPA_EDT_AMT_2_2,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_2,''),
                   SPAFILE.SPA_EDT_TERM_2_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_3,''),
                   SPAFILE.SPA_EDT_RATE_2_3,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_3,''),
                   SPAFILE.SPA_EDT_AMT_2_3,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_3,''),
                   SPAFILE.SPA_EDT_TERM_2_3,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_4,''),
                   SPAFILE.SPA_EDT_RATE_2_4,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_4,''),
                   SPAFILE.SPA_EDT_AMT_2_4,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_4,''),
                   SPAFILE.SPA_EDT_TERM_2_4,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_5,''),
                   SPAFILE.SPA_EDT_RATE_2_5,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_5,''),
                   SPAFILE.SPA_EDT_AMT_2_5,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_5,''),
                   SPAFILE.SPA_EDT_TERM_2_5,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_6,''),
                   SPAFILE.SPA_EDT_RATE_2_6,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_6,''),
                   SPAFILE.SPA_EDT_AMT_2_6,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_6,''),
                   SPAFILE.SPA_EDT_TERM_2_6
            INTO :QSPA-REC
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
           MOVE SPA-SELECT-PATH TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.
           INITIALIZE QSPA-REC.

           IF ( SPA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SPA1_CURSOR INTO :QSPA-REC
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
       READ-SPA1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.
           INITIALIZE QSPA-REC.

           IF ( SPA1-NG-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SPA1_NG_CURSOR INTO :QSPA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPA1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-SPA-FIELDS.

      *--------------------------------------------------------
       WRITE-SPA1-FILE.
           ACCEPT SPA-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
           MOVE SPA1-KEY        TO E-KEYX.
           PERFORM SET-SPA-FIELDS.

           EXEC SQL
            INSERT INTO DBO.SPAFILE
             (SPAFILE.SPA_ORGST,
              SPAFILE.SPA_SPRCLASS,
              SPAFILE.SPA_SUBCLASS,
              SPAFILE.SPA_LAWCODE,
              SPAFILE.SPA_LTOUCH_DATE,
              SPAFILE.SPA_DESC,
              SPAFILE.SPA_NOTES,
              SPAFILE.SPA_EFFDATE,
              SPAFILE.SPA_EXPDATE,
              SPAFILE.SPA_EXPD_MONTHS,
              SPAFILE.SPA_EXPD_DAYS,
              SPAFILE.SPA_EXPD_FRMLA,
              SPAFILE.SPA_EXPD_EXCEPT,
              SPAFILE.SPA_EXPR_FRMLA,
              SPAFILE.SPA_EXPR_REDU_FRMLA,
              SPAFILE.SPA_EXPR_RATE_1,
              SPAFILE.SPA_EXPR_BREAK_1,
              SPAFILE.SPA_EXPR_YRTYPE_1,
              SPAFILE.SPA_EXPR_INTMETH_1,
              SPAFILE.SPA_EXPR_RATE_2,
              SPAFILE.SPA_EXPR_BREAK_2,
              SPAFILE.SPA_EXPR_YRTYPE_2,
              SPAFILE.SPA_EXPR_INTMETH_2,
              SPAFILE.SPA_EXPR_RATE_3,
              SPAFILE.SPA_EXPR_BREAK_3,
              SPAFILE.SPA_EXPR_YRTYPE_3,
              SPAFILE.SPA_EXPR_INTMETH_3,
              SPAFILE.SPA_EXPR_RATE_4,
              SPAFILE.SPA_EXPR_BREAK_4,
              SPAFILE.SPA_EXPR_YRTYPE_4,
              SPAFILE.SPA_EXPR_INTMETH_4,
              SPAFILE.SPA_EXPR_RATE_5,
              SPAFILE.SPA_EXPR_BREAK_5,
              SPAFILE.SPA_EXPR_YRTYPE_5,
              SPAFILE.SPA_EXPR_INTMETH_5,
              SPAFILE.SPA_EXPR_LCHG,
              SPAFILE.SPA_JUDGFRMLA,
              SPAFILE.SPA_JUDGRATE,
              SPAFILE.SPA_JUDGYRTYPE,
              SPAFILE.SPA_JUDGINTMETH,
              SPAFILE.SPA_ACC_FRMLA,
              SPAFILE.SPA_ACC_YRTYPE,
              SPAFILE.SPA_ACC_INTMETH,
              SPAFILE.SPA_ACC_RATE_1,
              SPAFILE.SPA_ACC_BREAK_1,
              SPAFILE.SPA_ACC_RATE_2,
              SPAFILE.SPA_ACC_BREAK_2,
              SPAFILE.SPA_ACC_RATE_3,
              SPAFILE.SPA_ACC_BREAK_3,
              SPAFILE.SPA_ACC_RATE_4,
              SPAFILE.SPA_ACC_BREAK_4,
              SPAFILE.SPA_ACC_RATE_5,
              SPAFILE.SPA_ACC_BREAK_5,
              SPAFILE.SPA_1098FRMLA,
              SPAFILE.SPA_DISFRMLA,
              SPAFILE.SPA_CONTRFRMLA,
              SPAFILE.SPA_BANKRULE,
              SPAFILE.SPA_CONTPDTH,
              SPAFILE.SPA_FINCHG,
              SPAFILE.SPA_BANKUNITPER_CD,
              SPAFILE.SPA_TRWBANKRULE,
              SPAFILE.SPA_TRWCONTPDTH,
              SPAFILE.SPA_TRWBANKUNITPER_CD,
              SPAFILE.SPA_RECFRMLA,
              SPAFILE.SPA_RECPAYCD,
              SPAFILE.SPA_RECPDTH,
              SPAFILE.SPA_DISFRMLA_SPAY_FG,
              SPAFILE.SPA_ERNFRMLA_1,
              SPAFILE.SPA_ERNAMT_1,
              SPAFILE.SPA_ERNPERCT_1,
              SPAFILE.SPA_ERNPERCD_1,
              SPAFILE.SPA_ERNACCRL_FG_1,
              SPAFILE.SPA_ERNACCRL_START_CD_1,
              SPAFILE.SPA_ERNACCRL_DFCD_1,
              SPAFILE.SPA_ERN_DAYS_1,
              SPAFILE.SPA_ERN_TERM_1,
              SPAFILE.SPA_ERN_TYPE_1,
              SPAFILE.SPA_ERNFRMLA_2,
              SPAFILE.SPA_ERNAMT_2,
              SPAFILE.SPA_ERNPERCT_2,
              SPAFILE.SPA_ERNPERCD_2,
              SPAFILE.SPA_ERNACCRL_FG_2,
              SPAFILE.SPA_ERNACCRL_START_CD_2,
              SPAFILE.SPA_ERNACCRL_DFCD_2,
              SPAFILE.SPA_ERN_DAYS_2,
              SPAFILE.SPA_ERN_TERM_2,
              SPAFILE.SPA_ERN_TYPE_2,
              SPAFILE.SPA_ERNFRMLA_3,
              SPAFILE.SPA_ERNAMT_3,
              SPAFILE.SPA_ERNPERCT_3,
              SPAFILE.SPA_ERNPERCD_3,
              SPAFILE.SPA_ERNACCRL_FG_3,
              SPAFILE.SPA_ERNACCRL_START_CD_3,
              SPAFILE.SPA_ERNACCRL_DFCD_3,
              SPAFILE.SPA_ERN_DAYS_3,
              SPAFILE.SPA_ERN_TERM_3,
              SPAFILE.SPA_ERN_TYPE_3,
              SPAFILE.SPA_ERNFRMLA_4,
              SPAFILE.SPA_ERNAMT_4,
              SPAFILE.SPA_ERNPERCT_4,
              SPAFILE.SPA_ERNPERCD_4,
              SPAFILE.SPA_ERNACCRL_FG_4,
              SPAFILE.SPA_ERNACCRL_START_CD_4,
              SPAFILE.SPA_ERNACCRL_DFCD_4,
              SPAFILE.SPA_ERN_DAYS_4,
              SPAFILE.SPA_ERN_TERM_4,
              SPAFILE.SPA_ERN_TYPE_4,
              SPAFILE.SPA_ERNFRMLA_5,
              SPAFILE.SPA_ERNAMT_5,
              SPAFILE.SPA_ERNPERCT_5,
              SPAFILE.SPA_ERNPERCD_5,
              SPAFILE.SPA_ERNACCRL_FG_5,
              SPAFILE.SPA_ERNACCRL_START_CD_5,
              SPAFILE.SPA_ERNACCRL_DFCD_5,
              SPAFILE.SPA_ERN_DAYS_5,
              SPAFILE.SPA_ERN_TERM_5,
              SPAFILE.SPA_ERN_TYPE_5,
              SPAFILE.SPA_ERNFRMLA_6,
              SPAFILE.SPA_ERNAMT_6,
              SPAFILE.SPA_ERNPERCT_6,
              SPAFILE.SPA_ERNPERCD_6,
              SPAFILE.SPA_ERNACCRL_FG_6,
              SPAFILE.SPA_ERNACCRL_START_CD_6,
              SPAFILE.SPA_ERNACCRL_DFCD_6,
              SPAFILE.SPA_ERN_DAYS_6,
              SPAFILE.SPA_ERN_TERM_6,
              SPAFILE.SPA_ERN_TYPE_6,
              SPAFILE.SPA_ERNFRMLA_7,
              SPAFILE.SPA_ERNAMT_7,
              SPAFILE.SPA_ERNPERCT_7,
              SPAFILE.SPA_ERNPERCD_7,
              SPAFILE.SPA_ERNACCRL_FG_7,
              SPAFILE.SPA_ERNACCRL_START_CD_7,
              SPAFILE.SPA_ERNACCRL_DFCD_7,
              SPAFILE.SPA_ERN_DAYS_7,
              SPAFILE.SPA_ERN_TERM_7,
              SPAFILE.SPA_ERN_TYPE_7,
              SPAFILE.SPA_ERNFRMLA_8,
              SPAFILE.SPA_ERNAMT_8,
              SPAFILE.SPA_ERNPERCT_8,
              SPAFILE.SPA_ERNPERCD_8,
              SPAFILE.SPA_ERNACCRL_FG_8,
              SPAFILE.SPA_ERNACCRL_START_CD_8,
              SPAFILE.SPA_ERNACCRL_DFCD_8,
              SPAFILE.SPA_ERN_DAYS_8,
              SPAFILE.SPA_ERN_TERM_8,
              SPAFILE.SPA_ERN_TYPE_8,
              SPAFILE.SPA_ERNFRMLA_9,
              SPAFILE.SPA_ERNAMT_9,
              SPAFILE.SPA_ERNPERCT_9,
              SPAFILE.SPA_ERNPERCD_9,
              SPAFILE.SPA_ERNACCRL_FG_9,
              SPAFILE.SPA_ERNACCRL_START_CD_9,
              SPAFILE.SPA_ERNACCRL_DFCD_9,
              SPAFILE.SPA_ERN_DAYS_9,
              SPAFILE.SPA_ERN_TERM_9,
              SPAFILE.SPA_ERN_TYPE_9,
              SPAFILE.SPA_ERNFRMLA_10,
              SPAFILE.SPA_ERNAMT_10,
              SPAFILE.SPA_ERNPERCT_10,
              SPAFILE.SPA_ERNPERCD_10,
              SPAFILE.SPA_ERNACCRL_FG_10,
              SPAFILE.SPA_ERNACCRL_START_CD_10,
              SPAFILE.SPA_ERNACCRL_DFCD_10,
              SPAFILE.SPA_ERN_DAYS_10,
              SPAFILE.SPA_ERN_TERM_10,
              SPAFILE.SPA_ERN_TYPE_10,
              SPAFILE.SPA_ERNACCEL_CD,
              SPAFILE.SPA_ERNJD_CD,
              SPAFILE.SPA_MIDSTRERNCD,
              SPAFILE.SPA_EARN_ORDER_1,
              SPAFILE.SPA_EARN_ORDER_2,
              SPAFILE.SPA_EARN_ORDER_3,
              SPAFILE.SPA_EARN_ORDER_4,
              SPAFILE.SPA_EARN_ORDER_5,
              SPAFILE.SPA_EARN_ORDER_6,
              SPAFILE.SPA_EARN_ORDER_7,
              SPAFILE.SPA_EARN_ORDER_8,
              SPAFILE.SPA_EARN_ORDER_9,
              SPAFILE.SPA_EARN_ORDER_10,
              SPAFILE.SPA_ERNDELQFRMLA,
              SPAFILE.SPA_ERNDELQ_START,
              SPAFILE.SPA_ERNDELQ_STOP,
              SPAFILE.SPA_1098_INC_LC,
              SPAFILE.SPA_MAX_IB_ACR_DAYS,
              SPAFILE.SPA_LCGRACE,
              SPAFILE.SPA_LCGRACECD,
              SPAFILE.SPA_LCTYPE,
              SPAFILE.SPA_LCRATE,
              SPAFILE.SPA_LCFRMLA,
              SPAFILE.SPA_LCMINCHG,
              SPAFILE.SPA_LCMAXAMT,
              SPAFILE.SPA_LCDELFAC,
              SPAFILE.SPA_LCYRTYPE,
              SPAFILE.SPA_LC_CODE,
              SPAFILE.SPA_LCUNITPER_CD,
              SPAFILE.SPA_DEFFRMLA,
              SPAFILE.SPA_DEFEXCEPT,
              SPAFILE.SPA_DEFWORKER,
              SPAFILE.SPA_DEFPERCD,
              SPAFILE.SPA_DEFBASE,
              SPAFILE.SPA_DEFPOLCD,
              SPAFILE.SPA_DEFPOLMX_1,
              SPAFILE.SPA_DEFPOLMX_2,
              SPAFILE.SPA_DEFPOLMX_3,
              SPAFILE.SPA_DEFMIN,
              SPAFILE.SPA_DEFMAX,
              SPAFILE.SPA_DEFUNITPER_CD,
              SPAFILE.SPA_PRFORM_1,
              SPAFILE.SPA_PRFLAG_1,
              SPAFILE.SPA_PRFORM_2,
              SPAFILE.SPA_PRFLAG_2,
              SPAFILE.SPA_PRFORM_3,
              SPAFILE.SPA_PRFLAG_3,
              SPAFILE.SPA_PRFORM_4,
              SPAFILE.SPA_PRFLAG_4,
              SPAFILE.SPA_PRFORM_5,
              SPAFILE.SPA_PRFLAG_5,
              SPAFILE.SPA_PRFORM_6,
              SPAFILE.SPA_PRFLAG_6,
              SPAFILE.SPA_PRFORM_7,
              SPAFILE.SPA_PRFLAG_7,
              SPAFILE.SPA_PRFORM_8,
              SPAFILE.SPA_PRFLAG_8,
              SPAFILE.SPA_PRFORM_9,
              SPAFILE.SPA_PRFLAG_9,
              SPAFILE.SPA_PRFORM_10,
              SPAFILE.SPA_PRFLAG_10,
              SPAFILE.SPA_PRFORM_11,
              SPAFILE.SPA_PRFLAG_11,
              SPAFILE.SPA_PRFORM_12,
              SPAFILE.SPA_PRFLAG_12,
              SPAFILE.SPA_PRFORM_13,
              SPAFILE.SPA_PRFLAG_13,
              SPAFILE.SPA_PRFORM_14,
              SPAFILE.SPA_PRFLAG_14,
              SPAFILE.SPA_PRFORM_15,
              SPAFILE.SPA_PRFLAG_15,
              SPAFILE.SPA_PRFORM_16,
              SPAFILE.SPA_PRFLAG_16,
              SPAFILE.SPA_PRFORM_17,
              SPAFILE.SPA_PRFLAG_17,
              SPAFILE.SPA_PRFORM_18,
              SPAFILE.SPA_PRFLAG_18,
              SPAFILE.SPA_PRFORM_19,
              SPAFILE.SPA_PRFLAG_19,
              SPAFILE.SPA_PRFORM_20,
              SPAFILE.SPA_PRFLAG_20,
              SPAFILE.SPA_IMED_REC,
              SPAFILE.SPA_SCFRMLA,
              SPAFILE.SPA_SCRATE,
              SPAFILE.SPA_SCMIN,
              SPAFILE.SPA_SCMAX,
              SPAFILE.SPA_MFFRMLA,
              SPAFILE.SPA_MFRATE,
              SPAFILE.SPA_MFMIN,
              SPAFILE.SPA_MFMAX,
              SPAFILE.SPA_INTMETHOD,
              SPAFILE.SPA_YEARTYPE,
              SPAFILE.SPA_PYEXFRMLA,
              SPAFILE.SPA_PYEXSPCODE,
              SPAFILE.SPA_PYEXYEARTP,
              SPAFILE.SPA_PYEXFMDATE,
              SPAFILE.SPA_PYEXBASECD,
              SPAFILE.SPA_PYEXBASE,
              SPAFILE.SPA_PYEXDAYS,
              SPAFILE.SPA_CAL_SPFRMLA,
              SPAFILE.SPA_CAL_RATETYPE_1,
              SPAFILE.SPA_CAL_BREAKTYPE_1,
              SPAFILE.SPA_CAL_CHGABLE_1,
              SPAFILE.SPA_CAL_NORATES_1,
              SPAFILE.SPA_CAL_TBFRMLA_1,
              SPAFILE.SPA_CAL_RATE_1_1,
              SPAFILE.SPA_CAL_BREAK_1_1,
              SPAFILE.SPA_CAL_RATE_1_2,
              SPAFILE.SPA_CAL_BREAK_1_2,
              SPAFILE.SPA_CAL_RATE_1_3,
              SPAFILE.SPA_CAL_BREAK_1_3,
              SPAFILE.SPA_CAL_RATE_1_4,
              SPAFILE.SPA_CAL_BREAK_1_4,
              SPAFILE.SPA_CAL_RATE_1_5,
              SPAFILE.SPA_CAL_BREAK_1_5,
              SPAFILE.SPA_CAL_RATETYPE_2,
              SPAFILE.SPA_CAL_BREAKTYPE_2,
              SPAFILE.SPA_CAL_CHGABLE_2,
              SPAFILE.SPA_CAL_NORATES_2,
              SPAFILE.SPA_CAL_TBFRMLA_2,
              SPAFILE.SPA_CAL_RATE_2_1,
              SPAFILE.SPA_CAL_BREAK_2_1,
              SPAFILE.SPA_CAL_RATE_2_2,
              SPAFILE.SPA_CAL_BREAK_2_2,
              SPAFILE.SPA_CAL_RATE_2_3,
              SPAFILE.SPA_CAL_BREAK_2_3,
              SPAFILE.SPA_CAL_RATE_2_4,
              SPAFILE.SPA_CAL_BREAK_2_4,
              SPAFILE.SPA_CAL_RATE_2_5,
              SPAFILE.SPA_CAL_BREAK_2_5,
              SPAFILE.SPA_MINSF_FRMLA,
              SPAFILE.SPA_MINSF_CHG,
              SPAFILE.SPA_MAXAPR_FRMLA,
              SPAFILE.SPA_EDT_NOEDITS_1,
              SPAFILE.SPA_EDT_TYPE_1_1,
              SPAFILE.SPA_EDT_HL_1_1,
              SPAFILE.SPA_EDT_COLL_1_1,
              SPAFILE.SPA_EDT_CD_1_1,
              SPAFILE.SPA_EDT_RATE_1_1,
              SPAFILE.SPA_EDT_RATETYPE_1_1,
              SPAFILE.SPA_EDT_AMT_1_1,
              SPAFILE.SPA_EDT_AMTYPE_1_1,
              SPAFILE.SPA_EDT_TERM_1_1,
              SPAFILE.SPA_EDT_TYPE_1_2,
              SPAFILE.SPA_EDT_HL_1_2,
              SPAFILE.SPA_EDT_COLL_1_2,
              SPAFILE.SPA_EDT_CD_1_2,
              SPAFILE.SPA_EDT_RATE_1_2,
              SPAFILE.SPA_EDT_RATETYPE_1_2,
              SPAFILE.SPA_EDT_AMT_1_2,
              SPAFILE.SPA_EDT_AMTYPE_1_2,
              SPAFILE.SPA_EDT_TERM_1_2,
              SPAFILE.SPA_EDT_TYPE_1_3,
              SPAFILE.SPA_EDT_HL_1_3,
              SPAFILE.SPA_EDT_COLL_1_3,
              SPAFILE.SPA_EDT_CD_1_3,
              SPAFILE.SPA_EDT_RATE_1_3,
              SPAFILE.SPA_EDT_RATETYPE_1_3,
              SPAFILE.SPA_EDT_AMT_1_3,
              SPAFILE.SPA_EDT_AMTYPE_1_3,
              SPAFILE.SPA_EDT_TERM_1_3,
              SPAFILE.SPA_EDT_TYPE_1_4,
              SPAFILE.SPA_EDT_HL_1_4,
              SPAFILE.SPA_EDT_COLL_1_4,
              SPAFILE.SPA_EDT_CD_1_4,
              SPAFILE.SPA_EDT_RATE_1_4,
              SPAFILE.SPA_EDT_RATETYPE_1_4,
              SPAFILE.SPA_EDT_AMT_1_4,
              SPAFILE.SPA_EDT_AMTYPE_1_4,
              SPAFILE.SPA_EDT_TERM_1_4,
              SPAFILE.SPA_EDT_TYPE_1_5,
              SPAFILE.SPA_EDT_HL_1_5,
              SPAFILE.SPA_EDT_COLL_1_5,
              SPAFILE.SPA_EDT_CD_1_5,
              SPAFILE.SPA_EDT_RATE_1_5,
              SPAFILE.SPA_EDT_RATETYPE_1_5,
              SPAFILE.SPA_EDT_AMT_1_5,
              SPAFILE.SPA_EDT_AMTYPE_1_5,
              SPAFILE.SPA_EDT_TERM_1_5,
              SPAFILE.SPA_EDT_TYPE_1_6,
              SPAFILE.SPA_EDT_HL_1_6,
              SPAFILE.SPA_EDT_COLL_1_6,
              SPAFILE.SPA_EDT_CD_1_6,
              SPAFILE.SPA_EDT_RATE_1_6,
              SPAFILE.SPA_EDT_RATETYPE_1_6,
              SPAFILE.SPA_EDT_AMT_1_6,
              SPAFILE.SPA_EDT_AMTYPE_1_6,
              SPAFILE.SPA_EDT_TERM_1_6,
              SPAFILE.SPA_EDT_NOEDITS_2,
              SPAFILE.SPA_EDT_TYPE_2_1,
              SPAFILE.SPA_EDT_HL_2_1,
              SPAFILE.SPA_EDT_COLL_2_1,
              SPAFILE.SPA_EDT_CD_2_1,
              SPAFILE.SPA_EDT_RATE_2_1,
              SPAFILE.SPA_EDT_RATETYPE_2_1,
              SPAFILE.SPA_EDT_AMT_2_1,
              SPAFILE.SPA_EDT_AMTYPE_2_1,
              SPAFILE.SPA_EDT_TERM_2_1,
              SPAFILE.SPA_EDT_TYPE_2_2,
              SPAFILE.SPA_EDT_HL_2_2,
              SPAFILE.SPA_EDT_COLL_2_2,
              SPAFILE.SPA_EDT_CD_2_2,
              SPAFILE.SPA_EDT_RATE_2_2,
              SPAFILE.SPA_EDT_RATETYPE_2_2,
              SPAFILE.SPA_EDT_AMT_2_2,
              SPAFILE.SPA_EDT_AMTYPE_2_2,
              SPAFILE.SPA_EDT_TERM_2_2,
              SPAFILE.SPA_EDT_TYPE_2_3,
              SPAFILE.SPA_EDT_HL_2_3,
              SPAFILE.SPA_EDT_COLL_2_3,
              SPAFILE.SPA_EDT_CD_2_3,
              SPAFILE.SPA_EDT_RATE_2_3,
              SPAFILE.SPA_EDT_RATETYPE_2_3,
              SPAFILE.SPA_EDT_AMT_2_3,
              SPAFILE.SPA_EDT_AMTYPE_2_3,
              SPAFILE.SPA_EDT_TERM_2_3,
              SPAFILE.SPA_EDT_TYPE_2_4,
              SPAFILE.SPA_EDT_HL_2_4,
              SPAFILE.SPA_EDT_COLL_2_4,
              SPAFILE.SPA_EDT_CD_2_4,
              SPAFILE.SPA_EDT_RATE_2_4,
              SPAFILE.SPA_EDT_RATETYPE_2_4,
              SPAFILE.SPA_EDT_AMT_2_4,
              SPAFILE.SPA_EDT_AMTYPE_2_4,
              SPAFILE.SPA_EDT_TERM_2_4,
              SPAFILE.SPA_EDT_TYPE_2_5,
              SPAFILE.SPA_EDT_HL_2_5,
              SPAFILE.SPA_EDT_COLL_2_5,
              SPAFILE.SPA_EDT_CD_2_5,
              SPAFILE.SPA_EDT_RATE_2_5,
              SPAFILE.SPA_EDT_RATETYPE_2_5,
              SPAFILE.SPA_EDT_AMT_2_5,
              SPAFILE.SPA_EDT_AMTYPE_2_5,
              SPAFILE.SPA_EDT_TERM_2_5,
              SPAFILE.SPA_EDT_TYPE_2_6,
              SPAFILE.SPA_EDT_HL_2_6,
              SPAFILE.SPA_EDT_COLL_2_6,
              SPAFILE.SPA_EDT_CD_2_6,
              SPAFILE.SPA_EDT_RATE_2_6,
              SPAFILE.SPA_EDT_RATETYPE_2_6,
              SPAFILE.SPA_EDT_AMT_2_6,
              SPAFILE.SPA_EDT_AMTYPE_2_6,
              SPAFILE.SPA_EDT_TERM_2_6)
            VALUES
             (:QSPA-ORGST,
              :QSPA-SPRCLASS,
              :QSPA-SUBCLASS,
              :QSPA-LAWCODE,
              NULLIF(:QSPA-LTOUCH-DATE,'1900-01-01'),
              :QSPA-DESC,
              :QSPA-NOTES,
              NULLIF(:QSPA-EFFDATE,'1900-01-01'),
              NULLIF(:QSPA-EXPDATE,'1900-01-01'),
              :QSPA-EXPD-MONTHS,
              :QSPA-EXPD-DAYS,
              :QSPA-EXPD-FRMLA,
              :QSPA-EXPD-EXCEPT,
              :QSPA-EXPR-FRMLA,
              :QSPA-EXPR-REDU-FRMLA,
              :QSPA-EXPR-RATE-1,
              :QSPA-EXPR-BREAK-1,
              :QSPA-EXPR-YRTYPE-1,
              :QSPA-EXPR-INTMETH-1,
              :QSPA-EXPR-RATE-2,
              :QSPA-EXPR-BREAK-2,
              :QSPA-EXPR-YRTYPE-2,
              :QSPA-EXPR-INTMETH-2,
              :QSPA-EXPR-RATE-3,
              :QSPA-EXPR-BREAK-3,
              :QSPA-EXPR-YRTYPE-3,
              :QSPA-EXPR-INTMETH-3,
              :QSPA-EXPR-RATE-4,
              :QSPA-EXPR-BREAK-4,
              :QSPA-EXPR-YRTYPE-4,
              :QSPA-EXPR-INTMETH-4,
              :QSPA-EXPR-RATE-5,
              :QSPA-EXPR-BREAK-5,
              :QSPA-EXPR-YRTYPE-5,
              :QSPA-EXPR-INTMETH-5,
              :QSPA-EXPR-LCHG,
              :QSPA-JUDGFRMLA,
              :QSPA-JUDGRATE,
              :QSPA-JUDGYRTYPE,
              :QSPA-JUDGINTMETH,
              :QSPA-ACC-FRMLA,
              :QSPA-ACC-YRTYPE,
              :QSPA-ACC-INTMETH,
              :QSPA-ACC-RATE-1,
              :QSPA-ACC-BREAK-1,
              :QSPA-ACC-RATE-2,
              :QSPA-ACC-BREAK-2,
              :QSPA-ACC-RATE-3,
              :QSPA-ACC-BREAK-3,
              :QSPA-ACC-RATE-4,
              :QSPA-ACC-BREAK-4,
              :QSPA-ACC-RATE-5,
              :QSPA-ACC-BREAK-5,
              :QSPA-1098FRMLA,
              :QSPA-DISFRMLA,
              :QSPA-CONTRFRMLA,
              :QSPA-BANKRULE,
              :QSPA-CONTPDTH,
              :QSPA-FINCHG,
              :QSPA-BANKUNITPER-CD,
              :QSPA-TRWBANKRULE,
              :QSPA-TRWCONTPDTH,
              :QSPA-TRWBANKUNITPER-CD,
              :QSPA-RECFRMLA,
              :QSPA-RECPAYCD,
              :QSPA-RECPDTH,
              :QSPA-DISFRMLA-SPAY-FG,
              :QSPA-ERNFRMLA-1,
              :QSPA-ERNAMT-1,
              :QSPA-ERNPERCT-1,
              :QSPA-ERNPERCD-1,
              :QSPA-ERNACCRL-FG-1,
              :QSPA-ERNACCRL-START-CD-1,
              :QSPA-ERNACCRL-DFCD-1,
              :QSPA-ERN-DAYS-1,
              :QSPA-ERN-TERM-1,
              :QSPA-ERN-TYPE-1,
              :QSPA-ERNFRMLA-2,
              :QSPA-ERNAMT-2,
              :QSPA-ERNPERCT-2,
              :QSPA-ERNPERCD-2,
              :QSPA-ERNACCRL-FG-2,
              :QSPA-ERNACCRL-START-CD-2,
              :QSPA-ERNACCRL-DFCD-2,
              :QSPA-ERN-DAYS-2,
              :QSPA-ERN-TERM-2,
              :QSPA-ERN-TYPE-2,
              :QSPA-ERNFRMLA-3,
              :QSPA-ERNAMT-3,
              :QSPA-ERNPERCT-3,
              :QSPA-ERNPERCD-3,
              :QSPA-ERNACCRL-FG-3,
              :QSPA-ERNACCRL-START-CD-3,
              :QSPA-ERNACCRL-DFCD-3,
              :QSPA-ERN-DAYS-3,
              :QSPA-ERN-TERM-3,
              :QSPA-ERN-TYPE-3,
              :QSPA-ERNFRMLA-4,
              :QSPA-ERNAMT-4,
              :QSPA-ERNPERCT-4,
              :QSPA-ERNPERCD-4,
              :QSPA-ERNACCRL-FG-4,
              :QSPA-ERNACCRL-START-CD-4,
              :QSPA-ERNACCRL-DFCD-4,
              :QSPA-ERN-DAYS-4,
              :QSPA-ERN-TERM-4,
              :QSPA-ERN-TYPE-4,
              :QSPA-ERNFRMLA-5,
              :QSPA-ERNAMT-5,
              :QSPA-ERNPERCT-5,
              :QSPA-ERNPERCD-5,
              :QSPA-ERNACCRL-FG-5,
              :QSPA-ERNACCRL-START-CD-5,
              :QSPA-ERNACCRL-DFCD-5,
              :QSPA-ERN-DAYS-5,
              :QSPA-ERN-TERM-5,
              :QSPA-ERN-TYPE-5,
              :QSPA-ERNFRMLA-6,
              :QSPA-ERNAMT-6,
              :QSPA-ERNPERCT-6,
              :QSPA-ERNPERCD-6,
              :QSPA-ERNACCRL-FG-6,
              :QSPA-ERNACCRL-START-CD-6,
              :QSPA-ERNACCRL-DFCD-6,
              :QSPA-ERN-DAYS-6,
              :QSPA-ERN-TERM-6,
              :QSPA-ERN-TYPE-6,
              :QSPA-ERNFRMLA-7,
              :QSPA-ERNAMT-7,
              :QSPA-ERNPERCT-7,
              :QSPA-ERNPERCD-7,
              :QSPA-ERNACCRL-FG-7,
              :QSPA-ERNACCRL-START-CD-7,
              :QSPA-ERNACCRL-DFCD-7,
              :QSPA-ERN-DAYS-7,
              :QSPA-ERN-TERM-7,
              :QSPA-ERN-TYPE-7,
              :QSPA-ERNFRMLA-8,
              :QSPA-ERNAMT-8,
              :QSPA-ERNPERCT-8,
              :QSPA-ERNPERCD-8,
              :QSPA-ERNACCRL-FG-8,
              :QSPA-ERNACCRL-START-CD-8,
              :QSPA-ERNACCRL-DFCD-8,
              :QSPA-ERN-DAYS-8,
              :QSPA-ERN-TERM-8,
              :QSPA-ERN-TYPE-8,
              :QSPA-ERNFRMLA-9,
              :QSPA-ERNAMT-9,
              :QSPA-ERNPERCT-9,
              :QSPA-ERNPERCD-9,
              :QSPA-ERNACCRL-FG-9,
              :QSPA-ERNACCRL-START-CD-9,
              :QSPA-ERNACCRL-DFCD-9,
              :QSPA-ERN-DAYS-9,
              :QSPA-ERN-TERM-9,
              :QSPA-ERN-TYPE-9,
              :QSPA-ERNFRMLA-10,
              :QSPA-ERNAMT-10,
              :QSPA-ERNPERCT-10,
              :QSPA-ERNPERCD-10,
              :QSPA-ERNACCRL-FG-10,
              :QSPA-ERNACCRL-START-CD-10,
              :QSPA-ERNACCRL-DFCD-10,
              :QSPA-ERN-DAYS-10,
              :QSPA-ERN-TERM-10,
              :QSPA-ERN-TYPE-10,
              :QSPA-ERNACCEL-CD,
              :QSPA-ERNJD-CD,
              :QSPA-MIDSTRERNCD,
              :QSPA-EARN-ORDER-1,
              :QSPA-EARN-ORDER-2,
              :QSPA-EARN-ORDER-3,
              :QSPA-EARN-ORDER-4,
              :QSPA-EARN-ORDER-5,
              :QSPA-EARN-ORDER-6,
              :QSPA-EARN-ORDER-7,
              :QSPA-EARN-ORDER-8,
              :QSPA-EARN-ORDER-9,
              :QSPA-EARN-ORDER-10,
              :QSPA-ERNDELQFRMLA,
              :QSPA-ERNDELQ-START,
              :QSPA-ERNDELQ-STOP,
              :QSPA-1098-INC-LC,
              :QSPA-MAX-IB-ACR-DAYS,
              :QSPA-LCGRACE,
              :QSPA-LCGRACECD,
              :QSPA-LCTYPE,
              :QSPA-LCRATE,
              :QSPA-LCFRMLA,
              :QSPA-LCMINCHG,
              :QSPA-LCMAXAMT,
              :QSPA-LCDELFAC,
              :QSPA-LCYRTYPE,
              :QSPA-LC-CODE,
              :QSPA-LCUNITPER-CD,
              :QSPA-DEFFRMLA,
              :QSPA-DEFEXCEPT,
              :QSPA-DEFWORKER,
              :QSPA-DEFPERCD,
              :QSPA-DEFBASE,
              :QSPA-DEFPOLCD,
              :QSPA-DEFPOLMX-1,
              :QSPA-DEFPOLMX-2,
              :QSPA-DEFPOLMX-3,
              :QSPA-DEFMIN,
              :QSPA-DEFMAX,
              :QSPA-DEFUNITPER-CD,
              :QSPA-PRFORM-1,
              :QSPA-PRFLAG-1,
              :QSPA-PRFORM-2,
              :QSPA-PRFLAG-2,
              :QSPA-PRFORM-3,
              :QSPA-PRFLAG-3,
              :QSPA-PRFORM-4,
              :QSPA-PRFLAG-4,
              :QSPA-PRFORM-5,
              :QSPA-PRFLAG-5,
              :QSPA-PRFORM-6,
              :QSPA-PRFLAG-6,
              :QSPA-PRFORM-7,
              :QSPA-PRFLAG-7,
              :QSPA-PRFORM-8,
              :QSPA-PRFLAG-8,
              :QSPA-PRFORM-9,
              :QSPA-PRFLAG-9,
              :QSPA-PRFORM-10,
              :QSPA-PRFLAG-10,
              :QSPA-PRFORM-11,
              :QSPA-PRFLAG-11,
              :QSPA-PRFORM-12,
              :QSPA-PRFLAG-12,
              :QSPA-PRFORM-13,
              :QSPA-PRFLAG-13,
              :QSPA-PRFORM-14,
              :QSPA-PRFLAG-14,
              :QSPA-PRFORM-15,
              :QSPA-PRFLAG-15,
              :QSPA-PRFORM-16,
              :QSPA-PRFLAG-16,
              :QSPA-PRFORM-17,
              :QSPA-PRFLAG-17,
              :QSPA-PRFORM-18,
              :QSPA-PRFLAG-18,
              :QSPA-PRFORM-19,
              :QSPA-PRFLAG-19,
              :QSPA-PRFORM-20,
              :QSPA-PRFLAG-20,
              :QSPA-IMED-REC,
              :QSPA-SCFRMLA,
              :QSPA-SCRATE,
              :QSPA-SCMIN,
              :QSPA-SCMAX,
              :QSPA-MFFRMLA,
              :QSPA-MFRATE,
              :QSPA-MFMIN,
              :QSPA-MFMAX,
              :QSPA-INTMETHOD,
              :QSPA-YEARTYPE,
              :QSPA-PYEXFRMLA,
              :QSPA-PYEXSPCODE,
              :QSPA-PYEXYEARTP,
              :QSPA-PYEXFMDATE,
              :QSPA-PYEXBASECD,
              :QSPA-PYEXBASE,
              :QSPA-PYEXDAYS,
              :QSPA-CAL-SPFRMLA,
              :QSPA-CAL-RATETYPE-1,
              :QSPA-CAL-BREAKTYPE-1,
              :QSPA-CAL-CHGABLE-1,
              :QSPA-CAL-NORATES-1,
              :QSPA-CAL-TBFRMLA-1,
              :QSPA-CAL-RATE-1-1,
              :QSPA-CAL-BREAK-1-1,
              :QSPA-CAL-RATE-1-2,
              :QSPA-CAL-BREAK-1-2,
              :QSPA-CAL-RATE-1-3,
              :QSPA-CAL-BREAK-1-3,
              :QSPA-CAL-RATE-1-4,
              :QSPA-CAL-BREAK-1-4,
              :QSPA-CAL-RATE-1-5,
              :QSPA-CAL-BREAK-1-5,
              :QSPA-CAL-RATETYPE-2,
              :QSPA-CAL-BREAKTYPE-2,
              :QSPA-CAL-CHGABLE-2,
              :QSPA-CAL-NORATES-2,
              :QSPA-CAL-TBFRMLA-2,
              :QSPA-CAL-RATE-2-1,
              :QSPA-CAL-BREAK-2-1,
              :QSPA-CAL-RATE-2-2,
              :QSPA-CAL-BREAK-2-2,
              :QSPA-CAL-RATE-2-3,
              :QSPA-CAL-BREAK-2-3,
              :QSPA-CAL-RATE-2-4,
              :QSPA-CAL-BREAK-2-4,
              :QSPA-CAL-RATE-2-5,
              :QSPA-CAL-BREAK-2-5,
              :QSPA-MINSF-FRMLA,
              :QSPA-MINSF-CHG,
              :QSPA-MAXAPR-FRMLA,
              :QSPA-EDT-NOEDITS-1,
              :QSPA-EDT-TYPE-1-1,
              :QSPA-EDT-HL-1-1,
              :QSPA-EDT-COLL-1-1,
              :QSPA-EDT-CD-1-1,
              :QSPA-EDT-RATE-1-1,
              :QSPA-EDT-RATETYPE-1-1,
              :QSPA-EDT-AMT-1-1,
              :QSPA-EDT-AMTYPE-1-1,
              :QSPA-EDT-TERM-1-1,
              :QSPA-EDT-TYPE-1-2,
              :QSPA-EDT-HL-1-2,
              :QSPA-EDT-COLL-1-2,
              :QSPA-EDT-CD-1-2,
              :QSPA-EDT-RATE-1-2,
              :QSPA-EDT-RATETYPE-1-2,
              :QSPA-EDT-AMT-1-2,
              :QSPA-EDT-AMTYPE-1-2,
              :QSPA-EDT-TERM-1-2,
              :QSPA-EDT-TYPE-1-3,
              :QSPA-EDT-HL-1-3,
              :QSPA-EDT-COLL-1-3,
              :QSPA-EDT-CD-1-3,
              :QSPA-EDT-RATE-1-3,
              :QSPA-EDT-RATETYPE-1-3,
              :QSPA-EDT-AMT-1-3,
              :QSPA-EDT-AMTYPE-1-3,
              :QSPA-EDT-TERM-1-3,
              :QSPA-EDT-TYPE-1-4,
              :QSPA-EDT-HL-1-4,
              :QSPA-EDT-COLL-1-4,
              :QSPA-EDT-CD-1-4,
              :QSPA-EDT-RATE-1-4,
              :QSPA-EDT-RATETYPE-1-4,
              :QSPA-EDT-AMT-1-4,
              :QSPA-EDT-AMTYPE-1-4,
              :QSPA-EDT-TERM-1-4,
              :QSPA-EDT-TYPE-1-5,
              :QSPA-EDT-HL-1-5,
              :QSPA-EDT-COLL-1-5,
              :QSPA-EDT-CD-1-5,
              :QSPA-EDT-RATE-1-5,
              :QSPA-EDT-RATETYPE-1-5,
              :QSPA-EDT-AMT-1-5,
              :QSPA-EDT-AMTYPE-1-5,
              :QSPA-EDT-TERM-1-5,
              :QSPA-EDT-TYPE-1-6,
              :QSPA-EDT-HL-1-6,
              :QSPA-EDT-COLL-1-6,
              :QSPA-EDT-CD-1-6,
              :QSPA-EDT-RATE-1-6,
              :QSPA-EDT-RATETYPE-1-6,
              :QSPA-EDT-AMT-1-6,
              :QSPA-EDT-AMTYPE-1-6,
              :QSPA-EDT-TERM-1-6,
              :QSPA-EDT-NOEDITS-2,
              :QSPA-EDT-TYPE-2-1,
              :QSPA-EDT-HL-2-1,
              :QSPA-EDT-COLL-2-1,
              :QSPA-EDT-CD-2-1,
              :QSPA-EDT-RATE-2-1,
              :QSPA-EDT-RATETYPE-2-1,
              :QSPA-EDT-AMT-2-1,
              :QSPA-EDT-AMTYPE-2-1,
              :QSPA-EDT-TERM-2-1,
              :QSPA-EDT-TYPE-2-2,
              :QSPA-EDT-HL-2-2,
              :QSPA-EDT-COLL-2-2,
              :QSPA-EDT-CD-2-2,
              :QSPA-EDT-RATE-2-2,
              :QSPA-EDT-RATETYPE-2-2,
              :QSPA-EDT-AMT-2-2,
              :QSPA-EDT-AMTYPE-2-2,
              :QSPA-EDT-TERM-2-2,
              :QSPA-EDT-TYPE-2-3,
              :QSPA-EDT-HL-2-3,
              :QSPA-EDT-COLL-2-3,
              :QSPA-EDT-CD-2-3,
              :QSPA-EDT-RATE-2-3,
              :QSPA-EDT-RATETYPE-2-3,
              :QSPA-EDT-AMT-2-3,
              :QSPA-EDT-AMTYPE-2-3,
              :QSPA-EDT-TERM-2-3,
              :QSPA-EDT-TYPE-2-4,
              :QSPA-EDT-HL-2-4,
              :QSPA-EDT-COLL-2-4,
              :QSPA-EDT-CD-2-4,
              :QSPA-EDT-RATE-2-4,
              :QSPA-EDT-RATETYPE-2-4,
              :QSPA-EDT-AMT-2-4,
              :QSPA-EDT-AMTYPE-2-4,
              :QSPA-EDT-TERM-2-4,
              :QSPA-EDT-TYPE-2-5,
              :QSPA-EDT-HL-2-5,
              :QSPA-EDT-COLL-2-5,
              :QSPA-EDT-CD-2-5,
              :QSPA-EDT-RATE-2-5,
              :QSPA-EDT-RATETYPE-2-5,
              :QSPA-EDT-AMT-2-5,
              :QSPA-EDT-AMTYPE-2-5,
              :QSPA-EDT-TERM-2-5,
              :QSPA-EDT-TYPE-2-6,
              :QSPA-EDT-HL-2-6,
              :QSPA-EDT-COLL-2-6,
              :QSPA-EDT-CD-2-6,
              :QSPA-EDT-RATE-2-6,
              :QSPA-EDT-RATETYPE-2-6,
              :QSPA-EDT-AMT-2-6,
              :QSPA-EDT-AMTYPE-2-6,
              :QSPA-EDT-TERM-2-6)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-SPA1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-SPA1-FILE.

           ACCEPT SPA-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
           MOVE SPA1-KEY        TO E-KEYX.
           PERFORM SET-SPA-FIELDS.

           EXEC SQL
            UPDATE DBO.SPAFILE
             SET
              SPAFILE.SPA_ORGST = :QSPA-ORGST,
              SPAFILE.SPA_SPRCLASS = :QSPA-SPRCLASS,
              SPAFILE.SPA_SUBCLASS = :QSPA-SUBCLASS,
              SPAFILE.SPA_LAWCODE = :QSPA-LAWCODE,
              SPAFILE.SPA_LTOUCH_DATE =
                 NULLIF(:QSPA-LTOUCH-DATE,'1900-01-01'),
              SPAFILE.SPA_DESC = :QSPA-DESC,
              SPAFILE.SPA_NOTES = :QSPA-NOTES,
              SPAFILE.SPA_EFFDATE =
                 NULLIF(:QSPA-EFFDATE,'1900-01-01'),
              SPAFILE.SPA_EXPDATE =
                 NULLIF(:QSPA-EXPDATE,'1900-01-01'),
              SPAFILE.SPA_EXPD_MONTHS = :QSPA-EXPD-MONTHS,
              SPAFILE.SPA_EXPD_DAYS = :QSPA-EXPD-DAYS,
              SPAFILE.SPA_EXPD_FRMLA = :QSPA-EXPD-FRMLA,
              SPAFILE.SPA_EXPD_EXCEPT = :QSPA-EXPD-EXCEPT,
              SPAFILE.SPA_EXPR_FRMLA = :QSPA-EXPR-FRMLA,
              SPAFILE.SPA_EXPR_REDU_FRMLA = :QSPA-EXPR-REDU-FRMLA,
              SPAFILE.SPA_EXPR_RATE_1 = :QSPA-EXPR-RATE-1,
              SPAFILE.SPA_EXPR_BREAK_1 = :QSPA-EXPR-BREAK-1,
              SPAFILE.SPA_EXPR_YRTYPE_1 = :QSPA-EXPR-YRTYPE-1,
              SPAFILE.SPA_EXPR_INTMETH_1 = :QSPA-EXPR-INTMETH-1,
              SPAFILE.SPA_EXPR_RATE_2 = :QSPA-EXPR-RATE-2,
              SPAFILE.SPA_EXPR_BREAK_2 = :QSPA-EXPR-BREAK-2,
              SPAFILE.SPA_EXPR_YRTYPE_2 = :QSPA-EXPR-YRTYPE-2,
              SPAFILE.SPA_EXPR_INTMETH_2 = :QSPA-EXPR-INTMETH-2,
              SPAFILE.SPA_EXPR_RATE_3 = :QSPA-EXPR-RATE-3,
              SPAFILE.SPA_EXPR_BREAK_3 = :QSPA-EXPR-BREAK-3,
              SPAFILE.SPA_EXPR_YRTYPE_3 = :QSPA-EXPR-YRTYPE-3,
              SPAFILE.SPA_EXPR_INTMETH_3 = :QSPA-EXPR-INTMETH-3,
              SPAFILE.SPA_EXPR_RATE_4 = :QSPA-EXPR-RATE-4,
              SPAFILE.SPA_EXPR_BREAK_4 = :QSPA-EXPR-BREAK-4,
              SPAFILE.SPA_EXPR_YRTYPE_4 = :QSPA-EXPR-YRTYPE-4,
              SPAFILE.SPA_EXPR_INTMETH_4 = :QSPA-EXPR-INTMETH-4,
              SPAFILE.SPA_EXPR_RATE_5 = :QSPA-EXPR-RATE-5,
              SPAFILE.SPA_EXPR_BREAK_5 = :QSPA-EXPR-BREAK-5,
              SPAFILE.SPA_EXPR_YRTYPE_5 = :QSPA-EXPR-YRTYPE-5,
              SPAFILE.SPA_EXPR_INTMETH_5 = :QSPA-EXPR-INTMETH-5,
              SPAFILE.SPA_EXPR_LCHG = :QSPA-EXPR-LCHG,
              SPAFILE.SPA_JUDGFRMLA = :QSPA-JUDGFRMLA,
              SPAFILE.SPA_JUDGRATE = :QSPA-JUDGRATE,
              SPAFILE.SPA_JUDGYRTYPE = :QSPA-JUDGYRTYPE,
              SPAFILE.SPA_JUDGINTMETH = :QSPA-JUDGINTMETH,
              SPAFILE.SPA_ACC_FRMLA = :QSPA-ACC-FRMLA,
              SPAFILE.SPA_ACC_YRTYPE = :QSPA-ACC-YRTYPE,
              SPAFILE.SPA_ACC_INTMETH = :QSPA-ACC-INTMETH,
              SPAFILE.SPA_ACC_RATE_1 = :QSPA-ACC-RATE-1,
              SPAFILE.SPA_ACC_BREAK_1 = :QSPA-ACC-BREAK-1,
              SPAFILE.SPA_ACC_RATE_2 = :QSPA-ACC-RATE-2,
              SPAFILE.SPA_ACC_BREAK_2 = :QSPA-ACC-BREAK-2,
              SPAFILE.SPA_ACC_RATE_3 = :QSPA-ACC-RATE-3,
              SPAFILE.SPA_ACC_BREAK_3 = :QSPA-ACC-BREAK-3,
              SPAFILE.SPA_ACC_RATE_4 = :QSPA-ACC-RATE-4,
              SPAFILE.SPA_ACC_BREAK_4 = :QSPA-ACC-BREAK-4,
              SPAFILE.SPA_ACC_RATE_5 = :QSPA-ACC-RATE-5,
              SPAFILE.SPA_ACC_BREAK_5 = :QSPA-ACC-BREAK-5,
              SPAFILE.SPA_1098FRMLA = :QSPA-1098FRMLA,
              SPAFILE.SPA_DISFRMLA = :QSPA-DISFRMLA,
              SPAFILE.SPA_CONTRFRMLA = :QSPA-CONTRFRMLA,
              SPAFILE.SPA_BANKRULE = :QSPA-BANKRULE,
              SPAFILE.SPA_CONTPDTH = :QSPA-CONTPDTH,
              SPAFILE.SPA_FINCHG = :QSPA-FINCHG,
              SPAFILE.SPA_BANKUNITPER_CD = :QSPA-BANKUNITPER-CD,
              SPAFILE.SPA_TRWBANKRULE = :QSPA-TRWBANKRULE,
              SPAFILE.SPA_TRWCONTPDTH = :QSPA-TRWCONTPDTH,
              SPAFILE.SPA_TRWBANKUNITPER_CD =
                   :QSPA-TRWBANKUNITPER-CD,
              SPAFILE.SPA_RECFRMLA = :QSPA-RECFRMLA,
              SPAFILE.SPA_RECPAYCD = :QSPA-RECPAYCD,
              SPAFILE.SPA_RECPDTH = :QSPA-RECPDTH,
              SPAFILE.SPA_DISFRMLA_SPAY_FG =
                   :QSPA-DISFRMLA-SPAY-FG,
              SPAFILE.SPA_ERNFRMLA_1 = :QSPA-ERNFRMLA-1,
              SPAFILE.SPA_ERNAMT_1 = :QSPA-ERNAMT-1,
              SPAFILE.SPA_ERNPERCT_1 = :QSPA-ERNPERCT-1,
              SPAFILE.SPA_ERNPERCD_1 = :QSPA-ERNPERCD-1,
              SPAFILE.SPA_ERNACCRL_FG_1 = :QSPA-ERNACCRL-FG-1,
              SPAFILE.SPA_ERNACCRL_START_CD_1 =
                   :QSPA-ERNACCRL-START-CD-1,
              SPAFILE.SPA_ERNACCRL_DFCD_1 = :QSPA-ERNACCRL-DFCD-1,
              SPAFILE.SPA_ERN_DAYS_1 = :QSPA-ERN-DAYS-1,
              SPAFILE.SPA_ERN_TERM_1 = :QSPA-ERN-TERM-1,
              SPAFILE.SPA_ERN_TYPE_1 = :QSPA-ERN-TYPE-1,
              SPAFILE.SPA_ERNFRMLA_2 = :QSPA-ERNFRMLA-2,
              SPAFILE.SPA_ERNAMT_2 = :QSPA-ERNAMT-2,
              SPAFILE.SPA_ERNPERCT_2 = :QSPA-ERNPERCT-2,
              SPAFILE.SPA_ERNPERCD_2 = :QSPA-ERNPERCD-2,
              SPAFILE.SPA_ERNACCRL_FG_2 = :QSPA-ERNACCRL-FG-2,
              SPAFILE.SPA_ERNACCRL_START_CD_2 =
                   :QSPA-ERNACCRL-START-CD-2,
              SPAFILE.SPA_ERNACCRL_DFCD_2 = :QSPA-ERNACCRL-DFCD-2,
              SPAFILE.SPA_ERN_DAYS_2 = :QSPA-ERN-DAYS-2,
              SPAFILE.SPA_ERN_TERM_2 = :QSPA-ERN-TERM-2,
              SPAFILE.SPA_ERN_TYPE_2 = :QSPA-ERN-TYPE-2,
              SPAFILE.SPA_ERNFRMLA_3 = :QSPA-ERNFRMLA-3,
              SPAFILE.SPA_ERNAMT_3 = :QSPA-ERNAMT-3,
              SPAFILE.SPA_ERNPERCT_3 = :QSPA-ERNPERCT-3,
              SPAFILE.SPA_ERNPERCD_3 = :QSPA-ERNPERCD-3,
              SPAFILE.SPA_ERNACCRL_FG_3 = :QSPA-ERNACCRL-FG-3,
              SPAFILE.SPA_ERNACCRL_START_CD_3 =
                   :QSPA-ERNACCRL-START-CD-3,
              SPAFILE.SPA_ERNACCRL_DFCD_3 = :QSPA-ERNACCRL-DFCD-3,
              SPAFILE.SPA_ERN_DAYS_3 = :QSPA-ERN-DAYS-3,
              SPAFILE.SPA_ERN_TERM_3 = :QSPA-ERN-TERM-3,
              SPAFILE.SPA_ERN_TYPE_3 = :QSPA-ERN-TYPE-3,
              SPAFILE.SPA_ERNFRMLA_4 = :QSPA-ERNFRMLA-4,
              SPAFILE.SPA_ERNAMT_4 = :QSPA-ERNAMT-4,
              SPAFILE.SPA_ERNPERCT_4 = :QSPA-ERNPERCT-4,
              SPAFILE.SPA_ERNPERCD_4 = :QSPA-ERNPERCD-4,
              SPAFILE.SPA_ERNACCRL_FG_4 = :QSPA-ERNACCRL-FG-4,
              SPAFILE.SPA_ERNACCRL_START_CD_4 =
                   :QSPA-ERNACCRL-START-CD-4,
              SPAFILE.SPA_ERNACCRL_DFCD_4 = :QSPA-ERNACCRL-DFCD-4,
              SPAFILE.SPA_ERN_DAYS_4 = :QSPA-ERN-DAYS-4,
              SPAFILE.SPA_ERN_TERM_4 = :QSPA-ERN-TERM-4,
              SPAFILE.SPA_ERN_TYPE_4 = :QSPA-ERN-TYPE-4,
              SPAFILE.SPA_ERNFRMLA_5 = :QSPA-ERNFRMLA-5,
              SPAFILE.SPA_ERNAMT_5 = :QSPA-ERNAMT-5,
              SPAFILE.SPA_ERNPERCT_5 = :QSPA-ERNPERCT-5,
              SPAFILE.SPA_ERNPERCD_5 = :QSPA-ERNPERCD-5,
              SPAFILE.SPA_ERNACCRL_FG_5 = :QSPA-ERNACCRL-FG-5,
              SPAFILE.SPA_ERNACCRL_START_CD_5 =
                   :QSPA-ERNACCRL-START-CD-5,
              SPAFILE.SPA_ERNACCRL_DFCD_5 = :QSPA-ERNACCRL-DFCD-5,
              SPAFILE.SPA_ERN_DAYS_5 = :QSPA-ERN-DAYS-5,
              SPAFILE.SPA_ERN_TERM_5 = :QSPA-ERN-TERM-5,
              SPAFILE.SPA_ERN_TYPE_5 = :QSPA-ERN-TYPE-5,
              SPAFILE.SPA_ERNFRMLA_6 = :QSPA-ERNFRMLA-6,
              SPAFILE.SPA_ERNAMT_6 = :QSPA-ERNAMT-6,
              SPAFILE.SPA_ERNPERCT_6 = :QSPA-ERNPERCT-6,
              SPAFILE.SPA_ERNPERCD_6 = :QSPA-ERNPERCD-6,
              SPAFILE.SPA_ERNACCRL_FG_6 = :QSPA-ERNACCRL-FG-6,
              SPAFILE.SPA_ERNACCRL_START_CD_6 =
                   :QSPA-ERNACCRL-START-CD-6,
              SPAFILE.SPA_ERNACCRL_DFCD_6 = :QSPA-ERNACCRL-DFCD-6,
              SPAFILE.SPA_ERN_DAYS_6 = :QSPA-ERN-DAYS-6,
              SPAFILE.SPA_ERN_TERM_6 = :QSPA-ERN-TERM-6,
              SPAFILE.SPA_ERN_TYPE_6 = :QSPA-ERN-TYPE-6,
              SPAFILE.SPA_ERNFRMLA_7 = :QSPA-ERNFRMLA-7,
              SPAFILE.SPA_ERNAMT_7 = :QSPA-ERNAMT-7,
              SPAFILE.SPA_ERNPERCT_7 = :QSPA-ERNPERCT-7,
              SPAFILE.SPA_ERNPERCD_7 = :QSPA-ERNPERCD-7,
              SPAFILE.SPA_ERNACCRL_FG_7 = :QSPA-ERNACCRL-FG-7,
              SPAFILE.SPA_ERNACCRL_START_CD_7 =
                   :QSPA-ERNACCRL-START-CD-7,
              SPAFILE.SPA_ERNACCRL_DFCD_7 = :QSPA-ERNACCRL-DFCD-7,
              SPAFILE.SPA_ERN_DAYS_7 = :QSPA-ERN-DAYS-7,
              SPAFILE.SPA_ERN_TERM_7 = :QSPA-ERN-TERM-7,
              SPAFILE.SPA_ERN_TYPE_7 = :QSPA-ERN-TYPE-7,
              SPAFILE.SPA_ERNFRMLA_8 = :QSPA-ERNFRMLA-8,
              SPAFILE.SPA_ERNAMT_8 = :QSPA-ERNAMT-8,
              SPAFILE.SPA_ERNPERCT_8 = :QSPA-ERNPERCT-8,
              SPAFILE.SPA_ERNPERCD_8 = :QSPA-ERNPERCD-8,
              SPAFILE.SPA_ERNACCRL_FG_8 = :QSPA-ERNACCRL-FG-8,
              SPAFILE.SPA_ERNACCRL_START_CD_8 =
                   :QSPA-ERNACCRL-START-CD-8,
              SPAFILE.SPA_ERNACCRL_DFCD_8 = :QSPA-ERNACCRL-DFCD-8,
              SPAFILE.SPA_ERN_DAYS_8 = :QSPA-ERN-DAYS-8,
              SPAFILE.SPA_ERN_TERM_8 = :QSPA-ERN-TERM-8,
              SPAFILE.SPA_ERN_TYPE_8 = :QSPA-ERN-TYPE-8,
              SPAFILE.SPA_ERNFRMLA_9 = :QSPA-ERNFRMLA-9,
              SPAFILE.SPA_ERNAMT_9 = :QSPA-ERNAMT-9,
              SPAFILE.SPA_ERNPERCT_9 = :QSPA-ERNPERCT-9,
              SPAFILE.SPA_ERNPERCD_9 = :QSPA-ERNPERCD-9,
              SPAFILE.SPA_ERNACCRL_FG_9 = :QSPA-ERNACCRL-FG-9,
              SPAFILE.SPA_ERNACCRL_START_CD_9 =
                   :QSPA-ERNACCRL-START-CD-9,
              SPAFILE.SPA_ERNACCRL_DFCD_9 = :QSPA-ERNACCRL-DFCD-9,
              SPAFILE.SPA_ERN_DAYS_9 = :QSPA-ERN-DAYS-9,
              SPAFILE.SPA_ERN_TERM_9 = :QSPA-ERN-TERM-9,
              SPAFILE.SPA_ERN_TYPE_9 = :QSPA-ERN-TYPE-9,
              SPAFILE.SPA_ERNFRMLA_10 = :QSPA-ERNFRMLA-10,
              SPAFILE.SPA_ERNAMT_10 = :QSPA-ERNAMT-10,
              SPAFILE.SPA_ERNPERCT_10 = :QSPA-ERNPERCT-10,
              SPAFILE.SPA_ERNPERCD_10 = :QSPA-ERNPERCD-10,
              SPAFILE.SPA_ERNACCRL_FG_10 = :QSPA-ERNACCRL-FG-10,
              SPAFILE.SPA_ERNACCRL_START_CD_10 =
                   :QSPA-ERNACCRL-START-CD-10,
              SPAFILE.SPA_ERNACCRL_DFCD_10 =
                   :QSPA-ERNACCRL-DFCD-10,
              SPAFILE.SPA_ERN_DAYS_10 = :QSPA-ERN-DAYS-10,
              SPAFILE.SPA_ERN_TERM_10 = :QSPA-ERN-TERM-10,
              SPAFILE.SPA_ERN_TYPE_10 = :QSPA-ERN-TYPE-10,
              SPAFILE.SPA_ERNACCEL_CD = :QSPA-ERNACCEL-CD,
              SPAFILE.SPA_ERNJD_CD = :QSPA-ERNJD-CD,
              SPAFILE.SPA_MIDSTRERNCD = :QSPA-MIDSTRERNCD,
              SPAFILE.SPA_EARN_ORDER_1 = :QSPA-EARN-ORDER-1,
              SPAFILE.SPA_EARN_ORDER_2 = :QSPA-EARN-ORDER-2,
              SPAFILE.SPA_EARN_ORDER_3 = :QSPA-EARN-ORDER-3,
              SPAFILE.SPA_EARN_ORDER_4 = :QSPA-EARN-ORDER-4,
              SPAFILE.SPA_EARN_ORDER_5 = :QSPA-EARN-ORDER-5,
              SPAFILE.SPA_EARN_ORDER_6 = :QSPA-EARN-ORDER-6,
              SPAFILE.SPA_EARN_ORDER_7 = :QSPA-EARN-ORDER-7,
              SPAFILE.SPA_EARN_ORDER_8 = :QSPA-EARN-ORDER-8,
              SPAFILE.SPA_EARN_ORDER_9 = :QSPA-EARN-ORDER-9,
              SPAFILE.SPA_EARN_ORDER_10 = :QSPA-EARN-ORDER-10,
              SPAFILE.SPA_ERNDELQFRMLA = :QSPA-ERNDELQFRMLA,
              SPAFILE.SPA_ERNDELQ_START = :QSPA-ERNDELQ-START,
              SPAFILE.SPA_ERNDELQ_STOP = :QSPA-ERNDELQ-STOP,
              SPAFILE.SPA_1098_INC_LC = :QSPA-1098-INC-LC,
              SPAFILE.SPA_MAX_IB_ACR_DAYS = :QSPA-MAX-IB-ACR-DAYS,
              SPAFILE.SPA_LCGRACE = :QSPA-LCGRACE,
              SPAFILE.SPA_LCGRACECD = :QSPA-LCGRACECD,
              SPAFILE.SPA_LCTYPE = :QSPA-LCTYPE,
              SPAFILE.SPA_LCRATE = :QSPA-LCRATE,
              SPAFILE.SPA_LCFRMLA = :QSPA-LCFRMLA,
              SPAFILE.SPA_LCMINCHG = :QSPA-LCMINCHG,
              SPAFILE.SPA_LCMAXAMT = :QSPA-LCMAXAMT,
              SPAFILE.SPA_LCDELFAC = :QSPA-LCDELFAC,
              SPAFILE.SPA_LCYRTYPE = :QSPA-LCYRTYPE,
              SPAFILE.SPA_LC_CODE = :QSPA-LC-CODE,
              SPAFILE.SPA_LCUNITPER_CD = :QSPA-LCUNITPER-CD,
              SPAFILE.SPA_DEFFRMLA = :QSPA-DEFFRMLA,
              SPAFILE.SPA_DEFEXCEPT = :QSPA-DEFEXCEPT,
              SPAFILE.SPA_DEFWORKER = :QSPA-DEFWORKER,
              SPAFILE.SPA_DEFPERCD = :QSPA-DEFPERCD,
              SPAFILE.SPA_DEFBASE = :QSPA-DEFBASE,
              SPAFILE.SPA_DEFPOLCD = :QSPA-DEFPOLCD,
              SPAFILE.SPA_DEFPOLMX_1 = :QSPA-DEFPOLMX-1,
              SPAFILE.SPA_DEFPOLMX_2 = :QSPA-DEFPOLMX-2,
              SPAFILE.SPA_DEFPOLMX_3 = :QSPA-DEFPOLMX-3,
              SPAFILE.SPA_DEFMIN = :QSPA-DEFMIN,
              SPAFILE.SPA_DEFMAX = :QSPA-DEFMAX,
              SPAFILE.SPA_DEFUNITPER_CD = :QSPA-DEFUNITPER-CD,
              SPAFILE.SPA_PRFORM_1 = :QSPA-PRFORM-1,
              SPAFILE.SPA_PRFLAG_1 = :QSPA-PRFLAG-1,
              SPAFILE.SPA_PRFORM_2 = :QSPA-PRFORM-2,
              SPAFILE.SPA_PRFLAG_2 = :QSPA-PRFLAG-2,
              SPAFILE.SPA_PRFORM_3 = :QSPA-PRFORM-3,
              SPAFILE.SPA_PRFLAG_3 = :QSPA-PRFLAG-3,
              SPAFILE.SPA_PRFORM_4 = :QSPA-PRFORM-4,
              SPAFILE.SPA_PRFLAG_4 = :QSPA-PRFLAG-4,
              SPAFILE.SPA_PRFORM_5 = :QSPA-PRFORM-5,
              SPAFILE.SPA_PRFLAG_5 = :QSPA-PRFLAG-5,
              SPAFILE.SPA_PRFORM_6 = :QSPA-PRFORM-6,
              SPAFILE.SPA_PRFLAG_6 = :QSPA-PRFLAG-6,
              SPAFILE.SPA_PRFORM_7 = :QSPA-PRFORM-7,
              SPAFILE.SPA_PRFLAG_7 = :QSPA-PRFLAG-7,
              SPAFILE.SPA_PRFORM_8 = :QSPA-PRFORM-8,
              SPAFILE.SPA_PRFLAG_8 = :QSPA-PRFLAG-8,
              SPAFILE.SPA_PRFORM_9 = :QSPA-PRFORM-9,
              SPAFILE.SPA_PRFLAG_9 = :QSPA-PRFLAG-9,
              SPAFILE.SPA_PRFORM_10 = :QSPA-PRFORM-10,
              SPAFILE.SPA_PRFLAG_10 = :QSPA-PRFLAG-10,
              SPAFILE.SPA_PRFORM_11 = :QSPA-PRFORM-11,
              SPAFILE.SPA_PRFLAG_11 = :QSPA-PRFLAG-11,
              SPAFILE.SPA_PRFORM_12 = :QSPA-PRFORM-12,
              SPAFILE.SPA_PRFLAG_12 = :QSPA-PRFLAG-12,
              SPAFILE.SPA_PRFORM_13 = :QSPA-PRFORM-13,
              SPAFILE.SPA_PRFLAG_13 = :QSPA-PRFLAG-13,
              SPAFILE.SPA_PRFORM_14 = :QSPA-PRFORM-14,
              SPAFILE.SPA_PRFLAG_14 = :QSPA-PRFLAG-14,
              SPAFILE.SPA_PRFORM_15 = :QSPA-PRFORM-15,
              SPAFILE.SPA_PRFLAG_15 = :QSPA-PRFLAG-15,
              SPAFILE.SPA_PRFORM_16 = :QSPA-PRFORM-16,
              SPAFILE.SPA_PRFLAG_16 = :QSPA-PRFLAG-16,
              SPAFILE.SPA_PRFORM_17 = :QSPA-PRFORM-17,
              SPAFILE.SPA_PRFLAG_17 = :QSPA-PRFLAG-17,
              SPAFILE.SPA_PRFORM_18 = :QSPA-PRFORM-18,
              SPAFILE.SPA_PRFLAG_18 = :QSPA-PRFLAG-18,
              SPAFILE.SPA_PRFORM_19 = :QSPA-PRFORM-19,
              SPAFILE.SPA_PRFLAG_19 = :QSPA-PRFLAG-19,
              SPAFILE.SPA_PRFORM_20 = :QSPA-PRFORM-20,
              SPAFILE.SPA_PRFLAG_20 = :QSPA-PRFLAG-20,
              SPAFILE.SPA_IMED_REC = :QSPA-IMED-REC,
              SPAFILE.SPA_SCFRMLA = :QSPA-SCFRMLA,
              SPAFILE.SPA_SCRATE = :QSPA-SCRATE,
              SPAFILE.SPA_SCMIN = :QSPA-SCMIN,
              SPAFILE.SPA_SCMAX = :QSPA-SCMAX,
              SPAFILE.SPA_MFFRMLA = :QSPA-MFFRMLA,
              SPAFILE.SPA_MFRATE = :QSPA-MFRATE,
              SPAFILE.SPA_MFMIN = :QSPA-MFMIN,
              SPAFILE.SPA_MFMAX = :QSPA-MFMAX,
              SPAFILE.SPA_INTMETHOD = :QSPA-INTMETHOD,
              SPAFILE.SPA_YEARTYPE = :QSPA-YEARTYPE,
              SPAFILE.SPA_PYEXFRMLA = :QSPA-PYEXFRMLA,
              SPAFILE.SPA_PYEXSPCODE = :QSPA-PYEXSPCODE,
              SPAFILE.SPA_PYEXYEARTP = :QSPA-PYEXYEARTP,
              SPAFILE.SPA_PYEXFMDATE = :QSPA-PYEXFMDATE,
              SPAFILE.SPA_PYEXBASECD = :QSPA-PYEXBASECD,
              SPAFILE.SPA_PYEXBASE = :QSPA-PYEXBASE,
              SPAFILE.SPA_PYEXDAYS = :QSPA-PYEXDAYS,
              SPAFILE.SPA_CAL_SPFRMLA = :QSPA-CAL-SPFRMLA,
              SPAFILE.SPA_CAL_RATETYPE_1 = :QSPA-CAL-RATETYPE-1,
              SPAFILE.SPA_CAL_BREAKTYPE_1 = :QSPA-CAL-BREAKTYPE-1,
              SPAFILE.SPA_CAL_CHGABLE_1 = :QSPA-CAL-CHGABLE-1,
              SPAFILE.SPA_CAL_NORATES_1 = :QSPA-CAL-NORATES-1,
              SPAFILE.SPA_CAL_TBFRMLA_1 = :QSPA-CAL-TBFRMLA-1,
              SPAFILE.SPA_CAL_RATE_1_1 = :QSPA-CAL-RATE-1-1,
              SPAFILE.SPA_CAL_BREAK_1_1 = :QSPA-CAL-BREAK-1-1,
              SPAFILE.SPA_CAL_RATE_1_2 = :QSPA-CAL-RATE-1-2,
              SPAFILE.SPA_CAL_BREAK_1_2 = :QSPA-CAL-BREAK-1-2,
              SPAFILE.SPA_CAL_RATE_1_3 = :QSPA-CAL-RATE-1-3,
              SPAFILE.SPA_CAL_BREAK_1_3 = :QSPA-CAL-BREAK-1-3,
              SPAFILE.SPA_CAL_RATE_1_4 = :QSPA-CAL-RATE-1-4,
              SPAFILE.SPA_CAL_BREAK_1_4 = :QSPA-CAL-BREAK-1-4,
              SPAFILE.SPA_CAL_RATE_1_5 = :QSPA-CAL-RATE-1-5,
              SPAFILE.SPA_CAL_BREAK_1_5 = :QSPA-CAL-BREAK-1-5,
              SPAFILE.SPA_CAL_RATETYPE_2 = :QSPA-CAL-RATETYPE-2,
              SPAFILE.SPA_CAL_BREAKTYPE_2 = :QSPA-CAL-BREAKTYPE-2,
              SPAFILE.SPA_CAL_CHGABLE_2 = :QSPA-CAL-CHGABLE-2,
              SPAFILE.SPA_CAL_NORATES_2 = :QSPA-CAL-NORATES-2,
              SPAFILE.SPA_CAL_TBFRMLA_2 = :QSPA-CAL-TBFRMLA-2,
              SPAFILE.SPA_CAL_RATE_2_1 = :QSPA-CAL-RATE-2-1,
              SPAFILE.SPA_CAL_BREAK_2_1 = :QSPA-CAL-BREAK-2-1,
              SPAFILE.SPA_CAL_RATE_2_2 = :QSPA-CAL-RATE-2-2,
              SPAFILE.SPA_CAL_BREAK_2_2 = :QSPA-CAL-BREAK-2-2,
              SPAFILE.SPA_CAL_RATE_2_3 = :QSPA-CAL-RATE-2-3,
              SPAFILE.SPA_CAL_BREAK_2_3 = :QSPA-CAL-BREAK-2-3,
              SPAFILE.SPA_CAL_RATE_2_4 = :QSPA-CAL-RATE-2-4,
              SPAFILE.SPA_CAL_BREAK_2_4 = :QSPA-CAL-BREAK-2-4,
              SPAFILE.SPA_CAL_RATE_2_5 = :QSPA-CAL-RATE-2-5,
              SPAFILE.SPA_CAL_BREAK_2_5 = :QSPA-CAL-BREAK-2-5,
              SPAFILE.SPA_MINSF_FRMLA = :QSPA-MINSF-FRMLA,
              SPAFILE.SPA_MINSF_CHG = :QSPA-MINSF-CHG,
              SPAFILE.SPA_MAXAPR_FRMLA = :QSPA-MAXAPR-FRMLA,
              SPAFILE.SPA_EDT_NOEDITS_1 = :QSPA-EDT-NOEDITS-1,
              SPAFILE.SPA_EDT_TYPE_1_1 = :QSPA-EDT-TYPE-1-1,
              SPAFILE.SPA_EDT_HL_1_1 = :QSPA-EDT-HL-1-1,
              SPAFILE.SPA_EDT_COLL_1_1 = :QSPA-EDT-COLL-1-1,
              SPAFILE.SPA_EDT_CD_1_1 = :QSPA-EDT-CD-1-1,
              SPAFILE.SPA_EDT_RATE_1_1 = :QSPA-EDT-RATE-1-1,
              SPAFILE.SPA_EDT_RATETYPE_1_1 =
                   :QSPA-EDT-RATETYPE-1-1,
              SPAFILE.SPA_EDT_AMT_1_1 = :QSPA-EDT-AMT-1-1,
              SPAFILE.SPA_EDT_AMTYPE_1_1 = :QSPA-EDT-AMTYPE-1-1,
              SPAFILE.SPA_EDT_TERM_1_1 = :QSPA-EDT-TERM-1-1,
              SPAFILE.SPA_EDT_TYPE_1_2 = :QSPA-EDT-TYPE-1-2,
              SPAFILE.SPA_EDT_HL_1_2 = :QSPA-EDT-HL-1-2,
              SPAFILE.SPA_EDT_COLL_1_2 = :QSPA-EDT-COLL-1-2,
              SPAFILE.SPA_EDT_CD_1_2 = :QSPA-EDT-CD-1-2,
              SPAFILE.SPA_EDT_RATE_1_2 = :QSPA-EDT-RATE-1-2,
              SPAFILE.SPA_EDT_RATETYPE_1_2 =
                   :QSPA-EDT-RATETYPE-1-2,
              SPAFILE.SPA_EDT_AMT_1_2 = :QSPA-EDT-AMT-1-2,
              SPAFILE.SPA_EDT_AMTYPE_1_2 = :QSPA-EDT-AMTYPE-1-2,
              SPAFILE.SPA_EDT_TERM_1_2 = :QSPA-EDT-TERM-1-2,
              SPAFILE.SPA_EDT_TYPE_1_3 = :QSPA-EDT-TYPE-1-3,
              SPAFILE.SPA_EDT_HL_1_3 = :QSPA-EDT-HL-1-3,
              SPAFILE.SPA_EDT_COLL_1_3 = :QSPA-EDT-COLL-1-3,
              SPAFILE.SPA_EDT_CD_1_3 = :QSPA-EDT-CD-1-3,
              SPAFILE.SPA_EDT_RATE_1_3 = :QSPA-EDT-RATE-1-3,
              SPAFILE.SPA_EDT_RATETYPE_1_3 =
                   :QSPA-EDT-RATETYPE-1-3,
              SPAFILE.SPA_EDT_AMT_1_3 = :QSPA-EDT-AMT-1-3,
              SPAFILE.SPA_EDT_AMTYPE_1_3 = :QSPA-EDT-AMTYPE-1-3,
              SPAFILE.SPA_EDT_TERM_1_3 = :QSPA-EDT-TERM-1-3,
              SPAFILE.SPA_EDT_TYPE_1_4 = :QSPA-EDT-TYPE-1-4,
              SPAFILE.SPA_EDT_HL_1_4 = :QSPA-EDT-HL-1-4,
              SPAFILE.SPA_EDT_COLL_1_4 = :QSPA-EDT-COLL-1-4,
              SPAFILE.SPA_EDT_CD_1_4 = :QSPA-EDT-CD-1-4,
              SPAFILE.SPA_EDT_RATE_1_4 = :QSPA-EDT-RATE-1-4,
              SPAFILE.SPA_EDT_RATETYPE_1_4 =
                   :QSPA-EDT-RATETYPE-1-4,
              SPAFILE.SPA_EDT_AMT_1_4 = :QSPA-EDT-AMT-1-4,
              SPAFILE.SPA_EDT_AMTYPE_1_4 = :QSPA-EDT-AMTYPE-1-4,
              SPAFILE.SPA_EDT_TERM_1_4 = :QSPA-EDT-TERM-1-4,
              SPAFILE.SPA_EDT_TYPE_1_5 = :QSPA-EDT-TYPE-1-5,
              SPAFILE.SPA_EDT_HL_1_5 = :QSPA-EDT-HL-1-5,
              SPAFILE.SPA_EDT_COLL_1_5 = :QSPA-EDT-COLL-1-5,
              SPAFILE.SPA_EDT_CD_1_5 = :QSPA-EDT-CD-1-5,
              SPAFILE.SPA_EDT_RATE_1_5 = :QSPA-EDT-RATE-1-5,
              SPAFILE.SPA_EDT_RATETYPE_1_5 =
                   :QSPA-EDT-RATETYPE-1-5,
              SPAFILE.SPA_EDT_AMT_1_5 = :QSPA-EDT-AMT-1-5,
              SPAFILE.SPA_EDT_AMTYPE_1_5 = :QSPA-EDT-AMTYPE-1-5,
              SPAFILE.SPA_EDT_TERM_1_5 = :QSPA-EDT-TERM-1-5,
              SPAFILE.SPA_EDT_TYPE_1_6 = :QSPA-EDT-TYPE-1-6,
              SPAFILE.SPA_EDT_HL_1_6 = :QSPA-EDT-HL-1-6,
              SPAFILE.SPA_EDT_COLL_1_6 = :QSPA-EDT-COLL-1-6,
              SPAFILE.SPA_EDT_CD_1_6 = :QSPA-EDT-CD-1-6,
              SPAFILE.SPA_EDT_RATE_1_6 = :QSPA-EDT-RATE-1-6,
              SPAFILE.SPA_EDT_RATETYPE_1_6 =
                   :QSPA-EDT-RATETYPE-1-6,
              SPAFILE.SPA_EDT_AMT_1_6 = :QSPA-EDT-AMT-1-6,
              SPAFILE.SPA_EDT_AMTYPE_1_6 = :QSPA-EDT-AMTYPE-1-6,
              SPAFILE.SPA_EDT_TERM_1_6 = :QSPA-EDT-TERM-1-6,
              SPAFILE.SPA_EDT_NOEDITS_2 = :QSPA-EDT-NOEDITS-2,
              SPAFILE.SPA_EDT_TYPE_2_1 = :QSPA-EDT-TYPE-2-1,
              SPAFILE.SPA_EDT_HL_2_1 = :QSPA-EDT-HL-2-1,
              SPAFILE.SPA_EDT_COLL_2_1 = :QSPA-EDT-COLL-2-1,
              SPAFILE.SPA_EDT_CD_2_1 = :QSPA-EDT-CD-2-1,
              SPAFILE.SPA_EDT_RATE_2_1 = :QSPA-EDT-RATE-2-1,
              SPAFILE.SPA_EDT_RATETYPE_2_1 =
                   :QSPA-EDT-RATETYPE-2-1,
              SPAFILE.SPA_EDT_AMT_2_1 = :QSPA-EDT-AMT-2-1,
              SPAFILE.SPA_EDT_AMTYPE_2_1 = :QSPA-EDT-AMTYPE-2-1,
              SPAFILE.SPA_EDT_TERM_2_1 = :QSPA-EDT-TERM-2-1,
              SPAFILE.SPA_EDT_TYPE_2_2 = :QSPA-EDT-TYPE-2-2,
              SPAFILE.SPA_EDT_HL_2_2 = :QSPA-EDT-HL-2-2,
              SPAFILE.SPA_EDT_COLL_2_2 = :QSPA-EDT-COLL-2-2,
              SPAFILE.SPA_EDT_CD_2_2 = :QSPA-EDT-CD-2-2,
              SPAFILE.SPA_EDT_RATE_2_2 = :QSPA-EDT-RATE-2-2,
              SPAFILE.SPA_EDT_RATETYPE_2_2 =
                   :QSPA-EDT-RATETYPE-2-2,
              SPAFILE.SPA_EDT_AMT_2_2 = :QSPA-EDT-AMT-2-2,
              SPAFILE.SPA_EDT_AMTYPE_2_2 = :QSPA-EDT-AMTYPE-2-2,
              SPAFILE.SPA_EDT_TERM_2_2 = :QSPA-EDT-TERM-2-2,
              SPAFILE.SPA_EDT_TYPE_2_3 = :QSPA-EDT-TYPE-2-3,
              SPAFILE.SPA_EDT_HL_2_3 = :QSPA-EDT-HL-2-3,
              SPAFILE.SPA_EDT_COLL_2_3 = :QSPA-EDT-COLL-2-3,
              SPAFILE.SPA_EDT_CD_2_3 = :QSPA-EDT-CD-2-3,
              SPAFILE.SPA_EDT_RATE_2_3 = :QSPA-EDT-RATE-2-3,
              SPAFILE.SPA_EDT_RATETYPE_2_3 =
                   :QSPA-EDT-RATETYPE-2-3,
              SPAFILE.SPA_EDT_AMT_2_3 = :QSPA-EDT-AMT-2-3,
              SPAFILE.SPA_EDT_AMTYPE_2_3 = :QSPA-EDT-AMTYPE-2-3,
              SPAFILE.SPA_EDT_TERM_2_3 = :QSPA-EDT-TERM-2-3,
              SPAFILE.SPA_EDT_TYPE_2_4 = :QSPA-EDT-TYPE-2-4,
              SPAFILE.SPA_EDT_HL_2_4 = :QSPA-EDT-HL-2-4,
              SPAFILE.SPA_EDT_COLL_2_4 = :QSPA-EDT-COLL-2-4,
              SPAFILE.SPA_EDT_CD_2_4 = :QSPA-EDT-CD-2-4,
              SPAFILE.SPA_EDT_RATE_2_4 = :QSPA-EDT-RATE-2-4,
              SPAFILE.SPA_EDT_RATETYPE_2_4 =
                   :QSPA-EDT-RATETYPE-2-4,
              SPAFILE.SPA_EDT_AMT_2_4 = :QSPA-EDT-AMT-2-4,
              SPAFILE.SPA_EDT_AMTYPE_2_4 = :QSPA-EDT-AMTYPE-2-4,
              SPAFILE.SPA_EDT_TERM_2_4 = :QSPA-EDT-TERM-2-4,
              SPAFILE.SPA_EDT_TYPE_2_5 = :QSPA-EDT-TYPE-2-5,
              SPAFILE.SPA_EDT_HL_2_5 = :QSPA-EDT-HL-2-5,
              SPAFILE.SPA_EDT_COLL_2_5 = :QSPA-EDT-COLL-2-5,
              SPAFILE.SPA_EDT_CD_2_5 = :QSPA-EDT-CD-2-5,
              SPAFILE.SPA_EDT_RATE_2_5 = :QSPA-EDT-RATE-2-5,
              SPAFILE.SPA_EDT_RATETYPE_2_5 =
                   :QSPA-EDT-RATETYPE-2-5,
              SPAFILE.SPA_EDT_AMT_2_5 = :QSPA-EDT-AMT-2-5,
              SPAFILE.SPA_EDT_AMTYPE_2_5 = :QSPA-EDT-AMTYPE-2-5,
              SPAFILE.SPA_EDT_TERM_2_5 = :QSPA-EDT-TERM-2-5,
              SPAFILE.SPA_EDT_TYPE_2_6 = :QSPA-EDT-TYPE-2-6,
              SPAFILE.SPA_EDT_HL_2_6 = :QSPA-EDT-HL-2-6,
              SPAFILE.SPA_EDT_COLL_2_6 = :QSPA-EDT-COLL-2-6,
              SPAFILE.SPA_EDT_CD_2_6 = :QSPA-EDT-CD-2-6,
              SPAFILE.SPA_EDT_RATE_2_6 = :QSPA-EDT-RATE-2-6,
              SPAFILE.SPA_EDT_RATETYPE_2_6 =
                   :QSPA-EDT-RATETYPE-2-6,
              SPAFILE.SPA_EDT_AMT_2_6 = :QSPA-EDT-AMT-2-6,
              SPAFILE.SPA_EDT_AMTYPE_2_6 = :QSPA-EDT-AMTYPE-2-6,
              SPAFILE.SPA_EDT_TERM_2_6 = :QSPA-EDT-TERM-2-6
            WHERE SPAFILE.SPA_ORGST = :QSPA-ORGST
              AND SPAFILE.SPA_SPRCLASS = :QSPA-SPRCLASS
              AND SPAFILE.SPA_SUBCLASS = :QSPA-SUBCLASS
              AND SPAFILE.SPA_LAWCODE = :QSPA-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-SPA1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-SPA1-FILE.

           PERFORM DELETE-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
           MOVE SPA1-KEY       TO E-KEYX.
           PERFORM SET-SPA-FIELDS.

           EXEC SQL
            DELETE FROM DBO.SPAFILE
            WHERE SPAFILE.SPA_ORGST = :QSPA-ORGST
              AND SPAFILE.SPA_SPRCLASS = :QSPA-SPRCLASS
              AND SPAFILE.SPA_SUBCLASS = :QSPA-SUBCLASS
              AND SPAFILE.SPA_LAWCODE = :QSPA-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-SPA1-FILE.

      *-----------------------------------------------------------------
       CLOSE-SPA1-FILE.
           IF ( SPA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SPA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SPA1-CURSOR-STAT.

           IF ( SPA1-NG-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SPA1_NG_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SPA1-NG-CURSOR-STAT.


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
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*G2G  *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-SPBFILE NOT = "S" )
*G2G          MOVE SPB-PATH-BEG          TO SPB-SELECT-PATH-BEG
*G2G          MOVE SPB-PATH-FILE         TO SPB-SELECT-PATH-FILE
*G2G       ELSE
*G2G          MOVE SPB-PATH-SQL          TO SPB-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-SPB1-FILE.
           PERFORM LOAD-SPB1-FILE.
           PERFORM OPEN-IT.
           MOVE SPB-SELECT-PATH TO E-FILE.

      *-----------------------------------------------------------------
       READ-SPB1-FILE.
           PERFORM READ-IT.
           MOVE SPB-SELECT-PATH TO E-FILE.
           MOVE SPB1-KEY TO E-KEYX.

           MOVE SPB-ORGST            TO QSPB-ORGST.
           MOVE SPB-SPRCLASS         TO QSPB-SPRCLASS.
           MOVE SPB-SUBCLASS         TO QSPB-SUBCLASS.
           MOVE SPB-LAWCODE          TO QSPB-LAWCODE.

           EXEC SQL
            SELECT ISNULL(SPBFILE.SPB_ORGST,''),
                   SPBFILE.SPB_SPRCLASS,
                   SPBFILE.SPB_SUBCLASS,
                   SPBFILE.SPB_LAWCODE,
                   SPBFILE.SPB_INSCOM_1,
                   SPBFILE.SPB_INSRATECD_1,
                   SPBFILE.SPB_INSJFAC_1,
                   SPBFILE.SPB_INSDISCNT_1,
                   ISNULL(SPBFILE.SPB_INSCHGABL_1,''),
                   SPBFILE.SPB_INSTERM_1_1,
                   SPBFILE.SPB_INSRATE_1_1,
                   SPBFILE.SPB_INSTERM_1_2,
                   SPBFILE.SPB_INSRATE_1_2,
                   SPBFILE.SPB_INSTERM_1_3,
                   SPBFILE.SPB_INSRATE_1_3,
                   SPBFILE.SPB_INSTERM_1_4,
                   SPBFILE.SPB_INSRATE_1_4,
                   SPBFILE.SPB_INSTERM_1_5,
                   SPBFILE.SPB_INSRATE_1_5,
                   SPBFILE.SPB_INSTERM_1_6,
                   SPBFILE.SPB_INSRATE_1_6,
                   SPBFILE.SPB_INSTERM_1_7,
                   SPBFILE.SPB_INSRATE_1_7,
                   SPBFILE.SPB_INSTERM_1_8,
                   SPBFILE.SPB_INSRATE_1_8,
                   SPBFILE.SPB_INSTERM_1_9,
                   SPBFILE.SPB_INSRATE_1_9,
                   SPBFILE.SPB_INSTERM_1_10,
                   SPBFILE.SPB_INSRATE_1_10,
                   SPBFILE.SPB_INSTERM_1_11,
                   SPBFILE.SPB_INSRATE_1_11,
                   SPBFILE.SPB_INSTERM_1_12,
                   SPBFILE.SPB_INSRATE_1_12,
                   SPBFILE.SPB_INSTERM_1_13,
                   SPBFILE.SPB_INSRATE_1_13,
                   SPBFILE.SPB_INSTERM_1_14,
                   SPBFILE.SPB_INSRATE_1_14,
                   SPBFILE.SPB_INSTERM_1_15,
                   SPBFILE.SPB_INSRATE_1_15,
                   SPBFILE.SPB_INSTERM_1_16,
                   SPBFILE.SPB_INSRATE_1_16,
                   SPBFILE.SPB_INSTERM_1_17,
                   SPBFILE.SPB_INSRATE_1_17,
                   SPBFILE.SPB_INSTERM_1_18,
                   SPBFILE.SPB_INSRATE_1_18,
                   SPBFILE.SPB_INSTERM_1_19,
                   SPBFILE.SPB_INSRATE_1_19,
                   SPBFILE.SPB_INSTERM_1_20,
                   SPBFILE.SPB_INSRATE_1_20,
                   SPBFILE.SPB_INSMINPREM_1,
                   SPBFILE.SPB_INSMINCHGABL_1,
                   SPBFILE.SPB_INSMAXCHGABL_1,
                   SPBFILE.SPB_INSMAXBENIF_1,
                   SPBFILE.SPB_INSMAXTERM_1,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_1,''),
                   SPBFILE.SPB_INSEXCEPT_1,
                   SPBFILE.SPB_INSNOVER_1,
                   SPBFILE.SPB_INSMINAGE_1_1,
                   SPBFILE.SPB_INSMAXAGE_1_1,
                   SPBFILE.SPB_INSMAXCOV_1_1,
                   SPBFILE.SPB_INSMAXTRM_1_1,
                   SPBFILE.SPB_INSMINAGE_1_2,
                   SPBFILE.SPB_INSMAXAGE_1_2,
                   SPBFILE.SPB_INSMAXCOV_1_2,
                   SPBFILE.SPB_INSMAXTRM_1_2,
                   SPBFILE.SPB_INSMINAGE_1_3,
                   SPBFILE.SPB_INSMAXAGE_1_3,
                   SPBFILE.SPB_INSMAXCOV_1_3,
                   SPBFILE.SPB_INSMAXTRM_1_3,
                   SPBFILE.SPB_INSMINAGE_1_4,
                   SPBFILE.SPB_INSMAXAGE_1_4,
                   SPBFILE.SPB_INSMAXCOV_1_4,
                   SPBFILE.SPB_INSMAXTRM_1_4,
                   SPBFILE.SPB_INSMINAGE_1_5,
                   SPBFILE.SPB_INSMAXAGE_1_5,
                   SPBFILE.SPB_INSMAXCOV_1_5,
                   SPBFILE.SPB_INSMAXTRM_1_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_1,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_1,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_1,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_1,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_1,''),
                   SPBFILE.SPB_INSCOM_2,
                   SPBFILE.SPB_INSRATECD_2,
                   SPBFILE.SPB_INSJFAC_2,
                   SPBFILE.SPB_INSDISCNT_2,
                   ISNULL(SPBFILE.SPB_INSCHGABL_2,''),
                   SPBFILE.SPB_INSTERM_2_1,
                   SPBFILE.SPB_INSRATE_2_1,
                   SPBFILE.SPB_INSTERM_2_2,
                   SPBFILE.SPB_INSRATE_2_2,
                   SPBFILE.SPB_INSTERM_2_3,
                   SPBFILE.SPB_INSRATE_2_3,
                   SPBFILE.SPB_INSTERM_2_4,
                   SPBFILE.SPB_INSRATE_2_4,
                   SPBFILE.SPB_INSTERM_2_5,
                   SPBFILE.SPB_INSRATE_2_5,
                   SPBFILE.SPB_INSTERM_2_6,
                   SPBFILE.SPB_INSRATE_2_6,
                   SPBFILE.SPB_INSTERM_2_7,
                   SPBFILE.SPB_INSRATE_2_7,
                   SPBFILE.SPB_INSTERM_2_8,
                   SPBFILE.SPB_INSRATE_2_8,
                   SPBFILE.SPB_INSTERM_2_9,
                   SPBFILE.SPB_INSRATE_2_9,
                   SPBFILE.SPB_INSTERM_2_10,
                   SPBFILE.SPB_INSRATE_2_10,
                   SPBFILE.SPB_INSTERM_2_11,
                   SPBFILE.SPB_INSRATE_2_11,
                   SPBFILE.SPB_INSTERM_2_12,
                   SPBFILE.SPB_INSRATE_2_12,
                   SPBFILE.SPB_INSTERM_2_13,
                   SPBFILE.SPB_INSRATE_2_13,
                   SPBFILE.SPB_INSTERM_2_14,
                   SPBFILE.SPB_INSRATE_2_14,
                   SPBFILE.SPB_INSTERM_2_15,
                   SPBFILE.SPB_INSRATE_2_15,
                   SPBFILE.SPB_INSTERM_2_16,
                   SPBFILE.SPB_INSRATE_2_16,
                   SPBFILE.SPB_INSTERM_2_17,
                   SPBFILE.SPB_INSRATE_2_17,
                   SPBFILE.SPB_INSTERM_2_18,
                   SPBFILE.SPB_INSRATE_2_18,
                   SPBFILE.SPB_INSTERM_2_19,
                   SPBFILE.SPB_INSRATE_2_19,
                   SPBFILE.SPB_INSTERM_2_20,
                   SPBFILE.SPB_INSRATE_2_20,
                   SPBFILE.SPB_INSMINPREM_2,
                   SPBFILE.SPB_INSMINCHGABL_2,
                   SPBFILE.SPB_INSMAXCHGABL_2,
                   SPBFILE.SPB_INSMAXBENIF_2,
                   SPBFILE.SPB_INSMAXTERM_2,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_2,''),
                   SPBFILE.SPB_INSEXCEPT_2,
                   SPBFILE.SPB_INSNOVER_2,
                   SPBFILE.SPB_INSMINAGE_2_1,
                   SPBFILE.SPB_INSMAXAGE_2_1,
                   SPBFILE.SPB_INSMAXCOV_2_1,
                   SPBFILE.SPB_INSMAXTRM_2_1,
                   SPBFILE.SPB_INSMINAGE_2_2,
                   SPBFILE.SPB_INSMAXAGE_2_2,
                   SPBFILE.SPB_INSMAXCOV_2_2,
                   SPBFILE.SPB_INSMAXTRM_2_2,
                   SPBFILE.SPB_INSMINAGE_2_3,
                   SPBFILE.SPB_INSMAXAGE_2_3,
                   SPBFILE.SPB_INSMAXCOV_2_3,
                   SPBFILE.SPB_INSMAXTRM_2_3,
                   SPBFILE.SPB_INSMINAGE_2_4,
                   SPBFILE.SPB_INSMAXAGE_2_4,
                   SPBFILE.SPB_INSMAXCOV_2_4,
                   SPBFILE.SPB_INSMAXTRM_2_4,
                   SPBFILE.SPB_INSMINAGE_2_5,
                   SPBFILE.SPB_INSMAXAGE_2_5,
                   SPBFILE.SPB_INSMAXCOV_2_5,
                   SPBFILE.SPB_INSMAXTRM_2_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_2,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_2,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_2,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_2,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_2,''),
                   SPBFILE.SPB_INSCOM_3,
                   SPBFILE.SPB_INSRATECD_3,
                   SPBFILE.SPB_INSJFAC_3,
                   SPBFILE.SPB_INSDISCNT_3,
                   ISNULL(SPBFILE.SPB_INSCHGABL_3,''),
                   SPBFILE.SPB_INSTERM_3_1,
                   SPBFILE.SPB_INSRATE_3_1,
                   SPBFILE.SPB_INSTERM_3_2,
                   SPBFILE.SPB_INSRATE_3_2,
                   SPBFILE.SPB_INSTERM_3_3,
                   SPBFILE.SPB_INSRATE_3_3,
                   SPBFILE.SPB_INSTERM_3_4,
                   SPBFILE.SPB_INSRATE_3_4,
                   SPBFILE.SPB_INSTERM_3_5,
                   SPBFILE.SPB_INSRATE_3_5,
                   SPBFILE.SPB_INSTERM_3_6,
                   SPBFILE.SPB_INSRATE_3_6,
                   SPBFILE.SPB_INSTERM_3_7,
                   SPBFILE.SPB_INSRATE_3_7,
                   SPBFILE.SPB_INSTERM_3_8,
                   SPBFILE.SPB_INSRATE_3_8,
                   SPBFILE.SPB_INSTERM_3_9,
                   SPBFILE.SPB_INSRATE_3_9,
                   SPBFILE.SPB_INSTERM_3_10,
                   SPBFILE.SPB_INSRATE_3_10,
                   SPBFILE.SPB_INSTERM_3_11,
                   SPBFILE.SPB_INSRATE_3_11,
                   SPBFILE.SPB_INSTERM_3_12,
                   SPBFILE.SPB_INSRATE_3_12,
                   SPBFILE.SPB_INSTERM_3_13,
                   SPBFILE.SPB_INSRATE_3_13,
                   SPBFILE.SPB_INSTERM_3_14,
                   SPBFILE.SPB_INSRATE_3_14,
                   SPBFILE.SPB_INSTERM_3_15,
                   SPBFILE.SPB_INSRATE_3_15,
                   SPBFILE.SPB_INSTERM_3_16,
                   SPBFILE.SPB_INSRATE_3_16,
                   SPBFILE.SPB_INSTERM_3_17,
                   SPBFILE.SPB_INSRATE_3_17,
                   SPBFILE.SPB_INSTERM_3_18,
                   SPBFILE.SPB_INSRATE_3_18,
                   SPBFILE.SPB_INSTERM_3_19,
                   SPBFILE.SPB_INSRATE_3_19,
                   SPBFILE.SPB_INSTERM_3_20,
                   SPBFILE.SPB_INSRATE_3_20,
                   SPBFILE.SPB_INSMINPREM_3,
                   SPBFILE.SPB_INSMINCHGABL_3,
                   SPBFILE.SPB_INSMAXCHGABL_3,
                   SPBFILE.SPB_INSMAXBENIF_3,
                   SPBFILE.SPB_INSMAXTERM_3,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_3,''),
                   SPBFILE.SPB_INSEXCEPT_3,
                   SPBFILE.SPB_INSNOVER_3,
                   SPBFILE.SPB_INSMINAGE_3_1,
                   SPBFILE.SPB_INSMAXAGE_3_1,
                   SPBFILE.SPB_INSMAXCOV_3_1,
                   SPBFILE.SPB_INSMAXTRM_3_1,
                   SPBFILE.SPB_INSMINAGE_3_2,
                   SPBFILE.SPB_INSMAXAGE_3_2,
                   SPBFILE.SPB_INSMAXCOV_3_2,
                   SPBFILE.SPB_INSMAXTRM_3_2,
                   SPBFILE.SPB_INSMINAGE_3_3,
                   SPBFILE.SPB_INSMAXAGE_3_3,
                   SPBFILE.SPB_INSMAXCOV_3_3,
                   SPBFILE.SPB_INSMAXTRM_3_3,
                   SPBFILE.SPB_INSMINAGE_3_4,
                   SPBFILE.SPB_INSMAXAGE_3_4,
                   SPBFILE.SPB_INSMAXCOV_3_4,
                   SPBFILE.SPB_INSMAXTRM_3_4,
                   SPBFILE.SPB_INSMINAGE_3_5,
                   SPBFILE.SPB_INSMAXAGE_3_5,
                   SPBFILE.SPB_INSMAXCOV_3_5,
                   SPBFILE.SPB_INSMAXTRM_3_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_3,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_3,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_3,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_3,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_3,''),
                   SPBFILE.SPB_INSCOM_4,
                   SPBFILE.SPB_INSRATECD_4,
                   SPBFILE.SPB_INSJFAC_4,
                   SPBFILE.SPB_INSDISCNT_4,
                   ISNULL(SPBFILE.SPB_INSCHGABL_4,''),
                   SPBFILE.SPB_INSTERM_4_1,
                   SPBFILE.SPB_INSRATE_4_1,
                   SPBFILE.SPB_INSTERM_4_2,
                   SPBFILE.SPB_INSRATE_4_2,
                   SPBFILE.SPB_INSTERM_4_3,
                   SPBFILE.SPB_INSRATE_4_3,
                   SPBFILE.SPB_INSTERM_4_4,
                   SPBFILE.SPB_INSRATE_4_4,
                   SPBFILE.SPB_INSTERM_4_5,
                   SPBFILE.SPB_INSRATE_4_5,
                   SPBFILE.SPB_INSTERM_4_6,
                   SPBFILE.SPB_INSRATE_4_6,
                   SPBFILE.SPB_INSTERM_4_7,
                   SPBFILE.SPB_INSRATE_4_7,
                   SPBFILE.SPB_INSTERM_4_8,
                   SPBFILE.SPB_INSRATE_4_8,
                   SPBFILE.SPB_INSTERM_4_9,
                   SPBFILE.SPB_INSRATE_4_9,
                   SPBFILE.SPB_INSTERM_4_10,
                   SPBFILE.SPB_INSRATE_4_10,
                   SPBFILE.SPB_INSTERM_4_11,
                   SPBFILE.SPB_INSRATE_4_11,
                   SPBFILE.SPB_INSTERM_4_12,
                   SPBFILE.SPB_INSRATE_4_12,
                   SPBFILE.SPB_INSTERM_4_13,
                   SPBFILE.SPB_INSRATE_4_13,
                   SPBFILE.SPB_INSTERM_4_14,
                   SPBFILE.SPB_INSRATE_4_14,
                   SPBFILE.SPB_INSTERM_4_15,
                   SPBFILE.SPB_INSRATE_4_15,
                   SPBFILE.SPB_INSTERM_4_16,
                   SPBFILE.SPB_INSRATE_4_16,
                   SPBFILE.SPB_INSTERM_4_17,
                   SPBFILE.SPB_INSRATE_4_17,
                   SPBFILE.SPB_INSTERM_4_18,
                   SPBFILE.SPB_INSRATE_4_18,
                   SPBFILE.SPB_INSTERM_4_19,
                   SPBFILE.SPB_INSRATE_4_19,
                   SPBFILE.SPB_INSTERM_4_20,
                   SPBFILE.SPB_INSRATE_4_20,
                   SPBFILE.SPB_INSMINPREM_4,
                   SPBFILE.SPB_INSMINCHGABL_4,
                   SPBFILE.SPB_INSMAXCHGABL_4,
                   SPBFILE.SPB_INSMAXBENIF_4,
                   SPBFILE.SPB_INSMAXTERM_4,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_4,''),
                   SPBFILE.SPB_INSEXCEPT_4,
                   SPBFILE.SPB_INSNOVER_4,
                   SPBFILE.SPB_INSMINAGE_4_1,
                   SPBFILE.SPB_INSMAXAGE_4_1,
                   SPBFILE.SPB_INSMAXCOV_4_1,
                   SPBFILE.SPB_INSMAXTRM_4_1,
                   SPBFILE.SPB_INSMINAGE_4_2,
                   SPBFILE.SPB_INSMAXAGE_4_2,
                   SPBFILE.SPB_INSMAXCOV_4_2,
                   SPBFILE.SPB_INSMAXTRM_4_2,
                   SPBFILE.SPB_INSMINAGE_4_3,
                   SPBFILE.SPB_INSMAXAGE_4_3,
                   SPBFILE.SPB_INSMAXCOV_4_3,
                   SPBFILE.SPB_INSMAXTRM_4_3,
                   SPBFILE.SPB_INSMINAGE_4_4,
                   SPBFILE.SPB_INSMAXAGE_4_4,
                   SPBFILE.SPB_INSMAXCOV_4_4,
                   SPBFILE.SPB_INSMAXTRM_4_4,
                   SPBFILE.SPB_INSMINAGE_4_5,
                   SPBFILE.SPB_INSMAXAGE_4_5,
                   SPBFILE.SPB_INSMAXCOV_4_5,
                   SPBFILE.SPB_INSMAXTRM_4_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_4,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_4,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_4,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_4,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_4,''),
                   SPBFILE.SPB_INSCOM_5,
                   SPBFILE.SPB_INSRATECD_5,
                   SPBFILE.SPB_INSJFAC_5,
                   SPBFILE.SPB_INSDISCNT_5,
                   ISNULL(SPBFILE.SPB_INSCHGABL_5,''),
                   SPBFILE.SPB_INSTERM_5_1,
                   SPBFILE.SPB_INSRATE_5_1,
                   SPBFILE.SPB_INSTERM_5_2,
                   SPBFILE.SPB_INSRATE_5_2,
                   SPBFILE.SPB_INSTERM_5_3,
                   SPBFILE.SPB_INSRATE_5_3,
                   SPBFILE.SPB_INSTERM_5_4,
                   SPBFILE.SPB_INSRATE_5_4,
                   SPBFILE.SPB_INSTERM_5_5,
                   SPBFILE.SPB_INSRATE_5_5,
                   SPBFILE.SPB_INSTERM_5_6,
                   SPBFILE.SPB_INSRATE_5_6,
                   SPBFILE.SPB_INSTERM_5_7,
                   SPBFILE.SPB_INSRATE_5_7,
                   SPBFILE.SPB_INSTERM_5_8,
                   SPBFILE.SPB_INSRATE_5_8,
                   SPBFILE.SPB_INSTERM_5_9,
                   SPBFILE.SPB_INSRATE_5_9,
                   SPBFILE.SPB_INSTERM_5_10,
                   SPBFILE.SPB_INSRATE_5_10,
                   SPBFILE.SPB_INSTERM_5_11,
                   SPBFILE.SPB_INSRATE_5_11,
                   SPBFILE.SPB_INSTERM_5_12,
                   SPBFILE.SPB_INSRATE_5_12,
                   SPBFILE.SPB_INSTERM_5_13,
                   SPBFILE.SPB_INSRATE_5_13,
                   SPBFILE.SPB_INSTERM_5_14,
                   SPBFILE.SPB_INSRATE_5_14,
                   SPBFILE.SPB_INSTERM_5_15,
                   SPBFILE.SPB_INSRATE_5_15,
                   SPBFILE.SPB_INSTERM_5_16,
                   SPBFILE.SPB_INSRATE_5_16,
                   SPBFILE.SPB_INSTERM_5_17,
                   SPBFILE.SPB_INSRATE_5_17,
                   SPBFILE.SPB_INSTERM_5_18,
                   SPBFILE.SPB_INSRATE_5_18,
                   SPBFILE.SPB_INSTERM_5_19,
                   SPBFILE.SPB_INSRATE_5_19,
                   SPBFILE.SPB_INSTERM_5_20,
                   SPBFILE.SPB_INSRATE_5_20,
                   SPBFILE.SPB_INSMINPREM_5,
                   SPBFILE.SPB_INSMINCHGABL_5,
                   SPBFILE.SPB_INSMAXCHGABL_5,
                   SPBFILE.SPB_INSMAXBENIF_5,
                   SPBFILE.SPB_INSMAXTERM_5,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_5,''),
                   SPBFILE.SPB_INSEXCEPT_5,
                   SPBFILE.SPB_INSNOVER_5,
                   SPBFILE.SPB_INSMINAGE_5_1,
                   SPBFILE.SPB_INSMAXAGE_5_1,
                   SPBFILE.SPB_INSMAXCOV_5_1,
                   SPBFILE.SPB_INSMAXTRM_5_1,
                   SPBFILE.SPB_INSMINAGE_5_2,
                   SPBFILE.SPB_INSMAXAGE_5_2,
                   SPBFILE.SPB_INSMAXCOV_5_2,
                   SPBFILE.SPB_INSMAXTRM_5_2,
                   SPBFILE.SPB_INSMINAGE_5_3,
                   SPBFILE.SPB_INSMAXAGE_5_3,
                   SPBFILE.SPB_INSMAXCOV_5_3,
                   SPBFILE.SPB_INSMAXTRM_5_3,
                   SPBFILE.SPB_INSMINAGE_5_4,
                   SPBFILE.SPB_INSMAXAGE_5_4,
                   SPBFILE.SPB_INSMAXCOV_5_4,
                   SPBFILE.SPB_INSMAXTRM_5_4,
                   SPBFILE.SPB_INSMINAGE_5_5,
                   SPBFILE.SPB_INSMAXAGE_5_5,
                   SPBFILE.SPB_INSMAXCOV_5_5,
                   SPBFILE.SPB_INSMAXTRM_5_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_5,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_5,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_5,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_5,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_5,''),
                   SPBFILE.SPB_INSCOM_6,
                   SPBFILE.SPB_INSRATECD_6,
                   SPBFILE.SPB_INSJFAC_6,
                   SPBFILE.SPB_INSDISCNT_6,
                   ISNULL(SPBFILE.SPB_INSCHGABL_6,''),
                   SPBFILE.SPB_INSTERM_6_1,
                   SPBFILE.SPB_INSRATE_6_1,
                   SPBFILE.SPB_INSTERM_6_2,
                   SPBFILE.SPB_INSRATE_6_2,
                   SPBFILE.SPB_INSTERM_6_3,
                   SPBFILE.SPB_INSRATE_6_3,
                   SPBFILE.SPB_INSTERM_6_4,
                   SPBFILE.SPB_INSRATE_6_4,
                   SPBFILE.SPB_INSTERM_6_5,
                   SPBFILE.SPB_INSRATE_6_5,
                   SPBFILE.SPB_INSTERM_6_6,
                   SPBFILE.SPB_INSRATE_6_6,
                   SPBFILE.SPB_INSTERM_6_7,
                   SPBFILE.SPB_INSRATE_6_7,
                   SPBFILE.SPB_INSTERM_6_8,
                   SPBFILE.SPB_INSRATE_6_8,
                   SPBFILE.SPB_INSTERM_6_9,
                   SPBFILE.SPB_INSRATE_6_9,
                   SPBFILE.SPB_INSTERM_6_10,
                   SPBFILE.SPB_INSRATE_6_10,
                   SPBFILE.SPB_INSTERM_6_11,
                   SPBFILE.SPB_INSRATE_6_11,
                   SPBFILE.SPB_INSTERM_6_12,
                   SPBFILE.SPB_INSRATE_6_12,
                   SPBFILE.SPB_INSTERM_6_13,
                   SPBFILE.SPB_INSRATE_6_13,
                   SPBFILE.SPB_INSTERM_6_14,
                   SPBFILE.SPB_INSRATE_6_14,
                   SPBFILE.SPB_INSTERM_6_15,
                   SPBFILE.SPB_INSRATE_6_15,
                   SPBFILE.SPB_INSTERM_6_16,
                   SPBFILE.SPB_INSRATE_6_16,
                   SPBFILE.SPB_INSTERM_6_17,
                   SPBFILE.SPB_INSRATE_6_17,
                   SPBFILE.SPB_INSTERM_6_18,
                   SPBFILE.SPB_INSRATE_6_18,
                   SPBFILE.SPB_INSTERM_6_19,
                   SPBFILE.SPB_INSRATE_6_19,
                   SPBFILE.SPB_INSTERM_6_20,
                   SPBFILE.SPB_INSRATE_6_20,
                   SPBFILE.SPB_INSMINPREM_6,
                   SPBFILE.SPB_INSMINCHGABL_6,
                   SPBFILE.SPB_INSMAXCHGABL_6,
                   SPBFILE.SPB_INSMAXBENIF_6,
                   SPBFILE.SPB_INSMAXTERM_6,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_6,''),
                   SPBFILE.SPB_INSEXCEPT_6,
                   SPBFILE.SPB_INSNOVER_6,
                   SPBFILE.SPB_INSMINAGE_6_1,
                   SPBFILE.SPB_INSMAXAGE_6_1,
                   SPBFILE.SPB_INSMAXCOV_6_1,
                   SPBFILE.SPB_INSMAXTRM_6_1,
                   SPBFILE.SPB_INSMINAGE_6_2,
                   SPBFILE.SPB_INSMAXAGE_6_2,
                   SPBFILE.SPB_INSMAXCOV_6_2,
                   SPBFILE.SPB_INSMAXTRM_6_2,
                   SPBFILE.SPB_INSMINAGE_6_3,
                   SPBFILE.SPB_INSMAXAGE_6_3,
                   SPBFILE.SPB_INSMAXCOV_6_3,
                   SPBFILE.SPB_INSMAXTRM_6_3,
                   SPBFILE.SPB_INSMINAGE_6_4,
                   SPBFILE.SPB_INSMAXAGE_6_4,
                   SPBFILE.SPB_INSMAXCOV_6_4,
                   SPBFILE.SPB_INSMAXTRM_6_4,
                   SPBFILE.SPB_INSMINAGE_6_5,
                   SPBFILE.SPB_INSMAXAGE_6_5,
                   SPBFILE.SPB_INSMAXCOV_6_5,
                   SPBFILE.SPB_INSMAXTRM_6_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_6,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_6,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_6,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_6,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_6,''),
                   SPBFILE.SPB_INSCOM_7,
                   SPBFILE.SPB_INSRATECD_7,
                   SPBFILE.SPB_INSJFAC_7,
                   SPBFILE.SPB_INSDISCNT_7,
                   ISNULL(SPBFILE.SPB_INSCHGABL_7,''),
                   SPBFILE.SPB_INSTERM_7_1,
                   SPBFILE.SPB_INSRATE_7_1,
                   SPBFILE.SPB_INSTERM_7_2,
                   SPBFILE.SPB_INSRATE_7_2,
                   SPBFILE.SPB_INSTERM_7_3,
                   SPBFILE.SPB_INSRATE_7_3,
                   SPBFILE.SPB_INSTERM_7_4,
                   SPBFILE.SPB_INSRATE_7_4,
                   SPBFILE.SPB_INSTERM_7_5,
                   SPBFILE.SPB_INSRATE_7_5,
                   SPBFILE.SPB_INSTERM_7_6,
                   SPBFILE.SPB_INSRATE_7_6,
                   SPBFILE.SPB_INSTERM_7_7,
                   SPBFILE.SPB_INSRATE_7_7,
                   SPBFILE.SPB_INSTERM_7_8,
                   SPBFILE.SPB_INSRATE_7_8,
                   SPBFILE.SPB_INSTERM_7_9,
                   SPBFILE.SPB_INSRATE_7_9,
                   SPBFILE.SPB_INSTERM_7_10,
                   SPBFILE.SPB_INSRATE_7_10,
                   SPBFILE.SPB_INSTERM_7_11,
                   SPBFILE.SPB_INSRATE_7_11,
                   SPBFILE.SPB_INSTERM_7_12,
                   SPBFILE.SPB_INSRATE_7_12,
                   SPBFILE.SPB_INSTERM_7_13,
                   SPBFILE.SPB_INSRATE_7_13,
                   SPBFILE.SPB_INSTERM_7_14,
                   SPBFILE.SPB_INSRATE_7_14,
                   SPBFILE.SPB_INSTERM_7_15,
                   SPBFILE.SPB_INSRATE_7_15,
                   SPBFILE.SPB_INSTERM_7_16,
                   SPBFILE.SPB_INSRATE_7_16,
                   SPBFILE.SPB_INSTERM_7_17,
                   SPBFILE.SPB_INSRATE_7_17,
                   SPBFILE.SPB_INSTERM_7_18,
                   SPBFILE.SPB_INSRATE_7_18,
                   SPBFILE.SPB_INSTERM_7_19,
                   SPBFILE.SPB_INSRATE_7_19,
                   SPBFILE.SPB_INSTERM_7_20,
                   SPBFILE.SPB_INSRATE_7_20,
                   SPBFILE.SPB_INSMINPREM_7,
                   SPBFILE.SPB_INSMINCHGABL_7,
                   SPBFILE.SPB_INSMAXCHGABL_7,
                   SPBFILE.SPB_INSMAXBENIF_7,
                   SPBFILE.SPB_INSMAXTERM_7,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_7,''),
                   SPBFILE.SPB_INSEXCEPT_7,
                   SPBFILE.SPB_INSNOVER_7,
                   SPBFILE.SPB_INSMINAGE_7_1,
                   SPBFILE.SPB_INSMAXAGE_7_1,
                   SPBFILE.SPB_INSMAXCOV_7_1,
                   SPBFILE.SPB_INSMAXTRM_7_1,
                   SPBFILE.SPB_INSMINAGE_7_2,
                   SPBFILE.SPB_INSMAXAGE_7_2,
                   SPBFILE.SPB_INSMAXCOV_7_2,
                   SPBFILE.SPB_INSMAXTRM_7_2,
                   SPBFILE.SPB_INSMINAGE_7_3,
                   SPBFILE.SPB_INSMAXAGE_7_3,
                   SPBFILE.SPB_INSMAXCOV_7_3,
                   SPBFILE.SPB_INSMAXTRM_7_3,
                   SPBFILE.SPB_INSMINAGE_7_4,
                   SPBFILE.SPB_INSMAXAGE_7_4,
                   SPBFILE.SPB_INSMAXCOV_7_4,
                   SPBFILE.SPB_INSMAXTRM_7_4,
                   SPBFILE.SPB_INSMINAGE_7_5,
                   SPBFILE.SPB_INSMAXAGE_7_5,
                   SPBFILE.SPB_INSMAXCOV_7_5,
                   SPBFILE.SPB_INSMAXTRM_7_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_7,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_7,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_7,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_7,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_7,''),
                   SPBFILE.SPB_INSCOM_8,
                   SPBFILE.SPB_INSRATECD_8,
                   SPBFILE.SPB_INSJFAC_8,
                   SPBFILE.SPB_INSDISCNT_8,
                   ISNULL(SPBFILE.SPB_INSCHGABL_8,''),
                   SPBFILE.SPB_INSTERM_8_1,
                   SPBFILE.SPB_INSRATE_8_1,
                   SPBFILE.SPB_INSTERM_8_2,
                   SPBFILE.SPB_INSRATE_8_2,
                   SPBFILE.SPB_INSTERM_8_3,
                   SPBFILE.SPB_INSRATE_8_3,
                   SPBFILE.SPB_INSTERM_8_4,
                   SPBFILE.SPB_INSRATE_8_4,
                   SPBFILE.SPB_INSTERM_8_5,
                   SPBFILE.SPB_INSRATE_8_5,
                   SPBFILE.SPB_INSTERM_8_6,
                   SPBFILE.SPB_INSRATE_8_6,
                   SPBFILE.SPB_INSTERM_8_7,
                   SPBFILE.SPB_INSRATE_8_7,
                   SPBFILE.SPB_INSTERM_8_8,
                   SPBFILE.SPB_INSRATE_8_8,
                   SPBFILE.SPB_INSTERM_8_9,
                   SPBFILE.SPB_INSRATE_8_9,
                   SPBFILE.SPB_INSTERM_8_10,
                   SPBFILE.SPB_INSRATE_8_10,
                   SPBFILE.SPB_INSTERM_8_11,
                   SPBFILE.SPB_INSRATE_8_11,
                   SPBFILE.SPB_INSTERM_8_12,
                   SPBFILE.SPB_INSRATE_8_12,
                   SPBFILE.SPB_INSTERM_8_13,
                   SPBFILE.SPB_INSRATE_8_13,
                   SPBFILE.SPB_INSTERM_8_14,
                   SPBFILE.SPB_INSRATE_8_14,
                   SPBFILE.SPB_INSTERM_8_15,
                   SPBFILE.SPB_INSRATE_8_15,
                   SPBFILE.SPB_INSTERM_8_16,
                   SPBFILE.SPB_INSRATE_8_16,
                   SPBFILE.SPB_INSTERM_8_17,
                   SPBFILE.SPB_INSRATE_8_17,
                   SPBFILE.SPB_INSTERM_8_18,
                   SPBFILE.SPB_INSRATE_8_18,
                   SPBFILE.SPB_INSTERM_8_19,
                   SPBFILE.SPB_INSRATE_8_19,
                   SPBFILE.SPB_INSTERM_8_20,
                   SPBFILE.SPB_INSRATE_8_20,
                   SPBFILE.SPB_INSMINPREM_8,
                   SPBFILE.SPB_INSMINCHGABL_8,
                   SPBFILE.SPB_INSMAXCHGABL_8,
                   SPBFILE.SPB_INSMAXBENIF_8,
                   SPBFILE.SPB_INSMAXTERM_8,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_8,''),
                   SPBFILE.SPB_INSEXCEPT_8,
                   SPBFILE.SPB_INSNOVER_8,
                   SPBFILE.SPB_INSMINAGE_8_1,
                   SPBFILE.SPB_INSMAXAGE_8_1,
                   SPBFILE.SPB_INSMAXCOV_8_1,
                   SPBFILE.SPB_INSMAXTRM_8_1,
                   SPBFILE.SPB_INSMINAGE_8_2,
                   SPBFILE.SPB_INSMAXAGE_8_2,
                   SPBFILE.SPB_INSMAXCOV_8_2,
                   SPBFILE.SPB_INSMAXTRM_8_2,
                   SPBFILE.SPB_INSMINAGE_8_3,
                   SPBFILE.SPB_INSMAXAGE_8_3,
                   SPBFILE.SPB_INSMAXCOV_8_3,
                   SPBFILE.SPB_INSMAXTRM_8_3,
                   SPBFILE.SPB_INSMINAGE_8_4,
                   SPBFILE.SPB_INSMAXAGE_8_4,
                   SPBFILE.SPB_INSMAXCOV_8_4,
                   SPBFILE.SPB_INSMAXTRM_8_4,
                   SPBFILE.SPB_INSMINAGE_8_5,
                   SPBFILE.SPB_INSMAXAGE_8_5,
                   SPBFILE.SPB_INSMAXCOV_8_5,
                   SPBFILE.SPB_INSMAXTRM_8_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_8,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_8,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_8,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_8,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_8,''),
                   SPBFILE.SPB_INSCOM_9,
                   SPBFILE.SPB_INSRATECD_9,
                   SPBFILE.SPB_INSJFAC_9,
                   SPBFILE.SPB_INSDISCNT_9,
                   ISNULL(SPBFILE.SPB_INSCHGABL_9,''),
                   SPBFILE.SPB_INSTERM_9_1,
                   SPBFILE.SPB_INSRATE_9_1,
                   SPBFILE.SPB_INSTERM_9_2,
                   SPBFILE.SPB_INSRATE_9_2,
                   SPBFILE.SPB_INSTERM_9_3,
                   SPBFILE.SPB_INSRATE_9_3,
                   SPBFILE.SPB_INSTERM_9_4,
                   SPBFILE.SPB_INSRATE_9_4,
                   SPBFILE.SPB_INSTERM_9_5,
                   SPBFILE.SPB_INSRATE_9_5,
                   SPBFILE.SPB_INSTERM_9_6,
                   SPBFILE.SPB_INSRATE_9_6,
                   SPBFILE.SPB_INSTERM_9_7,
                   SPBFILE.SPB_INSRATE_9_7,
                   SPBFILE.SPB_INSTERM_9_8,
                   SPBFILE.SPB_INSRATE_9_8,
                   SPBFILE.SPB_INSTERM_9_9,
                   SPBFILE.SPB_INSRATE_9_9,
                   SPBFILE.SPB_INSTERM_9_10,
                   SPBFILE.SPB_INSRATE_9_10,
                   SPBFILE.SPB_INSTERM_9_11,
                   SPBFILE.SPB_INSRATE_9_11,
                   SPBFILE.SPB_INSTERM_9_12,
                   SPBFILE.SPB_INSRATE_9_12,
                   SPBFILE.SPB_INSTERM_9_13,
                   SPBFILE.SPB_INSRATE_9_13,
                   SPBFILE.SPB_INSTERM_9_14,
                   SPBFILE.SPB_INSRATE_9_14,
                   SPBFILE.SPB_INSTERM_9_15,
                   SPBFILE.SPB_INSRATE_9_15,
                   SPBFILE.SPB_INSTERM_9_16,
                   SPBFILE.SPB_INSRATE_9_16,
                   SPBFILE.SPB_INSTERM_9_17,
                   SPBFILE.SPB_INSRATE_9_17,
                   SPBFILE.SPB_INSTERM_9_18,
                   SPBFILE.SPB_INSRATE_9_18,
                   SPBFILE.SPB_INSTERM_9_19,
                   SPBFILE.SPB_INSRATE_9_19,
                   SPBFILE.SPB_INSTERM_9_20,
                   SPBFILE.SPB_INSRATE_9_20,
                   SPBFILE.SPB_INSMINPREM_9,
                   SPBFILE.SPB_INSMINCHGABL_9,
                   SPBFILE.SPB_INSMAXCHGABL_9,
                   SPBFILE.SPB_INSMAXBENIF_9,
                   SPBFILE.SPB_INSMAXTERM_9,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_9,''),
                   SPBFILE.SPB_INSEXCEPT_9,
                   SPBFILE.SPB_INSNOVER_9,
                   SPBFILE.SPB_INSMINAGE_9_1,
                   SPBFILE.SPB_INSMAXAGE_9_1,
                   SPBFILE.SPB_INSMAXCOV_9_1,
                   SPBFILE.SPB_INSMAXTRM_9_1,
                   SPBFILE.SPB_INSMINAGE_9_2,
                   SPBFILE.SPB_INSMAXAGE_9_2,
                   SPBFILE.SPB_INSMAXCOV_9_2,
                   SPBFILE.SPB_INSMAXTRM_9_2,
                   SPBFILE.SPB_INSMINAGE_9_3,
                   SPBFILE.SPB_INSMAXAGE_9_3,
                   SPBFILE.SPB_INSMAXCOV_9_3,
                   SPBFILE.SPB_INSMAXTRM_9_3,
                   SPBFILE.SPB_INSMINAGE_9_4,
                   SPBFILE.SPB_INSMAXAGE_9_4,
                   SPBFILE.SPB_INSMAXCOV_9_4,
                   SPBFILE.SPB_INSMAXTRM_9_4,
                   SPBFILE.SPB_INSMINAGE_9_5,
                   SPBFILE.SPB_INSMAXAGE_9_5,
                   SPBFILE.SPB_INSMAXCOV_9_5,
                   SPBFILE.SPB_INSMAXTRM_9_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_9,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_9,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_9,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_9,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_9,''),
                   SPBFILE.SPB_INSCOM_10,
                   SPBFILE.SPB_INSRATECD_10,
                   SPBFILE.SPB_INSJFAC_10,
                   SPBFILE.SPB_INSDISCNT_10,
                   ISNULL(SPBFILE.SPB_INSCHGABL_10,''),
                   SPBFILE.SPB_INSTERM_10_1,
                   SPBFILE.SPB_INSRATE_10_1,
                   SPBFILE.SPB_INSTERM_10_2,
                   SPBFILE.SPB_INSRATE_10_2,
                   SPBFILE.SPB_INSTERM_10_3,
                   SPBFILE.SPB_INSRATE_10_3,
                   SPBFILE.SPB_INSTERM_10_4,
                   SPBFILE.SPB_INSRATE_10_4,
                   SPBFILE.SPB_INSTERM_10_5,
                   SPBFILE.SPB_INSRATE_10_5,
                   SPBFILE.SPB_INSTERM_10_6,
                   SPBFILE.SPB_INSRATE_10_6,
                   SPBFILE.SPB_INSTERM_10_7,
                   SPBFILE.SPB_INSRATE_10_7,
                   SPBFILE.SPB_INSTERM_10_8,
                   SPBFILE.SPB_INSRATE_10_8,
                   SPBFILE.SPB_INSTERM_10_9,
                   SPBFILE.SPB_INSRATE_10_9,
                   SPBFILE.SPB_INSTERM_10_10,
                   SPBFILE.SPB_INSRATE_10_10,
                   SPBFILE.SPB_INSTERM_10_11,
                   SPBFILE.SPB_INSRATE_10_11,
                   SPBFILE.SPB_INSTERM_10_12,
                   SPBFILE.SPB_INSRATE_10_12,
                   SPBFILE.SPB_INSTERM_10_13,
                   SPBFILE.SPB_INSRATE_10_13,
                   SPBFILE.SPB_INSTERM_10_14,
                   SPBFILE.SPB_INSRATE_10_14,
                   SPBFILE.SPB_INSTERM_10_15,
                   SPBFILE.SPB_INSRATE_10_15,
                   SPBFILE.SPB_INSTERM_10_16,
                   SPBFILE.SPB_INSRATE_10_16,
                   SPBFILE.SPB_INSTERM_10_17,
                   SPBFILE.SPB_INSRATE_10_17,
                   SPBFILE.SPB_INSTERM_10_18,
                   SPBFILE.SPB_INSRATE_10_18,
                   SPBFILE.SPB_INSTERM_10_19,
                   SPBFILE.SPB_INSRATE_10_19,
                   SPBFILE.SPB_INSTERM_10_20,
                   SPBFILE.SPB_INSRATE_10_20,
                   SPBFILE.SPB_INSMINPREM_10,
                   SPBFILE.SPB_INSMINCHGABL_10,
                   SPBFILE.SPB_INSMAXCHGABL_10,
                   SPBFILE.SPB_INSMAXBENIF_10,
                   SPBFILE.SPB_INSMAXTERM_10,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_10,''),
                   SPBFILE.SPB_INSEXCEPT_10,
                   SPBFILE.SPB_INSNOVER_10,
                   SPBFILE.SPB_INSMINAGE_10_1,
                   SPBFILE.SPB_INSMAXAGE_10_1,
                   SPBFILE.SPB_INSMAXCOV_10_1,
                   SPBFILE.SPB_INSMAXTRM_10_1,
                   SPBFILE.SPB_INSMINAGE_10_2,
                   SPBFILE.SPB_INSMAXAGE_10_2,
                   SPBFILE.SPB_INSMAXCOV_10_2,
                   SPBFILE.SPB_INSMAXTRM_10_2,
                   SPBFILE.SPB_INSMINAGE_10_3,
                   SPBFILE.SPB_INSMAXAGE_10_3,
                   SPBFILE.SPB_INSMAXCOV_10_3,
                   SPBFILE.SPB_INSMAXTRM_10_3,
                   SPBFILE.SPB_INSMINAGE_10_4,
                   SPBFILE.SPB_INSMAXAGE_10_4,
                   SPBFILE.SPB_INSMAXCOV_10_4,
                   SPBFILE.SPB_INSMAXTRM_10_4,
                   SPBFILE.SPB_INSMINAGE_10_5,
                   SPBFILE.SPB_INSMAXAGE_10_5,
                   SPBFILE.SPB_INSMAXCOV_10_5,
                   SPBFILE.SPB_INSMAXTRM_10_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_10,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_10,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_10,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_10,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_10,''),
                   SPBFILE.SPB_INSCOM_11,
                   SPBFILE.SPB_INSRATECD_11,
                   SPBFILE.SPB_INSJFAC_11,
                   SPBFILE.SPB_INSDISCNT_11,
                   ISNULL(SPBFILE.SPB_INSCHGABL_11,''),
                   SPBFILE.SPB_INSTERM_11_1,
                   SPBFILE.SPB_INSRATE_11_1,
                   SPBFILE.SPB_INSTERM_11_2,
                   SPBFILE.SPB_INSRATE_11_2,
                   SPBFILE.SPB_INSTERM_11_3,
                   SPBFILE.SPB_INSRATE_11_3,
                   SPBFILE.SPB_INSTERM_11_4,
                   SPBFILE.SPB_INSRATE_11_4,
                   SPBFILE.SPB_INSTERM_11_5,
                   SPBFILE.SPB_INSRATE_11_5,
                   SPBFILE.SPB_INSTERM_11_6,
                   SPBFILE.SPB_INSRATE_11_6,
                   SPBFILE.SPB_INSTERM_11_7,
                   SPBFILE.SPB_INSRATE_11_7,
                   SPBFILE.SPB_INSTERM_11_8,
                   SPBFILE.SPB_INSRATE_11_8,
                   SPBFILE.SPB_INSTERM_11_9,
                   SPBFILE.SPB_INSRATE_11_9,
                   SPBFILE.SPB_INSTERM_11_10,
                   SPBFILE.SPB_INSRATE_11_10,
                   SPBFILE.SPB_INSTERM_11_11,
                   SPBFILE.SPB_INSRATE_11_11,
                   SPBFILE.SPB_INSTERM_11_12,
                   SPBFILE.SPB_INSRATE_11_12,
                   SPBFILE.SPB_INSTERM_11_13,
                   SPBFILE.SPB_INSRATE_11_13,
                   SPBFILE.SPB_INSTERM_11_14,
                   SPBFILE.SPB_INSRATE_11_14,
                   SPBFILE.SPB_INSTERM_11_15,
                   SPBFILE.SPB_INSRATE_11_15,
                   SPBFILE.SPB_INSTERM_11_16,
                   SPBFILE.SPB_INSRATE_11_16,
                   SPBFILE.SPB_INSTERM_11_17,
                   SPBFILE.SPB_INSRATE_11_17,
                   SPBFILE.SPB_INSTERM_11_18,
                   SPBFILE.SPB_INSRATE_11_18,
                   SPBFILE.SPB_INSTERM_11_19,
                   SPBFILE.SPB_INSRATE_11_19,
                   SPBFILE.SPB_INSTERM_11_20,
                   SPBFILE.SPB_INSRATE_11_20,
                   SPBFILE.SPB_INSMINPREM_11,
                   SPBFILE.SPB_INSMINCHGABL_11,
                   SPBFILE.SPB_INSMAXCHGABL_11,
                   SPBFILE.SPB_INSMAXBENIF_11,
                   SPBFILE.SPB_INSMAXTERM_11,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_11,''),
                   SPBFILE.SPB_INSEXCEPT_11,
                   SPBFILE.SPB_INSNOVER_11,
                   SPBFILE.SPB_INSMINAGE_11_1,
                   SPBFILE.SPB_INSMAXAGE_11_1,
                   SPBFILE.SPB_INSMAXCOV_11_1,
                   SPBFILE.SPB_INSMAXTRM_11_1,
                   SPBFILE.SPB_INSMINAGE_11_2,
                   SPBFILE.SPB_INSMAXAGE_11_2,
                   SPBFILE.SPB_INSMAXCOV_11_2,
                   SPBFILE.SPB_INSMAXTRM_11_2,
                   SPBFILE.SPB_INSMINAGE_11_3,
                   SPBFILE.SPB_INSMAXAGE_11_3,
                   SPBFILE.SPB_INSMAXCOV_11_3,
                   SPBFILE.SPB_INSMAXTRM_11_3,
                   SPBFILE.SPB_INSMINAGE_11_4,
                   SPBFILE.SPB_INSMAXAGE_11_4,
                   SPBFILE.SPB_INSMAXCOV_11_4,
                   SPBFILE.SPB_INSMAXTRM_11_4,
                   SPBFILE.SPB_INSMINAGE_11_5,
                   SPBFILE.SPB_INSMAXAGE_11_5,
                   SPBFILE.SPB_INSMAXCOV_11_5,
                   SPBFILE.SPB_INSMAXTRM_11_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_11,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_11,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_11,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_11,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_11,''),
                   SPBFILE.SPB_INSCOM_12,
                   SPBFILE.SPB_INSRATECD_12,
                   SPBFILE.SPB_INSJFAC_12,
                   SPBFILE.SPB_INSDISCNT_12,
                   ISNULL(SPBFILE.SPB_INSCHGABL_12,''),
                   SPBFILE.SPB_INSTERM_12_1,
                   SPBFILE.SPB_INSRATE_12_1,
                   SPBFILE.SPB_INSTERM_12_2,
                   SPBFILE.SPB_INSRATE_12_2,
                   SPBFILE.SPB_INSTERM_12_3,
                   SPBFILE.SPB_INSRATE_12_3,
                   SPBFILE.SPB_INSTERM_12_4,
                   SPBFILE.SPB_INSRATE_12_4,
                   SPBFILE.SPB_INSTERM_12_5,
                   SPBFILE.SPB_INSRATE_12_5,
                   SPBFILE.SPB_INSTERM_12_6,
                   SPBFILE.SPB_INSRATE_12_6,
                   SPBFILE.SPB_INSTERM_12_7,
                   SPBFILE.SPB_INSRATE_12_7,
                   SPBFILE.SPB_INSTERM_12_8,
                   SPBFILE.SPB_INSRATE_12_8,
                   SPBFILE.SPB_INSTERM_12_9,
                   SPBFILE.SPB_INSRATE_12_9,
                   SPBFILE.SPB_INSTERM_12_10,
                   SPBFILE.SPB_INSRATE_12_10,
                   SPBFILE.SPB_INSTERM_12_11,
                   SPBFILE.SPB_INSRATE_12_11,
                   SPBFILE.SPB_INSTERM_12_12,
                   SPBFILE.SPB_INSRATE_12_12,
                   SPBFILE.SPB_INSTERM_12_13,
                   SPBFILE.SPB_INSRATE_12_13,
                   SPBFILE.SPB_INSTERM_12_14,
                   SPBFILE.SPB_INSRATE_12_14,
                   SPBFILE.SPB_INSTERM_12_15,
                   SPBFILE.SPB_INSRATE_12_15,
                   SPBFILE.SPB_INSTERM_12_16,
                   SPBFILE.SPB_INSRATE_12_16,
                   SPBFILE.SPB_INSTERM_12_17,
                   SPBFILE.SPB_INSRATE_12_17,
                   SPBFILE.SPB_INSTERM_12_18,
                   SPBFILE.SPB_INSRATE_12_18,
                   SPBFILE.SPB_INSTERM_12_19,
                   SPBFILE.SPB_INSRATE_12_19,
                   SPBFILE.SPB_INSTERM_12_20,
                   SPBFILE.SPB_INSRATE_12_20,
                   SPBFILE.SPB_INSMINPREM_12,
                   SPBFILE.SPB_INSMINCHGABL_12,
                   SPBFILE.SPB_INSMAXCHGABL_12,
                   SPBFILE.SPB_INSMAXBENIF_12,
                   SPBFILE.SPB_INSMAXTERM_12,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_12,''),
                   SPBFILE.SPB_INSEXCEPT_12,
                   SPBFILE.SPB_INSNOVER_12,
                   SPBFILE.SPB_INSMINAGE_12_1,
                   SPBFILE.SPB_INSMAXAGE_12_1,
                   SPBFILE.SPB_INSMAXCOV_12_1,
                   SPBFILE.SPB_INSMAXTRM_12_1,
                   SPBFILE.SPB_INSMINAGE_12_2,
                   SPBFILE.SPB_INSMAXAGE_12_2,
                   SPBFILE.SPB_INSMAXCOV_12_2,
                   SPBFILE.SPB_INSMAXTRM_12_2,
                   SPBFILE.SPB_INSMINAGE_12_3,
                   SPBFILE.SPB_INSMAXAGE_12_3,
                   SPBFILE.SPB_INSMAXCOV_12_3,
                   SPBFILE.SPB_INSMAXTRM_12_3,
                   SPBFILE.SPB_INSMINAGE_12_4,
                   SPBFILE.SPB_INSMAXAGE_12_4,
                   SPBFILE.SPB_INSMAXCOV_12_4,
                   SPBFILE.SPB_INSMAXTRM_12_4,
                   SPBFILE.SPB_INSMINAGE_12_5,
                   SPBFILE.SPB_INSMAXAGE_12_5,
                   SPBFILE.SPB_INSMAXCOV_12_5,
                   SPBFILE.SPB_INSMAXTRM_12_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_12,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_12,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_12,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_12,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_12,'')
            INTO :QSPB-REC
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

      *----------------------------------------------------------
       WRITE-SPB1-FILE.
           PERFORM WRITE-IT.
           MOVE SPB-SELECT-PATH TO E-FILE.
           MOVE SPB1-KEY        TO E-KEYX.
           PERFORM SET-SPB-FIELDS.

           EXEC SQL
            INSERT INTO DBO.SPBFILE
             (SPBFILE.SPB_ORGST,
              SPBFILE.SPB_SPRCLASS,
              SPBFILE.SPB_SUBCLASS,
              SPBFILE.SPB_LAWCODE,
              SPBFILE.SPB_INSCOM_1,
              SPBFILE.SPB_INSRATECD_1,
              SPBFILE.SPB_INSJFAC_1,
              SPBFILE.SPB_INSDISCNT_1,
              SPBFILE.SPB_INSCHGABL_1,
              SPBFILE.SPB_INSTERM_1_1,
              SPBFILE.SPB_INSRATE_1_1,
              SPBFILE.SPB_INSTERM_1_2,
              SPBFILE.SPB_INSRATE_1_2,
              SPBFILE.SPB_INSTERM_1_3,
              SPBFILE.SPB_INSRATE_1_3,
              SPBFILE.SPB_INSTERM_1_4,
              SPBFILE.SPB_INSRATE_1_4,
              SPBFILE.SPB_INSTERM_1_5,
              SPBFILE.SPB_INSRATE_1_5,
              SPBFILE.SPB_INSTERM_1_6,
              SPBFILE.SPB_INSRATE_1_6,
              SPBFILE.SPB_INSTERM_1_7,
              SPBFILE.SPB_INSRATE_1_7,
              SPBFILE.SPB_INSTERM_1_8,
              SPBFILE.SPB_INSRATE_1_8,
              SPBFILE.SPB_INSTERM_1_9,
              SPBFILE.SPB_INSRATE_1_9,
              SPBFILE.SPB_INSTERM_1_10,
              SPBFILE.SPB_INSRATE_1_10,
              SPBFILE.SPB_INSTERM_1_11,
              SPBFILE.SPB_INSRATE_1_11,
              SPBFILE.SPB_INSTERM_1_12,
              SPBFILE.SPB_INSRATE_1_12,
              SPBFILE.SPB_INSTERM_1_13,
              SPBFILE.SPB_INSRATE_1_13,
              SPBFILE.SPB_INSTERM_1_14,
              SPBFILE.SPB_INSRATE_1_14,
              SPBFILE.SPB_INSTERM_1_15,
              SPBFILE.SPB_INSRATE_1_15,
              SPBFILE.SPB_INSTERM_1_16,
              SPBFILE.SPB_INSRATE_1_16,
              SPBFILE.SPB_INSTERM_1_17,
              SPBFILE.SPB_INSRATE_1_17,
              SPBFILE.SPB_INSTERM_1_18,
              SPBFILE.SPB_INSRATE_1_18,
              SPBFILE.SPB_INSTERM_1_19,
              SPBFILE.SPB_INSRATE_1_19,
              SPBFILE.SPB_INSTERM_1_20,
              SPBFILE.SPB_INSRATE_1_20,
              SPBFILE.SPB_INSMINPREM_1,
              SPBFILE.SPB_INSMINCHGABL_1,
              SPBFILE.SPB_INSMAXCHGABL_1,
              SPBFILE.SPB_INSMAXBENIF_1,
              SPBFILE.SPB_INSMAXTERM_1,
              SPBFILE.SPB_INSCHGBEN_1,
              SPBFILE.SPB_INSEXCEPT_1,
              SPBFILE.SPB_INSNOVER_1,
              SPBFILE.SPB_INSMINAGE_1_1,
              SPBFILE.SPB_INSMAXAGE_1_1,
              SPBFILE.SPB_INSMAXCOV_1_1,
              SPBFILE.SPB_INSMAXTRM_1_1,
              SPBFILE.SPB_INSMINAGE_1_2,
              SPBFILE.SPB_INSMAXAGE_1_2,
              SPBFILE.SPB_INSMAXCOV_1_2,
              SPBFILE.SPB_INSMAXTRM_1_2,
              SPBFILE.SPB_INSMINAGE_1_3,
              SPBFILE.SPB_INSMAXAGE_1_3,
              SPBFILE.SPB_INSMAXCOV_1_3,
              SPBFILE.SPB_INSMAXTRM_1_3,
              SPBFILE.SPB_INSMINAGE_1_4,
              SPBFILE.SPB_INSMAXAGE_1_4,
              SPBFILE.SPB_INSMAXCOV_1_4,
              SPBFILE.SPB_INSMAXTRM_1_4,
              SPBFILE.SPB_INSMINAGE_1_5,
              SPBFILE.SPB_INSMAXAGE_1_5,
              SPBFILE.SPB_INSMAXCOV_1_5,
              SPBFILE.SPB_INSMAXTRM_1_5,
              SPBFILE.SPB_INSFEECD_1,
              SPBFILE.SPB_INSFEE_FRMLA_1,
              SPBFILE.SPB_INSCHGABL_FG_1,
              SPBFILE.SPB_INSREJECT_FG_1,
              SPBFILE.SPB_INSFINCHG_1,
              SPBFILE.SPB_INSCOM_2,
              SPBFILE.SPB_INSRATECD_2,
              SPBFILE.SPB_INSJFAC_2,
              SPBFILE.SPB_INSDISCNT_2,
              SPBFILE.SPB_INSCHGABL_2,
              SPBFILE.SPB_INSTERM_2_1,
              SPBFILE.SPB_INSRATE_2_1,
              SPBFILE.SPB_INSTERM_2_2,
              SPBFILE.SPB_INSRATE_2_2,
              SPBFILE.SPB_INSTERM_2_3,
              SPBFILE.SPB_INSRATE_2_3,
              SPBFILE.SPB_INSTERM_2_4,
              SPBFILE.SPB_INSRATE_2_4,
              SPBFILE.SPB_INSTERM_2_5,
              SPBFILE.SPB_INSRATE_2_5,
              SPBFILE.SPB_INSTERM_2_6,
              SPBFILE.SPB_INSRATE_2_6,
              SPBFILE.SPB_INSTERM_2_7,
              SPBFILE.SPB_INSRATE_2_7,
              SPBFILE.SPB_INSTERM_2_8,
              SPBFILE.SPB_INSRATE_2_8,
              SPBFILE.SPB_INSTERM_2_9,
              SPBFILE.SPB_INSRATE_2_9,
              SPBFILE.SPB_INSTERM_2_10,
              SPBFILE.SPB_INSRATE_2_10,
              SPBFILE.SPB_INSTERM_2_11,
              SPBFILE.SPB_INSRATE_2_11,
              SPBFILE.SPB_INSTERM_2_12,
              SPBFILE.SPB_INSRATE_2_12,
              SPBFILE.SPB_INSTERM_2_13,
              SPBFILE.SPB_INSRATE_2_13,
              SPBFILE.SPB_INSTERM_2_14,
              SPBFILE.SPB_INSRATE_2_14,
              SPBFILE.SPB_INSTERM_2_15,
              SPBFILE.SPB_INSRATE_2_15,
              SPBFILE.SPB_INSTERM_2_16,
              SPBFILE.SPB_INSRATE_2_16,
              SPBFILE.SPB_INSTERM_2_17,
              SPBFILE.SPB_INSRATE_2_17,
              SPBFILE.SPB_INSTERM_2_18,
              SPBFILE.SPB_INSRATE_2_18,
              SPBFILE.SPB_INSTERM_2_19,
              SPBFILE.SPB_INSRATE_2_19,
              SPBFILE.SPB_INSTERM_2_20,
              SPBFILE.SPB_INSRATE_2_20,
              SPBFILE.SPB_INSMINPREM_2,
              SPBFILE.SPB_INSMINCHGABL_2,
              SPBFILE.SPB_INSMAXCHGABL_2,
              SPBFILE.SPB_INSMAXBENIF_2,
              SPBFILE.SPB_INSMAXTERM_2,
              SPBFILE.SPB_INSCHGBEN_2,
              SPBFILE.SPB_INSEXCEPT_2,
              SPBFILE.SPB_INSNOVER_2,
              SPBFILE.SPB_INSMINAGE_2_1,
              SPBFILE.SPB_INSMAXAGE_2_1,
              SPBFILE.SPB_INSMAXCOV_2_1,
              SPBFILE.SPB_INSMAXTRM_2_1,
              SPBFILE.SPB_INSMINAGE_2_2,
              SPBFILE.SPB_INSMAXAGE_2_2,
              SPBFILE.SPB_INSMAXCOV_2_2,
              SPBFILE.SPB_INSMAXTRM_2_2,
              SPBFILE.SPB_INSMINAGE_2_3,
              SPBFILE.SPB_INSMAXAGE_2_3,
              SPBFILE.SPB_INSMAXCOV_2_3,
              SPBFILE.SPB_INSMAXTRM_2_3,
              SPBFILE.SPB_INSMINAGE_2_4,
              SPBFILE.SPB_INSMAXAGE_2_4,
              SPBFILE.SPB_INSMAXCOV_2_4,
              SPBFILE.SPB_INSMAXTRM_2_4,
              SPBFILE.SPB_INSMINAGE_2_5,
              SPBFILE.SPB_INSMAXAGE_2_5,
              SPBFILE.SPB_INSMAXCOV_2_5,
              SPBFILE.SPB_INSMAXTRM_2_5,
              SPBFILE.SPB_INSFEECD_2,
              SPBFILE.SPB_INSFEE_FRMLA_2,
              SPBFILE.SPB_INSCHGABL_FG_2,
              SPBFILE.SPB_INSREJECT_FG_2,
              SPBFILE.SPB_INSFINCHG_2,
              SPBFILE.SPB_INSCOM_3,
              SPBFILE.SPB_INSRATECD_3,
              SPBFILE.SPB_INSJFAC_3,
              SPBFILE.SPB_INSDISCNT_3,
              SPBFILE.SPB_INSCHGABL_3,
              SPBFILE.SPB_INSTERM_3_1,
              SPBFILE.SPB_INSRATE_3_1,
              SPBFILE.SPB_INSTERM_3_2,
              SPBFILE.SPB_INSRATE_3_2,
              SPBFILE.SPB_INSTERM_3_3,
              SPBFILE.SPB_INSRATE_3_3,
              SPBFILE.SPB_INSTERM_3_4,
              SPBFILE.SPB_INSRATE_3_4,
              SPBFILE.SPB_INSTERM_3_5,
              SPBFILE.SPB_INSRATE_3_5,
              SPBFILE.SPB_INSTERM_3_6,
              SPBFILE.SPB_INSRATE_3_6,
              SPBFILE.SPB_INSTERM_3_7,
              SPBFILE.SPB_INSRATE_3_7,
              SPBFILE.SPB_INSTERM_3_8,
              SPBFILE.SPB_INSRATE_3_8,
              SPBFILE.SPB_INSTERM_3_9,
              SPBFILE.SPB_INSRATE_3_9,
              SPBFILE.SPB_INSTERM_3_10,
              SPBFILE.SPB_INSRATE_3_10,
              SPBFILE.SPB_INSTERM_3_11,
              SPBFILE.SPB_INSRATE_3_11,
              SPBFILE.SPB_INSTERM_3_12,
              SPBFILE.SPB_INSRATE_3_12,
              SPBFILE.SPB_INSTERM_3_13,
              SPBFILE.SPB_INSRATE_3_13,
              SPBFILE.SPB_INSTERM_3_14,
              SPBFILE.SPB_INSRATE_3_14,
              SPBFILE.SPB_INSTERM_3_15,
              SPBFILE.SPB_INSRATE_3_15,
              SPBFILE.SPB_INSTERM_3_16,
              SPBFILE.SPB_INSRATE_3_16,
              SPBFILE.SPB_INSTERM_3_17,
              SPBFILE.SPB_INSRATE_3_17,
              SPBFILE.SPB_INSTERM_3_18,
              SPBFILE.SPB_INSRATE_3_18,
              SPBFILE.SPB_INSTERM_3_19,
              SPBFILE.SPB_INSRATE_3_19,
              SPBFILE.SPB_INSTERM_3_20,
              SPBFILE.SPB_INSRATE_3_20,
              SPBFILE.SPB_INSMINPREM_3,
              SPBFILE.SPB_INSMINCHGABL_3,
              SPBFILE.SPB_INSMAXCHGABL_3,
              SPBFILE.SPB_INSMAXBENIF_3,
              SPBFILE.SPB_INSMAXTERM_3,
              SPBFILE.SPB_INSCHGBEN_3,
              SPBFILE.SPB_INSEXCEPT_3,
              SPBFILE.SPB_INSNOVER_3,
              SPBFILE.SPB_INSMINAGE_3_1,
              SPBFILE.SPB_INSMAXAGE_3_1,
              SPBFILE.SPB_INSMAXCOV_3_1,
              SPBFILE.SPB_INSMAXTRM_3_1,
              SPBFILE.SPB_INSMINAGE_3_2,
              SPBFILE.SPB_INSMAXAGE_3_2,
              SPBFILE.SPB_INSMAXCOV_3_2,
              SPBFILE.SPB_INSMAXTRM_3_2,
              SPBFILE.SPB_INSMINAGE_3_3,
              SPBFILE.SPB_INSMAXAGE_3_3,
              SPBFILE.SPB_INSMAXCOV_3_3,
              SPBFILE.SPB_INSMAXTRM_3_3,
              SPBFILE.SPB_INSMINAGE_3_4,
              SPBFILE.SPB_INSMAXAGE_3_4,
              SPBFILE.SPB_INSMAXCOV_3_4,
              SPBFILE.SPB_INSMAXTRM_3_4,
              SPBFILE.SPB_INSMINAGE_3_5,
              SPBFILE.SPB_INSMAXAGE_3_5,
              SPBFILE.SPB_INSMAXCOV_3_5,
              SPBFILE.SPB_INSMAXTRM_3_5,
              SPBFILE.SPB_INSFEECD_3,
              SPBFILE.SPB_INSFEE_FRMLA_3,
              SPBFILE.SPB_INSCHGABL_FG_3,
              SPBFILE.SPB_INSREJECT_FG_3,
              SPBFILE.SPB_INSFINCHG_3,
              SPBFILE.SPB_INSCOM_4,
              SPBFILE.SPB_INSRATECD_4,
              SPBFILE.SPB_INSJFAC_4,
              SPBFILE.SPB_INSDISCNT_4,
              SPBFILE.SPB_INSCHGABL_4,
              SPBFILE.SPB_INSTERM_4_1,
              SPBFILE.SPB_INSRATE_4_1,
              SPBFILE.SPB_INSTERM_4_2,
              SPBFILE.SPB_INSRATE_4_2,
              SPBFILE.SPB_INSTERM_4_3,
              SPBFILE.SPB_INSRATE_4_3,
              SPBFILE.SPB_INSTERM_4_4,
              SPBFILE.SPB_INSRATE_4_4,
              SPBFILE.SPB_INSTERM_4_5,
              SPBFILE.SPB_INSRATE_4_5,
              SPBFILE.SPB_INSTERM_4_6,
              SPBFILE.SPB_INSRATE_4_6,
              SPBFILE.SPB_INSTERM_4_7,
              SPBFILE.SPB_INSRATE_4_7,
              SPBFILE.SPB_INSTERM_4_8,
              SPBFILE.SPB_INSRATE_4_8,
              SPBFILE.SPB_INSTERM_4_9,
              SPBFILE.SPB_INSRATE_4_9,
              SPBFILE.SPB_INSTERM_4_10,
              SPBFILE.SPB_INSRATE_4_10,
              SPBFILE.SPB_INSTERM_4_11,
              SPBFILE.SPB_INSRATE_4_11,
              SPBFILE.SPB_INSTERM_4_12,
              SPBFILE.SPB_INSRATE_4_12,
              SPBFILE.SPB_INSTERM_4_13,
              SPBFILE.SPB_INSRATE_4_13,
              SPBFILE.SPB_INSTERM_4_14,
              SPBFILE.SPB_INSRATE_4_14,
              SPBFILE.SPB_INSTERM_4_15,
              SPBFILE.SPB_INSRATE_4_15,
              SPBFILE.SPB_INSTERM_4_16,
              SPBFILE.SPB_INSRATE_4_16,
              SPBFILE.SPB_INSTERM_4_17,
              SPBFILE.SPB_INSRATE_4_17,
              SPBFILE.SPB_INSTERM_4_18,
              SPBFILE.SPB_INSRATE_4_18,
              SPBFILE.SPB_INSTERM_4_19,
              SPBFILE.SPB_INSRATE_4_19,
              SPBFILE.SPB_INSTERM_4_20,
              SPBFILE.SPB_INSRATE_4_20,
              SPBFILE.SPB_INSMINPREM_4,
              SPBFILE.SPB_INSMINCHGABL_4,
              SPBFILE.SPB_INSMAXCHGABL_4,
              SPBFILE.SPB_INSMAXBENIF_4,
              SPBFILE.SPB_INSMAXTERM_4,
              SPBFILE.SPB_INSCHGBEN_4,
              SPBFILE.SPB_INSEXCEPT_4,
              SPBFILE.SPB_INSNOVER_4,
              SPBFILE.SPB_INSMINAGE_4_1,
              SPBFILE.SPB_INSMAXAGE_4_1,
              SPBFILE.SPB_INSMAXCOV_4_1,
              SPBFILE.SPB_INSMAXTRM_4_1,
              SPBFILE.SPB_INSMINAGE_4_2,
              SPBFILE.SPB_INSMAXAGE_4_2,
              SPBFILE.SPB_INSMAXCOV_4_2,
              SPBFILE.SPB_INSMAXTRM_4_2,
              SPBFILE.SPB_INSMINAGE_4_3,
              SPBFILE.SPB_INSMAXAGE_4_3,
              SPBFILE.SPB_INSMAXCOV_4_3,
              SPBFILE.SPB_INSMAXTRM_4_3,
              SPBFILE.SPB_INSMINAGE_4_4,
              SPBFILE.SPB_INSMAXAGE_4_4,
              SPBFILE.SPB_INSMAXCOV_4_4,
              SPBFILE.SPB_INSMAXTRM_4_4,
              SPBFILE.SPB_INSMINAGE_4_5,
              SPBFILE.SPB_INSMAXAGE_4_5,
              SPBFILE.SPB_INSMAXCOV_4_5,
              SPBFILE.SPB_INSMAXTRM_4_5,
              SPBFILE.SPB_INSFEECD_4,
              SPBFILE.SPB_INSFEE_FRMLA_4,
              SPBFILE.SPB_INSCHGABL_FG_4,
              SPBFILE.SPB_INSREJECT_FG_4,
              SPBFILE.SPB_INSFINCHG_4,
              SPBFILE.SPB_INSCOM_5,
              SPBFILE.SPB_INSRATECD_5,
              SPBFILE.SPB_INSJFAC_5,
              SPBFILE.SPB_INSDISCNT_5,
              SPBFILE.SPB_INSCHGABL_5,
              SPBFILE.SPB_INSTERM_5_1,
              SPBFILE.SPB_INSRATE_5_1,
              SPBFILE.SPB_INSTERM_5_2,
              SPBFILE.SPB_INSRATE_5_2,
              SPBFILE.SPB_INSTERM_5_3,
              SPBFILE.SPB_INSRATE_5_3,
              SPBFILE.SPB_INSTERM_5_4,
              SPBFILE.SPB_INSRATE_5_4,
              SPBFILE.SPB_INSTERM_5_5,
              SPBFILE.SPB_INSRATE_5_5,
              SPBFILE.SPB_INSTERM_5_6,
              SPBFILE.SPB_INSRATE_5_6,
              SPBFILE.SPB_INSTERM_5_7,
              SPBFILE.SPB_INSRATE_5_7,
              SPBFILE.SPB_INSTERM_5_8,
              SPBFILE.SPB_INSRATE_5_8,
              SPBFILE.SPB_INSTERM_5_9,
              SPBFILE.SPB_INSRATE_5_9,
              SPBFILE.SPB_INSTERM_5_10,
              SPBFILE.SPB_INSRATE_5_10,
              SPBFILE.SPB_INSTERM_5_11,
              SPBFILE.SPB_INSRATE_5_11,
              SPBFILE.SPB_INSTERM_5_12,
              SPBFILE.SPB_INSRATE_5_12,
              SPBFILE.SPB_INSTERM_5_13,
              SPBFILE.SPB_INSRATE_5_13,
              SPBFILE.SPB_INSTERM_5_14,
              SPBFILE.SPB_INSRATE_5_14,
              SPBFILE.SPB_INSTERM_5_15,
              SPBFILE.SPB_INSRATE_5_15,
              SPBFILE.SPB_INSTERM_5_16,
              SPBFILE.SPB_INSRATE_5_16,
              SPBFILE.SPB_INSTERM_5_17,
              SPBFILE.SPB_INSRATE_5_17,
              SPBFILE.SPB_INSTERM_5_18,
              SPBFILE.SPB_INSRATE_5_18,
              SPBFILE.SPB_INSTERM_5_19,
              SPBFILE.SPB_INSRATE_5_19,
              SPBFILE.SPB_INSTERM_5_20,
              SPBFILE.SPB_INSRATE_5_20,
              SPBFILE.SPB_INSMINPREM_5,
              SPBFILE.SPB_INSMINCHGABL_5,
              SPBFILE.SPB_INSMAXCHGABL_5,
              SPBFILE.SPB_INSMAXBENIF_5,
              SPBFILE.SPB_INSMAXTERM_5,
              SPBFILE.SPB_INSCHGBEN_5,
              SPBFILE.SPB_INSEXCEPT_5,
              SPBFILE.SPB_INSNOVER_5,
              SPBFILE.SPB_INSMINAGE_5_1,
              SPBFILE.SPB_INSMAXAGE_5_1,
              SPBFILE.SPB_INSMAXCOV_5_1,
              SPBFILE.SPB_INSMAXTRM_5_1,
              SPBFILE.SPB_INSMINAGE_5_2,
              SPBFILE.SPB_INSMAXAGE_5_2,
              SPBFILE.SPB_INSMAXCOV_5_2,
              SPBFILE.SPB_INSMAXTRM_5_2,
              SPBFILE.SPB_INSMINAGE_5_3,
              SPBFILE.SPB_INSMAXAGE_5_3,
              SPBFILE.SPB_INSMAXCOV_5_3,
              SPBFILE.SPB_INSMAXTRM_5_3,
              SPBFILE.SPB_INSMINAGE_5_4,
              SPBFILE.SPB_INSMAXAGE_5_4,
              SPBFILE.SPB_INSMAXCOV_5_4,
              SPBFILE.SPB_INSMAXTRM_5_4,
              SPBFILE.SPB_INSMINAGE_5_5,
              SPBFILE.SPB_INSMAXAGE_5_5,
              SPBFILE.SPB_INSMAXCOV_5_5,
              SPBFILE.SPB_INSMAXTRM_5_5,
              SPBFILE.SPB_INSFEECD_5,
              SPBFILE.SPB_INSFEE_FRMLA_5,
              SPBFILE.SPB_INSCHGABL_FG_5,
              SPBFILE.SPB_INSREJECT_FG_5,
              SPBFILE.SPB_INSFINCHG_5,
              SPBFILE.SPB_INSCOM_6,
              SPBFILE.SPB_INSRATECD_6,
              SPBFILE.SPB_INSJFAC_6,
              SPBFILE.SPB_INSDISCNT_6,
              SPBFILE.SPB_INSCHGABL_6,
              SPBFILE.SPB_INSTERM_6_1,
              SPBFILE.SPB_INSRATE_6_1,
              SPBFILE.SPB_INSTERM_6_2,
              SPBFILE.SPB_INSRATE_6_2,
              SPBFILE.SPB_INSTERM_6_3,
              SPBFILE.SPB_INSRATE_6_3,
              SPBFILE.SPB_INSTERM_6_4,
              SPBFILE.SPB_INSRATE_6_4,
              SPBFILE.SPB_INSTERM_6_5,
              SPBFILE.SPB_INSRATE_6_5,
              SPBFILE.SPB_INSTERM_6_6,
              SPBFILE.SPB_INSRATE_6_6,
              SPBFILE.SPB_INSTERM_6_7,
              SPBFILE.SPB_INSRATE_6_7,
              SPBFILE.SPB_INSTERM_6_8,
              SPBFILE.SPB_INSRATE_6_8,
              SPBFILE.SPB_INSTERM_6_9,
              SPBFILE.SPB_INSRATE_6_9,
              SPBFILE.SPB_INSTERM_6_10,
              SPBFILE.SPB_INSRATE_6_10,
              SPBFILE.SPB_INSTERM_6_11,
              SPBFILE.SPB_INSRATE_6_11,
              SPBFILE.SPB_INSTERM_6_12,
              SPBFILE.SPB_INSRATE_6_12,
              SPBFILE.SPB_INSTERM_6_13,
              SPBFILE.SPB_INSRATE_6_13,
              SPBFILE.SPB_INSTERM_6_14,
              SPBFILE.SPB_INSRATE_6_14,
              SPBFILE.SPB_INSTERM_6_15,
              SPBFILE.SPB_INSRATE_6_15,
              SPBFILE.SPB_INSTERM_6_16,
              SPBFILE.SPB_INSRATE_6_16,
              SPBFILE.SPB_INSTERM_6_17,
              SPBFILE.SPB_INSRATE_6_17,
              SPBFILE.SPB_INSTERM_6_18,
              SPBFILE.SPB_INSRATE_6_18,
              SPBFILE.SPB_INSTERM_6_19,
              SPBFILE.SPB_INSRATE_6_19,
              SPBFILE.SPB_INSTERM_6_20,
              SPBFILE.SPB_INSRATE_6_20,
              SPBFILE.SPB_INSMINPREM_6,
              SPBFILE.SPB_INSMINCHGABL_6,
              SPBFILE.SPB_INSMAXCHGABL_6,
              SPBFILE.SPB_INSMAXBENIF_6,
              SPBFILE.SPB_INSMAXTERM_6,
              SPBFILE.SPB_INSCHGBEN_6,
              SPBFILE.SPB_INSEXCEPT_6,
              SPBFILE.SPB_INSNOVER_6,
              SPBFILE.SPB_INSMINAGE_6_1,
              SPBFILE.SPB_INSMAXAGE_6_1,
              SPBFILE.SPB_INSMAXCOV_6_1,
              SPBFILE.SPB_INSMAXTRM_6_1,
              SPBFILE.SPB_INSMINAGE_6_2,
              SPBFILE.SPB_INSMAXAGE_6_2,
              SPBFILE.SPB_INSMAXCOV_6_2,
              SPBFILE.SPB_INSMAXTRM_6_2,
              SPBFILE.SPB_INSMINAGE_6_3,
              SPBFILE.SPB_INSMAXAGE_6_3,
              SPBFILE.SPB_INSMAXCOV_6_3,
              SPBFILE.SPB_INSMAXTRM_6_3,
              SPBFILE.SPB_INSMINAGE_6_4,
              SPBFILE.SPB_INSMAXAGE_6_4,
              SPBFILE.SPB_INSMAXCOV_6_4,
              SPBFILE.SPB_INSMAXTRM_6_4,
              SPBFILE.SPB_INSMINAGE_6_5,
              SPBFILE.SPB_INSMAXAGE_6_5,
              SPBFILE.SPB_INSMAXCOV_6_5,
              SPBFILE.SPB_INSMAXTRM_6_5,
              SPBFILE.SPB_INSFEECD_6,
              SPBFILE.SPB_INSFEE_FRMLA_6,
              SPBFILE.SPB_INSCHGABL_FG_6,
              SPBFILE.SPB_INSREJECT_FG_6,
              SPBFILE.SPB_INSFINCHG_6,
              SPBFILE.SPB_INSCOM_7,
              SPBFILE.SPB_INSRATECD_7,
              SPBFILE.SPB_INSJFAC_7,
              SPBFILE.SPB_INSDISCNT_7,
              SPBFILE.SPB_INSCHGABL_7,
              SPBFILE.SPB_INSTERM_7_1,
              SPBFILE.SPB_INSRATE_7_1,
              SPBFILE.SPB_INSTERM_7_2,
              SPBFILE.SPB_INSRATE_7_2,
              SPBFILE.SPB_INSTERM_7_3,
              SPBFILE.SPB_INSRATE_7_3,
              SPBFILE.SPB_INSTERM_7_4,
              SPBFILE.SPB_INSRATE_7_4,
              SPBFILE.SPB_INSTERM_7_5,
              SPBFILE.SPB_INSRATE_7_5,
              SPBFILE.SPB_INSTERM_7_6,
              SPBFILE.SPB_INSRATE_7_6,
              SPBFILE.SPB_INSTERM_7_7,
              SPBFILE.SPB_INSRATE_7_7,
              SPBFILE.SPB_INSTERM_7_8,
              SPBFILE.SPB_INSRATE_7_8,
              SPBFILE.SPB_INSTERM_7_9,
              SPBFILE.SPB_INSRATE_7_9,
              SPBFILE.SPB_INSTERM_7_10,
              SPBFILE.SPB_INSRATE_7_10,
              SPBFILE.SPB_INSTERM_7_11,
              SPBFILE.SPB_INSRATE_7_11,
              SPBFILE.SPB_INSTERM_7_12,
              SPBFILE.SPB_INSRATE_7_12,
              SPBFILE.SPB_INSTERM_7_13,
              SPBFILE.SPB_INSRATE_7_13,
              SPBFILE.SPB_INSTERM_7_14,
              SPBFILE.SPB_INSRATE_7_14,
              SPBFILE.SPB_INSTERM_7_15,
              SPBFILE.SPB_INSRATE_7_15,
              SPBFILE.SPB_INSTERM_7_16,
              SPBFILE.SPB_INSRATE_7_16,
              SPBFILE.SPB_INSTERM_7_17,
              SPBFILE.SPB_INSRATE_7_17,
              SPBFILE.SPB_INSTERM_7_18,
              SPBFILE.SPB_INSRATE_7_18,
              SPBFILE.SPB_INSTERM_7_19,
              SPBFILE.SPB_INSRATE_7_19,
              SPBFILE.SPB_INSTERM_7_20,
              SPBFILE.SPB_INSRATE_7_20,
              SPBFILE.SPB_INSMINPREM_7,
              SPBFILE.SPB_INSMINCHGABL_7,
              SPBFILE.SPB_INSMAXCHGABL_7,
              SPBFILE.SPB_INSMAXBENIF_7,
              SPBFILE.SPB_INSMAXTERM_7,
              SPBFILE.SPB_INSCHGBEN_7,
              SPBFILE.SPB_INSEXCEPT_7,
              SPBFILE.SPB_INSNOVER_7,
              SPBFILE.SPB_INSMINAGE_7_1,
              SPBFILE.SPB_INSMAXAGE_7_1,
              SPBFILE.SPB_INSMAXCOV_7_1,
              SPBFILE.SPB_INSMAXTRM_7_1,
              SPBFILE.SPB_INSMINAGE_7_2,
              SPBFILE.SPB_INSMAXAGE_7_2,
              SPBFILE.SPB_INSMAXCOV_7_2,
              SPBFILE.SPB_INSMAXTRM_7_2,
              SPBFILE.SPB_INSMINAGE_7_3,
              SPBFILE.SPB_INSMAXAGE_7_3,
              SPBFILE.SPB_INSMAXCOV_7_3,
              SPBFILE.SPB_INSMAXTRM_7_3,
              SPBFILE.SPB_INSMINAGE_7_4,
              SPBFILE.SPB_INSMAXAGE_7_4,
              SPBFILE.SPB_INSMAXCOV_7_4,
              SPBFILE.SPB_INSMAXTRM_7_4,
              SPBFILE.SPB_INSMINAGE_7_5,
              SPBFILE.SPB_INSMAXAGE_7_5,
              SPBFILE.SPB_INSMAXCOV_7_5,
              SPBFILE.SPB_INSMAXTRM_7_5,
              SPBFILE.SPB_INSFEECD_7,
              SPBFILE.SPB_INSFEE_FRMLA_7,
              SPBFILE.SPB_INSCHGABL_FG_7,
              SPBFILE.SPB_INSREJECT_FG_7,
              SPBFILE.SPB_INSFINCHG_7,
              SPBFILE.SPB_INSCOM_8,
              SPBFILE.SPB_INSRATECD_8,
              SPBFILE.SPB_INSJFAC_8,
              SPBFILE.SPB_INSDISCNT_8,
              SPBFILE.SPB_INSCHGABL_8,
              SPBFILE.SPB_INSTERM_8_1,
              SPBFILE.SPB_INSRATE_8_1,
              SPBFILE.SPB_INSTERM_8_2,
              SPBFILE.SPB_INSRATE_8_2,
              SPBFILE.SPB_INSTERM_8_3,
              SPBFILE.SPB_INSRATE_8_3,
              SPBFILE.SPB_INSTERM_8_4,
              SPBFILE.SPB_INSRATE_8_4,
              SPBFILE.SPB_INSTERM_8_5,
              SPBFILE.SPB_INSRATE_8_5,
              SPBFILE.SPB_INSTERM_8_6,
              SPBFILE.SPB_INSRATE_8_6,
              SPBFILE.SPB_INSTERM_8_7,
              SPBFILE.SPB_INSRATE_8_7,
              SPBFILE.SPB_INSTERM_8_8,
              SPBFILE.SPB_INSRATE_8_8,
              SPBFILE.SPB_INSTERM_8_9,
              SPBFILE.SPB_INSRATE_8_9,
              SPBFILE.SPB_INSTERM_8_10,
              SPBFILE.SPB_INSRATE_8_10,
              SPBFILE.SPB_INSTERM_8_11,
              SPBFILE.SPB_INSRATE_8_11,
              SPBFILE.SPB_INSTERM_8_12,
              SPBFILE.SPB_INSRATE_8_12,
              SPBFILE.SPB_INSTERM_8_13,
              SPBFILE.SPB_INSRATE_8_13,
              SPBFILE.SPB_INSTERM_8_14,
              SPBFILE.SPB_INSRATE_8_14,
              SPBFILE.SPB_INSTERM_8_15,
              SPBFILE.SPB_INSRATE_8_15,
              SPBFILE.SPB_INSTERM_8_16,
              SPBFILE.SPB_INSRATE_8_16,
              SPBFILE.SPB_INSTERM_8_17,
              SPBFILE.SPB_INSRATE_8_17,
              SPBFILE.SPB_INSTERM_8_18,
              SPBFILE.SPB_INSRATE_8_18,
              SPBFILE.SPB_INSTERM_8_19,
              SPBFILE.SPB_INSRATE_8_19,
              SPBFILE.SPB_INSTERM_8_20,
              SPBFILE.SPB_INSRATE_8_20,
              SPBFILE.SPB_INSMINPREM_8,
              SPBFILE.SPB_INSMINCHGABL_8,
              SPBFILE.SPB_INSMAXCHGABL_8,
              SPBFILE.SPB_INSMAXBENIF_8,
              SPBFILE.SPB_INSMAXTERM_8,
              SPBFILE.SPB_INSCHGBEN_8,
              SPBFILE.SPB_INSEXCEPT_8,
              SPBFILE.SPB_INSNOVER_8,
              SPBFILE.SPB_INSMINAGE_8_1,
              SPBFILE.SPB_INSMAXAGE_8_1,
              SPBFILE.SPB_INSMAXCOV_8_1,
              SPBFILE.SPB_INSMAXTRM_8_1,
              SPBFILE.SPB_INSMINAGE_8_2,
              SPBFILE.SPB_INSMAXAGE_8_2,
              SPBFILE.SPB_INSMAXCOV_8_2,
              SPBFILE.SPB_INSMAXTRM_8_2,
              SPBFILE.SPB_INSMINAGE_8_3,
              SPBFILE.SPB_INSMAXAGE_8_3,
              SPBFILE.SPB_INSMAXCOV_8_3,
              SPBFILE.SPB_INSMAXTRM_8_3,
              SPBFILE.SPB_INSMINAGE_8_4,
              SPBFILE.SPB_INSMAXAGE_8_4,
              SPBFILE.SPB_INSMAXCOV_8_4,
              SPBFILE.SPB_INSMAXTRM_8_4,
              SPBFILE.SPB_INSMINAGE_8_5,
              SPBFILE.SPB_INSMAXAGE_8_5,
              SPBFILE.SPB_INSMAXCOV_8_5,
              SPBFILE.SPB_INSMAXTRM_8_5,
              SPBFILE.SPB_INSFEECD_8,
              SPBFILE.SPB_INSFEE_FRMLA_8,
              SPBFILE.SPB_INSCHGABL_FG_8,
              SPBFILE.SPB_INSREJECT_FG_8,
              SPBFILE.SPB_INSFINCHG_8,
              SPBFILE.SPB_INSCOM_9,
              SPBFILE.SPB_INSRATECD_9,
              SPBFILE.SPB_INSJFAC_9,
              SPBFILE.SPB_INSDISCNT_9,
              SPBFILE.SPB_INSCHGABL_9,
              SPBFILE.SPB_INSTERM_9_1,
              SPBFILE.SPB_INSRATE_9_1,
              SPBFILE.SPB_INSTERM_9_2,
              SPBFILE.SPB_INSRATE_9_2,
              SPBFILE.SPB_INSTERM_9_3,
              SPBFILE.SPB_INSRATE_9_3,
              SPBFILE.SPB_INSTERM_9_4,
              SPBFILE.SPB_INSRATE_9_4,
              SPBFILE.SPB_INSTERM_9_5,
              SPBFILE.SPB_INSRATE_9_5,
              SPBFILE.SPB_INSTERM_9_6,
              SPBFILE.SPB_INSRATE_9_6,
              SPBFILE.SPB_INSTERM_9_7,
              SPBFILE.SPB_INSRATE_9_7,
              SPBFILE.SPB_INSTERM_9_8,
              SPBFILE.SPB_INSRATE_9_8,
              SPBFILE.SPB_INSTERM_9_9,
              SPBFILE.SPB_INSRATE_9_9,
              SPBFILE.SPB_INSTERM_9_10,
              SPBFILE.SPB_INSRATE_9_10,
              SPBFILE.SPB_INSTERM_9_11,
              SPBFILE.SPB_INSRATE_9_11,
              SPBFILE.SPB_INSTERM_9_12,
              SPBFILE.SPB_INSRATE_9_12,
              SPBFILE.SPB_INSTERM_9_13,
              SPBFILE.SPB_INSRATE_9_13,
              SPBFILE.SPB_INSTERM_9_14,
              SPBFILE.SPB_INSRATE_9_14,
              SPBFILE.SPB_INSTERM_9_15,
              SPBFILE.SPB_INSRATE_9_15,
              SPBFILE.SPB_INSTERM_9_16,
              SPBFILE.SPB_INSRATE_9_16,
              SPBFILE.SPB_INSTERM_9_17,
              SPBFILE.SPB_INSRATE_9_17,
              SPBFILE.SPB_INSTERM_9_18,
              SPBFILE.SPB_INSRATE_9_18,
              SPBFILE.SPB_INSTERM_9_19,
              SPBFILE.SPB_INSRATE_9_19,
              SPBFILE.SPB_INSTERM_9_20,
              SPBFILE.SPB_INSRATE_9_20,
              SPBFILE.SPB_INSMINPREM_9,
              SPBFILE.SPB_INSMINCHGABL_9,
              SPBFILE.SPB_INSMAXCHGABL_9,
              SPBFILE.SPB_INSMAXBENIF_9,
              SPBFILE.SPB_INSMAXTERM_9,
              SPBFILE.SPB_INSCHGBEN_9,
              SPBFILE.SPB_INSEXCEPT_9,
              SPBFILE.SPB_INSNOVER_9,
              SPBFILE.SPB_INSMINAGE_9_1,
              SPBFILE.SPB_INSMAXAGE_9_1,
              SPBFILE.SPB_INSMAXCOV_9_1,
              SPBFILE.SPB_INSMAXTRM_9_1,
              SPBFILE.SPB_INSMINAGE_9_2,
              SPBFILE.SPB_INSMAXAGE_9_2,
              SPBFILE.SPB_INSMAXCOV_9_2,
              SPBFILE.SPB_INSMAXTRM_9_2,
              SPBFILE.SPB_INSMINAGE_9_3,
              SPBFILE.SPB_INSMAXAGE_9_3,
              SPBFILE.SPB_INSMAXCOV_9_3,
              SPBFILE.SPB_INSMAXTRM_9_3,
              SPBFILE.SPB_INSMINAGE_9_4,
              SPBFILE.SPB_INSMAXAGE_9_4,
              SPBFILE.SPB_INSMAXCOV_9_4,
              SPBFILE.SPB_INSMAXTRM_9_4,
              SPBFILE.SPB_INSMINAGE_9_5,
              SPBFILE.SPB_INSMAXAGE_9_5,
              SPBFILE.SPB_INSMAXCOV_9_5,
              SPBFILE.SPB_INSMAXTRM_9_5,
              SPBFILE.SPB_INSFEECD_9,
              SPBFILE.SPB_INSFEE_FRMLA_9,
              SPBFILE.SPB_INSCHGABL_FG_9,
              SPBFILE.SPB_INSREJECT_FG_9,
              SPBFILE.SPB_INSFINCHG_9,
              SPBFILE.SPB_INSCOM_10,
              SPBFILE.SPB_INSRATECD_10,
              SPBFILE.SPB_INSJFAC_10,
              SPBFILE.SPB_INSDISCNT_10,
              SPBFILE.SPB_INSCHGABL_10,
              SPBFILE.SPB_INSTERM_10_1,
              SPBFILE.SPB_INSRATE_10_1,
              SPBFILE.SPB_INSTERM_10_2,
              SPBFILE.SPB_INSRATE_10_2,
              SPBFILE.SPB_INSTERM_10_3,
              SPBFILE.SPB_INSRATE_10_3,
              SPBFILE.SPB_INSTERM_10_4,
              SPBFILE.SPB_INSRATE_10_4,
              SPBFILE.SPB_INSTERM_10_5,
              SPBFILE.SPB_INSRATE_10_5,
              SPBFILE.SPB_INSTERM_10_6,
              SPBFILE.SPB_INSRATE_10_6,
              SPBFILE.SPB_INSTERM_10_7,
              SPBFILE.SPB_INSRATE_10_7,
              SPBFILE.SPB_INSTERM_10_8,
              SPBFILE.SPB_INSRATE_10_8,
              SPBFILE.SPB_INSTERM_10_9,
              SPBFILE.SPB_INSRATE_10_9,
              SPBFILE.SPB_INSTERM_10_10,
              SPBFILE.SPB_INSRATE_10_10,
              SPBFILE.SPB_INSTERM_10_11,
              SPBFILE.SPB_INSRATE_10_11,
              SPBFILE.SPB_INSTERM_10_12,
              SPBFILE.SPB_INSRATE_10_12,
              SPBFILE.SPB_INSTERM_10_13,
              SPBFILE.SPB_INSRATE_10_13,
              SPBFILE.SPB_INSTERM_10_14,
              SPBFILE.SPB_INSRATE_10_14,
              SPBFILE.SPB_INSTERM_10_15,
              SPBFILE.SPB_INSRATE_10_15,
              SPBFILE.SPB_INSTERM_10_16,
              SPBFILE.SPB_INSRATE_10_16,
              SPBFILE.SPB_INSTERM_10_17,
              SPBFILE.SPB_INSRATE_10_17,
              SPBFILE.SPB_INSTERM_10_18,
              SPBFILE.SPB_INSRATE_10_18,
              SPBFILE.SPB_INSTERM_10_19,
              SPBFILE.SPB_INSRATE_10_19,
              SPBFILE.SPB_INSTERM_10_20,
              SPBFILE.SPB_INSRATE_10_20,
              SPBFILE.SPB_INSMINPREM_10,
              SPBFILE.SPB_INSMINCHGABL_10,
              SPBFILE.SPB_INSMAXCHGABL_10,
              SPBFILE.SPB_INSMAXBENIF_10,
              SPBFILE.SPB_INSMAXTERM_10,
              SPBFILE.SPB_INSCHGBEN_10,
              SPBFILE.SPB_INSEXCEPT_10,
              SPBFILE.SPB_INSNOVER_10,
              SPBFILE.SPB_INSMINAGE_10_1,
              SPBFILE.SPB_INSMAXAGE_10_1,
              SPBFILE.SPB_INSMAXCOV_10_1,
              SPBFILE.SPB_INSMAXTRM_10_1,
              SPBFILE.SPB_INSMINAGE_10_2,
              SPBFILE.SPB_INSMAXAGE_10_2,
              SPBFILE.SPB_INSMAXCOV_10_2,
              SPBFILE.SPB_INSMAXTRM_10_2,
              SPBFILE.SPB_INSMINAGE_10_3,
              SPBFILE.SPB_INSMAXAGE_10_3,
              SPBFILE.SPB_INSMAXCOV_10_3,
              SPBFILE.SPB_INSMAXTRM_10_3,
              SPBFILE.SPB_INSMINAGE_10_4,
              SPBFILE.SPB_INSMAXAGE_10_4,
              SPBFILE.SPB_INSMAXCOV_10_4,
              SPBFILE.SPB_INSMAXTRM_10_4,
              SPBFILE.SPB_INSMINAGE_10_5,
              SPBFILE.SPB_INSMAXAGE_10_5,
              SPBFILE.SPB_INSMAXCOV_10_5,
              SPBFILE.SPB_INSMAXTRM_10_5,
              SPBFILE.SPB_INSFEECD_10,
              SPBFILE.SPB_INSFEE_FRMLA_10,
              SPBFILE.SPB_INSCHGABL_FG_10,
              SPBFILE.SPB_INSREJECT_FG_10,
              SPBFILE.SPB_INSFINCHG_10,
              SPBFILE.SPB_INSCOM_11,
              SPBFILE.SPB_INSRATECD_11,
              SPBFILE.SPB_INSJFAC_11,
              SPBFILE.SPB_INSDISCNT_11,
              SPBFILE.SPB_INSCHGABL_11,
              SPBFILE.SPB_INSTERM_11_1,
              SPBFILE.SPB_INSRATE_11_1,
              SPBFILE.SPB_INSTERM_11_2,
              SPBFILE.SPB_INSRATE_11_2,
              SPBFILE.SPB_INSTERM_11_3,
              SPBFILE.SPB_INSRATE_11_3,
              SPBFILE.SPB_INSTERM_11_4,
              SPBFILE.SPB_INSRATE_11_4,
              SPBFILE.SPB_INSTERM_11_5,
              SPBFILE.SPB_INSRATE_11_5,
              SPBFILE.SPB_INSTERM_11_6,
              SPBFILE.SPB_INSRATE_11_6,
              SPBFILE.SPB_INSTERM_11_7,
              SPBFILE.SPB_INSRATE_11_7,
              SPBFILE.SPB_INSTERM_11_8,
              SPBFILE.SPB_INSRATE_11_8,
              SPBFILE.SPB_INSTERM_11_9,
              SPBFILE.SPB_INSRATE_11_9,
              SPBFILE.SPB_INSTERM_11_10,
              SPBFILE.SPB_INSRATE_11_10,
              SPBFILE.SPB_INSTERM_11_11,
              SPBFILE.SPB_INSRATE_11_11,
              SPBFILE.SPB_INSTERM_11_12,
              SPBFILE.SPB_INSRATE_11_12,
              SPBFILE.SPB_INSTERM_11_13,
              SPBFILE.SPB_INSRATE_11_13,
              SPBFILE.SPB_INSTERM_11_14,
              SPBFILE.SPB_INSRATE_11_14,
              SPBFILE.SPB_INSTERM_11_15,
              SPBFILE.SPB_INSRATE_11_15,
              SPBFILE.SPB_INSTERM_11_16,
              SPBFILE.SPB_INSRATE_11_16,
              SPBFILE.SPB_INSTERM_11_17,
              SPBFILE.SPB_INSRATE_11_17,
              SPBFILE.SPB_INSTERM_11_18,
              SPBFILE.SPB_INSRATE_11_18,
              SPBFILE.SPB_INSTERM_11_19,
              SPBFILE.SPB_INSRATE_11_19,
              SPBFILE.SPB_INSTERM_11_20,
              SPBFILE.SPB_INSRATE_11_20,
              SPBFILE.SPB_INSMINPREM_11,
              SPBFILE.SPB_INSMINCHGABL_11,
              SPBFILE.SPB_INSMAXCHGABL_11,
              SPBFILE.SPB_INSMAXBENIF_11,
              SPBFILE.SPB_INSMAXTERM_11,
              SPBFILE.SPB_INSCHGBEN_11,
              SPBFILE.SPB_INSEXCEPT_11,
              SPBFILE.SPB_INSNOVER_11,
              SPBFILE.SPB_INSMINAGE_11_1,
              SPBFILE.SPB_INSMAXAGE_11_1,
              SPBFILE.SPB_INSMAXCOV_11_1,
              SPBFILE.SPB_INSMAXTRM_11_1,
              SPBFILE.SPB_INSMINAGE_11_2,
              SPBFILE.SPB_INSMAXAGE_11_2,
              SPBFILE.SPB_INSMAXCOV_11_2,
              SPBFILE.SPB_INSMAXTRM_11_2,
              SPBFILE.SPB_INSMINAGE_11_3,
              SPBFILE.SPB_INSMAXAGE_11_3,
              SPBFILE.SPB_INSMAXCOV_11_3,
              SPBFILE.SPB_INSMAXTRM_11_3,
              SPBFILE.SPB_INSMINAGE_11_4,
              SPBFILE.SPB_INSMAXAGE_11_4,
              SPBFILE.SPB_INSMAXCOV_11_4,
              SPBFILE.SPB_INSMAXTRM_11_4,
              SPBFILE.SPB_INSMINAGE_11_5,
              SPBFILE.SPB_INSMAXAGE_11_5,
              SPBFILE.SPB_INSMAXCOV_11_5,
              SPBFILE.SPB_INSMAXTRM_11_5,
              SPBFILE.SPB_INSFEECD_11,
              SPBFILE.SPB_INSFEE_FRMLA_11,
              SPBFILE.SPB_INSCHGABL_FG_11,
              SPBFILE.SPB_INSREJECT_FG_11,
              SPBFILE.SPB_INSFINCHG_11,
              SPBFILE.SPB_INSCOM_12,
              SPBFILE.SPB_INSRATECD_12,
              SPBFILE.SPB_INSJFAC_12,
              SPBFILE.SPB_INSDISCNT_12,
              SPBFILE.SPB_INSCHGABL_12,
              SPBFILE.SPB_INSTERM_12_1,
              SPBFILE.SPB_INSRATE_12_1,
              SPBFILE.SPB_INSTERM_12_2,
              SPBFILE.SPB_INSRATE_12_2,
              SPBFILE.SPB_INSTERM_12_3,
              SPBFILE.SPB_INSRATE_12_3,
              SPBFILE.SPB_INSTERM_12_4,
              SPBFILE.SPB_INSRATE_12_4,
              SPBFILE.SPB_INSTERM_12_5,
              SPBFILE.SPB_INSRATE_12_5,
              SPBFILE.SPB_INSTERM_12_6,
              SPBFILE.SPB_INSRATE_12_6,
              SPBFILE.SPB_INSTERM_12_7,
              SPBFILE.SPB_INSRATE_12_7,
              SPBFILE.SPB_INSTERM_12_8,
              SPBFILE.SPB_INSRATE_12_8,
              SPBFILE.SPB_INSTERM_12_9,
              SPBFILE.SPB_INSRATE_12_9,
              SPBFILE.SPB_INSTERM_12_10,
              SPBFILE.SPB_INSRATE_12_10,
              SPBFILE.SPB_INSTERM_12_11,
              SPBFILE.SPB_INSRATE_12_11,
              SPBFILE.SPB_INSTERM_12_12,
              SPBFILE.SPB_INSRATE_12_12,
              SPBFILE.SPB_INSTERM_12_13,
              SPBFILE.SPB_INSRATE_12_13,
              SPBFILE.SPB_INSTERM_12_14,
              SPBFILE.SPB_INSRATE_12_14,
              SPBFILE.SPB_INSTERM_12_15,
              SPBFILE.SPB_INSRATE_12_15,
              SPBFILE.SPB_INSTERM_12_16,
              SPBFILE.SPB_INSRATE_12_16,
              SPBFILE.SPB_INSTERM_12_17,
              SPBFILE.SPB_INSRATE_12_17,
              SPBFILE.SPB_INSTERM_12_18,
              SPBFILE.SPB_INSRATE_12_18,
              SPBFILE.SPB_INSTERM_12_19,
              SPBFILE.SPB_INSRATE_12_19,
              SPBFILE.SPB_INSTERM_12_20,
              SPBFILE.SPB_INSRATE_12_20,
              SPBFILE.SPB_INSMINPREM_12,
              SPBFILE.SPB_INSMINCHGABL_12,
              SPBFILE.SPB_INSMAXCHGABL_12,
              SPBFILE.SPB_INSMAXBENIF_12,
              SPBFILE.SPB_INSMAXTERM_12,
              SPBFILE.SPB_INSCHGBEN_12,
              SPBFILE.SPB_INSEXCEPT_12,
              SPBFILE.SPB_INSNOVER_12,
              SPBFILE.SPB_INSMINAGE_12_1,
              SPBFILE.SPB_INSMAXAGE_12_1,
              SPBFILE.SPB_INSMAXCOV_12_1,
              SPBFILE.SPB_INSMAXTRM_12_1,
              SPBFILE.SPB_INSMINAGE_12_2,
              SPBFILE.SPB_INSMAXAGE_12_2,
              SPBFILE.SPB_INSMAXCOV_12_2,
              SPBFILE.SPB_INSMAXTRM_12_2,
              SPBFILE.SPB_INSMINAGE_12_3,
              SPBFILE.SPB_INSMAXAGE_12_3,
              SPBFILE.SPB_INSMAXCOV_12_3,
              SPBFILE.SPB_INSMAXTRM_12_3,
              SPBFILE.SPB_INSMINAGE_12_4,
              SPBFILE.SPB_INSMAXAGE_12_4,
              SPBFILE.SPB_INSMAXCOV_12_4,
              SPBFILE.SPB_INSMAXTRM_12_4,
              SPBFILE.SPB_INSMINAGE_12_5,
              SPBFILE.SPB_INSMAXAGE_12_5,
              SPBFILE.SPB_INSMAXCOV_12_5,
              SPBFILE.SPB_INSMAXTRM_12_5,
              SPBFILE.SPB_INSFEECD_12,
              SPBFILE.SPB_INSFEE_FRMLA_12,
              SPBFILE.SPB_INSCHGABL_FG_12,
              SPBFILE.SPB_INSREJECT_FG_12,
              SPBFILE.SPB_INSFINCHG_12)
            VALUES
             (:QSPB-ORGST,
              :QSPB-SPRCLASS,
              :QSPB-SUBCLASS,
              :QSPB-LAWCODE,
              :QSPB-INSCOM-1,
              :QSPB-INSRATECD-1,
              :QSPB-INSJFAC-1,
              :QSPB-INSDISCNT-1,
              :QSPB-INSCHGABL-1,
              :QSPB-INSTERM-1-1,
              :QSPB-INSRATE-1-1,
              :QSPB-INSTERM-1-2,
              :QSPB-INSRATE-1-2,
              :QSPB-INSTERM-1-3,
              :QSPB-INSRATE-1-3,
              :QSPB-INSTERM-1-4,
              :QSPB-INSRATE-1-4,
              :QSPB-INSTERM-1-5,
              :QSPB-INSRATE-1-5,
              :QSPB-INSTERM-1-6,
              :QSPB-INSRATE-1-6,
              :QSPB-INSTERM-1-7,
              :QSPB-INSRATE-1-7,
              :QSPB-INSTERM-1-8,
              :QSPB-INSRATE-1-8,
              :QSPB-INSTERM-1-9,
              :QSPB-INSRATE-1-9,
              :QSPB-INSTERM-1-10,
              :QSPB-INSRATE-1-10,
              :QSPB-INSTERM-1-11,
              :QSPB-INSRATE-1-11,
              :QSPB-INSTERM-1-12,
              :QSPB-INSRATE-1-12,
              :QSPB-INSTERM-1-13,
              :QSPB-INSRATE-1-13,
              :QSPB-INSTERM-1-14,
              :QSPB-INSRATE-1-14,
              :QSPB-INSTERM-1-15,
              :QSPB-INSRATE-1-15,
              :QSPB-INSTERM-1-16,
              :QSPB-INSRATE-1-16,
              :QSPB-INSTERM-1-17,
              :QSPB-INSRATE-1-17,
              :QSPB-INSTERM-1-18,
              :QSPB-INSRATE-1-18,
              :QSPB-INSTERM-1-19,
              :QSPB-INSRATE-1-19,
              :QSPB-INSTERM-1-20,
              :QSPB-INSRATE-1-20,
              :QSPB-INSMINPREM-1,
              :QSPB-INSMINCHGABL-1,
              :QSPB-INSMAXCHGABL-1,
              :QSPB-INSMAXBENIF-1,
              :QSPB-INSMAXTERM-1,
              :QSPB-INSCHGBEN-1,
              :QSPB-INSEXCEPT-1,
              :QSPB-INSNOVER-1,
              :QSPB-INSMINAGE-1-1,
              :QSPB-INSMAXAGE-1-1,
              :QSPB-INSMAXCOV-1-1,
              :QSPB-INSMAXTRM-1-1,
              :QSPB-INSMINAGE-1-2,
              :QSPB-INSMAXAGE-1-2,
              :QSPB-INSMAXCOV-1-2,
              :QSPB-INSMAXTRM-1-2,
              :QSPB-INSMINAGE-1-3,
              :QSPB-INSMAXAGE-1-3,
              :QSPB-INSMAXCOV-1-3,
              :QSPB-INSMAXTRM-1-3,
              :QSPB-INSMINAGE-1-4,
              :QSPB-INSMAXAGE-1-4,
              :QSPB-INSMAXCOV-1-4,
              :QSPB-INSMAXTRM-1-4,
              :QSPB-INSMINAGE-1-5,
              :QSPB-INSMAXAGE-1-5,
              :QSPB-INSMAXCOV-1-5,
              :QSPB-INSMAXTRM-1-5,
              :QSPB-INSFEECD-1,
              :QSPB-INSFEE-FRMLA-1,
              :QSPB-INSCHGABL-FG-1,
              :QSPB-INSREJECT-FG-1,
              :QSPB-INSFINCHG-1,
              :QSPB-INSCOM-2,
              :QSPB-INSRATECD-2,
              :QSPB-INSJFAC-2,
              :QSPB-INSDISCNT-2,
              :QSPB-INSCHGABL-2,
              :QSPB-INSTERM-2-1,
              :QSPB-INSRATE-2-1,
              :QSPB-INSTERM-2-2,
              :QSPB-INSRATE-2-2,
              :QSPB-INSTERM-2-3,
              :QSPB-INSRATE-2-3,
              :QSPB-INSTERM-2-4,
              :QSPB-INSRATE-2-4,
              :QSPB-INSTERM-2-5,
              :QSPB-INSRATE-2-5,
              :QSPB-INSTERM-2-6,
              :QSPB-INSRATE-2-6,
              :QSPB-INSTERM-2-7,
              :QSPB-INSRATE-2-7,
              :QSPB-INSTERM-2-8,
              :QSPB-INSRATE-2-8,
              :QSPB-INSTERM-2-9,
              :QSPB-INSRATE-2-9,
              :QSPB-INSTERM-2-10,
              :QSPB-INSRATE-2-10,
              :QSPB-INSTERM-2-11,
              :QSPB-INSRATE-2-11,
              :QSPB-INSTERM-2-12,
              :QSPB-INSRATE-2-12,
              :QSPB-INSTERM-2-13,
              :QSPB-INSRATE-2-13,
              :QSPB-INSTERM-2-14,
              :QSPB-INSRATE-2-14,
              :QSPB-INSTERM-2-15,
              :QSPB-INSRATE-2-15,
              :QSPB-INSTERM-2-16,
              :QSPB-INSRATE-2-16,
              :QSPB-INSTERM-2-17,
              :QSPB-INSRATE-2-17,
              :QSPB-INSTERM-2-18,
              :QSPB-INSRATE-2-18,
              :QSPB-INSTERM-2-19,
              :QSPB-INSRATE-2-19,
              :QSPB-INSTERM-2-20,
              :QSPB-INSRATE-2-20,
              :QSPB-INSMINPREM-2,
              :QSPB-INSMINCHGABL-2,
              :QSPB-INSMAXCHGABL-2,
              :QSPB-INSMAXBENIF-2,
              :QSPB-INSMAXTERM-2,
              :QSPB-INSCHGBEN-2,
              :QSPB-INSEXCEPT-2,
              :QSPB-INSNOVER-2,
              :QSPB-INSMINAGE-2-1,
              :QSPB-INSMAXAGE-2-1,
              :QSPB-INSMAXCOV-2-1,
              :QSPB-INSMAXTRM-2-1,
              :QSPB-INSMINAGE-2-2,
              :QSPB-INSMAXAGE-2-2,
              :QSPB-INSMAXCOV-2-2,
              :QSPB-INSMAXTRM-2-2,
              :QSPB-INSMINAGE-2-3,
              :QSPB-INSMAXAGE-2-3,
              :QSPB-INSMAXCOV-2-3,
              :QSPB-INSMAXTRM-2-3,
              :QSPB-INSMINAGE-2-4,
              :QSPB-INSMAXAGE-2-4,
              :QSPB-INSMAXCOV-2-4,
              :QSPB-INSMAXTRM-2-4,
              :QSPB-INSMINAGE-2-5,
              :QSPB-INSMAXAGE-2-5,
              :QSPB-INSMAXCOV-2-5,
              :QSPB-INSMAXTRM-2-5,
              :QSPB-INSFEECD-2,
              :QSPB-INSFEE-FRMLA-2,
              :QSPB-INSCHGABL-FG-2,
              :QSPB-INSREJECT-FG-2,
              :QSPB-INSFINCHG-2,
              :QSPB-INSCOM-3,
              :QSPB-INSRATECD-3,
              :QSPB-INSJFAC-3,
              :QSPB-INSDISCNT-3,
              :QSPB-INSCHGABL-3,
              :QSPB-INSTERM-3-1,
              :QSPB-INSRATE-3-1,
              :QSPB-INSTERM-3-2,
              :QSPB-INSRATE-3-2,
              :QSPB-INSTERM-3-3,
              :QSPB-INSRATE-3-3,
              :QSPB-INSTERM-3-4,
              :QSPB-INSRATE-3-4,
              :QSPB-INSTERM-3-5,
              :QSPB-INSRATE-3-5,
              :QSPB-INSTERM-3-6,
              :QSPB-INSRATE-3-6,
              :QSPB-INSTERM-3-7,
              :QSPB-INSRATE-3-7,
              :QSPB-INSTERM-3-8,
              :QSPB-INSRATE-3-8,
              :QSPB-INSTERM-3-9,
              :QSPB-INSRATE-3-9,
              :QSPB-INSTERM-3-10,
              :QSPB-INSRATE-3-10,
              :QSPB-INSTERM-3-11,
              :QSPB-INSRATE-3-11,
              :QSPB-INSTERM-3-12,
              :QSPB-INSRATE-3-12,
              :QSPB-INSTERM-3-13,
              :QSPB-INSRATE-3-13,
              :QSPB-INSTERM-3-14,
              :QSPB-INSRATE-3-14,
              :QSPB-INSTERM-3-15,
              :QSPB-INSRATE-3-15,
              :QSPB-INSTERM-3-16,
              :QSPB-INSRATE-3-16,
              :QSPB-INSTERM-3-17,
              :QSPB-INSRATE-3-17,
              :QSPB-INSTERM-3-18,
              :QSPB-INSRATE-3-18,
              :QSPB-INSTERM-3-19,
              :QSPB-INSRATE-3-19,
              :QSPB-INSTERM-3-20,
              :QSPB-INSRATE-3-20,
              :QSPB-INSMINPREM-3,
              :QSPB-INSMINCHGABL-3,
              :QSPB-INSMAXCHGABL-3,
              :QSPB-INSMAXBENIF-3,
              :QSPB-INSMAXTERM-3,
              :QSPB-INSCHGBEN-3,
              :QSPB-INSEXCEPT-3,
              :QSPB-INSNOVER-3,
              :QSPB-INSMINAGE-3-1,
              :QSPB-INSMAXAGE-3-1,
              :QSPB-INSMAXCOV-3-1,
              :QSPB-INSMAXTRM-3-1,
              :QSPB-INSMINAGE-3-2,
              :QSPB-INSMAXAGE-3-2,
              :QSPB-INSMAXCOV-3-2,
              :QSPB-INSMAXTRM-3-2,
              :QSPB-INSMINAGE-3-3,
              :QSPB-INSMAXAGE-3-3,
              :QSPB-INSMAXCOV-3-3,
              :QSPB-INSMAXTRM-3-3,
              :QSPB-INSMINAGE-3-4,
              :QSPB-INSMAXAGE-3-4,
              :QSPB-INSMAXCOV-3-4,
              :QSPB-INSMAXTRM-3-4,
              :QSPB-INSMINAGE-3-5,
              :QSPB-INSMAXAGE-3-5,
              :QSPB-INSMAXCOV-3-5,
              :QSPB-INSMAXTRM-3-5,
              :QSPB-INSFEECD-3,
              :QSPB-INSFEE-FRMLA-3,
              :QSPB-INSCHGABL-FG-3,
              :QSPB-INSREJECT-FG-3,
              :QSPB-INSFINCHG-3,
              :QSPB-INSCOM-4,
              :QSPB-INSRATECD-4,
              :QSPB-INSJFAC-4,
              :QSPB-INSDISCNT-4,
              :QSPB-INSCHGABL-4,
              :QSPB-INSTERM-4-1,
              :QSPB-INSRATE-4-1,
              :QSPB-INSTERM-4-2,
              :QSPB-INSRATE-4-2,
              :QSPB-INSTERM-4-3,
              :QSPB-INSRATE-4-3,
              :QSPB-INSTERM-4-4,
              :QSPB-INSRATE-4-4,
              :QSPB-INSTERM-4-5,
              :QSPB-INSRATE-4-5,
              :QSPB-INSTERM-4-6,
              :QSPB-INSRATE-4-6,
              :QSPB-INSTERM-4-7,
              :QSPB-INSRATE-4-7,
              :QSPB-INSTERM-4-8,
              :QSPB-INSRATE-4-8,
              :QSPB-INSTERM-4-9,
              :QSPB-INSRATE-4-9,
              :QSPB-INSTERM-4-10,
              :QSPB-INSRATE-4-10,
              :QSPB-INSTERM-4-11,
              :QSPB-INSRATE-4-11,
              :QSPB-INSTERM-4-12,
              :QSPB-INSRATE-4-12,
              :QSPB-INSTERM-4-13,
              :QSPB-INSRATE-4-13,
              :QSPB-INSTERM-4-14,
              :QSPB-INSRATE-4-14,
              :QSPB-INSTERM-4-15,
              :QSPB-INSRATE-4-15,
              :QSPB-INSTERM-4-16,
              :QSPB-INSRATE-4-16,
              :QSPB-INSTERM-4-17,
              :QSPB-INSRATE-4-17,
              :QSPB-INSTERM-4-18,
              :QSPB-INSRATE-4-18,
              :QSPB-INSTERM-4-19,
              :QSPB-INSRATE-4-19,
              :QSPB-INSTERM-4-20,
              :QSPB-INSRATE-4-20,
              :QSPB-INSMINPREM-4,
              :QSPB-INSMINCHGABL-4,
              :QSPB-INSMAXCHGABL-4,
              :QSPB-INSMAXBENIF-4,
              :QSPB-INSMAXTERM-4,
              :QSPB-INSCHGBEN-4,
              :QSPB-INSEXCEPT-4,
              :QSPB-INSNOVER-4,
              :QSPB-INSMINAGE-4-1,
              :QSPB-INSMAXAGE-4-1,
              :QSPB-INSMAXCOV-4-1,
              :QSPB-INSMAXTRM-4-1,
              :QSPB-INSMINAGE-4-2,
              :QSPB-INSMAXAGE-4-2,
              :QSPB-INSMAXCOV-4-2,
              :QSPB-INSMAXTRM-4-2,
              :QSPB-INSMINAGE-4-3,
              :QSPB-INSMAXAGE-4-3,
              :QSPB-INSMAXCOV-4-3,
              :QSPB-INSMAXTRM-4-3,
              :QSPB-INSMINAGE-4-4,
              :QSPB-INSMAXAGE-4-4,
              :QSPB-INSMAXCOV-4-4,
              :QSPB-INSMAXTRM-4-4,
              :QSPB-INSMINAGE-4-5,
              :QSPB-INSMAXAGE-4-5,
              :QSPB-INSMAXCOV-4-5,
              :QSPB-INSMAXTRM-4-5,
              :QSPB-INSFEECD-4,
              :QSPB-INSFEE-FRMLA-4,
              :QSPB-INSCHGABL-FG-4,
              :QSPB-INSREJECT-FG-4,
              :QSPB-INSFINCHG-4,
              :QSPB-INSCOM-5,
              :QSPB-INSRATECD-5,
              :QSPB-INSJFAC-5,
              :QSPB-INSDISCNT-5,
              :QSPB-INSCHGABL-5,
              :QSPB-INSTERM-5-1,
              :QSPB-INSRATE-5-1,
              :QSPB-INSTERM-5-2,
              :QSPB-INSRATE-5-2,
              :QSPB-INSTERM-5-3,
              :QSPB-INSRATE-5-3,
              :QSPB-INSTERM-5-4,
              :QSPB-INSRATE-5-4,
              :QSPB-INSTERM-5-5,
              :QSPB-INSRATE-5-5,
              :QSPB-INSTERM-5-6,
              :QSPB-INSRATE-5-6,
              :QSPB-INSTERM-5-7,
              :QSPB-INSRATE-5-7,
              :QSPB-INSTERM-5-8,
              :QSPB-INSRATE-5-8,
              :QSPB-INSTERM-5-9,
              :QSPB-INSRATE-5-9,
              :QSPB-INSTERM-5-10,
              :QSPB-INSRATE-5-10,
              :QSPB-INSTERM-5-11,
              :QSPB-INSRATE-5-11,
              :QSPB-INSTERM-5-12,
              :QSPB-INSRATE-5-12,
              :QSPB-INSTERM-5-13,
              :QSPB-INSRATE-5-13,
              :QSPB-INSTERM-5-14,
              :QSPB-INSRATE-5-14,
              :QSPB-INSTERM-5-15,
              :QSPB-INSRATE-5-15,
              :QSPB-INSTERM-5-16,
              :QSPB-INSRATE-5-16,
              :QSPB-INSTERM-5-17,
              :QSPB-INSRATE-5-17,
              :QSPB-INSTERM-5-18,
              :QSPB-INSRATE-5-18,
              :QSPB-INSTERM-5-19,
              :QSPB-INSRATE-5-19,
              :QSPB-INSTERM-5-20,
              :QSPB-INSRATE-5-20,
              :QSPB-INSMINPREM-5,
              :QSPB-INSMINCHGABL-5,
              :QSPB-INSMAXCHGABL-5,
              :QSPB-INSMAXBENIF-5,
              :QSPB-INSMAXTERM-5,
              :QSPB-INSCHGBEN-5,
              :QSPB-INSEXCEPT-5,
              :QSPB-INSNOVER-5,
              :QSPB-INSMINAGE-5-1,
              :QSPB-INSMAXAGE-5-1,
              :QSPB-INSMAXCOV-5-1,
              :QSPB-INSMAXTRM-5-1,
              :QSPB-INSMINAGE-5-2,
              :QSPB-INSMAXAGE-5-2,
              :QSPB-INSMAXCOV-5-2,
              :QSPB-INSMAXTRM-5-2,
              :QSPB-INSMINAGE-5-3,
              :QSPB-INSMAXAGE-5-3,
              :QSPB-INSMAXCOV-5-3,
              :QSPB-INSMAXTRM-5-3,
              :QSPB-INSMINAGE-5-4,
              :QSPB-INSMAXAGE-5-4,
              :QSPB-INSMAXCOV-5-4,
              :QSPB-INSMAXTRM-5-4,
              :QSPB-INSMINAGE-5-5,
              :QSPB-INSMAXAGE-5-5,
              :QSPB-INSMAXCOV-5-5,
              :QSPB-INSMAXTRM-5-5,
              :QSPB-INSFEECD-5,
              :QSPB-INSFEE-FRMLA-5,
              :QSPB-INSCHGABL-FG-5,
              :QSPB-INSREJECT-FG-5,
              :QSPB-INSFINCHG-5,
              :QSPB-INSCOM-6,
              :QSPB-INSRATECD-6,
              :QSPB-INSJFAC-6,
              :QSPB-INSDISCNT-6,
              :QSPB-INSCHGABL-6,
              :QSPB-INSTERM-6-1,
              :QSPB-INSRATE-6-1,
              :QSPB-INSTERM-6-2,
              :QSPB-INSRATE-6-2,
              :QSPB-INSTERM-6-3,
              :QSPB-INSRATE-6-3,
              :QSPB-INSTERM-6-4,
              :QSPB-INSRATE-6-4,
              :QSPB-INSTERM-6-5,
              :QSPB-INSRATE-6-5,
              :QSPB-INSTERM-6-6,
              :QSPB-INSRATE-6-6,
              :QSPB-INSTERM-6-7,
              :QSPB-INSRATE-6-7,
              :QSPB-INSTERM-6-8,
              :QSPB-INSRATE-6-8,
              :QSPB-INSTERM-6-9,
              :QSPB-INSRATE-6-9,
              :QSPB-INSTERM-6-10,
              :QSPB-INSRATE-6-10,
              :QSPB-INSTERM-6-11,
              :QSPB-INSRATE-6-11,
              :QSPB-INSTERM-6-12,
              :QSPB-INSRATE-6-12,
              :QSPB-INSTERM-6-13,
              :QSPB-INSRATE-6-13,
              :QSPB-INSTERM-6-14,
              :QSPB-INSRATE-6-14,
              :QSPB-INSTERM-6-15,
              :QSPB-INSRATE-6-15,
              :QSPB-INSTERM-6-16,
              :QSPB-INSRATE-6-16,
              :QSPB-INSTERM-6-17,
              :QSPB-INSRATE-6-17,
              :QSPB-INSTERM-6-18,
              :QSPB-INSRATE-6-18,
              :QSPB-INSTERM-6-19,
              :QSPB-INSRATE-6-19,
              :QSPB-INSTERM-6-20,
              :QSPB-INSRATE-6-20,
              :QSPB-INSMINPREM-6,
              :QSPB-INSMINCHGABL-6,
              :QSPB-INSMAXCHGABL-6,
              :QSPB-INSMAXBENIF-6,
              :QSPB-INSMAXTERM-6,
              :QSPB-INSCHGBEN-6,
              :QSPB-INSEXCEPT-6,
              :QSPB-INSNOVER-6,
              :QSPB-INSMINAGE-6-1,
              :QSPB-INSMAXAGE-6-1,
              :QSPB-INSMAXCOV-6-1,
              :QSPB-INSMAXTRM-6-1,
              :QSPB-INSMINAGE-6-2,
              :QSPB-INSMAXAGE-6-2,
              :QSPB-INSMAXCOV-6-2,
              :QSPB-INSMAXTRM-6-2,
              :QSPB-INSMINAGE-6-3,
              :QSPB-INSMAXAGE-6-3,
              :QSPB-INSMAXCOV-6-3,
              :QSPB-INSMAXTRM-6-3,
              :QSPB-INSMINAGE-6-4,
              :QSPB-INSMAXAGE-6-4,
              :QSPB-INSMAXCOV-6-4,
              :QSPB-INSMAXTRM-6-4,
              :QSPB-INSMINAGE-6-5,
              :QSPB-INSMAXAGE-6-5,
              :QSPB-INSMAXCOV-6-5,
              :QSPB-INSMAXTRM-6-5,
              :QSPB-INSFEECD-6,
              :QSPB-INSFEE-FRMLA-6,
              :QSPB-INSCHGABL-FG-6,
              :QSPB-INSREJECT-FG-6,
              :QSPB-INSFINCHG-6,
              :QSPB-INSCOM-7,
              :QSPB-INSRATECD-7,
              :QSPB-INSJFAC-7,
              :QSPB-INSDISCNT-7,
              :QSPB-INSCHGABL-7,
              :QSPB-INSTERM-7-1,
              :QSPB-INSRATE-7-1,
              :QSPB-INSTERM-7-2,
              :QSPB-INSRATE-7-2,
              :QSPB-INSTERM-7-3,
              :QSPB-INSRATE-7-3,
              :QSPB-INSTERM-7-4,
              :QSPB-INSRATE-7-4,
              :QSPB-INSTERM-7-5,
              :QSPB-INSRATE-7-5,
              :QSPB-INSTERM-7-6,
              :QSPB-INSRATE-7-6,
              :QSPB-INSTERM-7-7,
              :QSPB-INSRATE-7-7,
              :QSPB-INSTERM-7-8,
              :QSPB-INSRATE-7-8,
              :QSPB-INSTERM-7-9,
              :QSPB-INSRATE-7-9,
              :QSPB-INSTERM-7-10,
              :QSPB-INSRATE-7-10,
              :QSPB-INSTERM-7-11,
              :QSPB-INSRATE-7-11,
              :QSPB-INSTERM-7-12,
              :QSPB-INSRATE-7-12,
              :QSPB-INSTERM-7-13,
              :QSPB-INSRATE-7-13,
              :QSPB-INSTERM-7-14,
              :QSPB-INSRATE-7-14,
              :QSPB-INSTERM-7-15,
              :QSPB-INSRATE-7-15,
              :QSPB-INSTERM-7-16,
              :QSPB-INSRATE-7-16,
              :QSPB-INSTERM-7-17,
              :QSPB-INSRATE-7-17,
              :QSPB-INSTERM-7-18,
              :QSPB-INSRATE-7-18,
              :QSPB-INSTERM-7-19,
              :QSPB-INSRATE-7-19,
              :QSPB-INSTERM-7-20,
              :QSPB-INSRATE-7-20,
              :QSPB-INSMINPREM-7,
              :QSPB-INSMINCHGABL-7,
              :QSPB-INSMAXCHGABL-7,
              :QSPB-INSMAXBENIF-7,
              :QSPB-INSMAXTERM-7,
              :QSPB-INSCHGBEN-7,
              :QSPB-INSEXCEPT-7,
              :QSPB-INSNOVER-7,
              :QSPB-INSMINAGE-7-1,
              :QSPB-INSMAXAGE-7-1,
              :QSPB-INSMAXCOV-7-1,
              :QSPB-INSMAXTRM-7-1,
              :QSPB-INSMINAGE-7-2,
              :QSPB-INSMAXAGE-7-2,
              :QSPB-INSMAXCOV-7-2,
              :QSPB-INSMAXTRM-7-2,
              :QSPB-INSMINAGE-7-3,
              :QSPB-INSMAXAGE-7-3,
              :QSPB-INSMAXCOV-7-3,
              :QSPB-INSMAXTRM-7-3,
              :QSPB-INSMINAGE-7-4,
              :QSPB-INSMAXAGE-7-4,
              :QSPB-INSMAXCOV-7-4,
              :QSPB-INSMAXTRM-7-4,
              :QSPB-INSMINAGE-7-5,
              :QSPB-INSMAXAGE-7-5,
              :QSPB-INSMAXCOV-7-5,
              :QSPB-INSMAXTRM-7-5,
              :QSPB-INSFEECD-7,
              :QSPB-INSFEE-FRMLA-7,
              :QSPB-INSCHGABL-FG-7,
              :QSPB-INSREJECT-FG-7,
              :QSPB-INSFINCHG-7,
              :QSPB-INSCOM-8,
              :QSPB-INSRATECD-8,
              :QSPB-INSJFAC-8,
              :QSPB-INSDISCNT-8,
              :QSPB-INSCHGABL-8,
              :QSPB-INSTERM-8-1,
              :QSPB-INSRATE-8-1,
              :QSPB-INSTERM-8-2,
              :QSPB-INSRATE-8-2,
              :QSPB-INSTERM-8-3,
              :QSPB-INSRATE-8-3,
              :QSPB-INSTERM-8-4,
              :QSPB-INSRATE-8-4,
              :QSPB-INSTERM-8-5,
              :QSPB-INSRATE-8-5,
              :QSPB-INSTERM-8-6,
              :QSPB-INSRATE-8-6,
              :QSPB-INSTERM-8-7,
              :QSPB-INSRATE-8-7,
              :QSPB-INSTERM-8-8,
              :QSPB-INSRATE-8-8,
              :QSPB-INSTERM-8-9,
              :QSPB-INSRATE-8-9,
              :QSPB-INSTERM-8-10,
              :QSPB-INSRATE-8-10,
              :QSPB-INSTERM-8-11,
              :QSPB-INSRATE-8-11,
              :QSPB-INSTERM-8-12,
              :QSPB-INSRATE-8-12,
              :QSPB-INSTERM-8-13,
              :QSPB-INSRATE-8-13,
              :QSPB-INSTERM-8-14,
              :QSPB-INSRATE-8-14,
              :QSPB-INSTERM-8-15,
              :QSPB-INSRATE-8-15,
              :QSPB-INSTERM-8-16,
              :QSPB-INSRATE-8-16,
              :QSPB-INSTERM-8-17,
              :QSPB-INSRATE-8-17,
              :QSPB-INSTERM-8-18,
              :QSPB-INSRATE-8-18,
              :QSPB-INSTERM-8-19,
              :QSPB-INSRATE-8-19,
              :QSPB-INSTERM-8-20,
              :QSPB-INSRATE-8-20,
              :QSPB-INSMINPREM-8,
              :QSPB-INSMINCHGABL-8,
              :QSPB-INSMAXCHGABL-8,
              :QSPB-INSMAXBENIF-8,
              :QSPB-INSMAXTERM-8,
              :QSPB-INSCHGBEN-8,
              :QSPB-INSEXCEPT-8,
              :QSPB-INSNOVER-8,
              :QSPB-INSMINAGE-8-1,
              :QSPB-INSMAXAGE-8-1,
              :QSPB-INSMAXCOV-8-1,
              :QSPB-INSMAXTRM-8-1,
              :QSPB-INSMINAGE-8-2,
              :QSPB-INSMAXAGE-8-2,
              :QSPB-INSMAXCOV-8-2,
              :QSPB-INSMAXTRM-8-2,
              :QSPB-INSMINAGE-8-3,
              :QSPB-INSMAXAGE-8-3,
              :QSPB-INSMAXCOV-8-3,
              :QSPB-INSMAXTRM-8-3,
              :QSPB-INSMINAGE-8-4,
              :QSPB-INSMAXAGE-8-4,
              :QSPB-INSMAXCOV-8-4,
              :QSPB-INSMAXTRM-8-4,
              :QSPB-INSMINAGE-8-5,
              :QSPB-INSMAXAGE-8-5,
              :QSPB-INSMAXCOV-8-5,
              :QSPB-INSMAXTRM-8-5,
              :QSPB-INSFEECD-8,
              :QSPB-INSFEE-FRMLA-8,
              :QSPB-INSCHGABL-FG-8,
              :QSPB-INSREJECT-FG-8,
              :QSPB-INSFINCHG-8,
              :QSPB-INSCOM-9,
              :QSPB-INSRATECD-9,
              :QSPB-INSJFAC-9,
              :QSPB-INSDISCNT-9,
              :QSPB-INSCHGABL-9,
              :QSPB-INSTERM-9-1,
              :QSPB-INSRATE-9-1,
              :QSPB-INSTERM-9-2,
              :QSPB-INSRATE-9-2,
              :QSPB-INSTERM-9-3,
              :QSPB-INSRATE-9-3,
              :QSPB-INSTERM-9-4,
              :QSPB-INSRATE-9-4,
              :QSPB-INSTERM-9-5,
              :QSPB-INSRATE-9-5,
              :QSPB-INSTERM-9-6,
              :QSPB-INSRATE-9-6,
              :QSPB-INSTERM-9-7,
              :QSPB-INSRATE-9-7,
              :QSPB-INSTERM-9-8,
              :QSPB-INSRATE-9-8,
              :QSPB-INSTERM-9-9,
              :QSPB-INSRATE-9-9,
              :QSPB-INSTERM-9-10,
              :QSPB-INSRATE-9-10,
              :QSPB-INSTERM-9-11,
              :QSPB-INSRATE-9-11,
              :QSPB-INSTERM-9-12,
              :QSPB-INSRATE-9-12,
              :QSPB-INSTERM-9-13,
              :QSPB-INSRATE-9-13,
              :QSPB-INSTERM-9-14,
              :QSPB-INSRATE-9-14,
              :QSPB-INSTERM-9-15,
              :QSPB-INSRATE-9-15,
              :QSPB-INSTERM-9-16,
              :QSPB-INSRATE-9-16,
              :QSPB-INSTERM-9-17,
              :QSPB-INSRATE-9-17,
              :QSPB-INSTERM-9-18,
              :QSPB-INSRATE-9-18,
              :QSPB-INSTERM-9-19,
              :QSPB-INSRATE-9-19,
              :QSPB-INSTERM-9-20,
              :QSPB-INSRATE-9-20,
              :QSPB-INSMINPREM-9,
              :QSPB-INSMINCHGABL-9,
              :QSPB-INSMAXCHGABL-9,
              :QSPB-INSMAXBENIF-9,
              :QSPB-INSMAXTERM-9,
              :QSPB-INSCHGBEN-9,
              :QSPB-INSEXCEPT-9,
              :QSPB-INSNOVER-9,
              :QSPB-INSMINAGE-9-1,
              :QSPB-INSMAXAGE-9-1,
              :QSPB-INSMAXCOV-9-1,
              :QSPB-INSMAXTRM-9-1,
              :QSPB-INSMINAGE-9-2,
              :QSPB-INSMAXAGE-9-2,
              :QSPB-INSMAXCOV-9-2,
              :QSPB-INSMAXTRM-9-2,
              :QSPB-INSMINAGE-9-3,
              :QSPB-INSMAXAGE-9-3,
              :QSPB-INSMAXCOV-9-3,
              :QSPB-INSMAXTRM-9-3,
              :QSPB-INSMINAGE-9-4,
              :QSPB-INSMAXAGE-9-4,
              :QSPB-INSMAXCOV-9-4,
              :QSPB-INSMAXTRM-9-4,
              :QSPB-INSMINAGE-9-5,
              :QSPB-INSMAXAGE-9-5,
              :QSPB-INSMAXCOV-9-5,
              :QSPB-INSMAXTRM-9-5,
              :QSPB-INSFEECD-9,
              :QSPB-INSFEE-FRMLA-9,
              :QSPB-INSCHGABL-FG-9,
              :QSPB-INSREJECT-FG-9,
              :QSPB-INSFINCHG-9,
              :QSPB-INSCOM-10,
              :QSPB-INSRATECD-10,
              :QSPB-INSJFAC-10,
              :QSPB-INSDISCNT-10,
              :QSPB-INSCHGABL-10,
              :QSPB-INSTERM-10-1,
              :QSPB-INSRATE-10-1,
              :QSPB-INSTERM-10-2,
              :QSPB-INSRATE-10-2,
              :QSPB-INSTERM-10-3,
              :QSPB-INSRATE-10-3,
              :QSPB-INSTERM-10-4,
              :QSPB-INSRATE-10-4,
              :QSPB-INSTERM-10-5,
              :QSPB-INSRATE-10-5,
              :QSPB-INSTERM-10-6,
              :QSPB-INSRATE-10-6,
              :QSPB-INSTERM-10-7,
              :QSPB-INSRATE-10-7,
              :QSPB-INSTERM-10-8,
              :QSPB-INSRATE-10-8,
              :QSPB-INSTERM-10-9,
              :QSPB-INSRATE-10-9,
              :QSPB-INSTERM-10-10,
              :QSPB-INSRATE-10-10,
              :QSPB-INSTERM-10-11,
              :QSPB-INSRATE-10-11,
              :QSPB-INSTERM-10-12,
              :QSPB-INSRATE-10-12,
              :QSPB-INSTERM-10-13,
              :QSPB-INSRATE-10-13,
              :QSPB-INSTERM-10-14,
              :QSPB-INSRATE-10-14,
              :QSPB-INSTERM-10-15,
              :QSPB-INSRATE-10-15,
              :QSPB-INSTERM-10-16,
              :QSPB-INSRATE-10-16,
              :QSPB-INSTERM-10-17,
              :QSPB-INSRATE-10-17,
              :QSPB-INSTERM-10-18,
              :QSPB-INSRATE-10-18,
              :QSPB-INSTERM-10-19,
              :QSPB-INSRATE-10-19,
              :QSPB-INSTERM-10-20,
              :QSPB-INSRATE-10-20,
              :QSPB-INSMINPREM-10,
              :QSPB-INSMINCHGABL-10,
              :QSPB-INSMAXCHGABL-10,
              :QSPB-INSMAXBENIF-10,
              :QSPB-INSMAXTERM-10,
              :QSPB-INSCHGBEN-10,
              :QSPB-INSEXCEPT-10,
              :QSPB-INSNOVER-10,
              :QSPB-INSMINAGE-10-1,
              :QSPB-INSMAXAGE-10-1,
              :QSPB-INSMAXCOV-10-1,
              :QSPB-INSMAXTRM-10-1,
              :QSPB-INSMINAGE-10-2,
              :QSPB-INSMAXAGE-10-2,
              :QSPB-INSMAXCOV-10-2,
              :QSPB-INSMAXTRM-10-2,
              :QSPB-INSMINAGE-10-3,
              :QSPB-INSMAXAGE-10-3,
              :QSPB-INSMAXCOV-10-3,
              :QSPB-INSMAXTRM-10-3,
              :QSPB-INSMINAGE-10-4,
              :QSPB-INSMAXAGE-10-4,
              :QSPB-INSMAXCOV-10-4,
              :QSPB-INSMAXTRM-10-4,
              :QSPB-INSMINAGE-10-5,
              :QSPB-INSMAXAGE-10-5,
              :QSPB-INSMAXCOV-10-5,
              :QSPB-INSMAXTRM-10-5,
              :QSPB-INSFEECD-10,
              :QSPB-INSFEE-FRMLA-10,
              :QSPB-INSCHGABL-FG-10,
              :QSPB-INSREJECT-FG-10,
              :QSPB-INSFINCHG-10,
              :QSPB-INSCOM-11,
              :QSPB-INSRATECD-11,
              :QSPB-INSJFAC-11,
              :QSPB-INSDISCNT-11,
              :QSPB-INSCHGABL-11,
              :QSPB-INSTERM-11-1,
              :QSPB-INSRATE-11-1,
              :QSPB-INSTERM-11-2,
              :QSPB-INSRATE-11-2,
              :QSPB-INSTERM-11-3,
              :QSPB-INSRATE-11-3,
              :QSPB-INSTERM-11-4,
              :QSPB-INSRATE-11-4,
              :QSPB-INSTERM-11-5,
              :QSPB-INSRATE-11-5,
              :QSPB-INSTERM-11-6,
              :QSPB-INSRATE-11-6,
              :QSPB-INSTERM-11-7,
              :QSPB-INSRATE-11-7,
              :QSPB-INSTERM-11-8,
              :QSPB-INSRATE-11-8,
              :QSPB-INSTERM-11-9,
              :QSPB-INSRATE-11-9,
              :QSPB-INSTERM-11-10,
              :QSPB-INSRATE-11-10,
              :QSPB-INSTERM-11-11,
              :QSPB-INSRATE-11-11,
              :QSPB-INSTERM-11-12,
              :QSPB-INSRATE-11-12,
              :QSPB-INSTERM-11-13,
              :QSPB-INSRATE-11-13,
              :QSPB-INSTERM-11-14,
              :QSPB-INSRATE-11-14,
              :QSPB-INSTERM-11-15,
              :QSPB-INSRATE-11-15,
              :QSPB-INSTERM-11-16,
              :QSPB-INSRATE-11-16,
              :QSPB-INSTERM-11-17,
              :QSPB-INSRATE-11-17,
              :QSPB-INSTERM-11-18,
              :QSPB-INSRATE-11-18,
              :QSPB-INSTERM-11-19,
              :QSPB-INSRATE-11-19,
              :QSPB-INSTERM-11-20,
              :QSPB-INSRATE-11-20,
              :QSPB-INSMINPREM-11,
              :QSPB-INSMINCHGABL-11,
              :QSPB-INSMAXCHGABL-11,
              :QSPB-INSMAXBENIF-11,
              :QSPB-INSMAXTERM-11,
              :QSPB-INSCHGBEN-11,
              :QSPB-INSEXCEPT-11,
              :QSPB-INSNOVER-11,
              :QSPB-INSMINAGE-11-1,
              :QSPB-INSMAXAGE-11-1,
              :QSPB-INSMAXCOV-11-1,
              :QSPB-INSMAXTRM-11-1,
              :QSPB-INSMINAGE-11-2,
              :QSPB-INSMAXAGE-11-2,
              :QSPB-INSMAXCOV-11-2,
              :QSPB-INSMAXTRM-11-2,
              :QSPB-INSMINAGE-11-3,
              :QSPB-INSMAXAGE-11-3,
              :QSPB-INSMAXCOV-11-3,
              :QSPB-INSMAXTRM-11-3,
              :QSPB-INSMINAGE-11-4,
              :QSPB-INSMAXAGE-11-4,
              :QSPB-INSMAXCOV-11-4,
              :QSPB-INSMAXTRM-11-4,
              :QSPB-INSMINAGE-11-5,
              :QSPB-INSMAXAGE-11-5,
              :QSPB-INSMAXCOV-11-5,
              :QSPB-INSMAXTRM-11-5,
              :QSPB-INSFEECD-11,
              :QSPB-INSFEE-FRMLA-11,
              :QSPB-INSCHGABL-FG-11,
              :QSPB-INSREJECT-FG-11,
              :QSPB-INSFINCHG-11,
              :QSPB-INSCOM-12,
              :QSPB-INSRATECD-12,
              :QSPB-INSJFAC-12,
              :QSPB-INSDISCNT-12,
              :QSPB-INSCHGABL-12,
              :QSPB-INSTERM-12-1,
              :QSPB-INSRATE-12-1,
              :QSPB-INSTERM-12-2,
              :QSPB-INSRATE-12-2,
              :QSPB-INSTERM-12-3,
              :QSPB-INSRATE-12-3,
              :QSPB-INSTERM-12-4,
              :QSPB-INSRATE-12-4,
              :QSPB-INSTERM-12-5,
              :QSPB-INSRATE-12-5,
              :QSPB-INSTERM-12-6,
              :QSPB-INSRATE-12-6,
              :QSPB-INSTERM-12-7,
              :QSPB-INSRATE-12-7,
              :QSPB-INSTERM-12-8,
              :QSPB-INSRATE-12-8,
              :QSPB-INSTERM-12-9,
              :QSPB-INSRATE-12-9,
              :QSPB-INSTERM-12-10,
              :QSPB-INSRATE-12-10,
              :QSPB-INSTERM-12-11,
              :QSPB-INSRATE-12-11,
              :QSPB-INSTERM-12-12,
              :QSPB-INSRATE-12-12,
              :QSPB-INSTERM-12-13,
              :QSPB-INSRATE-12-13,
              :QSPB-INSTERM-12-14,
              :QSPB-INSRATE-12-14,
              :QSPB-INSTERM-12-15,
              :QSPB-INSRATE-12-15,
              :QSPB-INSTERM-12-16,
              :QSPB-INSRATE-12-16,
              :QSPB-INSTERM-12-17,
              :QSPB-INSRATE-12-17,
              :QSPB-INSTERM-12-18,
              :QSPB-INSRATE-12-18,
              :QSPB-INSTERM-12-19,
              :QSPB-INSRATE-12-19,
              :QSPB-INSTERM-12-20,
              :QSPB-INSRATE-12-20,
              :QSPB-INSMINPREM-12,
              :QSPB-INSMINCHGABL-12,
              :QSPB-INSMAXCHGABL-12,
              :QSPB-INSMAXBENIF-12,
              :QSPB-INSMAXTERM-12,
              :QSPB-INSCHGBEN-12,
              :QSPB-INSEXCEPT-12,
              :QSPB-INSNOVER-12,
              :QSPB-INSMINAGE-12-1,
              :QSPB-INSMAXAGE-12-1,
              :QSPB-INSMAXCOV-12-1,
              :QSPB-INSMAXTRM-12-1,
              :QSPB-INSMINAGE-12-2,
              :QSPB-INSMAXAGE-12-2,
              :QSPB-INSMAXCOV-12-2,
              :QSPB-INSMAXTRM-12-2,
              :QSPB-INSMINAGE-12-3,
              :QSPB-INSMAXAGE-12-3,
              :QSPB-INSMAXCOV-12-3,
              :QSPB-INSMAXTRM-12-3,
              :QSPB-INSMINAGE-12-4,
              :QSPB-INSMAXAGE-12-4,
              :QSPB-INSMAXCOV-12-4,
              :QSPB-INSMAXTRM-12-4,
              :QSPB-INSMINAGE-12-5,
              :QSPB-INSMAXAGE-12-5,
              :QSPB-INSMAXCOV-12-5,
              :QSPB-INSMAXTRM-12-5,
              :QSPB-INSFEECD-12,
              :QSPB-INSFEE-FRMLA-12,
              :QSPB-INSCHGABL-FG-12,
              :QSPB-INSREJECT-FG-12,
              :QSPB-INSFINCHG-12)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-SPB1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-------------------------------------------------------
       REWRITE-SPB1-FILE.

           PERFORM REWRITE-IT.
           MOVE SPB-SELECT-PATH TO E-FILE.
           MOVE SPB1-KEY        TO E-KEYX.
           PERFORM SET-SPB-FIELDS.

           EXEC SQL
            UPDATE DBO.SPBFILE
             SET
              SPBFILE.SPB_ORGST = :QSPB-ORGST,
              SPBFILE.SPB_SPRCLASS = :QSPB-SPRCLASS,
              SPBFILE.SPB_SUBCLASS = :QSPB-SUBCLASS,
              SPBFILE.SPB_LAWCODE = :QSPB-LAWCODE,
              SPBFILE.SPB_INSCOM_1 = :QSPB-INSCOM-1,
              SPBFILE.SPB_INSRATECD_1 = :QSPB-INSRATECD-1,
              SPBFILE.SPB_INSJFAC_1 = :QSPB-INSJFAC-1,
              SPBFILE.SPB_INSDISCNT_1 = :QSPB-INSDISCNT-1,
              SPBFILE.SPB_INSCHGABL_1 = :QSPB-INSCHGABL-1,
              SPBFILE.SPB_INSTERM_1_1 = :QSPB-INSTERM-1-1,
              SPBFILE.SPB_INSRATE_1_1 = :QSPB-INSRATE-1-1,
              SPBFILE.SPB_INSTERM_1_2 = :QSPB-INSTERM-1-2,
              SPBFILE.SPB_INSRATE_1_2 = :QSPB-INSRATE-1-2,
              SPBFILE.SPB_INSTERM_1_3 = :QSPB-INSTERM-1-3,
              SPBFILE.SPB_INSRATE_1_3 = :QSPB-INSRATE-1-3,
              SPBFILE.SPB_INSTERM_1_4 = :QSPB-INSTERM-1-4,
              SPBFILE.SPB_INSRATE_1_4 = :QSPB-INSRATE-1-4,
              SPBFILE.SPB_INSTERM_1_5 = :QSPB-INSTERM-1-5,
              SPBFILE.SPB_INSRATE_1_5 = :QSPB-INSRATE-1-5,
              SPBFILE.SPB_INSTERM_1_6 = :QSPB-INSTERM-1-6,
              SPBFILE.SPB_INSRATE_1_6 = :QSPB-INSRATE-1-6,
              SPBFILE.SPB_INSTERM_1_7 = :QSPB-INSTERM-1-7,
              SPBFILE.SPB_INSRATE_1_7 = :QSPB-INSRATE-1-7,
              SPBFILE.SPB_INSTERM_1_8 = :QSPB-INSTERM-1-8,
              SPBFILE.SPB_INSRATE_1_8 = :QSPB-INSRATE-1-8,
              SPBFILE.SPB_INSTERM_1_9 = :QSPB-INSTERM-1-9,
              SPBFILE.SPB_INSRATE_1_9 = :QSPB-INSRATE-1-9,
              SPBFILE.SPB_INSTERM_1_10 = :QSPB-INSTERM-1-10,
              SPBFILE.SPB_INSRATE_1_10 = :QSPB-INSRATE-1-10,
              SPBFILE.SPB_INSTERM_1_11 = :QSPB-INSTERM-1-11,
              SPBFILE.SPB_INSRATE_1_11 = :QSPB-INSRATE-1-11,
              SPBFILE.SPB_INSTERM_1_12 = :QSPB-INSTERM-1-12,
              SPBFILE.SPB_INSRATE_1_12 = :QSPB-INSRATE-1-12,
              SPBFILE.SPB_INSTERM_1_13 = :QSPB-INSTERM-1-13,
              SPBFILE.SPB_INSRATE_1_13 = :QSPB-INSRATE-1-13,
              SPBFILE.SPB_INSTERM_1_14 = :QSPB-INSTERM-1-14,
              SPBFILE.SPB_INSRATE_1_14 = :QSPB-INSRATE-1-14,
              SPBFILE.SPB_INSTERM_1_15 = :QSPB-INSTERM-1-15,
              SPBFILE.SPB_INSRATE_1_15 = :QSPB-INSRATE-1-15,
              SPBFILE.SPB_INSTERM_1_16 = :QSPB-INSTERM-1-16,
              SPBFILE.SPB_INSRATE_1_16 = :QSPB-INSRATE-1-16,
              SPBFILE.SPB_INSTERM_1_17 = :QSPB-INSTERM-1-17,
              SPBFILE.SPB_INSRATE_1_17 = :QSPB-INSRATE-1-17,
              SPBFILE.SPB_INSTERM_1_18 = :QSPB-INSTERM-1-18,
              SPBFILE.SPB_INSRATE_1_18 = :QSPB-INSRATE-1-18,
              SPBFILE.SPB_INSTERM_1_19 = :QSPB-INSTERM-1-19,
              SPBFILE.SPB_INSRATE_1_19 = :QSPB-INSRATE-1-19,
              SPBFILE.SPB_INSTERM_1_20 = :QSPB-INSTERM-1-20,
              SPBFILE.SPB_INSRATE_1_20 = :QSPB-INSRATE-1-20,
              SPBFILE.SPB_INSMINPREM_1 = :QSPB-INSMINPREM-1,
              SPBFILE.SPB_INSMINCHGABL_1 = :QSPB-INSMINCHGABL-1,
              SPBFILE.SPB_INSMAXCHGABL_1 = :QSPB-INSMAXCHGABL-1,
              SPBFILE.SPB_INSMAXBENIF_1 = :QSPB-INSMAXBENIF-1,
              SPBFILE.SPB_INSMAXTERM_1 = :QSPB-INSMAXTERM-1,
              SPBFILE.SPB_INSCHGBEN_1 = :QSPB-INSCHGBEN-1,
              SPBFILE.SPB_INSEXCEPT_1 = :QSPB-INSEXCEPT-1,
              SPBFILE.SPB_INSNOVER_1 = :QSPB-INSNOVER-1,
              SPBFILE.SPB_INSMINAGE_1_1 = :QSPB-INSMINAGE-1-1,
              SPBFILE.SPB_INSMAXAGE_1_1 = :QSPB-INSMAXAGE-1-1,
              SPBFILE.SPB_INSMAXCOV_1_1 = :QSPB-INSMAXCOV-1-1,
              SPBFILE.SPB_INSMAXTRM_1_1 = :QSPB-INSMAXTRM-1-1,
              SPBFILE.SPB_INSMINAGE_1_2 = :QSPB-INSMINAGE-1-2,
              SPBFILE.SPB_INSMAXAGE_1_2 = :QSPB-INSMAXAGE-1-2,
              SPBFILE.SPB_INSMAXCOV_1_2 = :QSPB-INSMAXCOV-1-2,
              SPBFILE.SPB_INSMAXTRM_1_2 = :QSPB-INSMAXTRM-1-2,
              SPBFILE.SPB_INSMINAGE_1_3 = :QSPB-INSMINAGE-1-3,
              SPBFILE.SPB_INSMAXAGE_1_3 = :QSPB-INSMAXAGE-1-3,
              SPBFILE.SPB_INSMAXCOV_1_3 = :QSPB-INSMAXCOV-1-3,
              SPBFILE.SPB_INSMAXTRM_1_3 = :QSPB-INSMAXTRM-1-3,
              SPBFILE.SPB_INSMINAGE_1_4 = :QSPB-INSMINAGE-1-4,
              SPBFILE.SPB_INSMAXAGE_1_4 = :QSPB-INSMAXAGE-1-4,
              SPBFILE.SPB_INSMAXCOV_1_4 = :QSPB-INSMAXCOV-1-4,
              SPBFILE.SPB_INSMAXTRM_1_4 = :QSPB-INSMAXTRM-1-4,
              SPBFILE.SPB_INSMINAGE_1_5 = :QSPB-INSMINAGE-1-5,
              SPBFILE.SPB_INSMAXAGE_1_5 = :QSPB-INSMAXAGE-1-5,
              SPBFILE.SPB_INSMAXCOV_1_5 = :QSPB-INSMAXCOV-1-5,
              SPBFILE.SPB_INSMAXTRM_1_5 = :QSPB-INSMAXTRM-1-5,
              SPBFILE.SPB_INSFEECD_1 = :QSPB-INSFEECD-1,
              SPBFILE.SPB_INSFEE_FRMLA_1 = :QSPB-INSFEE-FRMLA-1,
              SPBFILE.SPB_INSCHGABL_FG_1 = :QSPB-INSCHGABL-FG-1,
              SPBFILE.SPB_INSREJECT_FG_1 = :QSPB-INSREJECT-FG-1,
              SPBFILE.SPB_INSFINCHG_1 = :QSPB-INSFINCHG-1,
              SPBFILE.SPB_INSCOM_2 = :QSPB-INSCOM-2,
              SPBFILE.SPB_INSRATECD_2 = :QSPB-INSRATECD-2,
              SPBFILE.SPB_INSJFAC_2 = :QSPB-INSJFAC-2,
              SPBFILE.SPB_INSDISCNT_2 = :QSPB-INSDISCNT-2,
              SPBFILE.SPB_INSCHGABL_2 = :QSPB-INSCHGABL-2,
              SPBFILE.SPB_INSTERM_2_1 = :QSPB-INSTERM-2-1,
              SPBFILE.SPB_INSRATE_2_1 = :QSPB-INSRATE-2-1,
              SPBFILE.SPB_INSTERM_2_2 = :QSPB-INSTERM-2-2,
              SPBFILE.SPB_INSRATE_2_2 = :QSPB-INSRATE-2-2,
              SPBFILE.SPB_INSTERM_2_3 = :QSPB-INSTERM-2-3,
              SPBFILE.SPB_INSRATE_2_3 = :QSPB-INSRATE-2-3,
              SPBFILE.SPB_INSTERM_2_4 = :QSPB-INSTERM-2-4,
              SPBFILE.SPB_INSRATE_2_4 = :QSPB-INSRATE-2-4,
              SPBFILE.SPB_INSTERM_2_5 = :QSPB-INSTERM-2-5,
              SPBFILE.SPB_INSRATE_2_5 = :QSPB-INSRATE-2-5,
              SPBFILE.SPB_INSTERM_2_6 = :QSPB-INSTERM-2-6,
              SPBFILE.SPB_INSRATE_2_6 = :QSPB-INSRATE-2-6,
              SPBFILE.SPB_INSTERM_2_7 = :QSPB-INSTERM-2-7,
              SPBFILE.SPB_INSRATE_2_7 = :QSPB-INSRATE-2-7,
              SPBFILE.SPB_INSTERM_2_8 = :QSPB-INSTERM-2-8,
              SPBFILE.SPB_INSRATE_2_8 = :QSPB-INSRATE-2-8,
              SPBFILE.SPB_INSTERM_2_9 = :QSPB-INSTERM-2-9,
              SPBFILE.SPB_INSRATE_2_9 = :QSPB-INSRATE-2-9,
              SPBFILE.SPB_INSTERM_2_10 = :QSPB-INSTERM-2-10,
              SPBFILE.SPB_INSRATE_2_10 = :QSPB-INSRATE-2-10,
              SPBFILE.SPB_INSTERM_2_11 = :QSPB-INSTERM-2-11,
              SPBFILE.SPB_INSRATE_2_11 = :QSPB-INSRATE-2-11,
              SPBFILE.SPB_INSTERM_2_12 = :QSPB-INSTERM-2-12,
              SPBFILE.SPB_INSRATE_2_12 = :QSPB-INSRATE-2-12,
              SPBFILE.SPB_INSTERM_2_13 = :QSPB-INSTERM-2-13,
              SPBFILE.SPB_INSRATE_2_13 = :QSPB-INSRATE-2-13,
              SPBFILE.SPB_INSTERM_2_14 = :QSPB-INSTERM-2-14,
              SPBFILE.SPB_INSRATE_2_14 = :QSPB-INSRATE-2-14,
              SPBFILE.SPB_INSTERM_2_15 = :QSPB-INSTERM-2-15,
              SPBFILE.SPB_INSRATE_2_15 = :QSPB-INSRATE-2-15,
              SPBFILE.SPB_INSTERM_2_16 = :QSPB-INSTERM-2-16,
              SPBFILE.SPB_INSRATE_2_16 = :QSPB-INSRATE-2-16,
              SPBFILE.SPB_INSTERM_2_17 = :QSPB-INSTERM-2-17,
              SPBFILE.SPB_INSRATE_2_17 = :QSPB-INSRATE-2-17,
              SPBFILE.SPB_INSTERM_2_18 = :QSPB-INSTERM-2-18,
              SPBFILE.SPB_INSRATE_2_18 = :QSPB-INSRATE-2-18,
              SPBFILE.SPB_INSTERM_2_19 = :QSPB-INSTERM-2-19,
              SPBFILE.SPB_INSRATE_2_19 = :QSPB-INSRATE-2-19,
              SPBFILE.SPB_INSTERM_2_20 = :QSPB-INSTERM-2-20,
              SPBFILE.SPB_INSRATE_2_20 = :QSPB-INSRATE-2-20,
              SPBFILE.SPB_INSMINPREM_2 = :QSPB-INSMINPREM-2,
              SPBFILE.SPB_INSMINCHGABL_2 = :QSPB-INSMINCHGABL-2,
              SPBFILE.SPB_INSMAXCHGABL_2 = :QSPB-INSMAXCHGABL-2,
              SPBFILE.SPB_INSMAXBENIF_2 = :QSPB-INSMAXBENIF-2,
              SPBFILE.SPB_INSMAXTERM_2 = :QSPB-INSMAXTERM-2,
              SPBFILE.SPB_INSCHGBEN_2 = :QSPB-INSCHGBEN-2,
              SPBFILE.SPB_INSEXCEPT_2 = :QSPB-INSEXCEPT-2,
              SPBFILE.SPB_INSNOVER_2 = :QSPB-INSNOVER-2,
              SPBFILE.SPB_INSMINAGE_2_1 = :QSPB-INSMINAGE-2-1,
              SPBFILE.SPB_INSMAXAGE_2_1 = :QSPB-INSMAXAGE-2-1,
              SPBFILE.SPB_INSMAXCOV_2_1 = :QSPB-INSMAXCOV-2-1,
              SPBFILE.SPB_INSMAXTRM_2_1 = :QSPB-INSMAXTRM-2-1,
              SPBFILE.SPB_INSMINAGE_2_2 = :QSPB-INSMINAGE-2-2,
              SPBFILE.SPB_INSMAXAGE_2_2 = :QSPB-INSMAXAGE-2-2,
              SPBFILE.SPB_INSMAXCOV_2_2 = :QSPB-INSMAXCOV-2-2,
              SPBFILE.SPB_INSMAXTRM_2_2 = :QSPB-INSMAXTRM-2-2,
              SPBFILE.SPB_INSMINAGE_2_3 = :QSPB-INSMINAGE-2-3,
              SPBFILE.SPB_INSMAXAGE_2_3 = :QSPB-INSMAXAGE-2-3,
              SPBFILE.SPB_INSMAXCOV_2_3 = :QSPB-INSMAXCOV-2-3,
              SPBFILE.SPB_INSMAXTRM_2_3 = :QSPB-INSMAXTRM-2-3,
              SPBFILE.SPB_INSMINAGE_2_4 = :QSPB-INSMINAGE-2-4,
              SPBFILE.SPB_INSMAXAGE_2_4 = :QSPB-INSMAXAGE-2-4,
              SPBFILE.SPB_INSMAXCOV_2_4 = :QSPB-INSMAXCOV-2-4,
              SPBFILE.SPB_INSMAXTRM_2_4 = :QSPB-INSMAXTRM-2-4,
              SPBFILE.SPB_INSMINAGE_2_5 = :QSPB-INSMINAGE-2-5,
              SPBFILE.SPB_INSMAXAGE_2_5 = :QSPB-INSMAXAGE-2-5,
              SPBFILE.SPB_INSMAXCOV_2_5 = :QSPB-INSMAXCOV-2-5,
              SPBFILE.SPB_INSMAXTRM_2_5 = :QSPB-INSMAXTRM-2-5,
              SPBFILE.SPB_INSFEECD_2 = :QSPB-INSFEECD-2,
              SPBFILE.SPB_INSFEE_FRMLA_2 = :QSPB-INSFEE-FRMLA-2,
              SPBFILE.SPB_INSCHGABL_FG_2 = :QSPB-INSCHGABL-FG-2,
              SPBFILE.SPB_INSREJECT_FG_2 = :QSPB-INSREJECT-FG-2,
              SPBFILE.SPB_INSFINCHG_2 = :QSPB-INSFINCHG-2,
              SPBFILE.SPB_INSCOM_3 = :QSPB-INSCOM-3,
              SPBFILE.SPB_INSRATECD_3 = :QSPB-INSRATECD-3,
              SPBFILE.SPB_INSJFAC_3 = :QSPB-INSJFAC-3,
              SPBFILE.SPB_INSDISCNT_3 = :QSPB-INSDISCNT-3,
              SPBFILE.SPB_INSCHGABL_3 = :QSPB-INSCHGABL-3,
              SPBFILE.SPB_INSTERM_3_1 = :QSPB-INSTERM-3-1,
              SPBFILE.SPB_INSRATE_3_1 = :QSPB-INSRATE-3-1,
              SPBFILE.SPB_INSTERM_3_2 = :QSPB-INSTERM-3-2,
              SPBFILE.SPB_INSRATE_3_2 = :QSPB-INSRATE-3-2,
              SPBFILE.SPB_INSTERM_3_3 = :QSPB-INSTERM-3-3,
              SPBFILE.SPB_INSRATE_3_3 = :QSPB-INSRATE-3-3,
              SPBFILE.SPB_INSTERM_3_4 = :QSPB-INSTERM-3-4,
              SPBFILE.SPB_INSRATE_3_4 = :QSPB-INSRATE-3-4,
              SPBFILE.SPB_INSTERM_3_5 = :QSPB-INSTERM-3-5,
              SPBFILE.SPB_INSRATE_3_5 = :QSPB-INSRATE-3-5,
              SPBFILE.SPB_INSTERM_3_6 = :QSPB-INSTERM-3-6,
              SPBFILE.SPB_INSRATE_3_6 = :QSPB-INSRATE-3-6,
              SPBFILE.SPB_INSTERM_3_7 = :QSPB-INSTERM-3-7,
              SPBFILE.SPB_INSRATE_3_7 = :QSPB-INSRATE-3-7,
              SPBFILE.SPB_INSTERM_3_8 = :QSPB-INSTERM-3-8,
              SPBFILE.SPB_INSRATE_3_8 = :QSPB-INSRATE-3-8,
              SPBFILE.SPB_INSTERM_3_9 = :QSPB-INSTERM-3-9,
              SPBFILE.SPB_INSRATE_3_9 = :QSPB-INSRATE-3-9,
              SPBFILE.SPB_INSTERM_3_10 = :QSPB-INSTERM-3-10,
              SPBFILE.SPB_INSRATE_3_10 = :QSPB-INSRATE-3-10,
              SPBFILE.SPB_INSTERM_3_11 = :QSPB-INSTERM-3-11,
              SPBFILE.SPB_INSRATE_3_11 = :QSPB-INSRATE-3-11,
              SPBFILE.SPB_INSTERM_3_12 = :QSPB-INSTERM-3-12,
              SPBFILE.SPB_INSRATE_3_12 = :QSPB-INSRATE-3-12,
              SPBFILE.SPB_INSTERM_3_13 = :QSPB-INSTERM-3-13,
              SPBFILE.SPB_INSRATE_3_13 = :QSPB-INSRATE-3-13,
              SPBFILE.SPB_INSTERM_3_14 = :QSPB-INSTERM-3-14,
              SPBFILE.SPB_INSRATE_3_14 = :QSPB-INSRATE-3-14,
              SPBFILE.SPB_INSTERM_3_15 = :QSPB-INSTERM-3-15,
              SPBFILE.SPB_INSRATE_3_15 = :QSPB-INSRATE-3-15,
              SPBFILE.SPB_INSTERM_3_16 = :QSPB-INSTERM-3-16,
              SPBFILE.SPB_INSRATE_3_16 = :QSPB-INSRATE-3-16,
              SPBFILE.SPB_INSTERM_3_17 = :QSPB-INSTERM-3-17,
              SPBFILE.SPB_INSRATE_3_17 = :QSPB-INSRATE-3-17,
              SPBFILE.SPB_INSTERM_3_18 = :QSPB-INSTERM-3-18,
              SPBFILE.SPB_INSRATE_3_18 = :QSPB-INSRATE-3-18,
              SPBFILE.SPB_INSTERM_3_19 = :QSPB-INSTERM-3-19,
              SPBFILE.SPB_INSRATE_3_19 = :QSPB-INSRATE-3-19,
              SPBFILE.SPB_INSTERM_3_20 = :QSPB-INSTERM-3-20,
              SPBFILE.SPB_INSRATE_3_20 = :QSPB-INSRATE-3-20,
              SPBFILE.SPB_INSMINPREM_3 = :QSPB-INSMINPREM-3,
              SPBFILE.SPB_INSMINCHGABL_3 = :QSPB-INSMINCHGABL-3,
              SPBFILE.SPB_INSMAXCHGABL_3 = :QSPB-INSMAXCHGABL-3,
              SPBFILE.SPB_INSMAXBENIF_3 = :QSPB-INSMAXBENIF-3,
              SPBFILE.SPB_INSMAXTERM_3 = :QSPB-INSMAXTERM-3,
              SPBFILE.SPB_INSCHGBEN_3 = :QSPB-INSCHGBEN-3,
              SPBFILE.SPB_INSEXCEPT_3 = :QSPB-INSEXCEPT-3,
              SPBFILE.SPB_INSNOVER_3 = :QSPB-INSNOVER-3,
              SPBFILE.SPB_INSMINAGE_3_1 = :QSPB-INSMINAGE-3-1,
              SPBFILE.SPB_INSMAXAGE_3_1 = :QSPB-INSMAXAGE-3-1,
              SPBFILE.SPB_INSMAXCOV_3_1 = :QSPB-INSMAXCOV-3-1,
              SPBFILE.SPB_INSMAXTRM_3_1 = :QSPB-INSMAXTRM-3-1,
              SPBFILE.SPB_INSMINAGE_3_2 = :QSPB-INSMINAGE-3-2,
              SPBFILE.SPB_INSMAXAGE_3_2 = :QSPB-INSMAXAGE-3-2,
              SPBFILE.SPB_INSMAXCOV_3_2 = :QSPB-INSMAXCOV-3-2,
              SPBFILE.SPB_INSMAXTRM_3_2 = :QSPB-INSMAXTRM-3-2,
              SPBFILE.SPB_INSMINAGE_3_3 = :QSPB-INSMINAGE-3-3,
              SPBFILE.SPB_INSMAXAGE_3_3 = :QSPB-INSMAXAGE-3-3,
              SPBFILE.SPB_INSMAXCOV_3_3 = :QSPB-INSMAXCOV-3-3,
              SPBFILE.SPB_INSMAXTRM_3_3 = :QSPB-INSMAXTRM-3-3,
              SPBFILE.SPB_INSMINAGE_3_4 = :QSPB-INSMINAGE-3-4,
              SPBFILE.SPB_INSMAXAGE_3_4 = :QSPB-INSMAXAGE-3-4,
              SPBFILE.SPB_INSMAXCOV_3_4 = :QSPB-INSMAXCOV-3-4,
              SPBFILE.SPB_INSMAXTRM_3_4 = :QSPB-INSMAXTRM-3-4,
              SPBFILE.SPB_INSMINAGE_3_5 = :QSPB-INSMINAGE-3-5,
              SPBFILE.SPB_INSMAXAGE_3_5 = :QSPB-INSMAXAGE-3-5,
              SPBFILE.SPB_INSMAXCOV_3_5 = :QSPB-INSMAXCOV-3-5,
              SPBFILE.SPB_INSMAXTRM_3_5 = :QSPB-INSMAXTRM-3-5,
              SPBFILE.SPB_INSFEECD_3 = :QSPB-INSFEECD-3,
              SPBFILE.SPB_INSFEE_FRMLA_3 = :QSPB-INSFEE-FRMLA-3,
              SPBFILE.SPB_INSCHGABL_FG_3 = :QSPB-INSCHGABL-FG-3,
              SPBFILE.SPB_INSREJECT_FG_3 = :QSPB-INSREJECT-FG-3,
              SPBFILE.SPB_INSFINCHG_3 = :QSPB-INSFINCHG-3,
              SPBFILE.SPB_INSCOM_4 = :QSPB-INSCOM-4,
              SPBFILE.SPB_INSRATECD_4 = :QSPB-INSRATECD-4,
              SPBFILE.SPB_INSJFAC_4 = :QSPB-INSJFAC-4,
              SPBFILE.SPB_INSDISCNT_4 = :QSPB-INSDISCNT-4,
              SPBFILE.SPB_INSCHGABL_4 = :QSPB-INSCHGABL-4,
              SPBFILE.SPB_INSTERM_4_1 = :QSPB-INSTERM-4-1,
              SPBFILE.SPB_INSRATE_4_1 = :QSPB-INSRATE-4-1,
              SPBFILE.SPB_INSTERM_4_2 = :QSPB-INSTERM-4-2,
              SPBFILE.SPB_INSRATE_4_2 = :QSPB-INSRATE-4-2,
              SPBFILE.SPB_INSTERM_4_3 = :QSPB-INSTERM-4-3,
              SPBFILE.SPB_INSRATE_4_3 = :QSPB-INSRATE-4-3,
              SPBFILE.SPB_INSTERM_4_4 = :QSPB-INSTERM-4-4,
              SPBFILE.SPB_INSRATE_4_4 = :QSPB-INSRATE-4-4,
              SPBFILE.SPB_INSTERM_4_5 = :QSPB-INSTERM-4-5,
              SPBFILE.SPB_INSRATE_4_5 = :QSPB-INSRATE-4-5,
              SPBFILE.SPB_INSTERM_4_6 = :QSPB-INSTERM-4-6,
              SPBFILE.SPB_INSRATE_4_6 = :QSPB-INSRATE-4-6,
              SPBFILE.SPB_INSTERM_4_7 = :QSPB-INSTERM-4-7,
              SPBFILE.SPB_INSRATE_4_7 = :QSPB-INSRATE-4-7,
              SPBFILE.SPB_INSTERM_4_8 = :QSPB-INSTERM-4-8,
              SPBFILE.SPB_INSRATE_4_8 = :QSPB-INSRATE-4-8,
              SPBFILE.SPB_INSTERM_4_9 = :QSPB-INSTERM-4-9,
              SPBFILE.SPB_INSRATE_4_9 = :QSPB-INSRATE-4-9,
              SPBFILE.SPB_INSTERM_4_10 = :QSPB-INSTERM-4-10,
              SPBFILE.SPB_INSRATE_4_10 = :QSPB-INSRATE-4-10,
              SPBFILE.SPB_INSTERM_4_11 = :QSPB-INSTERM-4-11,
              SPBFILE.SPB_INSRATE_4_11 = :QSPB-INSRATE-4-11,
              SPBFILE.SPB_INSTERM_4_12 = :QSPB-INSTERM-4-12,
              SPBFILE.SPB_INSRATE_4_12 = :QSPB-INSRATE-4-12,
              SPBFILE.SPB_INSTERM_4_13 = :QSPB-INSTERM-4-13,
              SPBFILE.SPB_INSRATE_4_13 = :QSPB-INSRATE-4-13,
              SPBFILE.SPB_INSTERM_4_14 = :QSPB-INSTERM-4-14,
              SPBFILE.SPB_INSRATE_4_14 = :QSPB-INSRATE-4-14,
              SPBFILE.SPB_INSTERM_4_15 = :QSPB-INSTERM-4-15,
              SPBFILE.SPB_INSRATE_4_15 = :QSPB-INSRATE-4-15,
              SPBFILE.SPB_INSTERM_4_16 = :QSPB-INSTERM-4-16,
              SPBFILE.SPB_INSRATE_4_16 = :QSPB-INSRATE-4-16,
              SPBFILE.SPB_INSTERM_4_17 = :QSPB-INSTERM-4-17,
              SPBFILE.SPB_INSRATE_4_17 = :QSPB-INSRATE-4-17,
              SPBFILE.SPB_INSTERM_4_18 = :QSPB-INSTERM-4-18,
              SPBFILE.SPB_INSRATE_4_18 = :QSPB-INSRATE-4-18,
              SPBFILE.SPB_INSTERM_4_19 = :QSPB-INSTERM-4-19,
              SPBFILE.SPB_INSRATE_4_19 = :QSPB-INSRATE-4-19,
              SPBFILE.SPB_INSTERM_4_20 = :QSPB-INSTERM-4-20,
              SPBFILE.SPB_INSRATE_4_20 = :QSPB-INSRATE-4-20,
              SPBFILE.SPB_INSMINPREM_4 = :QSPB-INSMINPREM-4,
              SPBFILE.SPB_INSMINCHGABL_4 = :QSPB-INSMINCHGABL-4,
              SPBFILE.SPB_INSMAXCHGABL_4 = :QSPB-INSMAXCHGABL-4,
              SPBFILE.SPB_INSMAXBENIF_4 = :QSPB-INSMAXBENIF-4,
              SPBFILE.SPB_INSMAXTERM_4 = :QSPB-INSMAXTERM-4,
              SPBFILE.SPB_INSCHGBEN_4 = :QSPB-INSCHGBEN-4,
              SPBFILE.SPB_INSEXCEPT_4 = :QSPB-INSEXCEPT-4,
              SPBFILE.SPB_INSNOVER_4 = :QSPB-INSNOVER-4,
              SPBFILE.SPB_INSMINAGE_4_1 = :QSPB-INSMINAGE-4-1,
              SPBFILE.SPB_INSMAXAGE_4_1 = :QSPB-INSMAXAGE-4-1,
              SPBFILE.SPB_INSMAXCOV_4_1 = :QSPB-INSMAXCOV-4-1,
              SPBFILE.SPB_INSMAXTRM_4_1 = :QSPB-INSMAXTRM-4-1,
              SPBFILE.SPB_INSMINAGE_4_2 = :QSPB-INSMINAGE-4-2,
              SPBFILE.SPB_INSMAXAGE_4_2 = :QSPB-INSMAXAGE-4-2,
              SPBFILE.SPB_INSMAXCOV_4_2 = :QSPB-INSMAXCOV-4-2,
              SPBFILE.SPB_INSMAXTRM_4_2 = :QSPB-INSMAXTRM-4-2,
              SPBFILE.SPB_INSMINAGE_4_3 = :QSPB-INSMINAGE-4-3,
              SPBFILE.SPB_INSMAXAGE_4_3 = :QSPB-INSMAXAGE-4-3,
              SPBFILE.SPB_INSMAXCOV_4_3 = :QSPB-INSMAXCOV-4-3,
              SPBFILE.SPB_INSMAXTRM_4_3 = :QSPB-INSMAXTRM-4-3,
              SPBFILE.SPB_INSMINAGE_4_4 = :QSPB-INSMINAGE-4-4,
              SPBFILE.SPB_INSMAXAGE_4_4 = :QSPB-INSMAXAGE-4-4,
              SPBFILE.SPB_INSMAXCOV_4_4 = :QSPB-INSMAXCOV-4-4,
              SPBFILE.SPB_INSMAXTRM_4_4 = :QSPB-INSMAXTRM-4-4,
              SPBFILE.SPB_INSMINAGE_4_5 = :QSPB-INSMINAGE-4-5,
              SPBFILE.SPB_INSMAXAGE_4_5 = :QSPB-INSMAXAGE-4-5,
              SPBFILE.SPB_INSMAXCOV_4_5 = :QSPB-INSMAXCOV-4-5,
              SPBFILE.SPB_INSMAXTRM_4_5 = :QSPB-INSMAXTRM-4-5,
              SPBFILE.SPB_INSFEECD_4 = :QSPB-INSFEECD-4,
              SPBFILE.SPB_INSFEE_FRMLA_4 = :QSPB-INSFEE-FRMLA-4,
              SPBFILE.SPB_INSCHGABL_FG_4 = :QSPB-INSCHGABL-FG-4,
              SPBFILE.SPB_INSREJECT_FG_4 = :QSPB-INSREJECT-FG-4,
              SPBFILE.SPB_INSFINCHG_4 = :QSPB-INSFINCHG-4,
              SPBFILE.SPB_INSCOM_5 = :QSPB-INSCOM-5,
              SPBFILE.SPB_INSRATECD_5 = :QSPB-INSRATECD-5,
              SPBFILE.SPB_INSJFAC_5 = :QSPB-INSJFAC-5,
              SPBFILE.SPB_INSDISCNT_5 = :QSPB-INSDISCNT-5,
              SPBFILE.SPB_INSCHGABL_5 = :QSPB-INSCHGABL-5,
              SPBFILE.SPB_INSTERM_5_1 = :QSPB-INSTERM-5-1,
              SPBFILE.SPB_INSRATE_5_1 = :QSPB-INSRATE-5-1,
              SPBFILE.SPB_INSTERM_5_2 = :QSPB-INSTERM-5-2,
              SPBFILE.SPB_INSRATE_5_2 = :QSPB-INSRATE-5-2,
              SPBFILE.SPB_INSTERM_5_3 = :QSPB-INSTERM-5-3,
              SPBFILE.SPB_INSRATE_5_3 = :QSPB-INSRATE-5-3,
              SPBFILE.SPB_INSTERM_5_4 = :QSPB-INSTERM-5-4,
              SPBFILE.SPB_INSRATE_5_4 = :QSPB-INSRATE-5-4,
              SPBFILE.SPB_INSTERM_5_5 = :QSPB-INSTERM-5-5,
              SPBFILE.SPB_INSRATE_5_5 = :QSPB-INSRATE-5-5,
              SPBFILE.SPB_INSTERM_5_6 = :QSPB-INSTERM-5-6,
              SPBFILE.SPB_INSRATE_5_6 = :QSPB-INSRATE-5-6,
              SPBFILE.SPB_INSTERM_5_7 = :QSPB-INSTERM-5-7,
              SPBFILE.SPB_INSRATE_5_7 = :QSPB-INSRATE-5-7,
              SPBFILE.SPB_INSTERM_5_8 = :QSPB-INSTERM-5-8,
              SPBFILE.SPB_INSRATE_5_8 = :QSPB-INSRATE-5-8,
              SPBFILE.SPB_INSTERM_5_9 = :QSPB-INSTERM-5-9,
              SPBFILE.SPB_INSRATE_5_9 = :QSPB-INSRATE-5-9,
              SPBFILE.SPB_INSTERM_5_10 = :QSPB-INSTERM-5-10,
              SPBFILE.SPB_INSRATE_5_10 = :QSPB-INSRATE-5-10,
              SPBFILE.SPB_INSTERM_5_11 = :QSPB-INSTERM-5-11,
              SPBFILE.SPB_INSRATE_5_11 = :QSPB-INSRATE-5-11,
              SPBFILE.SPB_INSTERM_5_12 = :QSPB-INSTERM-5-12,
              SPBFILE.SPB_INSRATE_5_12 = :QSPB-INSRATE-5-12,
              SPBFILE.SPB_INSTERM_5_13 = :QSPB-INSTERM-5-13,
              SPBFILE.SPB_INSRATE_5_13 = :QSPB-INSRATE-5-13,
              SPBFILE.SPB_INSTERM_5_14 = :QSPB-INSTERM-5-14,
              SPBFILE.SPB_INSRATE_5_14 = :QSPB-INSRATE-5-14,
              SPBFILE.SPB_INSTERM_5_15 = :QSPB-INSTERM-5-15,
              SPBFILE.SPB_INSRATE_5_15 = :QSPB-INSRATE-5-15,
              SPBFILE.SPB_INSTERM_5_16 = :QSPB-INSTERM-5-16,
              SPBFILE.SPB_INSRATE_5_16 = :QSPB-INSRATE-5-16,
              SPBFILE.SPB_INSTERM_5_17 = :QSPB-INSTERM-5-17,
              SPBFILE.SPB_INSRATE_5_17 = :QSPB-INSRATE-5-17,
              SPBFILE.SPB_INSTERM_5_18 = :QSPB-INSTERM-5-18,
              SPBFILE.SPB_INSRATE_5_18 = :QSPB-INSRATE-5-18,
              SPBFILE.SPB_INSTERM_5_19 = :QSPB-INSTERM-5-19,
              SPBFILE.SPB_INSRATE_5_19 = :QSPB-INSRATE-5-19,
              SPBFILE.SPB_INSTERM_5_20 = :QSPB-INSTERM-5-20,
              SPBFILE.SPB_INSRATE_5_20 = :QSPB-INSRATE-5-20,
              SPBFILE.SPB_INSMINPREM_5 = :QSPB-INSMINPREM-5,
              SPBFILE.SPB_INSMINCHGABL_5 = :QSPB-INSMINCHGABL-5,
              SPBFILE.SPB_INSMAXCHGABL_5 = :QSPB-INSMAXCHGABL-5,
              SPBFILE.SPB_INSMAXBENIF_5 = :QSPB-INSMAXBENIF-5,
              SPBFILE.SPB_INSMAXTERM_5 = :QSPB-INSMAXTERM-5,
              SPBFILE.SPB_INSCHGBEN_5 = :QSPB-INSCHGBEN-5,
              SPBFILE.SPB_INSEXCEPT_5 = :QSPB-INSEXCEPT-5,
              SPBFILE.SPB_INSNOVER_5 = :QSPB-INSNOVER-5,
              SPBFILE.SPB_INSMINAGE_5_1 = :QSPB-INSMINAGE-5-1,
              SPBFILE.SPB_INSMAXAGE_5_1 = :QSPB-INSMAXAGE-5-1,
              SPBFILE.SPB_INSMAXCOV_5_1 = :QSPB-INSMAXCOV-5-1,
              SPBFILE.SPB_INSMAXTRM_5_1 = :QSPB-INSMAXTRM-5-1,
              SPBFILE.SPB_INSMINAGE_5_2 = :QSPB-INSMINAGE-5-2,
              SPBFILE.SPB_INSMAXAGE_5_2 = :QSPB-INSMAXAGE-5-2,
              SPBFILE.SPB_INSMAXCOV_5_2 = :QSPB-INSMAXCOV-5-2,
              SPBFILE.SPB_INSMAXTRM_5_2 = :QSPB-INSMAXTRM-5-2,
              SPBFILE.SPB_INSMINAGE_5_3 = :QSPB-INSMINAGE-5-3,
              SPBFILE.SPB_INSMAXAGE_5_3 = :QSPB-INSMAXAGE-5-3,
              SPBFILE.SPB_INSMAXCOV_5_3 = :QSPB-INSMAXCOV-5-3,
              SPBFILE.SPB_INSMAXTRM_5_3 = :QSPB-INSMAXTRM-5-3,
              SPBFILE.SPB_INSMINAGE_5_4 = :QSPB-INSMINAGE-5-4,
              SPBFILE.SPB_INSMAXAGE_5_4 = :QSPB-INSMAXAGE-5-4,
              SPBFILE.SPB_INSMAXCOV_5_4 = :QSPB-INSMAXCOV-5-4,
              SPBFILE.SPB_INSMAXTRM_5_4 = :QSPB-INSMAXTRM-5-4,
              SPBFILE.SPB_INSMINAGE_5_5 = :QSPB-INSMINAGE-5-5,
              SPBFILE.SPB_INSMAXAGE_5_5 = :QSPB-INSMAXAGE-5-5,
              SPBFILE.SPB_INSMAXCOV_5_5 = :QSPB-INSMAXCOV-5-5,
              SPBFILE.SPB_INSMAXTRM_5_5 = :QSPB-INSMAXTRM-5-5,
              SPBFILE.SPB_INSFEECD_5 = :QSPB-INSFEECD-5,
              SPBFILE.SPB_INSFEE_FRMLA_5 = :QSPB-INSFEE-FRMLA-5,
              SPBFILE.SPB_INSCHGABL_FG_5 = :QSPB-INSCHGABL-FG-5,
              SPBFILE.SPB_INSREJECT_FG_5 = :QSPB-INSREJECT-FG-5,
              SPBFILE.SPB_INSFINCHG_5 = :QSPB-INSFINCHG-5,
              SPBFILE.SPB_INSCOM_6 = :QSPB-INSCOM-6,
              SPBFILE.SPB_INSRATECD_6 = :QSPB-INSRATECD-6,
              SPBFILE.SPB_INSJFAC_6 = :QSPB-INSJFAC-6,
              SPBFILE.SPB_INSDISCNT_6 = :QSPB-INSDISCNT-6,
              SPBFILE.SPB_INSCHGABL_6 = :QSPB-INSCHGABL-6,
              SPBFILE.SPB_INSTERM_6_1 = :QSPB-INSTERM-6-1,
              SPBFILE.SPB_INSRATE_6_1 = :QSPB-INSRATE-6-1,
              SPBFILE.SPB_INSTERM_6_2 = :QSPB-INSTERM-6-2,
              SPBFILE.SPB_INSRATE_6_2 = :QSPB-INSRATE-6-2,
              SPBFILE.SPB_INSTERM_6_3 = :QSPB-INSTERM-6-3,
              SPBFILE.SPB_INSRATE_6_3 = :QSPB-INSRATE-6-3,
              SPBFILE.SPB_INSTERM_6_4 = :QSPB-INSTERM-6-4,
              SPBFILE.SPB_INSRATE_6_4 = :QSPB-INSRATE-6-4,
              SPBFILE.SPB_INSTERM_6_5 = :QSPB-INSTERM-6-5,
              SPBFILE.SPB_INSRATE_6_5 = :QSPB-INSRATE-6-5,
              SPBFILE.SPB_INSTERM_6_6 = :QSPB-INSTERM-6-6,
              SPBFILE.SPB_INSRATE_6_6 = :QSPB-INSRATE-6-6,
              SPBFILE.SPB_INSTERM_6_7 = :QSPB-INSTERM-6-7,
              SPBFILE.SPB_INSRATE_6_7 = :QSPB-INSRATE-6-7,
              SPBFILE.SPB_INSTERM_6_8 = :QSPB-INSTERM-6-8,
              SPBFILE.SPB_INSRATE_6_8 = :QSPB-INSRATE-6-8,
              SPBFILE.SPB_INSTERM_6_9 = :QSPB-INSTERM-6-9,
              SPBFILE.SPB_INSRATE_6_9 = :QSPB-INSRATE-6-9,
              SPBFILE.SPB_INSTERM_6_10 = :QSPB-INSTERM-6-10,
              SPBFILE.SPB_INSRATE_6_10 = :QSPB-INSRATE-6-10,
              SPBFILE.SPB_INSTERM_6_11 = :QSPB-INSTERM-6-11,
              SPBFILE.SPB_INSRATE_6_11 = :QSPB-INSRATE-6-11,
              SPBFILE.SPB_INSTERM_6_12 = :QSPB-INSTERM-6-12,
              SPBFILE.SPB_INSRATE_6_12 = :QSPB-INSRATE-6-12,
              SPBFILE.SPB_INSTERM_6_13 = :QSPB-INSTERM-6-13,
              SPBFILE.SPB_INSRATE_6_13 = :QSPB-INSRATE-6-13,
              SPBFILE.SPB_INSTERM_6_14 = :QSPB-INSTERM-6-14,
              SPBFILE.SPB_INSRATE_6_14 = :QSPB-INSRATE-6-14,
              SPBFILE.SPB_INSTERM_6_15 = :QSPB-INSTERM-6-15,
              SPBFILE.SPB_INSRATE_6_15 = :QSPB-INSRATE-6-15,
              SPBFILE.SPB_INSTERM_6_16 = :QSPB-INSTERM-6-16,
              SPBFILE.SPB_INSRATE_6_16 = :QSPB-INSRATE-6-16,
              SPBFILE.SPB_INSTERM_6_17 = :QSPB-INSTERM-6-17,
              SPBFILE.SPB_INSRATE_6_17 = :QSPB-INSRATE-6-17,
              SPBFILE.SPB_INSTERM_6_18 = :QSPB-INSTERM-6-18,
              SPBFILE.SPB_INSRATE_6_18 = :QSPB-INSRATE-6-18,
              SPBFILE.SPB_INSTERM_6_19 = :QSPB-INSTERM-6-19,
              SPBFILE.SPB_INSRATE_6_19 = :QSPB-INSRATE-6-19,
              SPBFILE.SPB_INSTERM_6_20 = :QSPB-INSTERM-6-20,
              SPBFILE.SPB_INSRATE_6_20 = :QSPB-INSRATE-6-20,
              SPBFILE.SPB_INSMINPREM_6 = :QSPB-INSMINPREM-6,
              SPBFILE.SPB_INSMINCHGABL_6 = :QSPB-INSMINCHGABL-6,
              SPBFILE.SPB_INSMAXCHGABL_6 = :QSPB-INSMAXCHGABL-6,
              SPBFILE.SPB_INSMAXBENIF_6 = :QSPB-INSMAXBENIF-6,
              SPBFILE.SPB_INSMAXTERM_6 = :QSPB-INSMAXTERM-6,
              SPBFILE.SPB_INSCHGBEN_6 = :QSPB-INSCHGBEN-6,
              SPBFILE.SPB_INSEXCEPT_6 = :QSPB-INSEXCEPT-6,
              SPBFILE.SPB_INSNOVER_6 = :QSPB-INSNOVER-6,
              SPBFILE.SPB_INSMINAGE_6_1 = :QSPB-INSMINAGE-6-1,
              SPBFILE.SPB_INSMAXAGE_6_1 = :QSPB-INSMAXAGE-6-1,
              SPBFILE.SPB_INSMAXCOV_6_1 = :QSPB-INSMAXCOV-6-1,
              SPBFILE.SPB_INSMAXTRM_6_1 = :QSPB-INSMAXTRM-6-1,
              SPBFILE.SPB_INSMINAGE_6_2 = :QSPB-INSMINAGE-6-2,
              SPBFILE.SPB_INSMAXAGE_6_2 = :QSPB-INSMAXAGE-6-2,
              SPBFILE.SPB_INSMAXCOV_6_2 = :QSPB-INSMAXCOV-6-2,
              SPBFILE.SPB_INSMAXTRM_6_2 = :QSPB-INSMAXTRM-6-2,
              SPBFILE.SPB_INSMINAGE_6_3 = :QSPB-INSMINAGE-6-3,
              SPBFILE.SPB_INSMAXAGE_6_3 = :QSPB-INSMAXAGE-6-3,
              SPBFILE.SPB_INSMAXCOV_6_3 = :QSPB-INSMAXCOV-6-3,
              SPBFILE.SPB_INSMAXTRM_6_3 = :QSPB-INSMAXTRM-6-3,
              SPBFILE.SPB_INSMINAGE_6_4 = :QSPB-INSMINAGE-6-4,
              SPBFILE.SPB_INSMAXAGE_6_4 = :QSPB-INSMAXAGE-6-4,
              SPBFILE.SPB_INSMAXCOV_6_4 = :QSPB-INSMAXCOV-6-4,
              SPBFILE.SPB_INSMAXTRM_6_4 = :QSPB-INSMAXTRM-6-4,
              SPBFILE.SPB_INSMINAGE_6_5 = :QSPB-INSMINAGE-6-5,
              SPBFILE.SPB_INSMAXAGE_6_5 = :QSPB-INSMAXAGE-6-5,
              SPBFILE.SPB_INSMAXCOV_6_5 = :QSPB-INSMAXCOV-6-5,
              SPBFILE.SPB_INSMAXTRM_6_5 = :QSPB-INSMAXTRM-6-5,
              SPBFILE.SPB_INSFEECD_6 = :QSPB-INSFEECD-6,
              SPBFILE.SPB_INSFEE_FRMLA_6 = :QSPB-INSFEE-FRMLA-6,
              SPBFILE.SPB_INSCHGABL_FG_6 = :QSPB-INSCHGABL-FG-6,
              SPBFILE.SPB_INSREJECT_FG_6 = :QSPB-INSREJECT-FG-6,
              SPBFILE.SPB_INSFINCHG_6 = :QSPB-INSFINCHG-6,
              SPBFILE.SPB_INSCOM_7 = :QSPB-INSCOM-7,
              SPBFILE.SPB_INSRATECD_7 = :QSPB-INSRATECD-7,
              SPBFILE.SPB_INSJFAC_7 = :QSPB-INSJFAC-7,
              SPBFILE.SPB_INSDISCNT_7 = :QSPB-INSDISCNT-7,
              SPBFILE.SPB_INSCHGABL_7 = :QSPB-INSCHGABL-7,
              SPBFILE.SPB_INSTERM_7_1 = :QSPB-INSTERM-7-1,
              SPBFILE.SPB_INSRATE_7_1 = :QSPB-INSRATE-7-1,
              SPBFILE.SPB_INSTERM_7_2 = :QSPB-INSTERM-7-2,
              SPBFILE.SPB_INSRATE_7_2 = :QSPB-INSRATE-7-2,
              SPBFILE.SPB_INSTERM_7_3 = :QSPB-INSTERM-7-3,
              SPBFILE.SPB_INSRATE_7_3 = :QSPB-INSRATE-7-3,
              SPBFILE.SPB_INSTERM_7_4 = :QSPB-INSTERM-7-4,
              SPBFILE.SPB_INSRATE_7_4 = :QSPB-INSRATE-7-4,
              SPBFILE.SPB_INSTERM_7_5 = :QSPB-INSTERM-7-5,
              SPBFILE.SPB_INSRATE_7_5 = :QSPB-INSRATE-7-5,
              SPBFILE.SPB_INSTERM_7_6 = :QSPB-INSTERM-7-6,
              SPBFILE.SPB_INSRATE_7_6 = :QSPB-INSRATE-7-6,
              SPBFILE.SPB_INSTERM_7_7 = :QSPB-INSTERM-7-7,
              SPBFILE.SPB_INSRATE_7_7 = :QSPB-INSRATE-7-7,
              SPBFILE.SPB_INSTERM_7_8 = :QSPB-INSTERM-7-8,
              SPBFILE.SPB_INSRATE_7_8 = :QSPB-INSRATE-7-8,
              SPBFILE.SPB_INSTERM_7_9 = :QSPB-INSTERM-7-9,
              SPBFILE.SPB_INSRATE_7_9 = :QSPB-INSRATE-7-9,
              SPBFILE.SPB_INSTERM_7_10 = :QSPB-INSTERM-7-10,
              SPBFILE.SPB_INSRATE_7_10 = :QSPB-INSRATE-7-10,
              SPBFILE.SPB_INSTERM_7_11 = :QSPB-INSTERM-7-11,
              SPBFILE.SPB_INSRATE_7_11 = :QSPB-INSRATE-7-11,
              SPBFILE.SPB_INSTERM_7_12 = :QSPB-INSTERM-7-12,
              SPBFILE.SPB_INSRATE_7_12 = :QSPB-INSRATE-7-12,
              SPBFILE.SPB_INSTERM_7_13 = :QSPB-INSTERM-7-13,
              SPBFILE.SPB_INSRATE_7_13 = :QSPB-INSRATE-7-13,
              SPBFILE.SPB_INSTERM_7_14 = :QSPB-INSTERM-7-14,
              SPBFILE.SPB_INSRATE_7_14 = :QSPB-INSRATE-7-14,
              SPBFILE.SPB_INSTERM_7_15 = :QSPB-INSTERM-7-15,
              SPBFILE.SPB_INSRATE_7_15 = :QSPB-INSRATE-7-15,
              SPBFILE.SPB_INSTERM_7_16 = :QSPB-INSTERM-7-16,
              SPBFILE.SPB_INSRATE_7_16 = :QSPB-INSRATE-7-16,
              SPBFILE.SPB_INSTERM_7_17 = :QSPB-INSTERM-7-17,
              SPBFILE.SPB_INSRATE_7_17 = :QSPB-INSRATE-7-17,
              SPBFILE.SPB_INSTERM_7_18 = :QSPB-INSTERM-7-18,
              SPBFILE.SPB_INSRATE_7_18 = :QSPB-INSRATE-7-18,
              SPBFILE.SPB_INSTERM_7_19 = :QSPB-INSTERM-7-19,
              SPBFILE.SPB_INSRATE_7_19 = :QSPB-INSRATE-7-19,
              SPBFILE.SPB_INSTERM_7_20 = :QSPB-INSTERM-7-20,
              SPBFILE.SPB_INSRATE_7_20 = :QSPB-INSRATE-7-20,
              SPBFILE.SPB_INSMINPREM_7 = :QSPB-INSMINPREM-7,
              SPBFILE.SPB_INSMINCHGABL_7 = :QSPB-INSMINCHGABL-7,
              SPBFILE.SPB_INSMAXCHGABL_7 = :QSPB-INSMAXCHGABL-7,
              SPBFILE.SPB_INSMAXBENIF_7 = :QSPB-INSMAXBENIF-7,
              SPBFILE.SPB_INSMAXTERM_7 = :QSPB-INSMAXTERM-7,
              SPBFILE.SPB_INSCHGBEN_7 = :QSPB-INSCHGBEN-7,
              SPBFILE.SPB_INSEXCEPT_7 = :QSPB-INSEXCEPT-7,
              SPBFILE.SPB_INSNOVER_7 = :QSPB-INSNOVER-7,
              SPBFILE.SPB_INSMINAGE_7_1 = :QSPB-INSMINAGE-7-1,
              SPBFILE.SPB_INSMAXAGE_7_1 = :QSPB-INSMAXAGE-7-1,
              SPBFILE.SPB_INSMAXCOV_7_1 = :QSPB-INSMAXCOV-7-1,
              SPBFILE.SPB_INSMAXTRM_7_1 = :QSPB-INSMAXTRM-7-1,
              SPBFILE.SPB_INSMINAGE_7_2 = :QSPB-INSMINAGE-7-2,
              SPBFILE.SPB_INSMAXAGE_7_2 = :QSPB-INSMAXAGE-7-2,
              SPBFILE.SPB_INSMAXCOV_7_2 = :QSPB-INSMAXCOV-7-2,
              SPBFILE.SPB_INSMAXTRM_7_2 = :QSPB-INSMAXTRM-7-2,
              SPBFILE.SPB_INSMINAGE_7_3 = :QSPB-INSMINAGE-7-3,
              SPBFILE.SPB_INSMAXAGE_7_3 = :QSPB-INSMAXAGE-7-3,
              SPBFILE.SPB_INSMAXCOV_7_3 = :QSPB-INSMAXCOV-7-3,
              SPBFILE.SPB_INSMAXTRM_7_3 = :QSPB-INSMAXTRM-7-3,
              SPBFILE.SPB_INSMINAGE_7_4 = :QSPB-INSMINAGE-7-4,
              SPBFILE.SPB_INSMAXAGE_7_4 = :QSPB-INSMAXAGE-7-4,
              SPBFILE.SPB_INSMAXCOV_7_4 = :QSPB-INSMAXCOV-7-4,
              SPBFILE.SPB_INSMAXTRM_7_4 = :QSPB-INSMAXTRM-7-4,
              SPBFILE.SPB_INSMINAGE_7_5 = :QSPB-INSMINAGE-7-5,
              SPBFILE.SPB_INSMAXAGE_7_5 = :QSPB-INSMAXAGE-7-5,
              SPBFILE.SPB_INSMAXCOV_7_5 = :QSPB-INSMAXCOV-7-5,
              SPBFILE.SPB_INSMAXTRM_7_5 = :QSPB-INSMAXTRM-7-5,
              SPBFILE.SPB_INSFEECD_7 = :QSPB-INSFEECD-7,
              SPBFILE.SPB_INSFEE_FRMLA_7 = :QSPB-INSFEE-FRMLA-7,
              SPBFILE.SPB_INSCHGABL_FG_7 = :QSPB-INSCHGABL-FG-7,
              SPBFILE.SPB_INSREJECT_FG_7 = :QSPB-INSREJECT-FG-7,
              SPBFILE.SPB_INSFINCHG_7 = :QSPB-INSFINCHG-7,
              SPBFILE.SPB_INSCOM_8 = :QSPB-INSCOM-8,
              SPBFILE.SPB_INSRATECD_8 = :QSPB-INSRATECD-8,
              SPBFILE.SPB_INSJFAC_8 = :QSPB-INSJFAC-8,
              SPBFILE.SPB_INSDISCNT_8 = :QSPB-INSDISCNT-8,
              SPBFILE.SPB_INSCHGABL_8 = :QSPB-INSCHGABL-8,
              SPBFILE.SPB_INSTERM_8_1 = :QSPB-INSTERM-8-1,
              SPBFILE.SPB_INSRATE_8_1 = :QSPB-INSRATE-8-1,
              SPBFILE.SPB_INSTERM_8_2 = :QSPB-INSTERM-8-2,
              SPBFILE.SPB_INSRATE_8_2 = :QSPB-INSRATE-8-2,
              SPBFILE.SPB_INSTERM_8_3 = :QSPB-INSTERM-8-3,
              SPBFILE.SPB_INSRATE_8_3 = :QSPB-INSRATE-8-3,
              SPBFILE.SPB_INSTERM_8_4 = :QSPB-INSTERM-8-4,
              SPBFILE.SPB_INSRATE_8_4 = :QSPB-INSRATE-8-4,
              SPBFILE.SPB_INSTERM_8_5 = :QSPB-INSTERM-8-5,
              SPBFILE.SPB_INSRATE_8_5 = :QSPB-INSRATE-8-5,
              SPBFILE.SPB_INSTERM_8_6 = :QSPB-INSTERM-8-6,
              SPBFILE.SPB_INSRATE_8_6 = :QSPB-INSRATE-8-6,
              SPBFILE.SPB_INSTERM_8_7 = :QSPB-INSTERM-8-7,
              SPBFILE.SPB_INSRATE_8_7 = :QSPB-INSRATE-8-7,
              SPBFILE.SPB_INSTERM_8_8 = :QSPB-INSTERM-8-8,
              SPBFILE.SPB_INSRATE_8_8 = :QSPB-INSRATE-8-8,
              SPBFILE.SPB_INSTERM_8_9 = :QSPB-INSTERM-8-9,
              SPBFILE.SPB_INSRATE_8_9 = :QSPB-INSRATE-8-9,
              SPBFILE.SPB_INSTERM_8_10 = :QSPB-INSTERM-8-10,
              SPBFILE.SPB_INSRATE_8_10 = :QSPB-INSRATE-8-10,
              SPBFILE.SPB_INSTERM_8_11 = :QSPB-INSTERM-8-11,
              SPBFILE.SPB_INSRATE_8_11 = :QSPB-INSRATE-8-11,
              SPBFILE.SPB_INSTERM_8_12 = :QSPB-INSTERM-8-12,
              SPBFILE.SPB_INSRATE_8_12 = :QSPB-INSRATE-8-12,
              SPBFILE.SPB_INSTERM_8_13 = :QSPB-INSTERM-8-13,
              SPBFILE.SPB_INSRATE_8_13 = :QSPB-INSRATE-8-13,
              SPBFILE.SPB_INSTERM_8_14 = :QSPB-INSTERM-8-14,
              SPBFILE.SPB_INSRATE_8_14 = :QSPB-INSRATE-8-14,
              SPBFILE.SPB_INSTERM_8_15 = :QSPB-INSTERM-8-15,
              SPBFILE.SPB_INSRATE_8_15 = :QSPB-INSRATE-8-15,
              SPBFILE.SPB_INSTERM_8_16 = :QSPB-INSTERM-8-16,
              SPBFILE.SPB_INSRATE_8_16 = :QSPB-INSRATE-8-16,
              SPBFILE.SPB_INSTERM_8_17 = :QSPB-INSTERM-8-17,
              SPBFILE.SPB_INSRATE_8_17 = :QSPB-INSRATE-8-17,
              SPBFILE.SPB_INSTERM_8_18 = :QSPB-INSTERM-8-18,
              SPBFILE.SPB_INSRATE_8_18 = :QSPB-INSRATE-8-18,
              SPBFILE.SPB_INSTERM_8_19 = :QSPB-INSTERM-8-19,
              SPBFILE.SPB_INSRATE_8_19 = :QSPB-INSRATE-8-19,
              SPBFILE.SPB_INSTERM_8_20 = :QSPB-INSTERM-8-20,
              SPBFILE.SPB_INSRATE_8_20 = :QSPB-INSRATE-8-20,
              SPBFILE.SPB_INSMINPREM_8 = :QSPB-INSMINPREM-8,
              SPBFILE.SPB_INSMINCHGABL_8 = :QSPB-INSMINCHGABL-8,
              SPBFILE.SPB_INSMAXCHGABL_8 = :QSPB-INSMAXCHGABL-8,
              SPBFILE.SPB_INSMAXBENIF_8 = :QSPB-INSMAXBENIF-8,
              SPBFILE.SPB_INSMAXTERM_8 = :QSPB-INSMAXTERM-8,
              SPBFILE.SPB_INSCHGBEN_8 = :QSPB-INSCHGBEN-8,
              SPBFILE.SPB_INSEXCEPT_8 = :QSPB-INSEXCEPT-8,
              SPBFILE.SPB_INSNOVER_8 = :QSPB-INSNOVER-8,
              SPBFILE.SPB_INSMINAGE_8_1 = :QSPB-INSMINAGE-8-1,
              SPBFILE.SPB_INSMAXAGE_8_1 = :QSPB-INSMAXAGE-8-1,
              SPBFILE.SPB_INSMAXCOV_8_1 = :QSPB-INSMAXCOV-8-1,
              SPBFILE.SPB_INSMAXTRM_8_1 = :QSPB-INSMAXTRM-8-1,
              SPBFILE.SPB_INSMINAGE_8_2 = :QSPB-INSMINAGE-8-2,
              SPBFILE.SPB_INSMAXAGE_8_2 = :QSPB-INSMAXAGE-8-2,
              SPBFILE.SPB_INSMAXCOV_8_2 = :QSPB-INSMAXCOV-8-2,
              SPBFILE.SPB_INSMAXTRM_8_2 = :QSPB-INSMAXTRM-8-2,
              SPBFILE.SPB_INSMINAGE_8_3 = :QSPB-INSMINAGE-8-3,
              SPBFILE.SPB_INSMAXAGE_8_3 = :QSPB-INSMAXAGE-8-3,
              SPBFILE.SPB_INSMAXCOV_8_3 = :QSPB-INSMAXCOV-8-3,
              SPBFILE.SPB_INSMAXTRM_8_3 = :QSPB-INSMAXTRM-8-3,
              SPBFILE.SPB_INSMINAGE_8_4 = :QSPB-INSMINAGE-8-4,
              SPBFILE.SPB_INSMAXAGE_8_4 = :QSPB-INSMAXAGE-8-4,
              SPBFILE.SPB_INSMAXCOV_8_4 = :QSPB-INSMAXCOV-8-4,
              SPBFILE.SPB_INSMAXTRM_8_4 = :QSPB-INSMAXTRM-8-4,
              SPBFILE.SPB_INSMINAGE_8_5 = :QSPB-INSMINAGE-8-5,
              SPBFILE.SPB_INSMAXAGE_8_5 = :QSPB-INSMAXAGE-8-5,
              SPBFILE.SPB_INSMAXCOV_8_5 = :QSPB-INSMAXCOV-8-5,
              SPBFILE.SPB_INSMAXTRM_8_5 = :QSPB-INSMAXTRM-8-5,
              SPBFILE.SPB_INSFEECD_8 = :QSPB-INSFEECD-8,
              SPBFILE.SPB_INSFEE_FRMLA_8 = :QSPB-INSFEE-FRMLA-8,
              SPBFILE.SPB_INSCHGABL_FG_8 = :QSPB-INSCHGABL-FG-8,
              SPBFILE.SPB_INSREJECT_FG_8 = :QSPB-INSREJECT-FG-8,
              SPBFILE.SPB_INSFINCHG_8 = :QSPB-INSFINCHG-8,
              SPBFILE.SPB_INSCOM_9 = :QSPB-INSCOM-9,
              SPBFILE.SPB_INSRATECD_9 = :QSPB-INSRATECD-9,
              SPBFILE.SPB_INSJFAC_9 = :QSPB-INSJFAC-9,
              SPBFILE.SPB_INSDISCNT_9 = :QSPB-INSDISCNT-9,
              SPBFILE.SPB_INSCHGABL_9 = :QSPB-INSCHGABL-9,
              SPBFILE.SPB_INSTERM_9_1 = :QSPB-INSTERM-9-1,
              SPBFILE.SPB_INSRATE_9_1 = :QSPB-INSRATE-9-1,
              SPBFILE.SPB_INSTERM_9_2 = :QSPB-INSTERM-9-2,
              SPBFILE.SPB_INSRATE_9_2 = :QSPB-INSRATE-9-2,
              SPBFILE.SPB_INSTERM_9_3 = :QSPB-INSTERM-9-3,
              SPBFILE.SPB_INSRATE_9_3 = :QSPB-INSRATE-9-3,
              SPBFILE.SPB_INSTERM_9_4 = :QSPB-INSTERM-9-4,
              SPBFILE.SPB_INSRATE_9_4 = :QSPB-INSRATE-9-4,
              SPBFILE.SPB_INSTERM_9_5 = :QSPB-INSTERM-9-5,
              SPBFILE.SPB_INSRATE_9_5 = :QSPB-INSRATE-9-5,
              SPBFILE.SPB_INSTERM_9_6 = :QSPB-INSTERM-9-6,
              SPBFILE.SPB_INSRATE_9_6 = :QSPB-INSRATE-9-6,
              SPBFILE.SPB_INSTERM_9_7 = :QSPB-INSTERM-9-7,
              SPBFILE.SPB_INSRATE_9_7 = :QSPB-INSRATE-9-7,
              SPBFILE.SPB_INSTERM_9_8 = :QSPB-INSTERM-9-8,
              SPBFILE.SPB_INSRATE_9_8 = :QSPB-INSRATE-9-8,
              SPBFILE.SPB_INSTERM_9_9 = :QSPB-INSTERM-9-9,
              SPBFILE.SPB_INSRATE_9_9 = :QSPB-INSRATE-9-9,
              SPBFILE.SPB_INSTERM_9_10 = :QSPB-INSTERM-9-10,
              SPBFILE.SPB_INSRATE_9_10 = :QSPB-INSRATE-9-10,
              SPBFILE.SPB_INSTERM_9_11 = :QSPB-INSTERM-9-11,
              SPBFILE.SPB_INSRATE_9_11 = :QSPB-INSRATE-9-11,
              SPBFILE.SPB_INSTERM_9_12 = :QSPB-INSTERM-9-12,
              SPBFILE.SPB_INSRATE_9_12 = :QSPB-INSRATE-9-12,
              SPBFILE.SPB_INSTERM_9_13 = :QSPB-INSTERM-9-13,
              SPBFILE.SPB_INSRATE_9_13 = :QSPB-INSRATE-9-13,
              SPBFILE.SPB_INSTERM_9_14 = :QSPB-INSTERM-9-14,
              SPBFILE.SPB_INSRATE_9_14 = :QSPB-INSRATE-9-14,
              SPBFILE.SPB_INSTERM_9_15 = :QSPB-INSTERM-9-15,
              SPBFILE.SPB_INSRATE_9_15 = :QSPB-INSRATE-9-15,
              SPBFILE.SPB_INSTERM_9_16 = :QSPB-INSTERM-9-16,
              SPBFILE.SPB_INSRATE_9_16 = :QSPB-INSRATE-9-16,
              SPBFILE.SPB_INSTERM_9_17 = :QSPB-INSTERM-9-17,
              SPBFILE.SPB_INSRATE_9_17 = :QSPB-INSRATE-9-17,
              SPBFILE.SPB_INSTERM_9_18 = :QSPB-INSTERM-9-18,
              SPBFILE.SPB_INSRATE_9_18 = :QSPB-INSRATE-9-18,
              SPBFILE.SPB_INSTERM_9_19 = :QSPB-INSTERM-9-19,
              SPBFILE.SPB_INSRATE_9_19 = :QSPB-INSRATE-9-19,
              SPBFILE.SPB_INSTERM_9_20 = :QSPB-INSTERM-9-20,
              SPBFILE.SPB_INSRATE_9_20 = :QSPB-INSRATE-9-20,
              SPBFILE.SPB_INSMINPREM_9 = :QSPB-INSMINPREM-9,
              SPBFILE.SPB_INSMINCHGABL_9 = :QSPB-INSMINCHGABL-9,
              SPBFILE.SPB_INSMAXCHGABL_9 = :QSPB-INSMAXCHGABL-9,
              SPBFILE.SPB_INSMAXBENIF_9 = :QSPB-INSMAXBENIF-9,
              SPBFILE.SPB_INSMAXTERM_9 = :QSPB-INSMAXTERM-9,
              SPBFILE.SPB_INSCHGBEN_9 = :QSPB-INSCHGBEN-9,
              SPBFILE.SPB_INSEXCEPT_9 = :QSPB-INSEXCEPT-9,
              SPBFILE.SPB_INSNOVER_9 = :QSPB-INSNOVER-9,
              SPBFILE.SPB_INSMINAGE_9_1 = :QSPB-INSMINAGE-9-1,
              SPBFILE.SPB_INSMAXAGE_9_1 = :QSPB-INSMAXAGE-9-1,
              SPBFILE.SPB_INSMAXCOV_9_1 = :QSPB-INSMAXCOV-9-1,
              SPBFILE.SPB_INSMAXTRM_9_1 = :QSPB-INSMAXTRM-9-1,
              SPBFILE.SPB_INSMINAGE_9_2 = :QSPB-INSMINAGE-9-2,
              SPBFILE.SPB_INSMAXAGE_9_2 = :QSPB-INSMAXAGE-9-2,
              SPBFILE.SPB_INSMAXCOV_9_2 = :QSPB-INSMAXCOV-9-2,
              SPBFILE.SPB_INSMAXTRM_9_2 = :QSPB-INSMAXTRM-9-2,
              SPBFILE.SPB_INSMINAGE_9_3 = :QSPB-INSMINAGE-9-3,
              SPBFILE.SPB_INSMAXAGE_9_3 = :QSPB-INSMAXAGE-9-3,
              SPBFILE.SPB_INSMAXCOV_9_3 = :QSPB-INSMAXCOV-9-3,
              SPBFILE.SPB_INSMAXTRM_9_3 = :QSPB-INSMAXTRM-9-3,
              SPBFILE.SPB_INSMINAGE_9_4 = :QSPB-INSMINAGE-9-4,
              SPBFILE.SPB_INSMAXAGE_9_4 = :QSPB-INSMAXAGE-9-4,
              SPBFILE.SPB_INSMAXCOV_9_4 = :QSPB-INSMAXCOV-9-4,
              SPBFILE.SPB_INSMAXTRM_9_4 = :QSPB-INSMAXTRM-9-4,
              SPBFILE.SPB_INSMINAGE_9_5 = :QSPB-INSMINAGE-9-5,
              SPBFILE.SPB_INSMAXAGE_9_5 = :QSPB-INSMAXAGE-9-5,
              SPBFILE.SPB_INSMAXCOV_9_5 = :QSPB-INSMAXCOV-9-5,
              SPBFILE.SPB_INSMAXTRM_9_5 = :QSPB-INSMAXTRM-9-5,
              SPBFILE.SPB_INSFEECD_9 = :QSPB-INSFEECD-9,
              SPBFILE.SPB_INSFEE_FRMLA_9 = :QSPB-INSFEE-FRMLA-9,
              SPBFILE.SPB_INSCHGABL_FG_9 = :QSPB-INSCHGABL-FG-9,
              SPBFILE.SPB_INSREJECT_FG_9 = :QSPB-INSREJECT-FG-9,
              SPBFILE.SPB_INSFINCHG_9 = :QSPB-INSFINCHG-9,
              SPBFILE.SPB_INSCOM_10 = :QSPB-INSCOM-10,
              SPBFILE.SPB_INSRATECD_10 = :QSPB-INSRATECD-10,
              SPBFILE.SPB_INSJFAC_10 = :QSPB-INSJFAC-10,
              SPBFILE.SPB_INSDISCNT_10 = :QSPB-INSDISCNT-10,
              SPBFILE.SPB_INSCHGABL_10 = :QSPB-INSCHGABL-10,
              SPBFILE.SPB_INSTERM_10_1 = :QSPB-INSTERM-10-1,
              SPBFILE.SPB_INSRATE_10_1 = :QSPB-INSRATE-10-1,
              SPBFILE.SPB_INSTERM_10_2 = :QSPB-INSTERM-10-2,
              SPBFILE.SPB_INSRATE_10_2 = :QSPB-INSRATE-10-2,
              SPBFILE.SPB_INSTERM_10_3 = :QSPB-INSTERM-10-3,
              SPBFILE.SPB_INSRATE_10_3 = :QSPB-INSRATE-10-3,
              SPBFILE.SPB_INSTERM_10_4 = :QSPB-INSTERM-10-4,
              SPBFILE.SPB_INSRATE_10_4 = :QSPB-INSRATE-10-4,
              SPBFILE.SPB_INSTERM_10_5 = :QSPB-INSTERM-10-5,
              SPBFILE.SPB_INSRATE_10_5 = :QSPB-INSRATE-10-5,
              SPBFILE.SPB_INSTERM_10_6 = :QSPB-INSTERM-10-6,
              SPBFILE.SPB_INSRATE_10_6 = :QSPB-INSRATE-10-6,
              SPBFILE.SPB_INSTERM_10_7 = :QSPB-INSTERM-10-7,
              SPBFILE.SPB_INSRATE_10_7 = :QSPB-INSRATE-10-7,
              SPBFILE.SPB_INSTERM_10_8 = :QSPB-INSTERM-10-8,
              SPBFILE.SPB_INSRATE_10_8 = :QSPB-INSRATE-10-8,
              SPBFILE.SPB_INSTERM_10_9 = :QSPB-INSTERM-10-9,
              SPBFILE.SPB_INSRATE_10_9 = :QSPB-INSRATE-10-9,
              SPBFILE.SPB_INSTERM_10_10 = :QSPB-INSTERM-10-10,
              SPBFILE.SPB_INSRATE_10_10 = :QSPB-INSRATE-10-10,
              SPBFILE.SPB_INSTERM_10_11 = :QSPB-INSTERM-10-11,
              SPBFILE.SPB_INSRATE_10_11 = :QSPB-INSRATE-10-11,
              SPBFILE.SPB_INSTERM_10_12 = :QSPB-INSTERM-10-12,
              SPBFILE.SPB_INSRATE_10_12 = :QSPB-INSRATE-10-12,
              SPBFILE.SPB_INSTERM_10_13 = :QSPB-INSTERM-10-13,
              SPBFILE.SPB_INSRATE_10_13 = :QSPB-INSRATE-10-13,
              SPBFILE.SPB_INSTERM_10_14 = :QSPB-INSTERM-10-14,
              SPBFILE.SPB_INSRATE_10_14 = :QSPB-INSRATE-10-14,
              SPBFILE.SPB_INSTERM_10_15 = :QSPB-INSTERM-10-15,
              SPBFILE.SPB_INSRATE_10_15 = :QSPB-INSRATE-10-15,
              SPBFILE.SPB_INSTERM_10_16 = :QSPB-INSTERM-10-16,
              SPBFILE.SPB_INSRATE_10_16 = :QSPB-INSRATE-10-16,
              SPBFILE.SPB_INSTERM_10_17 = :QSPB-INSTERM-10-17,
              SPBFILE.SPB_INSRATE_10_17 = :QSPB-INSRATE-10-17,
              SPBFILE.SPB_INSTERM_10_18 = :QSPB-INSTERM-10-18,
              SPBFILE.SPB_INSRATE_10_18 = :QSPB-INSRATE-10-18,
              SPBFILE.SPB_INSTERM_10_19 = :QSPB-INSTERM-10-19,
              SPBFILE.SPB_INSRATE_10_19 = :QSPB-INSRATE-10-19,
              SPBFILE.SPB_INSTERM_10_20 = :QSPB-INSTERM-10-20,
              SPBFILE.SPB_INSRATE_10_20 = :QSPB-INSRATE-10-20,
              SPBFILE.SPB_INSMINPREM_10 = :QSPB-INSMINPREM-10,
              SPBFILE.SPB_INSMINCHGABL_10 = :QSPB-INSMINCHGABL-10,
              SPBFILE.SPB_INSMAXCHGABL_10 = :QSPB-INSMAXCHGABL-10,
              SPBFILE.SPB_INSMAXBENIF_10 = :QSPB-INSMAXBENIF-10,
              SPBFILE.SPB_INSMAXTERM_10 = :QSPB-INSMAXTERM-10,
              SPBFILE.SPB_INSCHGBEN_10 = :QSPB-INSCHGBEN-10,
              SPBFILE.SPB_INSEXCEPT_10 = :QSPB-INSEXCEPT-10,
              SPBFILE.SPB_INSNOVER_10 = :QSPB-INSNOVER-10,
              SPBFILE.SPB_INSMINAGE_10_1 = :QSPB-INSMINAGE-10-1,
              SPBFILE.SPB_INSMAXAGE_10_1 = :QSPB-INSMAXAGE-10-1,
              SPBFILE.SPB_INSMAXCOV_10_1 = :QSPB-INSMAXCOV-10-1,
              SPBFILE.SPB_INSMAXTRM_10_1 = :QSPB-INSMAXTRM-10-1,
              SPBFILE.SPB_INSMINAGE_10_2 = :QSPB-INSMINAGE-10-2,
              SPBFILE.SPB_INSMAXAGE_10_2 = :QSPB-INSMAXAGE-10-2,
              SPBFILE.SPB_INSMAXCOV_10_2 = :QSPB-INSMAXCOV-10-2,
              SPBFILE.SPB_INSMAXTRM_10_2 = :QSPB-INSMAXTRM-10-2,
              SPBFILE.SPB_INSMINAGE_10_3 = :QSPB-INSMINAGE-10-3,
              SPBFILE.SPB_INSMAXAGE_10_3 = :QSPB-INSMAXAGE-10-3,
              SPBFILE.SPB_INSMAXCOV_10_3 = :QSPB-INSMAXCOV-10-3,
              SPBFILE.SPB_INSMAXTRM_10_3 = :QSPB-INSMAXTRM-10-3,
              SPBFILE.SPB_INSMINAGE_10_4 = :QSPB-INSMINAGE-10-4,
              SPBFILE.SPB_INSMAXAGE_10_4 = :QSPB-INSMAXAGE-10-4,
              SPBFILE.SPB_INSMAXCOV_10_4 = :QSPB-INSMAXCOV-10-4,
              SPBFILE.SPB_INSMAXTRM_10_4 = :QSPB-INSMAXTRM-10-4,
              SPBFILE.SPB_INSMINAGE_10_5 = :QSPB-INSMINAGE-10-5,
              SPBFILE.SPB_INSMAXAGE_10_5 = :QSPB-INSMAXAGE-10-5,
              SPBFILE.SPB_INSMAXCOV_10_5 = :QSPB-INSMAXCOV-10-5,
              SPBFILE.SPB_INSMAXTRM_10_5 = :QSPB-INSMAXTRM-10-5,
              SPBFILE.SPB_INSFEECD_10 = :QSPB-INSFEECD-10,
              SPBFILE.SPB_INSFEE_FRMLA_10 = :QSPB-INSFEE-FRMLA-10,
              SPBFILE.SPB_INSCHGABL_FG_10 = :QSPB-INSCHGABL-FG-10,
              SPBFILE.SPB_INSREJECT_FG_10 = :QSPB-INSREJECT-FG-10,
              SPBFILE.SPB_INSFINCHG_10 = :QSPB-INSFINCHG-10,
              SPBFILE.SPB_INSCOM_11 = :QSPB-INSCOM-11,
              SPBFILE.SPB_INSRATECD_11 = :QSPB-INSRATECD-11,
              SPBFILE.SPB_INSJFAC_11 = :QSPB-INSJFAC-11,
              SPBFILE.SPB_INSDISCNT_11 = :QSPB-INSDISCNT-11,
              SPBFILE.SPB_INSCHGABL_11 = :QSPB-INSCHGABL-11,
              SPBFILE.SPB_INSTERM_11_1 = :QSPB-INSTERM-11-1,
              SPBFILE.SPB_INSRATE_11_1 = :QSPB-INSRATE-11-1,
              SPBFILE.SPB_INSTERM_11_2 = :QSPB-INSTERM-11-2,
              SPBFILE.SPB_INSRATE_11_2 = :QSPB-INSRATE-11-2,
              SPBFILE.SPB_INSTERM_11_3 = :QSPB-INSTERM-11-3,
              SPBFILE.SPB_INSRATE_11_3 = :QSPB-INSRATE-11-3,
              SPBFILE.SPB_INSTERM_11_4 = :QSPB-INSTERM-11-4,
              SPBFILE.SPB_INSRATE_11_4 = :QSPB-INSRATE-11-4,
              SPBFILE.SPB_INSTERM_11_5 = :QSPB-INSTERM-11-5,
              SPBFILE.SPB_INSRATE_11_5 = :QSPB-INSRATE-11-5,
              SPBFILE.SPB_INSTERM_11_6 = :QSPB-INSTERM-11-6,
              SPBFILE.SPB_INSRATE_11_6 = :QSPB-INSRATE-11-6,
              SPBFILE.SPB_INSTERM_11_7 = :QSPB-INSTERM-11-7,
              SPBFILE.SPB_INSRATE_11_7 = :QSPB-INSRATE-11-7,
              SPBFILE.SPB_INSTERM_11_8 = :QSPB-INSTERM-11-8,
              SPBFILE.SPB_INSRATE_11_8 = :QSPB-INSRATE-11-8,
              SPBFILE.SPB_INSTERM_11_9 = :QSPB-INSTERM-11-9,
              SPBFILE.SPB_INSRATE_11_9 = :QSPB-INSRATE-11-9,
              SPBFILE.SPB_INSTERM_11_10 = :QSPB-INSTERM-11-10,
              SPBFILE.SPB_INSRATE_11_10 = :QSPB-INSRATE-11-10,
              SPBFILE.SPB_INSTERM_11_11 = :QSPB-INSTERM-11-11,
              SPBFILE.SPB_INSRATE_11_11 = :QSPB-INSRATE-11-11,
              SPBFILE.SPB_INSTERM_11_12 = :QSPB-INSTERM-11-12,
              SPBFILE.SPB_INSRATE_11_12 = :QSPB-INSRATE-11-12,
              SPBFILE.SPB_INSTERM_11_13 = :QSPB-INSTERM-11-13,
              SPBFILE.SPB_INSRATE_11_13 = :QSPB-INSRATE-11-13,
              SPBFILE.SPB_INSTERM_11_14 = :QSPB-INSTERM-11-14,
              SPBFILE.SPB_INSRATE_11_14 = :QSPB-INSRATE-11-14,
              SPBFILE.SPB_INSTERM_11_15 = :QSPB-INSTERM-11-15,
              SPBFILE.SPB_INSRATE_11_15 = :QSPB-INSRATE-11-15,
              SPBFILE.SPB_INSTERM_11_16 = :QSPB-INSTERM-11-16,
              SPBFILE.SPB_INSRATE_11_16 = :QSPB-INSRATE-11-16,
              SPBFILE.SPB_INSTERM_11_17 = :QSPB-INSTERM-11-17,
              SPBFILE.SPB_INSRATE_11_17 = :QSPB-INSRATE-11-17,
              SPBFILE.SPB_INSTERM_11_18 = :QSPB-INSTERM-11-18,
              SPBFILE.SPB_INSRATE_11_18 = :QSPB-INSRATE-11-18,
              SPBFILE.SPB_INSTERM_11_19 = :QSPB-INSTERM-11-19,
              SPBFILE.SPB_INSRATE_11_19 = :QSPB-INSRATE-11-19,
              SPBFILE.SPB_INSTERM_11_20 = :QSPB-INSTERM-11-20,
              SPBFILE.SPB_INSRATE_11_20 = :QSPB-INSRATE-11-20,
              SPBFILE.SPB_INSMINPREM_11 = :QSPB-INSMINPREM-11,
              SPBFILE.SPB_INSMINCHGABL_11 = :QSPB-INSMINCHGABL-11,
              SPBFILE.SPB_INSMAXCHGABL_11 = :QSPB-INSMAXCHGABL-11,
              SPBFILE.SPB_INSMAXBENIF_11 = :QSPB-INSMAXBENIF-11,
              SPBFILE.SPB_INSMAXTERM_11 = :QSPB-INSMAXTERM-11,
              SPBFILE.SPB_INSCHGBEN_11 = :QSPB-INSCHGBEN-11,
              SPBFILE.SPB_INSEXCEPT_11 = :QSPB-INSEXCEPT-11,
              SPBFILE.SPB_INSNOVER_11 = :QSPB-INSNOVER-11,
              SPBFILE.SPB_INSMINAGE_11_1 = :QSPB-INSMINAGE-11-1,
              SPBFILE.SPB_INSMAXAGE_11_1 = :QSPB-INSMAXAGE-11-1,
              SPBFILE.SPB_INSMAXCOV_11_1 = :QSPB-INSMAXCOV-11-1,
              SPBFILE.SPB_INSMAXTRM_11_1 = :QSPB-INSMAXTRM-11-1,
              SPBFILE.SPB_INSMINAGE_11_2 = :QSPB-INSMINAGE-11-2,
              SPBFILE.SPB_INSMAXAGE_11_2 = :QSPB-INSMAXAGE-11-2,
              SPBFILE.SPB_INSMAXCOV_11_2 = :QSPB-INSMAXCOV-11-2,
              SPBFILE.SPB_INSMAXTRM_11_2 = :QSPB-INSMAXTRM-11-2,
              SPBFILE.SPB_INSMINAGE_11_3 = :QSPB-INSMINAGE-11-3,
              SPBFILE.SPB_INSMAXAGE_11_3 = :QSPB-INSMAXAGE-11-3,
              SPBFILE.SPB_INSMAXCOV_11_3 = :QSPB-INSMAXCOV-11-3,
              SPBFILE.SPB_INSMAXTRM_11_3 = :QSPB-INSMAXTRM-11-3,
              SPBFILE.SPB_INSMINAGE_11_4 = :QSPB-INSMINAGE-11-4,
              SPBFILE.SPB_INSMAXAGE_11_4 = :QSPB-INSMAXAGE-11-4,
              SPBFILE.SPB_INSMAXCOV_11_4 = :QSPB-INSMAXCOV-11-4,
              SPBFILE.SPB_INSMAXTRM_11_4 = :QSPB-INSMAXTRM-11-4,
              SPBFILE.SPB_INSMINAGE_11_5 = :QSPB-INSMINAGE-11-5,
              SPBFILE.SPB_INSMAXAGE_11_5 = :QSPB-INSMAXAGE-11-5,
              SPBFILE.SPB_INSMAXCOV_11_5 = :QSPB-INSMAXCOV-11-5,
              SPBFILE.SPB_INSMAXTRM_11_5 = :QSPB-INSMAXTRM-11-5,
              SPBFILE.SPB_INSFEECD_11 = :QSPB-INSFEECD-11,
              SPBFILE.SPB_INSFEE_FRMLA_11 = :QSPB-INSFEE-FRMLA-11,
              SPBFILE.SPB_INSCHGABL_FG_11 = :QSPB-INSCHGABL-FG-11,
              SPBFILE.SPB_INSREJECT_FG_11 = :QSPB-INSREJECT-FG-11,
              SPBFILE.SPB_INSFINCHG_11 = :QSPB-INSFINCHG-11,
              SPBFILE.SPB_INSCOM_12 = :QSPB-INSCOM-12,
              SPBFILE.SPB_INSRATECD_12 = :QSPB-INSRATECD-12,
              SPBFILE.SPB_INSJFAC_12 = :QSPB-INSJFAC-12,
              SPBFILE.SPB_INSDISCNT_12 = :QSPB-INSDISCNT-12,
              SPBFILE.SPB_INSCHGABL_12 = :QSPB-INSCHGABL-12,
              SPBFILE.SPB_INSTERM_12_1 = :QSPB-INSTERM-12-1,
              SPBFILE.SPB_INSRATE_12_1 = :QSPB-INSRATE-12-1,
              SPBFILE.SPB_INSTERM_12_2 = :QSPB-INSTERM-12-2,
              SPBFILE.SPB_INSRATE_12_2 = :QSPB-INSRATE-12-2,
              SPBFILE.SPB_INSTERM_12_3 = :QSPB-INSTERM-12-3,
              SPBFILE.SPB_INSRATE_12_3 = :QSPB-INSRATE-12-3,
              SPBFILE.SPB_INSTERM_12_4 = :QSPB-INSTERM-12-4,
              SPBFILE.SPB_INSRATE_12_4 = :QSPB-INSRATE-12-4,
              SPBFILE.SPB_INSTERM_12_5 = :QSPB-INSTERM-12-5,
              SPBFILE.SPB_INSRATE_12_5 = :QSPB-INSRATE-12-5,
              SPBFILE.SPB_INSTERM_12_6 = :QSPB-INSTERM-12-6,
              SPBFILE.SPB_INSRATE_12_6 = :QSPB-INSRATE-12-6,
              SPBFILE.SPB_INSTERM_12_7 = :QSPB-INSTERM-12-7,
              SPBFILE.SPB_INSRATE_12_7 = :QSPB-INSRATE-12-7,
              SPBFILE.SPB_INSTERM_12_8 = :QSPB-INSTERM-12-8,
              SPBFILE.SPB_INSRATE_12_8 = :QSPB-INSRATE-12-8,
              SPBFILE.SPB_INSTERM_12_9 = :QSPB-INSTERM-12-9,
              SPBFILE.SPB_INSRATE_12_9 = :QSPB-INSRATE-12-9,
              SPBFILE.SPB_INSTERM_12_10 = :QSPB-INSTERM-12-10,
              SPBFILE.SPB_INSRATE_12_10 = :QSPB-INSRATE-12-10,
              SPBFILE.SPB_INSTERM_12_11 = :QSPB-INSTERM-12-11,
              SPBFILE.SPB_INSRATE_12_11 = :QSPB-INSRATE-12-11,
              SPBFILE.SPB_INSTERM_12_12 = :QSPB-INSTERM-12-12,
              SPBFILE.SPB_INSRATE_12_12 = :QSPB-INSRATE-12-12,
              SPBFILE.SPB_INSTERM_12_13 = :QSPB-INSTERM-12-13,
              SPBFILE.SPB_INSRATE_12_13 = :QSPB-INSRATE-12-13,
              SPBFILE.SPB_INSTERM_12_14 = :QSPB-INSTERM-12-14,
              SPBFILE.SPB_INSRATE_12_14 = :QSPB-INSRATE-12-14,
              SPBFILE.SPB_INSTERM_12_15 = :QSPB-INSTERM-12-15,
              SPBFILE.SPB_INSRATE_12_15 = :QSPB-INSRATE-12-15,
              SPBFILE.SPB_INSTERM_12_16 = :QSPB-INSTERM-12-16,
              SPBFILE.SPB_INSRATE_12_16 = :QSPB-INSRATE-12-16,
              SPBFILE.SPB_INSTERM_12_17 = :QSPB-INSTERM-12-17,
              SPBFILE.SPB_INSRATE_12_17 = :QSPB-INSRATE-12-17,
              SPBFILE.SPB_INSTERM_12_18 = :QSPB-INSTERM-12-18,
              SPBFILE.SPB_INSRATE_12_18 = :QSPB-INSRATE-12-18,
              SPBFILE.SPB_INSTERM_12_19 = :QSPB-INSTERM-12-19,
              SPBFILE.SPB_INSRATE_12_19 = :QSPB-INSRATE-12-19,
              SPBFILE.SPB_INSTERM_12_20 = :QSPB-INSTERM-12-20,
              SPBFILE.SPB_INSRATE_12_20 = :QSPB-INSRATE-12-20,
              SPBFILE.SPB_INSMINPREM_12 = :QSPB-INSMINPREM-12,
              SPBFILE.SPB_INSMINCHGABL_12 = :QSPB-INSMINCHGABL-12,
              SPBFILE.SPB_INSMAXCHGABL_12 = :QSPB-INSMAXCHGABL-12,
              SPBFILE.SPB_INSMAXBENIF_12 = :QSPB-INSMAXBENIF-12,
              SPBFILE.SPB_INSMAXTERM_12 = :QSPB-INSMAXTERM-12,
              SPBFILE.SPB_INSCHGBEN_12 = :QSPB-INSCHGBEN-12,
              SPBFILE.SPB_INSEXCEPT_12 = :QSPB-INSEXCEPT-12,
              SPBFILE.SPB_INSNOVER_12 = :QSPB-INSNOVER-12,
              SPBFILE.SPB_INSMINAGE_12_1 = :QSPB-INSMINAGE-12-1,
              SPBFILE.SPB_INSMAXAGE_12_1 = :QSPB-INSMAXAGE-12-1,
              SPBFILE.SPB_INSMAXCOV_12_1 = :QSPB-INSMAXCOV-12-1,
              SPBFILE.SPB_INSMAXTRM_12_1 = :QSPB-INSMAXTRM-12-1,
              SPBFILE.SPB_INSMINAGE_12_2 = :QSPB-INSMINAGE-12-2,
              SPBFILE.SPB_INSMAXAGE_12_2 = :QSPB-INSMAXAGE-12-2,
              SPBFILE.SPB_INSMAXCOV_12_2 = :QSPB-INSMAXCOV-12-2,
              SPBFILE.SPB_INSMAXTRM_12_2 = :QSPB-INSMAXTRM-12-2,
              SPBFILE.SPB_INSMINAGE_12_3 = :QSPB-INSMINAGE-12-3,
              SPBFILE.SPB_INSMAXAGE_12_3 = :QSPB-INSMAXAGE-12-3,
              SPBFILE.SPB_INSMAXCOV_12_3 = :QSPB-INSMAXCOV-12-3,
              SPBFILE.SPB_INSMAXTRM_12_3 = :QSPB-INSMAXTRM-12-3,
              SPBFILE.SPB_INSMINAGE_12_4 = :QSPB-INSMINAGE-12-4,
              SPBFILE.SPB_INSMAXAGE_12_4 = :QSPB-INSMAXAGE-12-4,
              SPBFILE.SPB_INSMAXCOV_12_4 = :QSPB-INSMAXCOV-12-4,
              SPBFILE.SPB_INSMAXTRM_12_4 = :QSPB-INSMAXTRM-12-4,
              SPBFILE.SPB_INSMINAGE_12_5 = :QSPB-INSMINAGE-12-5,
              SPBFILE.SPB_INSMAXAGE_12_5 = :QSPB-INSMAXAGE-12-5,
              SPBFILE.SPB_INSMAXCOV_12_5 = :QSPB-INSMAXCOV-12-5,
              SPBFILE.SPB_INSMAXTRM_12_5 = :QSPB-INSMAXTRM-12-5,
              SPBFILE.SPB_INSFEECD_12 = :QSPB-INSFEECD-12,
              SPBFILE.SPB_INSFEE_FRMLA_12 = :QSPB-INSFEE-FRMLA-12,
              SPBFILE.SPB_INSCHGABL_FG_12 = :QSPB-INSCHGABL-FG-12,
              SPBFILE.SPB_INSREJECT_FG_12 = :QSPB-INSREJECT-FG-12,
              SPBFILE.SPB_INSFINCHG_12 = :QSPB-INSFINCHG-12
            WHERE SPBFILE.SPB_ORGST = :QSPB-ORGST
              AND SPBFILE.SPB_SPRCLASS = :QSPB-SPRCLASS
              AND SPBFILE.SPB_SUBCLASS = :QSPB-SUBCLASS
              AND SPBFILE.SPB_LAWCODE = :QSPB-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-SPB1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-SPB1-FILE.

           PERFORM DELETE-IT.
           MOVE SPB-SELECT-PATH TO E-FILE.
           MOVE SPB1-KEY       TO E-KEYX.
           PERFORM SET-SPB-FIELDS.

           EXEC SQL
            DELETE FROM DBO.SPBFILE
            WHERE SPBFILE.SPB_ORGST = :QSPB-ORGST
              AND SPBFILE.SPB_SPRCLASS = :QSPB-SPRCLASS
              AND SPBFILE.SPB_SUBCLASS = :QSPB-SUBCLASS
              AND SPBFILE.SPB_LAWCODE = :QSPB-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-SPB1-FILE.

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
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*G2G  *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-SPCFILE NOT = "S" )
*G2G          MOVE SPC-PATH-BEG          TO SPC-SELECT-PATH-BEG
*G2G          MOVE SPC-PATH-FILE         TO SPC-SELECT-PATH-FILE
*G2G       ELSE
*G2G          MOVE SPC-PATH-SQL          TO SPC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-SPC1-FILE.
           PERFORM LOAD-SPC1-FILE.
           PERFORM OPEN-IT.
           MOVE SPC-SELECT-PATH TO E-FILE.

      *-----------------------------------------------------------------
       READ-SPC1-FILE.
           PERFORM READ-IT.
           MOVE SPC-SELECT-PATH TO E-FILE.
           MOVE SPC1-KEY TO E-KEYX.

           MOVE SPC-ORGST            TO QSPC-ORGST.
           MOVE SPC-SPRCLASS         TO QSPC-SPRCLASS.
           MOVE SPC-SUBCLASS         TO QSPC-SUBCLASS.
           MOVE SPC-LAWCODE          TO QSPC-LAWCODE.

           EXEC SQL
            SELECT ISNULL(SPCFILE.SPC_ORGST,''),
                   SPCFILE.SPC_SPRCLASS,
                   SPCFILE.SPC_SUBCLASS,
                   SPCFILE.SPC_LAWCODE,
                   SPCFILE.SPC_INSEARN_ORDER_1,
                   SPCFILE.SPC_INSEARN_ORDER_2,
                   SPCFILE.SPC_INSEARN_ORDER_3,
                   SPCFILE.SPC_INSEARN_ORDER_4,
                   SPCFILE.SPC_INSEARN_ORDER_5,
                   SPCFILE.SPC_INSEARN_ORDER_6,
                   SPCFILE.SPC_INSEARN_ORDER_7,
                   SPCFILE.SPC_INSEARN_ORDER_8,
                   SPCFILE.SPC_INSEARN_ORDER_9,
                   SPCFILE.SPC_INSEARN_ORDER_10,
                   SPCFILE.SPC_INSEARN_ORDER_11,
                   SPCFILE.SPC_INSEARN_ORDER_12,
                   ISNULL(SPCFILE.SPC_INSAH_JOINT_FG,''),
                   ISNULL(SPCFILE.SPC_INS_O1_EDIT,''),
                   ISNULL(SPCFILE.SPC_INS_O2_EDIT,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_1,''),
                   ISNULL(SPCFILE.SPC_RBDTE_1_1,''),
                   SPCFILE.SPC_RBDAYS_1_1,
                   SPCFILE.SPC_RBYRTYPE_1_1,
                   ISNULL(SPCFILE.SPC_RBDTE_1_2,''),
                   SPCFILE.SPC_RBDAYS_1_2,
                   SPCFILE.SPC_RBYRTYPE_1_2,
                   SPCFILE.SPC_RBMINRET_1,
                   SPCFILE.SPC_RBNOMON_1,
                   SPCFILE.SPC_RBDEDCHG_1,
                   SPCFILE.SPC_RBRECAST_1,
                   SPCFILE.SPC_RBEARLY_1,
                   SPCFILE.SPC_RBDEFER_1,
                   SPCFILE.SPC_RBPCT_1,
                   SPCFILE.SPC_RBREDUC_1,
                   SPCFILE.SPC_RBMIN_1,
                   SPCFILE.SPC_RBSPOPT1_1,
                   SPCFILE.SPC_RBSPOPT2_1,
                   SPCFILE.SPC_RBMANAGER_1,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_1,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_1,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_2,''),
                   ISNULL(SPCFILE.SPC_RBDTE_2_1,''),
                   SPCFILE.SPC_RBDAYS_2_1,
                   SPCFILE.SPC_RBYRTYPE_2_1,
                   ISNULL(SPCFILE.SPC_RBDTE_2_2,''),
                   SPCFILE.SPC_RBDAYS_2_2,
                   SPCFILE.SPC_RBYRTYPE_2_2,
                   SPCFILE.SPC_RBMINRET_2,
                   SPCFILE.SPC_RBNOMON_2,
                   SPCFILE.SPC_RBDEDCHG_2,
                   SPCFILE.SPC_RBRECAST_2,
                   SPCFILE.SPC_RBEARLY_2,
                   SPCFILE.SPC_RBDEFER_2,
                   SPCFILE.SPC_RBPCT_2,
                   SPCFILE.SPC_RBREDUC_2,
                   SPCFILE.SPC_RBMIN_2,
                   SPCFILE.SPC_RBSPOPT1_2,
                   SPCFILE.SPC_RBSPOPT2_2,
                   SPCFILE.SPC_RBMANAGER_2,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_2,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_2,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_3,''),
                   ISNULL(SPCFILE.SPC_RBDTE_3_1,''),
                   SPCFILE.SPC_RBDAYS_3_1,
                   SPCFILE.SPC_RBYRTYPE_3_1,
                   ISNULL(SPCFILE.SPC_RBDTE_3_2,''),
                   SPCFILE.SPC_RBDAYS_3_2,
                   SPCFILE.SPC_RBYRTYPE_3_2,
                   SPCFILE.SPC_RBMINRET_3,
                   SPCFILE.SPC_RBNOMON_3,
                   SPCFILE.SPC_RBDEDCHG_3,
                   SPCFILE.SPC_RBRECAST_3,
                   SPCFILE.SPC_RBEARLY_3,
                   SPCFILE.SPC_RBDEFER_3,
                   SPCFILE.SPC_RBPCT_3,
                   SPCFILE.SPC_RBREDUC_3,
                   SPCFILE.SPC_RBMIN_3,
                   SPCFILE.SPC_RBSPOPT1_3,
                   SPCFILE.SPC_RBSPOPT2_3,
                   SPCFILE.SPC_RBMANAGER_3,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_3,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_3,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_4,''),
                   ISNULL(SPCFILE.SPC_RBDTE_4_1,''),
                   SPCFILE.SPC_RBDAYS_4_1,
                   SPCFILE.SPC_RBYRTYPE_4_1,
                   ISNULL(SPCFILE.SPC_RBDTE_4_2,''),
                   SPCFILE.SPC_RBDAYS_4_2,
                   SPCFILE.SPC_RBYRTYPE_4_2,
                   SPCFILE.SPC_RBMINRET_4,
                   SPCFILE.SPC_RBNOMON_4,
                   SPCFILE.SPC_RBDEDCHG_4,
                   SPCFILE.SPC_RBRECAST_4,
                   SPCFILE.SPC_RBEARLY_4,
                   SPCFILE.SPC_RBDEFER_4,
                   SPCFILE.SPC_RBPCT_4,
                   SPCFILE.SPC_RBREDUC_4,
                   SPCFILE.SPC_RBMIN_4,
                   SPCFILE.SPC_RBSPOPT1_4,
                   SPCFILE.SPC_RBSPOPT2_4,
                   SPCFILE.SPC_RBMANAGER_4,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_4,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_4,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_5,''),
                   ISNULL(SPCFILE.SPC_RBDTE_5_1,''),
                   SPCFILE.SPC_RBDAYS_5_1,
                   SPCFILE.SPC_RBYRTYPE_5_1,
                   ISNULL(SPCFILE.SPC_RBDTE_5_2,''),
                   SPCFILE.SPC_RBDAYS_5_2,
                   SPCFILE.SPC_RBYRTYPE_5_2,
                   SPCFILE.SPC_RBMINRET_5,
                   SPCFILE.SPC_RBNOMON_5,
                   SPCFILE.SPC_RBDEDCHG_5,
                   SPCFILE.SPC_RBRECAST_5,
                   SPCFILE.SPC_RBEARLY_5,
                   SPCFILE.SPC_RBDEFER_5,
                   SPCFILE.SPC_RBPCT_5,
                   SPCFILE.SPC_RBREDUC_5,
                   SPCFILE.SPC_RBMIN_5,
                   SPCFILE.SPC_RBSPOPT1_5,
                   SPCFILE.SPC_RBSPOPT2_5,
                   SPCFILE.SPC_RBMANAGER_5,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_5,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_5,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_6,''),
                   ISNULL(SPCFILE.SPC_RBDTE_6_1,''),
                   SPCFILE.SPC_RBDAYS_6_1,
                   SPCFILE.SPC_RBYRTYPE_6_1,
                   ISNULL(SPCFILE.SPC_RBDTE_6_2,''),
                   SPCFILE.SPC_RBDAYS_6_2,
                   SPCFILE.SPC_RBYRTYPE_6_2,
                   SPCFILE.SPC_RBMINRET_6,
                   SPCFILE.SPC_RBNOMON_6,
                   SPCFILE.SPC_RBDEDCHG_6,
                   SPCFILE.SPC_RBRECAST_6,
                   SPCFILE.SPC_RBEARLY_6,
                   SPCFILE.SPC_RBDEFER_6,
                   SPCFILE.SPC_RBPCT_6,
                   SPCFILE.SPC_RBREDUC_6,
                   SPCFILE.SPC_RBMIN_6,
                   SPCFILE.SPC_RBSPOPT1_6,
                   SPCFILE.SPC_RBSPOPT2_6,
                   SPCFILE.SPC_RBMANAGER_6,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_6,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_6,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_7,''),
                   ISNULL(SPCFILE.SPC_RBDTE_7_1,''),
                   SPCFILE.SPC_RBDAYS_7_1,
                   SPCFILE.SPC_RBYRTYPE_7_1,
                   ISNULL(SPCFILE.SPC_RBDTE_7_2,''),
                   SPCFILE.SPC_RBDAYS_7_2,
                   SPCFILE.SPC_RBYRTYPE_7_2,
                   SPCFILE.SPC_RBMINRET_7,
                   SPCFILE.SPC_RBNOMON_7,
                   SPCFILE.SPC_RBDEDCHG_7,
                   SPCFILE.SPC_RBRECAST_7,
                   SPCFILE.SPC_RBEARLY_7,
                   SPCFILE.SPC_RBDEFER_7,
                   SPCFILE.SPC_RBPCT_7,
                   SPCFILE.SPC_RBREDUC_7,
                   SPCFILE.SPC_RBMIN_7,
                   SPCFILE.SPC_RBSPOPT1_7,
                   SPCFILE.SPC_RBSPOPT2_7,
                   SPCFILE.SPC_RBMANAGER_7,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_7,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_7,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_8,''),
                   ISNULL(SPCFILE.SPC_RBDTE_8_1,''),
                   SPCFILE.SPC_RBDAYS_8_1,
                   SPCFILE.SPC_RBYRTYPE_8_1,
                   ISNULL(SPCFILE.SPC_RBDTE_8_2,''),
                   SPCFILE.SPC_RBDAYS_8_2,
                   SPCFILE.SPC_RBYRTYPE_8_2,
                   SPCFILE.SPC_RBMINRET_8,
                   SPCFILE.SPC_RBNOMON_8,
                   SPCFILE.SPC_RBDEDCHG_8,
                   SPCFILE.SPC_RBRECAST_8,
                   SPCFILE.SPC_RBEARLY_8,
                   SPCFILE.SPC_RBDEFER_8,
                   SPCFILE.SPC_RBPCT_8,
                   SPCFILE.SPC_RBREDUC_8,
                   SPCFILE.SPC_RBMIN_8,
                   SPCFILE.SPC_RBSPOPT1_8,
                   SPCFILE.SPC_RBSPOPT2_8,
                   SPCFILE.SPC_RBMANAGER_8,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_8,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_8,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_9,''),
                   ISNULL(SPCFILE.SPC_RBDTE_9_1,''),
                   SPCFILE.SPC_RBDAYS_9_1,
                   SPCFILE.SPC_RBYRTYPE_9_1,
                   ISNULL(SPCFILE.SPC_RBDTE_9_2,''),
                   SPCFILE.SPC_RBDAYS_9_2,
                   SPCFILE.SPC_RBYRTYPE_9_2,
                   SPCFILE.SPC_RBMINRET_9,
                   SPCFILE.SPC_RBNOMON_9,
                   SPCFILE.SPC_RBDEDCHG_9,
                   SPCFILE.SPC_RBRECAST_9,
                   SPCFILE.SPC_RBEARLY_9,
                   SPCFILE.SPC_RBDEFER_9,
                   SPCFILE.SPC_RBPCT_9,
                   SPCFILE.SPC_RBREDUC_9,
                   SPCFILE.SPC_RBMIN_9,
                   SPCFILE.SPC_RBSPOPT1_9,
                   SPCFILE.SPC_RBSPOPT2_9,
                   SPCFILE.SPC_RBMANAGER_9,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_9,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_9,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_10,''),
                   ISNULL(SPCFILE.SPC_RBDTE_10_1,''),
                   SPCFILE.SPC_RBDAYS_10_1,
                   SPCFILE.SPC_RBYRTYPE_10_1,
                   ISNULL(SPCFILE.SPC_RBDTE_10_2,''),
                   SPCFILE.SPC_RBDAYS_10_2,
                   SPCFILE.SPC_RBYRTYPE_10_2,
                   SPCFILE.SPC_RBMINRET_10,
                   SPCFILE.SPC_RBNOMON_10,
                   SPCFILE.SPC_RBDEDCHG_10,
                   SPCFILE.SPC_RBRECAST_10,
                   SPCFILE.SPC_RBEARLY_10,
                   SPCFILE.SPC_RBDEFER_10,
                   SPCFILE.SPC_RBPCT_10,
                   SPCFILE.SPC_RBREDUC_10,
                   SPCFILE.SPC_RBMIN_10,
                   SPCFILE.SPC_RBSPOPT1_10,
                   SPCFILE.SPC_RBSPOPT2_10,
                   SPCFILE.SPC_RBMANAGER_10,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_10,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_10,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_11,''),
                   ISNULL(SPCFILE.SPC_RBDTE_11_1,''),
                   SPCFILE.SPC_RBDAYS_11_1,
                   SPCFILE.SPC_RBYRTYPE_11_1,
                   ISNULL(SPCFILE.SPC_RBDTE_11_2,''),
                   SPCFILE.SPC_RBDAYS_11_2,
                   SPCFILE.SPC_RBYRTYPE_11_2,
                   SPCFILE.SPC_RBMINRET_11,
                   SPCFILE.SPC_RBNOMON_11,
                   SPCFILE.SPC_RBDEDCHG_11,
                   SPCFILE.SPC_RBRECAST_11,
                   SPCFILE.SPC_RBEARLY_11,
                   SPCFILE.SPC_RBDEFER_11,
                   SPCFILE.SPC_RBPCT_11,
                   SPCFILE.SPC_RBREDUC_11,
                   SPCFILE.SPC_RBMIN_11,
                   SPCFILE.SPC_RBSPOPT1_11,
                   SPCFILE.SPC_RBSPOPT2_11,
                   SPCFILE.SPC_RBMANAGER_11,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_11,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_11,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_12,''),
                   ISNULL(SPCFILE.SPC_RBDTE_12_1,''),
                   SPCFILE.SPC_RBDAYS_12_1,
                   SPCFILE.SPC_RBYRTYPE_12_1,
                   ISNULL(SPCFILE.SPC_RBDTE_12_2,''),
                   SPCFILE.SPC_RBDAYS_12_2,
                   SPCFILE.SPC_RBYRTYPE_12_2,
                   SPCFILE.SPC_RBMINRET_12,
                   SPCFILE.SPC_RBNOMON_12,
                   SPCFILE.SPC_RBDEDCHG_12,
                   SPCFILE.SPC_RBRECAST_12,
                   SPCFILE.SPC_RBEARLY_12,
                   SPCFILE.SPC_RBDEFER_12,
                   SPCFILE.SPC_RBPCT_12,
                   SPCFILE.SPC_RBREDUC_12,
                   SPCFILE.SPC_RBMIN_12,
                   SPCFILE.SPC_RBSPOPT1_12,
                   SPCFILE.SPC_RBSPOPT2_12,
                   SPCFILE.SPC_RBMANAGER_12,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_12,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_12,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_13,''),
                   ISNULL(SPCFILE.SPC_RBDTE_13_1,''),
                   SPCFILE.SPC_RBDAYS_13_1,
                   SPCFILE.SPC_RBYRTYPE_13_1,
                   ISNULL(SPCFILE.SPC_RBDTE_13_2,''),
                   SPCFILE.SPC_RBDAYS_13_2,
                   SPCFILE.SPC_RBYRTYPE_13_2,
                   SPCFILE.SPC_RBMINRET_13,
                   SPCFILE.SPC_RBNOMON_13,
                   SPCFILE.SPC_RBDEDCHG_13,
                   SPCFILE.SPC_RBRECAST_13,
                   SPCFILE.SPC_RBEARLY_13,
                   SPCFILE.SPC_RBDEFER_13,
                   SPCFILE.SPC_RBPCT_13,
                   SPCFILE.SPC_RBREDUC_13,
                   SPCFILE.SPC_RBMIN_13,
                   SPCFILE.SPC_RBSPOPT1_13,
                   SPCFILE.SPC_RBSPOPT2_13,
                   SPCFILE.SPC_RBMANAGER_13,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_13,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_13,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_14,''),
                   ISNULL(SPCFILE.SPC_RBDTE_14_1,''),
                   SPCFILE.SPC_RBDAYS_14_1,
                   SPCFILE.SPC_RBYRTYPE_14_1,
                   ISNULL(SPCFILE.SPC_RBDTE_14_2,''),
                   SPCFILE.SPC_RBDAYS_14_2,
                   SPCFILE.SPC_RBYRTYPE_14_2,
                   SPCFILE.SPC_RBMINRET_14,
                   SPCFILE.SPC_RBNOMON_14,
                   SPCFILE.SPC_RBDEDCHG_14,
                   SPCFILE.SPC_RBRECAST_14,
                   SPCFILE.SPC_RBEARLY_14,
                   SPCFILE.SPC_RBDEFER_14,
                   SPCFILE.SPC_RBPCT_14,
                   SPCFILE.SPC_RBREDUC_14,
                   SPCFILE.SPC_RBMIN_14,
                   SPCFILE.SPC_RBSPOPT1_14,
                   SPCFILE.SPC_RBSPOPT2_14,
                   SPCFILE.SPC_RBMANAGER_14,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_14,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_14,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_15,''),
                   ISNULL(SPCFILE.SPC_RBDTE_15_1,''),
                   SPCFILE.SPC_RBDAYS_15_1,
                   SPCFILE.SPC_RBYRTYPE_15_1,
                   ISNULL(SPCFILE.SPC_RBDTE_15_2,''),
                   SPCFILE.SPC_RBDAYS_15_2,
                   SPCFILE.SPC_RBYRTYPE_15_2,
                   SPCFILE.SPC_RBMINRET_15,
                   SPCFILE.SPC_RBNOMON_15,
                   SPCFILE.SPC_RBDEDCHG_15,
                   SPCFILE.SPC_RBRECAST_15,
                   SPCFILE.SPC_RBEARLY_15,
                   SPCFILE.SPC_RBDEFER_15,
                   SPCFILE.SPC_RBPCT_15,
                   SPCFILE.SPC_RBREDUC_15,
                   SPCFILE.SPC_RBMIN_15,
                   SPCFILE.SPC_RBSPOPT1_15,
                   SPCFILE.SPC_RBSPOPT2_15,
                   SPCFILE.SPC_RBMANAGER_15,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_15,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_15,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_16,''),
                   ISNULL(SPCFILE.SPC_RBDTE_16_1,''),
                   SPCFILE.SPC_RBDAYS_16_1,
                   SPCFILE.SPC_RBYRTYPE_16_1,
                   ISNULL(SPCFILE.SPC_RBDTE_16_2,''),
                   SPCFILE.SPC_RBDAYS_16_2,
                   SPCFILE.SPC_RBYRTYPE_16_2,
                   SPCFILE.SPC_RBMINRET_16,
                   SPCFILE.SPC_RBNOMON_16,
                   SPCFILE.SPC_RBDEDCHG_16,
                   SPCFILE.SPC_RBRECAST_16,
                   SPCFILE.SPC_RBEARLY_16,
                   SPCFILE.SPC_RBDEFER_16,
                   SPCFILE.SPC_RBPCT_16,
                   SPCFILE.SPC_RBREDUC_16,
                   SPCFILE.SPC_RBMIN_16,
                   SPCFILE.SPC_RBSPOPT1_16,
                   SPCFILE.SPC_RBSPOPT2_16,
                   SPCFILE.SPC_RBMANAGER_16,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_16,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_16,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_17,''),
                   ISNULL(SPCFILE.SPC_RBDTE_17_1,''),
                   SPCFILE.SPC_RBDAYS_17_1,
                   SPCFILE.SPC_RBYRTYPE_17_1,
                   ISNULL(SPCFILE.SPC_RBDTE_17_2,''),
                   SPCFILE.SPC_RBDAYS_17_2,
                   SPCFILE.SPC_RBYRTYPE_17_2,
                   SPCFILE.SPC_RBMINRET_17,
                   SPCFILE.SPC_RBNOMON_17,
                   SPCFILE.SPC_RBDEDCHG_17,
                   SPCFILE.SPC_RBRECAST_17,
                   SPCFILE.SPC_RBEARLY_17,
                   SPCFILE.SPC_RBDEFER_17,
                   SPCFILE.SPC_RBPCT_17,
                   SPCFILE.SPC_RBREDUC_17,
                   SPCFILE.SPC_RBMIN_17,
                   SPCFILE.SPC_RBSPOPT1_17,
                   SPCFILE.SPC_RBSPOPT2_17,
                   SPCFILE.SPC_RBMANAGER_17,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_17,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_17,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_18,''),
                   ISNULL(SPCFILE.SPC_RBDTE_18_1,''),
                   SPCFILE.SPC_RBDAYS_18_1,
                   SPCFILE.SPC_RBYRTYPE_18_1,
                   ISNULL(SPCFILE.SPC_RBDTE_18_2,''),
                   SPCFILE.SPC_RBDAYS_18_2,
                   SPCFILE.SPC_RBYRTYPE_18_2,
                   SPCFILE.SPC_RBMINRET_18,
                   SPCFILE.SPC_RBNOMON_18,
                   SPCFILE.SPC_RBDEDCHG_18,
                   SPCFILE.SPC_RBRECAST_18,
                   SPCFILE.SPC_RBEARLY_18,
                   SPCFILE.SPC_RBDEFER_18,
                   SPCFILE.SPC_RBPCT_18,
                   SPCFILE.SPC_RBREDUC_18,
                   SPCFILE.SPC_RBMIN_18,
                   SPCFILE.SPC_RBSPOPT1_18,
                   SPCFILE.SPC_RBSPOPT2_18,
                   SPCFILE.SPC_RBMANAGER_18,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_18,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_18,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_19,''),
                   ISNULL(SPCFILE.SPC_RBDTE_19_1,''),
                   SPCFILE.SPC_RBDAYS_19_1,
                   SPCFILE.SPC_RBYRTYPE_19_1,
                   ISNULL(SPCFILE.SPC_RBDTE_19_2,''),
                   SPCFILE.SPC_RBDAYS_19_2,
                   SPCFILE.SPC_RBYRTYPE_19_2,
                   SPCFILE.SPC_RBMINRET_19,
                   SPCFILE.SPC_RBNOMON_19,
                   SPCFILE.SPC_RBDEDCHG_19,
                   SPCFILE.SPC_RBRECAST_19,
                   SPCFILE.SPC_RBEARLY_19,
                   SPCFILE.SPC_RBDEFER_19,
                   SPCFILE.SPC_RBPCT_19,
                   SPCFILE.SPC_RBREDUC_19,
                   SPCFILE.SPC_RBMIN_19,
                   SPCFILE.SPC_RBSPOPT1_19,
                   SPCFILE.SPC_RBSPOPT2_19,
                   SPCFILE.SPC_RBMANAGER_19,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_19,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_19,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_20,''),
                   ISNULL(SPCFILE.SPC_RBDTE_20_1,''),
                   SPCFILE.SPC_RBDAYS_20_1,
                   SPCFILE.SPC_RBYRTYPE_20_1,
                   ISNULL(SPCFILE.SPC_RBDTE_20_2,''),
                   SPCFILE.SPC_RBDAYS_20_2,
                   SPCFILE.SPC_RBYRTYPE_20_2,
                   SPCFILE.SPC_RBMINRET_20,
                   SPCFILE.SPC_RBNOMON_20,
                   SPCFILE.SPC_RBDEDCHG_20,
                   SPCFILE.SPC_RBRECAST_20,
                   SPCFILE.SPC_RBEARLY_20,
                   SPCFILE.SPC_RBDEFER_20,
                   SPCFILE.SPC_RBPCT_20,
                   SPCFILE.SPC_RBREDUC_20,
                   SPCFILE.SPC_RBMIN_20,
                   SPCFILE.SPC_RBSPOPT1_20,
                   SPCFILE.SPC_RBSPOPT2_20,
                   SPCFILE.SPC_RBMANAGER_20,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_20,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_20,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_21,''),
                   ISNULL(SPCFILE.SPC_RBDTE_21_1,''),
                   SPCFILE.SPC_RBDAYS_21_1,
                   SPCFILE.SPC_RBYRTYPE_21_1,
                   ISNULL(SPCFILE.SPC_RBDTE_21_2,''),
                   SPCFILE.SPC_RBDAYS_21_2,
                   SPCFILE.SPC_RBYRTYPE_21_2,
                   SPCFILE.SPC_RBMINRET_21,
                   SPCFILE.SPC_RBNOMON_21,
                   SPCFILE.SPC_RBDEDCHG_21,
                   SPCFILE.SPC_RBRECAST_21,
                   SPCFILE.SPC_RBEARLY_21,
                   SPCFILE.SPC_RBDEFER_21,
                   SPCFILE.SPC_RBPCT_21,
                   SPCFILE.SPC_RBREDUC_21,
                   SPCFILE.SPC_RBMIN_21,
                   SPCFILE.SPC_RBSPOPT1_21,
                   SPCFILE.SPC_RBSPOPT2_21,
                   SPCFILE.SPC_RBMANAGER_21,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_21,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_21,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_22,''),
                   ISNULL(SPCFILE.SPC_RBDTE_22_1,''),
                   SPCFILE.SPC_RBDAYS_22_1,
                   SPCFILE.SPC_RBYRTYPE_22_1,
                   ISNULL(SPCFILE.SPC_RBDTE_22_2,''),
                   SPCFILE.SPC_RBDAYS_22_2,
                   SPCFILE.SPC_RBYRTYPE_22_2,
                   SPCFILE.SPC_RBMINRET_22,
                   SPCFILE.SPC_RBNOMON_22,
                   SPCFILE.SPC_RBDEDCHG_22,
                   SPCFILE.SPC_RBRECAST_22,
                   SPCFILE.SPC_RBEARLY_22,
                   SPCFILE.SPC_RBDEFER_22,
                   SPCFILE.SPC_RBPCT_22,
                   SPCFILE.SPC_RBREDUC_22,
                   SPCFILE.SPC_RBMIN_22,
                   SPCFILE.SPC_RBSPOPT1_22,
                   SPCFILE.SPC_RBSPOPT2_22,
                   SPCFILE.SPC_RBMANAGER_22,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_22,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_22,''),
                   ISNULL(SPCFILE.SPC_RBCOMCD,''),
                   ISNULL(SPCFILE.SPC_ADDON_DISFRMLA,''),
                   ISNULL(SPCFILE.SPC_ADDON_WHOLE_FG,''),
                   ISNULL(SPCFILE.SPC_ADDON_ORG_MATURITY,''),
                   ISNULL(SPCFILE.SPC_CONSOLIDATE_RT_FG,''),
                   SPCFILE.SPC_1STPY_ADDON_DAYS,
                   SPCFILE.SPC_1STPY_ADDON_YRTYPE,
                   ISNULL(SPCFILE.SPC_ADDON_RATE_FG,''),
                   SPCFILE.SPC_ADDON_RATE,
                   ISNULL(SPCFILE.SPC_FEEFRMLA_1,''),
                   ISNULL(SPCFILE.SPC_FEECD_1,''),
                   ISNULL(SPCFILE.SPC_FEEID_1,''),
                   ISNULL(SPCFILE.SPC_FEEFRMLA_2,''),
                   ISNULL(SPCFILE.SPC_FEECD_2,''),
                   ISNULL(SPCFILE.SPC_FEEID_2,''),
                   ISNULL(SPCFILE.SPC_ROUND_PAY_FG,''),
                   ISNULL(SPCFILE.SPC_IRREGULAR_PAY_FG,''),
                   ISNULL(SPCFILE.SPC_DLRVCHGS_RB_CD_1,''),
                   ISNULL(SPCFILE.SPC_DLRVCHGS_RB_CD_2,''),
                   ISNULL(SPCFILE.SPC_DLRVCHGS_RB_CD_3,''),
                   ISNULL(SPCFILE.SPC_PARVHELD_RB_CD,''),
                   ISNULL(SPCFILE.SPC_PARVPAID_RB_CD,''),
                   SPCFILE.SPC_CONTR_CREDIT,
                   ISNULL(SPCFILE.SPC_CONTR_CREDIT_CD,''),
                   SPCFILE.SPC_CONTR_CREDIT_MIN_DEL_PER,
                   ISNULL(SPCFILE.SPC_OPT_CAL_CHGABLE,''),
                   SPCFILE.SPC_PBDEL_DAYS,
                   SPCFILE.SPC_PBINCR_PERCENT,
                   ISNULL(SPCFILE.SPC_CPOT30_CD,''),
                   ISNULL(SPCFILE.SPC_CPOTCHGOFF_CD,''),
                   ISNULL(SPCFILE.SPC_RDPOT30_CD,''),
                   ISNULL(SPCFILE.SPC_RDPOTCHGOFF_CD,''),
                   SPCFILE.SPC_MAX_ALLOW_DELINQ,
                   ISNULL(SPCFILE.SPC_RESCIND_BUS_DAYS,''),
                   ISNULL(SPCFILE.SPC_RESCIND_SATURDAY,''),
                   SPCFILE.SPC_MIN_CREDIT_SCORE,
                   ISNULL(SPCFILE.SPC_DUAL_IN_BRANCH,''),
                   ISNULL(SPCFILE.SPC_DUAL_IN_STATE,''),
                   ISNULL(SPCFILE.SPC_PACESETTER_FG,''),
                   SPCFILE.SPC_WAGE_ASSIGN_DAYS,
                   ISNULL(SPCFILE.SPC_CAL_PAYMENT_ROUND_FG,''),
                   ISNULL(SPCFILE.SPC_CAN_LOANS_FG,''),
                   ISNULL(SPCFILE.SPC_ALLOW_DELINQ_POLCD,''),
                   SPCFILE.SPC_YTD_MAX_ALLOW_DELINQ,
                   ISNULL(SPCFILE.SPC_LNCOST_FRMLA,''),
                   SPCFILE.SPC_LNCOST_AMT,
                   SPCFILE.SPC_DLDISC_RVPRIN1_PCNT,
                   SPCFILE.SPC_RENEWABLE_PAYS,
                   SPCFILE.SPC_RENEWABLE_DAYS,
                   ISNULL(SPCFILE.SPC_ALLOW_LN_BACK_DATE,''),
                   SPCFILE.SPC_DLDISC_RVPRIN1_AF_PCNT,
                   ISNULL(SPCFILE.SPC_CAL_MAX_AT_ESTIMATE,''),
                   ISNULL(SPCFILE.SPC_INSAH_RETRO,''),
                   ISNULL(SPCFILE.SPC_INSAH_DAYS_WAIT,''),
                   ISNULL(SPCFILE.SPC_ADDON_EXCEPT_FG,''),
                   ISNULL(SPCFILE.SPC_REPO_TRCD_1,''),
                   SPCFILE.SPC_REPO_FMNO_1_1,
                   SPCFILE.SPC_REPO_FMNO_1_2,
                   SPCFILE.SPC_REPO_FMNO_1_3,
                   SPCFILE.SPC_REPO_FMNO_1_4,
                   ISNULL(SPCFILE.SPC_REPO_TRCD_2,''),
                   SPCFILE.SPC_REPO_FMNO_2_1,
                   SPCFILE.SPC_REPO_FMNO_2_2,
                   SPCFILE.SPC_REPO_FMNO_2_3,
                   SPCFILE.SPC_REPO_FMNO_2_4,
                   ISNULL(SPCFILE.SPC_REPO_TRCD_3,''),
                   SPCFILE.SPC_REPO_FMNO_3_1,
                   SPCFILE.SPC_REPO_FMNO_3_2,
                   SPCFILE.SPC_REPO_FMNO_3_3,
                   SPCFILE.SPC_REPO_FMNO_3_4,
                   ISNULL(SPCFILE.SPC_CANCEL_ADDON_FG,''),
                   SPCFILE.SPC_DEFAULT_O2_INS_COMP,
                   ISNULL(SPCFILE.SPC_LC_ON_JUDGEMENTS,''),
                   SPCFILE.SPC_DELQ_IB_AGE,
                   SPCFILE.SPC_DELQ_IB_MAX_DAYS,
                   ISNULL(SPCFILE.SPC_INSURANCE_WRITTEN,''),
                   ISNULL(SPCFILE.SPC_INTERIM_COUPON,''),
                   ISNULL(SPCFILE.SPC_AUTO_ASSESS_LCHG,''),
                   SPCFILE.SPC_MAX_CASH_PROCEEDS,
                   ISNULL(SPCFILE.SPC_ALLOW_ZERO_PROC,''),
                   SPCFILE.SPC_BANKRULE_RATE,
                   ISNULL(SPCFILE.SPC_AUTO_SEND_TRCD_1,''),
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_1,
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_2,
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_3,
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_4,
                   ISNULL(SPCFILE.SPC_AUTO_SEND_TRCD_2,''),
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_1,
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_2,
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_3,
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_4,
                   SPCFILE.SPC_DLDISC_CONV_BF_PCNT,
                   ISNULL(SPCFILE.SPC_UNITPER_CD,''),
                   ISNULL(SPCFILE.SPC_RENEWAL_EDIT,''),
                   ISNULL(SPCFILE.SPC_ROUND_MFEE,''),
                   ISNULL(SPCFILE.SPC_ADV_TO_1ST_NO_CORR,''),
                   ISNULL(SPCFILE.SPC_LC_INT_ON_REPO_NON_PL,''),
                   SPCFILE.SPC_CONTRACTUAL_SET_NONACCRUAL,
                   SPCFILE.SPC_CONTRACTUAL_RESET_ACCRUAL,
                   SPCFILE.SPC_MONTHS_PAY_NAPP_COUNTER,
                   SPCFILE.SPC_SHORT_PAY_THRESHOLD,
                   SPCFILE.SPC_CASH_AVAIL_PERCENT,
                   ISNULL(SPCFILE.SPC_O1_JOINT_FG,''),
                   SPCFILE.SPC_NET_BAL_REQ,
                   SPCFILE.SPC_AMT_PROCEEDS_REQ,
                   SPCFILE.SPC_ALT_PREPAY_MONTHS,
                   SPCFILE.SPC_PERCENT_REGPAY_AVAIL,
                   SPCFILE.SPC_ALT_DEFFRMLA,
                   SPCFILE.SPC_ALT_DEFMIN,
                   SPCFILE.SPC_ALT_DEFMAX,
                   ISNULL(SPCFILE.SPC_NON_LOAN_FLAG,''),
                   ISNULL(SPCFILE.SPC_SKIP_TRW,'')
            INTO :QSPC-REC
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
       WRITE-SPC1-FILE.

           PERFORM WRITE-IT.
           MOVE SPC-SELECT-PATH TO E-FILE.
           MOVE SPC1-KEY TO E-KEYX.
           PERFORM SET-SPC-FIELDS.

           EXEC SQL
            INSERT INTO DBO.SPCFILE
             (SPCFILE.SPC_ORGST,
              SPCFILE.SPC_SPRCLASS,
              SPCFILE.SPC_SUBCLASS,
              SPCFILE.SPC_LAWCODE,
              SPCFILE.SPC_INSEARN_ORDER_1,
              SPCFILE.SPC_INSEARN_ORDER_2,
              SPCFILE.SPC_INSEARN_ORDER_3,
              SPCFILE.SPC_INSEARN_ORDER_4,
              SPCFILE.SPC_INSEARN_ORDER_5,
              SPCFILE.SPC_INSEARN_ORDER_6,
              SPCFILE.SPC_INSEARN_ORDER_7,
              SPCFILE.SPC_INSEARN_ORDER_8,
              SPCFILE.SPC_INSEARN_ORDER_9,
              SPCFILE.SPC_INSEARN_ORDER_10,
              SPCFILE.SPC_INSEARN_ORDER_11,
              SPCFILE.SPC_INSEARN_ORDER_12,
              SPCFILE.SPC_INSAH_JOINT_FG,
              SPCFILE.SPC_INS_O1_EDIT,
              SPCFILE.SPC_INS_O2_EDIT,
              SPCFILE.SPC_RBFRMLA_1,
              SPCFILE.SPC_RBDTE_1_1,
              SPCFILE.SPC_RBDAYS_1_1,
              SPCFILE.SPC_RBYRTYPE_1_1,
              SPCFILE.SPC_RBDTE_1_2,
              SPCFILE.SPC_RBDAYS_1_2,
              SPCFILE.SPC_RBYRTYPE_1_2,
              SPCFILE.SPC_RBMINRET_1,
              SPCFILE.SPC_RBNOMON_1,
              SPCFILE.SPC_RBDEDCHG_1,
              SPCFILE.SPC_RBRECAST_1,
              SPCFILE.SPC_RBEARLY_1,
              SPCFILE.SPC_RBDEFER_1,
              SPCFILE.SPC_RBPCT_1,
              SPCFILE.SPC_RBREDUC_1,
              SPCFILE.SPC_RBMIN_1,
              SPCFILE.SPC_RBSPOPT1_1,
              SPCFILE.SPC_RBSPOPT2_1,
              SPCFILE.SPC_RBMANAGER_1,
              SPCFILE.SPC_RBUNITPER_CD_1,
              SPCFILE.SPC_RBFRMLA2_1,
              SPCFILE.SPC_RBFRMLA_2,
              SPCFILE.SPC_RBDTE_2_1,
              SPCFILE.SPC_RBDAYS_2_1,
              SPCFILE.SPC_RBYRTYPE_2_1,
              SPCFILE.SPC_RBDTE_2_2,
              SPCFILE.SPC_RBDAYS_2_2,
              SPCFILE.SPC_RBYRTYPE_2_2,
              SPCFILE.SPC_RBMINRET_2,
              SPCFILE.SPC_RBNOMON_2,
              SPCFILE.SPC_RBDEDCHG_2,
              SPCFILE.SPC_RBRECAST_2,
              SPCFILE.SPC_RBEARLY_2,
              SPCFILE.SPC_RBDEFER_2,
              SPCFILE.SPC_RBPCT_2,
              SPCFILE.SPC_RBREDUC_2,
              SPCFILE.SPC_RBMIN_2,
              SPCFILE.SPC_RBSPOPT1_2,
              SPCFILE.SPC_RBSPOPT2_2,
              SPCFILE.SPC_RBMANAGER_2,
              SPCFILE.SPC_RBUNITPER_CD_2,
              SPCFILE.SPC_RBFRMLA2_2,
              SPCFILE.SPC_RBFRMLA_3,
              SPCFILE.SPC_RBDTE_3_1,
              SPCFILE.SPC_RBDAYS_3_1,
              SPCFILE.SPC_RBYRTYPE_3_1,
              SPCFILE.SPC_RBDTE_3_2,
              SPCFILE.SPC_RBDAYS_3_2,
              SPCFILE.SPC_RBYRTYPE_3_2,
              SPCFILE.SPC_RBMINRET_3,
              SPCFILE.SPC_RBNOMON_3,
              SPCFILE.SPC_RBDEDCHG_3,
              SPCFILE.SPC_RBRECAST_3,
              SPCFILE.SPC_RBEARLY_3,
              SPCFILE.SPC_RBDEFER_3,
              SPCFILE.SPC_RBPCT_3,
              SPCFILE.SPC_RBREDUC_3,
              SPCFILE.SPC_RBMIN_3,
              SPCFILE.SPC_RBSPOPT1_3,
              SPCFILE.SPC_RBSPOPT2_3,
              SPCFILE.SPC_RBMANAGER_3,
              SPCFILE.SPC_RBUNITPER_CD_3,
              SPCFILE.SPC_RBFRMLA2_3,
              SPCFILE.SPC_RBFRMLA_4,
              SPCFILE.SPC_RBDTE_4_1,
              SPCFILE.SPC_RBDAYS_4_1,
              SPCFILE.SPC_RBYRTYPE_4_1,
              SPCFILE.SPC_RBDTE_4_2,
              SPCFILE.SPC_RBDAYS_4_2,
              SPCFILE.SPC_RBYRTYPE_4_2,
              SPCFILE.SPC_RBMINRET_4,
              SPCFILE.SPC_RBNOMON_4,
              SPCFILE.SPC_RBDEDCHG_4,
              SPCFILE.SPC_RBRECAST_4,
              SPCFILE.SPC_RBEARLY_4,
              SPCFILE.SPC_RBDEFER_4,
              SPCFILE.SPC_RBPCT_4,
              SPCFILE.SPC_RBREDUC_4,
              SPCFILE.SPC_RBMIN_4,
              SPCFILE.SPC_RBSPOPT1_4,
              SPCFILE.SPC_RBSPOPT2_4,
              SPCFILE.SPC_RBMANAGER_4,
              SPCFILE.SPC_RBUNITPER_CD_4,
              SPCFILE.SPC_RBFRMLA2_4,
              SPCFILE.SPC_RBFRMLA_5,
              SPCFILE.SPC_RBDTE_5_1,
              SPCFILE.SPC_RBDAYS_5_1,
              SPCFILE.SPC_RBYRTYPE_5_1,
              SPCFILE.SPC_RBDTE_5_2,
              SPCFILE.SPC_RBDAYS_5_2,
              SPCFILE.SPC_RBYRTYPE_5_2,
              SPCFILE.SPC_RBMINRET_5,
              SPCFILE.SPC_RBNOMON_5,
              SPCFILE.SPC_RBDEDCHG_5,
              SPCFILE.SPC_RBRECAST_5,
              SPCFILE.SPC_RBEARLY_5,
              SPCFILE.SPC_RBDEFER_5,
              SPCFILE.SPC_RBPCT_5,
              SPCFILE.SPC_RBREDUC_5,
              SPCFILE.SPC_RBMIN_5,
              SPCFILE.SPC_RBSPOPT1_5,
              SPCFILE.SPC_RBSPOPT2_5,
              SPCFILE.SPC_RBMANAGER_5,
              SPCFILE.SPC_RBUNITPER_CD_5,
              SPCFILE.SPC_RBFRMLA2_5,
              SPCFILE.SPC_RBFRMLA_6,
              SPCFILE.SPC_RBDTE_6_1,
              SPCFILE.SPC_RBDAYS_6_1,
              SPCFILE.SPC_RBYRTYPE_6_1,
              SPCFILE.SPC_RBDTE_6_2,
              SPCFILE.SPC_RBDAYS_6_2,
              SPCFILE.SPC_RBYRTYPE_6_2,
              SPCFILE.SPC_RBMINRET_6,
              SPCFILE.SPC_RBNOMON_6,
              SPCFILE.SPC_RBDEDCHG_6,
              SPCFILE.SPC_RBRECAST_6,
              SPCFILE.SPC_RBEARLY_6,
              SPCFILE.SPC_RBDEFER_6,
              SPCFILE.SPC_RBPCT_6,
              SPCFILE.SPC_RBREDUC_6,
              SPCFILE.SPC_RBMIN_6,
              SPCFILE.SPC_RBSPOPT1_6,
              SPCFILE.SPC_RBSPOPT2_6,
              SPCFILE.SPC_RBMANAGER_6,
              SPCFILE.SPC_RBUNITPER_CD_6,
              SPCFILE.SPC_RBFRMLA2_6,
              SPCFILE.SPC_RBFRMLA_7,
              SPCFILE.SPC_RBDTE_7_1,
              SPCFILE.SPC_RBDAYS_7_1,
              SPCFILE.SPC_RBYRTYPE_7_1,
              SPCFILE.SPC_RBDTE_7_2,
              SPCFILE.SPC_RBDAYS_7_2,
              SPCFILE.SPC_RBYRTYPE_7_2,
              SPCFILE.SPC_RBMINRET_7,
              SPCFILE.SPC_RBNOMON_7,
              SPCFILE.SPC_RBDEDCHG_7,
              SPCFILE.SPC_RBRECAST_7,
              SPCFILE.SPC_RBEARLY_7,
              SPCFILE.SPC_RBDEFER_7,
              SPCFILE.SPC_RBPCT_7,
              SPCFILE.SPC_RBREDUC_7,
              SPCFILE.SPC_RBMIN_7,
              SPCFILE.SPC_RBSPOPT1_7,
              SPCFILE.SPC_RBSPOPT2_7,
              SPCFILE.SPC_RBMANAGER_7,
              SPCFILE.SPC_RBUNITPER_CD_7,
              SPCFILE.SPC_RBFRMLA2_7,
              SPCFILE.SPC_RBFRMLA_8,
              SPCFILE.SPC_RBDTE_8_1,
              SPCFILE.SPC_RBDAYS_8_1,
              SPCFILE.SPC_RBYRTYPE_8_1,
              SPCFILE.SPC_RBDTE_8_2,
              SPCFILE.SPC_RBDAYS_8_2,
              SPCFILE.SPC_RBYRTYPE_8_2,
              SPCFILE.SPC_RBMINRET_8,
              SPCFILE.SPC_RBNOMON_8,
              SPCFILE.SPC_RBDEDCHG_8,
              SPCFILE.SPC_RBRECAST_8,
              SPCFILE.SPC_RBEARLY_8,
              SPCFILE.SPC_RBDEFER_8,
              SPCFILE.SPC_RBPCT_8,
              SPCFILE.SPC_RBREDUC_8,
              SPCFILE.SPC_RBMIN_8,
              SPCFILE.SPC_RBSPOPT1_8,
              SPCFILE.SPC_RBSPOPT2_8,
              SPCFILE.SPC_RBMANAGER_8,
              SPCFILE.SPC_RBUNITPER_CD_8,
              SPCFILE.SPC_RBFRMLA2_8,
              SPCFILE.SPC_RBFRMLA_9,
              SPCFILE.SPC_RBDTE_9_1,
              SPCFILE.SPC_RBDAYS_9_1,
              SPCFILE.SPC_RBYRTYPE_9_1,
              SPCFILE.SPC_RBDTE_9_2,
              SPCFILE.SPC_RBDAYS_9_2,
              SPCFILE.SPC_RBYRTYPE_9_2,
              SPCFILE.SPC_RBMINRET_9,
              SPCFILE.SPC_RBNOMON_9,
              SPCFILE.SPC_RBDEDCHG_9,
              SPCFILE.SPC_RBRECAST_9,
              SPCFILE.SPC_RBEARLY_9,
              SPCFILE.SPC_RBDEFER_9,
              SPCFILE.SPC_RBPCT_9,
              SPCFILE.SPC_RBREDUC_9,
              SPCFILE.SPC_RBMIN_9,
              SPCFILE.SPC_RBSPOPT1_9,
              SPCFILE.SPC_RBSPOPT2_9,
              SPCFILE.SPC_RBMANAGER_9,
              SPCFILE.SPC_RBUNITPER_CD_9,
              SPCFILE.SPC_RBFRMLA2_9,
              SPCFILE.SPC_RBFRMLA_10,
              SPCFILE.SPC_RBDTE_10_1,
              SPCFILE.SPC_RBDAYS_10_1,
              SPCFILE.SPC_RBYRTYPE_10_1,
              SPCFILE.SPC_RBDTE_10_2,
              SPCFILE.SPC_RBDAYS_10_2,
              SPCFILE.SPC_RBYRTYPE_10_2,
              SPCFILE.SPC_RBMINRET_10,
              SPCFILE.SPC_RBNOMON_10,
              SPCFILE.SPC_RBDEDCHG_10,
              SPCFILE.SPC_RBRECAST_10,
              SPCFILE.SPC_RBEARLY_10,
              SPCFILE.SPC_RBDEFER_10,
              SPCFILE.SPC_RBPCT_10,
              SPCFILE.SPC_RBREDUC_10,
              SPCFILE.SPC_RBMIN_10,
              SPCFILE.SPC_RBSPOPT1_10,
              SPCFILE.SPC_RBSPOPT2_10,
              SPCFILE.SPC_RBMANAGER_10,
              SPCFILE.SPC_RBUNITPER_CD_10,
              SPCFILE.SPC_RBFRMLA2_10,
              SPCFILE.SPC_RBFRMLA_11,
              SPCFILE.SPC_RBDTE_11_1,
              SPCFILE.SPC_RBDAYS_11_1,
              SPCFILE.SPC_RBYRTYPE_11_1,
              SPCFILE.SPC_RBDTE_11_2,
              SPCFILE.SPC_RBDAYS_11_2,
              SPCFILE.SPC_RBYRTYPE_11_2,
              SPCFILE.SPC_RBMINRET_11,
              SPCFILE.SPC_RBNOMON_11,
              SPCFILE.SPC_RBDEDCHG_11,
              SPCFILE.SPC_RBRECAST_11,
              SPCFILE.SPC_RBEARLY_11,
              SPCFILE.SPC_RBDEFER_11,
              SPCFILE.SPC_RBPCT_11,
              SPCFILE.SPC_RBREDUC_11,
              SPCFILE.SPC_RBMIN_11,
              SPCFILE.SPC_RBSPOPT1_11,
              SPCFILE.SPC_RBSPOPT2_11,
              SPCFILE.SPC_RBMANAGER_11,
              SPCFILE.SPC_RBUNITPER_CD_11,
              SPCFILE.SPC_RBFRMLA2_11,
              SPCFILE.SPC_RBFRMLA_12,
              SPCFILE.SPC_RBDTE_12_1,
              SPCFILE.SPC_RBDAYS_12_1,
              SPCFILE.SPC_RBYRTYPE_12_1,
              SPCFILE.SPC_RBDTE_12_2,
              SPCFILE.SPC_RBDAYS_12_2,
              SPCFILE.SPC_RBYRTYPE_12_2,
              SPCFILE.SPC_RBMINRET_12,
              SPCFILE.SPC_RBNOMON_12,
              SPCFILE.SPC_RBDEDCHG_12,
              SPCFILE.SPC_RBRECAST_12,
              SPCFILE.SPC_RBEARLY_12,
              SPCFILE.SPC_RBDEFER_12,
              SPCFILE.SPC_RBPCT_12,
              SPCFILE.SPC_RBREDUC_12,
              SPCFILE.SPC_RBMIN_12,
              SPCFILE.SPC_RBSPOPT1_12,
              SPCFILE.SPC_RBSPOPT2_12,
              SPCFILE.SPC_RBMANAGER_12,
              SPCFILE.SPC_RBUNITPER_CD_12,
              SPCFILE.SPC_RBFRMLA2_12,
              SPCFILE.SPC_RBFRMLA_13,
              SPCFILE.SPC_RBDTE_13_1,
              SPCFILE.SPC_RBDAYS_13_1,
              SPCFILE.SPC_RBYRTYPE_13_1,
              SPCFILE.SPC_RBDTE_13_2,
              SPCFILE.SPC_RBDAYS_13_2,
              SPCFILE.SPC_RBYRTYPE_13_2,
              SPCFILE.SPC_RBMINRET_13,
              SPCFILE.SPC_RBNOMON_13,
              SPCFILE.SPC_RBDEDCHG_13,
              SPCFILE.SPC_RBRECAST_13,
              SPCFILE.SPC_RBEARLY_13,
              SPCFILE.SPC_RBDEFER_13,
              SPCFILE.SPC_RBPCT_13,
              SPCFILE.SPC_RBREDUC_13,
              SPCFILE.SPC_RBMIN_13,
              SPCFILE.SPC_RBSPOPT1_13,
              SPCFILE.SPC_RBSPOPT2_13,
              SPCFILE.SPC_RBMANAGER_13,
              SPCFILE.SPC_RBUNITPER_CD_13,
              SPCFILE.SPC_RBFRMLA2_13,
              SPCFILE.SPC_RBFRMLA_14,
              SPCFILE.SPC_RBDTE_14_1,
              SPCFILE.SPC_RBDAYS_14_1,
              SPCFILE.SPC_RBYRTYPE_14_1,
              SPCFILE.SPC_RBDTE_14_2,
              SPCFILE.SPC_RBDAYS_14_2,
              SPCFILE.SPC_RBYRTYPE_14_2,
              SPCFILE.SPC_RBMINRET_14,
              SPCFILE.SPC_RBNOMON_14,
              SPCFILE.SPC_RBDEDCHG_14,
              SPCFILE.SPC_RBRECAST_14,
              SPCFILE.SPC_RBEARLY_14,
              SPCFILE.SPC_RBDEFER_14,
              SPCFILE.SPC_RBPCT_14,
              SPCFILE.SPC_RBREDUC_14,
              SPCFILE.SPC_RBMIN_14,
              SPCFILE.SPC_RBSPOPT1_14,
              SPCFILE.SPC_RBSPOPT2_14,
              SPCFILE.SPC_RBMANAGER_14,
              SPCFILE.SPC_RBUNITPER_CD_14,
              SPCFILE.SPC_RBFRMLA2_14,
              SPCFILE.SPC_RBFRMLA_15,
              SPCFILE.SPC_RBDTE_15_1,
              SPCFILE.SPC_RBDAYS_15_1,
              SPCFILE.SPC_RBYRTYPE_15_1,
              SPCFILE.SPC_RBDTE_15_2,
              SPCFILE.SPC_RBDAYS_15_2,
              SPCFILE.SPC_RBYRTYPE_15_2,
              SPCFILE.SPC_RBMINRET_15,
              SPCFILE.SPC_RBNOMON_15,
              SPCFILE.SPC_RBDEDCHG_15,
              SPCFILE.SPC_RBRECAST_15,
              SPCFILE.SPC_RBEARLY_15,
              SPCFILE.SPC_RBDEFER_15,
              SPCFILE.SPC_RBPCT_15,
              SPCFILE.SPC_RBREDUC_15,
              SPCFILE.SPC_RBMIN_15,
              SPCFILE.SPC_RBSPOPT1_15,
              SPCFILE.SPC_RBSPOPT2_15,
              SPCFILE.SPC_RBMANAGER_15,
              SPCFILE.SPC_RBUNITPER_CD_15,
              SPCFILE.SPC_RBFRMLA2_15,
              SPCFILE.SPC_RBFRMLA_16,
              SPCFILE.SPC_RBDTE_16_1,
              SPCFILE.SPC_RBDAYS_16_1,
              SPCFILE.SPC_RBYRTYPE_16_1,
              SPCFILE.SPC_RBDTE_16_2,
              SPCFILE.SPC_RBDAYS_16_2,
              SPCFILE.SPC_RBYRTYPE_16_2,
              SPCFILE.SPC_RBMINRET_16,
              SPCFILE.SPC_RBNOMON_16,
              SPCFILE.SPC_RBDEDCHG_16,
              SPCFILE.SPC_RBRECAST_16,
              SPCFILE.SPC_RBEARLY_16,
              SPCFILE.SPC_RBDEFER_16,
              SPCFILE.SPC_RBPCT_16,
              SPCFILE.SPC_RBREDUC_16,
              SPCFILE.SPC_RBMIN_16,
              SPCFILE.SPC_RBSPOPT1_16,
              SPCFILE.SPC_RBSPOPT2_16,
              SPCFILE.SPC_RBMANAGER_16,
              SPCFILE.SPC_RBUNITPER_CD_16,
              SPCFILE.SPC_RBFRMLA2_16,
              SPCFILE.SPC_RBFRMLA_17,
              SPCFILE.SPC_RBDTE_17_1,
              SPCFILE.SPC_RBDAYS_17_1,
              SPCFILE.SPC_RBYRTYPE_17_1,
              SPCFILE.SPC_RBDTE_17_2,
              SPCFILE.SPC_RBDAYS_17_2,
              SPCFILE.SPC_RBYRTYPE_17_2,
              SPCFILE.SPC_RBMINRET_17,
              SPCFILE.SPC_RBNOMON_17,
              SPCFILE.SPC_RBDEDCHG_17,
              SPCFILE.SPC_RBRECAST_17,
              SPCFILE.SPC_RBEARLY_17,
              SPCFILE.SPC_RBDEFER_17,
              SPCFILE.SPC_RBPCT_17,
              SPCFILE.SPC_RBREDUC_17,
              SPCFILE.SPC_RBMIN_17,
              SPCFILE.SPC_RBSPOPT1_17,
              SPCFILE.SPC_RBSPOPT2_17,
              SPCFILE.SPC_RBMANAGER_17,
              SPCFILE.SPC_RBUNITPER_CD_17,
              SPCFILE.SPC_RBFRMLA2_17,
              SPCFILE.SPC_RBFRMLA_18,
              SPCFILE.SPC_RBDTE_18_1,
              SPCFILE.SPC_RBDAYS_18_1,
              SPCFILE.SPC_RBYRTYPE_18_1,
              SPCFILE.SPC_RBDTE_18_2,
              SPCFILE.SPC_RBDAYS_18_2,
              SPCFILE.SPC_RBYRTYPE_18_2,
              SPCFILE.SPC_RBMINRET_18,
              SPCFILE.SPC_RBNOMON_18,
              SPCFILE.SPC_RBDEDCHG_18,
              SPCFILE.SPC_RBRECAST_18,
              SPCFILE.SPC_RBEARLY_18,
              SPCFILE.SPC_RBDEFER_18,
              SPCFILE.SPC_RBPCT_18,
              SPCFILE.SPC_RBREDUC_18,
              SPCFILE.SPC_RBMIN_18,
              SPCFILE.SPC_RBSPOPT1_18,
              SPCFILE.SPC_RBSPOPT2_18,
              SPCFILE.SPC_RBMANAGER_18,
              SPCFILE.SPC_RBUNITPER_CD_18,
              SPCFILE.SPC_RBFRMLA2_18,
              SPCFILE.SPC_RBFRMLA_19,
              SPCFILE.SPC_RBDTE_19_1,
              SPCFILE.SPC_RBDAYS_19_1,
              SPCFILE.SPC_RBYRTYPE_19_1,
              SPCFILE.SPC_RBDTE_19_2,
              SPCFILE.SPC_RBDAYS_19_2,
              SPCFILE.SPC_RBYRTYPE_19_2,
              SPCFILE.SPC_RBMINRET_19,
              SPCFILE.SPC_RBNOMON_19,
              SPCFILE.SPC_RBDEDCHG_19,
              SPCFILE.SPC_RBRECAST_19,
              SPCFILE.SPC_RBEARLY_19,
              SPCFILE.SPC_RBDEFER_19,
              SPCFILE.SPC_RBPCT_19,
              SPCFILE.SPC_RBREDUC_19,
              SPCFILE.SPC_RBMIN_19,
              SPCFILE.SPC_RBSPOPT1_19,
              SPCFILE.SPC_RBSPOPT2_19,
              SPCFILE.SPC_RBMANAGER_19,
              SPCFILE.SPC_RBUNITPER_CD_19,
              SPCFILE.SPC_RBFRMLA2_19,
              SPCFILE.SPC_RBFRMLA_20,
              SPCFILE.SPC_RBDTE_20_1,
              SPCFILE.SPC_RBDAYS_20_1,
              SPCFILE.SPC_RBYRTYPE_20_1,
              SPCFILE.SPC_RBDTE_20_2,
              SPCFILE.SPC_RBDAYS_20_2,
              SPCFILE.SPC_RBYRTYPE_20_2,
              SPCFILE.SPC_RBMINRET_20,
              SPCFILE.SPC_RBNOMON_20,
              SPCFILE.SPC_RBDEDCHG_20,
              SPCFILE.SPC_RBRECAST_20,
              SPCFILE.SPC_RBEARLY_20,
              SPCFILE.SPC_RBDEFER_20,
              SPCFILE.SPC_RBPCT_20,
              SPCFILE.SPC_RBREDUC_20,
              SPCFILE.SPC_RBMIN_20,
              SPCFILE.SPC_RBSPOPT1_20,
              SPCFILE.SPC_RBSPOPT2_20,
              SPCFILE.SPC_RBMANAGER_20,
              SPCFILE.SPC_RBUNITPER_CD_20,
              SPCFILE.SPC_RBFRMLA2_20,
              SPCFILE.SPC_RBFRMLA_21,
              SPCFILE.SPC_RBDTE_21_1,
              SPCFILE.SPC_RBDAYS_21_1,
              SPCFILE.SPC_RBYRTYPE_21_1,
              SPCFILE.SPC_RBDTE_21_2,
              SPCFILE.SPC_RBDAYS_21_2,
              SPCFILE.SPC_RBYRTYPE_21_2,
              SPCFILE.SPC_RBMINRET_21,
              SPCFILE.SPC_RBNOMON_21,
              SPCFILE.SPC_RBDEDCHG_21,
              SPCFILE.SPC_RBRECAST_21,
              SPCFILE.SPC_RBEARLY_21,
              SPCFILE.SPC_RBDEFER_21,
              SPCFILE.SPC_RBPCT_21,
              SPCFILE.SPC_RBREDUC_21,
              SPCFILE.SPC_RBMIN_21,
              SPCFILE.SPC_RBSPOPT1_21,
              SPCFILE.SPC_RBSPOPT2_21,
              SPCFILE.SPC_RBMANAGER_21,
              SPCFILE.SPC_RBUNITPER_CD_21,
              SPCFILE.SPC_RBFRMLA2_21,
              SPCFILE.SPC_RBFRMLA_22,
              SPCFILE.SPC_RBDTE_22_1,
              SPCFILE.SPC_RBDAYS_22_1,
              SPCFILE.SPC_RBYRTYPE_22_1,
              SPCFILE.SPC_RBDTE_22_2,
              SPCFILE.SPC_RBDAYS_22_2,
              SPCFILE.SPC_RBYRTYPE_22_2,
              SPCFILE.SPC_RBMINRET_22,
              SPCFILE.SPC_RBNOMON_22,
              SPCFILE.SPC_RBDEDCHG_22,
              SPCFILE.SPC_RBRECAST_22,
              SPCFILE.SPC_RBEARLY_22,
              SPCFILE.SPC_RBDEFER_22,
              SPCFILE.SPC_RBPCT_22,
              SPCFILE.SPC_RBREDUC_22,
              SPCFILE.SPC_RBMIN_22,
              SPCFILE.SPC_RBSPOPT1_22,
              SPCFILE.SPC_RBSPOPT2_22,
              SPCFILE.SPC_RBMANAGER_22,
              SPCFILE.SPC_RBUNITPER_CD_22,
              SPCFILE.SPC_RBFRMLA2_22,
              SPCFILE.SPC_RBCOMCD,
              SPCFILE.SPC_ADDON_DISFRMLA,
              SPCFILE.SPC_ADDON_WHOLE_FG,
              SPCFILE.SPC_ADDON_ORG_MATURITY,
              SPCFILE.SPC_CONSOLIDATE_RT_FG,
              SPCFILE.SPC_1STPY_ADDON_DAYS,
              SPCFILE.SPC_1STPY_ADDON_YRTYPE,
              SPCFILE.SPC_ADDON_RATE_FG,
              SPCFILE.SPC_ADDON_RATE,
              SPCFILE.SPC_FEEFRMLA_1,
              SPCFILE.SPC_FEECD_1,
              SPCFILE.SPC_FEEID_1,
              SPCFILE.SPC_FEEFRMLA_2,
              SPCFILE.SPC_FEECD_2,
              SPCFILE.SPC_FEEID_2,
              SPCFILE.SPC_ROUND_PAY_FG,
              SPCFILE.SPC_IRREGULAR_PAY_FG,
              SPCFILE.SPC_DLRVCHGS_RB_CD_1,
              SPCFILE.SPC_DLRVCHGS_RB_CD_2,
              SPCFILE.SPC_DLRVCHGS_RB_CD_3,
              SPCFILE.SPC_PARVHELD_RB_CD,
              SPCFILE.SPC_PARVPAID_RB_CD,
              SPCFILE.SPC_CONTR_CREDIT,
              SPCFILE.SPC_CONTR_CREDIT_CD,
              SPCFILE.SPC_CONTR_CREDIT_MIN_DEL_PER,
              SPCFILE.SPC_OPT_CAL_CHGABLE,
              SPCFILE.SPC_PBDEL_DAYS,
              SPCFILE.SPC_PBINCR_PERCENT,
              SPCFILE.SPC_CPOT30_CD,
              SPCFILE.SPC_CPOTCHGOFF_CD,
              SPCFILE.SPC_RDPOT30_CD,
              SPCFILE.SPC_RDPOTCHGOFF_CD,
              SPCFILE.SPC_MAX_ALLOW_DELINQ,
              SPCFILE.SPC_RESCIND_BUS_DAYS,
              SPCFILE.SPC_RESCIND_SATURDAY,
              SPCFILE.SPC_MIN_CREDIT_SCORE,
              SPCFILE.SPC_DUAL_IN_BRANCH,
              SPCFILE.SPC_DUAL_IN_STATE,
              SPCFILE.SPC_PACESETTER_FG,
              SPCFILE.SPC_WAGE_ASSIGN_DAYS,
              SPCFILE.SPC_CAL_PAYMENT_ROUND_FG,
              SPCFILE.SPC_CAN_LOANS_FG,
              SPCFILE.SPC_ALLOW_DELINQ_POLCD,
              SPCFILE.SPC_YTD_MAX_ALLOW_DELINQ,
              SPCFILE.SPC_LNCOST_FRMLA,
              SPCFILE.SPC_LNCOST_AMT,
              SPCFILE.SPC_DLDISC_RVPRIN1_PCNT,
              SPCFILE.SPC_RENEWABLE_PAYS,
              SPCFILE.SPC_RENEWABLE_DAYS,
              SPCFILE.SPC_ALLOW_LN_BACK_DATE,
              SPCFILE.SPC_DLDISC_RVPRIN1_AF_PCNT,
              SPCFILE.SPC_CAL_MAX_AT_ESTIMATE,
              SPCFILE.SPC_INSAH_RETRO,
              SPCFILE.SPC_INSAH_DAYS_WAIT,
              SPCFILE.SPC_ADDON_EXCEPT_FG,
              SPCFILE.SPC_REPO_TRCD_1,
              SPCFILE.SPC_REPO_FMNO_1_1,
              SPCFILE.SPC_REPO_FMNO_1_2,
              SPCFILE.SPC_REPO_FMNO_1_3,
              SPCFILE.SPC_REPO_FMNO_1_4,
              SPCFILE.SPC_REPO_TRCD_2,
              SPCFILE.SPC_REPO_FMNO_2_1,
              SPCFILE.SPC_REPO_FMNO_2_2,
              SPCFILE.SPC_REPO_FMNO_2_3,
              SPCFILE.SPC_REPO_FMNO_2_4,
              SPCFILE.SPC_REPO_TRCD_3,
              SPCFILE.SPC_REPO_FMNO_3_1,
              SPCFILE.SPC_REPO_FMNO_3_2,
              SPCFILE.SPC_REPO_FMNO_3_3,
              SPCFILE.SPC_REPO_FMNO_3_4,
              SPCFILE.SPC_CANCEL_ADDON_FG,
              SPCFILE.SPC_DEFAULT_O2_INS_COMP,
              SPCFILE.SPC_LC_ON_JUDGEMENTS,
              SPCFILE.SPC_DELQ_IB_AGE,
              SPCFILE.SPC_DELQ_IB_MAX_DAYS,
              SPCFILE.SPC_INSURANCE_WRITTEN,
              SPCFILE.SPC_INTERIM_COUPON,
              SPCFILE.SPC_AUTO_ASSESS_LCHG,
              SPCFILE.SPC_MAX_CASH_PROCEEDS,
              SPCFILE.SPC_ALLOW_ZERO_PROC,
              SPCFILE.SPC_BANKRULE_RATE,
              SPCFILE.SPC_AUTO_SEND_TRCD_1,
              SPCFILE.SPC_AUTO_SEND_FMNO_1_1,
              SPCFILE.SPC_AUTO_SEND_FMNO_1_2,
              SPCFILE.SPC_AUTO_SEND_FMNO_1_3,
              SPCFILE.SPC_AUTO_SEND_FMNO_1_4,
              SPCFILE.SPC_AUTO_SEND_TRCD_2,
              SPCFILE.SPC_AUTO_SEND_FMNO_2_1,
              SPCFILE.SPC_AUTO_SEND_FMNO_2_2,
              SPCFILE.SPC_AUTO_SEND_FMNO_2_3,
              SPCFILE.SPC_AUTO_SEND_FMNO_2_4,
              SPCFILE.SPC_DLDISC_CONV_BF_PCNT,
              SPCFILE.SPC_UNITPER_CD,
              SPCFILE.SPC_RENEWAL_EDIT,
              SPCFILE.SPC_ROUND_MFEE,
              SPCFILE.SPC_ADV_TO_1ST_NO_CORR,
              SPCFILE.SPC_LC_INT_ON_REPO_NON_PL,
              SPCFILE.SPC_CONTRACTUAL_SET_NONACCRUAL,
              SPCFILE.SPC_CONTRACTUAL_RESET_ACCRUAL,
              SPCFILE.SPC_MONTHS_PAY_NAPP_COUNTER,
              SPCFILE.SPC_SHORT_PAY_THRESHOLD,
              SPCFILE.SPC_CASH_AVAIL_PERCENT,
              SPCFILE.SPC_O1_JOINT_FG,
              SPCFILE.SPC_NET_BAL_REQ,
              SPCFILE.SPC_AMT_PROCEEDS_REQ,
              SPCFILE.SPC_ALT_PREPAY_MONTHS,
              SPCFILE.SPC_PERCENT_REGPAY_AVAIL,
              SPCFILE.SPC_ALT_DEFFRMLA,
              SPCFILE.SPC_ALT_DEFMIN,
              SPCFILE.SPC_ALT_DEFMAX,
              SPCFILE.SPC_NON_LOAN_FLAG,
              SPCFILE.SPC_SKIP_TRW)
            VALUES
             (:QSPC-ORGST,
              :QSPC-SPRCLASS,
              :QSPC-SUBCLASS,
              :QSPC-LAWCODE,
              :QSPC-INSEARN-ORDER-1,
              :QSPC-INSEARN-ORDER-2,
              :QSPC-INSEARN-ORDER-3,
              :QSPC-INSEARN-ORDER-4,
              :QSPC-INSEARN-ORDER-5,
              :QSPC-INSEARN-ORDER-6,
              :QSPC-INSEARN-ORDER-7,
              :QSPC-INSEARN-ORDER-8,
              :QSPC-INSEARN-ORDER-9,
              :QSPC-INSEARN-ORDER-10,
              :QSPC-INSEARN-ORDER-11,
              :QSPC-INSEARN-ORDER-12,
              :QSPC-INSAH-JOINT-FG,
              :QSPC-INS-O1-EDIT,
              :QSPC-INS-O2-EDIT,
              :QSPC-RBFRMLA-1,
              :QSPC-RBDTE-1-1,
              :QSPC-RBDAYS-1-1,
              :QSPC-RBYRTYPE-1-1,
              :QSPC-RBDTE-1-2,
              :QSPC-RBDAYS-1-2,
              :QSPC-RBYRTYPE-1-2,
              :QSPC-RBMINRET-1,
              :QSPC-RBNOMON-1,
              :QSPC-RBDEDCHG-1,
              :QSPC-RBRECAST-1,
              :QSPC-RBEARLY-1,
              :QSPC-RBDEFER-1,
              :QSPC-RBPCT-1,
              :QSPC-RBREDUC-1,
              :QSPC-RBMIN-1,
              :QSPC-RBSPOPT1-1,
              :QSPC-RBSPOPT2-1,
              :QSPC-RBMANAGER-1,
              :QSPC-RBUNITPER-CD-1,
              :QSPC-RBFRMLA2-1,
              :QSPC-RBFRMLA-2,
              :QSPC-RBDTE-2-1,
              :QSPC-RBDAYS-2-1,
              :QSPC-RBYRTYPE-2-1,
              :QSPC-RBDTE-2-2,
              :QSPC-RBDAYS-2-2,
              :QSPC-RBYRTYPE-2-2,
              :QSPC-RBMINRET-2,
              :QSPC-RBNOMON-2,
              :QSPC-RBDEDCHG-2,
              :QSPC-RBRECAST-2,
              :QSPC-RBEARLY-2,
              :QSPC-RBDEFER-2,
              :QSPC-RBPCT-2,
              :QSPC-RBREDUC-2,
              :QSPC-RBMIN-2,
              :QSPC-RBSPOPT1-2,
              :QSPC-RBSPOPT2-2,
              :QSPC-RBMANAGER-2,
              :QSPC-RBUNITPER-CD-2,
              :QSPC-RBFRMLA2-2,
              :QSPC-RBFRMLA-3,
              :QSPC-RBDTE-3-1,
              :QSPC-RBDAYS-3-1,
              :QSPC-RBYRTYPE-3-1,
              :QSPC-RBDTE-3-2,
              :QSPC-RBDAYS-3-2,
              :QSPC-RBYRTYPE-3-2,
              :QSPC-RBMINRET-3,
              :QSPC-RBNOMON-3,
              :QSPC-RBDEDCHG-3,
              :QSPC-RBRECAST-3,
              :QSPC-RBEARLY-3,
              :QSPC-RBDEFER-3,
              :QSPC-RBPCT-3,
              :QSPC-RBREDUC-3,
              :QSPC-RBMIN-3,
              :QSPC-RBSPOPT1-3,
              :QSPC-RBSPOPT2-3,
              :QSPC-RBMANAGER-3,
              :QSPC-RBUNITPER-CD-3,
              :QSPC-RBFRMLA2-3,
              :QSPC-RBFRMLA-4,
              :QSPC-RBDTE-4-1,
              :QSPC-RBDAYS-4-1,
              :QSPC-RBYRTYPE-4-1,
              :QSPC-RBDTE-4-2,
              :QSPC-RBDAYS-4-2,
              :QSPC-RBYRTYPE-4-2,
              :QSPC-RBMINRET-4,
              :QSPC-RBNOMON-4,
              :QSPC-RBDEDCHG-4,
              :QSPC-RBRECAST-4,
              :QSPC-RBEARLY-4,
              :QSPC-RBDEFER-4,
              :QSPC-RBPCT-4,
              :QSPC-RBREDUC-4,
              :QSPC-RBMIN-4,
              :QSPC-RBSPOPT1-4,
              :QSPC-RBSPOPT2-4,
              :QSPC-RBMANAGER-4,
              :QSPC-RBUNITPER-CD-4,
              :QSPC-RBFRMLA2-4,
              :QSPC-RBFRMLA-5,
              :QSPC-RBDTE-5-1,
              :QSPC-RBDAYS-5-1,
              :QSPC-RBYRTYPE-5-1,
              :QSPC-RBDTE-5-2,
              :QSPC-RBDAYS-5-2,
              :QSPC-RBYRTYPE-5-2,
              :QSPC-RBMINRET-5,
              :QSPC-RBNOMON-5,
              :QSPC-RBDEDCHG-5,
              :QSPC-RBRECAST-5,
              :QSPC-RBEARLY-5,
              :QSPC-RBDEFER-5,
              :QSPC-RBPCT-5,
              :QSPC-RBREDUC-5,
              :QSPC-RBMIN-5,
              :QSPC-RBSPOPT1-5,
              :QSPC-RBSPOPT2-5,
              :QSPC-RBMANAGER-5,
              :QSPC-RBUNITPER-CD-5,
              :QSPC-RBFRMLA2-5,
              :QSPC-RBFRMLA-6,
              :QSPC-RBDTE-6-1,
              :QSPC-RBDAYS-6-1,
              :QSPC-RBYRTYPE-6-1,
              :QSPC-RBDTE-6-2,
              :QSPC-RBDAYS-6-2,
              :QSPC-RBYRTYPE-6-2,
              :QSPC-RBMINRET-6,
              :QSPC-RBNOMON-6,
              :QSPC-RBDEDCHG-6,
              :QSPC-RBRECAST-6,
              :QSPC-RBEARLY-6,
              :QSPC-RBDEFER-6,
              :QSPC-RBPCT-6,
              :QSPC-RBREDUC-6,
              :QSPC-RBMIN-6,
              :QSPC-RBSPOPT1-6,
              :QSPC-RBSPOPT2-6,
              :QSPC-RBMANAGER-6,
              :QSPC-RBUNITPER-CD-6,
              :QSPC-RBFRMLA2-6,
              :QSPC-RBFRMLA-7,
              :QSPC-RBDTE-7-1,
              :QSPC-RBDAYS-7-1,
              :QSPC-RBYRTYPE-7-1,
              :QSPC-RBDTE-7-2,
              :QSPC-RBDAYS-7-2,
              :QSPC-RBYRTYPE-7-2,
              :QSPC-RBMINRET-7,
              :QSPC-RBNOMON-7,
              :QSPC-RBDEDCHG-7,
              :QSPC-RBRECAST-7,
              :QSPC-RBEARLY-7,
              :QSPC-RBDEFER-7,
              :QSPC-RBPCT-7,
              :QSPC-RBREDUC-7,
              :QSPC-RBMIN-7,
              :QSPC-RBSPOPT1-7,
              :QSPC-RBSPOPT2-7,
              :QSPC-RBMANAGER-7,
              :QSPC-RBUNITPER-CD-7,
              :QSPC-RBFRMLA2-7,
              :QSPC-RBFRMLA-8,
              :QSPC-RBDTE-8-1,
              :QSPC-RBDAYS-8-1,
              :QSPC-RBYRTYPE-8-1,
              :QSPC-RBDTE-8-2,
              :QSPC-RBDAYS-8-2,
              :QSPC-RBYRTYPE-8-2,
              :QSPC-RBMINRET-8,
              :QSPC-RBNOMON-8,
              :QSPC-RBDEDCHG-8,
              :QSPC-RBRECAST-8,
              :QSPC-RBEARLY-8,
              :QSPC-RBDEFER-8,
              :QSPC-RBPCT-8,
              :QSPC-RBREDUC-8,
              :QSPC-RBMIN-8,
              :QSPC-RBSPOPT1-8,
              :QSPC-RBSPOPT2-8,
              :QSPC-RBMANAGER-8,
              :QSPC-RBUNITPER-CD-8,
              :QSPC-RBFRMLA2-8,
              :QSPC-RBFRMLA-9,
              :QSPC-RBDTE-9-1,
              :QSPC-RBDAYS-9-1,
              :QSPC-RBYRTYPE-9-1,
              :QSPC-RBDTE-9-2,
              :QSPC-RBDAYS-9-2,
              :QSPC-RBYRTYPE-9-2,
              :QSPC-RBMINRET-9,
              :QSPC-RBNOMON-9,
              :QSPC-RBDEDCHG-9,
              :QSPC-RBRECAST-9,
              :QSPC-RBEARLY-9,
              :QSPC-RBDEFER-9,
              :QSPC-RBPCT-9,
              :QSPC-RBREDUC-9,
              :QSPC-RBMIN-9,
              :QSPC-RBSPOPT1-9,
              :QSPC-RBSPOPT2-9,
              :QSPC-RBMANAGER-9,
              :QSPC-RBUNITPER-CD-9,
              :QSPC-RBFRMLA2-9,
              :QSPC-RBFRMLA-10,
              :QSPC-RBDTE-10-1,
              :QSPC-RBDAYS-10-1,
              :QSPC-RBYRTYPE-10-1,
              :QSPC-RBDTE-10-2,
              :QSPC-RBDAYS-10-2,
              :QSPC-RBYRTYPE-10-2,
              :QSPC-RBMINRET-10,
              :QSPC-RBNOMON-10,
              :QSPC-RBDEDCHG-10,
              :QSPC-RBRECAST-10,
              :QSPC-RBEARLY-10,
              :QSPC-RBDEFER-10,
              :QSPC-RBPCT-10,
              :QSPC-RBREDUC-10,
              :QSPC-RBMIN-10,
              :QSPC-RBSPOPT1-10,
              :QSPC-RBSPOPT2-10,
              :QSPC-RBMANAGER-10,
              :QSPC-RBUNITPER-CD-10,
              :QSPC-RBFRMLA2-10,
              :QSPC-RBFRMLA-11,
              :QSPC-RBDTE-11-1,
              :QSPC-RBDAYS-11-1,
              :QSPC-RBYRTYPE-11-1,
              :QSPC-RBDTE-11-2,
              :QSPC-RBDAYS-11-2,
              :QSPC-RBYRTYPE-11-2,
              :QSPC-RBMINRET-11,
              :QSPC-RBNOMON-11,
              :QSPC-RBDEDCHG-11,
              :QSPC-RBRECAST-11,
              :QSPC-RBEARLY-11,
              :QSPC-RBDEFER-11,
              :QSPC-RBPCT-11,
              :QSPC-RBREDUC-11,
              :QSPC-RBMIN-11,
              :QSPC-RBSPOPT1-11,
              :QSPC-RBSPOPT2-11,
              :QSPC-RBMANAGER-11,
              :QSPC-RBUNITPER-CD-11,
              :QSPC-RBFRMLA2-11,
              :QSPC-RBFRMLA-12,
              :QSPC-RBDTE-12-1,
              :QSPC-RBDAYS-12-1,
              :QSPC-RBYRTYPE-12-1,
              :QSPC-RBDTE-12-2,
              :QSPC-RBDAYS-12-2,
              :QSPC-RBYRTYPE-12-2,
              :QSPC-RBMINRET-12,
              :QSPC-RBNOMON-12,
              :QSPC-RBDEDCHG-12,
              :QSPC-RBRECAST-12,
              :QSPC-RBEARLY-12,
              :QSPC-RBDEFER-12,
              :QSPC-RBPCT-12,
              :QSPC-RBREDUC-12,
              :QSPC-RBMIN-12,
              :QSPC-RBSPOPT1-12,
              :QSPC-RBSPOPT2-12,
              :QSPC-RBMANAGER-12,
              :QSPC-RBUNITPER-CD-12,
              :QSPC-RBFRMLA2-12,
              :QSPC-RBFRMLA-13,
              :QSPC-RBDTE-13-1,
              :QSPC-RBDAYS-13-1,
              :QSPC-RBYRTYPE-13-1,
              :QSPC-RBDTE-13-2,
              :QSPC-RBDAYS-13-2,
              :QSPC-RBYRTYPE-13-2,
              :QSPC-RBMINRET-13,
              :QSPC-RBNOMON-13,
              :QSPC-RBDEDCHG-13,
              :QSPC-RBRECAST-13,
              :QSPC-RBEARLY-13,
              :QSPC-RBDEFER-13,
              :QSPC-RBPCT-13,
              :QSPC-RBREDUC-13,
              :QSPC-RBMIN-13,
              :QSPC-RBSPOPT1-13,
              :QSPC-RBSPOPT2-13,
              :QSPC-RBMANAGER-13,
              :QSPC-RBUNITPER-CD-13,
              :QSPC-RBFRMLA2-13,
              :QSPC-RBFRMLA-14,
              :QSPC-RBDTE-14-1,
              :QSPC-RBDAYS-14-1,
              :QSPC-RBYRTYPE-14-1,
              :QSPC-RBDTE-14-2,
              :QSPC-RBDAYS-14-2,
              :QSPC-RBYRTYPE-14-2,
              :QSPC-RBMINRET-14,
              :QSPC-RBNOMON-14,
              :QSPC-RBDEDCHG-14,
              :QSPC-RBRECAST-14,
              :QSPC-RBEARLY-14,
              :QSPC-RBDEFER-14,
              :QSPC-RBPCT-14,
              :QSPC-RBREDUC-14,
              :QSPC-RBMIN-14,
              :QSPC-RBSPOPT1-14,
              :QSPC-RBSPOPT2-14,
              :QSPC-RBMANAGER-14,
              :QSPC-RBUNITPER-CD-14,
              :QSPC-RBFRMLA2-14,
              :QSPC-RBFRMLA-15,
              :QSPC-RBDTE-15-1,
              :QSPC-RBDAYS-15-1,
              :QSPC-RBYRTYPE-15-1,
              :QSPC-RBDTE-15-2,
              :QSPC-RBDAYS-15-2,
              :QSPC-RBYRTYPE-15-2,
              :QSPC-RBMINRET-15,
              :QSPC-RBNOMON-15,
              :QSPC-RBDEDCHG-15,
              :QSPC-RBRECAST-15,
              :QSPC-RBEARLY-15,
              :QSPC-RBDEFER-15,
              :QSPC-RBPCT-15,
              :QSPC-RBREDUC-15,
              :QSPC-RBMIN-15,
              :QSPC-RBSPOPT1-15,
              :QSPC-RBSPOPT2-15,
              :QSPC-RBMANAGER-15,
              :QSPC-RBUNITPER-CD-15,
              :QSPC-RBFRMLA2-15,
              :QSPC-RBFRMLA-16,
              :QSPC-RBDTE-16-1,
              :QSPC-RBDAYS-16-1,
              :QSPC-RBYRTYPE-16-1,
              :QSPC-RBDTE-16-2,
              :QSPC-RBDAYS-16-2,
              :QSPC-RBYRTYPE-16-2,
              :QSPC-RBMINRET-16,
              :QSPC-RBNOMON-16,
              :QSPC-RBDEDCHG-16,
              :QSPC-RBRECAST-16,
              :QSPC-RBEARLY-16,
              :QSPC-RBDEFER-16,
              :QSPC-RBPCT-16,
              :QSPC-RBREDUC-16,
              :QSPC-RBMIN-16,
              :QSPC-RBSPOPT1-16,
              :QSPC-RBSPOPT2-16,
              :QSPC-RBMANAGER-16,
              :QSPC-RBUNITPER-CD-16,
              :QSPC-RBFRMLA2-16,
              :QSPC-RBFRMLA-17,
              :QSPC-RBDTE-17-1,
              :QSPC-RBDAYS-17-1,
              :QSPC-RBYRTYPE-17-1,
              :QSPC-RBDTE-17-2,
              :QSPC-RBDAYS-17-2,
              :QSPC-RBYRTYPE-17-2,
              :QSPC-RBMINRET-17,
              :QSPC-RBNOMON-17,
              :QSPC-RBDEDCHG-17,
              :QSPC-RBRECAST-17,
              :QSPC-RBEARLY-17,
              :QSPC-RBDEFER-17,
              :QSPC-RBPCT-17,
              :QSPC-RBREDUC-17,
              :QSPC-RBMIN-17,
              :QSPC-RBSPOPT1-17,
              :QSPC-RBSPOPT2-17,
              :QSPC-RBMANAGER-17,
              :QSPC-RBUNITPER-CD-17,
              :QSPC-RBFRMLA2-17,
              :QSPC-RBFRMLA-18,
              :QSPC-RBDTE-18-1,
              :QSPC-RBDAYS-18-1,
              :QSPC-RBYRTYPE-18-1,
              :QSPC-RBDTE-18-2,
              :QSPC-RBDAYS-18-2,
              :QSPC-RBYRTYPE-18-2,
              :QSPC-RBMINRET-18,
              :QSPC-RBNOMON-18,
              :QSPC-RBDEDCHG-18,
              :QSPC-RBRECAST-18,
              :QSPC-RBEARLY-18,
              :QSPC-RBDEFER-18,
              :QSPC-RBPCT-18,
              :QSPC-RBREDUC-18,
              :QSPC-RBMIN-18,
              :QSPC-RBSPOPT1-18,
              :QSPC-RBSPOPT2-18,
              :QSPC-RBMANAGER-18,
              :QSPC-RBUNITPER-CD-18,
              :QSPC-RBFRMLA2-18,
              :QSPC-RBFRMLA-19,
              :QSPC-RBDTE-19-1,
              :QSPC-RBDAYS-19-1,
              :QSPC-RBYRTYPE-19-1,
              :QSPC-RBDTE-19-2,
              :QSPC-RBDAYS-19-2,
              :QSPC-RBYRTYPE-19-2,
              :QSPC-RBMINRET-19,
              :QSPC-RBNOMON-19,
              :QSPC-RBDEDCHG-19,
              :QSPC-RBRECAST-19,
              :QSPC-RBEARLY-19,
              :QSPC-RBDEFER-19,
              :QSPC-RBPCT-19,
              :QSPC-RBREDUC-19,
              :QSPC-RBMIN-19,
              :QSPC-RBSPOPT1-19,
              :QSPC-RBSPOPT2-19,
              :QSPC-RBMANAGER-19,
              :QSPC-RBUNITPER-CD-19,
              :QSPC-RBFRMLA2-19,
              :QSPC-RBFRMLA-20,
              :QSPC-RBDTE-20-1,
              :QSPC-RBDAYS-20-1,
              :QSPC-RBYRTYPE-20-1,
              :QSPC-RBDTE-20-2,
              :QSPC-RBDAYS-20-2,
              :QSPC-RBYRTYPE-20-2,
              :QSPC-RBMINRET-20,
              :QSPC-RBNOMON-20,
              :QSPC-RBDEDCHG-20,
              :QSPC-RBRECAST-20,
              :QSPC-RBEARLY-20,
              :QSPC-RBDEFER-20,
              :QSPC-RBPCT-20,
              :QSPC-RBREDUC-20,
              :QSPC-RBMIN-20,
              :QSPC-RBSPOPT1-20,
              :QSPC-RBSPOPT2-20,
              :QSPC-RBMANAGER-20,
              :QSPC-RBUNITPER-CD-20,
              :QSPC-RBFRMLA2-20,
              :QSPC-RBFRMLA-21,
              :QSPC-RBDTE-21-1,
              :QSPC-RBDAYS-21-1,
              :QSPC-RBYRTYPE-21-1,
              :QSPC-RBDTE-21-2,
              :QSPC-RBDAYS-21-2,
              :QSPC-RBYRTYPE-21-2,
              :QSPC-RBMINRET-21,
              :QSPC-RBNOMON-21,
              :QSPC-RBDEDCHG-21,
              :QSPC-RBRECAST-21,
              :QSPC-RBEARLY-21,
              :QSPC-RBDEFER-21,
              :QSPC-RBPCT-21,
              :QSPC-RBREDUC-21,
              :QSPC-RBMIN-21,
              :QSPC-RBSPOPT1-21,
              :QSPC-RBSPOPT2-21,
              :QSPC-RBMANAGER-21,
              :QSPC-RBUNITPER-CD-21,
              :QSPC-RBFRMLA2-21,
              :QSPC-RBFRMLA-22,
              :QSPC-RBDTE-22-1,
              :QSPC-RBDAYS-22-1,
              :QSPC-RBYRTYPE-22-1,
              :QSPC-RBDTE-22-2,
              :QSPC-RBDAYS-22-2,
              :QSPC-RBYRTYPE-22-2,
              :QSPC-RBMINRET-22,
              :QSPC-RBNOMON-22,
              :QSPC-RBDEDCHG-22,
              :QSPC-RBRECAST-22,
              :QSPC-RBEARLY-22,
              :QSPC-RBDEFER-22,
              :QSPC-RBPCT-22,
              :QSPC-RBREDUC-22,
              :QSPC-RBMIN-22,
              :QSPC-RBSPOPT1-22,
              :QSPC-RBSPOPT2-22,
              :QSPC-RBMANAGER-22,
              :QSPC-RBUNITPER-CD-22,
              :QSPC-RBFRMLA2-22,
              :QSPC-RBCOMCD,
              :QSPC-ADDON-DISFRMLA,
              :QSPC-ADDON-WHOLE-FG,
              :QSPC-ADDON-ORG-MATURITY,
              :QSPC-CONSOLIDATE-RT-FG,
              :QSPC-1STPY-ADDON-DAYS,
              :QSPC-1STPY-ADDON-YRTYPE,
              :QSPC-ADDON-RATE-FG,
              :QSPC-ADDON-RATE,
              :QSPC-FEEFRMLA-1,
              :QSPC-FEECD-1,
              :QSPC-FEEID-1,
              :QSPC-FEEFRMLA-2,
              :QSPC-FEECD-2,
              :QSPC-FEEID-2,
              :QSPC-ROUND-PAY-FG,
              :QSPC-IRREGULAR-PAY-FG,
              :QSPC-DLRVCHGS-RB-CD-1,
              :QSPC-DLRVCHGS-RB-CD-2,
              :QSPC-DLRVCHGS-RB-CD-3,
              :QSPC-PARVHELD-RB-CD,
              :QSPC-PARVPAID-RB-CD,
              :QSPC-CONTR-CREDIT,
              :QSPC-CONTR-CREDIT-CD,
              :QSPC-CONTR-CREDIT-MIN-DEL-PER,
              :QSPC-OPT-CAL-CHGABLE,
              :QSPC-PBDEL-DAYS,
              :QSPC-PBINCR-PERCENT,
              :QSPC-CPOT30-CD,
              :QSPC-CPOTCHGOFF-CD,
              :QSPC-RDPOT30-CD,
              :QSPC-RDPOTCHGOFF-CD,
              :QSPC-MAX-ALLOW-DELINQ,
              :QSPC-RESCIND-BUS-DAYS,
              :QSPC-RESCIND-SATURDAY,
              :QSPC-MIN-CREDIT-SCORE,
              :QSPC-DUAL-IN-BRANCH,
              :QSPC-DUAL-IN-STATE,
              :QSPC-PACESETTER-FG,
              :QSPC-WAGE-ASSIGN-DAYS,
              :QSPC-CAL-PAYMENT-ROUND-FG,
              :QSPC-CAN-LOANS-FG,
              :QSPC-ALLOW-DELINQ-POLCD,
              :QSPC-YTD-MAX-ALLOW-DELINQ,
              :QSPC-LNCOST-FRMLA,
              :QSPC-LNCOST-AMT,
              :QSPC-DLDISC-RVPRIN1-PCNT,
              :QSPC-RENEWABLE-PAYS,
              :QSPC-RENEWABLE-DAYS,
              :QSPC-ALLOW-LN-BACK-DATE,
              :QSPC-DLDISC-RVPRIN1-AF-PCNT,
              :QSPC-CAL-MAX-AT-ESTIMATE,
              :QSPC-INSAH-RETRO,
              :QSPC-INSAH-DAYS-WAIT,
              :QSPC-ADDON-EXCEPT-FG,
              :QSPC-REPO-TRCD-1,
              :QSPC-REPO-FMNO-1-1,
              :QSPC-REPO-FMNO-1-2,
              :QSPC-REPO-FMNO-1-3,
              :QSPC-REPO-FMNO-1-4,
              :QSPC-REPO-TRCD-2,
              :QSPC-REPO-FMNO-2-1,
              :QSPC-REPO-FMNO-2-2,
              :QSPC-REPO-FMNO-2-3,
              :QSPC-REPO-FMNO-2-4,
              :QSPC-REPO-TRCD-3,
              :QSPC-REPO-FMNO-3-1,
              :QSPC-REPO-FMNO-3-2,
              :QSPC-REPO-FMNO-3-3,
              :QSPC-REPO-FMNO-3-4,
              :QSPC-CANCEL-ADDON-FG,
              :QSPC-DEFAULT-O2-INS-COMP,
              :QSPC-LC-ON-JUDGEMENTS,
              :QSPC-DELQ-IB-AGE,
              :QSPC-DELQ-IB-MAX-DAYS,
              :QSPC-INSURANCE-WRITTEN,
              :QSPC-INTERIM-COUPON,
              :QSPC-AUTO-ASSESS-LCHG,
              :QSPC-MAX-CASH-PROCEEDS,
              :QSPC-ALLOW-ZERO-PROC,
              :QSPC-BANKRULE-RATE,
              :QSPC-AUTO-SEND-TRCD-1,
              :QSPC-AUTO-SEND-FMNO-1-1,
              :QSPC-AUTO-SEND-FMNO-1-2,
              :QSPC-AUTO-SEND-FMNO-1-3,
              :QSPC-AUTO-SEND-FMNO-1-4,
              :QSPC-AUTO-SEND-TRCD-2,
              :QSPC-AUTO-SEND-FMNO-2-1,
              :QSPC-AUTO-SEND-FMNO-2-2,
              :QSPC-AUTO-SEND-FMNO-2-3,
              :QSPC-AUTO-SEND-FMNO-2-4,
              :QSPC-DLDISC-CONV-BF-PCNT,
              :QSPC-UNITPER-CD,
              :QSPC-RENEWAL-EDIT,
              :QSPC-ROUND-MFEE,
              :QSPC-ADV-TO-1ST-NO-CORR,
              :QSPC-LC-INT-ON-REPO-NON-PL,
              :QSPC-CONTRACTUAL-SET-NONACCRUAL,
              :QSPC-CONTRACTUAL-RESET-ACCRUAL,
              :QSPC-MONTHS-PAY-NAPP-COUNTER,
              :QSPC-SHORT-PAY-THRESHOLD,
              :QSPC-CASH-AVAIL-PERCENT,
              :QSPC-O1-JOINT-FG,
              :QSPC-NET-BAL-REQ,
              :QSPC-AMT-PROCEEDS-REQ,
              :QSPC-ALT-PREPAY-MONTHS,
              :QSPC-PERCENT-REGPAY-AVAIL,
              :QSPC-ALT-DEFFRMLA,
              :QSPC-ALT-DEFMIN,
              :QSPC-ALT-DEFMAX,
              :QSPC-NON-LOAN-FLAG,
              :QSPC-SKIP-TRW)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-SPC1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-SPC1-FILE.

           PERFORM REWRITE-IT.
           MOVE SPC-SELECT-PATH TO E-FILE.
           MOVE SPC1-KEY TO E-KEYX.
           PERFORM SET-SPC-FIELDS.

           EXEC SQL
            UPDATE DBO.SPCFILE
             SET
              SPCFILE.SPC_ORGST = :QSPC-ORGST,
              SPCFILE.SPC_SPRCLASS = :QSPC-SPRCLASS,
              SPCFILE.SPC_SUBCLASS = :QSPC-SUBCLASS,
              SPCFILE.SPC_LAWCODE = :QSPC-LAWCODE,
              SPCFILE.SPC_INSEARN_ORDER_1 = :QSPC-INSEARN-ORDER-1,
              SPCFILE.SPC_INSEARN_ORDER_2 = :QSPC-INSEARN-ORDER-2,
              SPCFILE.SPC_INSEARN_ORDER_3 = :QSPC-INSEARN-ORDER-3,
              SPCFILE.SPC_INSEARN_ORDER_4 = :QSPC-INSEARN-ORDER-4,
              SPCFILE.SPC_INSEARN_ORDER_5 = :QSPC-INSEARN-ORDER-5,
              SPCFILE.SPC_INSEARN_ORDER_6 = :QSPC-INSEARN-ORDER-6,
              SPCFILE.SPC_INSEARN_ORDER_7 = :QSPC-INSEARN-ORDER-7,
              SPCFILE.SPC_INSEARN_ORDER_8 = :QSPC-INSEARN-ORDER-8,
              SPCFILE.SPC_INSEARN_ORDER_9 = :QSPC-INSEARN-ORDER-9,
              SPCFILE.SPC_INSEARN_ORDER_10 =
                   :QSPC-INSEARN-ORDER-10,
              SPCFILE.SPC_INSEARN_ORDER_11 =
                   :QSPC-INSEARN-ORDER-11,
              SPCFILE.SPC_INSEARN_ORDER_12 =
                   :QSPC-INSEARN-ORDER-12,
              SPCFILE.SPC_INSAH_JOINT_FG = :QSPC-INSAH-JOINT-FG,
              SPCFILE.SPC_INS_O1_EDIT = :QSPC-INS-O1-EDIT,
              SPCFILE.SPC_INS_O2_EDIT = :QSPC-INS-O2-EDIT,
              SPCFILE.SPC_RBFRMLA_1 = :QSPC-RBFRMLA-1,
              SPCFILE.SPC_RBDTE_1_1 = :QSPC-RBDTE-1-1,
              SPCFILE.SPC_RBDAYS_1_1 = :QSPC-RBDAYS-1-1,
              SPCFILE.SPC_RBYRTYPE_1_1 = :QSPC-RBYRTYPE-1-1,
              SPCFILE.SPC_RBDTE_1_2 = :QSPC-RBDTE-1-2,
              SPCFILE.SPC_RBDAYS_1_2 = :QSPC-RBDAYS-1-2,
              SPCFILE.SPC_RBYRTYPE_1_2 = :QSPC-RBYRTYPE-1-2,
              SPCFILE.SPC_RBMINRET_1 = :QSPC-RBMINRET-1,
              SPCFILE.SPC_RBNOMON_1 = :QSPC-RBNOMON-1,
              SPCFILE.SPC_RBDEDCHG_1 = :QSPC-RBDEDCHG-1,
              SPCFILE.SPC_RBRECAST_1 = :QSPC-RBRECAST-1,
              SPCFILE.SPC_RBEARLY_1 = :QSPC-RBEARLY-1,
              SPCFILE.SPC_RBDEFER_1 = :QSPC-RBDEFER-1,
              SPCFILE.SPC_RBPCT_1 = :QSPC-RBPCT-1,
              SPCFILE.SPC_RBREDUC_1 = :QSPC-RBREDUC-1,
              SPCFILE.SPC_RBMIN_1 = :QSPC-RBMIN-1,
              SPCFILE.SPC_RBSPOPT1_1 = :QSPC-RBSPOPT1-1,
              SPCFILE.SPC_RBSPOPT2_1 = :QSPC-RBSPOPT2-1,
              SPCFILE.SPC_RBMANAGER_1 = :QSPC-RBMANAGER-1,
              SPCFILE.SPC_RBUNITPER_CD_1 = :QSPC-RBUNITPER-CD-1,
              SPCFILE.SPC_RBFRMLA2_1 = :QSPC-RBFRMLA2-1,
              SPCFILE.SPC_RBFRMLA_2 = :QSPC-RBFRMLA-2,
              SPCFILE.SPC_RBDTE_2_1 = :QSPC-RBDTE-2-1,
              SPCFILE.SPC_RBDAYS_2_1 = :QSPC-RBDAYS-2-1,
              SPCFILE.SPC_RBYRTYPE_2_1 = :QSPC-RBYRTYPE-2-1,
              SPCFILE.SPC_RBDTE_2_2 = :QSPC-RBDTE-2-2,
              SPCFILE.SPC_RBDAYS_2_2 = :QSPC-RBDAYS-2-2,
              SPCFILE.SPC_RBYRTYPE_2_2 = :QSPC-RBYRTYPE-2-2,
              SPCFILE.SPC_RBMINRET_2 = :QSPC-RBMINRET-2,
              SPCFILE.SPC_RBNOMON_2 = :QSPC-RBNOMON-2,
              SPCFILE.SPC_RBDEDCHG_2 = :QSPC-RBDEDCHG-2,
              SPCFILE.SPC_RBRECAST_2 = :QSPC-RBRECAST-2,
              SPCFILE.SPC_RBEARLY_2 = :QSPC-RBEARLY-2,
              SPCFILE.SPC_RBDEFER_2 = :QSPC-RBDEFER-2,
              SPCFILE.SPC_RBPCT_2 = :QSPC-RBPCT-2,
              SPCFILE.SPC_RBREDUC_2 = :QSPC-RBREDUC-2,
              SPCFILE.SPC_RBMIN_2 = :QSPC-RBMIN-2,
              SPCFILE.SPC_RBSPOPT1_2 = :QSPC-RBSPOPT1-2,
              SPCFILE.SPC_RBSPOPT2_2 = :QSPC-RBSPOPT2-2,
              SPCFILE.SPC_RBMANAGER_2 = :QSPC-RBMANAGER-2,
              SPCFILE.SPC_RBUNITPER_CD_2 = :QSPC-RBUNITPER-CD-2,
              SPCFILE.SPC_RBFRMLA2_2 = :QSPC-RBFRMLA2-2,
              SPCFILE.SPC_RBFRMLA_3 = :QSPC-RBFRMLA-3,
              SPCFILE.SPC_RBDTE_3_1 = :QSPC-RBDTE-3-1,
              SPCFILE.SPC_RBDAYS_3_1 = :QSPC-RBDAYS-3-1,
              SPCFILE.SPC_RBYRTYPE_3_1 = :QSPC-RBYRTYPE-3-1,
              SPCFILE.SPC_RBDTE_3_2 = :QSPC-RBDTE-3-2,
              SPCFILE.SPC_RBDAYS_3_2 = :QSPC-RBDAYS-3-2,
              SPCFILE.SPC_RBYRTYPE_3_2 = :QSPC-RBYRTYPE-3-2,
              SPCFILE.SPC_RBMINRET_3 = :QSPC-RBMINRET-3,
              SPCFILE.SPC_RBNOMON_3 = :QSPC-RBNOMON-3,
              SPCFILE.SPC_RBDEDCHG_3 = :QSPC-RBDEDCHG-3,
              SPCFILE.SPC_RBRECAST_3 = :QSPC-RBRECAST-3,
              SPCFILE.SPC_RBEARLY_3 = :QSPC-RBEARLY-3,
              SPCFILE.SPC_RBDEFER_3 = :QSPC-RBDEFER-3,
              SPCFILE.SPC_RBPCT_3 = :QSPC-RBPCT-3,
              SPCFILE.SPC_RBREDUC_3 = :QSPC-RBREDUC-3,
              SPCFILE.SPC_RBMIN_3 = :QSPC-RBMIN-3,
              SPCFILE.SPC_RBSPOPT1_3 = :QSPC-RBSPOPT1-3,
              SPCFILE.SPC_RBSPOPT2_3 = :QSPC-RBSPOPT2-3,
              SPCFILE.SPC_RBMANAGER_3 = :QSPC-RBMANAGER-3,
              SPCFILE.SPC_RBUNITPER_CD_3 = :QSPC-RBUNITPER-CD-3,
              SPCFILE.SPC_RBFRMLA2_3 = :QSPC-RBFRMLA2-3,
              SPCFILE.SPC_RBFRMLA_4 = :QSPC-RBFRMLA-4,
              SPCFILE.SPC_RBDTE_4_1 = :QSPC-RBDTE-4-1,
              SPCFILE.SPC_RBDAYS_4_1 = :QSPC-RBDAYS-4-1,
              SPCFILE.SPC_RBYRTYPE_4_1 = :QSPC-RBYRTYPE-4-1,
              SPCFILE.SPC_RBDTE_4_2 = :QSPC-RBDTE-4-2,
              SPCFILE.SPC_RBDAYS_4_2 = :QSPC-RBDAYS-4-2,
              SPCFILE.SPC_RBYRTYPE_4_2 = :QSPC-RBYRTYPE-4-2,
              SPCFILE.SPC_RBMINRET_4 = :QSPC-RBMINRET-4,
              SPCFILE.SPC_RBNOMON_4 = :QSPC-RBNOMON-4,
              SPCFILE.SPC_RBDEDCHG_4 = :QSPC-RBDEDCHG-4,
              SPCFILE.SPC_RBRECAST_4 = :QSPC-RBRECAST-4,
              SPCFILE.SPC_RBEARLY_4 = :QSPC-RBEARLY-4,
              SPCFILE.SPC_RBDEFER_4 = :QSPC-RBDEFER-4,
              SPCFILE.SPC_RBPCT_4 = :QSPC-RBPCT-4,
              SPCFILE.SPC_RBREDUC_4 = :QSPC-RBREDUC-4,
              SPCFILE.SPC_RBMIN_4 = :QSPC-RBMIN-4,
              SPCFILE.SPC_RBSPOPT1_4 = :QSPC-RBSPOPT1-4,
              SPCFILE.SPC_RBSPOPT2_4 = :QSPC-RBSPOPT2-4,
              SPCFILE.SPC_RBMANAGER_4 = :QSPC-RBMANAGER-4,
              SPCFILE.SPC_RBUNITPER_CD_4 = :QSPC-RBUNITPER-CD-4,
              SPCFILE.SPC_RBFRMLA2_4 = :QSPC-RBFRMLA2-4,
              SPCFILE.SPC_RBFRMLA_5 = :QSPC-RBFRMLA-5,
              SPCFILE.SPC_RBDTE_5_1 = :QSPC-RBDTE-5-1,
              SPCFILE.SPC_RBDAYS_5_1 = :QSPC-RBDAYS-5-1,
              SPCFILE.SPC_RBYRTYPE_5_1 = :QSPC-RBYRTYPE-5-1,
              SPCFILE.SPC_RBDTE_5_2 = :QSPC-RBDTE-5-2,
              SPCFILE.SPC_RBDAYS_5_2 = :QSPC-RBDAYS-5-2,
              SPCFILE.SPC_RBYRTYPE_5_2 = :QSPC-RBYRTYPE-5-2,
              SPCFILE.SPC_RBMINRET_5 = :QSPC-RBMINRET-5,
              SPCFILE.SPC_RBNOMON_5 = :QSPC-RBNOMON-5,
              SPCFILE.SPC_RBDEDCHG_5 = :QSPC-RBDEDCHG-5,
              SPCFILE.SPC_RBRECAST_5 = :QSPC-RBRECAST-5,
              SPCFILE.SPC_RBEARLY_5 = :QSPC-RBEARLY-5,
              SPCFILE.SPC_RBDEFER_5 = :QSPC-RBDEFER-5,
              SPCFILE.SPC_RBPCT_5 = :QSPC-RBPCT-5,
              SPCFILE.SPC_RBREDUC_5 = :QSPC-RBREDUC-5,
              SPCFILE.SPC_RBMIN_5 = :QSPC-RBMIN-5,
              SPCFILE.SPC_RBSPOPT1_5 = :QSPC-RBSPOPT1-5,
              SPCFILE.SPC_RBSPOPT2_5 = :QSPC-RBSPOPT2-5,
              SPCFILE.SPC_RBMANAGER_5 = :QSPC-RBMANAGER-5,
              SPCFILE.SPC_RBUNITPER_CD_5 = :QSPC-RBUNITPER-CD-5,
              SPCFILE.SPC_RBFRMLA2_5 = :QSPC-RBFRMLA2-5,
              SPCFILE.SPC_RBFRMLA_6 = :QSPC-RBFRMLA-6,
              SPCFILE.SPC_RBDTE_6_1 = :QSPC-RBDTE-6-1,
              SPCFILE.SPC_RBDAYS_6_1 = :QSPC-RBDAYS-6-1,
              SPCFILE.SPC_RBYRTYPE_6_1 = :QSPC-RBYRTYPE-6-1,
              SPCFILE.SPC_RBDTE_6_2 = :QSPC-RBDTE-6-2,
              SPCFILE.SPC_RBDAYS_6_2 = :QSPC-RBDAYS-6-2,
              SPCFILE.SPC_RBYRTYPE_6_2 = :QSPC-RBYRTYPE-6-2,
              SPCFILE.SPC_RBMINRET_6 = :QSPC-RBMINRET-6,
              SPCFILE.SPC_RBNOMON_6 = :QSPC-RBNOMON-6,
              SPCFILE.SPC_RBDEDCHG_6 = :QSPC-RBDEDCHG-6,
              SPCFILE.SPC_RBRECAST_6 = :QSPC-RBRECAST-6,
              SPCFILE.SPC_RBEARLY_6 = :QSPC-RBEARLY-6,
              SPCFILE.SPC_RBDEFER_6 = :QSPC-RBDEFER-6,
              SPCFILE.SPC_RBPCT_6 = :QSPC-RBPCT-6,
              SPCFILE.SPC_RBREDUC_6 = :QSPC-RBREDUC-6,
              SPCFILE.SPC_RBMIN_6 = :QSPC-RBMIN-6,
              SPCFILE.SPC_RBSPOPT1_6 = :QSPC-RBSPOPT1-6,
              SPCFILE.SPC_RBSPOPT2_6 = :QSPC-RBSPOPT2-6,
              SPCFILE.SPC_RBMANAGER_6 = :QSPC-RBMANAGER-6,
              SPCFILE.SPC_RBUNITPER_CD_6 = :QSPC-RBUNITPER-CD-6,
              SPCFILE.SPC_RBFRMLA2_6 = :QSPC-RBFRMLA2-6,
              SPCFILE.SPC_RBFRMLA_7 = :QSPC-RBFRMLA-7,
              SPCFILE.SPC_RBDTE_7_1 = :QSPC-RBDTE-7-1,
              SPCFILE.SPC_RBDAYS_7_1 = :QSPC-RBDAYS-7-1,
              SPCFILE.SPC_RBYRTYPE_7_1 = :QSPC-RBYRTYPE-7-1,
              SPCFILE.SPC_RBDTE_7_2 = :QSPC-RBDTE-7-2,
              SPCFILE.SPC_RBDAYS_7_2 = :QSPC-RBDAYS-7-2,
              SPCFILE.SPC_RBYRTYPE_7_2 = :QSPC-RBYRTYPE-7-2,
              SPCFILE.SPC_RBMINRET_7 = :QSPC-RBMINRET-7,
              SPCFILE.SPC_RBNOMON_7 = :QSPC-RBNOMON-7,
              SPCFILE.SPC_RBDEDCHG_7 = :QSPC-RBDEDCHG-7,
              SPCFILE.SPC_RBRECAST_7 = :QSPC-RBRECAST-7,
              SPCFILE.SPC_RBEARLY_7 = :QSPC-RBEARLY-7,
              SPCFILE.SPC_RBDEFER_7 = :QSPC-RBDEFER-7,
              SPCFILE.SPC_RBPCT_7 = :QSPC-RBPCT-7,
              SPCFILE.SPC_RBREDUC_7 = :QSPC-RBREDUC-7,
              SPCFILE.SPC_RBMIN_7 = :QSPC-RBMIN-7,
              SPCFILE.SPC_RBSPOPT1_7 = :QSPC-RBSPOPT1-7,
              SPCFILE.SPC_RBSPOPT2_7 = :QSPC-RBSPOPT2-7,
              SPCFILE.SPC_RBMANAGER_7 = :QSPC-RBMANAGER-7,
              SPCFILE.SPC_RBUNITPER_CD_7 = :QSPC-RBUNITPER-CD-7,
              SPCFILE.SPC_RBFRMLA2_7 = :QSPC-RBFRMLA2-7,
              SPCFILE.SPC_RBFRMLA_8 = :QSPC-RBFRMLA-8,
              SPCFILE.SPC_RBDTE_8_1 = :QSPC-RBDTE-8-1,
              SPCFILE.SPC_RBDAYS_8_1 = :QSPC-RBDAYS-8-1,
              SPCFILE.SPC_RBYRTYPE_8_1 = :QSPC-RBYRTYPE-8-1,
              SPCFILE.SPC_RBDTE_8_2 = :QSPC-RBDTE-8-2,
              SPCFILE.SPC_RBDAYS_8_2 = :QSPC-RBDAYS-8-2,
              SPCFILE.SPC_RBYRTYPE_8_2 = :QSPC-RBYRTYPE-8-2,
              SPCFILE.SPC_RBMINRET_8 = :QSPC-RBMINRET-8,
              SPCFILE.SPC_RBNOMON_8 = :QSPC-RBNOMON-8,
              SPCFILE.SPC_RBDEDCHG_8 = :QSPC-RBDEDCHG-8,
              SPCFILE.SPC_RBRECAST_8 = :QSPC-RBRECAST-8,
              SPCFILE.SPC_RBEARLY_8 = :QSPC-RBEARLY-8,
              SPCFILE.SPC_RBDEFER_8 = :QSPC-RBDEFER-8,
              SPCFILE.SPC_RBPCT_8 = :QSPC-RBPCT-8,
              SPCFILE.SPC_RBREDUC_8 = :QSPC-RBREDUC-8,
              SPCFILE.SPC_RBMIN_8 = :QSPC-RBMIN-8,
              SPCFILE.SPC_RBSPOPT1_8 = :QSPC-RBSPOPT1-8,
              SPCFILE.SPC_RBSPOPT2_8 = :QSPC-RBSPOPT2-8,
              SPCFILE.SPC_RBMANAGER_8 = :QSPC-RBMANAGER-8,
              SPCFILE.SPC_RBUNITPER_CD_8 = :QSPC-RBUNITPER-CD-8,
              SPCFILE.SPC_RBFRMLA2_8 = :QSPC-RBFRMLA2-8,
              SPCFILE.SPC_RBFRMLA_9 = :QSPC-RBFRMLA-9,
              SPCFILE.SPC_RBDTE_9_1 = :QSPC-RBDTE-9-1,
              SPCFILE.SPC_RBDAYS_9_1 = :QSPC-RBDAYS-9-1,
              SPCFILE.SPC_RBYRTYPE_9_1 = :QSPC-RBYRTYPE-9-1,
              SPCFILE.SPC_RBDTE_9_2 = :QSPC-RBDTE-9-2,
              SPCFILE.SPC_RBDAYS_9_2 = :QSPC-RBDAYS-9-2,
              SPCFILE.SPC_RBYRTYPE_9_2 = :QSPC-RBYRTYPE-9-2,
              SPCFILE.SPC_RBMINRET_9 = :QSPC-RBMINRET-9,
              SPCFILE.SPC_RBNOMON_9 = :QSPC-RBNOMON-9,
              SPCFILE.SPC_RBDEDCHG_9 = :QSPC-RBDEDCHG-9,
              SPCFILE.SPC_RBRECAST_9 = :QSPC-RBRECAST-9,
              SPCFILE.SPC_RBEARLY_9 = :QSPC-RBEARLY-9,
              SPCFILE.SPC_RBDEFER_9 = :QSPC-RBDEFER-9,
              SPCFILE.SPC_RBPCT_9 = :QSPC-RBPCT-9,
              SPCFILE.SPC_RBREDUC_9 = :QSPC-RBREDUC-9,
              SPCFILE.SPC_RBMIN_9 = :QSPC-RBMIN-9,
              SPCFILE.SPC_RBSPOPT1_9 = :QSPC-RBSPOPT1-9,
              SPCFILE.SPC_RBSPOPT2_9 = :QSPC-RBSPOPT2-9,
              SPCFILE.SPC_RBMANAGER_9 = :QSPC-RBMANAGER-9,
              SPCFILE.SPC_RBUNITPER_CD_9 = :QSPC-RBUNITPER-CD-9,
              SPCFILE.SPC_RBFRMLA2_9 = :QSPC-RBFRMLA2-9,
              SPCFILE.SPC_RBFRMLA_10 = :QSPC-RBFRMLA-10,
              SPCFILE.SPC_RBDTE_10_1 = :QSPC-RBDTE-10-1,
              SPCFILE.SPC_RBDAYS_10_1 = :QSPC-RBDAYS-10-1,
              SPCFILE.SPC_RBYRTYPE_10_1 = :QSPC-RBYRTYPE-10-1,
              SPCFILE.SPC_RBDTE_10_2 = :QSPC-RBDTE-10-2,
              SPCFILE.SPC_RBDAYS_10_2 = :QSPC-RBDAYS-10-2,
              SPCFILE.SPC_RBYRTYPE_10_2 = :QSPC-RBYRTYPE-10-2,
              SPCFILE.SPC_RBMINRET_10 = :QSPC-RBMINRET-10,
              SPCFILE.SPC_RBNOMON_10 = :QSPC-RBNOMON-10,
              SPCFILE.SPC_RBDEDCHG_10 = :QSPC-RBDEDCHG-10,
              SPCFILE.SPC_RBRECAST_10 = :QSPC-RBRECAST-10,
              SPCFILE.SPC_RBEARLY_10 = :QSPC-RBEARLY-10,
              SPCFILE.SPC_RBDEFER_10 = :QSPC-RBDEFER-10,
              SPCFILE.SPC_RBPCT_10 = :QSPC-RBPCT-10,
              SPCFILE.SPC_RBREDUC_10 = :QSPC-RBREDUC-10,
              SPCFILE.SPC_RBMIN_10 = :QSPC-RBMIN-10,
              SPCFILE.SPC_RBSPOPT1_10 = :QSPC-RBSPOPT1-10,
              SPCFILE.SPC_RBSPOPT2_10 = :QSPC-RBSPOPT2-10,
              SPCFILE.SPC_RBMANAGER_10 = :QSPC-RBMANAGER-10,
              SPCFILE.SPC_RBUNITPER_CD_10 = :QSPC-RBUNITPER-CD-10,
              SPCFILE.SPC_RBFRMLA2_10 = :QSPC-RBFRMLA2-10,
              SPCFILE.SPC_RBFRMLA_11 = :QSPC-RBFRMLA-11,
              SPCFILE.SPC_RBDTE_11_1 = :QSPC-RBDTE-11-1,
              SPCFILE.SPC_RBDAYS_11_1 = :QSPC-RBDAYS-11-1,
              SPCFILE.SPC_RBYRTYPE_11_1 = :QSPC-RBYRTYPE-11-1,
              SPCFILE.SPC_RBDTE_11_2 = :QSPC-RBDTE-11-2,
              SPCFILE.SPC_RBDAYS_11_2 = :QSPC-RBDAYS-11-2,
              SPCFILE.SPC_RBYRTYPE_11_2 = :QSPC-RBYRTYPE-11-2,
              SPCFILE.SPC_RBMINRET_11 = :QSPC-RBMINRET-11,
              SPCFILE.SPC_RBNOMON_11 = :QSPC-RBNOMON-11,
              SPCFILE.SPC_RBDEDCHG_11 = :QSPC-RBDEDCHG-11,
              SPCFILE.SPC_RBRECAST_11 = :QSPC-RBRECAST-11,
              SPCFILE.SPC_RBEARLY_11 = :QSPC-RBEARLY-11,
              SPCFILE.SPC_RBDEFER_11 = :QSPC-RBDEFER-11,
              SPCFILE.SPC_RBPCT_11 = :QSPC-RBPCT-11,
              SPCFILE.SPC_RBREDUC_11 = :QSPC-RBREDUC-11,
              SPCFILE.SPC_RBMIN_11 = :QSPC-RBMIN-11,
              SPCFILE.SPC_RBSPOPT1_11 = :QSPC-RBSPOPT1-11,
              SPCFILE.SPC_RBSPOPT2_11 = :QSPC-RBSPOPT2-11,
              SPCFILE.SPC_RBMANAGER_11 = :QSPC-RBMANAGER-11,
              SPCFILE.SPC_RBUNITPER_CD_11 = :QSPC-RBUNITPER-CD-11,
              SPCFILE.SPC_RBFRMLA2_11 = :QSPC-RBFRMLA2-11,
              SPCFILE.SPC_RBFRMLA_12 = :QSPC-RBFRMLA-12,
              SPCFILE.SPC_RBDTE_12_1 = :QSPC-RBDTE-12-1,
              SPCFILE.SPC_RBDAYS_12_1 = :QSPC-RBDAYS-12-1,
              SPCFILE.SPC_RBYRTYPE_12_1 = :QSPC-RBYRTYPE-12-1,
              SPCFILE.SPC_RBDTE_12_2 = :QSPC-RBDTE-12-2,
              SPCFILE.SPC_RBDAYS_12_2 = :QSPC-RBDAYS-12-2,
              SPCFILE.SPC_RBYRTYPE_12_2 = :QSPC-RBYRTYPE-12-2,
              SPCFILE.SPC_RBMINRET_12 = :QSPC-RBMINRET-12,
              SPCFILE.SPC_RBNOMON_12 = :QSPC-RBNOMON-12,
              SPCFILE.SPC_RBDEDCHG_12 = :QSPC-RBDEDCHG-12,
              SPCFILE.SPC_RBRECAST_12 = :QSPC-RBRECAST-12,
              SPCFILE.SPC_RBEARLY_12 = :QSPC-RBEARLY-12,
              SPCFILE.SPC_RBDEFER_12 = :QSPC-RBDEFER-12,
              SPCFILE.SPC_RBPCT_12 = :QSPC-RBPCT-12,
              SPCFILE.SPC_RBREDUC_12 = :QSPC-RBREDUC-12,
              SPCFILE.SPC_RBMIN_12 = :QSPC-RBMIN-12,
              SPCFILE.SPC_RBSPOPT1_12 = :QSPC-RBSPOPT1-12,
              SPCFILE.SPC_RBSPOPT2_12 = :QSPC-RBSPOPT2-12,
              SPCFILE.SPC_RBMANAGER_12 = :QSPC-RBMANAGER-12,
              SPCFILE.SPC_RBUNITPER_CD_12 = :QSPC-RBUNITPER-CD-12,
              SPCFILE.SPC_RBFRMLA2_12 = :QSPC-RBFRMLA2-12,
              SPCFILE.SPC_RBFRMLA_13 = :QSPC-RBFRMLA-13,
              SPCFILE.SPC_RBDTE_13_1 = :QSPC-RBDTE-13-1,
              SPCFILE.SPC_RBDAYS_13_1 = :QSPC-RBDAYS-13-1,
              SPCFILE.SPC_RBYRTYPE_13_1 = :QSPC-RBYRTYPE-13-1,
              SPCFILE.SPC_RBDTE_13_2 = :QSPC-RBDTE-13-2,
              SPCFILE.SPC_RBDAYS_13_2 = :QSPC-RBDAYS-13-2,
              SPCFILE.SPC_RBYRTYPE_13_2 = :QSPC-RBYRTYPE-13-2,
              SPCFILE.SPC_RBMINRET_13 = :QSPC-RBMINRET-13,
              SPCFILE.SPC_RBNOMON_13 = :QSPC-RBNOMON-13,
              SPCFILE.SPC_RBDEDCHG_13 = :QSPC-RBDEDCHG-13,
              SPCFILE.SPC_RBRECAST_13 = :QSPC-RBRECAST-13,
              SPCFILE.SPC_RBEARLY_13 = :QSPC-RBEARLY-13,
              SPCFILE.SPC_RBDEFER_13 = :QSPC-RBDEFER-13,
              SPCFILE.SPC_RBPCT_13 = :QSPC-RBPCT-13,
              SPCFILE.SPC_RBREDUC_13 = :QSPC-RBREDUC-13,
              SPCFILE.SPC_RBMIN_13 = :QSPC-RBMIN-13,
              SPCFILE.SPC_RBSPOPT1_13 = :QSPC-RBSPOPT1-13,
              SPCFILE.SPC_RBSPOPT2_13 = :QSPC-RBSPOPT2-13,
              SPCFILE.SPC_RBMANAGER_13 = :QSPC-RBMANAGER-13,
              SPCFILE.SPC_RBUNITPER_CD_13 = :QSPC-RBUNITPER-CD-13,
              SPCFILE.SPC_RBFRMLA2_13 = :QSPC-RBFRMLA2-13,
              SPCFILE.SPC_RBFRMLA_14 = :QSPC-RBFRMLA-14,
              SPCFILE.SPC_RBDTE_14_1 = :QSPC-RBDTE-14-1,
              SPCFILE.SPC_RBDAYS_14_1 = :QSPC-RBDAYS-14-1,
              SPCFILE.SPC_RBYRTYPE_14_1 = :QSPC-RBYRTYPE-14-1,
              SPCFILE.SPC_RBDTE_14_2 = :QSPC-RBDTE-14-2,
              SPCFILE.SPC_RBDAYS_14_2 = :QSPC-RBDAYS-14-2,
              SPCFILE.SPC_RBYRTYPE_14_2 = :QSPC-RBYRTYPE-14-2,
              SPCFILE.SPC_RBMINRET_14 = :QSPC-RBMINRET-14,
              SPCFILE.SPC_RBNOMON_14 = :QSPC-RBNOMON-14,
              SPCFILE.SPC_RBDEDCHG_14 = :QSPC-RBDEDCHG-14,
              SPCFILE.SPC_RBRECAST_14 = :QSPC-RBRECAST-14,
              SPCFILE.SPC_RBEARLY_14 = :QSPC-RBEARLY-14,
              SPCFILE.SPC_RBDEFER_14 = :QSPC-RBDEFER-14,
              SPCFILE.SPC_RBPCT_14 = :QSPC-RBPCT-14,
              SPCFILE.SPC_RBREDUC_14 = :QSPC-RBREDUC-14,
              SPCFILE.SPC_RBMIN_14 = :QSPC-RBMIN-14,
              SPCFILE.SPC_RBSPOPT1_14 = :QSPC-RBSPOPT1-14,
              SPCFILE.SPC_RBSPOPT2_14 = :QSPC-RBSPOPT2-14,
              SPCFILE.SPC_RBMANAGER_14 = :QSPC-RBMANAGER-14,
              SPCFILE.SPC_RBUNITPER_CD_14 = :QSPC-RBUNITPER-CD-14,
              SPCFILE.SPC_RBFRMLA2_14 = :QSPC-RBFRMLA2-14,
              SPCFILE.SPC_RBFRMLA_15 = :QSPC-RBFRMLA-15,
              SPCFILE.SPC_RBDTE_15_1 = :QSPC-RBDTE-15-1,
              SPCFILE.SPC_RBDAYS_15_1 = :QSPC-RBDAYS-15-1,
              SPCFILE.SPC_RBYRTYPE_15_1 = :QSPC-RBYRTYPE-15-1,
              SPCFILE.SPC_RBDTE_15_2 = :QSPC-RBDTE-15-2,
              SPCFILE.SPC_RBDAYS_15_2 = :QSPC-RBDAYS-15-2,
              SPCFILE.SPC_RBYRTYPE_15_2 = :QSPC-RBYRTYPE-15-2,
              SPCFILE.SPC_RBMINRET_15 = :QSPC-RBMINRET-15,
              SPCFILE.SPC_RBNOMON_15 = :QSPC-RBNOMON-15,
              SPCFILE.SPC_RBDEDCHG_15 = :QSPC-RBDEDCHG-15,
              SPCFILE.SPC_RBRECAST_15 = :QSPC-RBRECAST-15,
              SPCFILE.SPC_RBEARLY_15 = :QSPC-RBEARLY-15,
              SPCFILE.SPC_RBDEFER_15 = :QSPC-RBDEFER-15,
              SPCFILE.SPC_RBPCT_15 = :QSPC-RBPCT-15,
              SPCFILE.SPC_RBREDUC_15 = :QSPC-RBREDUC-15,
              SPCFILE.SPC_RBMIN_15 = :QSPC-RBMIN-15,
              SPCFILE.SPC_RBSPOPT1_15 = :QSPC-RBSPOPT1-15,
              SPCFILE.SPC_RBSPOPT2_15 = :QSPC-RBSPOPT2-15,
              SPCFILE.SPC_RBMANAGER_15 = :QSPC-RBMANAGER-15,
              SPCFILE.SPC_RBUNITPER_CD_15 = :QSPC-RBUNITPER-CD-15,
              SPCFILE.SPC_RBFRMLA2_15 = :QSPC-RBFRMLA2-15,
              SPCFILE.SPC_RBFRMLA_16 = :QSPC-RBFRMLA-16,
              SPCFILE.SPC_RBDTE_16_1 = :QSPC-RBDTE-16-1,
              SPCFILE.SPC_RBDAYS_16_1 = :QSPC-RBDAYS-16-1,
              SPCFILE.SPC_RBYRTYPE_16_1 = :QSPC-RBYRTYPE-16-1,
              SPCFILE.SPC_RBDTE_16_2 = :QSPC-RBDTE-16-2,
              SPCFILE.SPC_RBDAYS_16_2 = :QSPC-RBDAYS-16-2,
              SPCFILE.SPC_RBYRTYPE_16_2 = :QSPC-RBYRTYPE-16-2,
              SPCFILE.SPC_RBMINRET_16 = :QSPC-RBMINRET-16,
              SPCFILE.SPC_RBNOMON_16 = :QSPC-RBNOMON-16,
              SPCFILE.SPC_RBDEDCHG_16 = :QSPC-RBDEDCHG-16,
              SPCFILE.SPC_RBRECAST_16 = :QSPC-RBRECAST-16,
              SPCFILE.SPC_RBEARLY_16 = :QSPC-RBEARLY-16,
              SPCFILE.SPC_RBDEFER_16 = :QSPC-RBDEFER-16,
              SPCFILE.SPC_RBPCT_16 = :QSPC-RBPCT-16,
              SPCFILE.SPC_RBREDUC_16 = :QSPC-RBREDUC-16,
              SPCFILE.SPC_RBMIN_16 = :QSPC-RBMIN-16,
              SPCFILE.SPC_RBSPOPT1_16 = :QSPC-RBSPOPT1-16,
              SPCFILE.SPC_RBSPOPT2_16 = :QSPC-RBSPOPT2-16,
              SPCFILE.SPC_RBMANAGER_16 = :QSPC-RBMANAGER-16,
              SPCFILE.SPC_RBUNITPER_CD_16 = :QSPC-RBUNITPER-CD-16,
              SPCFILE.SPC_RBFRMLA2_16 = :QSPC-RBFRMLA2-16,
              SPCFILE.SPC_RBFRMLA_17 = :QSPC-RBFRMLA-17,
              SPCFILE.SPC_RBDTE_17_1 = :QSPC-RBDTE-17-1,
              SPCFILE.SPC_RBDAYS_17_1 = :QSPC-RBDAYS-17-1,
              SPCFILE.SPC_RBYRTYPE_17_1 = :QSPC-RBYRTYPE-17-1,
              SPCFILE.SPC_RBDTE_17_2 = :QSPC-RBDTE-17-2,
              SPCFILE.SPC_RBDAYS_17_2 = :QSPC-RBDAYS-17-2,
              SPCFILE.SPC_RBYRTYPE_17_2 = :QSPC-RBYRTYPE-17-2,
              SPCFILE.SPC_RBMINRET_17 = :QSPC-RBMINRET-17,
              SPCFILE.SPC_RBNOMON_17 = :QSPC-RBNOMON-17,
              SPCFILE.SPC_RBDEDCHG_17 = :QSPC-RBDEDCHG-17,
              SPCFILE.SPC_RBRECAST_17 = :QSPC-RBRECAST-17,
              SPCFILE.SPC_RBEARLY_17 = :QSPC-RBEARLY-17,
              SPCFILE.SPC_RBDEFER_17 = :QSPC-RBDEFER-17,
              SPCFILE.SPC_RBPCT_17 = :QSPC-RBPCT-17,
              SPCFILE.SPC_RBREDUC_17 = :QSPC-RBREDUC-17,
              SPCFILE.SPC_RBMIN_17 = :QSPC-RBMIN-17,
              SPCFILE.SPC_RBSPOPT1_17 = :QSPC-RBSPOPT1-17,
              SPCFILE.SPC_RBSPOPT2_17 = :QSPC-RBSPOPT2-17,
              SPCFILE.SPC_RBMANAGER_17 = :QSPC-RBMANAGER-17,
              SPCFILE.SPC_RBUNITPER_CD_17 = :QSPC-RBUNITPER-CD-17,
              SPCFILE.SPC_RBFRMLA2_17 = :QSPC-RBFRMLA2-17,
              SPCFILE.SPC_RBFRMLA_18 = :QSPC-RBFRMLA-18,
              SPCFILE.SPC_RBDTE_18_1 = :QSPC-RBDTE-18-1,
              SPCFILE.SPC_RBDAYS_18_1 = :QSPC-RBDAYS-18-1,
              SPCFILE.SPC_RBYRTYPE_18_1 = :QSPC-RBYRTYPE-18-1,
              SPCFILE.SPC_RBDTE_18_2 = :QSPC-RBDTE-18-2,
              SPCFILE.SPC_RBDAYS_18_2 = :QSPC-RBDAYS-18-2,
              SPCFILE.SPC_RBYRTYPE_18_2 = :QSPC-RBYRTYPE-18-2,
              SPCFILE.SPC_RBMINRET_18 = :QSPC-RBMINRET-18,
              SPCFILE.SPC_RBNOMON_18 = :QSPC-RBNOMON-18,
              SPCFILE.SPC_RBDEDCHG_18 = :QSPC-RBDEDCHG-18,
              SPCFILE.SPC_RBRECAST_18 = :QSPC-RBRECAST-18,
              SPCFILE.SPC_RBEARLY_18 = :QSPC-RBEARLY-18,
              SPCFILE.SPC_RBDEFER_18 = :QSPC-RBDEFER-18,
              SPCFILE.SPC_RBPCT_18 = :QSPC-RBPCT-18,
              SPCFILE.SPC_RBREDUC_18 = :QSPC-RBREDUC-18,
              SPCFILE.SPC_RBMIN_18 = :QSPC-RBMIN-18,
              SPCFILE.SPC_RBSPOPT1_18 = :QSPC-RBSPOPT1-18,
              SPCFILE.SPC_RBSPOPT2_18 = :QSPC-RBSPOPT2-18,
              SPCFILE.SPC_RBMANAGER_18 = :QSPC-RBMANAGER-18,
              SPCFILE.SPC_RBUNITPER_CD_18 = :QSPC-RBUNITPER-CD-18,
              SPCFILE.SPC_RBFRMLA2_18 = :QSPC-RBFRMLA2-18,
              SPCFILE.SPC_RBFRMLA_19 = :QSPC-RBFRMLA-19,
              SPCFILE.SPC_RBDTE_19_1 = :QSPC-RBDTE-19-1,
              SPCFILE.SPC_RBDAYS_19_1 = :QSPC-RBDAYS-19-1,
              SPCFILE.SPC_RBYRTYPE_19_1 = :QSPC-RBYRTYPE-19-1,
              SPCFILE.SPC_RBDTE_19_2 = :QSPC-RBDTE-19-2,
              SPCFILE.SPC_RBDAYS_19_2 = :QSPC-RBDAYS-19-2,
              SPCFILE.SPC_RBYRTYPE_19_2 = :QSPC-RBYRTYPE-19-2,
              SPCFILE.SPC_RBMINRET_19 = :QSPC-RBMINRET-19,
              SPCFILE.SPC_RBNOMON_19 = :QSPC-RBNOMON-19,
              SPCFILE.SPC_RBDEDCHG_19 = :QSPC-RBDEDCHG-19,
              SPCFILE.SPC_RBRECAST_19 = :QSPC-RBRECAST-19,
              SPCFILE.SPC_RBEARLY_19 = :QSPC-RBEARLY-19,
              SPCFILE.SPC_RBDEFER_19 = :QSPC-RBDEFER-19,
              SPCFILE.SPC_RBPCT_19 = :QSPC-RBPCT-19,
              SPCFILE.SPC_RBREDUC_19 = :QSPC-RBREDUC-19,
              SPCFILE.SPC_RBMIN_19 = :QSPC-RBMIN-19,
              SPCFILE.SPC_RBSPOPT1_19 = :QSPC-RBSPOPT1-19,
              SPCFILE.SPC_RBSPOPT2_19 = :QSPC-RBSPOPT2-19,
              SPCFILE.SPC_RBMANAGER_19 = :QSPC-RBMANAGER-19,
              SPCFILE.SPC_RBUNITPER_CD_19 = :QSPC-RBUNITPER-CD-19,
              SPCFILE.SPC_RBFRMLA2_19 = :QSPC-RBFRMLA2-19,
              SPCFILE.SPC_RBFRMLA_20 = :QSPC-RBFRMLA-20,
              SPCFILE.SPC_RBDTE_20_1 = :QSPC-RBDTE-20-1,
              SPCFILE.SPC_RBDAYS_20_1 = :QSPC-RBDAYS-20-1,
              SPCFILE.SPC_RBYRTYPE_20_1 = :QSPC-RBYRTYPE-20-1,
              SPCFILE.SPC_RBDTE_20_2 = :QSPC-RBDTE-20-2,
              SPCFILE.SPC_RBDAYS_20_2 = :QSPC-RBDAYS-20-2,
              SPCFILE.SPC_RBYRTYPE_20_2 = :QSPC-RBYRTYPE-20-2,
              SPCFILE.SPC_RBMINRET_20 = :QSPC-RBMINRET-20,
              SPCFILE.SPC_RBNOMON_20 = :QSPC-RBNOMON-20,
              SPCFILE.SPC_RBDEDCHG_20 = :QSPC-RBDEDCHG-20,
              SPCFILE.SPC_RBRECAST_20 = :QSPC-RBRECAST-20,
              SPCFILE.SPC_RBEARLY_20 = :QSPC-RBEARLY-20,
              SPCFILE.SPC_RBDEFER_20 = :QSPC-RBDEFER-20,
              SPCFILE.SPC_RBPCT_20 = :QSPC-RBPCT-20,
              SPCFILE.SPC_RBREDUC_20 = :QSPC-RBREDUC-20,
              SPCFILE.SPC_RBMIN_20 = :QSPC-RBMIN-20,
              SPCFILE.SPC_RBSPOPT1_20 = :QSPC-RBSPOPT1-20,
              SPCFILE.SPC_RBSPOPT2_20 = :QSPC-RBSPOPT2-20,
              SPCFILE.SPC_RBMANAGER_20 = :QSPC-RBMANAGER-20,
              SPCFILE.SPC_RBUNITPER_CD_20 = :QSPC-RBUNITPER-CD-20,
              SPCFILE.SPC_RBFRMLA2_20 = :QSPC-RBFRMLA2-20,
              SPCFILE.SPC_RBFRMLA_21 = :QSPC-RBFRMLA-21,
              SPCFILE.SPC_RBDTE_21_1 = :QSPC-RBDTE-21-1,
              SPCFILE.SPC_RBDAYS_21_1 = :QSPC-RBDAYS-21-1,
              SPCFILE.SPC_RBYRTYPE_21_1 = :QSPC-RBYRTYPE-21-1,
              SPCFILE.SPC_RBDTE_21_2 = :QSPC-RBDTE-21-2,
              SPCFILE.SPC_RBDAYS_21_2 = :QSPC-RBDAYS-21-2,
              SPCFILE.SPC_RBYRTYPE_21_2 = :QSPC-RBYRTYPE-21-2,
              SPCFILE.SPC_RBMINRET_21 = :QSPC-RBMINRET-21,
              SPCFILE.SPC_RBNOMON_21 = :QSPC-RBNOMON-21,
              SPCFILE.SPC_RBDEDCHG_21 = :QSPC-RBDEDCHG-21,
              SPCFILE.SPC_RBRECAST_21 = :QSPC-RBRECAST-21,
              SPCFILE.SPC_RBEARLY_21 = :QSPC-RBEARLY-21,
              SPCFILE.SPC_RBDEFER_21 = :QSPC-RBDEFER-21,
              SPCFILE.SPC_RBPCT_21 = :QSPC-RBPCT-21,
              SPCFILE.SPC_RBREDUC_21 = :QSPC-RBREDUC-21,
              SPCFILE.SPC_RBMIN_21 = :QSPC-RBMIN-21,
              SPCFILE.SPC_RBSPOPT1_21 = :QSPC-RBSPOPT1-21,
              SPCFILE.SPC_RBSPOPT2_21 = :QSPC-RBSPOPT2-21,
              SPCFILE.SPC_RBMANAGER_21 = :QSPC-RBMANAGER-21,
              SPCFILE.SPC_RBUNITPER_CD_21 = :QSPC-RBUNITPER-CD-21,
              SPCFILE.SPC_RBFRMLA2_21 = :QSPC-RBFRMLA2-21,
              SPCFILE.SPC_RBFRMLA_22 = :QSPC-RBFRMLA-22,
              SPCFILE.SPC_RBDTE_22_1 = :QSPC-RBDTE-22-1,
              SPCFILE.SPC_RBDAYS_22_1 = :QSPC-RBDAYS-22-1,
              SPCFILE.SPC_RBYRTYPE_22_1 = :QSPC-RBYRTYPE-22-1,
              SPCFILE.SPC_RBDTE_22_2 = :QSPC-RBDTE-22-2,
              SPCFILE.SPC_RBDAYS_22_2 = :QSPC-RBDAYS-22-2,
              SPCFILE.SPC_RBYRTYPE_22_2 = :QSPC-RBYRTYPE-22-2,
              SPCFILE.SPC_RBMINRET_22 = :QSPC-RBMINRET-22,
              SPCFILE.SPC_RBNOMON_22 = :QSPC-RBNOMON-22,
              SPCFILE.SPC_RBDEDCHG_22 = :QSPC-RBDEDCHG-22,
              SPCFILE.SPC_RBRECAST_22 = :QSPC-RBRECAST-22,
              SPCFILE.SPC_RBEARLY_22 = :QSPC-RBEARLY-22,
              SPCFILE.SPC_RBDEFER_22 = :QSPC-RBDEFER-22,
              SPCFILE.SPC_RBPCT_22 = :QSPC-RBPCT-22,
              SPCFILE.SPC_RBREDUC_22 = :QSPC-RBREDUC-22,
              SPCFILE.SPC_RBMIN_22 = :QSPC-RBMIN-22,
              SPCFILE.SPC_RBSPOPT1_22 = :QSPC-RBSPOPT1-22,
              SPCFILE.SPC_RBSPOPT2_22 = :QSPC-RBSPOPT2-22,
              SPCFILE.SPC_RBMANAGER_22 = :QSPC-RBMANAGER-22,
              SPCFILE.SPC_RBUNITPER_CD_22 = :QSPC-RBUNITPER-CD-22,
              SPCFILE.SPC_RBFRMLA2_22 = :QSPC-RBFRMLA2-22,
              SPCFILE.SPC_RBCOMCD = :QSPC-RBCOMCD,
              SPCFILE.SPC_ADDON_DISFRMLA = :QSPC-ADDON-DISFRMLA,
              SPCFILE.SPC_ADDON_WHOLE_FG = :QSPC-ADDON-WHOLE-FG,
              SPCFILE.SPC_ADDON_ORG_MATURITY =
                   :QSPC-ADDON-ORG-MATURITY,
              SPCFILE.SPC_CONSOLIDATE_RT_FG =
                   :QSPC-CONSOLIDATE-RT-FG,
              SPCFILE.SPC_1STPY_ADDON_DAYS =
                   :QSPC-1STPY-ADDON-DAYS,
              SPCFILE.SPC_1STPY_ADDON_YRTYPE =
                   :QSPC-1STPY-ADDON-YRTYPE,
              SPCFILE.SPC_ADDON_RATE_FG = :QSPC-ADDON-RATE-FG,
              SPCFILE.SPC_ADDON_RATE = :QSPC-ADDON-RATE,
              SPCFILE.SPC_FEEFRMLA_1 = :QSPC-FEEFRMLA-1,
              SPCFILE.SPC_FEECD_1 = :QSPC-FEECD-1,
              SPCFILE.SPC_FEEID_1 = :QSPC-FEEID-1,
              SPCFILE.SPC_FEEFRMLA_2 = :QSPC-FEEFRMLA-2,
              SPCFILE.SPC_FEECD_2 = :QSPC-FEECD-2,
              SPCFILE.SPC_FEEID_2 = :QSPC-FEEID-2,
              SPCFILE.SPC_ROUND_PAY_FG = :QSPC-ROUND-PAY-FG,
              SPCFILE.SPC_IRREGULAR_PAY_FG =
                   :QSPC-IRREGULAR-PAY-FG,
              SPCFILE.SPC_DLRVCHGS_RB_CD_1 =
                   :QSPC-DLRVCHGS-RB-CD-1,
              SPCFILE.SPC_DLRVCHGS_RB_CD_2 =
                   :QSPC-DLRVCHGS-RB-CD-2,
              SPCFILE.SPC_DLRVCHGS_RB_CD_3 =
                   :QSPC-DLRVCHGS-RB-CD-3,
              SPCFILE.SPC_PARVHELD_RB_CD = :QSPC-PARVHELD-RB-CD,
              SPCFILE.SPC_PARVPAID_RB_CD = :QSPC-PARVPAID-RB-CD,
              SPCFILE.SPC_CONTR_CREDIT = :QSPC-CONTR-CREDIT,
              SPCFILE.SPC_CONTR_CREDIT_CD = :QSPC-CONTR-CREDIT-CD,
              SPCFILE.SPC_CONTR_CREDIT_MIN_DEL_PER =
                   :QSPC-CONTR-CREDIT-MIN-DEL-PER,
              SPCFILE.SPC_OPT_CAL_CHGABLE = :QSPC-OPT-CAL-CHGABLE,
              SPCFILE.SPC_PBDEL_DAYS = :QSPC-PBDEL-DAYS,
              SPCFILE.SPC_PBINCR_PERCENT = :QSPC-PBINCR-PERCENT,
              SPCFILE.SPC_CPOT30_CD = :QSPC-CPOT30-CD,
              SPCFILE.SPC_CPOTCHGOFF_CD = :QSPC-CPOTCHGOFF-CD,
              SPCFILE.SPC_RDPOT30_CD = :QSPC-RDPOT30-CD,
              SPCFILE.SPC_RDPOTCHGOFF_CD = :QSPC-RDPOTCHGOFF-CD,
              SPCFILE.SPC_MAX_ALLOW_DELINQ =
                   :QSPC-MAX-ALLOW-DELINQ,
              SPCFILE.SPC_RESCIND_BUS_DAYS =
                   :QSPC-RESCIND-BUS-DAYS,
              SPCFILE.SPC_RESCIND_SATURDAY =
                   :QSPC-RESCIND-SATURDAY,
              SPCFILE.SPC_MIN_CREDIT_SCORE =
                   :QSPC-MIN-CREDIT-SCORE,
              SPCFILE.SPC_DUAL_IN_BRANCH = :QSPC-DUAL-IN-BRANCH,
              SPCFILE.SPC_DUAL_IN_STATE = :QSPC-DUAL-IN-STATE,
              SPCFILE.SPC_PACESETTER_FG = :QSPC-PACESETTER-FG,
              SPCFILE.SPC_WAGE_ASSIGN_DAYS =
                   :QSPC-WAGE-ASSIGN-DAYS,
              SPCFILE.SPC_CAL_PAYMENT_ROUND_FG =
                   :QSPC-CAL-PAYMENT-ROUND-FG,
              SPCFILE.SPC_CAN_LOANS_FG = :QSPC-CAN-LOANS-FG,
              SPCFILE.SPC_ALLOW_DELINQ_POLCD =
                   :QSPC-ALLOW-DELINQ-POLCD,
              SPCFILE.SPC_YTD_MAX_ALLOW_DELINQ =
                   :QSPC-YTD-MAX-ALLOW-DELINQ,
              SPCFILE.SPC_LNCOST_FRMLA = :QSPC-LNCOST-FRMLA,
              SPCFILE.SPC_LNCOST_AMT = :QSPC-LNCOST-AMT,
              SPCFILE.SPC_DLDISC_RVPRIN1_PCNT =
                   :QSPC-DLDISC-RVPRIN1-PCNT,
              SPCFILE.SPC_RENEWABLE_PAYS = :QSPC-RENEWABLE-PAYS,
              SPCFILE.SPC_RENEWABLE_DAYS = :QSPC-RENEWABLE-DAYS,
              SPCFILE.SPC_ALLOW_LN_BACK_DATE =
                   :QSPC-ALLOW-LN-BACK-DATE,
              SPCFILE.SPC_DLDISC_RVPRIN1_AF_PCNT =
                   :QSPC-DLDISC-RVPRIN1-AF-PCNT,
              SPCFILE.SPC_CAL_MAX_AT_ESTIMATE =
                   :QSPC-CAL-MAX-AT-ESTIMATE,
              SPCFILE.SPC_INSAH_RETRO = :QSPC-INSAH-RETRO,
              SPCFILE.SPC_INSAH_DAYS_WAIT = :QSPC-INSAH-DAYS-WAIT,
              SPCFILE.SPC_ADDON_EXCEPT_FG = :QSPC-ADDON-EXCEPT-FG,
              SPCFILE.SPC_REPO_TRCD_1 = :QSPC-REPO-TRCD-1,
              SPCFILE.SPC_REPO_FMNO_1_1 = :QSPC-REPO-FMNO-1-1,
              SPCFILE.SPC_REPO_FMNO_1_2 = :QSPC-REPO-FMNO-1-2,
              SPCFILE.SPC_REPO_FMNO_1_3 = :QSPC-REPO-FMNO-1-3,
              SPCFILE.SPC_REPO_FMNO_1_4 = :QSPC-REPO-FMNO-1-4,
              SPCFILE.SPC_REPO_TRCD_2 = :QSPC-REPO-TRCD-2,
              SPCFILE.SPC_REPO_FMNO_2_1 = :QSPC-REPO-FMNO-2-1,
              SPCFILE.SPC_REPO_FMNO_2_2 = :QSPC-REPO-FMNO-2-2,
              SPCFILE.SPC_REPO_FMNO_2_3 = :QSPC-REPO-FMNO-2-3,
              SPCFILE.SPC_REPO_FMNO_2_4 = :QSPC-REPO-FMNO-2-4,
              SPCFILE.SPC_REPO_TRCD_3 = :QSPC-REPO-TRCD-3,
              SPCFILE.SPC_REPO_FMNO_3_1 = :QSPC-REPO-FMNO-3-1,
              SPCFILE.SPC_REPO_FMNO_3_2 = :QSPC-REPO-FMNO-3-2,
              SPCFILE.SPC_REPO_FMNO_3_3 = :QSPC-REPO-FMNO-3-3,
              SPCFILE.SPC_REPO_FMNO_3_4 = :QSPC-REPO-FMNO-3-4,
              SPCFILE.SPC_CANCEL_ADDON_FG = :QSPC-CANCEL-ADDON-FG,
              SPCFILE.SPC_DEFAULT_O2_INS_COMP =
                   :QSPC-DEFAULT-O2-INS-COMP,
              SPCFILE.SPC_LC_ON_JUDGEMENTS =
                   :QSPC-LC-ON-JUDGEMENTS,
              SPCFILE.SPC_DELQ_IB_AGE = :QSPC-DELQ-IB-AGE,
              SPCFILE.SPC_DELQ_IB_MAX_DAYS =
                   :QSPC-DELQ-IB-MAX-DAYS,
              SPCFILE.SPC_INSURANCE_WRITTEN =
                   :QSPC-INSURANCE-WRITTEN,
              SPCFILE.SPC_INTERIM_COUPON = :QSPC-INTERIM-COUPON,
              SPCFILE.SPC_AUTO_ASSESS_LCHG =
                   :QSPC-AUTO-ASSESS-LCHG,
              SPCFILE.SPC_MAX_CASH_PROCEEDS =
                   :QSPC-MAX-CASH-PROCEEDS,
              SPCFILE.SPC_ALLOW_ZERO_PROC = :QSPC-ALLOW-ZERO-PROC,
              SPCFILE.SPC_BANKRULE_RATE = :QSPC-BANKRULE-RATE,
              SPCFILE.SPC_AUTO_SEND_TRCD_1 =
                   :QSPC-AUTO-SEND-TRCD-1,
              SPCFILE.SPC_AUTO_SEND_FMNO_1_1 =
                   :QSPC-AUTO-SEND-FMNO-1-1,
              SPCFILE.SPC_AUTO_SEND_FMNO_1_2 =
                   :QSPC-AUTO-SEND-FMNO-1-2,
              SPCFILE.SPC_AUTO_SEND_FMNO_1_3 =
                   :QSPC-AUTO-SEND-FMNO-1-3,
              SPCFILE.SPC_AUTO_SEND_FMNO_1_4 =
                   :QSPC-AUTO-SEND-FMNO-1-4,
              SPCFILE.SPC_AUTO_SEND_TRCD_2 =
                   :QSPC-AUTO-SEND-TRCD-2,
              SPCFILE.SPC_AUTO_SEND_FMNO_2_1 =
                   :QSPC-AUTO-SEND-FMNO-2-1,
              SPCFILE.SPC_AUTO_SEND_FMNO_2_2 =
                   :QSPC-AUTO-SEND-FMNO-2-2,
              SPCFILE.SPC_AUTO_SEND_FMNO_2_3 =
                   :QSPC-AUTO-SEND-FMNO-2-3,
              SPCFILE.SPC_AUTO_SEND_FMNO_2_4 =
                   :QSPC-AUTO-SEND-FMNO-2-4,
              SPCFILE.SPC_DLDISC_CONV_BF_PCNT =
                   :QSPC-DLDISC-CONV-BF-PCNT,
              SPCFILE.SPC_UNITPER_CD = :QSPC-UNITPER-CD,
              SPCFILE.SPC_RENEWAL_EDIT = :QSPC-RENEWAL-EDIT,
              SPCFILE.SPC_ROUND_MFEE = :QSPC-ROUND-MFEE,
              SPCFILE.SPC_ADV_TO_1ST_NO_CORR =
                   :QSPC-ADV-TO-1ST-NO-CORR,
              SPCFILE.SPC_LC_INT_ON_REPO_NON_PL =
                   :QSPC-LC-INT-ON-REPO-NON-PL,
              SPCFILE.SPC_CONTRACTUAL_SET_NONACCRUAL =
                   :QSPC-CONTRACTUAL-SET-NONACCRUAL,
              SPCFILE.SPC_CONTRACTUAL_RESET_ACCRUAL =
                   :QSPC-CONTRACTUAL-RESET-ACCRUAL,
              SPCFILE.SPC_MONTHS_PAY_NAPP_COUNTER =
                   :QSPC-MONTHS-PAY-NAPP-COUNTER,
              SPCFILE.SPC_SHORT_PAY_THRESHOLD =
                   :QSPC-SHORT-PAY-THRESHOLD,
              SPCFILE.SPC_CASH_AVAIL_PERCENT =
                   :QSPC-CASH-AVAIL-PERCENT,
              SPCFILE.SPC_O1_JOINT_FG = :QSPC-O1-JOINT-FG,
              SPCFILE.SPC_NET_BAL_REQ = :QSPC-NET-BAL-REQ,
              SPCFILE.SPC_AMT_PROCEEDS_REQ =
                   :QSPC-AMT-PROCEEDS-REQ,
              SPCFILE.SPC_ALT_PREPAY_MONTHS =
                   :QSPC-ALT-PREPAY-MONTHS,
              SPCFILE.SPC_PERCENT_REGPAY_AVAIL =
                   :QSPC-PERCENT-REGPAY-AVAIL,
              SPCFILE.SPC_ALT_DEFFRMLA = :QSPC-ALT-DEFFRMLA,
              SPCFILE.SPC_ALT_DEFMIN = :QSPC-ALT-DEFMIN,
              SPCFILE.SPC_ALT_DEFMAX = :QSPC-ALT-DEFMAX,
              SPCFILE.SPC_NON_LOAN_FLAG = :QSPC-NON-LOAN-FLAG,
              SPCFILE.SPC_SKIP_TRW = :QSPC-SKIP-TRW
            WHERE SPCFILE.SPC_ORGST = :QSPC-ORGST
              AND SPCFILE.SPC_SPRCLASS = :QSPC-SPRCLASS
              AND SPCFILE.SPC_SUBCLASS = :QSPC-SUBCLASS
              AND SPCFILE.SPC_LAWCODE = :QSPC-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-SPC1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-SPC1-FILE.

           PERFORM DELETE-IT.
           MOVE SPC-SELECT-PATH TO E-FILE.
           MOVE SPC1-KEY TO E-KEYX.
           PERFORM SET-SPC-FIELDS.

           EXEC SQL
            DELETE FROM DBO.SPCFILE
            WHERE SPCFILE.SPC_ORGST = :QSPC-ORGST
              AND SPCFILE.SPC_SPRCLASS = :QSPC-SPRCLASS
              AND SPCFILE.SPC_SUBCLASS = :QSPC-SUBCLASS
              AND SPCFILE.SPC_LAWCODE = :QSPC-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-SPC1-FILE.

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
              DESTROY IOSPIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
