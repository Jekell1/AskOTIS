      * THIS IS A TEST PROGRAM FOR TURNING REBATE COPY MEMBER INTO
      * A CALLED PROGRAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      REBATE.
       DATE-WRITTEN.    06-29-2018.
      ****************************************************************
      *
      * USAGE:
      *  
      *  MUST HAVE BR, LN AND SP ALREADY READ IN THE CALLING PROGRAM
      *  MUST HAVE REBATE INPUT FIELDS FILLED IN AS IF YOU WERE CALLING
      *    THE SECTION FROM THE COPY MEMBER
      *
      *    MOVE "UT/REBATE" TO FORM-NAM.
      *    CALL FORM-PROGX USING FORM-PATH REBATE-LINK BR-REC
      *                              LN-REC SP-REC.
      *    CANCEL FORM-PROGX.
      *
      ****************************************************************
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/REBATE.CBL S35 03/21/24-11:49:15 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".

       01  ERRCD                  PIC X           VALUE " ".

      * INTERNAL WORKERS:
       01  REB-REFDATE            PIC 9(8)        COMP.
       01  REB-TRIGGER-DATE       PIC 9(8)        COMP.
       01  REB-HOLD-RBDTE         PIC X           VALUE " ".
       01  REB-DEDCHG             PIC S9(5)V99    COMP.
       01  REB-MPRATE             PIC 9(3)V9(10)  COMP.
       01  REB-FACTOR             PIC 9(3)V9(10)  COMP.
       01  REB-PRATE              PIC 9(3)V9(10)  COMP.
       01  REB-BAL                PIC S9(6)V9(9)  COMP.
      *01  REB-INT                PIC 9(6)V99     COMP.
       01  REB-SUMORG             PIC S9(8)V9(8)  COMP.
       01  REB-SUMREM             PIC S9(8)V9(8)  COMP.
       01  REB-HOLD-ORGTERM       PIC S9(5)       COMP.
       01  REB-ELAPSED-MONTHS     PIC S9(5)       COMP.
       01  REB-ELAPSED-REM        PIC S9(5)V9(10) COMP.
       01  REB-ELAPSED-DAYS       PIC S9(5)       COMP.
       01  REB-SWING              PIC X           VALUE " ".
       01  REB-EARN-FOR-1MON      PIC S9(5)V99    COMP.
       01  REB-PRORATA-30DAY-PCT  PIC 9V999       COMP.
       01  REB-UNUSED-EXTDAYS     PIC S9(5)       COMP.
       01  REB-TOTAL-EXTDAYS      PIC S9(5)       COMP.
       01  REB-INTSUB             PIC 99          COMP VALUE 7.
       01  REB-DEFSUB             PIC 99          COMP VALUE 10.
       01  REB-AHSUB              PIC 9           COMP VALUE 5.
       01  REB-INSSUB             PIC 99          COMP VALUE 0.
       01  REB-HUNDREDS           PIC 9(5)V99     COMP.
       01  REB-MANYDECIMALS       PIC 9(5)V9(4)   COMP.
       01  REB-HALF-ORGTERM       PIC S9(5)       COMP.
       01  REB-HALF-ORGTERM-DEC   PIC S9(5)V99    COMP.
       01  REB-WORKER             PIC S9(7)V99    COMP.
       01  REB-WORK2              PIC S9(7)V99    COMP.
       01  REB-STATEWK2           PIC S9(7)V99    COMP.

       01  REBWK1                 PIC S999V9(15)  COMP.
       01  REBWK2                 PIC S999V9(15)  COMP.
       01  REBAN-M-1              PIC S999V9(15)  COMP.
       01  REBAN-M-1-T            PIC S999V9(15)  COMP.
       01  REBF                   PIC S9V9(15)    COMP.
       01  REBN                   PIC S999        COMP.
       01  REBM                   PIC S999        COMP.
       01  REBR                   PIC S999        COMP.
       01  REBN-M                 PIC S999        COMP.
       01  REBAN                  PIC S999V9(15)  COMP.
       01  REBAN-M                PIC S999V9(15)  COMP.
       01  REBAN-MPLUSR           PIC S999V9(15)  COMP.
       01  REBM-1                 PIC S999        COMP.
       01  REBM-T                 PIC S999        COMP.
       01  REBT                   PIC S999        COMP.
       01  REBP                   PIC S9(6)V99    COMP.
       01  REBI1                  PIC S999V9(15)  COMP.
       01  REBJ                   PIC S999V9(15)  COMP.
       01  REBJ1                  PIC S999V9(15)  COMP.
       01  REBI                   PIC S999V9(15)  COMP.
       01  REB-RCF1               REDEFINES REBI
                                  PIC S999V9(15)  COMP.
       01  REBADJ                 PIC S999V9(15)  COMP.
       01  REB-RCF                REDEFINES REBADJ
                                  PIC S999V9(15)  COMP.
       01  REBPW                  PIC S999V9(15)  COMP.
       01  REB-WFAC               REDEFINES REBPW
                                  PIC S999V9(15)  COMP.

       01  REB-UPR                PIC S9(7)V9999  COMP.
       01  REB-UPR-WHOLE          PIC S9(7)       COMP.

       01  REB-A--P-J             PIC S99999V9(13) COMP.
       01  REB-A--P-I             PIC S99999V9(13) COMP.
       01  REB-WK                 PIC S9(6)V9(11)  COMP.

       01  REBTX-CALC-INT-TOT     PIC S9(6)V99     COMP.
       01  REBTX-CALC-INT         PIC S9(6)V99     COMP.
       01  REBTX-AMTFIN           PIC S9(6)V99     COMP.
       01  REBTX-PYAMT            PIC S9(6)V99     COMP.
       01  REBTX-SERCHG-SLICE     PIC S9(6)V9(3)   COMP.
       01  REBTX-FROM-DATE        PIC 9(8)         COMP.
       01  REBTX-TO-DATE          PIC 9(8)         COMP.
       01  REBTX-RATE-BAL         PIC S9(6)V99     COMP.
       01  REBTX-RATE-AMT         PIC S9(6)V99     COMP.
       01  REBTX-STEP             PIC 9.     
       
      * FIELDS USED FOR SP-RBDTE = "D"
       01  REB-LTI-ADDON-FG       PIC X.
       88  REB-LTI-ADDON-NOT-FOUND VALUE "N".
       01  REB-TEST-LTI-TYPE      PIC 99 COMP-3.
       01  REB-HOLD-LNORIG1STPAY  PIC 9(8) COMP-3.
       01  REB-HOLD-LTI-REC       PIC X(LTI-SIZE).

      **********************************************************************
      *    A R E A  FOR PROGRAM WORKERS THAT GET REDEFINED TO GIVE
      *    NAMES THAT MAKE SENSE FOR THE ROUTINE.
      *
      **********************************************************************
      *   'REBX' FIELDS ARE PREFIXED WITH REBX TO REFLECT THEY ARE REDEFINED
      **********************************************************************
       01  REBX-REDEFINED-FIELDS.
           03  REBX-DAYS-IN-TERM         PIC S9(5)    COMP.
           03  REBX-DAYS-IN-FORCE        PIC S9(5)    COMP.
           03  REBX-DAYS-REMAINING       PIC S9(5)    COMP.
           03  REBX-EARNED-COST          PIC S9(7)    COMP.
           03  REBX-SHORT-RATE-REFUND    PIC S9(7)    COMP.

           03  REBX-INS-POLICY-FEE       PIC S9(7)V99 COMP.
           03  REBX-NET-CONTRACT-COST    PIC S9(7)V99 COMP.
           03  REBX-DAILY-COST           PIC S9(7)V99 COMP.
           03  REBX-PRO-RATA-REFUND      PIC S9(7)V99 COMP.
      *USED BY PEOPLE NO ROUND TO WHOLE DOLLARS
           03  REBX-DAILY-COST-NO-ROUND  PIC S9(7)V9(7) COMP.
           03  REBX-EARNED-COST-NO-ROUND PIC S9(7)V9(7) COMP.
           03  REBX-PRO-RATA-REFUND-NO-ROUND PIC S9(7)V9(7) COMP.
           03  REBX-SHORT-RATE-REFUND-NO-ROUND    PIC S9(7)V99.
      *    03  REBX-UNUSED
      *-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

      * FIELDS USED FOR CALLING SUBPROGRAMS:
       01  REB-FORM-PATH.
           03  FILLER                 PIC X(6)     VALUE "CL/REB".
           03  REB-FORM-PROG-TYPE     PIC X.
           03  REB-FORM-PROG-NO       PIC 99.
       01  REB-CANCEL-FG              PIC X        VALUE "Y".
       01  REB-COMMON.
           03  REB-COMMON-RTNCD       PIC X        VALUE SPACES.
           03  REB-COMMON-REBATE      PIC S9(7)V99 COMP-3 VALUE 0.
           03  REB-COMMON-AREA        PIC X(40)    VALUE SPACES.
           03  FILLER REDEFINES REB-COMMON-AREA.
               05  REB-COMMON-PAYDATE PIC 9(8).
               05  FILLER             PIC X(32).

      * USED FOR MISSISSIPPI REDUCTION CODE:
           03  REB-A01                REDEFINES REB-COMMON-AREA.
               05  A01-ELAPSED-MO     PIC S9(3)    COMP-3.
               05  A01-MS-PENALTY     PIC S9(5)V99 COMP-3.
               05  A01-PAYDATE        PIC 9(8)     COMP-3.
               05  FILLER             PIC X(29).
      * USED FOR TENNESSEE DEFERMENT REBATE FORMULA:
           03  REB-B01                REDEFINES REB-COMMON-AREA.
               05  B01-PAYDATE        PIC 9(8)     COMP-3.
               05  B01-FRMLA          PIC X(2).
               05  B01-PAY-SCHLD-FG   PIC X.
               05  B01-DEF-STOP       PIC X.
               05  FILLER             PIC X(31).
      * USED FOR INDIANA DEFERMENT REBATE FORMULA:
           03  REB-B02                REDEFINES REB-COMMON-AREA.
               05  B02-PAYDATE        PIC 9(8)     COMP-3.
               05  B02-DEF-STOP       PIC X.
               05  FILLER             PIC X(34).
      * USED FOR SP-RBFRMLA = 'T', TABLE REFUND FORMULA:
           03  REB-T01                REDEFINES REB-COMMON-AREA.
               05  T01-TBL-NO         PIC 99.
               05  T01-ORGTERM        PIC 9(3).
               05  T01-ELAPSED-DAYS   PIC S9(4).
               05  T01-ELAPSED-MONTHS PIC S9(4).
               05  T01-REFUND-RATE    PIC 999V9.
               05  FILLER             PIC X(23).
      * USED FOR SP-RBRBEARLY = 13 TABLE REFUND FORMULA
      * MERCURY 'NV', PER PROPERTY REFUND ACTION 8:
           03  REB-T02                REDEFINES REB-COMMON-AREA.
               05  T02-ELAPSED-DAYS   PIC S9(5).
               05  T02-INSTERM        PIC 9(5).
               05  FILLER             PIC X(30).

      * NEW SHORT RATE FORMULA SP-RBEARLY = 24 (PP)    TFC #96
           03  REB-T04                REDEFINES REB-COMMON-AREA.
               05  T04-ELAPSED-DAYS   PIC S9(5).
               05  FILLER             PIC X(35).

           03  REB-IRREBT             REDEFINES REB-COMMON-AREA.
               05  IRREBT-REMTERM     PIC S9(5).
               05  IRREBT-INSSUB      PIC 99.
               05  IRREBT-MANYDECIMALS PIC 9(5)V9(4).
               05  FILLER             PIC X(23).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBUT/REBATEW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01SP.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME REBATE-LINK BR-REC LN-REC SP-REC.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

      D    STOP MESS.
           MOVE SPACES TO ERRCD.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LTI-PATH-OVERRIDE.

           PERFORM REBATE.

           GO TO END-ROUTINE.

      **************************************
      *     VARIOUS COPY MEMBERS
      ************************************
       COPY "LIBGB/REBATE.CPY".

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBGB/RBACT.CPY".


      /*******************************************
      *     I N P U T  /  O U T P U T            *
      ********************************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTI1-FILE.
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
