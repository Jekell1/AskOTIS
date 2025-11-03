       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       APILNS.
       DATE-WRITTEN.     02/07/2025.
      *************************************************************************
      * DESC:  API CALLED PROGRAM
      *
      * CALLED FROM SP/APIDRV
      *
      * INPUT-PROCESS-CODE
      *
      *    1 - CALCULATIONS, RESTRICTIONS, AND VERIFY
      *             APILNS CALLS CALCZL AND LNVERI
      *             IF SUCCESSFUL, CREATES A LOG AND ASCII FILE WITH CALCS
      *               IN FTP/LOANS
      *                 L.(CORRELATION#).YYMMDD     LOG WITH PASS OR FAIL
      *                 A.(CORRELATION#).YYMMDD     ASCCI FILE WITH CALCS
      *
      *   CUSTOMER ACCEPTS OFFER
      *
      *    2 - CALCULATIONS AND RESTRICTIONS AGAIN, CREATE STAGE FILE
      *             APILNS CALLS CALCZL AND LNVERI
      *             IF SUCCESSFUL, CREATES A NEW VISION FILE (STAGE) WITH
      *              PD-REC BURIED IN IT SO WE CAN PASS THAT TO LN-REC AND
      *              PRINT DOCUMENTS IN THE END.
      *        STAGE FILE LOCAL ED/STAGEFILE
      *
      *   CUSTOMER HAS SIGNED DOCS
      *
      *   3 - READ THE STAGEFILE RECORD AND TURN INTO A LOAN AND
      *             DISBURSE TO A DEBIT CARD
      *
      *   CUSTOMER DOES NOT SIGN DOCS
      *
      *   4 - READ THE STAGEFILE RECORD AND DELETE IT
      *
      * RECEIVES INPUT-REC 
      *
      * 2025.0207 BAH 
      *************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSWK.CPY".
       COPY "LIBLP/LPFSOP.CPY".
       COPY "LIBFP/FPFSPST.CPY".
           SELECT ASCII-FILE ASSIGN TO ASCII-PATH
                  ORGANIZATION RELATIVE
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY ASCII-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDWK.CPY".
       COPY "LIBLP/LPFDOP.CPY".
       COPY "LIBFP/FPFDPST.CPY".

      * OUTPUT ASCII FILE WITH CALC RESULTS
       FD  ASCII-FILE
           LABEL RECORDS ARE STANDARD.
       01  ASCII-REC.
           03  ASCII-ORGST          PIC XX.          
           03  ASCII-ORGBR          PIC 9(4).
           03  ASCII-SUBCLASS       PIC 9(2).
           03  ASCII-FILEDATE       PIC 9(8).
           03  ASCII-LOANDATE       PIC 9(8).
           03  ASCII-INTDATE        PIC 9(8).
           03  ASCII-1STPYDATE      PIC 9(8).
           03  ASCII-ORGTERM        PIC 9(3).

           03  ASCII-PURCD          PIC X(3).
           03  ASCII-COLLCD         PIC X(2).
           03  ASCII-COLLID         PIC X(20).
           03  ASCII-SRCD           PIC X(2).
           03  ASCII-INSCODES       PIC X(3).

           03  ASCII-APRATE          PIC 9(03).999.
           03  ASCII-EFFRATE         PIC 9(03).999.
           03  ASCII-SMPRATE         PIC 9(03).999.

           03  ASCII-PROCEEDS       PIC 9(06).99.
           03  ASCII-LNAMT          PIC 9(06).99.
           03  ASCII-FEE-TABLE OCCURS 10 TIMES.
               05  ASCII-FEECODE     PIC X(02).
               05  ASCII-FEEAMT      PIC 9(04).99.
           03  ASCII-INSURANCE-TABLE OCCURS 6 TIMES.
               05  ASCII-INSCOMP     PIC 9(02).
               05  ASCII-INSOURS     PIC X(01).
               05  ASCII-INSEFF      PIC 9(08).
               05  ASCII-INSEXP      PIC 9(08).
               05  ASCII-INSPREM     PIC 9(04).99.
               05  ASCII-INSCOVR     PIC 9(06).99.
               05  ASCII-INSCOMM     PIC 9(04).99.

           03  ASCII-CLSFEE          PIC 9(06).99.
           03  ASCII-SERCHG          PIC 9(06).99.
           03  ASCII-MAINTFEE        PIC 9(06).99.
           03  ASCII-EXTCHG          PIC 9(06).99.
           03  ASCII-INTCHG          PIC 9(06).99.
           03  ASCII-1STPYAMT        PIC 9(06).99.
           03  ASCII-REGPYAMT        PIC 9(06).99.
           03  ASCII-FINAMT          PIC 9(06).99.
           03  ASCII-REPAY           PIC 9(06).99.



       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/APILNS.CBL S35 04/14/25-11:23:24 >".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01CL.CPY".
       COPY "LIBLP/LP01CL_SQL.CPY".
       COPY "LIBLP/LPWSCL.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".
       COPY "LIBLP/LP01LTD.CPY".
       COPY "LIBLP/LP01LTD_SQL.CPY".
       COPY "LIBLP/LPWSLTD.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBFP/FPWSPST.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       01  ASCII-PATH.
           03  FILLER               PIC X(01)   VALUE "/".
           03  ASCII-BASE           PIC X(09).
           03  ASCII-MACHINE        PIC X(02).
           03  ASCII-DIRECTORY      PIC X(5) VALUE "/FTP/".
           03  FILLER               PIC X(8) VALUE "LOANS/A.".
           03  ASCII-PATHNAME.
               05  ASCII-CORRELATION     PIC X(12).
               05  FILLER              PIC X    VALUE ".".
               05  ASCII-PATH-YY    PIC 99.
               05  ASCII-PATH-MM    PIC 99.
               05  ASCII-PATH-DD    PIC 99.

       01  WK-IFN              PIC X(2)     VALUE "WK".

       01  ASCII-KEY           PIC 9(4).
       01  WK-KEY              PIC 9(2)     VALUE 0.

      * FOR DOC PRINT
       01  PST-CALLED-FROM      PIC X(6).
       01  HOLD-OPT-OUT         PIC X.
       01  HOLD-SSNO.
           03  SSNO-FIRST-3     PIC 9(3).
           03  SSNO-MID-2       PIC 9(2).
           03  SSNO-LAST-4      PIC 9(4).
       01  HOLD-MAKER-FNAME     PIC X(16).
       01  HOLD-MAKER-LNAME     PIC X(16).
      * MMDDYYYY
       01  HOLD-MAKER-DOB       PIC 9(08).
       01  HOLD-COMAKER-FNAME   PIC X(16).
       01  HOLD-COMAKER-LNAME   PIC X(16).
       01  HOLD-COMAKER-OPT-OUT PIC X.
       78  PROG-DIR-PREFIX-LONP                  VALUE "LP/LONP".
       78  PROG-DIR-PREFIX-PST                   VALUE "FP/PST ".
       01  PROG-WK.
           03  PROG-DIR-NAME       PIC X(07)     VALUE "LP/LONP".
           03  FILLER              REDEFINES PROG-DIR-NAME.
               05  FILLER          PIC X(06).
               05  PROG-DIR-PREFIX PIC X(01).
           03  PROG-WK-IP          PIC X(02)     VALUE SPACES.
      *01  SYSDATE-MMDDYY              PIC 9(06)     VALUE ZEROES.
      *01  FILLER                      REDEFINES SYSDATE-MMDDYY.
      *    03  SYSDATE-MMDDYY-MM       PIC 9(02).
      *    03  SYSDATE-MMDDYY-DD       PIC 9(02).
      *    03  SYSDATE-MMDDYY-YY       PIC 9(02).
       01  INSPECT-W       PIC X(86).
       01  ADD-ON          PIC X(25).

       01  REBL-WORKERS.
           03  REBL-LN-ACCTNO           PIC 9(8)       VALUE 0.
           03  REBL-LN-SSNO             PIC 9(9)       VALUE 0.
           03  REBL-TRANS-DATE          PIC 9(8)       VALUE 0.
           03  REBL-REBATE-ADJ          PIC S9(7)V99   VALUE 0.
           03  REBL-MEMO                PIC X(60)      VALUE SPACES.
           03  REBL-BR-NOT-OURS-REFUND  PIC X          VALUE " ".

      * USED FOR LONPC2
       01  EXT-CALLED-FROM     PIC X(6) EXTERNAL.
       01  EXT-RETURN-ERR-BUF  PIC X(55) EXTERNAL.

      * FIELDS SPECIFICALLY FOR SU-MODULE
       01  NEW-DIRECTORY       PIC X(22).
       01  ERR-NUM             PIC 9(09)  COMP-4.
       01  TO-BRANCH           PIC 9(04)  VALUE ZERO.
       01  TO-DATA-PATH        PIC X(06)  VALUE " ".

       01  HOLD-EXT-FILPATH.
           03  FILLER                PIC X(13).
           03  HOLD-EXT-FILPATH-DATA PIC X(6).

      * ENVIROMENT: POSTNAME AS IT WAS WHEN USER ENTERED PROGRAM
       01  ORIG-POSTNAME.
           03  ORIG-SECURITY   PIC X.
           03  ORIG-BR         PIC 9(4).
           03  ORIG-NAME       PIC X(5).
           
      * ENVIROMENT: FIL AS IT WAS WHEN USER ENTERED PROGRAM
       01  ORIG-FIL.
           03  ORIG-XXX        PIC X(3).
           03  ORIG-SLASH1     PIC X.
           03  ORIG-XXXX       PIC X(4).
           03  ORIG-SLASH2     PIC X.
           03  ORIG-MACHINE    PIC X(2).
           03  ORIG-SLASH3     PIC X.
           03  ORIG-DATA-PATH  PIC X(6).

       01 WORK-USERID.
          03  WORK-SECURITY        PIC X.
          03  WORK-BR              PIC 9(4).
          03  WORK-NAME            PIC X(5).

       01 HOLD-FIL.
          03  HOLD-XXX             PIC X(3).
          03  HOLD-SLASH1          PIC X.
          03  HOLD-XXXX            PIC X(4).
          03  HOLD-SLASH2          PIC X.
          03  HOLD-MACHINE         PIC X(2).
          03  HOLD-SLASH3          PIC X.
          03  HOLD-DATA-PATH       PIC X(6).

      * FOR CALLING LNVERI
       01  ACTION-LIST          PIC X(4) VALUE "SEL.".
       01  ACTION               PIC X(6).
       01  ERRORS-EXIST        PIC X VALUE "N".
       01  RETURN-ERR-LIST      PIC X(8) VALUE "ERRMESS.".
       01  RETURN-ERR-BUF       PIC X(55) VALUE SPACES.
 
       01  INS-SUB             PIC 9(2).
       01  SUB                 PIC 9(6)      COMP-3 VALUE 0.
       01  SUB1                PIC 9(6)      COMP-3 VALUE 0.
       01  XAGE-SUB            PIC 9(2).
       01  XAGE-MAX            PIC 9(2).
       01  TEST-COLLCD     PIC XX.
           88  AUTO-COLLCD VALUE "AU" "AC" "AL" "RP" "HP" "AW"
                                      "MU" "MV" "AP" "WA" "WB" "AS".

       01  DEFAULT-APR-FG        PIC X.
       01  COUNTER               PIC 99       COMP.
       01  ADV-TO-1ST-NO-CORR    PIC X       VALUE " ".
       01  JULIAN-HLDR           PIC 9(8).
       01  QUOTE-ID-FG        PIC X       VALUE " ".
           88  QUOTE-ONLY                 VALUE "Q".
           88  QUOTE-CHANGED-TO-LOAN      VALUE "L".
       01  AMT-HOLDER                    PIC S9(9)V99 COMP.
       01  TOTAL-GL-HLDR                 PIC S9(5)V99 COMP.
       01  TOTAL-GL-HLDR-SALESTAX        PIC S9(5)V99 COMP.
       01  TOTAL-GL-FINCHG-HLDR          PIC S9(5)V99 COMP.
       01  TOTAL-GL-FINCHG-HLDR-SALESTAX PIC S9(5)V99 COMP.
       01  USERID-GROUP-NAME.
           03  USER-GROUP           PIC X(3).
           03  FILLER               PIC X(2).

       01  GA-SCHG-PATH.
           03  FILLER         PIC X(7)     VALUE "CL/GASC".
           03  GA-SCHG-NO     PIC 99.
       01  SC-SCHG-PATH.
           03  FILLER         PIC X(7)     VALUE "CL/SCSC".
           03  SC-SCHG-NO     PIC 99.
       01  SCHG18-UPDATE-FG   PIC X       VALUE " ".

       01  BW-1-INFO.
           03  BW-1-STATE              PIC XX.
           03  BW-1-CITY-TAX-CD        PIC XXX.
           03  BW-1-COUNTY             PIC XXX.

      * USED IN VALID-INS-TYPES-ELIG
       01  FEECODE-TABLE      VALUE " ".
           03  FEECODE        OCCURS 4 TIMES
                              PIC XX.
     
       01  SPEC-TABLE.
           03  SPEC-REC    OCCURS 39 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
       01  CL-ELE             PIC 99 VALUE 25.
       01  AH-ELE             PIC 99 VALUE 26.
       01  PP-ELE             PIC 99 VALUE 27.
       01  O1-ELE             PIC 99 VALUE 28.
       01  O2-ELE             PIC 99 VALUE 29.
       01  O3-ELE             PIC 99 VALUE 30.

       COPY "LIBLP/LPIPBUF.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBCL/CASCHGW.CPY".
       COPY "LIBCL/NCSCHGW.CPY".
       COPY "LIBCL/GASCHGW.CPY".
       COPY "LIBCL/SCSCHGW.CPY".
       COPY "LIBCL/TXSCHGW.CPY".
       COPY "LIBCL/WISCHGW.CPY".
       COPY "LIBCL/MFEE04W.CPY".
       COPY "LIBCL/NCINSW.CPY".
       COPY "LIBLP/LPANTIW.CPY".
       COPY "LIBLP/LPWKCIPL.CPY".
       COPY "LIBLP/LPWKCEPL.CPY".
       COPY "LIBLP/LPWKCOPL.CPY".
       COPY "LIBLP/LPSPEDW.CPY".
       COPY "LIBLP/INSELIGW.CPY".
 
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/UNAMEW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPEDPATH.CPY".
       COPY "LIBCL/BISCANW.CPY".
       COPY "LIBCL/BICLASW.CPY".

       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *COPY "LIBGB/GETFMW.CPY".
       01  EXIT-PATHNAME.
           03  FILLER         PIC X(12).
           03  EXIT-PATH      PIC X(9).
           03  FILLER         PIC X.
       01  FORM-PATH.
           03  FILLER         PIC X.
           03  FORM-REL       PIC X(7).
           03  FILLER REDEFINES FORM-REL.
               05  FILLER     PIC X(4).
               05  LOGUID-REL PIC X(3).
           03  FILLER         PIC X.
           03  FORM-OBJ       PIC XX.
           03  FILLER         PIC X.
           03  FORM-NAM.
               05  FORM-DIRX  PIC XX.
               05  FILLER     PIC X.
               05  FORM-PROGX PIC X(6).
           03  FILLER         PIC X.
       01  FORM-PROG          PIC X(6) VALUE " ".

       LINKAGE SECTION.

       01  FORM-PATHNAME      PIC X(22).
       01  INPUT-REC.
           03  INPUT-PROCESS-CODE    PIC 9(4).
           03  INPUT-PROCESS-CODEX REDEFINES
                                     INPUT-PROCESS-CODE PIC X(4).
           03  INPUT-CORRELATION     PIC X(12).
           03  INPUT-BRNO            PIC 9(4).
           03  INPUT-BRNOX REDEFINES INPUT-BRNO PIC X(4).
           03  INPUT-SSNO          PIC 9(9).
           03  INPUT-SSNOX REDEFINES INPUT-SSNO PIC X(9).
           03  INPUT-CAN-LOAN      PIC 9(2).
           03  INPUT-CAN-LOANX REDEFINES INPUT-CAN-LOAN PIC X(2).
           03  INPUT-DEBIT-CARD      PIC 9(10).
           03  INPUT-DEBIT-CARDX REDEFINES INPUT-DEBIT-CARD PIC X(10).
       01  RESULT-REC.
           03  RESULT-STATUS          PIC 9(4).
           03  RESULT-MSG             PIC X(40).


       PROCEDURE DIVISION USING FORM-PATHNAME INPUT-REC RESULT-REC.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              WK-FILE OP-FILE ASCII-FILE STAGE-FILE.
       COPY "LIBGB/DECLARE.CPY".
           CLOSE
              WK-FILE OP-FILE ASCII-FILE STAGE-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LTD1-FILE.
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-CL1-FILE.
           PERFORM CLOSE-CDB1-FILE.
       COPY "LIBGB/DECLAR2.CPY".
 
      ******************************************************************
      *
      *  PROGRAM CONTROL
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.

      * NEED TO GET EXTERNAL VARIABLES SET
           MOVE "CO" TO FORM-OBJ.
           MOVE "GB/SETENV" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE 99            TO RESULT-STATUS.

           PERFORM GET-GPENV.

           MOVE EXT-FILPATH TO HOLD-EXT-FILPATH.
           ACCEPT ORIG-POSTNAME FROM ENVIRONMENT "POSTNAME".

           PERFORM INITIALIZATION.

           MOVE INPUT-BRNO TO BR-KEY.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9 
              MOVE 2                TO RESULT-STATUS
              MOVE "APILNS: INVALID BRANCH; &&&&" TO RESULT-MSG
              INSPECT RESULT-MSG REPLACING FIRST "&&&&" BY INPUT-BRNO
              GO TO END-ROUTINE.

      * ONLY PROCESS FILES FOR THE MACHINE YOU ARE ON

           IF BR-MACHINE NOT = ORIG-MACHINE
              MOVE 4                                   TO RESULT-STATUS
              MOVE "APILNS: BRANCH NOT ON THIS SERVER" TO RESULT-MSG
              GO TO END-ROUTINE.

      * WANT STAGE TO HAVE FULL PATHNAME, NO ../..
           MOVE ORIG-FIL     TO STAGE-ALL-BASE
                                ASCII-BASE.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE
                                              STAGE-ALL-MACHINE
                                              ASCII-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA
                                              STAGE-ALL-DATA.

           MOVE "B"  TO BW-PATH-OVERRIDE
                        LN-PATH-OVERRIDE
                        LTD-PATH-OVERRIDE
                        GB-PATH-OVERRIDE.
           
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

      * CHANGE THE EXTERNAL FILPATH TO THAT OF THE CURRENT BRANCH

           MOVE BR-MACHINE            TO EXT-FILPATH-MACHINE.
           MOVE BR-DATA-PATH          TO EXT-FILPATH-DATA.
           SET ENVIRONMENT "FIL"      TO EXT-FILPATH-FIL.
           MOVE ORIG-POSTNAME         TO WORK-USERID.
           MOVE BR-NO                 TO WORK-BR.
           SET ENVIRONMENT "POSTNAME" TO WORK-USERID.
           MOVE BR-NO                 TO BRANCH.


      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                                     *
      * FROM THIS POINT FORWARD THE USER HAS BE PLACED, LOGICALLY, INTO     *
      * THE BRANCH FOR WHICH HE/SHE IS POSTING FOR.                         *
      *                                                                     *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

 
      * READ STAGEFILE AND CREATE LOAN AND DISBURSEMENT
           IF INPUT-PROCESS-CODE = 3
              PERFORM CREATE-LOAN-AND-DISBURSEMENT 
              GO TO END-ROUTINE.

      * READ STAGEFILE AND DELETE IT, CUSTOMER DIDNT SIGN DOCS
           IF INPUT-PROCESS-CODE = 4
              PERFORM REMOVE-STAGEFILE
              GO TO END-ROUTINE.

      * PASSING CAN LOAN #
          
           MOVE BR-CAN-GROUP-ID TO CL-GROUP-ID.
           MOVE INPUT-CAN-LOAN  TO CL-CAN-NO.
           PERFORM READ-CL1-FILE.
           IF IO-FG NOT = 0
              MOVE 12                             TO RESULT-STATUS
              MOVE "APILNS: CAN LOAN NOT ON FILE" TO RESULT-MSG
              GO TO END-ROUTINE.

           MOVE CL-PD-REC TO PD-REC.

           MOVE TRANS-DATE TO PD-LOANDATE PD-INTDATE PD-ENTDATE.
           PERFORM MAKE-DATES-CURRENT.
           MOVE BR-STATE   TO PD-ORGST
                              PD-OWNST.
           MOVE BR-NO      TO PD-ORGBR
                              PD-OWNBR.

           MOVE PD-CLASS TO LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG = 9
              MOVE 13                      TO RESULT-STATUS
              MOVE "APILNS: INVALID CLASS" TO RESULT-MSG
              GO TO END-ROUTINE.

           PERFORM FIND-EFFECTIVE-SPR.
           IF IO-FG = 9
              MOVE 14                                 TO RESULT-STATUS
              MOVE "APILNS: NO EFFECTIVE SPR FOR CAN" TO RESULT-MSG
              GO TO END-ROUTINE.

      * VERIFY THAT ALL THE FEECODES WITHIN THE CAN LOAN EXIST
      * FOR THE BRANCH
      
           MOVE 0 TO IO-FG.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL 
                                           (SUB > 10 OR IO-FG = 9)
              IF PD-FEECODE(SUB) NOT = " "
                 MOVE PD-FEECODE(SUB) TO CDB-CODE
                 PERFORM GET-CODE
              END-IF
           END-PERFORM.
           IF IO-FG = 9
              MOVE 15                        TO RESULT-STATUS
              MOVE "APILNS: INVALID FEECODE" TO RESULT-MSG
              GO TO END-ROUTINE.


           MOVE INPUT-SSNO TO PD-SSNO(1).
           MOVE "M"        TO PD-MAKERCD(1).
           MOVE LOW-VALUE TO PD2-KEY.
           MOVE SPACES TO PD2-BWNAME.
           MOVE BRANCH TO PD2-BRANCH
                          PD-BRANCH
                          PD-POSTING-BR
                          PD-OWNBR
                          PD-ORGBR.

           MOVE CL-CAN-NO TO PD-CAN-LOAN-NO.
           MOVE PD-PROCEEDS TO PD-CAN-LOAN-MAX.

           PERFORM COMPUTE-GL-FEEAMT.
           MOVE 800 TO CIP-NO-OF-TIMES-CALLED.

      * CREATE EMPTY COLLATERAL WORK RECORD
           PERFORM CREATE-COLLATERAL-TRAILER.

           PERFORM VALID-INS-TYPES-ELIG.

           PERFORM SETUP-CALC-AREA.

           PERFORM D17-TEST-FOR-GEORGIA-SCHG.
           PERFORM D18-TEST-FOR-SC-SCHG.
           PERFORM TEST-FOR-WI-SCHG.
           PERFORM D18-TEST-FOR-MFEE.

           MOVE CIP-CALCULATION-INPUT-AREA TO
                                  COP-CALCULATION-OUTPUT-AREA.

      * CHECK FOR RESTRICTIONS THAT CAME BACK
      * *****XXXXXXX ????

           MOVE 800 TO CIP-NO-OF-TIMES-CALLED.

           MOVE "Y" TO VALID-LOAN.

           PERFORM CALL-LOAN-CALCULATIONS.
           IF COP-CALCULATION-OK
              MOVE "Y" TO EXT-API-SCREEN-DISABLE
              PERFORM VERIFY-AGAINST-SPEDT 
              MOVE " " TO EXT-API-SCREEN-DISABLE
              IF EXT-API-MESS NOT = SPACES
                 MOVE 18                       TO RESULT-STATUS
                 MOVE "APILNS:  SPEDT ERROR"   TO RESULT-MSG
                 GO TO END-ROUTINE
              END-IF
           ELSE
              IF COP-RETURN-CODE = "V"
                 MOVE 17                               TO RESULT-STATUS
                 MOVE "APILNS: CALC ERROR, MAX EXCEED" TO RESULT-MSG
                 GO TO END-ROUTINE
              ELSE
                 MOVE 18                       TO RESULT-STATUS
                 MOVE "CALC ERROR, INACCURATE" TO RESULT-MSG
                 GO TO END-ROUTINE.

      * OUTPUT FROM VERIFY-AGAINST-SPEDT
           IF VALID-LOAN = "N"
              MOVE 16                                 TO RESULT-STATUS
              MOVE "APILNS: INSURANCE COVERAGE ERROR" TO RESULT-MSG
              GO TO END-ROUTINE.

      * MUST SET ALL PAYMENTS CORRECTLY
        
           IF PD-ORGTERM = 1
              MOVE 0 TO PD-NOREGPY
              IF PD-1STPYAMT = 0
                 MOVE PD-REGPYAMT TO PD-1STPYAMT
              ELSE
                 NEXT SENTENCE
           ELSE
              IF PD-ORGTERM = 2
                 MOVE 0 TO PD-NOREGPY
                 IF PD-1STPYAMT = 0
                    MOVE PD-REGPYAMT TO PD-1STPYAMT
                                        PD-LASTPYAMT
                 ELSE
                    MOVE PD-REGPYAMT TO PD-LASTPYAMT
              ELSE
                 MOVE PD-REGPYAMT TO PD-LASTPYAMT
                 SUBTRACT 2 FROM PD-ORGTERM GIVING PD-NOREGPY
                 IF PD-1STPYAMT = 0
                    MOVE PD-REGPYAMT TO PD-1STPYAMT.

      * XONPC0 WHEN YOU TYPE "U" TO UPDATE;
           PERFORM VERIFY-REQD-FLDS.
           IF IO-FG = 9
              GO TO END-ROUTINE.

      * TOOK BELOW FROM LNCRE2 - LIVE CHECKS

           PERFORM SAVE-SCREEN.

      D    STOP MESS.
      * USING THIS SO SEND-MESS DOESNT GO TO SCREEN IN LNVERI
           MOVE "API   " TO LPLOAN-CALLED-FROM.
           MOVE "Y" TO EXT-API-SCREEN-DISABLE.
           MOVE "CL/LNVERI" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRORS-EXIST
                                 PROG-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE " " TO EXT-API-SCREEN-DISABLE.
           IF ERRORS-EXIST = "Y"
              MOVE 19                         TO RESULT-STATUS
      * ACTUAL LNVERI ERROR MESSAGE
              MOVE RETURN-ERR-BUF             TO RESULT-MSG
              GO TO END-ROUTINE.

      *******************************************************
      *******************************************************

           IF INPUT-PROCESS-CODE = 1
              PERFORM CREATE-ASCII-CALC-FILE
              GO TO END-ROUTINE
           ELSE
              IF INPUT-PROCESS-CODE = 2
                 PERFORM CREATE-STAGE-FILE
                 IF RESULT-STATUS NOT = 0
                    GO TO END-ROUTINE
                 ELSE
                    PERFORM DOC-PRINT
                    GO TO END-ROUTINE.


      *******************************************************
      *******************************************************

           MOVE 0                   TO RESULT-STATUS.
           MOVE "LOAN CREATE SUCCESSFUL" TO RESULT-MSG.

       END-ROUTINE.
           MOVE TEMP-PATH TO ACCESS-BUF.
           CALL "C$TOLOWER" USING RM-CMD, VALUE 2.
           PERFORM REMOVE-WORKFILE.
           PERFORM CLOSE-OP-FILE.

           GO TO END-PROGRAM.
 
      *********************************
       CREATE-STAGE-FILE SECTION.
      *********************************
      
      * MUST HAVE ACCOUNT NUMBER FOR THE DOCUMENTS

             IF PD-SMALLOAN = "Y" 
                PERFORM GET-ACCOUNT-NUMBER
             ELSE
                IF PD-SMALLOAN = "N" 
                   PERFORM GET-ACCOUNT-NUMBER
                ELSE
                   MOVE 20                            TO RESULT-STATUS
                   MOVE "APILNS: ERROR NEXT ACCT NUMBER" TO RESULT-MSG
                   GO TO EXIT-CREATE-STAGE-FILE.

           IF RESULT-STATUS NOT = 99
              GO TO EXIT-CREATE-STAGE-FILE.

           MOVE STAGE-ALL-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              PERFORM OPEN-STAGE-FILE-OUTPUT
              PERFORM CLOSE-STAGE-FILE.

           PERFORM OPEN-STAGE-FILE.

           MOVE INPUT-BRNO         TO STAGE-BRNO.
           MOVE INPUT-SSNO         TO STAGE-SSNO.
           MOVE INPUT-CORRELATION  TO STAGE-CORRELATION.
           MOVE INPUT-CAN-LOAN     TO STAGE-CAN-LOAN.
           MOVE PD-RECORD          TO STAGE-PD-REC.
           PERFORM WRITE-STAGE-FILE.
           IF IO-FG NOT = 0 
              PERFORM REWRITE-STAGE-FILE.
           IF IO-FG NOT = 0
              MOVE 21                          TO RESULT-STATUS
              MOVE "APILNS: ERROR CREATING STAGE FILE" TO RESULT-MSG
           ELSE
              MOVE 0                           TO RESULT-STATUS
              MOVE "STAGE FILE CREATE SUCCESSFUL; &&&&&&&&"
                                          TO RESULT-MSG
              INSPECT RESULT-MSG REPLACING FIRST "&&&&&&&&"
                                           BY PD-NUMBER.

       EXIT-CREATE-STAGE-FILE.
           EXIT.

      *********************************
       GET-ACCOUNT-NUMBER SECTION.
      *********************************
           MOVE 0 TO STAGE-LN-NUMBER.

           MOVE PD-OWNBR TO GB-BRNO.
           MOVE 1        TO GB-NO.
           PERFORM OPEN-GB-FILE.
           PERFORM READ-GB-FILE.
           IF IO-FG = 9
              MOVE 22                             TO RESULT-STATUS
              MOVE "APILNS: ERROR READING GBFILE" TO RESULT-MSG
              GO TO EXIT-GET-ACCOUNT-NUMBER.

           IF PD-SMALLOAN = "Y"
              IF GB-NEXT-SMALL = 0
                 ADD 1 TO GB-NEXT-SMALL
              END-IF
              MOVE GB-NEXT-SMALL TO STAGE-LN-NUMBER
              ADD 1 TO GB-NEXT-SMALL
           ELSE
              IF GB-NEXT-OTHER = 0
                 ADD 1 TO GB-NEXT-OTHER
              END-IF
              MOVE GB-NEXT-OTHER TO STAGE-LN-NUMBER
              ADD 1 TO GB-NEXT-OTHER.

           PERFORM REWRITE-GB-FILE.
           PERFORM CLOSE-GB-FILE.

           MOVE STAGE-LN-NUMBER TO PD-ACCTNO.
           MOVE PD-ACCTNO       TO LOAN-KEY.

       EXIT-GET-ACCOUNT-NUMBER.
           EXIT.

      *********************************
       DOC-PRINT SECTION.
      *********************************
      * CAN NOT CALL APIDOC FROM HERE AS IT HAS ITS OWN RUNCBL AS IT
      * HAS CHAINING IN THE PROCEDURE DIVISION. IT WILL NOT COME BACK TO HERE
      * DUPLICATE CODE IN HERE

           SET ENVIRONMENT "CALLED_FROM" TO "APILNS".

            PERFORM GET-SP.
            IF IO-FG NOT = 0
              MOVE 23                         TO RESULT-STATUS
              MOVE "APILNS: MISSING SPR"      TO RESULT-MSG
              GO TO EXIT-DOC-PRINT.

           MOVE SPACES TO HOLD-OPT-OUT
                          HOLD-MAKER-FNAME
                          HOLD-MAKER-LNAME
                          HOLD-COMAKER-FNAME
                          HOLD-COMAKER-LNAME
                          HOLD-COMAKER-OPT-OUT.
           MOVE 0      TO HOLD-MAKER-DOB.

           PERFORM GET-BORROWERS.
           IF IO-FG = 9
              GO TO EXIT-DOC-PRINT.

           PERFORM PRINT-ALL-DOCUMENTS.

      * IF YOU COME BACK WITH NO ERRORS IN RESULT-STATUS, RUN WAS GOOD
           IF RESULT-STATUS = 99
              MOVE 0                           TO RESULT-STATUS
              MOVE "DOC PRINT SUCCESSFUL; &&&&&&&&"
                                          TO RESULT-MSG
              INSPECT RESULT-MSG REPLACING FIRST "&&&&&&&&"
                                           BY PD-NUMBER.

       EXIT-DOC-PRINT.
           EXIT.
           
      *****************************
       GET-BORROWERS SECTION.
      *****************************

           PERFORM OPEN-BW1-FILE.
           MOVE 1 TO SUB.
       GET-NEXT-BW.
           MOVE SPACES       TO BW-LNAME BW-FNAME.
           IF PD-SSNO(SUB) NOT = 0
              MOVE PD-SSNO(SUB) TO BW-SSNO
              PERFORM READ-BW1-FILE
              IF IO-FG = 9
                 MOVE 24                         TO RESULT-STATUS
                 MOVE "APILNS: INVALID BORROWER" TO RESULT-MSG
                 GO TO EXIT-GET-BORROWERS
              ELSE
                 PERFORM APPEND-BW-NAMES
                 IF SUB = 2
                    MOVE BW-FNAME           TO HOLD-COMAKER-FNAME
                    MOVE BW-LNAME           TO HOLD-COMAKER-LNAME
                    MOVE BW-PRIVACY-OPT-OUT TO HOLD-COMAKER-OPT-OUT.
           
           IF SUB = 1
              MOVE BW-PRIVACY-OPT-OUT TO HOLD-OPT-OUT
              MOVE BW-FNAME           TO HOLD-MAKER-FNAME
              MOVE BW-LNAME           TO HOLD-MAKER-LNAME
              MOVE BW-BDATE           TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY
              MOVE DATE-MMDDYYYY      TO HOLD-MAKER-DOB.
           IF SUB < 4
              ADD 1 TO SUB
              GO TO GET-NEXT-BW.

           PERFORM CLOSE-BW1-FILE.

       EXIT-GET-BORROWERS.
           EXIT.

      *********************************
       CREATE-ASCII-CALC-FILE SECTION.
      *********************************
      D    STOP MESS.

           CALL "C$TOLOWER" USING ASCII-DIRECTORY, VALUE 5.
           MOVE TRANS-DATE    TO DATE-MMDDYY.
           MOVE DATE-MMDDYY-YY TO ASCII-PATH-YY.
           MOVE DATE-MMDDYY-MM TO ASCII-PATH-MM.
           MOVE DATE-MMDDYY-DD TO ASCII-PATH-DD.
           MOVE INPUT-CORRELATION TO ASCII-CORRELATION.
           PERFORM OPEN-ASCII-FILE-OUTPUT.

           MOVE PD-ORGST TO ASCII-ORGST.
           MOVE PD-ORGBR TO ASCII-ORGBR.
           MOVE PD-SUBCLASS TO ASCII-SUBCLASS.
           MOVE PD-FILEDATE TO ASCII-FILEDATE.
           MOVE PD-LOANDATE TO ASCII-LOANDATE.
           MOVE PD-INTDATE TO ASCII-INTDATE.
           MOVE PD-1STPYDATE TO ASCII-1STPYDATE.
           MOVE PD-ORGTERM TO ASCII-ORGTERM.
           MOVE PD-PURCD TO ASCII-PURCD.
           MOVE PD-COLLCD TO ASCII-COLLCD.
           MOVE PD-SRCD TO ASCII-SRCD.
           MOVE PD-PROCEEDS TO ASCII-PROCEEDS.
           MOVE PD-INSCODES TO ASCII-INSCODES.
           MOVE PD-LNAMT TO ASCII-LNAMT.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
             MOVE PD-FEECODE(SUB) TO ASCII-FEECODE(SUB)
             MOVE PD-FEEAMT(SUB)  TO ASCII-FEEAMT(SUB)
           END-PERFORM.

           MOVE PD-POINTS TO ASCII-CLSFEE.
           MOVE PD-COLLID TO ASCII-COLLID.
           MOVE PD-EFFRATE TO ASCII-EFFRATE.
           MOVE PD-SMPRATE TO ASCII-SMPRATE.
           MOVE PD-APRATE TO ASCII-APRATE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE PD-INSCOMP(SUB) TO ASCII-INSCOMP(SUB)
              MOVE PD-INSPREM(SUB) TO ASCII-INSPREM(SUB)
              MOVE PD-INSCOVR(SUB) TO ASCII-INSCOVR(SUB)
              MOVE PD-INSOURS(SUB) TO ASCII-INSOURS(SUB)
              MOVE PD-INSEFF-DATE(SUB) TO ASCII-INSEFF(SUB)
              MOVE PD-INSEXP-DATE(SUB) TO ASCII-INSEXP(SUB)
              MOVE PD-INSCOMM(SUB) TO ASCII-INSCOMM(SUB)
           END-PERFORM.
 
           IF PD-POINTS NOT = 0
              COMPUTE ASCII-SERCHG = PD-SERCHG + PD-POINTS
           ELSE
              MOVE PD-SERCHG TO ASCII-SERCHG.
           COMPUTE ASCII-MAINTFEE = PD-MAINTFEE * PD-ORGTERM.

           MOVE PD-EXTCHG TO ASCII-EXTCHG.
           MOVE PD-INTCHG TO ASCII-INTCHG.
           MOVE PD-1STPYAMT TO ASCII-1STPYAMT.
           MOVE PD-REGPYAMT TO ASCII-REGPYAMT.
           PERFORM LOAN-CALCULATIONS.
           MOVE DISCLOSED-FINAMT TO ASCII-FINAMT.
           MOVE TOTAL-REPAY      TO ASCII-REPAY.

           MOVE 1 TO ASCII-KEY.
           PERFORM WRITE-ASCII-FILE.
           IF IO-FG NOT = 0
              MOVE 25                            TO RESULT-STATUS
              MOVE "APILNS: ASCII FILE CREATE FAILED" TO RESULT-MSG
           ELSE
              MOVE 0                   TO RESULT-STATUS
              MOVE "ASCII FILE CREATE SUCCESSFUL" TO RESULT-MSG.
  
      *****************************
       PRINT-ALL-DOCUMENTS SECTION.
      *****************************

           SET ENVIRONMENT "PST_OUTPUT" TO "A".

       PRINT-ALL-DOCUMENTS-ASK.

           PERFORM OPEN-FD1-FILE.
         
           MOVE ORIG-FIL                 TO PST-DATA-PATH.

      * CREATE ORIGINAL MANIFEST WHILE BUILDING DOCUMENTS WITHOUT THE M
      * WHEN THEY ARE ALL DONE, MOVE THE MANIFEST FILE TO HAVE THE M SO
      * THE API KNOWS THEY ARE ALL DONE CREATED
           MOVE PST-FIL-BASE             TO PST-PATH-MAN-FIL
                                            PST-PATH-MAN-D-AUTO-FIL.
           MOVE PD-OWNBR                 TO PST-PATH-MAN-BRNO
                                            PST-PATH-MAN-D-AUTO-BRNO.
           MOVE PD-NUMBER                TO PST-PATH-MAN-ACCTNO
                                            PST-PATH-MAN-D-AUTO-ACCTNO.

           PERFORM OPEN-MANIFEST-FILE-OUTPUT.
           PERFORM CLOSE-MANIFEST-FILE.
           PERFORM OPEN-MANIFEST-FILE.

           MOVE SPACES TO MANIFEST-FIRST-RECORD
                          MANIFEST-REC.
           MOVE PD-ORGST                 TO MANIFEST-STATE.
           MOVE PD-SSNO(1)               TO HOLD-SSNO.
           MOVE SSNO-LAST-4              TO MANIFEST-LAST-4.
           MOVE PD-OWNBR                 TO MANIFEST-BR-NO.
           MOVE SSNO-MID-2               TO MANIFEST-MID-2.
           MOVE PD-NUMBER                TO MANIFEST-LN-NUMBER.
           MOVE SSNO-FIRST-3             TO MANIFEST-FIRST-3.
           MOVE PD-CLASS                 TO MANIFEST-CLASS.
           MOVE PD-SUBCLASS              TO MANIFEST-SUBCLASS.
           MOVE PD-LNAMT                 TO MANIFEST-LNAMT.
           MOVE HOLD-OPT-OUT             TO MANIFEST-OPT-OUT.
           MOVE HOLD-MAKER-FNAME         TO MANIFEST-MAKER-FNAME.
           MOVE HOLD-MAKER-LNAME         TO MANIFEST-MAKER-LNAME.
           MOVE HOLD-MAKER-DOB           TO MANIFEST-MAKER-DOB.
           MOVE HOLD-COMAKER-FNAME       TO MANIFEST-COMAKER-FNAME.
           MOVE HOLD-COMAKER-LNAME       TO MANIFEST-COMAKER-LNAME.
           MOVE HOLD-COMAKER-OPT-OUT     TO MANIFEST-COMAKER-OPT-OUT.
           MOVE PD-LOANDATE              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY            TO MANIFEST-LOANDATE.
           MOVE X"0D"                    TO MANIFEST-FIRST-CR.
           MOVE MANIFEST-FIRST-RECORD    TO MANIFEST-REC.
           PERFORM WRITE-MANIFEST-FILE.
           PERFORM CLOSE-MANIFEST-FILE.

           MOVE 0 TO SUB.

       PRINT-NEXT-DOCUMENT.
           ADD 1 TO SUB.
           IF SUB > 20
              GO TO END-PRINT-ALL-DOCUMENTS.

           IF SP-PRFORM(SUB) = 0
              GO TO PRINT-NEXT-DOCUMENT.

           MOVE SP-PRFORM(SUB) TO FD-FMNO.
           PERFORM READ-FD1-FILE.
           IF IO-FG NOT = 0
              GO TO PRINT-NEXT-DOCUMENT.

           IF FD-TYPE-OF-DOC = SPACES
              GO TO PRINT-NEXT-DOCUMENT.

           IF ( FD-PROG = GB-CHECK-PROG )
              GO TO PRINT-NEXT-DOCUMENT.

           PERFORM LPFRFD-FIND-PREFIX.

           IF NOT ( FD-OPTION-PST-LASER )
                 GO TO PRINT-NEXT-DOCUMENT.

      D    STOP MESS.
           MOVE FD-PROG       TO IP-PROG.
           MOVE FD-FMNO       TO IP-FMNO.
           MOVE "0"           TO CONT-PRINT.

           ACCEPT DATE-YYYYMMDD FROM CENTURY-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE TO LPLOAN-SYSDATE.

           PERFORM LPFRFD-FIND-PREFIX.
           IF ( LPWSFD-PREFIX-PST )
              MOVE PROG-DIR-PREFIX-PST  TO PROG-DIR-NAME
              MOVE FD-PROG-PREFIX       TO PROG-DIR-PREFIX
              MOVE IP-PROG              TO PROG-WK-IP.

           MOVE PROG-WK TO FORM-NAM.

           CALL FORM-PROGX USING FORM-PATH PROG-BUF IP-BUF PRU-PATH.
           CANCEL FORM-PROGX.
      * FILE ERROR
           IF ERRCD = "E"
              MOVE 33                               TO RESULT-STATUS
              MOVE "APILNS: IO-ERROR IN PST DOC PRINT" TO RESULT-MSG
              GO TO EXIT-PRINT-ALL-DOCUMENTS.

           GO TO PRINT-NEXT-DOCUMENT.

       END-PRINT-ALL-DOCUMENTS.
      
      * THIS MANIFEST WILL BE IN A SUB DIR UNDER DOCS, CALLED AUTO

      * NOW MOVE THE FILE TO HAVE THE LEADING M SO THEY KNOW THE FORMS
      * ARE DONE AND THEY CAN GRAB THEM

           MOVE PST-PATH-MANIFEST           TO PST-WS-MV-MANI-FILE-1.
           MOVE PST-PATH-MANIFEST-DONE-AUTO TO PST-WS-MV-MANI-FILE-2.
           MOVE PST-WS-MV-MANIFEST-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE 28                          TO RESULT-STATUS
              MOVE "APILNS: BAD MANIFEST MOVE" TO RESULT-MSG
              GO TO EXIT-PRINT-ALL-DOCUMENTS.

       EXIT-PRINT-ALL-DOCUMENTS.
            EXIT.

      ********************************************
       CREATE-LOAN-AND-DISBURSEMENT SECTION.
      ********************************************
      D    STOP MESS.

      * TEST RC-STATUS FOR OPEN DAY, DAY MUST BE OPEN
      
           MOVE "A1"       TO RC-STATUS.
           MOVE BR-NO      TO RC-BRNO.
           MOVE TRANS-DATE TO RC-TRANS-DATE.
           PERFORM OPEN-RC2-FILE.
           PERFORM READ-RC2-FILE.
           PERFORM CLOSE-RC2-FILE.
           IF IO-FG NOT = 0
              MOVE 35                      TO RESULT-STATUS
              MOVE "APILNS: DAY NOT OPEN"  TO RESULT-MSG
              GO TO EXIT-CREATE-LOAN-AND-DISBURSEMENT.

      * DAY IS OPEN, RESERVE OP FILE SO THE DAY CANNOT BE CLOSED.
      
           MOVE BR-NO      TO OPEN-BR.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO OPEN-PATH-TRDATE.
           PERFORM LOAD-OP-FILE.
           MOVE OPEN-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE 36                           TO RESULT-STATUS
              MOVE "APILNS: COULD NOT OPEN DAY" TO RESULT-MSG
              GO TO EXIT-CREATE-LOAN-AND-DISBURSEMENT
           ELSE
              PERFORM OPEN-OP-FILE.

           MOVE INPUT-BRNO         TO STAGE-BRNO.
           MOVE INPUT-SSNO         TO STAGE-SSNO.
           MOVE INPUT-CORRELATION  TO STAGE-CORRELATION.
           MOVE INPUT-CAN-LOAN     TO STAGE-CAN-LOAN.
           PERFORM OPEN-STAGE-FILE.
           PERFORM READ-STAGE-FILE.
           IF IO-FG NOT = 0 
              MOVE 37                           TO RESULT-STATUS
              MOVE "APILNS: MISSING STAGEFILE RECORD" TO RESULT-MSG
              GO TO EXIT-CREATE-LOAN-AND-DISBURSEMENT.

           MOVE STAGE-PD-REC  TO PD-RECORD.

           PERFORM SAVE-SCREEN.

           PERFORM GET-SP.
           IF IO-FG NOT = 0
              MOVE 23                         TO RESULT-STATUS
              MOVE "APILNS: MISSING SPR"      TO RESULT-MSG
              GO TO EXIT-CREATE-LOAN-AND-DISBURSEMENT.

      * UPDATE LOAN - TOOK FROM LNCRE2 LIVE CHECKS

      * CALL DIRECT LOAN UPDATE:
      * USING THIS FOR NOW SO MESSAGES DONT DISPLAY

      D    STOP MESS.
           MOVE "LNCRE2" TO LPLOAN-CALLED-FROM.
           MOVE SPACES   TO LPLOAN-RETURN-ERR-BUF.
           MOVE "LP/LONPC2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME PROG-BUF
                                 UPDATE-BUF.
           CANCEL FORM-PROGX.

           IF LPLOAN-RETURN-ERR-BUF NOT = SPACES
              MOVE 38                         TO RESULT-STATUS
      * ACTUAL LONPC2 ERROR MESSAGE
              MOVE LPLOAN-RETURN-ERR-BUF      TO RESULT-MSG
              GO TO EXIT-CREATE-LOAN-AND-DISBURSEMENT.

      * PREPARE FOR DEBIT CARD DISBURSEMENT (LONPD0)

      * TEST FOR ADJUSTMENT OF INTEREST REBATE FOR MISSISSIPPI:
      *                                         OR SOURTH CAROLINA:
      *                                         OR ALABAMA
           IF SP-RBEARLY(7) = 3 OR 19 OR 16
              MOVE LOAN-KEY   TO REBL-LN-ACCTNO
              MOVE PD-SSNO(1) TO REBL-LN-SSNO
              MOVE TRANS-DATE TO REBL-TRANS-DATE
              MOVE 0          TO REBL-REBATE-ADJ
              MOVE BR-NOT-OURS-REFUND TO REBL-BR-NOT-OURS-REFUND
              MOVE "CL/REBL01" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH SP-REC REBL-WORKERS
                   ON OVERFLOW
                      MOVE 39                     TO RESULT-STATUS
                      MOVE "APILNS: REBL01 OVERFLOW" TO RESULT-MSG
                      GO TO EXIT-CREATE-LOAN-AND-DISBURSEMENT.
           IF SP-RBEARLY(7) = 3 OR 19 OR 16
              CANCEL FORM-PROGX.

           PERFORM OPEN-LN1-FILE.
           MOVE LOAN-KEY TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE 40                             TO RESULT-STATUS
              MOVE "APILNS: BAD LOAN READ; &&&&&&&&&" TO RESULT-MSG
              INSPECT RESULT-MSG REPLACING FIRST "&&&&&&&&&"
                                           BY LOAN-KEY
              GO TO EXIT-CREATE-LOAN-AND-DISBURSEMENT.

           PERFORM UPDATE-LT-DEBIT-CARD.
           IF IO-FG NOT = 0
              MOVE 41                               TO RESULT-STATUS
              MOVE "APILNS: FAIL UPDATE DEBIT CARD" TO RESULT-MSG
              GO TO EXIT-CREATE-LOAN-AND-DISBURSEMENT.

           MOVE "Y" TO LN-PROC-DISBURSED.
           PERFORM REWRITE-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE 42                            TO RESULT-STATUS
              MOVE "APILNS: REWRITE LOAN DISBURSED" TO RESULT-MSG
              GO TO EXIT-CREATE-LOAN-AND-DISBURSEMENT.

      * IF ALL WAS SUCCESSFUL, DELETE STAGE RECORD
           PERFORM DELETE-STAGE-FILE.
 
           IF IO-FG = 0
              MOVE 0                           TO RESULT-STATUS
              MOVE "CREATE LOAN SUCCESSFUL; &&&&&&&&"
                                          TO RESULT-MSG
              INSPECT RESULT-MSG REPLACING FIRST "&&&&&&&&"
                                           BY LN-ACCTNO.

       EXIT-CREATE-LOAN-AND-DISBURSEMENT.
           EXIT.

      *********************************
       UPDATE-LT-DEBIT-CARD SECTION.
      ********************************
           PERFORM OPEN-LTD1-FILE.

           PERFORM CLEAR-LTD-FILE.

           MOVE LN-OWNBR  TO LTD-BRNO.
           MOVE LN-ACCTNO TO LTD-ACCTNO.
           MOVE 1         TO LTD-SEQNO.

           MOVE INPUT-DEBIT-CARD TO LTD-DEBIT-CARD-NUMBER.
           MOVE LN-PROCEEDS      TO LTD-DEBIT-CARD-AMT.
           MOVE LN-LOANDATE      TO LTD-LOANDATE.
           PERFORM WRITE-LTD1-FILE.

      *******************************************************
       CALL-LOAN-CALCULATIONS SECTION.
      ********************************************************

      * SETUP VERIFY-REJECT-TERMS RE: SP-INSREJECT-FG = "C":
           PERFORM ESTABLISH-REJECT-TERMS.

      *-----------------------------------------------------------------
      * THE LOAN CALCULATOR WILL DEFAULT 1ST PAYMENT TO ONE MONTH FROM INT
      * IF A CAN LOAN.  THAT IS OK FOR THE CAN LOAN BUILDER BUT WHEN
      * BOOKING THE LOAN WE NEED TO MAKE SURE 1ST PAYMENT IS ADVANCED
      * CORRECTLY IF THE GP FLAG IS SET TO DO SO.   THE CALCULATOR DOES
      * NOT DIFFERENTIATE BETWEEN CAN LOAN BUILDER OR LOAN ENTRY THAT
      * I CAN FIND SO I AM MANIPULATING THE CAN LOAN NUMBER HERE.
      * SOME ISSUES WITH REGIONAL MANAGMENT (AND POSSIBLY ALL),
      * AS PER JIM EXCLUDING THEN UNTIL RESOLUTION CAN BE FOUND.
      *------------------------------------------------------------------
           IF NOT (SP-ADV-TO-1ST-NO-CORR = "N" OR " ")
              MOVE SP-ADV-TO-1ST-NO-CORR TO ADV-TO-1ST-NO-CORR
           ELSE
              MOVE GP-ADV-TO-1ST-NO-CORR TO ADV-TO-1ST-NO-CORR.
           IF ( ADV-TO-1ST-NO-CORR NOT = "A") 
              MOVE PD-CAN-LOAN-NO TO CIP-CAN-LOAN-NO.

           MOVE "LP/CALCZL" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                CIP-CALCULATION-INPUT-AREA
                                CEP-CALCULATION-EDIT-AREA
                                COP-CALCULATION-OUTPUT-AREA
                                SP-REC
                                BR-REC BW-1-STATE BW-1-CITY-TAX-CD
                                BW-1-COUNTY.
           CANCEL FORM-PROGX.

           MOVE COP-PRLOAN-RESTRICTED TO PRLOAN-RESTRICTED.

           MOVE COP-PROCEEDS TO PD-PROCEEDS.
           MOVE PD-PROCEEDS TO PD-CAN-LOAN-MAX.
           MOVE COP-PROCEEDS-SW TO PD-PROCEEDS-FG.
           MOVE COP-POINTS TO PD-POINTS.
           MOVE COP-POINTS-SW TO PD-POINTS-FG.
           MOVE COP-OVR-RATE TO PD-OVRDRATE.
           MOVE COP-OVR-RATE-SW TO PD-OVRDRATE-FG.
           MOVE COP-EFF-RATE TO PD-EFFRATE.
           MOVE COP-SIM-RATE TO PD-SMPRATE.
           MOVE COP-APR-RATE TO PD-APRATE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE COP-INS-CO(SUB) TO PD-INSCOMP(SUB)
              MOVE COP-INS-USE-SW(SUB) TO PD-INSOURS(SUB)
              MOVE COP-INS-TERM(SUB) TO PD-INSTRM(SUB)
              MOVE COP-INS-COVERAGE(SUB) TO PD-INSCOVR(SUB)
              MOVE COP-INS-PREMIUM(SUB) TO PD-INSPREM(SUB)
              MOVE COP-INS-SW(SUB) TO PD-INS-FG(SUB)
              MOVE COP-INSFINCHG-SW(SUB) TO PD-INSFINCHG(SUB)
              MOVE COP-INS-TERM-SW(SUB) TO PD-INS-TERM-SW(SUB)
              MOVE COP-COVERAGE-SW(SUB) TO PD-COVERAGE-SW(SUB)
              MOVE COP-PREMIUM-SW(SUB) TO PD-PREMIUM-SW(SUB)
           END-PERFORM.

           MOVE COP-SERVICE-CHG TO PD-SERCHG
                                   PD-SUM-SERCHG.
           MOVE COP-STATE-WORKER TO PD-STATE-WORKER.
           MOVE COP-SERVICE-CHG-SW TO PD-SERCHG-FG.
           MOVE COP-MAINT-FEES TO PD-SUM-MAINTFEE.
           DIVIDE COP-MAINT-FEES BY COP-TERM GIVING 
                                     PD-MAINTFEE ROUNDED.
           MOVE COP-MAINT-FEES-SW TO PD-MAINTFEE-FG.
           MOVE COP-1ST-PY-EXT TO PD-EXTCHG
                                  PD-SUM-EXTCHG.

           MOVE COP-1ST-PY-EXT-SW TO PD-EXTCHG-FG.
           MOVE COP-INT-CHARGE TO PD-INTCHG
                                  PD-SUM-INTCHG.
           MOVE COP-APR-FINANCD TO PD-APR-FINAMT.
           MOVE COP-AMT-FINANCD TO PD-FINAMT.
           MOVE COP-LOAN-AMOUNT TO PD-LNAMT
                                   PD-SUM-LNAMT
                                   PD-PURBAL
                                   PD-ORIGPURAMT
                                   PD-SUM-ORIGPURAMT.
           IF PD-LOANTYPE = "I"
              MOVE PD-SERCHG TO PD-ANTICERN(3)
              MOVE 0 TO PD-ANTICERN(1)
                        PD-ANTICERN(2)
              IF GP-ALLOW-MF-ON-IB = "Y"
                 MOVE COP-MAINT-FEES TO PD-ANTICERN(4)
              ELSE
                 MOVE 0 TO PD-ANTICERN(4)
              END-IF
           ELSE
              ADD COP-INT-CHARGE COP-1ST-PY-EXT GIVING PD-ANTICERN(1)
              MOVE COP-SERVICE-CHG TO PD-ANTICERN(3)
              MOVE COP-MAINT-FEES TO PD-ANTICERN(4).

           MOVE 0 TO PD-ANTICERN(5).

           IF SP-LNCOST-FRMLA NOT = "M"
              MOVE ZEROS TO PD-ANTICERN(6).

            MOVE PD-ANTICERN(1) TO WK-ANTICERN(1).
            MOVE PD-ANTICERN(2) TO WK-ANTICERN(2).
            MOVE PD-ANTICERN(3) TO WK-ANTICERN(3).
            MOVE PD-ANTICERN(4) TO WK-ANTICERN(4).
            MOVE PD-ANTICERN(5) TO WK-ANTICERN(5).
            MOVE PD-ANTICERN(6) TO WK-ANTICERN(6).
            PERFORM ADJUST-ANTICERN.
            MOVE WK-ANTICERN(1) TO PD-ANTICERN(1).
            MOVE WK-ANTICERN(2) TO PD-ANTICERN(2).
            MOVE WK-ANTICERN(3) TO PD-ANTICERN(3).
            MOVE WK-ANTICERN(4) TO PD-ANTICERN(4).
            MOVE WK-ANTICERN(5) TO PD-ANTICERN(5).
            MOVE WK-ANTICERN(6) TO PD-ANTICERN(6).

           MOVE COP-ODD-1ST-PMT TO PD-1STPYAMT.
           MOVE COP-REG-PAYMENT TO PD-REGPYAMT
                                   PD-SUM-REGPYAMT.
           MOVE COP-REG-PAYMENT-SW TO PD-REGPYAMT-FG.
           MOVE COP-FINCHG TO PD-FINCHG PD-SUM-FINCHG.

      * XXXXX ****** COP-RETURN-CODE
      *    MOVE COP-RETURN-CODE TO DF2-RETURN-CODE.
      *    MOVE COP-LOOP-COUNT TO DF2-LOOP-COUNT.

           IF COP-1ST-PMT-MTHS = 1 AND
              COP-1ST-PMT-DAYS = 0
              MOVE "Y" TO PD-ONE-MTH-NO-DAY-EXT-SW
           ELSE
              MOVE " " TO PD-ONE-MTH-NO-DAY-EXT-SW.


           IF ((BR-FORCE-VALID-CITY-COUNTY = "Y") AND 
              (SP-INSURANCE-WRITTEN NOT = "N"))
              IF COP-INS-TAX(1) = 0
                 MOVE " "            TO PD-FEECODE(5)
                 MOVE " "            TO PD-FEEOURS(5)
                 MOVE " "            TO PD-FEEFINCHG(5)
                 MOVE 0              TO PD-FEEAMT(5)
              ELSE
                 MOVE "TC"           TO PD-FEECODE(5)
                 MOVE "Y"            TO PD-FEEOURS(5)
                 MOVE "N"            TO PD-FEEFINCHG(5)
                 MOVE COP-INS-TAX(1) TO PD-FEEAMT(5)
              END-IF
   
              IF COP-INS-TAX(2) = 0
                 MOVE " "            TO PD-FEECODE(6)
                 MOVE " "            TO PD-FEEOURS(6)
                 MOVE " "            TO PD-FEEFINCHG(6)
                 MOVE 0              TO PD-FEEAMT(6)
              ELSE
                 MOVE "TA"           TO PD-FEECODE(6)
                 MOVE "Y"            TO PD-FEEOURS(6)
                 MOVE "N"            TO PD-FEEFINCHG(6)
                 MOVE COP-INS-TAX(2) TO PD-FEEAMT(6)
              END-IF
   
              IF COP-INS-TAX(3) = 0
                 MOVE " "            TO PD-FEECODE(7)
                 MOVE " "            TO PD-FEEOURS(7)
                 MOVE " "            TO PD-FEEFINCHG(7)
                 MOVE 0              TO PD-FEEAMT(7)
              ELSE
                 MOVE "TP"           TO PD-FEECODE(7)
                 MOVE "Y"            TO PD-FEEOURS(7)
                 MOVE "N"            TO PD-FEEFINCHG(7)
                 MOVE COP-INS-TAX(3) TO PD-FEEAMT(7)
              END-IF
   
              IF COP-INS-TAX(4) = 0
                 MOVE " "            TO PD-FEECODE(8)
                 MOVE " "            TO PD-FEEOURS(8)
                 MOVE " "            TO PD-FEEFINCHG(8)
                 MOVE 0              TO PD-FEEAMT(8)
              ELSE
                 MOVE "T1"           TO PD-FEECODE(8)
                 MOVE "Y"            TO PD-FEEOURS(8)
                 MOVE "N"            TO PD-FEEFINCHG(8)
                 MOVE COP-INS-TAX(4) TO PD-FEEAMT(8)
              END-IF
   
              IF COP-INS-TAX(5) = 0
                 MOVE " "            TO PD-FEECODE(9)
                 MOVE " "            TO PD-FEEOURS(9)
                 MOVE " "            TO PD-FEEFINCHG(9)
                 MOVE 0              TO PD-FEEAMT(9)
              ELSE
                 MOVE "T2"           TO PD-FEECODE(9)
                 MOVE "Y"            TO PD-FEEOURS(9)
                 MOVE "N"            TO PD-FEEFINCHG(9)
                 MOVE COP-INS-TAX(5) TO PD-FEEAMT(9)
              END-IF
   
              IF COP-INS-TAX(6) = 0
                 MOVE " "            TO PD-FEECODE(10)
                 MOVE " "            TO PD-FEEOURS(10)
                 MOVE " "            TO PD-FEEFINCHG(10)
                 MOVE 0              TO PD-FEEAMT(10)
              ELSE
                 MOVE "T3"           TO PD-FEECODE(10)
                 MOVE "Y"            TO PD-FEEOURS(10)
                 MOVE "N"            TO PD-FEEFINCHG(10)
                 MOVE COP-INS-TAX(6) TO PD-FEEAMT(10)
              END-IF.
   
   
           MOVE COP-CALCULATION-OUTPUT-AREA TO
                                 CIP-CALCULATION-INPUT-AREA.

           IF PD-1STPYAMT = 0
              MOVE 0 TO PD-NOODDPY
              MOVE PD-ORGTERM TO PD-NOREGPY
           ELSE
              MOVE 1 TO PD-NOODDPY
              SUBTRACT 1 FROM PD-ORGTERM GIVING PD-NOREGPY.

           IF COP-AMT-FINANCD > COP-LOAN-AMOUNT
              MOVE "X" TO COP-RETURN-CODE.

      *  TEST FOR PROCEEDS < 0

           COMPUTE AMT-HOLDER = COP-TOTAL-NOTE
                              - COP-NEW-LOAN-PROCEEDS
                              - COP-SERVICE-CHG.
           IF NOT COP-INS-USE-IS-MEMO(1)
              SUBTRACT COP-INS-PREMIUM(1) FROM AMT-HOLDER.
           IF NOT COP-INS-USE-IS-MEMO(2)
              SUBTRACT COP-INS-PREMIUM(2) FROM AMT-HOLDER.
           IF NOT COP-INS-USE-IS-MEMO(3)
              SUBTRACT COP-INS-PREMIUM(3) FROM AMT-HOLDER.
           IF NOT COP-INS-USE-IS-MEMO(4)
              SUBTRACT COP-INS-PREMIUM(4) FROM AMT-HOLDER.
           IF NOT COP-INS-USE-IS-MEMO(5)
              SUBTRACT COP-INS-PREMIUM(5) FROM AMT-HOLDER.
           IF NOT COP-INS-USE-IS-MEMO(6)
              SUBTRACT COP-INS-PREMIUM(6) FROM AMT-HOLDER.
           IF NOT COP-INS-USE-IS-MEMO(7)
              SUBTRACT COP-INS-PREMIUM(7) FROM AMT-HOLDER.
           IF NOT COP-INS-USE-IS-MEMO(8)
              SUBTRACT COP-INS-PREMIUM(8) FROM AMT-HOLDER.
           IF AMT-HOLDER < 0
              MOVE "X" TO COP-RETURN-CODE.

       CALL-LOAN-CALCULATIONS-EXIT.
           EXIT.

      ******************************************************************
       VERIFY-REQD-FLDS SECTION.
      ******************************************************************
           MOVE 0 TO IO-FG.
           MOVE 0 TO SUB BISCAN-CALL-CODE.

           PERFORM DUAL-CHECK 4 TIMES.

           PERFORM TEST-IL-COLLCD.

       EXIT-VERIFY-REQD-FLDS.
           EXIT.

      **********************************************
       DUAL-CHECK SECTION.
      **********************************************
           ADD 1 TO SUB.

           IF ( PD-SSNO(SUB)     = ZEROES ) OR
              ( BISCAN-CALL-CODE = 9      )
              GO TO EXIT-DUAL-CHECK.

           MOVE PD-SSNO(SUB) TO BISCAN-SSNO.
           MOVE PD-SRCD      TO BISCAN-SRCD.
           MOVE PD-PRLNNO    TO BISCAN-PRLNNO.

           IF (SP-DUAL-IN-STATE  NOT = "Y") OR
              (SP-DUAL-IN-BRANCH NOT = "Y")
                 MOVE 2 TO BISCAN-CALL-CODE
                 PERFORM CALL-SCAN.

           IF ( BISCAN-CALL-CODE = 9 )
              MOVE 9 TO IO-FG.

       EXIT-DUAL-CHECK.
           EXIT.

      ******************************************************
       CALL-SCAN SECTION.
      ******************************************************

      * NO DUAL LOANS WITHIN THE SAME CLASS (TAX ADVANCE)
           IF (SP-DUAL-IN-STATE = "C") OR (SP-DUAL-IN-BRANCH = "C")
              MOVE 0                 TO BISCAN-CALL-CODE
      * USE BISCAN-SSNO BECAUSE IT GETS SET PROPERLY THROUGHOUT THE PROGRAM
              MOVE BISCAN-SSNO       TO BICLAS-SSNO
              MOVE PD-OWNBR          TO BICLAS-BRANCH
              MOVE PD-CLASS          TO BICLAS-CLASS
              MOVE "CL/BICLAS"       TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH BICLAS-WORKERS
              CANCEL FORM-PROGX
              IF BICLAS-RESTRICTED = "Y"
                 MOVE SPACES TO RESULT-MSG
                 STRING "APILNS: SAME CLASS STILL OPEN IN BRANCH; ",
                        BICLAS-BRANCH INTO RESULT-MSG
                 MOVE 29     TO RESULT-STATUS
                 MOVE 9 TO BISCAN-CALL-CODE
                 GO TO EXIT-CALL-SCAN
              ELSE
                 GO TO EXIT-CALL-SCAN.

      * SET EXT-CALLED-FROM SO MESSAGES DONT DISPLAY TO SCREEN
           MOVE "LNCRE2" TO EXT-CALLED-FROM.
           MOVE SPACES   TO EXT-RETURN-ERR-BUF.
           MOVE SP-DUAL-IN-STATE  TO BISCAN-STATE-FG.
           MOVE SP-DUAL-IN-BRANCH TO BISCAN-BRANCH-FG.
           MOVE PD-OWNBR          TO BISCAN-BRANCH.
           MOVE PD-ORGST          TO BISCAN-STATE.
           MOVE TRANS-DATE        TO BISCAN-TRANS-DATE.
           MOVE "CL/BISCAN"       TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BISCAN-WORKERS.
           CANCEL FORM-PROGX.
           MOVE SPACES TO EXT-CALLED-FROM EXT-RETURN-ERR-BUF.
           IF BISCAN-CALL-CODE = 9
              MOVE "APILNS: BISCAN ERROR" TO RESULT-MSG
              MOVE 30                     TO RESULT-STATUS.

       EXIT-CALL-SCAN.
           EXIT.

      ******************************************************************
      *    VERIFY AGAINST SPEDT
      ******************************************************************
       VERIFY-AGAINST-SPEDT SECTION.

           MOVE "Y" TO VALID-LOAN.

           MOVE PD-EFFRATE TO VERIFY-TERMCEIL-EFF.
           MOVE PD-APRATE  TO VERIFY-TERMCEIL-APR.
           MOVE "Y"        TO VERIFY-TERMCEIL-EFF-FG.
           MOVE " "        TO VERIFY-REJECT-TERM-FG.
           MOVE TRANS-DATE TO VERIFY-TODAY-DATE.
           PERFORM GET-BW1-FOR-SPEDT.

           PERFORM VERIFY-TERMCEIL.

           IF COP-OTINS-COV-ERR
              MOVE "N" TO VALID-LOAN.

      ************************************
       MAKE-DATES-CURRENT SECTION.
      ************************************
      * SET NEW 1ST PAY DATE, WONT WIPE OUT AN EXTENSION
           MOVE PD-INTDATE TO NDTE-DATE
                              SYS-DATE.
           MOVE 1 TO NDTE-HOLD.
           MOVE PD-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO NUM-DATE.

      * WORLD OPTION TO ADVANCE TO 1ST OF NEXT MONTH IF NO CORRESPONDING DAY
      * IN NEW MONTH:
           IF NOT (SP-ADV-TO-1ST-NO-CORR = "N" OR " ")
              MOVE SP-ADV-TO-1ST-NO-CORR TO ADV-TO-1ST-NO-CORR
           ELSE
              MOVE GP-ADV-TO-1ST-NO-CORR TO ADV-TO-1ST-NO-CORR.

      * WORLD SPR'S ARE FLAGGED WITH GP-ADV-TO-1ST-NO-CORR = "Y"
      *       WHICH WILL ADVANCE TO THE NEXT MONTH IF THE DEFAULT
      *       PAYMENT DUE DATE FALLS ON A NON EXISTING DAY
      *       THIS CHANGE OVERRIDES THIS OPTION FOR LOANS BOOKED WHEN
      *       GP-I-AM-WORLD AND PD-ORGST = "TN" AND PD-CLASS = 01 AND
      *       PD-ORGTERM = 12                                   WORLD 532
           IF ADV-TO-1ST-NO-CORR = "Y"
              IF NOT ( GP-I-AM-WORLD     AND
                       PD-ORGST   = "TN" AND
                       PD-CLASS   = 01   AND
                       PD-ORGTERM = 12
                     )
                 IF PD-UNITPER-CD = "M" AND PD-UNITPER-FREQ = 1
                    IF S-DD > NDTE-DD
                       MOVE 1 TO NDTE-DD
                                 NDTE-HOLD
                       PERFORM INCREMENT-MONTHS
                       MOVE NDTE-DATE TO NUM-DATE.

      * IF DATE OF THE LOAN IS ON THE 26 THRU 31 THEN ADVANCE THE
      *   FIRST PAYMENT DUE DATE TO THE FIRST OF THE SECOND MONTH
      *   FOLLOWING THE DATE OF THE LOAN. (REGMGM PR#1099)
           IF ADV-TO-1ST-NO-CORR = "A"
              IF PD-UNITPER-CD = "M" AND PD-UNITPER-FREQ = 1
                 IF S-DD > 25
                    MOVE 1 TO NDTE-DD
                              NDTE-HOLD
                    PERFORM INCREMENT-MONTHS
                    MOVE NDTE-DATE TO NUM-DATE.

           PERFORM JUL.
           MOVE JULIAN-DATE TO JULIAN-HLDR.

      * TEST FOR ORIGINAL SAVED QUOTE HAVING AN EXTENDED FIRST PAY
           MOVE PD-1STPYDATE TO NUM-DATE.
           PERFORM JUL.
           IF JULIAN-HLDR > JULIAN-DATE
              MOVE JULIAN-HLDR TO JULIAN-DATE
              PERFORM CJUL
              MOVE NUM-DATE TO PD-1STPYDATE
                               PD-ORIG-1STPYDATE.

      * SET INSURANCE EXPIRATION DATES, IF NECESSARY

           PERFORM VARYING INS-SUB FROM 1 BY 1 UNTIL INS-SUB > 6
              IF PD-INSEXP-DATE(INS-SUB) NOT = 0 AND
                         PD-INSPREM(INS-SUB) NOT = 0
                 MOVE PD-INTDATE TO NDTE-DATE
                 MOVE PD-ORGTERM TO NDTE-HOLD
                 MOVE PD-UNITPER-CD TO DATER-UNITPER-CD
                 MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ
                 PERFORM INCREMENT-UNITPER
                 MOVE NDTE-DATE TO PD-INSEXP-DATE(INS-SUB)
              END-IF
           END-PERFORM.

       MAKE-DATES-CURRENT-EXIT.
           EXIT.

      ************************************
       CREATE-COLLATERAL-TRAILER SECTION.
      ************************************
           MOVE 6 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           MOVE SPACES TO WK-SAVE-RECORD.
           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              MOVE 0 TO WK-ELE
              PERFORM WRITE-WK-FILE.
           PERFORM CLOSE-WK-FILE.

      *********************************************
      *    TEST FOR WISCONSIN SERVICE CHARGE
      *********************************************
       TEST-FOR-WI-SCHG SECTION.
           IF SP-SCFRMLA NOT = 63
              GO TO TEST-FOR-WI-EXIT.

           MOVE 0 TO WISCHG-PRIOR-SERCHG
                     CIP-RESTRICT-SERCHG.

           IF NOT (PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
              GO TO TEST-FOR-WI-EXIT.

           MOVE PD-SSNO(1)       TO WISCHG-LN-SSNO(1).
           MOVE PD-SSNO(2)       TO WISCHG-LN-SSNO(2).
           MOVE PD-SSNO(3)       TO WISCHG-LN-SSNO(3).
           MOVE PD-SSNO(4)       TO WISCHG-LN-SSNO(4).
           MOVE PD-LOANDATE      TO WISCHG-TRANS-DATE.
           MOVE PD-PRLNNO        TO WISCHG-PRIOR-ACCTNO.
           MOVE PD-ORGST         TO WISCHG-ORGST.
           MOVE SP-SCFRMLA       TO WISCHG-SCFRMLA.

           MOVE "CL/WISCHG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH WISCHG-WORKERS.
           CANCEL FORM-PROGX.

      * COMES OUT WITH PRIOR SERCHG IF RESTRICTED AND IT MUST
      * BE SUBTRACTED FROM NEW SERCHG
           MOVE WISCHG-PRIOR-SERCHG TO CIP-RESTRICT-SERCHG.

       TEST-FOR-WI-EXIT.
           EXIT.

      *******************************************
      * TEST FOR TENNESSEE MAINTFEE RESTRICTIONS
      * CAN NOT CHARGE A MAINTFEE IF AN OPEN LOAN
      * EXIST WITH A MAINTFEE ALREADY
      *******************************************
       D18-TEST-FOR-MFEE SECTION.
           IF NOT (SP-MFFRMLA = 4)
              MOVE 0 TO CIP-MFEE-RESTRICT
              GO TO D18-MFEE-EXIT.

           MOVE PD-SSNO(1) TO MFEE04-LN-SSNO(1).
           MOVE PD-SSNO(2) TO MFEE04-LN-SSNO(2).
           MOVE PD-SSNO(3) TO MFEE04-LN-SSNO(3).
           MOVE PD-SSNO(4) TO MFEE04-LN-SSNO(4).
           MOVE TRANS-DATE TO MFEE04-TRANS-DATE.

           IF PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
              MOVE PD-PRLNNO TO MFEE04-PRIOR-ACCTNO
           ELSE
              MOVE 0 TO MFEE04-PRIOR-ACCTNO.

           MOVE SP-MFFRMLA TO MFEE04-MFFRMLA.
           MOVE "Y" TO MFEE04-CHARGE-MFEE.

           MOVE "CL/MFEE04" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH MFEE04-WORKERS.
           CANCEL FORM-PROGX.

           IF MFEE04-CHARGE-MFEE = "Y"
              MOVE 0 TO CIP-MFEE-RESTRICT
           ELSE
              MOVE 50.00 TO CIP-MFEE-RESTRICT.

       D18-MFEE-EXIT.
           EXIT.

      *********************************************
      *    TEST FOR GEORGIA SERVICE CHARGE
      *********************************************
       D17-TEST-FOR-GEORGIA-SCHG SECTION.

      * WORLD 1154
           IF SP-SCFRMLA = 58
              GO TO TEST-FOR-TEXAS-SCHG.

           IF (SP-SCFRMLA = 4 OR 16 OR 29 OR 47 OR 49 OR 52 OR 53 OR 59)
              GO TO D17-TEST-FOR-CALIF-SCHG.

           IF (SP-SCFRMLA = 23 OR 43 OR 45 OR 48 OR 50 OR 55
                                OR 56 OR 57)
              GO TO D17-TEST-FOR-NC-SCHG.

           IF NOT (SP-SCFRMLA = 12 OR 13 OR 54)
              GO TO D17-EXIT.

           MOVE " "          TO SCHG-UPDATE-FG.
           MOVE 0            TO SCHG-NEW-ACCTNO.
           MOVE PD-SSNO(1)   TO SCHG-LN-SSNO(1).
           MOVE PD-SSNO(2)   TO SCHG-LN-SSNO(2).
           MOVE PD-SSNO(3)   TO SCHG-LN-SSNO(3).
           MOVE PD-SSNO(4)   TO SCHG-LN-SSNO(4).
           MOVE TRANS-DATE   TO SCHG-TRANS-DATE.
           IF PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
              MOVE PD-PRLNNO TO SCHG-PRIOR-ACCTNO
           ELSE
              MOVE 0         TO SCHG-PRIOR-ACCTNO.
           MOVE PD-ORGST     TO SCHG-ORGST.
           MOVE PD-CLASS     TO SCHG-LN-CLASS.
           MOVE SP-SCFRMLA   TO GA-SCHG-NO.
           MOVE GA-SCHG-PATH TO FORM-NAM.
           IF GA-SCHG-NO = 13
              CALL FORM-PROGX USING FORM-PATH SCHG-WORKERS
                                             SCHG-WORKERS-2
           ELSE
              CALL FORM-PROGX USING FORM-PATH SCHG-WORKERS.
           CANCEL FORM-PROGX.
           MOVE SCHG-RESTRICT-ADJ  TO CIP-RESTRICT-ADJ
                                      PD-RESTRICT-ADJ.
           MOVE SCHG-DUAL-LOAN-ADJ TO CIP-DUAL-LOAN-ADJ
                                      PD-DUAL-LOAN-ADJ.
           MOVE SCHG-DUAL-LIAB TO SPDUAL-LIAB. 

           GO TO D17-EXIT.

      ***************************************************************
      * WORLD, TX CLASS 5 (58)
      ***************************************************************
       TEST-FOR-TEXAS-SCHG.
           IF NOT (SP-SCFRMLA = 58)
              GO TO D17-EXIT.

           MOVE SP-SCFRMLA   TO TXSCHG-SCFRMLA.
           IF PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
              MOVE PD-PRLNNO TO TXSCHG-PRIOR-ACCTNO
           ELSE
              MOVE 0         TO TXSCHG-PRIOR-ACCTNO.
           MOVE PD-LOANDATE  TO TXSCHG-LOANDATE.
           MOVE PD-SSNO(1)   TO TXSCHG-SSNO.
           MOVE PD-ORGST     TO TXSCHG-ORGST.
           MOVE PD-CLASS     TO TXSCHG-CLASS.
           MOVE " " TO TXSCHG-RESTRICTED.

           MOVE "CL/TXSCHG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH TXSCHG-WORKERS.
           CANCEL FORM-PROGX.

      *  USE CIP-RESTRICT-ADJ TO FLAG CALCZL THAT YOU CANT TAKE
      *  ANOTHER SERVICE CHARGE IF ANYTHING IN HERE

           IF TXSCHG-RESTRICTED = "Y"
      *  RESTRICTED FLAG = "Y" MEANS THAT YOU CANT TAKE ANOTHER CHARGE
              MOVE 50.00  TO CIP-RESTRICT-ADJ
           ELSE
              MOVE 0      TO CIP-RESTRICT-ADJ.

           GO TO D17-EXIT.


      * COSMO, CA 2-8-95 (4)
      * MULLEN, CA 12-11-95 (16)
      * MULLEN, CA 12-30-97 (29)
      * BAY,       11-09-11 (47) ADDED 12 MONTH CHECK
      * REGMGM, TX 12-30-97 (49)
      * MULLEN, CA 06-02-11 (52,53)
      * WORLD,  KY 05-29-19 (59)
       D17-TEST-FOR-CALIF-SCHG.
           IF NOT (SP-SCFRMLA = 4 OR 16 OR 29 OR 47 OR 49 OR 52 OR 53 OR
                                                             59)
              GO TO D17-EXIT.

           MOVE SP-SCFRMLA   TO CASCHG-SCFRMLA.
           IF PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
              MOVE PD-PRLNNO TO CASCHG-PRIOR-ACCTNO
           ELSE
              MOVE 0         TO CASCHG-PRIOR-ACCTNO.
           MOVE PD-LOANDATE  TO CASCHG-LOANDATE.
           MOVE " "          TO CASCHG-RESTRICTED.

           MOVE "CL/CASC16" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CASCHG-WORKERS.
           CANCEL FORM-PROGX.

      * USE CIP-RESTRICT-ADJ TO FLAG CALCZL THAT YOU CANT TAKE
      * ANOTHER SERVICE CHARGE IF ANYTHING IN HERE
           IF CASCHG-RESTRICTED = "Y"
      * RESTRICTED FLAG = "Y" MEANS THAT YOU CANT TAKE ANOTHER CHARGE
              MOVE 50.00 TO CIP-RESTRICT-ADJ
           ELSE
              MOVE 0 TO CIP-RESTRICT-ADJ.

           GO TO D17-EXIT.

       D17-TEST-FOR-NC-SCHG.
           IF NOT (SP-SCFRMLA = 23 OR 43 OR 45 OR 48 OR 50
                                    OR 55 OR 56 OR 57)
              GO TO D17-EXIT.

           MOVE 0 TO NCSCHG-LN-ACCTNO.
           IF SP-SCFRMLA = 48 
              IF PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
                 MOVE PD-PRLNNO TO NCSCHG-LN-ACCTNO.
              
           IF SP-SCFRMLA = 50 
              IF PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
                 MOVE PD-PRLNNO TO NCSCHG-LN-ACCTNO
              ELSE
      * IF SCFRMLA 50 AND NOT A RENEWAL, THEN NO RESTRICTION TEST
      *   IS EVEN NECESSARY, SO SKIP IT, SCHG WILL BE $100
                 MOVE 100.00 TO CIP-RESTRICT-ADJ
                 GO TO D17-EXIT.

           MOVE PD-SSNO(1) TO NCSCHG-LN-SSNO.
           MOVE PD-SSNO(2) TO NCSCHG-LN-SSNO-2.
           MOVE PD-SSNO(3) TO NCSCHG-LN-SSNO-3.
           MOVE PD-SSNO(4) TO NCSCHG-LN-SSNO-4.
           MOVE PD-ORGST   TO NCSCHG-ORGST.
           MOVE PD-CLASS   TO NCSCHG-CLASS.
           MOVE TRANS-DATE TO NCSCHG-TRANS-DATE.
           MOVE " "        TO NCSCHG-RESTRICTED.
           MOVE 0          TO NCSCHG-NO-SERCHG.
           MOVE SP-SCFRMLA TO NCSCHG-SCFRMLA.

           MOVE "CL/NCSC23" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH NCSCHG-WORKERS.
           CANCEL FORM-PROGX.


      * USE CIP-RESTRICT-ADJ TO FLAG CALCZL THAT YOU CANT TAKE
      * ANOTHER SERVICE CHARGE IF ANYTHING IN THERE
           IF NCSCHG-RESTRICTED = "Y"
      * RESTRICTED FLAG = "Y" MEANS THAT YOU CANT TAKE ANOTHER CHARGE
              MOVE 50.00  TO CIP-RESTRICT-ADJ
           ELSE
              MOVE 0 TO CIP-RESTRICT-ADJ.

      * NCSCHG-RESTRICTED "1" = CHARGE $100
      * NCSCHG-RESTRICTED "2" = CHARGE $50
      * NCSCHG-RESTRICTED "3" = CHARGE $25
           IF SP-SCFRMLA = 50 
              MOVE 100.00 TO CIP-RESTRICT-ADJ
              IF NCSCHG-RESTRICTED = "3"
                 MOVE 25.00 TO CIP-RESTRICT-ADJ
              ELSE
              IF NCSCHG-RESTRICTED = "2"
                 MOVE 50.00 TO CIP-RESTRICT-ADJ.

       D17-EXIT.
           EXIT.


      *********************************************
      *    TEST FOR SOUTH CAROLINA SERVICE CHARGE
      *********************************************
       D18-TEST-FOR-SC-SCHG SECTION.
           IF NOT (SP-SCFRMLA = 18 OR 42)
              GO TO D18-EXIT.

           MOVE " " TO SCSCHG-UPDATE-FG.
           MOVE 0 TO SCSCHG-NEW-ACCTNO.
           MOVE PD-SSNO(1) TO SCSCHG-LN-SSNO(1).
           MOVE PD-SSNO(2) TO SCSCHG-LN-SSNO(2).
           MOVE PD-SSNO(3) TO SCSCHG-LN-SSNO(3).
           MOVE PD-SSNO(4) TO SCSCHG-LN-SSNO(4).
           MOVE TRANS-DATE TO SCSCHG-TRANS-DATE.
           IF PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
              MOVE PD-PRLNNO TO SCSCHG-PRIOR-ACCTNO
           ELSE
              MOVE 0 TO SCSCHG-PRIOR-ACCTNO.
           MOVE PD-ORGST TO SCSCHG-ORGST.
           MOVE SP-SCFRMLA TO SC-SCHG-NO SCSCHG-SCFRMLA.
           IF SP-SCFRMLA = 42
              MOVE 18 TO SC-SCHG-NO.
           MOVE SC-SCHG-PATH TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH SCSCHG-WORKERS.
           CANCEL FORM-PROGX.
           MOVE SCSCHG-PRIOR-CASHADV TO CIP-PRIOR-CASHADV.
           MOVE SCSCHG-RESTRICT-SERCHG TO CIP-RESTRICT-SERCHG.
           MOVE SCSCHG-RESTRICT-HIGH-CASHADV
                           TO CIP-RESTRICT-HIGH-CASHADV.
           MOVE 0 TO SPDUAL-LIAB. 

       D18-EXIT.
           EXIT.

      ******************************************************************
      *    ESTABLISH REJECT TERMS
      ******************************************************************
       ESTABLISH-REJECT-TERMS SECTION.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              MOVE 0 TO CIP-INS-REJECT-TERM(SUB)
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
               IF SP-INSREJECT-FG(SUB) = "C"
                  MOVE 98 TO SUB
               END-IF
           END-PERFORM.

           IF SUB = 99
              MOVE PD-1STPYDATE TO NDTE-DATE
              COMPUTE NDTE-HOLD =  PD-ORGTERM - 1
              MOVE PD-UNITPER-CD TO DATER-UNITPER-CD
              MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ
              PERFORM INCREMENT-UNITPER
              MOVE NDTE-DATE  TO VERIFY-TODAY-DATE
              MOVE "Y"        TO VERIFY-TERMCEIL-EFF-FG
              MOVE "Y"        TO VERIFY-REJECT-TERM-FG
              PERFORM GET-BW1-FOR-SPEDT.

      ******************************************************************
      *    GET BORROWER FOR SPEDT EDITS
      ******************************************************************
       GET-BW1-FOR-SPEDT SECTION.

           MOVE 4 TO XAGE-MAX.

           IF PD-SSNO(1) = 0
              MOVE 0 TO BW-BDATE BW-REFDATE BW-CREDIT-SCORE
              MOVE BW-BDATE   TO VERIFY-AGE
              MOVE BW-REFDATE TO VERIFY-REFDATE
              MOVE 1          TO VERIFY-MAKER-SUB
              PERFORM VERIFY-TERMCEIL
              GO TO DONE-WITH-BW1-SPEDT.

      * IF MAKER INSURANCE, TEST PRIMARY MAKER'S AGE EDITS...
      
           IF PD-INSURED = "M"
              IF PD-SSNO(1) NOT = BW-SSNO
                 MOVE PD-SSNO(1) TO BW-SSNO
                 PERFORM GET-BW1
                 IF IO-FG = 9
                    MOVE 0 TO BW-BDATE BW-REFDATE
                 END-IF
              END-IF
              MOVE BW-BDATE   TO VERIFY-AGE
              MOVE BW-REFDATE TO VERIFY-REFDATE
              MOVE 1          TO VERIFY-MAKER-SUB
              PERFORM VERIFY-TERMCEIL
              GO TO DONE-WITH-BW1-SPEDT.

      * IF SECONDARY (SPOUSE) INSURANCE, TEST SECOND MAKER'S AGE EDITS...
      
           IF PD-INSURED = "S"
              IF PD-SSNO(2) NOT = BW-SSNO
                 MOVE PD-SSNO(2) TO BW-SSNO
                 PERFORM GET-BW1
                 IF IO-FG = 9
                    MOVE 0 TO BW-BDATE BW-REFDATE
                 END-IF
              END-IF
              MOVE BW-BDATE   TO VERIFY-AGE
              MOVE BW-REFDATE TO VERIFY-REFDATE
      * THIS IS USED TO SIGNAL TESTING A JOINT MAKER
              MOVE 1          TO VERIFY-MAKER-SUB
              PERFORM VERIFY-TERMCEIL
              GO TO DONE-WITH-BW1-SPEDT.

      * IF JOINT INSURANCE, AND GP FLAG SET, TEST ENDORSERS ALONG 
      *        WITH THE JOINT MAKERS
      
           IF PD-INSURED = "J" AND
                              (GP-VALIDATE-ENDORSER-AGE = "Y" OR " ")
              PERFORM VARYING XAGE-SUB FROM 1 BY 1 UNTIL
                                               XAGE-SUB > XAGE-MAX 
                 IF PD-MAKERCD(XAGE-SUB) = "M" OR "J" OR "E"
                    IF PD-SSNO(XAGE-SUB) NOT = BW-SSNO
                       MOVE PD-SSNO(XAGE-SUB) TO BW-SSNO
                       PERFORM GET-BW1
                       IF IO-FG = 9
                          MOVE 0 TO BW-BDATE BW-REFDATE
                       END-IF
                    END-IF
                    MOVE BW-BDATE   TO VERIFY-AGE
                    MOVE BW-REFDATE TO VERIFY-REFDATE
                    MOVE XAGE-SUB   TO VERIFY-MAKER-SUB
                    PERFORM VERIFY-TERMCEIL
                    IF VERIFY-REJECT-TERM-FG = "Y"
                       PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
                          IF VERIFY-REJECT-TERM(SUB) NOT = 0
                             IF (CIP-INS-REJECT-TERM(SUB) = 0)
                               OR
                                (VERIFY-REJECT-TERM(SUB) <
                                      CIP-INS-REJECT-TERM(SUB)
                                )
                                MOVE VERIFY-REJECT-TERM(SUB)
                                         TO CIP-INS-REJECT-TERM(SUB)
                             END-IF
                          END-IF
                       END-PERFORM
                    END-IF
                 END-IF
              END-PERFORM.

      * IF JOINT INSURANCE, AND GP FLAG NOT SET, JUST TEST JOINT
      
           IF PD-INSURED = "J" AND GP-VALIDATE-ENDORSER-AGE = "N"
              PERFORM VARYING XAGE-SUB FROM 1 BY 1 UNTIL
                                               XAGE-SUB > XAGE-MAX 
                 IF PD-MAKERCD(XAGE-SUB) = "M" OR "J"
                    IF PD-SSNO(XAGE-SUB) NOT = BW-SSNO
                       MOVE PD-SSNO(XAGE-SUB) TO BW-SSNO
                       PERFORM GET-BW1
                       IF IO-FG = 9
                          MOVE 0 TO BW-BDATE BW-REFDATE
                       END-IF
                    END-IF
                    MOVE BW-BDATE   TO VERIFY-AGE
                    MOVE BW-REFDATE TO VERIFY-REFDATE
                    MOVE XAGE-SUB   TO VERIFY-MAKER-SUB
                    PERFORM VERIFY-TERMCEIL
                    IF VERIFY-REJECT-TERM-FG = "Y"
                       PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
                          IF VERIFY-REJECT-TERM(SUB) NOT = 0
                             IF (CIP-INS-REJECT-TERM(SUB) = 0)
                               OR
                                (VERIFY-REJECT-TERM(SUB) <
                                      CIP-INS-REJECT-TERM(SUB)
                                )
                                MOVE VERIFY-REJECT-TERM(SUB)
                                         TO CIP-INS-REJECT-TERM(SUB)
                             END-IF
                          END-IF
                       END-PERFORM
                    END-IF
                 END-IF
              END-PERFORM.

       DONE-WITH-BW1-SPEDT.
           IF VERIFY-REJECT-TERM-FG = "Y"
              IF PD-INSURED NOT = "J"
                 PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
                    MOVE VERIFY-REJECT-TERM(SUB)
                            TO CIP-INS-REJECT-TERM(SUB)
                 END-PERFORM.


      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE INPUT-BRNO TO BRANCH.

           MOVE " " TO ERRCD.
            
      * CLEARED FOR 'SC' TEST ON RENEWALS FOR LESS THAN 10%:
           MOVE " " TO SCHG18-UPDATE-FG.

      * MUST CLEAR/RESET THESE FIELDS WHICH ARE PASSED TO THE CALCZL
      * TO DETERMINE KY SALES TAX. PROBLEM WITH CAN LOAN'S QUOTES
      * WHICH INITIALLY DON'T HAVE BORROWERS.
           MOVE " " TO BW-1-INFO.

      * CLEAR REQUIRED RE: (GA) SP-SCFRMLA = 12 OR 13 OR 54
           MOVE 0 TO SPDUAL-LIAB.

      *  CREATE WK-FILE, IF RUNNING FROM EOCRON, WORK FILE IN /USR/TMP
      *  WAS NOT CREATED IN LONPA0
           PERFORM CREATE-WK-FILE.

           MOVE USER-PID TO TEMP-PID.

           PERFORM UNAME-CALL.

           ACCEPT ORIG-FIL      FROM ENVIRONMENT "FIL".
           ACCEPT ORIG-POSTNAME FROM ENVIRONMENT "POSTNAME".
           ACCEPT TRANS-DATE FROM CENTURY-DATE.

           PERFORM OPEN-BR-FILE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE. 
           PERFORM OPEN-LC1-FILE. 
           PERFORM OPEN-CL1-FILE. 
           PERFORM OPEN-CDB1-FILE. 

       EXIT-INITIALIZE.
           EXIT.

 
      *****************************************************************
       SU-MODULE SECTION.
      *****************************************************************

      *  CHANGE 'POSTNAME' TO NEW BRANCH
      *  CHANGE 'FIL' TO NEW BRANCH
      *  CHANGE CURRENT WORKING DIRECTORY TO /{NEW FIL}/GB

      * SETUP NEW DATA PATH
      
           MOVE " " TO ERRCD.
           MOVE ORIG-POSTNAME TO WORK-USERID.
           MOVE TO-BRANCH     TO WORK-BR.
           MOVE ORIG-FIL      TO HOLD-FIL.
           MOVE TO-DATA-PATH  TO HOLD-DATA-PATH.

      * CHANGE USERS DIRECTORY TO NEW DATA SET
      
           STRING "/" HOLD-FIL "/GB"
                  DELIMITED BY " " INTO NEW-DIRECTORY.
           CALL "C$CHDIR" USING NEW-DIRECTORY ERR-NUM.
           IF ERR-NUM NOT = 0
              MOVE "E" TO ERRCD
              GO TO EXIT-SU-MODULE.

      * MUST SET UP NEW ENVIRONMENT FOR NEW POSTING BRANCH!
      
           SET ENVIRONMENT "POSTNAME" TO WORK-USERID.
           SET ENVIRONMENT "FIL"      TO HOLD-FIL.

           MOVE "GB/SETENV" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           MOVE TO-BRANCH TO BRANCH.

       EXIT-SU-MODULE.
            EXIT.

      ******************************************************************
       CREATE-WK-FILE SECTION.
      ******************************************************************

      *  CREATE WK-FILE, IF RUNNING FROM EOCRON, WORK FILE IN /USR/TMP
      *  WAS NOT CREATED IN LONPA0

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.

      * WK-FILE FOR LOAN PROCESSING USED PROCESS ID XL(PROCESS ID)
      
           MOVE PROCESS-ID-BUF TO USER-PID TEMP-PID.
           MOVE 9 TO IO-FG.
           PERFORM OPEN-IT.
           MOVE WK-IFN TO E-FILE.
       OPEN-WK-OUTPUT.
           OPEN OUTPUT WK-FILE.
           IF ( IO-FG = 8 )
              GO TO OPEN-WK-OUTPUT.
           IF ( IO-FG = 7 )
              PERFORM CLOSE-WK-FILE
              GO TO OPEN-WK-OUTPUT.
           PERFORM CLOSE-WK-FILE.
       EXIT-CREATE-WK-FILE.
           EXIT.

      **************************************
       FIND-EFFECTIVE-SPR SECTION.
      **************************************
           MOVE PD-ORGST    TO SP-ORGST.
           MOVE PD-SPRCLASS TO SP-SPRCLASS.
           MOVE PD-SUBCLASS TO SP-SUBCLASS.
           MOVE PD-LAWCODE  TO SP-LAWCODE.
           PERFORM START-SP1-FILE.
       FIND-SPR-NEXT.
           PERFORM READ-SP1-FILE-NEXT.
           IF NOT (( SP-ORGST    = PD-ORGST    ) AND
                   ( SP-SPRCLASS = PD-SPRCLASS ) AND
                   ( SP-SUBCLASS = PD-SUBCLASS ))
              MOVE 9 TO IO-FG.
           IF IO-FG NOT = 0
              GO TO EXIT-FIND-EFFECTIVE-SPR.

           IF SP-EXPDATE NOT = 0
              MOVE TRANS-DATE TO NUM-DATE
              MOVE SP-EXPDATE TO SYS-DATE
              PERFORM CMPDAT
              IF SYS-IS-LE
                 GO TO FIND-SPR-NEXT.

      * WILL END UP HERE WITH A VALID EFFECTIVE SPR, OTHERWISE YOU EXITED
      * WITH AN IO-FG OF 9 AND WILL LOG AN ERROR AND EXIT PROGRAM

      * IT MAY OR MAY NOT BE THE SAME AS THE PRIOR LOAN

           MOVE SP-LAWCODE TO PD-LAWCODE.

       EXIT-FIND-EFFECTIVE-SPR.
           EXIT.

      ******************************************
       REMOVE-STAGEFILE SECTION.
      ******************************************
           MOVE INPUT-BRNO         TO STAGE-BRNO.
           MOVE INPUT-SSNO         TO STAGE-SSNO.
           MOVE INPUT-CORRELATION  TO STAGE-CORRELATION.
           MOVE INPUT-CAN-LOAN     TO STAGE-CAN-LOAN.
           PERFORM OPEN-STAGE-FILE.
           PERFORM READ-STAGE-FILE.
           PERFORM DELETE-STAGE-FILE.
           IF IO-FG NOT = 0 
              MOVE 37                           TO RESULT-STATUS
              MOVE "APILNS: MISSING STAGEFILE RECORD" TO RESULT-MSG
           ELSE
              MOVE 0                           TO RESULT-STATUS
              MOVE "STAGE FILE REMOVED SUCCESSFUL; &&&&&&&&"
                                          TO RESULT-MSG
              INSPECT RESULT-MSG REPLACING FIRST "&&&&&&&&"
                                           BY PD-NUMBER.



       COPY "LIBLP/LPLTDCL.CPY".
       COPY "LIBLP/LPPDCL.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPFRFD.CPY".
       COPY "LIBLP/INSELIG.CPY".
       COPY "LIBLP/LPANTI.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPDAMTS.CPY".
       COPY "LIBLP/LPSPEDP.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBLP/LPZLCL.CPY".

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/UNAME.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETPR.CPY".
       VDU-CONVERT-FIELD.
       SETUP-LINK-INFO.
       APPEND-BW-NAMES.
           MOVE BW-LNAME TO INSPECT-W.
           MOVE BW-FNAME TO ADD-ON.
           INSPECT INSPECT-W REPLACING FIRST "  " BY " +".
           INSPECT INSPECT-W REPLACING FIRST
              "+                        " BY ADD-ON.
       GET-CODE.
           MOVE "GC"     TO CDB-TYPE.
           MOVE PD-OWNBR TO CDB-BRANCH.
           PERFORM READ-CDB1-FILE.
           IF IO-FG = 9
              MOVE " * NOT ON FILE * " TO CDB-DESC.
       TEST-IL-COLLCD.
      * OF THE 4 AUTO COLLATERAL CODES THAT WORLD USES, ONLY "AU" AND "WA"
      * ARE ALLOWED IN THE STATE OF "IL". EXCEPT CLASS 8 WHICH CAN HAVE ANY.
      * CLASS 5, SUBCLASS 10 MUST HAVE "AU", "WA" ONLY!
           MOVE PD-COLLCD TO TEST-COLLCD.
           IF PD-ORGST = "IL"
              IF (PD-CLASS = 5 AND PD-SUBCLASS = 10)
                  IF PD-COLLCD NOT = "AU" AND NOT = "WA"
                     MOVE "APILNS: MUST HAVE TITLE SECURED COLLATERAL"
                             TO RESULT-MSG
                     MOVE 31 TO RESULT-STATUS
                     MOVE 9 TO IO-FG.
           IF PD-ORGST = "IL"
              IF NOT (PD-CLASS = 5 AND PD-SUBCLASS = 10)
                 IF PD-CLASS NOT = 8
                    IF AUTO-COLLCD
                     MOVE "APILNS: TITLE SECURED COLLATERAL NOT ALLOWED"
                                     TO RESULT-MSG
                     MOVE 32 TO RESULT-STATUS
                     MOVE 9 TO IO-FG.
       SAVE-SCREEN.
      * SAVE PD-REC TO WORK FILE - USED IN LOAN UPDATE
           MOVE 2 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           MOVE PD-RECORD TO WK-SAVE-RECORD.
           IF IO-BAD
              PERFORM WRITE-WK-FILE
           ELSE
              PERFORM REWRITE-WK-FILE.
           PERFORM CLOSE-WK-FILE.
       GET-SP.
           MOVE PD-ORGST TO SP-ORGST.
           MOVE PD-SPRCLASS TO SP-SPRCLASS.
           MOVE PD-SUBCLASS TO SP-SUBCLASS.
           MOVE PD-LAWCODE TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
       GET-BW1.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-GOOD
              IF BW-SSNO = PD-SSNO(1)
                 MOVE BW-STATE       TO BW-1-STATE
                 MOVE BW-CITY-TAX-CD TO BW-1-CITY-TAX-CD
                 MOVE BW-COUNTY      TO BW-1-COUNTY.

       COMPUTE-GL-FEEAMT.
           MOVE 0 TO TOTAL-GL-HLDR
                     TOTAL-GL-HLDR-SALESTAX
                     TOTAL-GL-FINCHG-HLDR
                     TOTAL-GL-FINCHG-HLDR-SALESTAX.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              ADD PD-FEEAMT(SUB) TO TOTAL-GL-HLDR
              IF PD-FEEFINCHG(SUB) = "Y"
                 ADD PD-FEEAMT(SUB) TO TOTAL-GL-FINCHG-HLDR
              END-IF
      * ACCUMULATE INS. SALES TAX
              IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                 IF PD-FEECODE(SUB) = "TC" OR "TA" OR "TP" OR "T1"
                                   OR "T2" OR "T3" OR "T4" OR "T5"
                    ADD PD-FEEAMT(SUB) TO TOTAL-GL-HLDR-SALESTAX
                 END-IF
              END-IF
           END-PERFORM.

       SETUP-CALC-AREA.

           MOVE 999.99 TO PD-SERVPER.
           MOVE 999.999 TO PD-ENDORATE
                           PD-OVRDRATE.
           MOVE "Y" TO DEFAULT-APR-FG.

           MOVE "C" TO PD-OVRDRATE-FG PD-INS-FG(1) PD-INS-FG(2)
                       PD-INS-FG(3) PD-INS-FG(4) PD-SERCHG-FG
                       PD-MAINTFEE-FG PD-EXTCHG-FG PD-REGPYAMT-FG.

           MOVE " " TO QUOTE-ID-FG.

           PERFORM CLEAR-CALC-AREA-ZL.

           MOVE "C" TO CIP-OVR-RATE-SW
                       CIP-INS-SW(1) CIP-INS-SW(2)
                       CIP-INS-SW(3) CIP-INS-SW(4)
                       CIP-SERVICE-CHG-SW CIP-MAINT-FEES-SW
                       CIP-1ST-PY-EXT-SW CIP-REG-PAYMENT-SW.
           MOVE "I" TO CIP-INS-SW(5) CIP-INS-SW(6).

           MOVE PD-LOANTYPE TO CIP-LOANTYPE.
           MOVE PD-SMALLOAN TO CIP-SMALLOAN.
           MOVE PD-SALEFIN TO CIP-SALEFIN.
           MOVE PD-SERVACCT TO CIP-SERVACCT.

           MOVE PD-LOANDATE TO CIP-LN-DT.
           MOVE PD-INTDATE TO CIP-IN-DT.
           MOVE PD-FILEDATE TO CIP-FL-DT.
           MOVE PD-1STPYDATE TO CIP-PMT-DT.
           MOVE PD-ORGTERM TO CIP-TERM.

           MOVE PD-PURCD TO CIP-PURPOSE-CD.
           MOVE PD-COLLCD TO CIP-COLLTRL-CD.
           MOVE PD-SRCD TO CIP-SOURCE-CD.
           MOVE PD-PROCEEDS TO CIP-PROCEEDS.
           MOVE PD-PROCEEDS-FG TO CIP-PROCEEDS-SW.

           MOVE PD-INSURED TO CIP-INS-WHO.
           MOVE PD-INSTYPES TO CIP-INS-TYPE.
           MOVE PD-INSELIGIBLE TO CIP-INS-ELIG.
           MOVE PD-POINTS TO CIP-POINTS.
           MOVE PD-POINTS-FG TO CIP-POINTS-SW.
           MOVE TOTAL-GL-HLDR TO CIP-GL-AMOUNT-TOTAL.
           IF BR-FORCE-VALID-CITY-COUNTY = "Y"
              SUBTRACT TOTAL-GL-HLDR-SALESTAX
                            FROM CIP-GL-AMOUNT-TOTAL.
           MOVE TOTAL-GL-FINCHG-HLDR TO CIP-GL-AMOUNT-TOTAL-FINCHG.

      * ------------------------
           MOVE PD-OVRDRATE    TO CIP-OVR-RATE.
           MOVE PD-OVRDRATE-FG TO CIP-OVR-RATE-SW.
           MOVE PD-EFFRATE     TO CIP-EFF-RATE
                                  CEP-EFF-RATE.
           MOVE PD-SMPRATE     TO CIP-SIM-RATE.
           MOVE PD-APRATE      TO CIP-APR-RATE
                                  CEP-APR-RATE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE PD-INSCOMP(SUB)     TO CIP-INS-CO(SUB)
              MOVE PD-INSPREM(SUB)     TO CIP-INS-PREMIUM(SUB)
                                          CEP-INS-PREMIUM(SUB)
              MOVE PD-INSTRM(SUB)      TO CIP-INS-TERM(SUB)
                                          CEP-INS-TERM(SUB)
              MOVE PD-INSCOVR(SUB)     TO CIP-INS-COVERAGE(SUB)
                                          CEP-INS-COVERAGE(SUB)
              MOVE PD-INSOURS(SUB)     TO CIP-INS-USE-SW(SUB)
              MOVE PD-INS-FG(SUB)      TO CIP-INS-SW(SUB)
              MOVE PD-INS-TERM-SW(SUB) TO CIP-INS-TERM-SW(SUB)
              MOVE PD-COVERAGE-SW(SUB) TO CIP-COVERAGE-SW(SUB)
              MOVE PD-PREMIUM-SW(SUB)  TO CIP-PREMIUM-SW(SUB)
           END-PERFORM.

           MOVE PD-SERCHG       TO CIP-SERVICE-CHG
                                   CEP-SERVICE-CHG.
           MOVE PD-STATE-WORKER TO CIP-STATE-WORKER.
           MOVE PD-SERCHG-FG    TO CIP-SERVICE-CHG-SW.
           MULTIPLY PD-MAINTFEE BY PD-ORGTERM GIVING 
                               CIP-MAINT-FEES ROUNDED
                               CEP-MAINT-FEES ROUNDED.
           MOVE PD-MAINTFEE-FG  TO CIP-MAINT-FEES-SW.
           MOVE PD-EXTCHG       TO CIP-1ST-PY-EXT
                                   CEP-1ST-PY-EXT.
           MOVE PD-EXTCHG-FG    TO CIP-1ST-PY-EXT-SW.
           MOVE PD-INTCHG       TO CIP-INT-CHARGE.
           MOVE PD-APR-FINAMT   TO CIP-APR-FINANCD.
           MOVE PD-FINAMT       TO CIP-AMT-FINANCD.
           MOVE PD-LNAMT        TO CIP-LOAN-AMOUNT.
           ADD PD-ANTICERN(1) PD-ANTICERN(2) PD-ANTICERN(3)
               PD-ANTICERN(4) PD-ANTICERN(5) GIVING CIP-ANTIC-EARNS.
           IF NOT GP-INCL-OTH-IN-EARNED-N
              ADD PD-ANTICERN(6) TO CIP-ANTIC-EARNS.
           MOVE PD-1STPYAMT      TO CIP-ODD-1ST-PMT.
           MOVE PD-REGPYAMT      TO CIP-REG-PAYMENT.
           MOVE PD-REGPYAMT-FG   TO CIP-REG-PAYMENT-SW.
           IF PD-LOANTYPE = "I"
              MOVE PD-SERCHG TO PD-ANTICERN(3)
              MOVE 0 TO PD-ANTICERN(1)
                        PD-ANTICERN(2)
                        PD-ANTICERN(5)
                        PD-ANTICERN(6)
              IF GP-ALLOW-MF-ON-IB = "Y"
                 COMPUTE PD-ANTICERN(4) = PD-ORGTERM * PD-MAINTFEE
              ELSE
                 MOVE 0 TO PD-ANTICERN(4)
              END-IF
           ELSE
              ADD PD-INTCHG PD-EXTCHG GIVING PD-ANTICERN(1)
              MOVE PD-SERCHG TO PD-ANTICERN(3)
              COMPUTE PD-ANTICERN(4) = PD-ORGTERM * PD-MAINTFEE
              MOVE 0 TO PD-ANTICERN(5).

           IF NOT (SP-LNCOST-FRMLA = "M")
              MOVE ZEROS TO PD-ANTICERN(6).

           MOVE PD-ANTICERN(1) TO WK-ANTICERN(1).
           MOVE PD-ANTICERN(2) TO WK-ANTICERN(2).
           MOVE PD-ANTICERN(3) TO WK-ANTICERN(3).
           MOVE PD-ANTICERN(4) TO WK-ANTICERN(4).
           MOVE PD-ANTICERN(5) TO WK-ANTICERN(5).
           MOVE PD-ANTICERN(6) TO WK-ANTICERN(6).
           PERFORM ADJUST-ANTICERN.
           MOVE WK-ANTICERN(1) TO PD-ANTICERN(1).
           MOVE WK-ANTICERN(2) TO PD-ANTICERN(2).
           MOVE WK-ANTICERN(3) TO PD-ANTICERN(3).
           MOVE WK-ANTICERN(4) TO PD-ANTICERN(4).
           MOVE WK-ANTICERN(5) TO PD-ANTICERN(5).
           MOVE WK-ANTICERN(6) TO PD-ANTICERN(6).


      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBFP/FPIOPST.CPY".
       OPEN-ASCII-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE ASCII-PATH TO E-FILE.
           OPEN OUTPUT ASCII-FILE.
           IF IO-FG = 8
             GO TO OPEN-ASCII-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE ASCII-FILE
             GO TO OPEN-ASCII-FILE-OUTPUT.
           UNLOCK ASCII-FILE.
       WRITE-ASCII-FILE.
           PERFORM WRITE-IT.
           MOVE ASCII-PATH TO E-FILE.
           MOVE ASCII-KEY TO E-KEYX.
           WRITE ASCII-REC.
           IF IO-FG = 8
             GO TO WRITE-ASCII-FILE.
           UNLOCK ASCII-FILE.
       CLOSE-ASCII-FILE.
           PERFORM CLOSE-IT.
           CLOSE ASCII-FILE.

       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCLGS_SQL.CPY".
       COPY "LIBLP/LPLTDGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGBIN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPCL1RN.CPY".
       COPY "LIBLP/LPLTD1IN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBGB/GBRC2RN.CPY".
       COPY "LIBLP/LPOPIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-PROGRAM.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           MOVE RESULT-STATUS TO RETURN-CODE.

           EXIT PROGRAM RETURNING RETURN-CODE.


