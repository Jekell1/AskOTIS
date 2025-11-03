       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BICLAS.
       DATE-WRITTEN.    12/05/2019.
      **************************************************************************
      *   DIRECTORY  : CL                              
      *
      *   DESCRIPTION: CALLED PROGRAM TO SCAN FOR LOANS ON ALL BORROWERS
      *                BIFILE MASTER INDEX RECORDS
      * 
      *                STOP THE LOAN IF THERE IS ANOTHER ACCOUNT WITH THE SAME
      *                CLASS OPEN ON THE SAME MACHINE (CLASS 50). 
      *                IF IT FINDS A BI WITH SAME SSNO, GO READ LOAN AND
      *                COMPARE THE CLASS
      *
      *                IF SP-DUAL-STATE = "C" OR SP-DUAL-BRANCH = "C"
      *                  THIS PROGRAM WILL BE CALLED
      *
      *
      * REV:
      *  BAH 20191205 NEW, #1407
      *  BAH 20220830 HARD CODED START
      **************************************************************************
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
           "@(#)CL/BICLAS.CBL S34 05/21/21-13:10:38 >".
       COPY "LIBLP/LP01BI.CPY".
       COPY "LIBLP/LP01BI_SQL.CPY".
       COPY "LIBLP/LPWSBI.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  ERRCD               PIC X         VALUE SPACES.

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
 
      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/BICLASW.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME BICLAS-WORKERS EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

      * CLEAR THE RETURN FLAG
           MOVE " " TO BICLAS-RESTRICTED.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-BI1-FILE.

           MOVE BICLAS-SSNO TO BI-SSNO
                               QBI1-WBEG-SSNO
                               QBI1-WEND-SSNO.
           MOVE 0           TO BI-ACCTNO BI-OWNBR
                               QBI1-WBEG-ACCTNO
                               QBI1-WBEG-OWNBR.
           MOVE ALL "9"     TO QBI1-WEND-ACCTNO
                               QBI1-WEND-OWNBR.

           PERFORM START-BI1-FILE.
       NEXT-REC.
           PERFORM READ-BI1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (BI-SSNO NOT = BICLAS-SSNO)
              GO TO END-ROUTINE.

      * ONLY LOOKING FOR OTHER OPEN TAX ACCOUNT LOANS IN SAME CLASS
           IF BI-POFFDATE NOT = 0
              GO TO NEXT-REC.

           MOVE BI-OWNBR TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO NEXT-REC.

           MOVE BR-MACHINE   TO LN-ALL-MACHINE.
           MOVE BR-DATA-PATH TO LN-ALL-DATA.
           MOVE LN-ALL-PATH  TO LN-PATH.

           PERFORM OPEN-LN1-FILE.
           MOVE BI-OWNBR  TO LN-OWNBR.
           MOVE BI-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              PERFORM CLOSE-LN1-FILE
              GO TO NEXT-REC.

      * MAKE SURE SSNO STILL MATCHES
           IF LN-SSNO(1) NOT = BI-SSNO
              PERFORM CLOSE-LN1-FILE
              GO TO NEXT-REC.

      * CHECK IF SAME CLASS

           IF LN-CLASS = BICLAS-CLASS
              MOVE "Y"      TO BICLAS-RESTRICTED
      * PASS BACK BRANCH THAT HAS EXISTING ACCOUNT
              MOVE LN-OWNBR TO BICLAS-BRANCH
              GO TO END-ROUTINE
           ELSE
              PERFORM CLOSE-LN1-FILE
              GO TO NEXT-REC.

       COPY "LIBGB/DATER.CPY".

      ********************************************
      *     I N P U T  /  O U T P U T            *
      ********************************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BI1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPBI1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
