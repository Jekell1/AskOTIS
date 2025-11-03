      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CDREAD.
       DATE-WRITTEN.    111502.
      **************************************************************************
      *                 (CL)
      *  DESCRIPTION:   READ CODE FILE
      *
      *    CAN BE CALLED FROM ANY PROGRAM
      *    CURRENTLY CALL FROM LPLCAP
      *
      *    NOTE:
      *          AN INVALID READ CAUSES CD-REC TO BE RETURNED WITH ALL SPACES
      *
      * REV:
      *  JTG 021115 ** NEW **
      *             ADDED NEW CODE FILE TYPE 'BR' -BATCH PAYMENT REFERENCE CODES
      *             BATCH PAYMENT REVISIONS                         REGACC #0006
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./CL/CDREAD.CBL S35 09/20/22-14:31:30 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  ERRCD               PIC X     VALUE SPACES.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       01  CDX-REC          PIC X(CD-SIZE).

       PROCEDURE DIVISION
             USING FORM-PATHNAME
                   CDX-REC 
                   EXIT-PATHNAME. 

      /***********************************************************
      *    C O N T R O L   M O D U L E
      ************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP IO-FG.

           PERFORM OPEN-CD1-FILE.

           MOVE CDX-REC TO CD-REC.

           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-BAD
              MOVE SPACES TO CD-REC
           ELSE
              MOVE CD-REC TO CDX-REC.

           GO TO END-ROUTINE.

       COPY "LIBGB/DATER.CPY".

      /********************************
      *      F I L E   I / O          *
      *********************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
       END-ROUTINE SECTION.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
