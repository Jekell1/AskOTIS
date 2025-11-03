      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CDBRRD.
       DATE-WRITTEN.    111802.
      **************************************************************************
      *                 (CL)
      *  DESCRIPTION:   READ CODE FILE FOR A GIVEN PASSED CD-BR-REFCD
      *
      *    CAN BE CALLED FROM ANY PROGRAM
      *    CURRENTLY CALL FROM LPLCAP
      *
      *    NOTE:
      *          AN INVALID READ CAUSES CD-BR-REFCD, CD-BR-LC-FG
      *                                              CD-BR-FILE-TYPE
      *          TO BE RETURNED WITH " "
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
           "@(#)./CL/CDBRRD.CBL S35 09/20/22-14:30:54 >".
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
       01  CDX-BR-REFCD          PIC X(5).
       01  CDX-BR-LC-FG          PIC X.
       01  CDX-BR-FILE-TYPE      PIC XX.

       PROCEDURE DIVISION
             USING FORM-PATHNAME
                   CDX-BR-REFCD
                   CDX-BR-LC-FG
                   CDX-BR-FILE-TYPE 
                   EXIT-PATHNAME.   

      ************************************************************
      *    C O N T R O L   M O D U L E
      ************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.
      D    STOP IO-FG.

           PERFORM OPEN-CD1-FILE.
           MOVE "BR"         TO CD-TYPE.
           MOVE CDX-BR-REFCD TO CD-CODE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-BAD
              MOVE " "         TO CDX-BR-REFCD
                                  CDX-BR-LC-FG
                                  CDX-BR-FILE-TYPE
           ELSE
              MOVE CD-BR-LC-FG     TO CDX-BR-LC-FG
              MOVE CD-BR-FILE-TYPE TO CDX-BR-FILE-TYPE.

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
