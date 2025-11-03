      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      INSTAX.
       DATE-WRITTEN.    020221.
      **************************************************************************
      *                 (CL)
      *  DESCRIPTION:   READ INSURANCE TAX RATE FILE
      *
      *                 CALLED FROM CALCZL.C & CALCZ3.C, LONPG3
      * REV:
      * JTG 020221 **NEW**
      *            ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS   WORLD #288
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
           "@(#)CL/INSTAX.CBL S34 05/24/21-15:16:17 >".
      **********************************
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  ERRCD           PIC X     VALUE SPACES.
       01  SUB             PIC 99.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".


      **********************************
      * LINKAGE PROGRAM SECTION:
      **********************************
       COPY "LIBGB/GETFMW.CPY".
       01  CD-CCY-REC.
           03  CD-CCY-KEY.
               05  CD-CCY-TYPE                   PIC X(2).
               05  CD-CCY-STATE                  PIC X(2).
               05  CD-CCY-CITY                   PIC X(3).
               05  CD-CCY-FILLER                 PIC X(3).
      *-----------------------------------------------------------------
           03  CD-CCY-INSTBL                     OCCURS 12.
               05  CD-CCY-INSTAXFRMLA            PIC X.
               05  CD-CCY-INSTAXAMT              PIC 99V99 COMP-3.
           03  CD-CCY-BOTH-TAX-FG                PIC X.

       01  CD-CCT-REC.
           03  CD-CCT-KEY.
               05  CD-CCT-TYPE                   PIC X(2).
               05  CD-CCT-STATE                  PIC X(2).
               05  CD-CCT-COUNTY                 PIC X(3).
               05  CD-CCT-FILLER                 PIC X(3).
      *-----------------------------------------------------------------
           03  CD-CCT-INSTBL                     OCCURS 12.
               05  CD-CCT-INSTAXFRMLA            PIC X.
               05  CD-CCT-INSTAXAMT              PIC 99V99 COMP-3.
           03  CD-CCT-BOTH-TAX-FG                PIC X.

       PROCEDURE DIVISION
             USING FORM-PATHNAME CD-CCY-REC CD-CCT-REC EXIT-PATHNAME.

      /***********************************************************
      *    C O N T R O L   M O D U L E
      ************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           PERFORM OPEN-CD1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    GET CITY TAX RECORD:
      *
      *      IF IO-BAD
      *         CLEAR CD-CY-RECX TO SPACES      (INDICATES NO TAXES)
      *      ELSE
      *         MOVE CD-CY-REC   TO CD-CY-RECX  (INDICATES CHARGE CITY TAX)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE CD-CCY-TYPE   TO CD-TYPE.
           MOVE CD-CCY-STATE  TO CD-CY-STATE.
           MOVE CD-CCY-CITY   TO CD-CY-CITY.
           MOVE CD-CCY-FILLER TO CD-CY-FILLER.
           PERFORM READ-CD1-FILE.
           IF IO-BAD
              MOVE SPACES TO CD-CCY-REC
           ELSE
              MOVE CD-CY-BOTH-TAX-FG TO CD-CCY-BOTH-TAX-FG
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
                 MOVE CD-CY-INSTBL(SUB) TO CD-CCY-INSTBL(SUB)
              END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    GET COUNTY TAX RECORD:
      *
      *      IF IO-BAD
      *         CLEAR CD-CT-RECX TO SPACES      (INDICATES NO TAXES)
      *      ELSE
      *         MOVE CD-CT-REC   TO CD-CT-RECX  (INDICATES CHARGE COUNTY TAX)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE CD-CCT-TYPE   TO CD-TYPE.
           MOVE CD-CCT-STATE  TO CD-CT-STATE.
           MOVE CD-CCT-COUNTY TO CD-CT-COUNTY.
           MOVE CD-CCT-FILLER TO CD-CT-FILLER.
           PERFORM READ-CD1-FILE.
           IF IO-BAD
              MOVE SPACES TO CD-CCT-REC
           ELSE
              MOVE CD-CT-BOTH-TAX-FG TO CD-CCT-BOTH-TAX-FG
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
                 MOVE CD-CT-INSTBL(SUB) TO CD-CCT-INSTBL(SUB)
              END-PERFORM.

           PERFORM CLOSE-CD1-FILE.
           GO TO END-ROUTINE.

       COPY "LIBGB/DATER.CPY".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
