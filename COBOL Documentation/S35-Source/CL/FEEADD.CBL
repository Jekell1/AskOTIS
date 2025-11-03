       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FEEADD.
       DATE-WRITTEN.    10/29/2024.
      ************************************************************
      *                 (CL)
      *  DESCRIPTION:   FIND LOANS WITH AD&D FEECODES
      *
      *  IF ANY LOANS WITHIN 12 MONTHS WITH AD&D, CAN NOT SELL ANY AD&D
      *
      *                 CALLED FROM LNVERI
      *
      *  REV: 
      *   2024.1111 BAH #1685 S35Q-144
      ************************************************************
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
       77  SCCS-IDENTIFIER PIC X(39) VALUE
           "@(#)CL/FEEADD.C Z15 11/04/24-12:29:21 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
 
       01  ERRCD           PIC X         VALUE SPACES.
       01  IO-TYPE         PIC X(3).
       01  MSEL-LIST       PIC X(5)      VALUE "MSEL.".
 
       01  SUB             PIC 9(3) COMP-3 VALUE 0.
 
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
 
      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/FEEADDW.CPY".
       PROCEDURE DIVISION
           USING FORM-PATHNAME FEEADD-WORKERS EXIT-PATHNAME.

      ************************************************************
      *    C O N T R O L   M O D U L E 
      ************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "SEARCHING FOR PRIOR LOANS....." TO MESS.
           PERFORM SEND-LEGEND.

      * NEED TO KEEP CHECKING PRIORS BACK UNTIL GREATER THAN 12 MONTHS
      * IF ANY HAVE AD&D WITHIN 12 MONTHS, CAN NOT WRITE AD&D
      D    STOP MESS.

           MOVE " " TO FEEADD-CAN-SELL.

           PERFORM OPEN-LN4-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE FEEADD-SSNO   TO LN-SSNO(1).

           PERFORM START-LN4-FILE.
       READ-NEXT.
           PERFORM READ-LN4-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-ROUTINE.

      * NO LOANS OR AT THE END OF THEIR LOANS
           IF LN-SSNO(1) NOT = FEEADD-SSNO
              GO TO END-ROUTINE.

           IF ( LN-POCD-NONLOAN )
              GO TO READ-NEXT.

      * DOES THE ACCOUNT HAVE AD&D SOLD
      * STOP CHECKING WHEN THE LOANS ARE OLDER THAN 12 MONTHS FROM NEW LOAN

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF LN-FEECODE(SUB) = FEEADD-INS-FEECODE
                 MOVE LN-LOANDATE       TO NUM-DATE
                 MOVE FEEADD-TRANS-DATE TO SYS-DATE
                 PERFORM TIM365
      * IF IS < 12, YOU CAN NOT SELL MORE AD&D, GET OUT
                 IF (ELAPSED-MONTHS < 12) OR
                     (ELAPSED-MONTHS = 12 AND ELAPSED-REM = 0)
                    MOVE "N" TO FEEADD-CAN-SELL
                    GO TO END-ROUTINE
                 ELSE
      * HERE IF > 12 OR 12 AND A REMAINDER(EXTRA DAYS), KEEP CHECKING MORE
                    GO TO READ-NEXT
                 END-IF
              END-IF
           END-PERFORM.

           GO TO READ-NEXT.

       EXIT-MAIN-PROGRAM.
           EXIT.
           GO TO END-ROUTINE.
                 
      *********************************
       MISC SECTION.
      *********************************
       SETUP-LINK-INFO.
       FORM-RESET.
       VDU-CONVERT-FIELD.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN4-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLN4RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
