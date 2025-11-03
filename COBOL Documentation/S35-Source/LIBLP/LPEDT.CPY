      * COPYMEMBER: LIBLP/LPEDT
      ********************************************************************
      *   E D I T   D A T A            
      *   INPUT : DATA-DIGITS          
      *           DATA-TYPE            
      *           DATA-DECIMAL         
      *           EDT-COL              
      *           EDT-EDIT            
      *           EDT-CLEAR-FG     
      *           EDT-LINE          
      *   OUTPUT: EDITED EDT-LINE  
      *           DATA-DIGITS        
      *           EDT-COL          
      *   USED:   LONPMC (VARIABLE FORM PRINT) LONPS3 (FORM INQUIRY)
      * 100226 BAH ADDED EDIT TYPE 12 TO MASK FIRST 3 DIGITS OF ACCOUNT
      *            NUMBER WITH XXX FOR REGENCY PR# 3545
      *              ACCOUNT 50112501 PRINTS AS XXX12501
      * 180313 BAH FIXED EDT-N7 FOR 8 DIGIT DATES TO STILL PRINT MM/DD/YY
      ********************************************************************
       EDIT-DATA SECTION.
       EDT-WHAT-TYPE.
           IF EDT-CLEAR-FG = "Y"
              MOVE 0 TO DATA-NUMOUT.
           MOVE 40 TO EDT-SUB.
           GO TO EDT-N1 EDT-N2 EDT-N3 EDT-N4 EDT-N5 EDT-N6 EDT-N7
              EDT-N8 EDT-N9 EDT-N10 EDT-N11 EDT-N12 EDT-N13
                                DEPENDING EDT-EDIT.
       EDT-CHK-0.
           IF DATA-NUMOUT = 0
              MOVE "Y" TO EDT-CLEAR-FG.
       EDT-N1.
           MOVE A-BUF TO EDT-BUF1.
           MOVE 1 TO EDT-SUB.
           PERFORM EDT-MOVE-LTOR.
           GO TO EXIT-EDIT-DATA.
       EDT-N2.
           MOVE DATA-NUMOUT TO EDT-2.
           PERFORM EDT-MOVE-NOPUNC.
           PERFORM EDT-MOVE-LTOR.
           GO TO EXIT-EDIT-DATA.
       EDT-N3.
           MOVE DATA-NUMOUT TO EDT-3.
           PERFORM EDT-MOVE-NOPUNC.
           PERFORM EDT-MOVE-LTOR.
           GO TO EXIT-EDIT-DATA.
       EDT-N4.
           MOVE DATA-NUMOUT TO EDT-4.
           PERFORM EDT-MOVE-PUNC.
           PERFORM EDT-MOVE-LTOR.
           GO TO EXIT-EDIT-DATA.
       EDT-N5.
           MOVE DATA-NUMOUT TO EDT-5.
           PERFORM EDT-MOVE-PUNC.
           ADD 1 TO DATA-DIGITS.
           SUBTRACT 1 FROM EDT-SUB.
           PERFORM EDT-MOVE-LTOR.
           GO TO EXIT-EDIT-DATA.
       EDT-N6.
           MOVE DATA-NUMOUT TO EDT-6.
           PERFORM EDT-MOVE-NOPUNC.
           ADD 1 TO DATA-DIGITS.
           SUBTRACT 1 FROM EDT-SUB.
           PERFORM EDT-MOVE-LTOR.
           GO TO EXIT-EDIT-DATA.
       EDT-N7.
      *    MOVE DATA-NUMOUT TO EDT-7.
           MOVE DATA-NUMOUT TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO EDT-7.
           MOVE 1 TO EDT-SUB.
           MOVE 8 TO DATA-DIGITS.
           PERFORM EDT-MOVE-LTOR.
           GO TO EXIT-EDIT-DATA.
       EDT-N8.
           MOVE DATA-NUMOUT TO EDT-PHONE.
           MOVE "(" TO EDT-1(1).
           MOVE ")" TO EDT-1(5).
           MOVE "-" TO EDT-1(10).
           MOVE EDT-BUF1 TO A-BUF.
           MOVE 14 TO DATA-DIGITS.
           GO TO EDT-CHK-0.
       EDT-N9.
           MOVE DATA-NUMOUT TO EDT-SSNO.
           MOVE "-" TO EDT-1(4) EDT-1(7).
           MOVE EDT-BUF1 TO A-BUF.
           MOVE 11 TO DATA-DIGITS.
           GO TO EDT-CHK-0.
       EDT-N10.
           MOVE EXT-CONAME-SYSDATE TO A-BUF.
           MOVE 8 TO DATA-DIGITS.
           GO TO EDT-N1.
       EDT-N11.
           MOVE DATA-NUMOUT TO EDT-TAXID.
           MOVE "-" TO EDT-1(3).
           MOVE EDT-BUF1 TO A-BUF.
           MOVE 10 TO DATA-DIGITS.
           GO TO EDT-CHK-0.
       EDT-N12.
           MOVE DATA-NUMOUT TO EDT-X-ACCT.
           MOVE "X" TO EDT-1(1) EDT-1(2) EDT-1(3).
           MOVE EDT-BUF1 TO A-BUF.
           MOVE 8 TO DATA-DIGITS.
           GO TO EDT-CHK-0.
       EDT-N13.
           MOVE DATA-NUMOUT TO EDT-6X-ACCT.
           MOVE "X" TO EDT-1(1) EDT-1(2) EDT-1(3).
           MOVE EDT-BUF1 TO A-BUF.
           MOVE 6 TO DATA-DIGITS.
           GO TO EDT-CHK-0.
       EXIT-EDIT-DATA.
           EXIT.

       EDT-MOVE-LTOR SECTION.
       EDT-DIGITS-LTOR.
           IF EDT-CLEAR-FG = "Y"
      *       FIELD IS CLEARED TO DISPLAYED LENGTH
              MOVE "N" TO EDT-CLEAR-FG
              MOVE " " TO EDT-BUF1.
           MOVE EDT-1(EDT-SUB) TO EDT-CHAR(EDT-COL).
           IF EDT-COL = 80
              GO TO EXIT-EDT-MOVE-LTOR.
           IF DATA-DIGITS > 1
              SUBTRACT 1 FROM DATA-DIGITS
              ADD 1 TO EDT-COL EDT-SUB
              GO TO EDT-DIGITS-LTOR.
       EXIT-EDT-MOVE-LTOR.
           EXIT.

       EDT-MOVE-NOPUNC SECTION.
       EDT-SET-LENGTH.
      *    THIS STMT ALLOWS 1 BYTE FOR ".", 1 SIGN
           COMPUTE DATA-DIGITS = DATA-DIGITS + 2.
      *    DETERMINE RIGHTMOST POSITION
           COMPUTE EDT-SUB = EDT-SUB - 5 + DATA-DECIMAL.
      *    NO DECIMAL PLACES ?, Y-ADJUST DATA-DIGITS, POSITION
           IF DATA-DECIMAL = 0
              SUBTRACT 1 FROM EDT-SUB DATA-DIGITS.
      *    ALL NUMERICS MUST ALLOW SIGN BYTE
           IF DATA-NUMOUT < 0
              MOVE "-" TO EDT-1(EDT-SUB)
           ELSE
              MOVE " " TO EDT-1(EDT-SUB).
           COMPUTE EDT-SUB = EDT-SUB - DATA-DIGITS + 1.

       EDT-MOVE-PUNC SECTION.
       EDT-SET-DIGITS.
      *    ALLOW 1 BYTE FOR ".", 1 FOR SIGN
           COMPUTE DATA-DIGITS = DATA-DIGITS + 2
      *    ADD AS NEEDED FOR PUNCTUATION
              + (DATA-DIGITS - DATA-DECIMAL - 1) / 3.
           COMPUTE EDT-SUB = EDT-SUB - 5 + DATA-DECIMAL.
           IF DATA-DECIMAL = 0
              SUBTRACT 1 FROM EDT-SUB DATA-DIGITS.
           IF DATA-NUMOUT < 0
              MOVE "-" TO EDT-1(EDT-SUB)
           ELSE
              MOVE " " TO EDT-1(EDT-SUB).
           COMPUTE EDT-SUB = EDT-SUB - DATA-DIGITS + 1.
