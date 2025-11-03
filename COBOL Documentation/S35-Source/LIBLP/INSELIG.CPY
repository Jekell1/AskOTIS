      * COPYMEMBER: LIBLP/INSELIG
      **************************************************************************
      *                    LIBLP/INSELIG
      *            (FRONT END INSURANCE TEST ROUTINE)
      *
      *      VALIDATE INSURANCE TYPES ELIGABLITY
      *                      AND
      *      ESTABLISH VALID INS. ORIGINATION FEES
      *            CL - FEECODE(1)
      *            AH - FEECODE(2)
      *            PP - FEECODE(3)
      *            O1 - FEECODE(4)
      *
      *      NOTE:
      *           IF CHANGES MADE, REVIEW LIBLP/REVOLINSELIG INSURANCE
      *              ELIGIBILITY FOR REVOLVING LOANS
      * REV:
      *  JTG 010496 CHANGED REFERENCE: (27) TO (CL-ELE)  (28) TO (AH-ELE)
      *  JTG 011596 PUT BACK DISPLAY OF INSURANCE COMPANIES
      *  JTG 060397 ADDED TEST OF 'O3' RE: BR-OTH-INS-LEVELS
      *  JTG 073197 ADDED LOGIC FOR TOTAL-FEEFINCHG AND TOTAL-INSFINCHG
      *  BAH 990222 ADDED SP-INSEXCEPT 13, REGENCY  (SP-CL-AH-MAXIMUMS) #023
      *  BAH 011203 ADDED SP-INSEXCEPT 14, REGENCY (SP-ORGTERM-MAXIMUM) PR#1657
      *   CS 050330 ADD CHECK OF SP-INSURANCE-WRITTEN.  IF 'N' SET CHANGE FLAGS
      *             TO 'N' FOR INSURANCES O2 & O3  [LENDMARK PR#2479]
      **************************************************************************
       VALID-INS-TYPES-ELIG SECTION.
           MOVE " " TO FEECODE-TABLE.
           MOVE 0 TO IO-FG.
           IF PD-INSURED = "S" OR "J"
              IF NOT QUOTE-ONLY
                 IF PD-SSNO(2) = 0
                    MOVE 9 TO IO-FG
                    GO TO EXIT-VALID-INS-TYPES-ELIG.

      * READ CURRENT STATE PARAMETER RECORD
      * INORDER TO CHECK INSURANCE COMPANIES (SUBSCRIPTED) = 0:

           PERFORM GET-SP.
           IF SP-INSURANCE-WRITTEN = "N"
              MOVE "N" TO CHFG(O2-ELE) CHFG(O3-ELE).

      * CHECK IF OTHER INSURANCES WERE PURCHASED:
           IF BR-OTH-INS-LEVELS NOT = "Y"
              MOVE "N" TO CHFG(O1-ELE) CHFG(O2-ELE) CHFG(O3-ELE).

           IF PD-INSURED = "N"
              IF PD-INSTYPES NOT = "N"
                 MOVE 9 TO IO-FG
                 GO TO EXIT-VALID-INS-TYPES-ELIG
              ELSE
                 MOVE "N" TO CHFG(CL-ELE) CHFG(AH-ELE)
                             CHFG(PP-ELE) CHFG(O1-ELE)
                 GO TO EXIT-VALID-INS-TYPES-ELIG.

           MOVE "N" TO CHFG(CL-ELE) CHFG(AH-ELE)
                       CHFG(PP-ELE) CHFG(O1-ELE).

           IF PD-INSTYPES-LL
              MOVE " " TO CHFG(CL-ELE)
              PERFORM LEVEL-INSURANCE.
           IF PD-INSTYPES-DL
              MOVE " " TO CHFG(CL-ELE)
              PERFORM DECREASING-INSURANCE.
           IF PD-INSTYPES-AH
              MOVE " " TO CHFG(AH-ELE)
              PERFORM ACCIDENT-INSURANCE.
           IF PD-INSTYPES-PP OR PD-INSTYPES-PP2
              MOVE " " TO CHFG(PP-ELE)
              PERFORM PROPERTY-INSURANCE.
           IF PD-INSTYPES-O1
              MOVE " " TO CHFG(O1-ELE)
              PERFORM O1-INSURANCE.

       EXIT-VALID-INS-TYPES-ELIG.
           EXIT.

       LEVEL-INSURANCE SECTION.
           IF PD-INSURED = "J"
              IF SP-INSCOM(4) = 0
                 MOVE "N" TO CHFG(CL-ELE)
                 MOVE 9 TO IO-FG
                 MOVE "JOINT LEVEL LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 IF SP-CL-AH-MAXIMUMS(4) OR SP-ORGTERM-MAXIMUM(4)
                    IF PD-ORGTERM > SP-INSMAXTERM(4)
                       MOVE "N" TO CHFG(CL-ELE)
                       MOVE 9 TO IO-FG
                       MOVE "JOINT LEVEL LIFE EXCEEDS MAX TERM" TO MESS
                       PERFORM SEND-MESS
                    END-IF
                 END-IF
                 IF INSELIG-DISPLAY-FG = "Y"
                    MOVE "S  N020000." TO VDU
                    MOVE CL-ELE TO VDUROW
                    MOVE SP-INSCOM(4) TO VDUNUM0
                                         PD-INSCOMP(1)
                    PERFORM SCREENIO
                 END-IF
                 MOVE SP-INSFINCHG(4) TO PD-INSFINCHG(1)
                 IF SP-INSFEE-FRMLA(4) NOT = " "
                    MOVE SP-INSFEECD(4) TO FEECODE(1)
                 ELSE
                    NEXT SENTENCE
           ELSE
              IF SP-INSCOM(3) = 0
                 MOVE "N" TO CHFG(CL-ELE)
                 MOVE 9 TO IO-FG
                 MOVE "LEVEL LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 IF SP-CL-AH-MAXIMUMS(3) OR SP-ORGTERM-MAXIMUM(3)
                    IF PD-ORGTERM > SP-INSMAXTERM(3)
                       MOVE "N" TO CHFG(CL-ELE)
                       MOVE 9 TO IO-FG
                       MOVE "LEVEL LIFE EXCEEDS MAX TERM" TO MESS
                       PERFORM SEND-MESS
                    END-IF
                 END-IF
                 IF INSELIG-DISPLAY-FG = "Y"
                    MOVE "S  N020000." TO VDU
                    MOVE CL-ELE TO VDUROW
                    MOVE SP-INSCOM(3) TO VDUNUM0
                                         PD-INSCOMP(1)
                    PERFORM SCREENIO
                 END-IF
                 MOVE SP-INSFINCHG(3) TO PD-INSFINCHG(1)
                 IF SP-INSFEE-FRMLA(3) NOT = " "
                    MOVE SP-INSFEECD(3) TO FEECODE(1).
           IF BR-AH-AND-CL = "Y"
              IF PD-INSTYPES-AH
                 IF NOT(PD-INSTYPES-DL OR PD-INSTYPES-LL)
                    MOVE "N" TO CHFG(CL-ELE) CHFG(AH-ELE)
                    MOVE 9 TO IO-FG
                    MOVE "MUST WRITE LIFE TO WRITE A&&H" TO MESS
                    PERFORM SEND-MESS.

       DECREASING-INSURANCE SECTION.
           IF PD-INSURED = "J"
              IF SP-INSCOM(2) = 0
                 MOVE "N" TO CHFG(CL-ELE)
                 MOVE 9 TO IO-FG
                 MOVE "JOINT DECLINING LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 IF SP-CL-AH-MAXIMUMS(2) OR SP-ORGTERM-MAXIMUM(2)
                    IF PD-ORGTERM > SP-INSMAXTERM(2)
                       MOVE "N" TO CHFG(CL-ELE)
                       MOVE 9 TO IO-FG
                       MOVE "JOINT DECLINING LIFE EXCEEDS MAX TERM"
                              TO MESS
                       PERFORM SEND-MESS
                    END-IF
                 END-IF
                 IF INSELIG-DISPLAY-FG = "Y"
                    MOVE "S  N020000." TO VDU
                    MOVE CL-ELE TO VDUROW
                    MOVE SP-INSCOM(2) TO VDUNUM0
                                         PD-INSCOMP(1)
                    PERFORM SCREENIO
                  END-IF
                  MOVE SP-INSFINCHG(2) TO PD-INSFINCHG(1)
                  IF SP-INSFEE-FRMLA(2) NOT = " "
                     MOVE SP-INSFEECD(2) TO FEECODE(1)
                  ELSE
                     NEXT SENTENCE
           ELSE
              IF SP-INSCOM(1) = 0
                 MOVE "N" TO CHFG(CL-ELE)
                 MOVE 9 TO IO-FG
                 MOVE "DECLINING LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 IF SP-CL-AH-MAXIMUMS(1) OR SP-ORGTERM-MAXIMUM(1)
                    IF PD-ORGTERM > SP-INSMAXTERM(1)
                       MOVE "N" TO CHFG(CL-ELE)
                       MOVE 9 TO IO-FG
                       MOVE "DECLINING LIFE EXCEEDS MAX TERM" TO MESS
                       PERFORM SEND-MESS
                    END-IF
                 END-IF
                 IF INSELIG-DISPLAY-FG = "Y"
                    MOVE "S  N020000." TO VDU
                    MOVE CL-ELE TO VDUROW
                    MOVE SP-INSCOM(1) TO VDUNUM0
                                         PD-INSCOMP(1)
                    PERFORM SCREENIO
                 END-IF
                 MOVE SP-INSFINCHG(1) TO PD-INSFINCHG(1)
                 IF SP-INSFEE-FRMLA(1) NOT = " "
                    MOVE SP-INSFEECD(1) TO FEECODE(1).
           IF BR-AH-AND-CL = "Y"
              IF PD-INSTYPES-AH
                 IF NOT(PD-INSTYPES-DL OR PD-INSTYPES-LL)
                    MOVE "N" TO CHFG(CL-ELE) CHFG(AH-ELE)
                    MOVE 9 TO IO-FG
                    MOVE "MUST WRITE LIFE TO WRITE A&&H" TO MESS
                    PERFORM SEND-MESS.

       ACCIDENT-INSURANCE SECTION.
           IF SP-INSCOM(5) = 0
              MOVE "N" TO CHFG(AH-ELE)
              MOVE 9 TO IO-FG
              MOVE "A && H NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
           ELSE
              IF SP-CL-AH-MAXIMUMS(5) OR SP-ORGTERM-MAXIMUM(5)
                 IF PD-ORGTERM > SP-INSMAXTERM(5)
                    MOVE "N" TO CHFG(AH-ELE)
                    MOVE 9 TO IO-FG
                    MOVE "A && H EXCEEDS MAX TERM" TO MESS
                    PERFORM SEND-MESS
                 END-IF
              END-IF
              IF INSELIG-DISPLAY-FG = "Y"
                 MOVE "S  N020000." TO VDU
                 MOVE AH-ELE TO VDUROW
                 MOVE SP-INSCOM(5) TO VDUNUM0
                                      PD-INSCOMP(2)
                 PERFORM SCREENIO
              END-IF
              MOVE SP-INSFINCHG(5) TO PD-INSFINCHG(2)
              IF SP-INSFEE-FRMLA(5) NOT = " "
                 MOVE SP-INSFEECD(5) TO FEECODE(2).
           IF BR-AH-AND-CL = "Y"
              IF PD-INSTYPES-AH
                 IF NOT(PD-INSTYPES-DL OR PD-INSTYPES-LL)
                    MOVE "N" TO CHFG(CL-ELE) CHFG(AH-ELE)
                    MOVE 9 TO IO-FG
                    MOVE "MUST WRITE LIFE TO WRITE A&&H" TO MESS
                    PERFORM SEND-MESS.

       PROPERTY-INSURANCE SECTION.
           IF PD-INSTYPES-PP
              IF SP-INSCOM(6) = 0
                 MOVE "N" TO CHFG(PP-ELE)
                 MOVE 9 TO IO-FG
                 MOVE "PROPERTY INSURANCE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 IF INSELIG-DISPLAY-FG = "Y"
                    MOVE "S  N020000." TO VDU
                    MOVE PP-ELE TO VDUROW
                    MOVE SP-INSCOM(6) TO VDUNUM0
                                         PD-INSCOMP(3)
                    PERFORM SCREENIO
                 END-IF
                 MOVE SP-INSFINCHG(6) TO PD-INSFINCHG(3)
                 IF SP-INSFEE-FRMLA(6) NOT = " "
                    MOVE SP-INSFEECD(6) TO FEECODE(3).

           IF PD-INSTYPES-PP2
              IF SP-INSCOM(7) = 0
                 MOVE "N" TO CHFG(PP-ELE)
                 MOVE 9 TO IO-FG
                 MOVE "PROPERTY INSURANCE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 IF INSELIG-DISPLAY-FG = "Y"
                    MOVE "S  N020000." TO VDU
                    MOVE PP-ELE TO VDUROW
                    MOVE SP-INSCOM(7) TO VDUNUM0
                                         PD-INSCOMP(3)
                    PERFORM SCREENIO
                 END-IF
                 MOVE SP-INSFINCHG(7) TO PD-INSFINCHG(3)
                 IF SP-INSFEE-FRMLA(7) NOT = " "
                    MOVE SP-INSFEECD(7) TO FEECODE(3).

       O1-INSURANCE SECTION.
           IF SP-INSCOM(8) = 0
              MOVE "N" TO CHFG(O1-ELE)
              MOVE 9 TO IO-FG
              MOVE "OTHER 1 INSURANCE NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
           ELSE
              IF INSELIG-DISPLAY-FG = "Y"
                 MOVE "S  N020000." TO VDU
                 MOVE O1-ELE TO VDUROW
                 MOVE SP-INSCOM(8) TO VDUNUM0
                                      PD-INSCOMP(4)
                 PERFORM SCREENIO
              END-IF
              MOVE SP-INSFINCHG(8) TO PD-INSFINCHG(4)
              IF SP-INSFEE-FRMLA(8) NOT = " "
                 MOVE SP-INSFEECD(8) TO FEECODE(4).

