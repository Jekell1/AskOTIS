      * COPYMEMBER: LIBUP/UPCKCL
      ******************************************************************
      * CLEAR ASCII CK FILE
      *
      ******************************************************************
       CLEAR-ACK-FILE SECTION.
           MOVE ALL X"09" TO ACK-REC.

           MOVE 0 TO ACK-BRNO ACK-NO ACK-DATE ACK-AMT ACK-REF
                     ACK-LTOUCH-DATE ACK-GLNO(1)  ACK-GLNO(2)
                     ACK-GLNO(3)     ACK-GLNO(4)  ACK-GLNO(5)
                     ACK-GLAMT(1) ACK-GLAMT(2) ACK-GLAMT(3) ACK-GLAMT(4)
                     ACK-GLAMT(5)
                     ACK-CLASS ACK-DLNO ACK-VOID-DATE ACK-ACCTNO
                     ACK-DLRVNO ACK-SSNO
                     ACK-BATCHID ACK-CLEARED-DATE ACK-PAYEE-NO
                     ACK-NC-SEQNO ACK-TILLNO.

            MOVE SPACES TO ACK-TYPE ACK-PAYEE ACK-ADR1 ACK-ADR2       
                           ACK-CITY ACK-STATE ACK-ZIP ACK-REASON     
                           ACK-VOID ACK-PAID ACK-PGM ACK-CLEARED-FG 
                           ACK-UNPAID ACK-EVENT-CD ACK-FREE1      
                           ACK-FREE2 ACK-BWPROC.
