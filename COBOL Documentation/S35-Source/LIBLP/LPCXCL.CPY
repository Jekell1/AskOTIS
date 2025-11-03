      * COPYMEMBER: LIBLP/LPCXCL

      ***********************************************************************
      *    LPCXCL    CHECK REQUEST CLEAR ROUTINE
      *
      * REV:
      *  GLB 080200  NEW COPY MEMBER
      *  BLF 060713 ADD CLEAR OF CK-PAYEE-NO, DEVELOPMENT PR# 2633
      *   CS 190520 CLEAR FUTURE FIELDS
      *  BAH 20210427 REMOVED FUTURE FIELDS
      ***********************************************************************
       CLEAR-CX-FILE SECTION.
           MOVE SPACES TO CX-REC.

           MOVE 0 TO CX-BRNO CX-SEQNO CX-LTOUCH-DATE
                     CX-AMT CX-REF CX-CLASS CX-CKNO CX-DLNO CX-DATE
                     CX-BATCHID CX-CKNO-NEW CX-DLRVNO CX-DT-SEQ
                     CX-TILLNO CX-ACCTNO CX-DT-DATE CX-SSNO
                     CX-GLNO(1) CX-GLAMT(1)
                     CX-GLNO(2) CX-GLAMT(2)
                     CX-GLNO(3) CX-GLAMT(3)
                     CX-GLNO(4) CX-GLAMT(4)
                     CX-GLNO(5) CX-GLAMT(5)
                     CX-PAYEE-NO.
