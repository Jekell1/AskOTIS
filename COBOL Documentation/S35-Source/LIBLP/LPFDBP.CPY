      * COPYMEMBER: LIBLP/LPFDBP
      *******************************************************
      *    BATCH PAYMENT RECORD
      *                  (24 BYTES)
      *
      *    FILE NAME:
      *              B      - BATCH PAYMENT FILE
      *              99     - BRANCH NO
      *              MMDD   - DATE BATCH ESTABLISHED
      *              999    - BATCH NO
      *
      *         WHEN THE FILE IS CREATED FROM MAGNETIC
      *         TAPE THE BATCH NO AND DATE ARE SET TO ZERO
      *
      *    WHEN BP-LNNO = 0, IT IS A HEADER RECORD.
      *            TYPE = "M", IT WAS CREATED FROM MAG TAPE
      *                 = "I", IT WAS INPUT
      *******************************************************
       FD  BP-FILE
           LABEL RECORD IS STANDARD.
       COPY "LIBLP/LP01BP.CPY".
