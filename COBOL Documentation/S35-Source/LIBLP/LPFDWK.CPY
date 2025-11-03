      * COPYMEMBER: LIBLP/LPFDWK
      *************************************************************
      *
      *          LOAN PROCESSING WORK RECORD
      *
      *  WK-KEY =
      *         =    1  SALES FINANCE INPUT
      *         =    2  DIRECT LOAN INPUT
      *         =    3  TRANSACTION POSTING
      *         =    4  CHECK DISBURSEMENT (DIRECT)
      *         =    5  SALES FINANCE MODIFY
      *         =    6  COLLATERAL TRAILER
      *         =    7  DIRECT LOAN MODIFY
      *         =    8  ADDONS
      *         =    9  ADDONS (USED TO HOLD ORIGINAL LOAN RECORD)
      *         =   11+ CHECKS
      *************************************************************
      $XFD  FILE=WKFILE
       FD  WK-FILE
           LABEL RECORD IS STANDARD.
       COPY "LIBLP/LP01WK.CPY".
