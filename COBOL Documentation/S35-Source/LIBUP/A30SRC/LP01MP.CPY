      * COPYMEMBER: LIBLP/LP01MP
      ******************************************************************
      *
      * DESCRIPTION:  LOAN MEMO PAYMENT FILE (CONVERTED)
      *
      * RECORD KEYS:  [MP1] LOAN #, SEQUENCE #  (ASCENDING)
      *
      * ***NOTE*** :  MP-REC IS COPY OF LP-REC.  THE RECORDS WERE
      *               CONVERTED AFTER THE RECORDS WERE ALREADY CREATED
      *               IN THE LP-REC (CURRENT PAYMENTS HAD ALREADY
      *               BEEN POSTED).  THIS WAS DONE IN A15 FOR REGACC.
      *
      *=================================================================
      * REV:
      * BAH 070711 MEMO PAYMENT FILE FOR VIEWING ONLY.
      *            CONVERTED PAYMENT RECORDS FOR REGACC PR# 154.
      *            THESE PAYMENTS WERE BEING CONVERTED AFTER REAL MORE
      *            CURRENT PAYMENTS HAD ALREADY BEEN POSTED.
      *
      * KEC 2017.0120 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED MP-REC TO USE 'REPLACING LEADING'
      *          ON LP-REC [LIBLP/LP01LP].
      *
      ******************************************************************
       COPY "LIBLP/LP01LP.CPY" REPLACING LEADING "LP" BY "MP".

