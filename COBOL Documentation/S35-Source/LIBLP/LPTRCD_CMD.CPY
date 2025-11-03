      * COPYMEMBER: LIBLP/LPTRCD.CMD
      **************************************************************************
      *               LPTRCD.CMD
      *
      *    TRANSACTION CODE SCREEN DISPLAY BUFFERS
      *
      * rev:
      *  jtg 042998 added display for 'rb' and 'ro' renewal options
      *  bah 060598 added display for 'px' chargeoff partial, mercury #e175
      *  bah 060998 added display for 'bk' bankruptcy, mercury #e174
      *  bah 070898 added display for 'bd' remove bankruptcy, mercury #e195
      *  jtg 990218 added display for 'pu' revolving purchase, regency #019
      *  bah 991228 removed all insurance cancel codes until they work, jkruse
      *  jtg 020712 added 'r3' 'av' and moved 'pu'                      lori
      *  bah 021011 added "in" insurance loss regacc 0007
      *  bah 021231 added all missing trcd's per kruse.
      **************************************************************************
       01  TRCD-LINE.
           03  FILLER        PIC X(20) VALUE
               "CD  DESCRIPTION    ".
           03  FILLER        PIC X(20) VALUE
               "CD  DESCRIPTION    ".
           03  FILLER        PIC X(20) VALUE
               "CD  DESCRIPTION    ".
           03  FILLER        PIC X(20) VALUE
               "CD  DESCRIPTION    ".
       01  TRCD-BUF.
           03  FILLER        PIC X(20) VALUE
               "PY  Normal  Payment ".
           03  FILLER        PIC X(20) VALUE
               "AL  Assess Late Chg ".
           03  FILLER        PIC X(20) VALUE
               "JD  Judgement       ".
           03  FILLER        PIC X(20) VALUE
               "PO  Normal  Payoff  ".
           03  FILLER       PIC X(20) VALUE
               "PA  Allot   Payment ".
           03  FILLER        PIC X(20) VALUE
               "AI  Assess Interest ".
           03  FILLER        PIC X(20) VALUE
               "PX  Partial Chrg Off".
           03  FILLER        PIC X(20) VALUE
               "RN  Renewal (Bal)   ".
           03  FILLER        PIC X(20) VALUE
               "PE  Excptn  Payment ".
           03  FILLER        PIC X(20) VALUE
               "OT  Assess OtherChg ".
           03  FILLER        PIC X(20) VALUE
               "PI  P&L to Inactive ".
           03  FILLER        PIC X(20) VALUE
               "RB  Renewal (Bankrpt".
           03  FILLER        PIC X(20) VALUE
               "AH  A&H     Payment ".
           03  FILLER        PIC X(20) VALUE
               "O2  Assess Oth2 Chg ".
           03  FILLER        PIC X(20) VALUE
               "PW  Remove P&L Sys  ".
           03  FILLER        PIC X(20) VALUE
               "RO  Renewal (Other) ".
           03  FILLER        PIC X(20) VALUE
               "DF  Deferment Chg   ".
           03  FILLER        PIC X(20) VALUE
               "WL  Waive Late Chg  ".
           03  FILLER        PIC X(20) VALUE
               "SE  Shift P&L Recv  ".
           03  FILLER        PIC X(20) VALUE
               "PB  Renewal (Addon) ".
           03  FILLER        PIC X(20) VALUE
               "D2 to D9  Multi DF  ".
           03  FILLER        PIC X(20) VALUE
               "WI  Waive Interest  ".
           03  FILLER        PIC X(20) VALUE
               "AC  Accelerate Acct ".
           03  FILLER        PIC X(20) VALUE
               "SC  Sales Conversion".
           03  FILLER        PIC X(20) VALUE
               "PL  Charge Off Full ".
           03  FILLER        PIC X(20) VALUE
               "WO  Waive Other Chg ".
           03  FILLER        PIC X(20) VALUE
               "IR  Interest Reduct ".
           03  FILLER        PIC X(20) VALUE
               "TS  Transfer Service".
           03  FILLER        PIC X(20) VALUE
               "P2 to P5, P&L       ".  
           03  FILLER        PIC X(20) VALUE
               "W2  Waive Oth2  Chg ".
           03  FILLER        PIC X(20) VALUE
               "DC  Due Date Change ".
           03  FILLER        PIC X(20) VALUE
               "TP  Transfer Purchse".

       01  TRCD-BUF2.
           03  FILLER        PIC X(20) VALUE
               "RC  Rebate CL Ins   ".
           03  FILLER        PIC X(20) VALUE
               "AP  Apply to Prin   ".
      *        "CC  Cancel CL Ins   ".
           03  FILLER        PIC X(20) VALUE
               "RI  Rebate Interest ".
           03  FILLER        PIC X(20) VALUE
               "PR  Pyt Out Rsv Prin".
           03  FILLER        PIC X(20) VALUE
               "RA  Rebate AH Ins   ".
           03  FILLER        PIC X(20) VALUE
               "OP  ot/ap combo     ".
      *        "CA  Cancel AH Ins   ".
           03  FILLER        PIC X(20) VALUE
               "RM  Rebate MaintFee ".
           03  FILLER        PIC X(20) VALUE
               "PC  Pyt Out Rsv Chgs".
           03  FILLER        PIC X(20) VALUE
               "RP  Rebate PP Ins   ".
           03  FILLER        PIC X(20) VALUE
               "CP  Cancel PP Ins   ".
           03  FILLER        PIC X(20) VALUE
               "RD  Rebate Deferment".
           03  FILLER        PIC X(20) VALUE
               "PP  Pyt Out Rsv Part".
           03  FILLER        PIC X(20) VALUE
               "R1  Rebate O1 Ins   ".
           03  FILLER        PIC X(20) VALUE
               "C1  Cancel O1 Ins   ".
           03  FILLER        PIC X(20) VALUE
               "RS  Rebate Serv Chg ".
           03  FILLER        PIC X(20) VALUE
               "Z2  Pyt Agst OT2 BAL".
           03  FILLER        PIC X(20) VALUE
               "R2  Rebate O2 Ins   ".
           03  FILLER        PIC X(20) VALUE
               "IN  Ins. Claim Check".
      *        "C2  Cancel O2 Ins   ".
           03  FILLER        PIC X(20) VALUE
               "BK  Set Bankruptcy  ".
           03  FILLER        PIC X(20) VALUE
               "RE  Repo Exp Addon  ".
           03  FILLER        PIC X(20) VALUE
               "R3  Rebate O3 Ins  ".
           03  FILLER        PIC X(20) VALUE
               "SP  Set Pool ID     ".
           03  FILLER        PIC X(20) VALUE
               "BD  Remove Bankrptcy".
           03  FILLER        PIC X(20) VALUE
               "SX  Set Repo        ".
           03  FILLER        PIC X(20) VALUE
               "                    ".
           03  FILLER        PIC X(20) VALUE
               " ".
           03  FILLER        PIC X(20) VALUE
               "PN  Non cash w/glno ".
           03  FILLER        PIC X(20) VALUE
               "SV  Set Repo Redeem ".
           03  FILLER        PIC X(20) VALUE
               "AD  Allowable Delnq ".
           03  FILLER        PIC X(20) VALUE
               "A2 to A9  Multi AD  ".
           03  FILLER        PIC X(20) VALUE
               "PZ  pn,  no recdel  ".
           03  FILLER        PIC X(20) VALUE
               "SS  Set Repo Sold   ".
