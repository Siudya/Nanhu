
`include "prim_assert.sv"

module rot_top #(
  // parameters for hmac
  // parameters for kmac
  parameter bit KmacEnMasking = 1,
  parameter bit KmacSwKeyMasked = 0,
  parameter int SecKmacCmdDelay = 0,
  parameter bit SecKmacIdleAcceptSwMsg = 0,
  // parameters for keymgr
  parameter bit KeymgrKmacEnMasking = 1,
  // parameters for rom_ctrl
  parameter RomCtrlBootRomInitFile = "/nfs/home/zhangdongrong/Desktop/Nanhu-V3/src/main/resources/TLROT/test.vmem",
  parameter bit SecRomCtrlDisableScrambling = 1'b0,
  // parameters for csrng
  parameter aes_pkg::sbox_impl_e CsrngSBoxImpl = aes_pkg::SBoxImplCanright,
  // parameters for entropy_src
  parameter int EntropySrcEsFifoDepth = 4,
  parameter bit EntropySrcStub = 0,
  // parameters for edn0
  // parameters for otbn
  parameter bit OtbnStub = 0,
  parameter otbn_pkg::regfile_e OtbnRegFile = otbn_pkg::RegFileFF,
  parameter bit SecOtbnMuteUrnd = 0,
  parameter bit SecOtbnSkipUrndReseedAtStart = 0,
  // alert
  parameter logic [14-1:0] AlertAsyncOn = {14{1'b1}}
) (
    input clk_i,
    input rst_ni,
    input rst_shadowed_ni,
    input clk_edn_i,
    input rst_edn_ni,

    // Bus Interface
    input  tlul_pkg::tl_h2d_t tl_i,
    output tlul_pkg::tl_d2h_t tl_o,

    // Interrupt
    output logic intr_hmac_hmac_done_o,
    output logic intr_hmac_fifo_empty_o,
    output logic intr_hmac_hmac_err_o,
    output logic intr_kmac_kmac_done_o,
    output logic intr_kmac_fifo_empty_o,
    output logic intr_kmac_kmac_err_o,
    output logic intr_keymgr_op_done_o,
    output logic intr_csrng_cs_cmd_req_done_o,
    output logic intr_csrng_cs_entropy_req_o,
    output logic intr_csrng_cs_hw_inst_exc_o,
    output logic intr_csrng_cs_fatal_err_o,
    output logic intr_entropy_src_es_entropy_valid_o,
    output logic intr_entropy_src_es_health_test_failed_o,
    output logic intr_entropy_src_es_observe_fifo_ready_o,
    output logic intr_entropy_src_es_fatal_err_o,
    output logic intr_edn0_edn_cmd_req_done_o,
    output logic intr_edn0_edn_fatal_err_o,
    output logic intr_otbn_done,

    // key output
    output keymgr_pkg::hw_key_req_t       keymgr_aes_key,
    // output keymgr_pkg::hw_key_req_t       keymgr_kmac_key,
    // output keymgr_pkg::otbn_key_req_t       keymgr_otbn_key,

    // entropy src
    output entropy_src_pkg::entropy_src_rng_req_t       es_rng_req_o,
    input entropy_src_pkg::entropy_src_rng_rsp_t       es_rng_rsp_i,
    // input prim_mubi_pkg::mubi8_t       entropy_src_otp_en_entropy_src_fw_read,
    // input prim_mubi_pkg::mubi8_t       entropy_src_otp_en_entropy_src_fw_over,
    output logic       es_rng_fips_o, 
    // input tlul_pkg::tl_h2d_t       entropy_src_tl_req,
    // output tlul_pkg::tl_d2h_t       entropy_src_tl_rsp,

    // rom
    // input kmac_pkg::app_rsp_t kmac_app_rsp_rom,
    // output kmac_pkg::app_req_t kmac_app_req_rom,
    output rom_ctrl_pkg::pwrmgr_data_t       rom_ctrl_pwrmgr_data,
    // input prim_rom_pkg::rom_cfg_t       ast_rom_cfg,
    input tlul_pkg::tl_h2d_t64 rom_ctrl_rom_tl_req,
    output tlul_pkg::tl_d2h_t64 rom_ctrl_rom_tl_rsp,

    // kmac
    // output kmac_pkg::app_rsp_t kmac_app_rsp_lc,
    // input kmac_pkg::app_req_t kmac_app_req_lc,
    // output prim_mubi_pkg::mubi4_t  clkmgr_aon_idle_rot,

    //csrng
    // input csrng_pkg::csrng_req_t  rot_top_csrng_csrng_cmd_req,
    // output csrng_pkg::csrng_rsp_t  rot_top_csrng_csrng_cmd_rsp,
    // input csrng_pkg::csrng_req_t [1:0] csrng_csrng_cmd_req,
    // output csrng_pkg::csrng_rsp_t [1:0]  csrng_csrng_cmd_rsp,
    // input prim_mubi_pkg::mubi8_t       csrng_otp_en_csrng_sw_app_read,
    // input lc_ctrl_pkg::lc_tx_t lc_ctrl_lc_hw_debug_en,
    // input tlul_pkg::tl_h2d_t       csrng_tl_req,
    // output tlul_pkg::tl_d2h_t       csrng_tl_rsp,

    //edn0
    // output edn_pkg::edn_req_t edn0_edn_req_rot,
    // input edn_pkg::edn_rsp_t edn0_edn_rsp_rot,
    input edn_pkg::edn_req_t [7:0] edn0_edn_req,
    output edn_pkg::edn_rsp_t [7:0] edn0_edn_rsp,
    // input tlul_pkg::tl_h2d_t       edn0_tl_req,
    // output tlul_pkg::tl_d2h_t       edn0_tl_rsp,

    //otbn
    // input prim_ram_1p_pkg::ram_1p_cfg_t       ast_ram_1p_cfg,
    // output otp_ctrl_pkg::otbn_otp_key_req_t       otp_ctrl_otbn_otp_key_req,
    // input otp_ctrl_pkg::otbn_otp_key_rsp_t       otp_ctrl_otbn_otp_key_rsp,
    // input lc_ctrl_pkg::lc_tx_t       flash_ctrl_rma_ack,
    // output lc_ctrl_pkg::lc_tx_t       otbn_lc_rma_ack,

    // alerts NAlerts = 14
    input  prim_alert_pkg::alert_rx_t [14-1:0] alert_rx_i,
    output prim_alert_pkg::alert_tx_t [14-1:0] alert_tx_o
);

  import tlul_pkg::*;
  import top_pkg::*;
  // Compile-time random constants
  import top_earlgrey_rnd_cnst_rot_pkg::*;

  //local parameter
  

  // Signals
  //tlul signle
  tlul_pkg::tl_h2d_t       hmac_tl_req;
  tlul_pkg::tl_d2h_t       hmac_tl_rsp;
  tlul_pkg::tl_h2d_t       kmac_tl_req;
  tlul_pkg::tl_d2h_t       kmac_tl_rsp;
  tlul_pkg::tl_h2d_t       entropy_src_tl_req;
  tlul_pkg::tl_d2h_t       entropy_src_tl_rsp;
  tlul_pkg::tl_h2d_t       csrng_tl_req;
  tlul_pkg::tl_d2h_t       csrng_tl_rsp;
  tlul_pkg::tl_h2d_t       edn0_tl_req;
  tlul_pkg::tl_d2h_t       edn0_tl_rsp;
  tlul_pkg::tl_h2d_t       keymgr_tl_req;
  tlul_pkg::tl_d2h_t       keymgr_tl_rsp;
  // tlul_pkg::tl_h2d_t       rom_ctrl_rom_tl_req;
  // tlul_pkg::tl_d2h_t       rom_ctrl_rom_tl_rsp;
  tlul_pkg::tl_h2d_t       rom_ctrl_regs_tl_req;
  tlul_pkg::tl_d2h_t       rom_ctrl_regs_tl_rsp;
  tlul_pkg::tl_h2d_t       otbn_tl_req;
  tlul_pkg::tl_d2h_t       otbn_tl_rsp;
  tlul_pkg::tl_h2d_t       sm3_tl_req;
  tlul_pkg::tl_d2h_t       sm3_tl_rsp;
  tlul_pkg::tl_h2d_t       sm4_tl_req;
  tlul_pkg::tl_d2h_t       sm4_tl_rsp;
  tlul_pkg::tl_h2d_t       rs_encode_tl_req;
  tlul_pkg::tl_d2h_t       rs_encode_tl_rsp;
  tlul_pkg::tl_h2d_t       rs_decode_tl_req;
  tlul_pkg::tl_d2h_t       rs_decode_tl_rsp;
  tlul_pkg::tl_h2d_t       puf_tl_req;
  tlul_pkg::tl_d2h_t       puf_tl_rsp;


  // Alert list
  // localparam NAlerts = 12;
  // prim_alert_pkg::alert_tx_t [NAlerts-1:0]  alert_tx;
  // prim_alert_pkg::alert_rx_t [NAlerts-1:0]  alert_rx;
  // localparam prim_alert_pkg::alert_rx_t [NAlerts-1:0] alert_rx = {NAlerts{prim_alert_pkg::ALERT_RX_DEFAULT}};

  
  // Interrupt source list
  // logic [16:0]  intr_vector;
  // logic  unused_intr_vector;
  // logic intr_hmac_hmac_done;
  // logic intr_hmac_fifo_empty;
  // logic intr_hmac_hmac_err;
  // logic intr_kmac_kmac_done;
  // logic intr_kmac_fifo_empty;
  // logic intr_kmac_kmac_err;
  // logic intr_keymgr_op_done;
  // logic intr_csrng_cs_cmd_req_done;
  // logic intr_csrng_cs_entropy_req;
  // logic intr_csrng_cs_hw_inst_exc;
  // logic intr_csrng_cs_fatal_err;
  // logic intr_entropy_src_es_entropy_valid;
  // logic intr_entropy_src_es_health_test_failed;
  // logic intr_entropy_src_es_observe_fifo_ready;
  // logic intr_entropy_src_es_fatal_err;
  // logic intr_edn0_edn_cmd_req_done;
  // logic intr_edn0_edn_fatal_err;

  // define inter-module signal
  prim_mubi_pkg::mubi4_t [2:0] clkmgr_aon_idle;
  logic unused_clkmgr_aon_idle;
  assign unused_clkmgr_aon_idle = ^ clkmgr_aon_idle;

  // localparam  kmac_pkg::lc_tx_t       lc_ctrl_lc_escalate_en = kmac_pkg::LC_TX_DEFAULT;
  localparam  lc_ctrl_pkg::lc_tx_t       lc_ctrl_lc_escalate_en = lc_ctrl_pkg::Off;

  //Keymgr
  // edn_pkg::edn_req_t [1:0] edn0_edn_req;
  // edn_pkg::edn_rsp_t [1:0] edn0_edn_rsp;
  // otp_ctrl_pkg::otp_keymgr_key_t       otp_ctrl_otp_keymgr_key;
  // otp_ctrl_pkg::otp_device_id_t       keymgr_otp_device_id;
  localparam otp_ctrl_pkg::otp_keymgr_key_t otp_ctrl_otp_keymgr_key = otp_ctrl_pkg::OTP_KEYMGR_KEY_DEFAULT;
  localparam otp_ctrl_pkg::otp_device_id_t keymgr_otp_device_id = 256'h48ecf6c738f0f108a5b08620695ffd4d48ecf6c738f0f108a5b08620695ffd4d;
  // keymgr_pkg::hw_key_req_t       keymgr_aes_key;
  keymgr_pkg::hw_key_req_t       keymgr_kmac_key;
  keymgr_pkg::otbn_key_req_t       keymgr_otbn_key;
  kmac_pkg::app_req_t [2:0] kmac_app_req;
  // assign kmac_app_req[1] = kmac_pkg::APP_REQ_DEFAULT;
  kmac_pkg::app_rsp_t [2:0] kmac_app_rsp;
  // assign kmac_app_rsp[1] = kmac_pkg::APP_RSP_DEFAULT;
  logic       kmac_en_masking;
  // flash_ctrl_pkg::keymgr_flash_t       flash_ctrl_keymgr;
  // localparam keymgr_pkg::keymgr_flash_t flash_ctrl_keymgr = keymgr_pkg::KEYMGR_FLASH_DEFAULT;
  localparam flash_ctrl_pkg::keymgr_flash_t flash_ctrl_keymgr = flash_ctrl_pkg::KEYMGR_FLASH_DEFAULT;

  // lc_ctrl_pkg::lc_tx_t       lc_ctrl_lc_keymgr_en;
  // lc_ctrl_pkg::lc_keymgr_div_t       lc_ctrl_lc_keymgr_div;
  localparam lc_ctrl_pkg::lc_tx_t lc_ctrl_lc_keymgr_en = lc_ctrl_pkg::On;
  localparam lc_ctrl_pkg::lc_keymgr_div_t lc_ctrl_lc_keymgr_div = 128'h48aaf6c738f0f108a5b08620695ffd4d;
  rom_ctrl_pkg::keymgr_data_t       rom_ctrl_keymgr_data;
  // localparam rom_ctrl_pkg::keymgr_data_t rom_ctrl_keymgr_data = rom_ctrl_pkg::ROM_KEYMGR_DATA_DEFAULT;

  //ROM
  // prim_rom_pkg::rom_cfg_t       ast_rom_cfg;
  localparam prim_rom_pkg::rom_cfg_t       ast_rom_cfg = prim_rom_pkg::ROM_CFG_DEFAULT;
  // rom_ctrl_pkg::pwrmgr_data_t       rom_ctrl_pwrmgr_data;
  // logic       unused_rom_ctrl_pwrmgr_data;
  // assign unused_rom_ctrl_pwrmgr_data = ^ rom_ctrl_pwrmgr_data;
  // rom_ctrl_pkg::keymgr_data_t       rom_ctrl_keymgr_data;

  //csrng
  csrng_pkg::csrng_req_t [1:0]  csrng_csrng_cmd_req;
  csrng_pkg::csrng_rsp_t [1:0] csrng_csrng_cmd_rsp;
  // prim_mubi_pkg::mubi8_t       csrng_otp_en_csrng_sw_app_read;
  localparam  MuBi8False = 8'h69;
  localparam prim_mubi_pkg::mubi8_t       csrng_otp_en_csrng_sw_app_read = prim_mubi_pkg::mubi8_t'(MuBi8False);
  // lc_ctrl_pkg::lc_tx_t       lc_ctrl_lc_hw_debug_en;
  localparam lc_ctrl_pkg::lc_tx_t lc_ctrl_lc_hw_debug_en = lc_ctrl_pkg::On;

    // EDN0
  edn_pkg::edn_req_t [7:0] edn0_edn_req_intr;
  edn_pkg::edn_rsp_t [7:0] edn0_edn_rsp_intr;

  //entropy src
  entropy_src_pkg::entropy_src_hw_if_req_t       csrng_entropy_src_hw_if_req;
  entropy_src_pkg::entropy_src_hw_if_rsp_t       csrng_entropy_src_hw_if_rsp;
  entropy_src_pkg::cs_aes_halt_req_t       csrng_cs_aes_halt_req;
  entropy_src_pkg::cs_aes_halt_rsp_t       csrng_cs_aes_halt_rsp;
  // entropy_src_pkg::entropy_src_rng_req_t       es_rng_req_o;
  // entropy_src_pkg::entropy_src_rng_rsp_t       es_rng_rsp_i;
  // localparam entropy_src_pkg::entropy_src_rng_rsp_t       es_rng_rsp_i = entropy_src_pkg::ENTROPY_SRC_RNG_RSP_DEFAULT; 
  // prim_mubi_pkg::mubi8_t       entropy_src_otp_en_entropy_src_fw_read;
  // prim_mubi_pkg::mubi8_t       entropy_src_otp_en_entropy_src_fw_over;
  localparam prim_mubi_pkg::mubi8_t       entropy_src_otp_en_entropy_src_fw_read = prim_mubi_pkg::mubi8_t'(MuBi8False);
  localparam prim_mubi_pkg::mubi8_t       entropy_src_otp_en_entropy_src_fw_over = prim_mubi_pkg::mubi8_t'(MuBi8False);
  // logic       es_rng_fips_o;

  // otbn
  localparam prim_ram_1p_pkg::ram_1p_cfg_t       ast_ram_1p_cfg = prim_ram_1p_pkg::RAM_1P_CFG_DEFAULT;
  otp_ctrl_pkg::otbn_otp_key_req_t       otp_ctrl_otbn_otp_key_req;
  otp_ctrl_pkg::otbn_otp_key_rsp_t       otp_ctrl_otbn_otp_key_rsp;
  localparam lc_ctrl_pkg::lc_tx_t       flash_ctrl_rma_ack = lc_ctrl_pkg::LC_TX_DEFAULT;
  lc_ctrl_pkg::lc_tx_t       otbn_lc_rma_ack;

  // sinterrupt assignments
  // assign intr_vector = {
  //   intr_hmac_hmac_done,
  //   intr_hmac_fifo_empty,
  //   intr_hmac_hmac_err,
  //   intr_kmac_kmac_done,
  //   intr_kmac_fifo_empty,
  //   intr_kmac_kmac_err,
  //   intr_keymgr_op_done,
  //   intr_edn0_edn_cmd_req_done,
  //   intr_edn0_edn_fatal_err,
  //   intr_entropy_src_es_fatal_err, 
  //   intr_entropy_src_es_observe_fifo_ready, 
  //   intr_entropy_src_es_health_test_failed, 
  //   intr_entropy_src_es_entropy_valid, 
  //   intr_csrng_cs_fatal_err, 
  //   intr_csrng_cs_hw_inst_exc, 
  //   intr_csrng_cs_entropy_req, 
  //   intr_csrng_cs_cmd_req_done
  // };

  // assign unused_intr_vector = ^ intr_vector;

  assign kmac_app_req[2] = kmac_pkg::APP_REQ_DEFAULT;
  
  // assign kmac_app_rsp_lc = kmac_app_rsp[2]; 

  // assign csrng_csrng_cmd_req[1] = rot_top_csrng_csrng_cmd_req;
  // assign rot_top_csrng_csrng_cmd_rsp = csrng_csrng_cmd_rsp[1];

  assign edn0_edn_req_intr[1] = edn0_edn_req[1];
  assign edn0_edn_req_intr[2] = edn0_edn_req[2];
  assign edn0_edn_req_intr[4] = edn0_edn_req[4];
  // assign edn0_edn_req_intr[5] = edn0_edn_req[5];
  // assign edn0_edn_req_intr[6] = edn0_edn_req[6];
  assign edn0_edn_req_intr[7] = edn0_edn_req[7];

  assign edn0_edn_rsp[1] = edn0_edn_rsp_intr[1];
  assign edn0_edn_rsp[2] = edn0_edn_rsp_intr[2];
  assign edn0_edn_rsp[4] = edn0_edn_rsp_intr[4];
  // assign edn0_edn_rsp[5] = edn0_edn_rsp_intr[5];
  // assign edn0_edn_rsp[6] = edn0_edn_rsp_intr[6];
  assign edn0_edn_rsp[7] = edn0_edn_rsp_intr[7];

  hmac #(
    .AlertAsyncOn(1'b1)
  ) u_hmac (

      // Interrupt
      .intr_hmac_done_o  (intr_hmac_hmac_done_o),
      .intr_fifo_empty_o (intr_hmac_fifo_empty_o),
      .intr_hmac_err_o   (intr_hmac_hmac_err_o),
      // [0]: fatal_fault
      .alert_tx_o  ( alert_tx_o[0:0] ),
      .alert_rx_i  ( alert_rx_i[0:0] ),

      // Inter-module signals
      .idle_o(clkmgr_aon_idle[0]),
      .tl_i(hmac_tl_req),
      .tl_o(hmac_tl_rsp),

      // Clock and reset connections
      .clk_i,
      .rst_ni
  );
  
  kmac #(
    .AlertAsyncOn(2'b11),
    .EnMasking(KmacEnMasking),
    .SwKeyMasked(KmacSwKeyMasked),
    .SecCmdDelay(SecKmacCmdDelay),
    .SecIdleAcceptSwMsg(SecKmacIdleAcceptSwMsg),
    .RndCnstLfsrSeed(RndCnstKmacLfsrSeed),
    .RndCnstLfsrPerm(RndCnstKmacLfsrPerm),
    .RndCnstLfsrFwdPerm(RndCnstKmacLfsrFwdPerm),
    .RndCnstMsgPerm(RndCnstKmacMsgPerm)
  ) u_kmac (

      // Interrupt
      .intr_kmac_done_o  (intr_kmac_kmac_done_o),
      .intr_fifo_empty_o (intr_kmac_fifo_empty_o),
      .intr_kmac_err_o   (intr_kmac_kmac_err_o),
      // [1]: recov_operation_err
      // [2]: fatal_fault_err
      .alert_tx_o  ( alert_tx_o[2:1] ),
      .alert_rx_i  ( alert_rx_i[2:1] ),

      // Inter-module signals
      .keymgr_key_i(keymgr_kmac_key),
      .app_i(kmac_app_req),
      .app_o(kmac_app_rsp),
      .entropy_o(edn0_edn_req_intr[3]),
      .entropy_i(edn0_edn_rsp_intr[3]),
      // .entropy_o(edn0_edn_req_rot),
      // .entropy_i(edn0_edn_rsp_rot),
      .idle_o(clkmgr_aon_idle[1]),
      // .idle_o(clkmgr_aon_idle_rot),
      .en_masking_o(kmac_en_masking),
      .lc_escalate_en_i(lc_ctrl_lc_escalate_en),
      .tl_i(kmac_tl_req),
      .tl_o(kmac_tl_rsp),

      // Clock and reset connections
      .clk_i,
      .clk_edn_i,
      .rst_shadowed_ni,
      .rst_ni,
      .rst_edn_ni
  );
  
  keymgr #(
    .AlertAsyncOn(2'b11),
    .KmacEnMasking(KeymgrKmacEnMasking),
    .RndCnstLfsrSeed(RndCnstKeymgrLfsrSeed),
    .RndCnstLfsrPerm(RndCnstKeymgrLfsrPerm),
    .RndCnstRandPerm(RndCnstKeymgrRandPerm),
    .RndCnstRevisionSeed(RndCnstKeymgrRevisionSeed),
    .RndCnstCreatorIdentitySeed(RndCnstKeymgrCreatorIdentitySeed),
    .RndCnstOwnerIntIdentitySeed(RndCnstKeymgrOwnerIntIdentitySeed),
    .RndCnstOwnerIdentitySeed(RndCnstKeymgrOwnerIdentitySeed),
    .RndCnstSoftOutputSeed(RndCnstKeymgrSoftOutputSeed),
    .RndCnstHardOutputSeed(RndCnstKeymgrHardOutputSeed),
    .RndCnstAesSeed(RndCnstKeymgrAesSeed),
    .RndCnstKmacSeed(RndCnstKeymgrKmacSeed),
    .RndCnstOtbnSeed(RndCnstKeymgrOtbnSeed),
    .RndCnstCdi(RndCnstKeymgrCdi),
    .RndCnstNoneSeed(RndCnstKeymgrNoneSeed)
  ) u_keymgr (

      // Interrupt
      .intr_op_done_o (intr_keymgr_op_done_o),
      // [3]: recov_operation_err
      // [4]: fatal_fault_err
      .alert_tx_o  ( alert_tx_o[4:3] ),
      .alert_rx_i  ( alert_rx_i[4:3] ),

      // Inter-module signals
      .edn_o(edn0_edn_req_intr[0]),
      .edn_i(edn0_edn_rsp_intr[0]),
      .aes_key_o(keymgr_aes_key),
      .kmac_key_o(keymgr_kmac_key),
      .otbn_key_o(keymgr_otbn_key),
      .kmac_data_o(kmac_app_req[0]),
      .kmac_data_i(kmac_app_rsp[0]),
      .otp_key_i(otp_ctrl_otp_keymgr_key),
      .otp_device_id_i(keymgr_otp_device_id),
      .flash_i(flash_ctrl_keymgr),
      .lc_keymgr_en_i(lc_ctrl_lc_keymgr_en),
      .lc_keymgr_div_i(lc_ctrl_lc_keymgr_div),
      .rom_digest_i(rom_ctrl_keymgr_data),
      .kmac_en_masking_i(kmac_en_masking),
      .tl_i(keymgr_tl_req),
      .tl_o(keymgr_tl_rsp),

      // Clock and reset connections
      .clk_i,
      .clk_edn_i,
      .rst_shadowed_ni,
      .rst_ni,
      .rst_edn_ni
  );

  rom_ctrl #(
    .AlertAsyncOn(1'b1),
    .BootRomInitFile(RomCtrlBootRomInitFile),
    .RndCnstScrNonce(RndCnstRomCtrlScrNonce),
    .RndCnstScrKey(RndCnstRomCtrlScrKey),
    .SecDisableScrambling(SecRomCtrlDisableScrambling)
  ) u_rom_ctrl (
      // [5]: fatal
      .alert_tx_o  ( alert_tx_o[5:5] ),
      .alert_rx_i  ( alert_rx_i[5:5] ),

      // Inter-module signals
      .rom_cfg_i(ast_rom_cfg),
      .pwrmgr_data_o(rom_ctrl_pwrmgr_data),
      .keymgr_data_o(rom_ctrl_keymgr_data),
      .kmac_data_o(kmac_app_req[1]),
      .kmac_data_i(kmac_app_rsp[1]),
      // .kmac_data_o(kmac_app_req_rom),
      // .kmac_data_i(kmac_app_rsp_rom),
      .regs_tl_i(rom_ctrl_regs_tl_req),
      .regs_tl_o(rom_ctrl_regs_tl_rsp),
      .rom_tl_i(rom_ctrl_rom_tl_req),
      .rom_tl_o(rom_ctrl_rom_tl_rsp),

      // Clock and reset connections
      .clk_i,
      .rst_ni
  );

  edn #(
    .AlertAsyncOn(2'b11),
    .NumEndPoints(8)
  ) u_edn0 (

      // Interrupt
      .intr_edn_cmd_req_done_o (intr_edn0_edn_cmd_req_done_o),
      .intr_edn_fatal_err_o    (intr_edn0_edn_fatal_err_o),
      // [6]: recov_alert
      // [7]: fatal_alert
      .alert_tx_o  ( alert_tx_o[7:6] ),
      .alert_rx_i  ( alert_rx_i[7:6] ),

      // Inter-module signals
      .csrng_cmd_o(csrng_csrng_cmd_req[0]),
      .csrng_cmd_i(csrng_csrng_cmd_rsp[0]),
      .edn_i(edn0_edn_req_intr),
      .edn_o(edn0_edn_rsp_intr),
      .tl_i(edn0_tl_req),
      .tl_o(edn0_tl_rsp),

      // Clock and reset connections
      .clk_i,
      .rst_ni
  );

  csrng #(
    .AlertAsyncOn(2'b11),
    .RndCnstCsKeymgrDivNonProduction(RndCnstCsrngCsKeymgrDivNonProduction),
    .RndCnstCsKeymgrDivProduction(RndCnstCsrngCsKeymgrDivProduction),
    .SBoxImpl(CsrngSBoxImpl),
    .NHwApps(2)
  ) u_csrng (

      // Interrupt
      .intr_cs_cmd_req_done_o (intr_csrng_cs_cmd_req_done_o),
      .intr_cs_entropy_req_o  (intr_csrng_cs_entropy_req_o),
      .intr_cs_hw_inst_exc_o  (intr_csrng_cs_hw_inst_exc_o),
      .intr_cs_fatal_err_o    (intr_csrng_cs_fatal_err_o),
      // [8]: recov_alert
      // [9]: fatal_alert
      .alert_tx_o  ( alert_tx_o[9:8] ),
      .alert_rx_i  ( alert_rx_i[9:8] ),

      // Inter-module signals
      .csrng_cmd_i(csrng_csrng_cmd_req),
      .csrng_cmd_o(csrng_csrng_cmd_rsp),
      .entropy_src_hw_if_o(csrng_entropy_src_hw_if_req),
      .entropy_src_hw_if_i(csrng_entropy_src_hw_if_rsp),
      .cs_aes_halt_i(csrng_cs_aes_halt_req),
      .cs_aes_halt_o(csrng_cs_aes_halt_rsp),
      .otp_en_csrng_sw_app_read_i(csrng_otp_en_csrng_sw_app_read),
      .lc_hw_debug_en_i(lc_ctrl_lc_hw_debug_en),
      .tl_i(csrng_tl_req),
      .tl_o(csrng_tl_rsp),

      // Clock and reset connections
      .clk_i,
      .rst_ni
  );

  entropy_src #(
    .AlertAsyncOn(2'b11),
    .EsFifoDepth(EntropySrcEsFifoDepth),
    .Stub(EntropySrcStub)
  ) u_entropy_src (

      // Interrupt
      .intr_es_entropy_valid_o      (intr_entropy_src_es_entropy_valid_o),
      .intr_es_health_test_failed_o (intr_entropy_src_es_health_test_failed_o),
      .intr_es_observe_fifo_ready_o (intr_entropy_src_es_observe_fifo_ready_o),
      .intr_es_fatal_err_o          (intr_entropy_src_es_fatal_err_o),
      // [10]: recov_alert
      // [11]: fatal_alert
      .alert_tx_o  ( alert_tx_o[11:10] ),
      .alert_rx_i  ( alert_rx_i[11:10] ),

      // Inter-module signals
      .entropy_src_hw_if_i(csrng_entropy_src_hw_if_req),
      .entropy_src_hw_if_o(csrng_entropy_src_hw_if_rsp),
      .cs_aes_halt_o(csrng_cs_aes_halt_req),
      .cs_aes_halt_i(csrng_cs_aes_halt_rsp),
      .entropy_src_rng_o(es_rng_req_o),
      .entropy_src_rng_i(es_rng_rsp_i),
      .entropy_src_xht_o(),
      .entropy_src_xht_i(entropy_src_pkg::ENTROPY_SRC_XHT_RSP_DEFAULT),
      .otp_en_entropy_src_fw_read_i(entropy_src_otp_en_entropy_src_fw_read),
      .otp_en_entropy_src_fw_over_i(entropy_src_otp_en_entropy_src_fw_over),
      .rng_fips_o(es_rng_fips_o),
      .tl_i(entropy_src_tl_req),
      .tl_o(entropy_src_tl_rsp),

      // Clock and reset connections
      .clk_i,
      .rst_ni
  );

  otbn #(
    .AlertAsyncOn(2'b11),
    .Stub(OtbnStub),
    .RegFile(OtbnRegFile),
    .RndCnstUrndPrngSeed(RndCnstOtbnUrndPrngSeed),
    .SecMuteUrnd(SecOtbnMuteUrnd),
    .SecSkipUrndReseedAtStart(SecOtbnSkipUrndReseedAtStart),
    .RndCnstOtbnKey(RndCnstOtbnOtbnKey),
    .RndCnstOtbnNonce(RndCnstOtbnOtbnNonce)
  ) u_otbn (

      // Interrupt
      .intr_done_o (intr_otbn_done),
      // [12]: fatal
      // [13]: recov
      .alert_tx_o  ( alert_tx_o[13:12] ),
      .alert_rx_i  ( alert_rx_i[13:12] ),

      // Inter-module signals
      .otbn_otp_key_o(otp_ctrl_otbn_otp_key_req),
      .otbn_otp_key_i(otp_ctrl_otbn_otp_key_rsp),
      .edn_rnd_o(edn0_edn_req_intr[5]),
      .edn_rnd_i(edn0_edn_rsp_intr[5]),
      .edn_urnd_o(edn0_edn_req_intr[6]),
      .edn_urnd_i(edn0_edn_rsp_intr[6]),
      .idle_o(clkmgr_aon_idle[2]),
      .ram_cfg_i(ast_ram_1p_cfg),
      .lc_escalate_en_i(lc_ctrl_lc_escalate_en),
      .lc_rma_req_i(flash_ctrl_rma_ack),
      .lc_rma_ack_o(otbn_lc_rma_ack),
      .keymgr_key_i(keymgr_otbn_key),
      .tl_i(otbn_tl_req),
      .tl_o(otbn_tl_rsp),

      // Clock and reset connections
      .clk_i (clk_i),
      .clk_edn_i (clk_edn_i),
      .clk_otp_i (clk_i),
      .rst_ni (rst_ni),
      .rst_edn_ni (rst_edn_ni),
      .rst_otp_ni (rst_ni)
  );

  otbn_otp_controller u_otbn_otp_ctrl (
    .clk(clk_i),
    .rst_n(rst_ni),
    .req(otp_ctrl_otbn_otp_key_req),
    .rsp(otp_ctrl_otbn_otp_key_rsp)
  );

  sm3 u_sm3 (

      // Inter-module signals
      .tl_i(sm3_tl_req),
      .tl_o(sm3_tl_rsp),

      // Clock and reset connections
      .clk_i (clk_i),
      .rst_ni (rst_ni)
  );
  sm4 u_sm4 (

      // Inter-module signals
      .tl_i(sm4_tl_req),
      .tl_o(sm4_tl_rsp),

      // Clock and reset connections
      .clk_i (clk_i),
      .rst_ni (rst_ni)
  );

  rs_encode u_rs_encode (

      // Inter-module signals
      .tl_i(rs_encode_tl_req),
      .tl_o(rs_encode_tl_rsp),

      // Clock and reset connections
      .clk_i (clk_i),
      .rst_ni (rst_ni)
  );
  rs_decode u_rs_decode (

      // Inter-module signals
      .tl_i(rs_decode_tl_req),
      .tl_o(rs_decode_tl_rsp),

      // Clock and reset connections
      .clk_i (clk_i),
      .rst_ni (rst_ni)
  );
  puf u_puf (

      // Inter-module signals
      .tl_i(puf_tl_req),
      .tl_o(puf_tl_rsp),

      // Clock and reset connections
      .clk_i (clk_i),
      .rst_ni (rst_ni)
  );

  xbar_main_rot u_xbar_main (
    .clk_i,
    .rst_ni,

    // port: tl_rv_core_ibex__corei
    .tl_host_i(tl_i),
    .tl_host_o(tl_o),

    // // port: tl_rom_ctrl__rom
    // .tl_rom_ctrl__rom_o(rom_ctrl_rom_tl_req),
    // .tl_rom_ctrl__rom_i(rom_ctrl_rom_tl_rsp),

    // port: tl_rom_ctrl__regs
    .tl_rom_ctrl__regs_o(rom_ctrl_regs_tl_req),
    .tl_rom_ctrl__regs_i(rom_ctrl_regs_tl_rsp),

    // port: tl_hmac
    .tl_hmac_o(hmac_tl_req),
    .tl_hmac_i(hmac_tl_rsp),

    // port: tl_kmac
    .tl_kmac_o(kmac_tl_req),
    .tl_kmac_i(kmac_tl_rsp),

    // port: tl_keymgr
    .tl_keymgr_o(keymgr_tl_req),
    .tl_keymgr_i(keymgr_tl_rsp),

    // port: tl_entropy_src
    .tl_entropy_src_o(entropy_src_tl_req),
    .tl_entropy_src_i(entropy_src_tl_rsp),

    // // port: tl_csrng
    .tl_csrng_o(csrng_tl_req),
    .tl_csrng_i(csrng_tl_rsp),

    // port: tl_edn0
    .tl_edn0_o(edn0_tl_req),
    .tl_edn0_i(edn0_tl_rsp),
    
    // port: tl_otbn
    .tl_otbn_o(otbn_tl_req),
    .tl_otbn_i(otbn_tl_rsp),

    // port: tl_sm3
    .tl_sm3_o(sm3_tl_req),
    .tl_sm3_i(sm3_tl_rsp),

    // port: tl_sm4
    .tl_sm4_o(sm4_tl_req),
    .tl_sm4_i(sm4_tl_rsp),

     // port: tl_rs_encode
    .tl_rs_encode_o(rs_encode_tl_req),
    .tl_rs_encode_i(rs_encode_tl_rsp),

    // port: tl_rs_decode
    .tl_rs_decode_o(rs_decode_tl_req),
    .tl_rs_decode_i(rs_decode_tl_rsp),

    // port: tl_puf
    .tl_puf_o(puf_tl_req),
    .tl_puf_i(puf_tl_rsp)

  );
    

endmodule

