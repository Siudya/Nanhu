/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package top

import axi2tl.{AXI2TLParam, AXI2TLParamKey}
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import system._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, XLen}
import xiangshan.frontend.icache.ICacheParameters
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.tile.MaxHartIdBits
import xiangshan.cache.DCacheParameters
import xiangshan.cache.mmu.{L2TLBParameters, TLBParameters}
import xiangshan.backend.execute.exublock.ExuParameters
import device.{EnableJtag, XSDebugModuleParams}
import huancun._
import coupledL2._
import xs.utils.perf.DebugOptions
import xiangshan.mem.prefetch.SMSParams
import darecreek.exu.vfu._
import xs.utils.perf.DebugOptionsKey
case object PrefixKey extends Field[String]
class BaseConfig(n: Int, mbist:Boolean = false) extends Config((site, here, up) => {
  case XLen => 64
  case DebugOptionsKey => DebugOptions()
  case SoCParamsKey => SoCParameters(
    hasShareBus = mbist, hasMbist = mbist
  )
  case PMParameKey => PMParameters()
  case XSTileKey => Seq.tabulate(n){
    i => XSCoreParameters(HartId = i, hasMbist = mbist, hasShareBus = mbist,
    prefetcher = Some(SMSParams()))
  }
  case ExportDebug => DebugAttachParams(protocols = Set(JTAG))
  case DebugModuleKey => Some(XSDebugModuleParams(site(XLen)))
  case JtagDTMKey => JtagDTMKey
  case MaxHartIdBits => 2
  case EnableJtag => true.B
  case PrefixKey => ""

  case VFuParamsKey => VFuParameters()
  case AXI2TLParamKey => AXI2TLParam()
})

// Synthesizable minimal XiangShan
// * It is still an out-of-order, super-scalaer arch
// * L1 cache included
// * L2 cache NOT included
// * L3 cache included
class MinimalConfig(n: Int = 1) extends Config(
  new BaseConfig(n).alter((site, here, up) => {
    case XSTileKey => up(XSTileKey).map(
      _.copy(
        DecodeWidth = 2,
        RenameWidth = 2,
        FetchWidth = 4,
        NRPhyRegs = 64,
        LoadQueueSize = 16,
        LoadQueueNWriteBanks = 4,
        StoreQueueSize = 12,
        StoreQueueNWriteBanks = 4,
        RobSize = 32,
        FtqSize = 8,
        IBufSize = 16,
        StoreBufferSize = 4,
        StoreBufferThreshold = 3,
        exuParameters = ExuParameters(),
        prefetcher = None,
        icacheParameters = ICacheParameters(
          nSets = 64, // 16KB ICache
          tagECC = Some("parity"),
          dataECC = Some("parity"),
          replacer = Some("setplru"),
          nMissEntries = 2,
          nReleaseEntries = 1,
          nProbeEntries = 2,
          nPrefetchEntries = 2,
          hasPrefetch = false
        ),
        dcacheParametersOpt = Some(DCacheParameters(
          nSets = 64, // 32KB DCache
          nWays = 8,
          tagECC = Some("secded"),
          dataECC = Some("secded"),
          replacer = Some("setplru"),
          nMissEntries = 4,
          nProbeEntries = 4,
          nReleaseEntries = 8,
        )),
        EnableBPD = false, // disable TAGE
        EnableLoop = false,
        itlbParameters = TLBParameters(
          name = "itlb",
          fetchi = true,
          useDmode = false,
          sameCycle = false,
          missSameCycle = true,
          normalReplacer = Some("plru"),
          superReplacer = Some("plru"),
          normalNWays = 4,
          normalNSets = 1,
          superNWays = 2,
          shouldBlock = true
        ),
        ldtlbParameters = TLBParameters(
          name = "ldtlb",
          normalNSets = 16, // 6when da or sa
          normalNWays = 1, // when fa or sa
          normalAssociative = "sa",
          normalReplacer = Some("setplru"),
          superNWays = 4,
          normalAsVictim = true,
          partialStaticPMP = true,
          outReplace = false
        ),
        sttlbParameters = TLBParameters(
          name = "sttlb",
          normalNSets = 16, // when da or sa
          normalNWays = 1, // when fa or sa
          normalAssociative = "sa",
          normalReplacer = Some("setplru"),
          normalAsVictim = true,
          superNWays = 4,
          partialStaticPMP = true,
          outReplace = false
        ),
        btlbParameters = TLBParameters(
          name = "btlb",
          normalNSets = 1,
          normalNWays = 8,
          superNWays = 2
        ),
        l2tlbParameters = L2TLBParameters(
          l1Size = 4,
          l2nSets = 4,
          l2nWays = 4,
          l3nSets = 4,
          l3nWays = 8,
          spSize = 2,
        ),
        // L2CacheParamsOpt = None // remove L2 Cache
        // L2CacheParamsOpt = Some(L2Param(
        //   name = "L2",
        //   ways = 8,
        //   sets = 128,
        //   echoField = Seq(huancun.DirtyField()),
        //   prefetch = None
        // )),
        // L2NBanks = 2,
        // prefetcher = None // if L2 pf_recv_node does not exist, disable SMS prefetcher
      )
    )
    case SoCParamsKey =>
      val tiles = site(XSTileKey)
      up(SoCParamsKey).copy(
        L3CacheParamsOpt = Some(up(SoCParamsKey).L3CacheParamsOpt.get.copy(
          // sets = 1024,
          // inclusive = false,
          // clientCaches = tiles.map{ p =>
          //   CacheParameters(
          //     "dcache",
          //     sets = 2 * p.dcacheParametersOpt.get.nSets,
          //     ways = p.dcacheParametersOpt.get.nWays + 2,
          //     blockGranularity = log2Ceil(2 * p.dcacheParametersOpt.get.nSets),
          //     aliasBitsOpt = None
          //   )
          // },
          // simulation = !site(DebugOptionsKey).FPGAPlatform
        )),
        L3NBanks = 1
      )
  })
)

// Non-synthesizable MinimalConfig, for fast simulation only
class MinimalSimConfig(n: Int = 1) extends Config(
  new MinimalConfig(n).alter((site, here, up) => {
    case XSTileKey => up(XSTileKey).map(_.copy(
      dcacheParametersOpt = None,
      softPTW = true
    ))
    case SoCParamsKey => up(SoCParamsKey).copy(
      L3CacheParamsOpt = None
    )
  })
)

class WithNKBL1D(n: Int, ways: Int = 4) extends Config((site, here, up) => {
  case XSTileKey =>
    val sets = n * 1024 / ways / 64
    up(XSTileKey).map(_.copy(
      dcacheParametersOpt = Some(DCacheParameters(
        nSets = sets,
        nWays = ways,
        tagECC = Some("secded"),
        dataECC = Some("secded"),
        replacer = Some("setplru"),
        nMissEntries = 16,
        nProbeEntries = 8,
        nReleaseEntries = 18
      ))
    ))
})

class WithNKBL2
(
  n: Int,
  ways: Int = 8,
  inclusive: Boolean = true,
  banks: Int = 1,
  alwaysReleaseData: Boolean = false
) extends Config((site, here, up) => {
  case XSTileKey =>
    val upParams = up(XSTileKey)
    val l2sets = n * 1024 / banks / ways / 64
    upParams.map(p => p.copy(
      L2CacheParamsOpt = Some(L2Param(
        name = "L2",
        // level = 2,
        ways = ways,
        sets = l2sets,
        // inclusive = inclusive,
        // alwaysReleaseData = alwaysReleaseData,
        // clientCaches = Seq(CacheParameters(
        //   "dcache",
        //   sets = 2 * p.dcacheParametersOpt.get.nSets / banks,
        //   ways = p.dcacheParametersOpt.get.nWays + 2,
        //   blockGranularity = log2Ceil(2 * p.dcacheParametersOpt.get.nSets / banks),
        //   aliasBitsOpt = p.dcacheParametersOpt.get.aliasBitsOpt
        // )),
        clientCaches = Seq(L1Param(
          "dcache",
          sets = p.dcacheParametersOpt.get.nSets / banks,
          ways = p.dcacheParametersOpt.get.nWays,
          aliasBitsOpt = p.dcacheParametersOpt.get.aliasBitsOpt
        )),
        reqField = Seq(xs.utils.tl.ReqSourceField()),
        echoField = Seq(coupledL2.DirtyField()),
        elaboratedTopDown = false,
        enablePerf = false,
        hasMbist = p.hasMbist,
        hasShareBus = p.hasShareBus,
        prefetch = Some(coupledL2.prefetch.HyperPrefetchParams()),
        /*
        del L2 prefetche recv option, move into: prefetch =  PrefetchReceiverParams
        prefetch options:
          SPPParameters          => spp only
          BOPParameters          => bop only
          PrefetchReceiverParams => sms+bop
          HyperPrefetchParams    => spp+bop+sms
        */
        // sppMultiLevelRefill = Some(coupledL2.prefetch.PrefetchReceiverParams()),
        /*must has spp, otherwise Assert Fail
        sppMultiLevelRefill options:
        PrefetchReceiverParams() => spp has cross level refill
        None                     => spp only refill L2
        */
        // prefetch = None
        // enablePerf = true,
        // sramDepthDiv = 2,
        // tagECC = None,
        // dataECC = None,
        // hasShareBus = false,
        // simulation = !site(DebugOptionsKey).FPGAPlatform
      )),
      L2NBanks = banks
    ))
})

class WithNKBL3(n: Int, ways: Int = 8, inclusive: Boolean = true, banks: Int = 1, core_num: Int = 1) extends Config((site, here, up) => {
  case SoCParamsKey =>
    val sets = n * 1024 / banks / ways / 64
    val tiles = site(XSTileKey)
    val clientDirBytes = tiles.map{ t =>
      t.L2NBanks * t.L2CacheParamsOpt.map(_.toCacheParams.capacity).getOrElse(0)
    }.sum
    up(SoCParamsKey).copy(
      L3NBanks = banks,
      L3CacheParamsOpt = Some(HCCacheParameters(
        name = "L3",
        level = 3,
        ways = ways,
        sets = sets,
        inclusive = inclusive,
        clientCaches = tiles.map{ core =>
          val l2params = core.L2CacheParamsOpt.get.toCacheParams
          l2params.copy(sets = 2 * clientDirBytes / core.L2NBanks / l2params.ways / 64, ways = l2params.ways + 2)
        },
        enablePerf = false,
        prefetch = None,
        // prefetchRecv = Some(huancun.prefetch.PrefetchReceiverParams()),
        ctrl = Some(huancun.CacheCtrl(
          address = 0x39000000,
          beatBytes = 8,
          numCores = up(XSTileKey).length
        )),
        reqField = Seq(xs.utils.tl.ReqSourceField()),
        sramClkDivBy2 = true,
        sramDepthDiv = 8,
        hasMbist = up(SoCParamsKey).hasMbist,
        hasShareBus = up(SoCParamsKey).hasShareBus,
        tagECC = Some("secded"),
        dataECC = Some("secded"),
        simulation = !site(DebugOptionsKey).FPGAPlatform
      ))
    )
})

class WithL3DebugConfig extends Config(
  new WithNKBL3(256, inclusive = false) ++ new WithNKBL2(64)
)

class MinimalL3DebugConfig(n: Int = 1) extends Config(
  new WithL3DebugConfig ++ new MinimalConfig(n)
)

class DefaultL3DebugConfig(n: Int = 1) extends Config(
  new WithL3DebugConfig ++ new BaseConfig(n)
)

class MediumConfig(n: Int = 1) extends Config(
  new WithNKBL3(4096, inclusive = false, banks = 4)
    ++ new WithNKBL2(512, inclusive = false, alwaysReleaseData = true)
    ++ new WithNKBL1D(128)
    ++ new BaseConfig(n)
)

class DefaultConfig(n: Int = 1) extends Config(
  new WithNKBL3(1024, inclusive = false, banks = 1, ways = 8, core_num = n)
    ++ new WithNKBL2(128, inclusive = false, banks = 1, ways = 8, alwaysReleaseData = true)
    ++ new WithNKBL1D(32)
    ++ new BaseConfig(n)
)

