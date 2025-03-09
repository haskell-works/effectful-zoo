module Effectful.Zoo.Blockfrost.Api
  ( Blockfrost,

    AccountDelegation(..),
    AccountHistory(..),
    AccountInfo(..),
    AccountMir(..),
    AccountRegistration(..),
    AccountRegistrationAction(..),
    AccountReward(..),
    AccountWithdrawal(..),
    Address(..),
    AddressAssociated(..),
    AddressAssociatedTotal(..),
    AddressDetails(..),
    AddressInfo(..),
    AddressInfoExtended(..),
    AddressTransaction(..),
    AddressType(..),
    AddressUtxo(..),
    Amount(..),
    AssetAction(..),
    AssetAddress(..),
    AssetDetails(..),
    AssetHistory(..),
    AssetId(..),
    AssetInfo(..),
    AssetMetadata(..),
    AssetOnChainMetadata(..),
    AssetTransaction(..),
    Block(..),
    BlockfrostError(..),
    BlockHash(..),
    CBORString(..),
    CostModels(..),
    DatumHash(..),
    DerivedAddress(..),
    Epoch(..),
    EpochInfo(..),
    EpochLength(..),
    Genesis(..),
    Healthy(..),
    InlineDatum(..),
    IPFSAdd(..),
    IPFSData(..),
    IPFSPin(..),
    IPFSPinChange(..),
    Lovelaces,
    MetadataMediaFile(..),
    Metric(..),
    Network(..),
    NetworkEraBound(..),
    NetworkEraParameters(..),
    NetworkEraSummary(..),
    NetworkStake(..),
    NetworkSupply(..),
    NutlinkAddress(..),
    NutlinkAddressTicker(..),
    NutlinkTicker(..),
    Paged(..),
    PinState(..),
    PolicyId,
    Pool(..),
    PoolDelegator(..),
    PoolEpoch(..),
    PoolHistory(..),
    PoolId(..),
    PoolInfo(..),
    PoolMetadata(..),
    PoolRegistrationAction(..),
    PoolRelay(..),
    PoolStakeDistribution(..),
    PoolUpdate(..),
    PoolUpdateMetadata(..),
    POSIXMillis,
    Pot(..),
    Project(..),
    ProtocolParams(..),
    Quantity(..),
    RewardType(..),
    Script(..),
    ScriptCBOR(..),
    ScriptDatum(..),
    ScriptDatumCBOR(..),
    ScriptHash(..),
    ScriptHashList(..),
    ScriptJSON(..),
    ScriptRedeemer(..),
    ScriptType(..),
    ServerTime(..),
    Slot(..),
    SortOrder(..),
    StakeDistribution(..),
    ToLower,
    Transaction(..),
    TransactionDelegation(..),
    TransactionMetaCBOR(..),
    TransactionMetaJSON(..),
    TransactionMir(..),
    TransactionPoolRetiring(..),
    TransactionPoolUpdate(..),
    TransactionRedeemer(..),
    TransactionStake(..),
    TransactionUtxos(..),
    TransactionWithdrawal(..),
    TxEval(..),
    TxEvalBudget(..),
    TxEvalInput(..),
    TxHash(..),
    TxMeta,
    TxMetaCBOR(..),
    TxMetaJSON(..),
    URLVersion(..),
    UtxoInput(..),
    UtxoOutput(..),
    ValidationPurpose(..),
    mkAddress,
    unAddress,
    mkAssetId,
    unAssetId,
    mkBlockHash,
    unBlockHash,
    unEpoch,
    unEpochLength,
    mkPolicyId,
    unPolicyId,
    mkPoolId,
    unPoolId,
    millisecondsToPosix,
    posixToMilliseconds,
    unQuantity,
    unSlot,
    runBlockfrost,
    -- Client
    getRoot,
    getHealth,
    getClock,
    getMetrics,
    getMetricsEndpoints,
    -- Client.NutLink
    nutlinkListAddress,
    nutlinkListAddressTickers',
    nutlinkListAddressTickers,
    nutlinkAddressTickers',
    nutlinkAddressTickers,
    nutlinkTickers',
    nutlinkTickers,
    -- Client.IPFS
    ipfsGateway,
    ipfsPin,
    ipfsListPins',
    ipfsListPins,
    ipfsGetPin,
    ipfsRemovePin,
    -- Client.Cardano.Blocks
    getLatestBlock,
    getLatestBlockTxs',
    getLatestBlockTxs,
    getBlock,
    getBlockSlot,
    getBlockEpochSlot,
    getNextBlocks',
    getNextBlocks,
    getPreviousBlocks',
    getPreviousBlocks,
    getBlockTxs',
    getBlockTxs,
    getBlockAffectedAddresses',
    getBlockAffectedAddresses,
    -- Client.Cardano.Network
    getNetworkInfo,
    getNetworkEras,
    -- Client.Cardano.Addresses
    getAddressInfo,
    getAddressInfoExtended,
    getAddressDetails,
    getAddressUtxos',
    getAddressUtxos,
    getAddressUtxosAsset',
    getAddressUtxosAsset,
    getAddressTransactions',
    getAddressTransactions,
    -- Client.Cardano.Assets
    getAssets',
    getAssets,
    getAssetDetails,
    getAssetHistory',
    getAssetHistory,
    getAssetTransactions',
    getAssetTransactions,
    getAssetAddresses',
    getAssetAddresses,
    getAssetsByPolicy',
    getAssetsByPolicy,
    -- Client.Cardano.Scripts
    listScripts',
    listScripts,
    getScript,
    getScriptRedeemers',
    getScriptRedeemers,
    getScriptDatum,
    getScriptDatumCBOR,
    getScriptJSON,
    getScriptCBOR,
    -- Client.Cardano.Epochs
    getLatestEpoch,
    getLatestEpochProtocolParams,
    getEpoch,
    getNextEpochs',
    getNextEpochs,
    getPreviousEpochs',
    getPreviousEpochs,
    getEpochStake',
    getEpochStake,
    getEpochStakeByPool',
    getEpochStakeByPool,
    getEpochBlocks',
    getEpochBlocks,
    getEpochBlocksByPool',
    getEpochBlocksByPool,
    getEpochProtocolParams,
    -- Client.Cardano.Transactions
    getTx,
    getTxUtxos,
    getTxRedeemers,
    getTxStakes,
    getTxDelegations,
    getTxWithdrawals,
    getTxMirs,
    getTxPoolUpdates,
    getTxPoolRetiring,
    getTxMetadataJSON,
    getTxMetadataCBOR,
    submitTx,
    -- Client.Cardano.Ledger
    getLedgerGenesis,
    -- Client.Cardano.Mempool
    getMempoolTransactions,
    -- Client.Cardano.Accounts
    getAccount,
    getAccountRewards',
    getAccountRewards,
    getAccountHistory',
    getAccountHistory,
    getAccountDelegations',
    getAccountDelegations,
    getAccountRegistrations',
    getAccountRegistrations,
    getAccountWithdrawals',
    getAccountWithdrawals,
    getAccountMirs',
    getAccountMirs,
    getAccountAssociatedAddresses',
    getAccountAssociatedAddresses,
    getAccountAssociatedAddressesTotal  ,
    getAccountAssociatedAssets',
    getAccountAssociatedAssets,
    -- Client.Cardano.Pools
    listPools',
    listPools,
    listPoolsExtended',
    listPoolsExtended,
    listRetiredPools',
    listRetiredPools,
    listRetiringPools',
    listRetiringPools,
    getPool,
    getPoolHistory',
    getPoolHistory,
    getPoolMetadata,
    getPoolRelays,
    getPoolDelegators',
    getPoolDelegators,
    getPoolBlocks',
    getPoolBlocks,
    getPoolUpdates',
    getPoolUpdates,
    -- Client.Cardano.Metadata
    getTxMetadataLabels',
    getTxMetadataLabels,
    getTxMetadataByLabelJSON',
    getTxMetadataByLabelJSON,
    getTxMetadataByLabelCBOR',
    getTxMetadataByLabelCBOR,
  ) where

import Blockfrost.Client (BlockfrostError (..), Paged (..), Project (..), SortOrder (..))
import Blockfrost.Types.Cardano.Accounts
import Blockfrost.Types.Cardano.Addresses
import Blockfrost.Types.Cardano.Assets
import Blockfrost.Types.Cardano.Blocks
import Blockfrost.Types.Cardano.Epochs
import Blockfrost.Types.Cardano.Genesis
import Blockfrost.Types.Cardano.Metadata
import Blockfrost.Types.Cardano.Network
import Blockfrost.Types.Cardano.Pools
import Blockfrost.Types.Cardano.Scripts
import Blockfrost.Types.Cardano.Transactions
import Blockfrost.Types.Cardano.Utils
import Blockfrost.Types.Common
import Blockfrost.Types.IPFS
import Blockfrost.Types.NutLink
import Blockfrost.Types.Shared.Ada
import Blockfrost.Types.Shared.Address
import Blockfrost.Types.Shared.Amount
import Blockfrost.Types.Shared.AssetId
import Blockfrost.Types.Shared.BlockHash
import Blockfrost.Types.Shared.BlockIndex
import Blockfrost.Types.Shared.CBOR
import Blockfrost.Types.Shared.DatumHash
import Blockfrost.Types.Shared.Epoch
import Blockfrost.Types.Shared.Opts
import Blockfrost.Types.Shared.POSIXMillis
import Blockfrost.Types.Shared.PolicyId
import Blockfrost.Types.Shared.PoolId
import Blockfrost.Types.Shared.Quantity
import Blockfrost.Types.Shared.ScriptHash
import Blockfrost.Types.Shared.Slot
import Blockfrost.Types.Shared.TxHash
import Blockfrost.Types.Shared.ValidationPurpose
import Control.Monad
import Data.Either
import Data.Maybe
import Data.Text
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Blockfrost.Dynamic (Blockfrost, runBlockfrost)
import Effectful.Zoo.Blockfrost.Dynamic qualified as BF
import Effectful.Zoo.Core
import Effectful.Zoo.Error.Static
import HaskellWorks.Prelude

-- Client
getRoot                             :: r <: Blockfrost => r <: Error BlockfrostError => Eff r URLVersion
getHealth                           :: r <: Blockfrost => r <: Error BlockfrostError => Eff r Healthy
getClock                            :: r <: Blockfrost => r <: Error BlockfrostError => Eff r ServerTime
getMetrics                          :: r <: Blockfrost => r <: Error BlockfrostError => Eff r [Metric]
getMetricsEndpoints                 :: r <: Blockfrost => r <: Error BlockfrostError => Eff r [(Text, Metric)]
-- Client.NutLink
nutlinkListAddress                  :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Eff r NutlinkAddress
nutlinkListAddressTickers'          :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Paged -> SortOrder -> Eff r [NutlinkAddressTicker]
nutlinkListAddressTickers           :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Eff r [NutlinkAddressTicker]
nutlinkAddressTickers'              :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Text -> Paged -> SortOrder -> Eff r [NutlinkTicker]
nutlinkAddressTickers               :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Text -> Eff r [NutlinkTicker]
nutlinkTickers'                     :: r <: Blockfrost => r <: Error BlockfrostError => Text -> Paged -> SortOrder -> Eff r [(Address, NutlinkTicker)]
nutlinkTickers                      :: r <: Blockfrost => r <: Error BlockfrostError => Text -> Eff r [(Address, NutlinkTicker)]
-- Client.IPFS
ipfsGateway                         :: r <: Blockfrost => r <: Error BlockfrostError => Text -> Eff r IPFSData
ipfsPin                             :: r <: Blockfrost => r <: Error BlockfrostError => Text -> Eff r IPFSPinChange
ipfsListPins'                       :: r <: Blockfrost => r <: Error BlockfrostError => Paged -> SortOrder -> Eff r [IPFSPin]
ipfsListPins                        :: r <: Blockfrost => r <: Error BlockfrostError => Eff r [IPFSPin]
ipfsGetPin                          :: r <: Blockfrost => r <: Error BlockfrostError => Text -> Eff r IPFSPin
ipfsRemovePin                       :: r <: Blockfrost => r <: Error BlockfrostError => Text -> Eff r IPFSPinChange
-- Client.Cardano.Blocks
getLatestBlock                      :: r <: Blockfrost => r <: Error BlockfrostError => Eff r Block
getLatestBlockTxs'                  :: r <: Blockfrost => r <: Error BlockfrostError => Paged -> SortOrder -> Eff r [TxHash]
getLatestBlockTxs                   :: r <: Blockfrost => r <: Error BlockfrostError => Eff r [TxHash]
getBlock                            :: r <: Blockfrost => r <: Error BlockfrostError => Either Integer BlockHash -> Eff r Block
getBlockSlot                        :: r <: Blockfrost => r <: Error BlockfrostError => Slot -> Eff r Block
getBlockEpochSlot                   :: r <: Blockfrost => r <: Error BlockfrostError => Epoch -> Slot -> Eff r Block
getNextBlocks'                      :: r <: Blockfrost => r <: Error BlockfrostError => Either Integer BlockHash -> Paged -> Eff r [Block]
getNextBlocks                       :: r <: Blockfrost => r <: Error BlockfrostError => Either Integer BlockHash -> Eff r [Block]
getPreviousBlocks'                  :: r <: Blockfrost => r <: Error BlockfrostError => Either Integer BlockHash -> Paged -> Eff r [Block]
getPreviousBlocks                   :: r <: Blockfrost => r <: Error BlockfrostError => Either Integer BlockHash -> Eff r [Block]
getBlockTxs'                        :: r <: Blockfrost => r <: Error BlockfrostError => Either Integer BlockHash -> Paged -> SortOrder -> Eff r [TxHash]
getBlockTxs                         :: r <: Blockfrost => r <: Error BlockfrostError => Either Integer BlockHash -> Eff r [TxHash]
getBlockAffectedAddresses'          :: r <: Blockfrost => r <: Error BlockfrostError => Either Integer BlockHash -> Paged -> Eff r [(Address, [TxHash])]
getBlockAffectedAddresses           :: r <: Blockfrost => r <: Error BlockfrostError => Either Integer BlockHash -> Eff r [(Address, [TxHash])]
-- Client.Cardano.Network
getNetworkInfo                      :: r <: Blockfrost => r <: Error BlockfrostError => Eff r Network
getNetworkEras                      :: r <: Blockfrost => r <: Error BlockfrostError => Eff r [NetworkEraSummary]
-- Client.Cardano.Addresses
getAddressInfo                      :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Eff r AddressInfo
getAddressInfoExtended              :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Eff r AddressInfoExtended
getAddressDetails                   :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Eff r AddressDetails
getAddressUtxos'                    :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Paged -> SortOrder -> Eff r [AddressUtxo]
getAddressUtxos                     :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Eff r [AddressUtxo]
getAddressUtxosAsset'               :: r <: Blockfrost => r <: Error BlockfrostError => Address -> AssetId -> Paged -> SortOrder -> Eff r [AddressUtxo]
getAddressUtxosAsset                :: r <: Blockfrost => r <: Error BlockfrostError => Address -> AssetId -> Eff r [AddressUtxo]
getAddressTransactions'             :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Paged -> SortOrder -> Maybe BlockIndex -> Maybe BlockIndex -> Eff r [AddressTransaction]
getAddressTransactions              :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Eff r [AddressTransaction]
-- Client.Cardano.Assets
getAssets'                          :: r <: Blockfrost => r <: Error BlockfrostError => Paged -> SortOrder -> Eff r [AssetInfo]
getAssets                           :: r <: Blockfrost => r <: Error BlockfrostError => Eff r [AssetInfo]
getAssetDetails                     :: r <: Blockfrost => r <: Error BlockfrostError => AssetId -> Eff r AssetDetails
getAssetHistory'                    :: r <: Blockfrost => r <: Error BlockfrostError => AssetId -> Paged -> SortOrder -> Eff r [AssetHistory]
getAssetHistory                     :: r <: Blockfrost => r <: Error BlockfrostError => AssetId -> Eff r [AssetHistory]
getAssetTransactions'               :: r <: Blockfrost => r <: Error BlockfrostError => AssetId -> Paged -> SortOrder -> Eff r [AssetTransaction]
getAssetTransactions                :: r <: Blockfrost => r <: Error BlockfrostError => AssetId -> Eff r [AssetTransaction]
getAssetAddresses'                  :: r <: Blockfrost => r <: Error BlockfrostError => AssetId -> Paged -> SortOrder -> Eff r [AssetAddress]
getAssetAddresses                   :: r <: Blockfrost => r <: Error BlockfrostError => AssetId -> Eff r [AssetAddress]
getAssetsByPolicy'                  :: r <: Blockfrost => r <: Error BlockfrostError => PolicyId -> Paged -> SortOrder -> Eff r [AssetInfo]
getAssetsByPolicy                   :: r <: Blockfrost => r <: Error BlockfrostError => PolicyId -> Eff r [AssetInfo]
-- Client.Cardano.Scripts
listScripts'                        :: r <: Blockfrost => r <: Error BlockfrostError => Paged -> SortOrder -> Eff r ScriptHashList
listScripts                         :: r <: Blockfrost => r <: Error BlockfrostError => Eff r ScriptHashList
getScript                           :: r <: Blockfrost => r <: Error BlockfrostError => ScriptHash -> Eff r Script
getScriptRedeemers'                 :: r <: Blockfrost => r <: Error BlockfrostError => ScriptHash -> Paged -> SortOrder -> Eff r [ScriptRedeemer]
getScriptRedeemers                  :: r <: Blockfrost => r <: Error BlockfrostError => ScriptHash -> Eff r [ScriptRedeemer]
getScriptDatum                      :: r <: Blockfrost => r <: Error BlockfrostError => DatumHash -> Eff r ScriptDatum
getScriptDatumCBOR                  :: r <: Blockfrost => r <: Error BlockfrostError => DatumHash -> Eff r ScriptDatumCBOR
getScriptJSON                       :: r <: Blockfrost => r <: Error BlockfrostError => ScriptHash -> Eff r ScriptJSON
getScriptCBOR                       :: r <: Blockfrost => r <: Error BlockfrostError => ScriptHash -> Eff r ScriptCBOR
-- Client.Cardano.Epochs
getLatestEpoch                      :: r <: Blockfrost => r <: Error BlockfrostError => Eff r EpochInfo
getLatestEpochProtocolParams        :: r <: Blockfrost => r <: Error BlockfrostError => Eff r ProtocolParams
getEpoch                            :: r <: Blockfrost => r <: Error BlockfrostError => Epoch -> Eff r EpochInfo
getNextEpochs'                      :: r <: Blockfrost => r <: Error BlockfrostError => Epoch -> Paged -> Eff r [EpochInfo]
getNextEpochs                       :: r <: Blockfrost => r <: Error BlockfrostError => Epoch -> Eff r [EpochInfo]
getPreviousEpochs'                  :: r <: Blockfrost => r <: Error BlockfrostError => Epoch -> Paged -> Eff r [EpochInfo]
getPreviousEpochs                   :: r <: Blockfrost => r <: Error BlockfrostError => Epoch -> Eff r [EpochInfo]
getEpochStake'                      :: r <: Blockfrost => r <: Error BlockfrostError => Epoch -> Paged -> Eff r [StakeDistribution]
getEpochStake                       :: r <: Blockfrost => r <: Error BlockfrostError => Epoch -> Eff r [StakeDistribution]
getEpochStakeByPool'                :: r <: Blockfrost => r <: Error BlockfrostError => Epoch -> PoolId -> Paged -> Eff r [PoolStakeDistribution]
getEpochStakeByPool                 :: r <: Blockfrost => r <: Error BlockfrostError => Epoch -> PoolId -> Eff r [PoolStakeDistribution]
getEpochBlocks'                     :: r <: Blockfrost => r <: Error BlockfrostError => Epoch -> Paged -> SortOrder -> Eff r [BlockHash]
getEpochBlocks                      :: r <: Blockfrost => r <: Error BlockfrostError => Epoch -> Eff r [BlockHash]
getEpochBlocksByPool'               :: r <: Blockfrost => r <: Error BlockfrostError => Epoch -> PoolId -> Paged -> SortOrder -> Eff r [BlockHash]
getEpochBlocksByPool                :: r <: Blockfrost => r <: Error BlockfrostError => Epoch -> PoolId -> Eff r [BlockHash]
getEpochProtocolParams              :: r <: Blockfrost => r <: Error BlockfrostError => Epoch -> Eff r ProtocolParams
-- Client.Cardano.Transactions
getTx                               :: r <: Blockfrost => r <: Error BlockfrostError => TxHash -> Eff r Transaction
getTxUtxos                          :: r <: Blockfrost => r <: Error BlockfrostError => TxHash -> Eff r TransactionUtxos
getTxRedeemers                      :: r <: Blockfrost => r <: Error BlockfrostError => TxHash -> Eff r [TransactionRedeemer]
getTxStakes                         :: r <: Blockfrost => r <: Error BlockfrostError => TxHash -> Eff r [TransactionStake]
getTxDelegations                    :: r <: Blockfrost => r <: Error BlockfrostError => TxHash -> Eff r [TransactionDelegation]
getTxWithdrawals                    :: r <: Blockfrost => r <: Error BlockfrostError => TxHash -> Eff r [TransactionWithdrawal]
getTxMirs                           :: r <: Blockfrost => r <: Error BlockfrostError => TxHash -> Eff r [TransactionMir]
getTxPoolUpdates                    :: r <: Blockfrost => r <: Error BlockfrostError => TxHash -> Eff r [TransactionPoolUpdate]
getTxPoolRetiring                   :: r <: Blockfrost => r <: Error BlockfrostError => TxHash -> Eff r [TransactionPoolRetiring]
getTxMetadataJSON                   :: r <: Blockfrost => r <: Error BlockfrostError => TxHash -> Eff r [TransactionMetaJSON]
getTxMetadataCBOR                   :: r <: Blockfrost => r <: Error BlockfrostError => TxHash -> Eff r [TransactionMetaCBOR]
submitTx                            :: r <: Blockfrost => r <: Error BlockfrostError => CBORString -> Eff r TxHash
-- Client.Cardano.Ledger
getLedgerGenesis                    :: r <: Blockfrost => r <: Error BlockfrostError => Eff r Genesis
-- Client.Cardano.Mempool
getMempoolTransactions              :: r <: Blockfrost => r <: Error BlockfrostError => Paged -> SortOrder -> Eff r [TxHashObject]
-- Client.Cardano.Accounts
getAccount                          :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Eff r AccountInfo
getAccountRewards'                  :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Paged -> SortOrder -> Eff r [AccountReward]
getAccountRewards                   :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Eff r [AccountReward]
getAccountHistory'                  :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Paged -> SortOrder -> Eff r [AccountHistory]
getAccountHistory                   :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Eff r [AccountHistory]
getAccountDelegations'              :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Paged -> SortOrder -> Eff r [AccountDelegation]
getAccountDelegations               :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Eff r [AccountDelegation]
getAccountRegistrations'            :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Paged -> SortOrder -> Eff r [AccountRegistration]
getAccountRegistrations             :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Eff r [AccountRegistration]
getAccountWithdrawals'              :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Paged -> SortOrder -> Eff r [AccountWithdrawal]
getAccountWithdrawals               :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Eff r [AccountWithdrawal]
getAccountMirs'                     :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Paged -> SortOrder -> Eff r [AccountMir]
getAccountMirs                      :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Eff r [AccountMir]
getAccountAssociatedAddresses'      :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Paged -> SortOrder -> Eff r [AddressAssociated]
getAccountAssociatedAddresses       :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Eff r [AddressAssociated]
getAccountAssociatedAddressesTotal  :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Eff r AddressAssociatedTotal
getAccountAssociatedAssets'         :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Paged -> SortOrder -> Eff r [Amount]
getAccountAssociatedAssets          :: r <: Blockfrost => r <: Error BlockfrostError => Address -> Eff r [Amount]
-- Client.Cardano.Pools
listPools'                          :: r <: Blockfrost => r <: Error BlockfrostError => Paged -> SortOrder -> Eff r [PoolId]
listPools                           :: r <: Blockfrost => r <: Error BlockfrostError => Eff r [PoolId]
listPoolsExtended'                  :: r <: Blockfrost => r <: Error BlockfrostError => Paged -> SortOrder -> Eff r [Pool]
listPoolsExtended                   :: r <: Blockfrost => r <: Error BlockfrostError => Eff r [Pool]
listRetiredPools'                   :: r <: Blockfrost => r <: Error BlockfrostError => Paged -> SortOrder -> Eff r [PoolEpoch]
listRetiredPools                    :: r <: Blockfrost => r <: Error BlockfrostError => Eff r [PoolEpoch]
listRetiringPools'                  :: r <: Blockfrost => r <: Error BlockfrostError => Paged -> SortOrder -> Eff r [PoolEpoch]
listRetiringPools                   :: r <: Blockfrost => r <: Error BlockfrostError => Eff r [PoolEpoch]
getPool                             :: r <: Blockfrost => r <: Error BlockfrostError => PoolId -> Eff r PoolInfo
getPoolHistory'                     :: r <: Blockfrost => r <: Error BlockfrostError => PoolId -> Paged -> SortOrder -> Eff r [PoolHistory]
getPoolHistory                      :: r <: Blockfrost => r <: Error BlockfrostError => PoolId -> Eff r [PoolHistory]
getPoolMetadata                     :: r <: Blockfrost => r <: Error BlockfrostError => PoolId -> Eff r (Maybe PoolMetadata)
getPoolRelays                       :: r <: Blockfrost => r <: Error BlockfrostError => PoolId -> Eff r [PoolRelay]
getPoolDelegators'                  :: r <: Blockfrost => r <: Error BlockfrostError => PoolId -> Paged -> SortOrder -> Eff r [PoolDelegator]
getPoolDelegators                   :: r <: Blockfrost => r <: Error BlockfrostError => PoolId -> Eff r [PoolDelegator]
getPoolBlocks'                      :: r <: Blockfrost => r <: Error BlockfrostError => PoolId -> Paged -> SortOrder -> Eff r [BlockHash]
getPoolBlocks                       :: r <: Blockfrost => r <: Error BlockfrostError => PoolId -> Eff r [BlockHash]
getPoolUpdates'                     :: r <: Blockfrost => r <: Error BlockfrostError => PoolId -> Paged -> SortOrder -> Eff r [PoolUpdate]
getPoolUpdates                      :: r <: Blockfrost => r <: Error BlockfrostError => PoolId -> Eff r [PoolUpdate]
-- Client.Cardano.Metadata
getTxMetadataLabels'                :: r <: Blockfrost => r <: Error BlockfrostError => Paged -> SortOrder -> Eff r [TxMeta]
getTxMetadataLabels                 :: r <: Blockfrost => r <: Error BlockfrostError => Eff r [TxMeta]
getTxMetadataByLabelJSON'           :: r <: Blockfrost => r <: Error BlockfrostError => Text -> Paged -> SortOrder -> Eff r [TxMetaJSON]
getTxMetadataByLabelJSON            :: r <: Blockfrost => r <: Error BlockfrostError => Text -> Eff r [TxMetaJSON]
getTxMetadataByLabelCBOR'           :: r <: Blockfrost => r <: Error BlockfrostError => Text -> Paged -> SortOrder -> Eff r [TxMetaCBOR]
getTxMetadataByLabelCBOR            :: r <: Blockfrost => r <: Error BlockfrostError => Text -> Eff r [TxMetaCBOR]
-- Client
getRoot                                         = fromEither =<< send do BF.GetRoot
getHealth                                       = fromEither =<< send do BF.GetHealth
getClock                                        = fromEither =<< send do BF.GetClock
getMetrics                                      = fromEither =<< send do BF.GetMetrics
getMetricsEndpoints                             = fromEither =<< send do BF.GetMetricsEndpoints

-- Client.NutLink
nutlinkListAddress                  a           = fromEither =<< send do BF.NutlinkListAddress                    a
nutlinkListAddressTickers'          a b c       = fromEither =<< send do BF.NutlinkListAddressTickers'            a b c
nutlinkListAddressTickers           a           = fromEither =<< send do BF.NutlinkListAddressTickers             a
nutlinkAddressTickers'              a b c d     = fromEither =<< send do BF.NutlinkAddressTickers'                a b c d
nutlinkAddressTickers               a b         = fromEither =<< send do BF.NutlinkAddressTickers                 a b
nutlinkTickers'                     a b c       = fromEither =<< send do BF.NutlinkTickers'                       a b c
nutlinkTickers                      a           = fromEither =<< send do BF.NutlinkTickers                        a

-- -- Client.IPFS
ipfsGateway                         a           = fromEither =<< send do BF.IpfsGateway                           a
ipfsPin                             a           = fromEither =<< send do BF.IpfsPin                               a
ipfsListPins'                       a b         = fromEither =<< send do BF.IpfsListPins'                         a b
ipfsListPins                                    = fromEither =<< send do BF.IpfsListPins
ipfsGetPin                          a           = fromEither =<< send do BF.IpfsGetPin                            a
ipfsRemovePin                       a           = fromEither =<< send do BF.IpfsRemovePin                         a

-- Client.Cardano.Blocks
getLatestBlock                                  = fromEither =<< send do BF.GetLatestBlock
getLatestBlockTxs'                  a b         = fromEither =<< send do BF.GetLatestBlockTxs'                    a b
getLatestBlockTxs                               = fromEither =<< send do BF.GetLatestBlockTxs
getBlock                            a           = fromEither =<< send do BF.GetBlock                              a
getBlockSlot                        a           = fromEither =<< send do BF.GetBlockSlot                          a
getBlockEpochSlot                   a b         = fromEither =<< send do BF.GetBlockEpochSlot                     a b
getNextBlocks'                      a b         = fromEither =<< send do BF.GetNextBlocks'                        a b
getNextBlocks                       a           = fromEither =<< send do BF.GetNextBlocks                         a
getPreviousBlocks'                  a b         = fromEither =<< send do BF.GetPreviousBlocks'                    a b
getPreviousBlocks                   a           = fromEither =<< send do BF.GetPreviousBlocks                     a
getBlockTxs'                        a b c       = fromEither =<< send do BF.GetBlockTxs'                          a b c
getBlockTxs                         a           = fromEither =<< send do BF.GetBlockTxs                           a
getBlockAffectedAddresses'          a b         = fromEither =<< send do BF.GetBlockAffectedAddresses'            a b
getBlockAffectedAddresses           a           = fromEither =<< send do BF.GetBlockAffectedAddresses             a

-- -- Client.Cardano.Network
getNetworkInfo                                  = fromEither =<< send do BF.GetNetworkInfo
getNetworkEras                                  = fromEither =<< send do BF.GetNetworkEras

-- Client.Cardano.Addresses
getAddressInfo                      a           = fromEither =<< send do BF.GetAddressInfo                        a
getAddressInfoExtended              a           = fromEither =<< send do BF.GetAddressInfoExtended                a
getAddressDetails                   a           = fromEither =<< send do BF.GetAddressDetails                     a
getAddressUtxos'                    a b c       = fromEither =<< send do BF.GetAddressUtxos'                      a b c
getAddressUtxos                     a           = fromEither =<< send do BF.GetAddressUtxos                       a
getAddressUtxosAsset'               a b c d     = fromEither =<< send do BF.GetAddressUtxosAsset'                 a b c d
getAddressUtxosAsset                a b         = fromEither =<< send do BF.GetAddressUtxosAsset                  a b
getAddressTransactions              a           = fromEither =<< send do BF.GetAddressTransactions                a
getAddressTransactions'             a b c d e   = fromEither =<< send do BF.GetAddressTransactions'               a b c d e

-- Client.Cardano.Assets
getAssets'                          a b         = fromEither =<< send do BF.GetAssets'                          a b
getAssets                                       = fromEither =<< send do BF.GetAssets
getAssetDetails                     a           = fromEither =<< send do BF.GetAssetDetails                     a
getAssetHistory'                    a b c       = fromEither =<< send do BF.GetAssetHistory'                    a b c
getAssetHistory                     a           = fromEither =<< send do BF.GetAssetHistory                     a
getAssetTransactions'               a b c       = fromEither =<< send do BF.GetAssetTransactions'               a b c
getAssetTransactions                a           = fromEither =<< send do BF.GetAssetTransactions                a
getAssetAddresses'                  a b c       = fromEither =<< send do BF.GetAssetAddresses'                  a b c
getAssetAddresses                   a           = fromEither =<< send do BF.GetAssetAddresses                   a
getAssetsByPolicy'                  a b c       = fromEither =<< send do BF.GetAssetsByPolicy'                  a b c
getAssetsByPolicy                   a           = fromEither =<< send do BF.GetAssetsByPolicy                   a

-- Client.Cardano.Scripts
listScripts'                        a b         = fromEither =<< send do BF.ListScripts'                        a b
listScripts                                     = fromEither =<< send do BF.ListScripts
getScript                           a           = fromEither =<< send do BF.GetScript                           a
getScriptRedeemers'                 a b c       = fromEither =<< send do BF.GetScriptRedeemers'                 a b c
getScriptRedeemers                  a           = fromEither =<< send do BF.GetScriptRedeemers                  a
getScriptDatum                      a           = fromEither =<< send do BF.GetScriptDatum                      a
getScriptDatumCBOR                  a           = fromEither =<< send do BF.GetScriptDatumCBOR                  a
getScriptJSON                       a           = fromEither =<< send do BF.GetScriptJSON                       a
getScriptCBOR                       a           = fromEither =<< send do BF.GetScriptCBOR                       a

-- Client.Cardano.Epochs
getLatestEpoch                                  = fromEither =<< send do BF.GetLatestEpoch
getLatestEpochProtocolParams                    = fromEither =<< send do BF.GetLatestEpochProtocolParams
getEpoch                            a           = fromEither =<< send do BF.GetEpoch                            a
getNextEpochs'                      a b         = fromEither =<< send do BF.GetNextEpochs'                      a b
getNextEpochs                       a           = fromEither =<< send do BF.GetNextEpochs                       a
getPreviousEpochs'                  a b         = fromEither =<< send do BF.GetPreviousEpochs'                  a b
getPreviousEpochs                   a           = fromEither =<< send do BF.GetPreviousEpochs                   a
getEpochStake'                      a b         = fromEither =<< send do BF.GetEpochStake'                      a b
getEpochStake                       a           = fromEither =<< send do BF.GetEpochStake                       a
getEpochStakeByPool'                a b c       = fromEither =<< send do BF.GetEpochStakeByPool'                a b c
getEpochStakeByPool                 a b         = fromEither =<< send do BF.GetEpochStakeByPool                 a b
getEpochBlocks'                     a b c       = fromEither =<< send do BF.GetEpochBlocks'                     a b c
getEpochBlocks                      a           = fromEither =<< send do BF.GetEpochBlocks                      a
getEpochBlocksByPool'               a b c d     = fromEither =<< send do BF.GetEpochBlocksByPool'               a b c d
getEpochBlocksByPool                a b         = fromEither =<< send do BF.GetEpochBlocksByPool                a b
getEpochProtocolParams              a           = fromEither =<< send do BF.GetEpochProtocolParams              a

-- Client.Cardano.Transactions
getTx                               a           = fromEither =<< send do BF.GetTx                               a
getTxUtxos                          a           = fromEither =<< send do BF.GetTxUtxos                          a
getTxRedeemers                      a           = fromEither =<< send do BF.GetTxRedeemers                      a
getTxStakes                         a           = fromEither =<< send do BF.GetTxStakes                         a
getTxDelegations                    a           = fromEither =<< send do BF.GetTxDelegations                    a
getTxWithdrawals                    a           = fromEither =<< send do BF.GetTxWithdrawals                    a
getTxMirs                           a           = fromEither =<< send do BF.GetTxMirs                           a
getTxPoolUpdates                    a           = fromEither =<< send do BF.GetTxPoolUpdates                    a
getTxPoolRetiring                   a           = fromEither =<< send do BF.GetTxPoolRetiring                   a
getTxMetadataJSON                   a           = fromEither =<< send do BF.GetTxMetadataJSON                   a
getTxMetadataCBOR                   a           = fromEither =<< send do BF.GetTxMetadataCBOR                   a
submitTx                            a           = fromEither =<< send do BF.SubmitTx                            a

-- Client.Cardano.Ledger
getLedgerGenesis                                = fromEither =<< send do BF.GetLedgerGenesis

-- Client.Cardano.Mempool
getMempoolTransactions              a b         = fromEither =<< send do BF.GetMempoolTransactions              a b

-- Client.Cardano.Accounts
getAccount                          a           = fromEither =<< send do BF.GetAccount                          a
getAccountRewards'                  a b c       = fromEither =<< send do BF.GetAccountRewards'                  a b c
getAccountRewards                   a           = fromEither =<< send do BF.GetAccountRewards                   a
getAccountHistory'                  a b c       = fromEither =<< send do BF.GetAccountHistory'                  a b c
getAccountHistory                   a           = fromEither =<< send do BF.GetAccountHistory                   a
getAccountDelegations'              a b c       = fromEither =<< send do BF.GetAccountDelegations'              a b c
getAccountDelegations               a           = fromEither =<< send do BF.GetAccountDelegations               a
getAccountRegistrations'            a b c       = fromEither =<< send do BF.GetAccountRegistrations'            a b c
getAccountRegistrations             a           = fromEither =<< send do BF.GetAccountRegistrations             a
getAccountWithdrawals'              a b c       = fromEither =<< send do BF.GetAccountWithdrawals'              a b c
getAccountWithdrawals               a           = fromEither =<< send do BF.GetAccountWithdrawals               a
getAccountMirs'                     a b c       = fromEither =<< send do BF.GetAccountMirs'                     a b c
getAccountMirs                      a           = fromEither =<< send do BF.GetAccountMirs                      a
getAccountAssociatedAddresses'      a b c       = fromEither =<< send do BF.GetAccountAssociatedAddresses'      a b c
getAccountAssociatedAddresses       a           = fromEither =<< send do BF.GetAccountAssociatedAddresses       a
getAccountAssociatedAddressesTotal  a           = fromEither =<< send do BF.GetAccountAssociatedAddressesTotal  a
getAccountAssociatedAssets'         a b c       = fromEither =<< send do BF.GetAccountAssociatedAssets'         a b c
getAccountAssociatedAssets          a           = fromEither =<< send do BF.GetAccountAssociatedAssets          a

-- Client.Cardano.Pools
listPools'                          a b         = fromEither =<< send do BF.ListPools'                          a b
listPools                                       = fromEither =<< send do BF.ListPools
listPoolsExtended'                  a b         = fromEither =<< send do BF.ListPoolsExtended'                  a b
listPoolsExtended                               = fromEither =<< send do BF.ListPoolsExtended
listRetiredPools'                   a b         = fromEither =<< send do BF.ListRetiredPools'                   a b
listRetiredPools                                = fromEither =<< send do BF.ListRetiredPools
listRetiringPools'                  a b         = fromEither =<< send do BF.ListRetiringPools'                  a b
listRetiringPools                               = fromEither =<< send do BF.ListRetiringPools
getPool                             a           = fromEither =<< send do BF.GetPool                             a
getPoolHistory'                     a b c       = fromEither =<< send do BF.GetPoolHistory'                     a b c
getPoolHistory                      a           = fromEither =<< send do BF.GetPoolHistory                      a
getPoolMetadata                     a           = fromEither =<< send do BF.GetPoolMetadata                     a
getPoolRelays                       a           = fromEither =<< send do BF.GetPoolRelays                       a
getPoolDelegators'                  a b c       = fromEither =<< send do BF.GetPoolDelegators'                  a b c
getPoolDelegators                   a           = fromEither =<< send do BF.GetPoolDelegators                   a
getPoolBlocks'                      a b c       = fromEither =<< send do BF.GetPoolBlocks'                      a b c
getPoolBlocks                       a           = fromEither =<< send do BF.GetPoolBlocks                       a
getPoolUpdates'                     a b c       = fromEither =<< send do BF.GetPoolUpdates'                     a b c
getPoolUpdates                      a           = fromEither =<< send do BF.GetPoolUpdates                      a

-- Client.Cardano.Metadata
getTxMetadataLabels'                a b         = fromEither =<< send do BF.GetTxMetadataLabels'                a b
getTxMetadataLabels                             = fromEither =<< send do BF.GetTxMetadataLabels
getTxMetadataByLabelJSON'           a b c       = fromEither =<< send do BF.GetTxMetadataByLabelJSON'           a b c
getTxMetadataByLabelJSON            a           = fromEither =<< send do BF.GetTxMetadataByLabelJSON            a
getTxMetadataByLabelCBOR'           a b c       = fromEither =<< send do BF.GetTxMetadataByLabelCBOR'           a b c
getTxMetadataByLabelCBOR            a           = fromEither =<< send do BF.GetTxMetadataByLabelCBOR            a
