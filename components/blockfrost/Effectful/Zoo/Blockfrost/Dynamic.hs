{- HLINT ignore "Redundant $" -}

module Effectful.Zoo.Blockfrost.Dynamic where

import Blockfrost.Client (BlockfrostError, Project)
import Blockfrost.Client qualified as BF
import Blockfrost.Types
import Blockfrost.Util.Pagination
import Blockfrost.Util.Sorting
import Data.Either
import Data.Maybe
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Effectful.Zoo.Core
import HaskellWorks.Prelude

data Blockfrost :: Effect where

  -- Client
  GetRoot :: Blockfrost m (Either BlockfrostError URLVersion)
  GetHealth :: Blockfrost m (Either BlockfrostError Healthy)
  GetClock :: Blockfrost m (Either BlockfrostError ServerTime)
  GetMetrics :: Blockfrost m (Either BlockfrostError [Metric])
  GetMetricsEndpoints :: Blockfrost m (Either BlockfrostError [(Text, Metric)])

  -- Client.NutLink
  NutlinkListAddress :: Address -> Blockfrost m (Either BlockfrostError NutlinkAddress)
  NutlinkListAddressTickers' :: Address -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [NutlinkAddressTicker])
  NutlinkListAddressTickers :: Address -> Blockfrost m (Either BlockfrostError [NutlinkAddressTicker])
  NutlinkAddressTickers' :: Address -> Text -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [NutlinkTicker])
  NutlinkAddressTickers :: Address -> Text -> Blockfrost m (Either BlockfrostError [NutlinkTicker])
  NutlinkTickers' :: Text -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [(Address, NutlinkTicker)])
  NutlinkTickers :: Text -> Blockfrost m (Either BlockfrostError [(Address, NutlinkTicker)])

  -- Client.IPFS
  IpfsGateway :: Text -> Blockfrost m (Either BlockfrostError IPFSData)
  IpfsPin :: Text -> Blockfrost m (Either BlockfrostError IPFSPinChange)
  IpfsListPins' :: Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [IPFSPin])
  IpfsListPins :: Blockfrost m (Either BlockfrostError [IPFSPin])
  IpfsGetPin :: Text -> Blockfrost m (Either BlockfrostError IPFSPin)
  IpfsRemovePin :: Text -> Blockfrost m (Either BlockfrostError IPFSPinChange)

  -- Client.Cardano.Blocks
  GetLatestBlock :: Blockfrost m (Either BlockfrostError Block)
  GetLatestBlockTxs' :: Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [TxHash])
  GetLatestBlockTxs :: Blockfrost m (Either BlockfrostError [TxHash])
  GetBlock :: Either Integer BlockHash -> Blockfrost m (Either BlockfrostError Block)
  GetBlockSlot :: Slot -> Blockfrost m (Either BlockfrostError Block)
  GetBlockEpochSlot :: Epoch -> Slot -> Blockfrost m (Either BlockfrostError Block)
  GetNextBlocks' :: Either Integer BlockHash -> Paged -> Blockfrost m (Either BlockfrostError [Block])
  GetNextBlocks :: Either Integer BlockHash -> Blockfrost m (Either BlockfrostError [Block])
  GetPreviousBlocks' :: Either Integer BlockHash -> Paged -> Blockfrost m (Either BlockfrostError [Block])
  GetPreviousBlocks :: Either Integer BlockHash -> Blockfrost m (Either BlockfrostError [Block])
  GetBlockTxs' :: Either Integer BlockHash -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [TxHash])
  GetBlockTxs :: Either Integer BlockHash -> Blockfrost m (Either BlockfrostError [TxHash])
  GetBlockAffectedAddresses' :: Either Integer BlockHash -> Paged -> Blockfrost m (Either BlockfrostError [(Address, [TxHash])])
  GetBlockAffectedAddresses :: Either Integer BlockHash -> Blockfrost m (Either BlockfrostError [(Address, [TxHash])])

  -- Client.Cardano.Network
  GetNetworkInfo :: Blockfrost m (Either BlockfrostError Network)
  GetNetworkEras :: Blockfrost m (Either BlockfrostError [NetworkEraSummary])

  -- Client.Cardano.Addresses
  GetAddressInfo :: Address -> Blockfrost m (Either BlockfrostError AddressInfo)
  GetAddressInfoExtended :: Address -> Blockfrost m (Either BlockfrostError AddressInfoExtended)
  GetAddressDetails :: Address -> Blockfrost m (Either BlockfrostError AddressDetails)
  GetAddressUtxos' :: Address -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [AddressUtxo])
  GetAddressUtxos :: Address -> Blockfrost m (Either BlockfrostError [AddressUtxo])
  GetAddressUtxosAsset' :: Address -> AssetId-> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [AddressUtxo])
  GetAddressUtxosAsset :: Address -> AssetId -> Blockfrost m (Either BlockfrostError [AddressUtxo])
  GetAddressTransactions' :: Address -> Paged -> SortOrder -> Maybe BlockIndex -> Maybe BlockIndex -> Blockfrost m (Either BlockfrostError [AddressTransaction])
  GetAddressTransactions :: Address -> Blockfrost m (Either BlockfrostError [AddressTransaction])

  -- Client.Cardano.Assets
  GetAssets' :: Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [AssetInfo])
  GetAssets :: Blockfrost m (Either BlockfrostError [AssetInfo])
  GetAssetDetails :: AssetId -> Blockfrost m (Either BlockfrostError AssetDetails)
  GetAssetHistory' :: AssetId -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [AssetHistory])
  GetAssetHistory :: AssetId -> Blockfrost m (Either BlockfrostError [AssetHistory])
  GetAssetTransactions' :: AssetId -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [AssetTransaction])
  GetAssetTransactions :: AssetId -> Blockfrost m (Either BlockfrostError [AssetTransaction])
  GetAssetAddresses' :: AssetId -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [AssetAddress])
  GetAssetAddresses :: AssetId -> Blockfrost m (Either BlockfrostError [AssetAddress])
  GetAssetsByPolicy' :: PolicyId -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [AssetInfo])
  GetAssetsByPolicy :: PolicyId -> Blockfrost m (Either BlockfrostError [AssetInfo])

  -- Client.Cardano.Scripts
  ListScripts' :: Paged -> SortOrder -> Blockfrost m (Either BlockfrostError ScriptHashList)
  ListScripts :: Blockfrost m (Either BlockfrostError ScriptHashList)
  GetScript :: ScriptHash -> Blockfrost m (Either BlockfrostError Script)
  GetScriptRedeemers' :: ScriptHash -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [ScriptRedeemer])
  GetScriptRedeemers :: ScriptHash -> Blockfrost m (Either BlockfrostError [ScriptRedeemer])
  GetScriptDatum :: DatumHash -> Blockfrost m (Either BlockfrostError ScriptDatum)
  GetScriptDatumCBOR :: DatumHash -> Blockfrost m (Either BlockfrostError ScriptDatumCBOR)
  GetScriptJSON :: ScriptHash -> Blockfrost m (Either BlockfrostError ScriptJSON)
  GetScriptCBOR :: ScriptHash -> Blockfrost m (Either BlockfrostError ScriptCBOR)

  -- Client.Cardano.Epochs
  GetLatestEpoch :: Blockfrost m (Either BlockfrostError EpochInfo)
  GetLatestEpochProtocolParams :: Blockfrost m (Either BlockfrostError ProtocolParams)
  GetEpoch :: Epoch -> Blockfrost m (Either BlockfrostError EpochInfo)
  GetNextEpochs' :: Epoch -> Paged -> Blockfrost m (Either BlockfrostError [EpochInfo])
  GetNextEpochs :: Epoch -> Blockfrost m (Either BlockfrostError [EpochInfo])
  GetPreviousEpochs' :: Epoch -> Paged -> Blockfrost m (Either BlockfrostError [EpochInfo])
  GetPreviousEpochs :: Epoch -> Blockfrost m (Either BlockfrostError [EpochInfo])
  GetEpochStake' :: Epoch -> Paged -> Blockfrost m (Either BlockfrostError [StakeDistribution])
  GetEpochStake :: Epoch -> Blockfrost m (Either BlockfrostError [StakeDistribution])
  GetEpochStakeByPool' :: Epoch -> PoolId -> Paged -> Blockfrost m (Either BlockfrostError [PoolStakeDistribution])
  GetEpochStakeByPool :: Epoch -> PoolId -> Blockfrost m (Either BlockfrostError [PoolStakeDistribution])
  GetEpochBlocks' :: Epoch -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [BlockHash])
  GetEpochBlocks :: Epoch -> Blockfrost m (Either BlockfrostError [BlockHash])
  GetEpochBlocksByPool' :: Epoch -> PoolId -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [BlockHash])
  GetEpochBlocksByPool :: Epoch -> PoolId -> Blockfrost m (Either BlockfrostError [BlockHash])
  GetEpochProtocolParams :: Epoch -> Blockfrost m (Either BlockfrostError ProtocolParams)

  -- Client.Cardano.Transactions
  GetTx :: TxHash -> Blockfrost m (Either BlockfrostError Transaction)
  GetTxUtxos :: TxHash -> Blockfrost m (Either BlockfrostError TransactionUtxos)
  GetTxRedeemers :: TxHash -> Blockfrost m (Either BlockfrostError [TransactionRedeemer])
  GetTxStakes :: TxHash -> Blockfrost m (Either BlockfrostError [TransactionStake])
  GetTxDelegations :: TxHash -> Blockfrost m (Either BlockfrostError [TransactionDelegation])
  GetTxWithdrawals :: TxHash -> Blockfrost m (Either BlockfrostError [TransactionWithdrawal])
  GetTxMirs :: TxHash -> Blockfrost m (Either BlockfrostError [TransactionMir])
  GetTxPoolUpdates :: TxHash -> Blockfrost m (Either BlockfrostError [TransactionPoolUpdate])
  GetTxPoolRetiring :: TxHash -> Blockfrost m (Either BlockfrostError [TransactionPoolRetiring])
  GetTxMetadataJSON :: TxHash -> Blockfrost m (Either BlockfrostError [TransactionMetaJSON])
  GetTxMetadataCBOR :: TxHash -> Blockfrost m (Either BlockfrostError [TransactionMetaCBOR])
  SubmitTx :: CBORString -> Blockfrost m (Either BlockfrostError TxHash)

  -- Client.Cardano.Ledger
  GetLedgerGenesis :: Blockfrost m (Either BlockfrostError Genesis)

  -- Client.Cardano.Accounts
  GetAccount :: Address -> Blockfrost m (Either BlockfrostError AccountInfo)
  GetAccountRewards' :: Address -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [AccountReward])
  GetAccountRewards :: Address -> Blockfrost m (Either BlockfrostError [AccountReward])
  GetAccountHistory' :: Address -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [AccountHistory])
  GetAccountHistory :: Address -> Blockfrost m (Either BlockfrostError [AccountHistory])
  GetAccountDelegations' :: Address -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [AccountDelegation])
  GetAccountDelegations :: Address -> Blockfrost m (Either BlockfrostError [AccountDelegation])
  GetAccountRegistrations' :: Address -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [AccountRegistration])
  GetAccountRegistrations :: Address -> Blockfrost m (Either BlockfrostError [AccountRegistration])
  GetAccountWithdrawals' :: Address -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [AccountWithdrawal])
  GetAccountWithdrawals :: Address -> Blockfrost m (Either BlockfrostError [AccountWithdrawal])
  GetAccountMirs' :: Address -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [AccountMir])
  GetAccountMirs :: Address -> Blockfrost m (Either BlockfrostError [AccountMir])
  GetAccountAssociatedAddresses' :: Address -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [AddressAssociated])
  GetAccountAssociatedAddresses :: Address -> Blockfrost m (Either BlockfrostError [AddressAssociated])
  GetAccountAssociatedAddressesTotal :: Address -> Blockfrost m (Either BlockfrostError AddressAssociatedTotal)
  GetAccountAssociatedAssets' :: Address -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [Amount])
  GetAccountAssociatedAssets :: Address -> Blockfrost m (Either BlockfrostError [Amount])

  -- Client.Cardano.Pools
  ListPools' :: Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [PoolId])
  ListPools :: Blockfrost m (Either BlockfrostError [PoolId])
  ListPoolsExtended' :: Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [Pool])
  ListPoolsExtended :: Blockfrost m (Either BlockfrostError [Pool])
  ListRetiredPools' :: Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [PoolEpoch])
  ListRetiredPools :: Blockfrost m (Either BlockfrostError [PoolEpoch])
  ListRetiringPools' :: Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [PoolEpoch])
  ListRetiringPools :: Blockfrost m (Either BlockfrostError [PoolEpoch])
  GetPool :: PoolId -> Blockfrost m (Either BlockfrostError PoolInfo)
  GetPoolHistory' :: PoolId -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [PoolHistory])
  GetPoolHistory :: PoolId -> Blockfrost m (Either BlockfrostError [PoolHistory])
  GetPoolMetadata :: PoolId -> Blockfrost m (Either BlockfrostError (Maybe PoolMetadata))
  GetPoolRelays :: PoolId -> Blockfrost m (Either BlockfrostError [PoolRelay])
  GetPoolDelegators' :: PoolId -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [PoolDelegator])
  GetPoolDelegators :: PoolId -> Blockfrost m (Either BlockfrostError [PoolDelegator])
  GetPoolBlocks' :: PoolId -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [BlockHash])
  GetPoolBlocks :: PoolId -> Blockfrost m (Either BlockfrostError [BlockHash])
  GetPoolUpdates' :: PoolId -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [PoolUpdate])
  GetPoolUpdates :: PoolId -> Blockfrost m (Either BlockfrostError [PoolUpdate])

  -- Client.Cardano.Metadata
  GetTxMetadataLabels' :: Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [TxMeta])
  GetTxMetadataLabels :: Blockfrost m (Either BlockfrostError [TxMeta])
  GetTxMetadataByLabelJSON' :: Text -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [TxMetaJSON])
  GetTxMetadataByLabelJSON :: Text -> Blockfrost m (Either BlockfrostError [TxMetaJSON])
  GetTxMetadataByLabelCBOR' :: Text -> Paged -> SortOrder -> Blockfrost m (Either BlockfrostError [TxMetaCBOR])
  GetTxMetadataByLabelCBOR :: Text -> Blockfrost m (Either BlockfrostError [TxMetaCBOR])

type instance DispatchOf (Blockfrost) = Dynamic

callBlockfrost :: ()
  => r <: Reader Project
  => r <: IOE
  => BF.BlockfrostClientT IO b
  -> Eff r (Either BlockfrostError b)
callBlockfrost f = do
  project <- ask @Project
  liftIO (BF.runBlockfrost project f)

runBlockfrost :: ()
  => r <: IOE
  => r <: Reader Project
  => Eff (Blockfrost ': r) a
  -> Eff r a
runBlockfrost h =
  interpret
    do \_ -> \case
        -- Client
        GetRoot                                       -> callBlockfrost $ BF.getRoot
        GetHealth                                     -> callBlockfrost $ BF.getHealth
        GetClock                                      -> callBlockfrost $ BF.getClock
        GetMetrics                                    -> callBlockfrost $ BF.getMetrics
        GetMetricsEndpoints                           -> callBlockfrost $ BF.getMetricsEndpoints

        -- Client.NutLink
        NutlinkListAddress                  a         -> callBlockfrost $ BF.nutlinkListAddress                 a
        NutlinkListAddressTickers'          a b c     -> callBlockfrost $ BF.nutlinkListAddressTickers'         a b c
        NutlinkListAddressTickers           a         -> callBlockfrost $ BF.nutlinkListAddressTickers          a
        NutlinkAddressTickers'              a b c d   -> callBlockfrost $ BF.nutlinkAddressTickers'             a b c d
        NutlinkAddressTickers               a b       -> callBlockfrost $ BF.nutlinkAddressTickers              a b
        NutlinkTickers'                     a b c     -> callBlockfrost $ BF.nutlinkTickers'                    a b c
        NutlinkTickers                      a         -> callBlockfrost $ BF.nutlinkTickers                     a

        -- -- Client.IPFS
        IpfsGateway                         a         -> callBlockfrost $ BF.ipfsGateway                        a
        IpfsPin                             a         -> callBlockfrost $ BF.ipfsPin                            a
        IpfsListPins'                       a b       -> callBlockfrost $ BF.ipfsListPins'                      a b
        IpfsListPins                                  -> callBlockfrost $ BF.ipfsListPins
        IpfsGetPin                          a         -> callBlockfrost $ BF.ipfsGetPin                         a
        IpfsRemovePin                       a         -> callBlockfrost $ BF.ipfsRemovePin                      a

        -- Client.Cardano.Blocks
        GetLatestBlock                                -> callBlockfrost $ BF.getLatestBlock
        GetLatestBlockTxs'                  a b       -> callBlockfrost $ BF.getLatestBlockTxs'                 a b
        GetLatestBlockTxs                             -> callBlockfrost $ BF.getLatestBlockTxs
        GetBlock                            a         -> callBlockfrost $ BF.getBlock a
        GetBlockSlot                        a         -> callBlockfrost $ BF.getBlockSlot                       a
        GetBlockEpochSlot                   a b       -> callBlockfrost $ BF.getBlockEpochSlot                  a b
        GetNextBlocks'                      a b       -> callBlockfrost $ BF.getNextBlocks'                     a b
        GetNextBlocks                       a         -> callBlockfrost $ BF.getNextBlocks                      a
        GetPreviousBlocks'                  a b       -> callBlockfrost $ BF.getPreviousBlocks'                 a b
        GetPreviousBlocks                   a         -> callBlockfrost $ BF.getPreviousBlocks                  a
        GetBlockTxs'                        a b c     -> callBlockfrost $ BF.getBlockTxs'                       a b c
        GetBlockTxs                         a         -> callBlockfrost $ BF.getBlockTxs                        a
        GetBlockAffectedAddresses'          a b       -> callBlockfrost $ BF.getBlockAffectedAddresses'         a b
        GetBlockAffectedAddresses           a         -> callBlockfrost $ BF.getBlockAffectedAddresses          a

        -- -- Client.Cardano.Network
        GetNetworkInfo                                -> callBlockfrost $ BF.getNetworkInfo
        GetNetworkEras                                -> callBlockfrost $ BF.getNetworkEras

        -- Client.Cardano.Addresses
        GetAddressInfo                      a         -> callBlockfrost $ BF.getAddressInfo                   a
        GetAddressInfoExtended              a         -> callBlockfrost $ BF.getAddressInfoExtended           a
        GetAddressDetails                   a         -> callBlockfrost $ BF.getAddressDetails                a
        GetAddressUtxos'                    a b c     -> callBlockfrost $ BF.getAddressUtxos'                 a b c
        GetAddressUtxos                     a         -> callBlockfrost $ BF.getAddressUtxos                  a
        GetAddressUtxosAsset'               a b c d   -> callBlockfrost $ BF.getAddressUtxosAsset'            a b c d
        GetAddressUtxosAsset                a b       -> callBlockfrost $ BF.getAddressUtxosAsset             a b
        GetAddressTransactions              a         -> callBlockfrost $ BF.getAddressTransactions           a
        GetAddressTransactions'             a b c d e -> callBlockfrost $ BF.getAddressTransactions'          a b c d e

        -- Client.Cardano.Assets
        GetAssets'                          a b       -> callBlockfrost $ BF.getAssets'                       a b
        GetAssets                                     -> callBlockfrost $ BF.getAssets
        GetAssetDetails                     a         -> callBlockfrost $ BF.getAssetDetails                  a
        GetAssetHistory'                    a b c     -> callBlockfrost $ BF.getAssetHistory'                 a b c
        GetAssetHistory                     a         -> callBlockfrost $ BF.getAssetHistory                  a
        GetAssetTransactions'               a b c     -> callBlockfrost $ BF.getAssetTransactions'            a b c
        GetAssetTransactions                a         -> callBlockfrost $ BF.getAssetTransactions             a
        GetAssetAddresses'                  a b c     -> callBlockfrost $ BF.getAssetAddresses'               a b c
        GetAssetAddresses                   a         -> callBlockfrost $ BF.getAssetAddresses                a
        GetAssetsByPolicy'                  a b c     -> callBlockfrost $ BF.getAssetsByPolicy'               a b c
        GetAssetsByPolicy                   a         -> callBlockfrost $ BF.getAssetsByPolicy                a

        -- Client.Cardano.Scripts
        ListScripts'                        a b       -> callBlockfrost $ BF.listScripts'                       a b
        ListScripts                                   -> callBlockfrost $ BF.listScripts
        GetScript                           a         -> callBlockfrost $ BF.getScript                          a
        GetScriptRedeemers'                 a b c     -> callBlockfrost $ BF.getScriptRedeemers'                a b c
        GetScriptRedeemers                  a         -> callBlockfrost $ BF.getScriptRedeemers                 a
        GetScriptDatum                      a         -> callBlockfrost $ BF.getScriptDatum                     a
        GetScriptDatumCBOR                  a         -> callBlockfrost $ BF.getScriptDatumCBOR                 a
        GetScriptJSON                       a         -> callBlockfrost $ BF.getScriptJSON                      a
        GetScriptCBOR                       a         -> callBlockfrost $ BF.getScriptCBOR                      a

        -- Client.Cardano.Epochs
        GetLatestEpoch                                -> callBlockfrost $ BF.getLatestEpoch
        GetLatestEpochProtocolParams                  -> callBlockfrost $ BF.getLatestEpochProtocolParams
        GetEpoch                            a         -> callBlockfrost $ BF.getEpoch                           a
        GetNextEpochs'                      a b       -> callBlockfrost $ BF.getNextEpochs'                     a b
        GetNextEpochs                       a         -> callBlockfrost $ BF.getNextEpochs                      a
        GetPreviousEpochs'                  a b       -> callBlockfrost $ BF.getPreviousEpochs'                 a b
        GetPreviousEpochs                   a         -> callBlockfrost $ BF.getPreviousEpochs                  a
        GetEpochStake'                      a b       -> callBlockfrost $ BF.getEpochStake'                     a b
        GetEpochStake                       a         -> callBlockfrost $ BF.getEpochStake                      a
        GetEpochStakeByPool'                a b c     -> callBlockfrost $ BF.getEpochStakeByPool'               a b c
        GetEpochStakeByPool                 a b       -> callBlockfrost $ BF.getEpochStakeByPool                a b
        GetEpochBlocks'                     a b c     -> callBlockfrost $ BF.getEpochBlocks'                    a b c
        GetEpochBlocks                      a         -> callBlockfrost $ BF.getEpochBlocks                     a
        GetEpochBlocksByPool'               a b c d   -> callBlockfrost $ BF.getEpochBlocksByPool'              a b c d
        GetEpochBlocksByPool                a b       -> callBlockfrost $ BF.getEpochBlocksByPool               a b
        GetEpochProtocolParams              a         -> callBlockfrost $ BF.getEpochProtocolParams             a

        -- Client.Cardano.Transactions
        GetTx                               a         -> callBlockfrost $ BF.getTx                              a
        GetTxUtxos                          a         -> callBlockfrost $ BF.getTxUtxos                         a
        GetTxRedeemers                      a         -> callBlockfrost $ BF.getTxRedeemers                     a
        GetTxStakes                         a         -> callBlockfrost $ BF.getTxStakes                        a
        GetTxDelegations                    a         -> callBlockfrost $ BF.getTxDelegations                   a
        GetTxWithdrawals                    a         -> callBlockfrost $ BF.getTxWithdrawals                   a
        GetTxMirs                           a         -> callBlockfrost $ BF.getTxMirs                          a
        GetTxPoolUpdates                    a         -> callBlockfrost $ BF.getTxPoolUpdates                   a
        GetTxPoolRetiring                   a         -> callBlockfrost $ BF.getTxPoolRetiring                  a
        GetTxMetadataJSON                   a         -> callBlockfrost $ BF.getTxMetadataJSON                  a
        GetTxMetadataCBOR                   a         -> callBlockfrost $ BF.getTxMetadataCBOR                  a
        SubmitTx                            a         -> callBlockfrost $ BF.submitTx                           a

        -- Client.Cardano.Ledger
        GetLedgerGenesis                              -> callBlockfrost $ BF.getLedgerGenesis

        -- Client.Cardano.Accounts
        GetAccount                          a         -> callBlockfrost $ BF.getAccount                         a
        GetAccountRewards'                  a b c     -> callBlockfrost $ BF.getAccountRewards'                 a b c
        GetAccountRewards                   a         -> callBlockfrost $ BF.getAccountRewards                  a
        GetAccountHistory'                  a b c     -> callBlockfrost $ BF.getAccountHistory'                 a b c
        GetAccountHistory                   a         -> callBlockfrost $ BF.getAccountHistory                  a
        GetAccountDelegations'              a b c     -> callBlockfrost $ BF.getAccountDelegations'             a b c
        GetAccountDelegations               a         -> callBlockfrost $ BF.getAccountDelegations              a
        GetAccountRegistrations'            a b c     -> callBlockfrost $ BF.getAccountRegistrations'           a b c
        GetAccountRegistrations             a         -> callBlockfrost $ BF.getAccountRegistrations            a
        GetAccountWithdrawals'              a b c     -> callBlockfrost $ BF.getAccountWithdrawals'             a b c
        GetAccountWithdrawals               a         -> callBlockfrost $ BF.getAccountWithdrawals              a
        GetAccountMirs'                     a b c     -> callBlockfrost $ BF.getAccountMirs'                    a b c
        GetAccountMirs                      a         -> callBlockfrost $ BF.getAccountMirs                     a
        GetAccountAssociatedAddresses'      a b c     -> callBlockfrost $ BF.getAccountAssociatedAddresses'     a b c
        GetAccountAssociatedAddresses       a         -> callBlockfrost $ BF.getAccountAssociatedAddresses      a
        GetAccountAssociatedAddressesTotal  a         -> callBlockfrost $ BF.getAccountAssociatedAddressesTotal a
        GetAccountAssociatedAssets'         a b c     -> callBlockfrost $ BF.getAccountAssociatedAssets'        a b c
        GetAccountAssociatedAssets          a         -> callBlockfrost $ BF.getAccountAssociatedAssets         a

        -- Client.Cardano.Pools
        ListPools'                          a b       -> callBlockfrost $ BF.listPools'                         a b
        ListPools                                     -> callBlockfrost $ BF.listPools
        ListPoolsExtended'                  a b       -> callBlockfrost $ BF.listPoolsExtended'                 a b
        ListPoolsExtended                             -> callBlockfrost $ BF.listPoolsExtended
        ListRetiredPools'                   a b       -> callBlockfrost $ BF.listRetiredPools'                  a b
        ListRetiredPools                              -> callBlockfrost $ BF.listRetiredPools
        ListRetiringPools'                  a b       -> callBlockfrost $ BF.listRetiringPools'                 a b
        ListRetiringPools                             -> callBlockfrost $ BF.listRetiringPools
        GetPool                             a         -> callBlockfrost $ BF.getPool                            a
        GetPoolHistory'                     a b c     -> callBlockfrost $ BF.getPoolHistory'                    a b c
        GetPoolHistory                      a         -> callBlockfrost $ BF.getPoolHistory                     a
        GetPoolMetadata                     a         -> callBlockfrost $ BF.getPoolMetadata                    a
        GetPoolRelays                       a         -> callBlockfrost $ BF.getPoolRelays                      a
        GetPoolDelegators'                  a b c     -> callBlockfrost $ BF.getPoolDelegators'                 a b c
        GetPoolDelegators                   a         -> callBlockfrost $ BF.getPoolDelegators                  a
        GetPoolBlocks'                      a b c     -> callBlockfrost $ BF.getPoolBlocks'                     a b c
        GetPoolBlocks                       a         -> callBlockfrost $ BF.getPoolBlocks                      a
        GetPoolUpdates'                     a b c     -> callBlockfrost $ BF.getPoolUpdates'                    a b c
        GetPoolUpdates                      a         -> callBlockfrost $ BF.getPoolUpdates                     a

        -- Client.Cardano.Metadata
        GetTxMetadataLabels'                a b       -> callBlockfrost $ BF.getTxMetadataLabels'               a b
        GetTxMetadataLabels                           -> callBlockfrost $ BF.getTxMetadataLabels
        GetTxMetadataByLabelJSON'           a b c     -> callBlockfrost $ BF.getTxMetadataByLabelJSON'          a b c
        GetTxMetadataByLabelJSON            a         -> callBlockfrost $ BF.getTxMetadataByLabelJSON           a
        GetTxMetadataByLabelCBOR'           a b c     -> callBlockfrost $ BF.getTxMetadataByLabelCBOR'          a b c
        GetTxMetadataByLabelCBOR            a         -> callBlockfrost $ BF.getTxMetadataByLabelCBOR           a
    do h
