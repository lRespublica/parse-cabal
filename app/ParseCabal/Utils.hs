module ParseCabal.Utils (
    PackageDescription (..),
    readFinalPackageDescription,
    buildDependencies,
    PackageIdentifier (..),
    getPackageDesc,
    getPkgLicenseString,
    getPkgName,
    getPkgVersion,
    getPkgId,
    getPkgDescriptionText,
    getPkgSynopsis,
    getPkgHomepage,
    getPkgBuildDeps,
    getPkgExecutables
 ) where

import SimpleCabal (
    PackageDescription (..),
    findCabalFile,
    readFinalPackageDescription,
    buildDependencies,
    PackageDescription (..),
    PackageIdentifier (..),
    unPackageName, showVersion,
 )

import Distribution.PackageDescription (
    Executable(..), unUnqualComponentName
 )

import Distribution.Utils.ShortText (
    fromShortText
 )

import qualified Distribution.SPDX.License as SPDX (
    License (..)
 )
import qualified Distribution.SPDX.LicenseExpression as SPDX (
    LicenseExpression (..),
    SimpleLicenseExpression (..),
 )
import qualified Distribution.SPDX.LicenseId as SPDX (
    licenseId
 )
import qualified Distribution.SPDX.LicenseReference as SPDX (
    licenseRef
 )

import qualified Distribution.License as Dist (
    License (..),
    licenseToSPDX
 )

import Data.List (intercalate)

getPackageDesc :: IO PackageDescription
getPackageDesc = do
    cabalFile <- findCabalFile
    readFinalPackageDescription [] cabalFile

getPkgLicenseString :: PackageDescription -> String
getPkgLicenseString desc = case raw of
                            Left  license -> fromSPDX license
                            Right license -> fromDistLicense license
    where raw = licenseRaw desc

          fromSimpleLicense :: SPDX.SimpleLicenseExpression -> String
          fromSimpleLicense (SPDX.ELicenseId license) = SPDX.licenseId license
          fromSimpleLicense (SPDX.ELicenseIdPlus license) = SPDX.licenseId license
          fromSimpleLicense (SPDX.ELicenseRef license) = SPDX.licenseRef license

          fromSPDXExpression :: SPDX.LicenseExpression -> String
          fromSPDXExpression (SPDX.ELicense simpleLicense _) = fromSimpleLicense simpleLicense
          fromSPDXExpression (SPDX.EAnd license1 license2) = license1Val ++ "AND" ++ license2Val
                where license1Val = fromSPDXExpression license1
                      license2Val = fromSPDXExpression license2
          fromSPDXExpression (SPDX.EOr license1 license2) = license1Val ++ "OR" ++ license2Val
                where license1Val = fromSPDXExpression license1
                      license2Val = fromSPDXExpression license2

          fromSPDX :: SPDX.License -> String
          fromSPDX SPDX.NONE  = "UNKNOWN"
          fromSPDX (SPDX.License license) = fromSPDXExpression license

          fromDistLicense :: Dist.License -> String
          fromDistLicense = fromSPDX . Dist.licenseToSPDX

getPkgName :: PackageDescription -> String
getPkgName = unPackageName . pkgName . package

getPkgVersion :: PackageDescription -> String
getPkgVersion = showVersion . pkgVersion . package

getPkgId :: PackageDescription -> String
getPkgId pkg = name ++ "-" ++ version
    where name = getPkgName pkg
          version = getPkgVersion pkg

getPkgDescriptionText :: PackageDescription -> String
getPkgDescriptionText = fromShortText . description

getPkgSynopsis :: PackageDescription -> String
getPkgSynopsis = fromShortText . synopsis

getPkgHomepage :: PackageDescription -> String
getPkgHomepage = fromShortText . homepage

getPkgBuildDeps :: PackageDescription -> String
getPkgBuildDeps = intercalate "\n" . fmap unPackageName . buildDependencies

getPkgExecutables :: PackageDescription -> String
getPkgExecutables = intercalate "\n" . fmap (unUnqualComponentName . exeName) . executables
