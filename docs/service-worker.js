/**
 * Welcome to your Workbox-powered service worker!
 *
 * You'll need to register this file in your web app and you should
 * disable HTTP caching for this file too.
 * See https://goo.gl/nhQhGp
 *
 * The rest of the code is auto-generated. Please don't update this file
 * directly; instead, make changes to your Workbox build configuration
 * and re-run your build process.
 * See https://goo.gl/2aRDsh
 */

importScripts("https://storage.googleapis.com/workbox-cdn/releases/3.6.2/workbox-sw.js");

/**
 * The workboxSW.precacheAndRoute() method efficiently caches and responds to
 * requests for URLs in the manifest.
 * See https://goo.gl/S9QRab
 */
self.__precacheManifest = [
  {
    "url": "Animations/iframe.html",
    "revision": "38044aacffbc86391e468f67650b1e40"
  },
  {
    "url": "Animations/index.html",
    "revision": "8ce01215292511177ebea87a0ed1d7bb"
  },
  {
    "url": "Controls/iframe.html",
    "revision": "5ddbd7b18f6bb2ae4edf1b943ca5da3f"
  },
  {
    "url": "Controls/index.html",
    "revision": "d838784aa1498fa16f6b1a0602dc053f"
  },
  {
    "url": "CultSim/iframe.html",
    "revision": "a012a921ce51ab40f049df8ffb8a36f5"
  },
  {
    "url": "CultSim/index.html",
    "revision": "7f878b6e06e913a0b8f50dd9f3b1018c"
  },
  {
    "url": "DigDigBoom/index.html",
    "revision": "dcd4db195adc8d377e8d5b81baccaff5"
  },
  {
    "url": "index.html",
    "revision": "5be7b274a1ffaf2d373d018305547bca"
  },
  {
    "url": "Main/iframe.html",
    "revision": "ba86cf8605c3a9558f30bf91c81e7c62"
  },
  {
    "url": "Main/index.html",
    "revision": "a13739a6fb821904cdab870d67c0487c"
  },
  {
    "url": "MiniWorldWar/index.html",
    "revision": "dabb25b86aff3d380a6a1c1660d4dbbc"
  },
  {
    "url": "Render/iframe.html",
    "revision": "34b2b551c82012d6691754133a207f71"
  },
  {
    "url": "Render/index.html",
    "revision": "2251e3d1476b88c223f468fe7c4b4a93"
  },
  {
    "url": "RuinJump/index.html",
    "revision": "fb02633927ca416b14ae76359a05e8ba"
  },
  {
    "url": "Test/iframe.html",
    "revision": "e08263cd28953553e0eb4313d473fe02"
  },
  {
    "url": "Test/index.html",
    "revision": "d3539200a3797c8e55981d086e57f295"
  },
  {
    "url": "TicTacToe/iframe.html",
    "revision": "005d57efce7f6c7bf88c55d20f782901"
  },
  {
    "url": "TicTacToe/index.html",
    "revision": "d09245957bb358bcad2bb98ddeecd158"
  },
  {
    "url": "Animations/preview.png",
    "revision": "6fc5fda7788fc77d27babd446a7fd61b"
  },
  {
    "url": "Controls/preview.png",
    "revision": "6fc5fda7788fc77d27babd446a7fd61b"
  },
  {
    "url": "CultSim/preview.png",
    "revision": "305331e740541dfd6c220b269a4387eb"
  },
  {
    "url": "Main/preview.png",
    "revision": "6fc5fda7788fc77d27babd446a7fd61b"
  },
  {
    "url": "Render/preview.png",
    "revision": "fdff764bed91094d89ce17154248634d"
  },
  {
    "url": "Test/preview.png",
    "revision": "5cab4ab0f2ed6b4e88117d661e027977"
  },
  {
    "url": "TicTacToe/preview.png",
    "revision": "6fc5fda7788fc77d27babd446a7fd61b"
  }
].concat(self.__precacheManifest || []);
workbox.precaching.suppressWarnings();
workbox.precaching.precacheAndRoute(self.__precacheManifest, {});
