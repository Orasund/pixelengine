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
    "revision": "398decbf9da920236495b0a979f829d1"
  },
  {
    "url": "Animations/index.html",
    "revision": "fc2f38284e4cce8e34d55d3a9247b40a"
  },
  {
    "url": "Controls/iframe.html",
    "revision": "84c2cbd3b647bf2958d477b7b699d2df"
  },
  {
    "url": "Controls/index.html",
    "revision": "2bc4cf58e4e4484bea8a64383c0653ca"
  },
  {
    "url": "CultSim/index.html",
    "revision": "416568497a87050ce3af51517420692b"
  },
  {
    "url": "DigDigBoom/index.html",
    "revision": "dcd4db195adc8d377e8d5b81baccaff5"
  },
  {
    "url": "index.html",
    "revision": "38466e13209d250d9e9684bdaa24a4ca"
  },
  {
    "url": "MiniWorldWar/index.html",
    "revision": "dabb25b86aff3d380a6a1c1660d4dbbc"
  },
  {
    "url": "Render/iframe.html",
    "revision": "64658dc3358a1677bcd7c1aa4e926631"
  },
  {
    "url": "Render/index.html",
    "revision": "c8225213e9588b10952986041c300d7c"
  },
  {
    "url": "RuinJump/index.html",
    "revision": "fb02633927ca416b14ae76359a05e8ba"
  },
  {
    "url": "TicTacToe/iframe.html",
    "revision": "005d57efce7f6c7bf88c55d20f782901"
  },
  {
    "url": "TicTacToe/index.html",
    "revision": "8a23c37dcc0a55f4cd54e06e55a7288a"
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
    "url": "Render/preview.png",
    "revision": "872c3966ede83bd1db6f3017ba523cf0"
  },
  {
    "url": "TicTacToe/preview.png",
    "revision": "6fc5fda7788fc77d27babd446a7fd61b"
  }
].concat(self.__precacheManifest || []);
workbox.precaching.suppressWarnings();
workbox.precaching.precacheAndRoute(self.__precacheManifest, {});
