import * as THREE from 'three';
import { API_ROUTES } from '../../../api.config';
import Orientation from '../../ThreeDModule/orientation.js';

export const assetBase = API_ROUTES.threeDModuleAssetsBase;

export const generalParameters = {};
export const audioParameters = {
  enabled: false,
  introductionClips: [
    {
      url: `${assetBase}/clips/el-gringo-12613.mp3`,
      position: 'initial', // Global (non-positional) audio object: ""; positional audio object: "scene x y z" (scene specific position in cartesian coordinates), "maze line column" (maze specific position in cell coordinates), "exit" (maze exit location), "initial" (player initial position), "player" (player current position), "spotlight" (spotlight current position)
      referenceDistance: 1.0,
      loop: false,
      volume: 0.5,
    },
  ],
  idleClips: [
    {
      url: `${assetBase}/clips/Clearing-Throat-Moderate-Speed-www.fesliyanstudios.com.mp3`,
      position: 'player',
      referenceDistance: 1.0,
      loop: false,
      volume: 0.75,
    },
    {
      url: `${assetBase}/clips/Small-Double-Cough-1-www.fesliyanstudios.com.mp3`,
      position: 'player',
      referenceDistance: 1.0,
      loop: false,
      volume: 0.75,
    },
    {
      url: `${assetBase}/clips/Yawn-A2-www.fesliyanstudios.com.mp3`,
      position: 'player',
      referenceDistance: 1.0,
      loop: false,
      volume: 0.75,
    },
  ],
  jumpClips: [
    {
      url: `${assetBase}/clips/Cheering-A6-www.fesliyanstudios.com.mp3`,
      position: 'player',
      referenceDistance: 1.0,
      loop: false,
      volume: 0.75,
    },
    {
      url: `${assetBase}/clips/Cheering-A7-www.fesliyanstudios.com.mp3`,
      position: 'player',
      referenceDistance: 1.0,
      loop: false,
      volume: 0.75,
    },
  ],
  deathClips: [
    {
      url: `${assetBase}/clips/176653326.mp3`,
      position: 'player',
      referenceDistance: 1.0,
      loop: false,
      volume: 0.75,
    },
    {
      url: `${assetBase}/clips/Horn+Squeeze+Clown.mp3`,
      position: 'player',
      referenceDistance: 1.0,
      loop: false,
      volume: 0.75,
    },
  ],
  danceClips: [
    {
      url: `${assetBase}/clips/best-buddies-12609.mp3`,
      position: 'exit',
      referenceDistance: 1.0,
      loop: true,
      volume: 0.5,
    },
  ],
  endClips: [
    {
      url: `${assetBase}/clips/Ba-Bum-Tss-Joke-Drum-A1-www.fesliyanstudios.com.mp3`,
      position: 'exit',
      referenceDistance: 1.0,
      loop: false,
      volume: 2.0,
    },
    {
      url: `${assetBase}/clips/yay-6326.mp3`,
      position: 'exit',
      referenceDistance: 1.0,
      loop: false,
      volume: 0.75,
    },
    {
      url: `${assetBase}/clips/crowd-cheer-ii-6263.mp3`,
      position: 'exit',
      referenceDistance: 1.0,
      loop: false,
      volume: 0.75,
    },
  ],
  credits:
    "Sound clips downloaded from <a href='https://www.dreamstime.com/' target='_blank' rel='noopener'>Dreamstime</a>, <a href='https://www.fesliyanstudios.com/' target='_blank' rel='noopener'>Fesliyan Studios</a> and <a href='https://pixabay.com/' target='_blank' rel='noopener'>Pixabay</a>.",
};
export const cubeTexturesParameters = {
  skyboxes: [
    {
      // Stormy days
      name: 'Stormy days',
      texturePath: `${assetBase}/cube_textures/envmap_stormydays/`,
      texturePositiveXUrl: 'stormydays_ft.jpg',
      textureNegativeXUrl: 'stormydays_bk.jpg',
      texturePositiveYUrl: 'stormydays_up.jpg',
      textureNegativeYUrl: 'stormydays_dn.jpg',
      texturePositiveZUrl: 'stormydays_rt.jpg',
      textureNegativeZUrl: 'stormydays_lf.jpg',
      credits: "Skybox created by <a href='https://opengameart.org/content/stormy-days-skybox' target='_blank' rel='noopener'>Jockum Skoglund (hipshot)</a>.",
    },
    {
      // Miramar
      name: 'Miramar',
      texturePath: `${assetBase}/cube_textures/red-eclipse-skyboxes/skyboxes/`,
      texturePositiveXUrl: 'miramar_ft.jpg',
      textureNegativeXUrl: 'miramar_bk.jpg',
      texturePositiveYUrl: 'miramar_up.jpg',
      textureNegativeYUrl: 'miramar_dn.jpg',
      texturePositiveZUrl: 'miramar_rt.jpg',
      textureNegativeZUrl: 'miramar_lf.jpg',
      credits: "Skybox created by <a href='https://opengameart.org/content/red-eclipse-skyboxes' target='_blank' rel='noopener'>Red Eclipse</a>.",
    },
    {
      // Calm sea
      name: 'Calm sea',
      texturePath: `${assetBase}cube_textures/xonotic-skyboxes/skyboxes/calm_sea/`,
      texturePositiveXUrl: 'calm_sea_ft.jpg',
      textureNegativeXUrl: 'calm_sea_bk.jpg',
      texturePositiveYUrl: 'calm_sea_up.jpg',
      textureNegativeYUrl: 'calm_sea_dn.jpg',
      texturePositiveZUrl: 'calm_sea_rt.jpg',
      textureNegativeZUrl: 'calm_sea_lf.jpg',
      credits: "Skybox created by <a href='https://opengameart.org/content/xonotic-skyboxes' target='_blank' rel='noopener'>Xonotic</a>.",
    },
  ],
  selected: 2,
};
export const mazeParameters = { helpersColor: new THREE.Color(0xff0077) };
export const playerParameters = { helpersColor: new THREE.Color(0x0055ff) };
export const ambientLightParameters = {
  intensity: 0.5,
};
export const directionalLightParameters = {
  intensity: 0.5,
  distance: 20.0,
  orientation: new Orientation(-38.7, 53.1),
  castShadow: true,
  shadow: {
    mapSize: new THREE.Vector2(2048, 2048),
    camera: {
      left: -20.0,
      right: 20.0,
      top: 20.0,
      bottom: -20.0,
      near: 0.0,
      far: 40.0,
    },
  },
};
export const spotLightParameters = {
  visible: false,
  intensity: 90.0,
  distance: 40.0,
  angle: 4.0,
  position: new THREE.Vector3(0.0, 10.0, 0.0),
  castShadow: true,
  shadow: {
    camera: {
      near: 5.0,
      far: 30.0,
    },
  },
};
export const flashLightParameters = {
  color: new THREE.Color(0xffffa0),
  visible: false,
  intensity: 2.0,
  distance: 5.0,
  angle: 20.0,
  orientation: new Orientation(0.0, -20.0),
  castShadow: true,
  shadow: {
    camera: {
      near: 0.01,
      far: 10.0,
    },
  },
};
export const shadowsParameters = { type: THREE.PCFSoftShadowMap };
export const fogParameters = {};
export const collisionDetectionParameters = {};
export const fixedViewCameraParameters = {
  view: 'fixed',
  initialViewport: new THREE.Vector4(0.0, 1.0, 0.45, 0.5),
  initialFogDensity: 0.1,
};
export const firstPersonViewCameraParameters = {
  view: 'first-person',
  initialViewport: new THREE.Vector4(1.0, 1.0, 0.55, 0.5),
  initialOrientation: new Orientation(0.0, -10.0),
  orientationMax: new Orientation(180.0, 90.0),
  initialFogDensity: 0.7,
};
export const thirdPersonViewCameraParameters = {
  view: 'third-person',
  initialViewport: new THREE.Vector4(0.0, 0.0, 0.55, 0.5),
  initialOrientation: new Orientation(0.0, -20.0),
  initialDistance: 2.0,
  distanceMin: 1.0,
  distanceMax: 4.0,
  initialFogDensity: 0.3,
};
export const topViewCameraParameters = {
  view: 'top',
  initialViewport: new THREE.Vector4(1.0, 0.0, 0.45, 0.5),
  initialOrientation: new Orientation(0.0, -90.0),
  initialDistance: 4.0,
  distanceMin: 1.0,
  distanceMax: 16.0,
  initialFogDensity: 0.2,
};
export const miniMapCameraParameters = {
  view: 'mini-map',
  initialViewport: new THREE.Vector4(0.5, 0.5, 0.3, 0.3),
  initialOrientation: new Orientation(180.0, -90.0),
  initialZoom: 0.64,
  zoomMin: 0.64,
  zoomMax: 5.12,
};
