const multer = require('multer');
const util = require('util');
const fs = require('fs');
import config from '../../../config';

const storage = multer.diskStorage({
  destination: (req, file, cb) => {
    cb(null, config.base_dir + config.publicFolder);
  },
  filename: (req, file, cb) => {
    req.res.locals.file = file;
    cb(null, file.originalname);
  },
});

const uploadFile = multer({ storage: storage }).single('file');
const uploadFilesMiddleware = util.promisify(uploadFile);
export default uploadFilesMiddleware;
