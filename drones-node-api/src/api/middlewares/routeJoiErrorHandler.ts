import { NextFunction, Request, Response } from 'express';
import { ValidationError } from 'joi';

export default function routeJoiErrorHandler(error: ValidationError, req: Request, res: Response, next: NextFunction) {
  if (error && error.details) {
    const errorMessage = error.details.map((error) => error.message).join(', ');
    res.status(400).json({ error: errorMessage });
  } else {
    next(error);
  }
}
