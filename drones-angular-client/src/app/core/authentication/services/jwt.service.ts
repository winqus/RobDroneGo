import { Injectable } from "@angular/core";
import { jwtDecode } from "jwt-decode";

@Injectable({ providedIn: "root" })
export class JwtService {
  private readonly JWT_TOKEN_KEY = 'JWT_TOKEN';

  getToken(): string | null {
    return window.localStorage.getItem(this.JWT_TOKEN_KEY);
  }

  saveToken(token: string): void {
    window.localStorage.setItem(this.JWT_TOKEN_KEY, token);
  }

  isTokenExpired(): boolean {
    const token = this.getToken();
    if (!token) return true;

    const decoded: any = jwtDecode(token);
    return decoded.exp < Date.now() / 1000;
  }

  clearToken(): void {
    window.localStorage.removeItem(this.JWT_TOKEN_KEY);
  }
}
