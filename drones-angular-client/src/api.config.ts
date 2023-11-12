// const API_BASE = 'http://127.0.0.1/api';
// const API_BASE = 'https://webhook.site/a8a47c5e-d6e8-4757-8167-93f042322b53/api';

import { environment } from "./environments/environment";

const API_BASE = environment.apiUrl;

export const API_ROUTES = {
  base: API_BASE,
  user: {
    me: `${API_BASE}/auth/me`,
    login: `${API_BASE}/auth/signin`,
    register: `${API_BASE}/auth/signup`,
    update: `${API_BASE}/auth/user`,
    logout: `${API_BASE}/auth/logout`,
  },
  // Other Examples:
  // users: {
  //   base: 'https://your-api-url.com/users',
  //   profile: (userId: number) => `https://your-api-url.com/users/${userId}`,
  //   updateProfile: (userId: number) => `https://your-api-url.com/users/${userId}/update`,
  // },
  // products: {
  //   base: 'https://your-api-url.com/products',
  //   detail: (productId: number) => `https://your-api-url.com/products/${productId}`,
  // },
};

// Usage example:
// getUserProfile(id: number) {
//   return this.http.get<UserProfile>(API_ROUTES.users.profile(id));
// }