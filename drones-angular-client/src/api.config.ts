export const API_ROUTES = {
  users: {
    base: 'https://your-api-url.com/users',
    profile: (userId: number) => `https://your-api-url.com/users/${userId}`,
    updateProfile: (userId: number) => `https://your-api-url.com/users/${userId}/update`,
    // ... other user-related endpoints
  },
  products: {
    base: 'https://your-api-url.com/products',
    detail: (productId: number) => `https://your-api-url.com/products/${productId}`,
    // ... other product-related endpoints
  },
  // ... other groups of endpoints
};

// Usage example:
// getUserProfile(id: number) {
//   return this.http.get<UserProfile>(API_ROUTES.users.profile(id));
// }