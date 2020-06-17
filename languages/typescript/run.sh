cat > program.ts
tsc --lib ESNext --target ES2019 --strict --skipLibCheck program.ts && cat program.js | node -p || true 
