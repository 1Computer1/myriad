cat > program.ts
tsc --lib DOM,ESNext --target ES2019 --strict --skipLibCheck program.ts && cat program.js | node -p 
